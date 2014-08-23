{
  Copyright 2002-2014 Michalis Kamburelis.

  This file is part of "view3dscene".

  "view3dscene" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ view3dscene, a VRML/X3D browser and general 3D model viewer.
  See [http://castle-engine.sourceforge.net/view3dscene.php] for user
  documentation.

  The real name of this program is "Castle Game Engine swiss army knife" :)
  This program shows and uses a lot of our engine features.
  It's basically a giant GUI to load everything,
  and tweak every option from the menu and toolbar.

  If you want to find out how to use "Castle Game Engine",
  the view3dscene source code isn't the best place to study.
  Look instead at simple examples in engine sources, like
  ../castle_game_engine/examples/3d_rendering_processing/view_3d_model_basic.lpr
  For an example 3D viewer using Lazarus forms, see
  ../castle_game_engine/examples/lazarus/model_3d_viewer/
}

program view3dscene;

{$I castleconf.inc}

{ Icon for Windows. .res file managed manually. }
{$ifdef MSWINDOWS}
  {$R windows/view3dscene.res}
{$endif MSWINDOWS}

{ Icon for Mac OS X. .res file managed by Lazarus. }
{$ifdef DARWIN}
  {$R *.res}
{$endif}

uses Math, CastleUtils, SysUtils, CastleVectors, CastleBoxes, Classes, CastleClassUtils,
  CastleTriangles,
  CastleParameters, CastleProgress, CastleCameras, CastleOpenDocument, CastleConfig,
  CastleStringUtils, CastleFilesUtils, CastleTimeUtils,
  CastleWarnings, CastleLog, CastleProgressConsole, DateUtils, CastleFrustum,
  CastleImages, CastleCubeMaps, CastleDDS, Castle3D, CastleSoundEngine, CastleUIControls, CastleColors,
  CastleKeysMouse, CastleDownload, CastleURIUtils, CastleRays,
  { OpenGL related units: }
  CastleGL, CastleWindow, CastleGLUtils, CastleFonts,
  CastleMessages, CastleWindowProgress, CastleWindowRecentFiles, CastleGLImages,
  CastleGLVersion, CastleGLCubeMaps, CastleControls, CastleGLShaders,
  CastleControlsImages,
  { VRML/X3D (and possibly OpenGL) related units: }
  X3DFields, CastleShapeOctree,
  X3DNodes, X3DLoad, CastleScene, X3DTriangles,
  CastleSceneCore, X3DNodesDetailOptions,
  X3DCameraUtils, CastlePrecalculatedAnimation, CastleBackground,
  CastleRenderer, CastleShapes, CastleRenderingCamera, X3DShadowMaps, CastleSceneManager,
  CastleMaterialProperties,
  { view3dscene-specific units: }
  V3DSceneTextureFilters, V3DSceneLights, V3DSceneRaytrace,
  V3DSceneNavigationTypes, V3DSceneSceneChanges, V3DSceneBGColors, V3DSceneViewpoints,
  V3DSceneBlending, V3DSceneWarnings, V3DSceneFillMode,
  V3DSceneAntiAliasing, V3DSceneScreenShot, V3DSceneCaptions,
  V3DSceneShadows, V3DSceneOctreeVisualize, V3DSceneMiscConfig, V3DSceneImages,
  V3DSceneScreenEffects, V3DSceneHAnim, V3DSceneViewports, V3DSceneVersion,
  V3DSceneLightsEditor, V3DSceneWindow, V3DSceneStatus, V3DSceneNamedAnimations;

var
  ShowFrustum: boolean = false;
  ShowFrustumAlwaysVisible: boolean = false;

  MenuCollisions: TMenuItemChecked;

  RecentMenu: TWindowRecentFiles;

  { Scene global variables.
    Modified only by LoadSceneCore (and all using it Load*Scene* procedures)
    and FreeScene.
    Note that Window.Caption also should be modified only by those procedures.

    In this program's comments I often talk about "null values" of these
    variables, "null values" mean that these variables have some *defined
    but useless* values, i.e.
      SceneAnimation.Loaded = false
      SceneURL = '' }
  { Note that only one SceneAnimation object is created and present for the whole
    lifetime of this program, i.e. when I load new scene (from "Open"
    menu item) I DO NOT free and create new SceneAnimation object.
    Instead I'm only freeing and creating underlying scenes
    (by Close / Load of TCastlePrecalculatedAnimation).
    This way I'm preserving values of all Attributes.Xxx when opening new scene
    from "Open" menu item. }
  SceneAnimation: TCastlePrecalculatedAnimation;
  SceneURL: string;

  SelectedItem: PTriangle;
  { SelectedPoint* always lies on SelectedItem item,
    and it's meaningless when SelectedItem = nil.
    World is in world coords,
    local in local shape (SelectedItem^.State.Transform) coords. }
  SelectedPointWorld, SelectedPointLocal: TVector3Single;
  { SelectedShape is always SelectedItem^.Shape.
    Except in GeometryChanged, when SelectedItem may be already freed,
    and then SelectedShape variable is useful to compare with OnlyShapeChanged. }
  SelectedShape: TShape;

  { Set to non-nil by CreateMainMenu. }
  MenuSelectedOctreeStat: TMenuItem;
  MenuSelectedInfo: TMenuItem;
  MenuSelectedLightsInfo: TMenuItem;
  MenuRemoveSelectedShape: TMenuItem;
  MenuRemoveSelectedFace: TMenuItem;
  MenuEditMaterial: TMenu;
  MenuMergeCloseVertexes: TMenuItem;
  MenuHeadlight, MenuGravity: TMenuItemChecked;
  MenuPreferGravityUpForRotations: TMenuItemChecked;
  MenuPreferGravityUpForMoving: TMenuItemChecked;
  MenuReopen: TMenuItem;

  SceneWarnings: TSceneWarnings;

  { Does user want to process VRML/X3D events? Set by menu item.

    When false, this also makes Time stopped (just like
    AnimationTimePlaying = @false. IOW, always
    SceneAnimation.TimePlaying := AnimationTimePlaying and ProcessEventsWanted.)

    This is simpler for user --- ProcessEventsWanted=false is something
    stricly "stronger" than AnimationTimePlaying=false. Actually, the engine
    *could* do something otherwise, if we would allow Time to pass
    with ProcessEventsWanted=false: time-dependent  MovieTexture
    would still play, as this is not using events (also precalculated
    animation would play). }
  ProcessEventsWanted: boolean = true;

  { If WarningsButton.GetExists is allowed. If false, then WarningsButton.Exists
    should be false, regardless of warnings count. }
  WarningsButtonEnabled: boolean = true;
  WarningsButton: TCastleButton;

  ToolbarPanel: TCastlePanel;
  CollisionsButton: TCastleButton;

  AnimationTimeSpeedWhenLoading: TFloatTime = 1.0;
  AnimationTimePlaying: boolean = true;
  MenuAnimationTimePlaying: TMenuItemChecked;

  ControlsOnScreenshot: boolean = false;

{ Helper class ---------------------------------------------------------------

  Some callbacks here must be methods (procedure of class),
  so we use this dummy class to contain them. }

type
  THelper = class
    class procedure OpenRecent(const URL: string);
    class procedure GeometryChanged(Scene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);
    class procedure ViewpointsChanged(Scene: TCastleSceneCore);
    class procedure BoundViewpointChanged(Sender: TObject);
    class procedure BoundNavigationInfoChanged(Sender: TObject);
    class procedure PointingDeviceSensorsChange(Sender: TObject);
    class procedure HeadlightOnChanged(Sender: TObject);
    class procedure WarningsButtonClick(Sender: TObject);
    class procedure NavigationTypeButtonClick(Sender: TObject);
    class procedure OpenButtonClick(Sender: TObject);
    class procedure CollisionsButtonClick(Sender: TObject);
    class procedure ScreenshotButtonClick(Sender: TObject);
  end;

{ SceneManager and viewport ------------------------------------------------ }

type
  TV3DSceneManager = class(TV3DShadowsSceneManager)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
    procedure Render3D(const Params: TRenderParams); override;
  public
    procedure BeforeRender; override;
    procedure Render; override;
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function ScreenEffectsCount: Integer; override;
    function ScreenEffectsNeedDepth: boolean; override;
    function Background: TBackground; override;
  end;

  TV3DViewport = class(TV3DShadowsViewport)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
    procedure Render3D(const Params: TRenderParams); override;
  public
    procedure BeforeRender; override;
    procedure Render; override;
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function ScreenEffectsCount: Integer; override;
    function ScreenEffectsNeedDepth: boolean; override;
    function Background: TBackground; override;
  end;

var
  SceneManager: TV3DSceneManager;

procedure ViewportProperties(Viewport: TCastleAbstractViewport);
begin
  ViewportShadowsProperties(Viewport);
  Viewport.BackgroundWireframe := FillModes[FillMode].BackgroundWireframe;
end;

function TV3DSceneManager.GetScreenEffects(const Index: Integer): TGLSLProgram;
var
  C: Integer;
begin
  C := inherited ScreenEffectsCount;
  if Index >= C then
    Result := V3DSceneScreenEffects.ScreenEffects.ActiveEffects(Index - C) else
    Result := inherited GetScreenEffects(Index);
end;

function TV3DSceneManager.ScreenEffectsCount: Integer;
begin
  Result := (inherited ScreenEffectsCount) +
    V3DSceneScreenEffects.ScreenEffects.ActiveEffectsCount;
end;

function TV3DSceneManager.ScreenEffectsNeedDepth: boolean;
begin
  Result := (inherited ScreenEffectsNeedDepth) or
    V3DSceneScreenEffects.ScreenEffects.ActiveEffectsNeedDepth;
end;

var
  DisableBackground: Cardinal;

function TV3DSceneManager.Background: TBackground;
begin
  if DisableBackground <> 0 then
    Result := nil else
    Result := inherited;
end;

function TV3DViewport.GetScreenEffects(const Index: Integer): TGLSLProgram;
var
  C: Integer;
begin
  C := inherited ScreenEffectsCount;
  if Index >= C then
    Result := V3DSceneScreenEffects.ScreenEffects.ActiveEffects(Index - C) else
    Result := inherited GetScreenEffects(Index);
end;

function TV3DViewport.ScreenEffectsCount: Integer;
begin
  Result := (inherited ScreenEffectsCount) +
    V3DSceneScreenEffects.ScreenEffects.ActiveEffectsCount;
end;

function TV3DViewport.ScreenEffectsNeedDepth: boolean;
begin
  Result := (inherited ScreenEffectsNeedDepth) or
    V3DSceneScreenEffects.ScreenEffects.ActiveEffectsNeedDepth;
end;

function TV3DViewport.Background: TBackground;
begin
  if DisableBackground <> 0 then
    Result := nil else
    Result := inherited;
end;

{ Helper functions ----------------------------------------------------------- }

{ First animation scene. Always present when loaded,
  nil if not (although our SceneAnimation tries to always stay loaded).
  We use it for VRML/X3D events, and all other stuff where
  a single scene is needed. }
function Scene: TCastleScene;
begin
  if SceneAnimation.Loaded then
    Result := SceneAnimation.Scenes[0] else
    Result := nil;
end;

procedure UpdateSelectedEnabled;
begin
  if MenuSelectedInfo <> nil then
    MenuSelectedInfo.Enabled := SelectedItem <> nil;
  if MenuSelectedOctreeStat <> nil then
    MenuSelectedOctreeStat.Enabled := SelectedItem <> nil;
  if MenuSelectedLightsInfo <> nil then
    MenuSelectedLightsInfo.Enabled := SelectedItem <> nil;
  if MenuRemoveSelectedShape <> nil then
    MenuRemoveSelectedShape.Enabled := SelectedItem <> nil;
  if MenuRemoveSelectedFace <> nil then
    MenuRemoveSelectedFace.Enabled := SelectedItem <> nil;
  if MenuEditMaterial <> nil then
    MenuEditMaterial.Enabled := SelectedItem <> nil;
  if MenuMergeCloseVertexes <> nil then
    MenuMergeCloseVertexes.Enabled := SelectedItem <> nil;
end;

{ Update menu items and buttons that reflect Camera properties }
procedure UpdateCameraUI;
begin
  UpdateCameraNavigationTypeUI;
  if MenuGravity <> nil then
    MenuGravity.Checked := Camera.Walk.Gravity;
  if MenuPreferGravityUpForRotations <> nil then
    MenuPreferGravityUpForRotations.Checked := Camera.Walk.PreferGravityUpForRotations;
  if MenuPreferGravityUpForMoving <> nil then
    MenuPreferGravityUpForMoving.Checked := Camera.Walk.PreferGravityUpForMoving;
end;

{ Return currently used collisions octree.

  Note: When SceneAnimation.Collides = true, octree is always initialized
  (SceneOctreeCreate is called, and corresponding SceneOctreeDestroy
  was not).
  Otherwise, when SceneAnimation.Collides = false, octree *may* be available
  but doesn't have to. When setting SceneAnimation.Collides to false we do not
  immediately destroy the octree (in case user will just go back
  to SceneAnimation.Collides = true next), but it will be destroyed on next
  rebuild of octree (when we will just destroy old and not recreate new).
}
function SceneOctreeCollisions: TBaseTrianglesOctree;
begin
  if (SceneAnimation <> nil) and
     (Scene <> nil) and
     (Scene.OctreeCollisions <> nil) then
    Result := Scene.OctreeCollisions else
    Result := nil;
end;

function SceneOctreeRendering: TShapeOctree;
begin
  if (SceneAnimation <> nil) and
     (Scene <> nil) and
     (Scene.OctreeRendering <> nil) then
    Result := Scene.OctreeRendering else
    Result := nil;
end;

{ This calls SceneManager.PrepareResources
  (that causes SceneAnimation.PrepareResources).
  Additionally, if AllowProgess and some other conditions are met,
  this shows progress of operation.

  Remember that you can call this only when gl context is already active
  (SceneAnimation.PrepareResources requires this) }
procedure PrepareResources(AllowProgress: boolean);
begin
  if AllowProgress and (SceneAnimation.ScenesCount > 1) then
    SceneManager.PrepareResources('Preparing animation') else
    SceneManager.PrepareResources;
end;

procedure SceneOctreeCreate; forward;

procedure SetCollisions(const Value: boolean;
  const NeedMenuUpdate: boolean = true);
begin
  if SceneAnimation.Collides <> Value then
  begin
    SceneAnimation.Collides := Value;
    CollisionsButton.Pressed := Value;
    if NeedMenuUpdate then
      MenuCollisions.Checked := Value;
    if SceneAnimation.Collides and
      (SceneAnimation.FirstScene.OctreeCollisions = nil) then
      SceneOctreeCreate;
  end;
end;

function ViewpointNode: TAbstractViewpointNode; forward;

{ TExtendedStatusText -------------------------------------------------------- }

type
  TExtendedStatusText = class(TStatusText)
  protected
    procedure CalculateText; override;
  end;

procedure TExtendedStatusText.CalculateText;
const
  HighlightBegin = '<font color="#ffffff">';
  HighlightEnd = '</font>';

  { Describe pointing-device sensors (active and under the mouse). }
  procedure DescribeSensors;

    function DescribeSensor(Sensor: TX3DNode): string;
    var
      Description: string;
      Anchor: TAnchorNode;
      J: Integer;
    begin
      Result := Format('%s', [Sensor.NiceName]);

      if Sensor is TAbstractPointingDeviceSensorNode then
      begin
        { use description instead, if any provided }
        Description := Trim(TAbstractPointingDeviceSensorNode(Sensor).FdDescription.Value);
        if Description <> '' then
          Result := Description;
      end else
      if Sensor is TAnchorNode then
      begin
        Anchor := TAnchorNode(Sensor);
        { use description instead, if any provided }
        Description := Trim(Anchor.FdDescription.Value);
        if Description <> '' then
          Result := Description;
        if Anchor.FdUrl.Count <> 0 then
        begin
          Result += ' [' + URIDisplay(Anchor.FdUrl.Items[0]);
          for J := 1 to Anchor.FdUrl.Count - 1 do
            Result += ', ' + URIDisplay(Anchor.FdUrl.Items[J]);
          Result += ']';
        end;
      end;

      Result := SForCaption(Result, MaxLineChars);
    end;

  var
    Over: TPointingDeviceSensorList;
    Active: TX3DNodeList;
    I: Integer;
    S: string;
  begin
    if SceneAnimation.ScenesCount = 1 then
    begin
      if Scene.PointingDeviceOverItem <> nil then
        Over := Scene.PointingDeviceSensors else
        Over := nil;
      Active := Scene.PointingDeviceActiveSensors;

      if (Active.Count <> 0) or
         ((Over <> nil) and
          (Over.Count <> 0)) then
      begin
        { Display sensors active but not over.
          We do not just list all active sensors in the 1st pass,
          because we prefer to list active sensors in the (more stable) order
          they have on Over list. See also g3l_stapes_dbg.wrl testcase. }
        for I := 0 to Active.Count - 1 do
          if (Over = nil) or
             (Over.IndexOf(Active[I]) = -1) then
            Text.Append(HighlightBegin + DescribeSensor(Active[I]) + HighlightEnd);

        { Display sensors over which the mouse hovers (and enabled).
          Highlight active sensors on the list. }
        if Over <> nil then
          for I := 0 to Over.Count - 1 do
            if Over.Enabled(I) then
            begin
              S := DescribeSensor(Over[I]);
              if Active.IndexOf(Over[I]) <> -1 then
                S := HighlightBegin + S + HighlightEnd;
              Text.Append(S);
            end;

        { Note that sensors that are "active and over" are undistinguishable
          from "active and not over".
          This means that a tiny bit of information is lost
          (because you are not *always* over an active sensor, so this way
          you don't know if you're over or not over an active sensor).
          But it's not really useful information in practice, and hiding it
          makes the sensors status look much cleaner. }

        { a blank line, separating from the rest of status, if needed }
        if Text.Count <> 0 then
          Text.Append('');
      end;
    end;
  end;

  function CurrentAboveHeight: string;
  begin
    if SceneOctreeCollisions = nil then
      Result := 'no collisions' else
    if not Camera.Walk.Gravity then
      Result := 'no gravity' else
    if not Camera.Walk.IsAbove then
      Result := 'no ground beneath' else
      Result := FloatToNiceStr(Camera.Walk.AboveHeight);
  end;

var
  s: string;
  Statistics: TRenderStatistics;
  Pos, Dir, Up: TVector3Single;
begin
  inherited;

  Statistics := SceneManager.Statistics;

  DescribeSensors;

  { S := Format('Collision detection: %s', [ BoolToStrOO[SceneAnimation.Collides] ]);
  if SceneOctreeCollisions = nil then
    S += ' (octree resources released)';
  Text.Append(S); }

  Camera.GetView(Pos, Dir, Up);
  Text.Append(Format('Camera: pos %s, dir %s, up %s',
    [ VectorToNiceStr(Pos), VectorToNiceStr(Dir), VectorToNiceStr(Up) ]));

  if Camera.NavigationClass = ncWalk then
  begin
    Text.Append(Format('Move speed (per sec) : %f, Avatar height: %f (last height above the ground: %s)',
      [ Camera.Walk.MoveSpeed,
        Camera.Walk.PreferredHeight,
        CurrentAboveHeight ]));
  end;

  { if SceneLightsCount = 0 then
   s := '(useless, scene has no lights)' else
   s := BoolToStrOO[SceneAnimation.Attributes.UseSceneLights];
  Text.Append(Format('Use scene lights: %s', [s])); }

  if SceneAnimation.Attributes.UseOcclusionQuery or
     SceneAnimation.Attributes.UseHierarchicalOcclusionQuery then
    S := Format(' (+ %d boxes to occl query)', [Statistics.BoxesOcclusionQueriedCount]) else
    S := '';
  Text.Append(Format('Rendered Shapes : %d%s of %d ',
    [ Statistics.ShapesRendered, S,
      Statistics.ShapesVisible ]) + OctreeDisplayStatus);

  if SceneAnimation.TimeAtLoad = 0.0 then
    S := Format('World time: %d', [Trunc(SceneAnimation.Time)]) else
    S := Format('World time: load time + %d = %d',
      [Trunc(SceneAnimation.Time - SceneAnimation.TimeAtLoad),
       Trunc(SceneAnimation.Time)]);
  if not AnimationTimePlaying then
    S += ' (paused)';
  if not ProcessEventsWanted then
    S += ' (paused, not processing VRML/X3D events)';
  Text.Append(S);
end;

{ TCastleWindowCustom callbacks --------------------------------------------------------- }

{ Render visualization of various stuff, like bounding box, octree and such. }
procedure RenderVisualizations;

  procedure RenderFrustum(AlwaysVisible: boolean);
  {$ifndef OpenGLES} //TODO-es
  var
    FrustumPoints: TFrustumPointsDouble;
  begin
    if AlwaysVisible then
    begin
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
    end;
    try
      (Camera as TUniversalCamera).Walk.Frustum.CalculatePoints(FrustumPoints);
      glColor3f(1, 1, 1);
      glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(4, GL_DOUBLE, 0, @FrustumPoints);
        glDrawElements(GL_LINES, 12 * 2, GL_UNSIGNED_INT,
          @FrustumPointsLinesIndexes);
      glDisableClientState(GL_VERTEX_ARRAY);
    finally
      if AlwaysVisible then glPopAttrib;
    end;
  {$else}
  begin
  {$endif}
  end;

begin
  if (RenderingCamera.Target = rtScreen) and (not MakingScreenShot) then
  begin
    { Visualization below depends on DEPTH_TEST enabled
      (and after rendering scene, it is disabled by TGLRenderer.RenderCleanState) }
    glEnable(GL_DEPTH_TEST);

    { Use SceneAnimation.Attributes.LineWidth for our visualizations as well }
    glLineWidth(SceneAnimation.Attributes.LineWidth);

    OctreeDisplay(SceneAnimation);

    if ShowBBox then
    begin
      {$ifndef OpenGLES} //TODO-es
      { Display current bounding box only if there's a chance that it's
        different than whole animation BoundingBox --- this requires that animation
        has at least two frames. }
      if SceneAnimation.ScenesCount > 1 then
      begin
        glColorv(Red);
        if not SceneAnimation.CurrentScene.BoundingBox.IsEmpty then
          glDrawBox3DWire(SceneAnimation.CurrentScene.BoundingBox);
      end;

      glColorv(Green);
      if not SceneAnimation.BoundingBox.IsEmpty then
        glDrawBox3DWire(SceneAnimation.BoundingBox);
      {$endif}
    end;

    { Note that there is no sense in showing viewing frustum in
      Camera.NavigationClass <> ncExamine, since viewing frustum should
      be never visible then (or should be just at the exact borders
      or visibility, so it's actually unspecified whether OpenGL would
      show it or not). }
    if ShowFrustum and ((Camera as TUniversalCamera).NavigationClass = ncExamine) then
      RenderFrustum(ShowFrustumAlwaysVisible);

    if SelectedItem <> nil then
    begin
      {$ifndef OpenGLES} //TODO-es
      glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
        glDisable(GL_DEPTH_TEST); { saved by GL_ENABLE_BIT }
        glColorv(Vector4Single(1, 1, 1, 0.25)); { may be changed carelessly }

        glBegin(GL_LINE_LOOP);
          glVertexv(SelectedItem^.World.Triangle[0]);
          glVertexv(SelectedItem^.World.Triangle[1]);
          glVertexv(SelectedItem^.World.Triangle[2]);
        glEnd;

        glPointSize(5.0); { may be changed carelessly }
        glBegin(GL_POINTS);
          glVertexv(SelectedPointWorld);
        glEnd;

        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); { saved by GL_COLOR_BUFFER_BIT }
        glEnable(GL_BLEND); { saved by GL_ENABLE_BIT }
        glBegin(GL_TRIANGLES);
          glVertexv(SelectedItem^.World.Triangle[0]);
          glVertexv(SelectedItem^.World.Triangle[1]);
          glVertexv(SelectedItem^.World.Triangle[2]);
        glEnd;
      glPopAttrib;
      {$endif}
    end;
  end;
end;

procedure TV3DSceneManager.Render3D(const Params: TRenderParams);
begin
  inherited;
  { RenderVisualizations are opaque, so they should be rendered here
    to correctly mix with partially transparent 3D scenes.
    Render as ShadowVolumesReceivers=true to make selected triangle
    drawn last (on top), to be clearly and always visible. }
  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
    RenderVisualizations;
end;

procedure TV3DSceneManager.RenderFromView3D(const Params: TRenderParams);
begin
  { Although TCastleAbstractViewport is ready for MainScene = nil case,
    this RenderFromView3D below is not. Doesn't seem needed,
    but better to secure for this case, since any TCastleAbstractViewport
    should always work with MainScene = nil. }
  if MainScene = nil then Exit;

  if FillMode = fmSilhouetteBorderEdges then
  begin
    { Use SceneAnimation.Attributes.LineWidth for our visualizations as well }
    glLineWidth(SceneAnimation.Attributes.LineWidth);
    RenderSilhouetteBorderEdges(Camera.GetPosition, MainScene);
    RenderVisualizations;
  end else
  begin
    inherited;
    { inherited will call Render3D that will call RenderVisualizations }
  end;
end;

procedure TV3DSceneManager.BeforeRender;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DSceneManager.Render;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DViewport.Render3D(const Params: TRenderParams);
begin
  inherited;
  { RenderVisualizations are opaque, so they should be rendered here
    to correctly mix with partially transparent 3D scenes.
    Render as ShadowVolumesReceivers=true to make selected triangle
    drawn last (on top), to be clearly and always visible. }
  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
    RenderVisualizations;
end;

procedure TV3DViewport.RenderFromView3D(const Params: TRenderParams);
begin
  { Although TCastleAbstractViewport is ready for MainScene = nil case,
    this RenderFromView3D below is not. Doesn't seem needed,
    but better to secure for this case, since any TCastleAbstractViewport
    should always work with MainScene = nil. }
  if GetMainScene = nil then Exit;

  if FillMode = fmSilhouetteBorderEdges then
  begin
    RenderVisualizations;
    RenderSilhouetteBorderEdges(Camera.GetPosition, GetMainScene);
  end else
  begin
    inherited;
    { inherited will call Render3D that will call RenderVisualizations }
  end;
end;

procedure TV3DViewport.BeforeRender;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DViewport.Render;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

const
  SNavigationClassWalkNeeded =
    'You must be in "Walk", "Fly" or "None" navigation types to use this function.';

  SOnlyWhenOctreeAvailable = 'This is not possible when octree is not generated. Turn on "Navigation -> Collision Detection" to make it available.';

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  { Support selecting item by ctrl + right button click. }
  if Event.IsMouseButton(mbRight) and (mkCtrl in Window.Pressed.Modifiers) then
  begin
    SelectedItem := Scene.PointingDeviceOverItem;
    SelectedPointWorld := Scene.PointingDeviceOverPoint;

    { calculate SelectedPointLocal }
    if SelectedItem <> nil then
    begin
      SelectedShape := TShape(SelectedItem^.Shape);
      try
        SelectedPointLocal := MatrixMultPoint(
          SelectedItem^.State.InvertedTransform, SelectedPointWorld);
      except
        on ETransformedResultInvalid do
          SelectedItem := nil;
      end;
    end;

    UpdateSelectedEnabled;

    Window.Invalidate;
  end;
end;

class procedure THelper.PointingDeviceSensorsChange(Sender: TObject);
begin
  { Our status text displays current sensors (under the mouse,
    and currently active (if any)), so we have to redisplay. }
  Window.Invalidate;
end;

{ Setting viewpoint ---------------------------------------------------------- }

function NavigationNode: TNavigationInfoNode;
begin
  if Scene <> nil then
    Result := Scene.NavigationInfoStack.Top else
    Result := nil;
end;

function ViewpointNode: TAbstractViewpointNode;
begin
  if Scene <> nil then
    Result := Scene.ViewpointStack.Top else
    Result := nil;
end;

class procedure THelper.ViewpointsChanged(Scene: TCastleSceneCore);
begin
  Viewpoints.Recalculate(Scene);
end;

class procedure THelper.BoundViewpointChanged(Sender: TObject);
var
  V: TAbstractViewpointNode;
begin
  V := SceneManager.MainScene.ViewpointStack.Top;
  Viewpoints.BoundViewpoint := Viewpoints.ItemOf(V);
end;

class procedure THelper.BoundNavigationInfoChanged(Sender: TObject);
begin
  UpdateCameraUI;
end;

class procedure THelper.HeadlightOnChanged(Sender: TObject);
begin
  if MenuHeadlight <> nil then
    MenuHeadlight.Checked := Scene.HeadLightOn;
end;

{ Scene operations ---------------------------------------------------------- }

{ Call when WarningsButtonEnabled or SceneWarnings.Count changes
  or when window sizes change. }
procedure UpdateWarningsButton;
begin
  WarningsButton.Caption := Format('%d warnings', [SceneWarnings.Count]);
  WarningsButton.Exists := WarningsButtonEnabled and (SceneWarnings.Count <> 0);
  { When window is closed, width/height may be incorrect (even negative,
    because of WindowDefaultSize). Do not call EventResize then.
    May happen when you used --write, and some warning occurs. }
  if not Window.Closed then
    Window.Container.EventResize; { update WarningsButton.Left }
end;

procedure OnWarningHandle(const AType: TWarningType; const Category, S: string);
begin
  { Write to ErrOutput, not normal Output, since when --write is used,
    we write to output VRML/X3D contents. }
  Writeln(ErrOutput, ApplicationName + ': ' + Category + ' warning: ' + S);
  SceneWarnings.Add(Category + ': ' + S);
  UpdateWarningsButton;
  if Window <> nil then
    Window.Invalidate;
end;

procedure SceneOctreeCreate;
var
  OldRender, OldBeforeRender: TContainerEvent;
begin
  { Do not create octrees when SceneAnimation.Collides = false. This makes
    setting SceneAnimation.Collides to false an optimization: octree doesn't have to
    be recomputed when animation frame changes, or new scene is loaded etc. }

  if SceneAnimation.Collides then
  begin
    { Beware: constructing octrees will cause progress drawing,
      and progress drawing may cause SaveScreen,
      and SaveScreen may cause OnRender and OnBeforeRender to be called.
      That's why we simply turn normal Render/BeforeRender temporarily off. }
    OldRender := Window.OnRender;
    OldBeforeRender := Window.OnBeforeRender;
    Window.OnRender := nil;
    Window.OnBeforeRender := nil;
    try
      { For now we construct and store octrees only for the 1st animation frame. }

      Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
      Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
      Scene.Spatial := [ssRendering, ssDynamicCollisions];
    finally
      Window.OnRender := OldRender;
      Window.OnBeforeRender := OldBeforeRender;
    end;
  end;
end;

procedure SceneOctreeFree;
begin
  if Scene <> nil then
  begin
    { Since we destroy our PTriangles, make sure Scene
      doesn't hold a reference to it.

      Note: PointingDeviceClear will automatically update current cursor
      (by calling OnPointingDeviceSensorsChange that leads to our method). }
    Scene.PointingDeviceClear;

    Scene.Spatial := [];
  end;
end;

procedure Unselect;
begin
  SelectedItem := nil;
  SelectedShape := nil;
  UpdateSelectedEnabled;
end;

{ Frees (and sets to some null values) "scene global variables".

  Note about OpenGL context: remember that calling Close
  on SceneAnimation also calls GLContextClose  that closes all connections
  of Scene to OpenGL context. This means that:
  1) SceneAnimation must not be Loaded, or Scene must not have any
     connections with OpenGL context (like e.g. after calling
     SceneAnimation.GLContextClose)
  2) or you must call FreeScene in the same OpenGL context that the
     SceneAnimation is connected to. }
procedure FreeScene;
begin
  SceneOctreeFree;

  SceneAnimation.Close;
  { SceneAnimation.Close must free all scenes, including FirstScene,
    which should (through free notification) set to nil also
    SceneManager.MainScene }
  Assert(SceneManager.MainScene = nil);

  Viewpoints.Recalculate(nil);
  if MenuNamedAnimations <> nil then
    MenuNamedAnimations.Clear;

  SceneURL := '';

  if MenuReopen <> nil then
    MenuReopen.Enabled := false;

  Unselect;
end;

{ Update state of ProcessEvents for all SceneAnimation.Scenes[].

  This looks at ProcessEventsWanted and SceneAnimation.ScenesCount,
  so should be repeated when they change, and when new Scenes[] are loaded. }
procedure UpdateProcessEvents;
var
  I: Integer;
  Value: boolean;
begin
  { Always disable ProcessEvents for TCastlePrecalculatedAnimation consisting of many models. }
  Value := ProcessEventsWanted and (SceneAnimation.ScenesCount = 1);

  for I := 0 to SceneAnimation.ScenesCount - 1 do
    SceneAnimation.Scenes[I].ProcessEvents := Value;
end;

procedure LoadClearScene; forward;

{ Calls FreeScene and then inits "scene global variables".
  Pass here ACameraRadius = 0.0 to say that CameraRadius should be
  somehow calculated (guessed) based on loaded Scene data.

  Camera settings for scene are inited from VRML/X3D defaults and
  from camera node in scene.

  Exceptions: if this function will raise any exception you should assume
  that scene loading failed for some reason and "scene global variables"
  are set to their "null values". I.e. everything is in a "clean" state
  like after FreeScene.

  This procedure does not really open any file
  (so ASceneURL need not be a name of existing file/URL).
  Instead it uses already created RootNode to init "scene global variables".

  Note that all RootNodes[] will be owned by Scene.
  So do not Free RootNodes[] items after using this procedure
  (still, you should use RootNodes list itself).

  Note that this may change the value of Times list.

  Note that there is one "scene global variable" that will
  not be completely handled by this procedure:
  SceneWarnings. During this procedure some warnings may
  occur and be appended to SceneWarnings. You have to take care
  about the rest of issues with the SceneWarnings, like clearing
  them before calling LoadSceneCore.

  ASceneURL is not const parameter, to allow you to pass
  SceneURL as ASceneURL. If ASceneURL would be const
  we would have problem, because FreeScene modifies SceneURL
  global variable, and this would change ASceneURL value
  (actually, making it possibly totally invalid pointer,
  pointing at some other place of memory). That's always the
  problem with passing pointers to global variables
  (ASceneURL is a pointer) as local vars.

  If UseInitialVars then we will use and reset
  InitialNavigationType and InitialViewpoint*.
  This should be @false if you're only loading
  temporary scene, like LoadClearScene. }
procedure LoadSceneCore(
  RootNodes: TX3DNodeList;
  ATimes: TSingleList;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;

  ASceneURL: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  InitializeCamera: boolean;

  UseInitialVars: boolean = true);

  procedure ScaleAll(A: TSingleList; const Value: Single);
  var
    I: Integer;
  begin
    for I := 0 to A.Count - 1 do
      A.L[I] *= Value;
  end;

var
  NewCaption: string;
  I: Integer;
  ForceNavigationType: string;
begin
  FreeScene;

  try
    SceneURL := ASceneURL;

    if AnimationTimeSpeedWhenLoading <> 1.0 then
      ScaleAll(ATimes, 1 / AnimationTimeSpeedWhenLoading);

    { set InitialViewpoint* before creating the TCastleSceneCores, so before
      doing SceneAnimation.Load }
    SetInitialViewpoint(SceneAnimation, UseInitialVars);

    SceneAnimation.Load(RootNodes, true, ATimes, ScenesPerTime, EqualityEpsilon);
    SceneAnimation.TimeLoop := TimeLoop;
    SceneAnimation.TimeBackwards := TimeBackwards;
    { do it before even assigning MainScene, as assigning MainScene may
      (through VisibleChange notification) already want to refer
      to current animation scene, so better make it sensible. }
    SceneAnimation.ResetTimeAtLoad;

    { assign SceneManager.MainScene relatively early, because our
      rendering assumes that SceneManager.MainScene is usable,
      and rendering may be called during progress bars even from this function. }
    SceneManager.MainScene := Scene;

    ChangeAnimation(SceneChanges, SceneAnimation);

    { calculate Viewpoints, including MenuJumpToViewpoint. }
    Viewpoints.Recalculate(Scene);

    if UseInitialVars then
    begin
      ForceNavigationType := InitialNavigationType;
      InitialNavigationType := '';
    end else
      ForceNavigationType := '';

    if InitializeCamera then
    begin
      SceneManager.MainScene.CameraFromNavigationInfo(Camera,
        SceneAnimation.BoundingBox, ForceNavigationType, ACameraRadius);
      SceneManager.MainScene.CameraFromViewpoint(Camera, false, false);
      for I := 0 to High(Viewports) do
        AssignCamera(Viewports[I], SceneManager, SceneManager, false);
      Viewpoints.BoundViewpoint := Viewpoints.ItemOf(ViewpointNode);
    end else
      { No CameraFromViewpoint of this scene callled, so no viewpoint bound }
      Viewpoints.BoundViewpoint := nil;

    SceneInitLights(SceneAnimation, NavigationNode);

    { update MenuHeadlight.Checked now, and make it always updated. }
    THelper.HeadlightOnChanged(Scene);
    Scene.OnHeadlightOnChanged := @THelper(nil).HeadlightOnChanged;

    NewCaption := Scene.Caption;
    if NewCaption = '' then
      NewCaption := URICaption(SceneURL);
    NewCaption := SForCaption(NewCaption) + ' - view3dscene';
    Window.Caption := NewCaption;

    UpdateCameraUI;

    SceneOctreeCreate;

    for I := 0 to SceneAnimation.ScenesCount - 1 do
    begin
      SceneAnimation.Scenes[I].OnGeometryChanged := @THelper(nil).GeometryChanged;
      SceneAnimation.Scenes[I].OnViewpointsChanged := @THelper(nil).ViewpointsChanged;
      SceneAnimation.Scenes[I].OnPointingDeviceSensorsChange := @THelper(nil).PointingDeviceSensorsChange;

      { Regardless of ProcessEvents, we may change the nodes graph,
        e.g. by Edit->Material->...
        This is now ensured by TryFirstSceneDynamic. }
      Assert(SceneAnimation.Scenes[I].Static = (SceneAnimation.ScenesCount <> 1));
    end;

    UpdateProcessEvents;

    { Make initial CameraChanged to make initial events to
      ProximitySensor, if user is within. }
    Scene.CameraChanged(SceneManager.Camera, SceneManager.CameraToChanges);

    RefreshNamedAnimations(Scene);

    if not Window.Closed then
      Scene.VisibleChangeHere([]);

    if MenuReopen <> nil then
      MenuReopen.Enabled := SceneURL <> '';
  except
    FreeScene;
    raise;
  end;
end;

{ Show the loading message. In normal circumstances, just shows using MessageOK
  (on Window, which must be initialized now).
  When we're in screenshot (batch) mode, simply shows it on StdErr. }
procedure LoadErrorMessage(const S: string);
begin
  if not MakingScreenShot then
    MessageOK(Window, S) else
    Writeln(ErrOutput, 'view3dscene: ', S);
end;

{ This loads the scene from file (using Load3DSequence) and
  then inits our scene variables by LoadSceneCore.

  If it fails, it tries to preserve current scene
  (if it can't preserve current scene, only then it resets it to clear scene).
  Also, it shows the error message using MessageOK
  (so Glw must be already open).

  It may seem that ASceneURL could be constant parameter here.
  Yes, it could. However, you will sometimes want to pass here
  SceneURL global value and this would cause memory havoc
  (parameter is passed as const, however when global variable
  SceneURL is changed then the parameter value implicitly
  changes, it may even cause suddenly invalid pointer --- yeah,
  I experienced it). }
procedure LoadScene(ASceneURL: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  InitializeCamera: boolean);

{ It's useful to undefine it only for debug purposes:
  FPC dumps then backtrace of where exception happened,
  which is often enough to trace the error.
  In release versions this should be defined to produce a nice
  message box in case of errors (instead of just a crash). }
{$define CATCH_EXCEPTIONS}

var
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;
  SavedSceneWarnings: TSceneWarnings;
begin
  RootNodes := TX3DNodeList.Create(false);
  Times := TSingleList.Create;
  try
    { We have to clear SceneWarnings here (not later)
      to catch also all warnings raised during parsing of the file.
      This causes a potential problem: if loading the scene will fail,
      we should restore the old warnings (if the old scene will be
      preserved) or clear them (if the clear scene will be loaded
      --- LoadClearScene will clear them). }
    SavedSceneWarnings := TSceneWarnings.Create;
    try
      SavedSceneWarnings.Assign(SceneWarnings);
      SceneWarnings.Clear;

      {$ifdef CATCH_EXCEPTIONS}
      try
      {$endif CATCH_EXCEPTIONS}
        Load3DSequence(ASceneURL, true,
          RootNodes, Times, ScenesPerTime, EqualityEpsilon,
          TimeLoop, TimeBackwards);
      {$ifdef CATCH_EXCEPTIONS}
      except
        on E: Exception do
        begin
          LoadErrorMessage('Error while loading scene from "' +ASceneURL+ '": ' +
            E.Message);
          { In this case we can preserve current scene. }
          SceneWarnings.Assign(SavedSceneWarnings);
          Exit;
        end;
      end;
      {$endif CATCH_EXCEPTIONS}
    finally FreeAndNil(SavedSceneWarnings) end;

    {$ifdef CATCH_EXCEPTIONS}
    try
    {$endif CATCH_EXCEPTIONS}
      LoadSceneCore(
        RootNodes, Times,
        ScenesPerTime, EqualityEpsilon,
        TimeLoop, TimeBackwards,
        ASceneURL, SceneChanges, ACameraRadius, InitializeCamera);
    {$ifdef CATCH_EXCEPTIONS}
    except
      on E: Exception do
      begin
        { In this case we cannot preserve old scene, because
          LoadSceneCore does FreeScene when it exits with exception
          (and that's because LoadSceneCore modifies some global scene variables
          when it works --- so when something fails inside LoadSceneCore,
          we are left with some partially-initiaized state,
          that is not usable; actually, LoadSceneCore
          also does FreeScene when it starts it's work --- to start
          with a clean state).

          We call LoadClearScene before we call MessageOK, this way
          our Render routine works OK when it's called to draw background
          under MessageOK. }
        LoadClearScene;
        LoadErrorMessage('Error while loading scene from "' + ASceneURL + '": ' +
          E.Message);
        Exit;
      end;
    end;
    {$endif CATCH_EXCEPTIONS}

    { For batch operation (making screenshots), do not save the scene
      on "recent files" menu. This also applies when using view3dscene
      as a thumbnailer. }
    if not MakingScreenShot then
      RecentMenu.Add(ASceneURL);

    { We call PrepareResources to make SceneAnimation.PrepareResources to gather
      warnings (because some warnings, e.g. invalid texture URL,
      are reported only from SceneAnimation.PrepareResources).
      Also, this allows us to show first PrepareResources with progress bar. }
    PrepareResources(true);
    WarningsButtonEnabled := true;
    UpdateWarningsButton;
  finally
    FreeAndNil(RootNodes);
    FreeAndNil(Times);
  end;
end;

{ This should be used to load special "clear" and "welcome" scenes.
  This loads a scene directly from TX3DNode, and assumes that
  LoadSceneCore will not fail. }
procedure LoadSimpleScene(Node: TX3DNode;
  UseInitialVars: boolean = true);
var
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;
begin
  RootNodes := TX3DNodeList.Create(false);
  Times := TSingleList.Create;
  try
    RootNodes.Add(Node);
    Times.Add(0);

    ScenesPerTime := 1;      { doesn't matter }
    EqualityEpsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }

    SceneWarnings.Clear;
    LoadSceneCore(
      RootNodes, Times,
      ScenesPerTime,
      EqualityEpsilon,
      TimeLoop, TimeBackwards,
      '', [], 1.0, true,
      UseInitialVars);
  finally
    FreeAndNil(RootNodes);
    FreeAndNil(Times);
  end;
end;

{ This works like LoadScene, but loaded scene is an empty scene.
  More specifically, this calls FreeScene, and then inits
  "scene global variables" to some non-null values. }
procedure LoadClearScene;
begin
  { As a clear scene, I'm simply loading an empty 3D model.
    This way everything seems normal: SceneAnimation is Loaded,
    FirstScene is available and FirstScene.RootNode is non-nil.

    The other idea was to use some special state like Loaded = @false
    to indicate clear scene, but this would only complicate code
    with checks for "if Loaded" everywhere.

    Also, non-empty clear scene allows me to put there WorldInfo with a title.
    This way clear scene has an effect on view3dscene window's title,
    and at the same time I don't have to set SceneURL to something dummy.

    I'm not constructing here RootNode in code (i.e. Pascal).
    This would allow a fast implementation, but it's easier for me to
    design scene in pure VRML/X3D and then auto-generate
    xxx_scene.inc file to load scene from a simple string. }
  LoadSimpleScene(LoadX3DClassicFromString({$I clear_scene.inc}, ''), false);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
begin
  LoadSimpleScene(LoadX3DClassicFromString({$I welcome_scene.inc}, ''));
end;

const
  SaveGenerator = 'view3dscene, http://castle-engine.sourceforge.net/view3dscene.php';

{ Load model from ASceneURL ('-' means stdin),
  do SceneChanges, and write it as VRML/X3D to stdout.
  This is used to handle --write command-line option. }
procedure WriteModel(const ASceneURL: string;
  const SceneChanges: TSceneChanges; const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean);
var
  Node: TX3DRootNode;
begin
  Node := Load3D(ASceneURL, true);
  try
    ChangeNode(SceneChanges, Node);
    Save3D(Node, StdOutStream, SaveGenerator,
      ExtractURIName(ASceneURL), Encoding, ForceConvertingToX3D);
  finally FreeAndNil(Node) end;
end;

class procedure THelper.OpenRecent(const URL: string);
begin
  LoadScene(URL, [], 0.0, true);
end;

procedure DropFiles(Container: TUIContainer; const FileNames: array of string);
var
  URL: string;
begin
  if High(FileNames) >= 0 then
  begin
    URL := FilenameToURISafe(FileNames[0]);
    if URL <> '' then
      LoadScene(URL, [], 0.0, true);
  end;
end;

class procedure THelper.GeometryChanged(Scene: TCastleSceneCore;
  const SomeLocalGeometryChanged: boolean;
  OnlyShapeChanged: TShape);
begin
  if SomeLocalGeometryChanged then
  begin
    if (OnlyShapeChanged = nil) or
       (OnlyShapeChanged = SelectedShape) then
      { Our PTriangle pointer is possibly invalid now. }
      Unselect;
  end else
  if SelectedItem <> nil then
  begin
    { We can keep SelectedItem, but we have to take into account that it's
      transformation possibly changed. So world coordinates of this triangle
      are different. }
    SelectedItem^.UpdateWorld;

    { Also SelectedPointWorld changed now. To apply the change, convert
      SelectedPointLocal to world coords by new trasform.
      This is the main reason why we keep SelectedPointLocal recorded. }
    try
      SelectedPointWorld := MatrixMultPoint(SelectedItem^.State.Transform,
        SelectedPointLocal);
    except
      on ETransformedResultInvalid do
        Unselect;
    end;

  end;
end;

class procedure THelper.WarningsButtonClick(Sender: TObject);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Append(Format('Total %d warnings about current scene "%s":',
      [ SceneWarnings.Count, SceneURL ]));
    S.Append('');
    S.AddStrings(SceneWarnings.Items);
    S.Append('');
    S.Append('You can always see the console or use File->"View warnings" menu command to view these warnings again.');
    MessageOK(Window, S);
    WarningsButtonEnabled := false;
    UpdateWarningsButton;
  finally FreeAndNil(S) end;
end;

procedure AttributesLoadFromConfig(Attributes: TRenderingAttributes);
begin
  Attributes.LineWidth := Config.GetFloat('video_options/line_width',
    Attributes.DefaultLineWidth);
  Attributes.PointSize := Config.GetFloat('video_options/point_size',
    Attributes.DefaultPointSize);
end;

procedure AttributesSaveToConfig(Attributes: TRenderingAttributes);
begin
  Config.SetDeleteFloat('video_options/line_width',
    Attributes.LineWidth, Attributes.DefaultLineWidth);
  Config.SetDeleteFloat('video_options/point_size',
    Attributes.PointSize, Attributes.DefaultPointSize);
end;

{ make screen shots ---------------------------------------------------------- }

function RenderAndSaveScreen(ImageClass: TCastleImageClass;
  const ReadBuffer: TColorBuffer): TCastleImage;
begin
  if ControlsOnScreenshot then
  begin
    Window.Container.EventRender;
  end else
  begin
    ViewportsRender;
    SceneManager.Render;
  end;
  Result := SaveScreen_NoFlush(ImageClass, Window.Rect, ReadBuffer);
end;

{ This performs all screenshot takes, as specified in ScreenShotsList.
  It is used both for batch mode screenshots (--screenshot, --screenshot-range)
  and interactive (menu items about screenshots) operation. }
procedure MakeAllScreenShots(ReadBuffer: TColorBuffer);
var
  I, J: Integer;
  OldProgressUserInterface: TProgressUserInterface;
  OldTime: TFloatTime;
  Image: TCastleImage;
begin
  { Save global things that we change, to restore them later.
    This isn't needed for batch mode screenshots, but it doesn't hurt
    to be clean. }
  OldProgressUserInterface := Progress.UserInterface;
  OldTime := SceneAnimation.Time;
  try
    SceneManager.BeforeRender;

    { For TRangeScreenShot to display progress on console
      (it cannot display progress on GL window, since this would
      mess rendered image; besides, in the future GL window may be
      hidden during rendering). }
    Progress.UserInterface := ProgressConsoleInterface;

    ScreenShotsList.BeginCapture;

    for I := 0 to ScreenShotsList.Count - 1 do
    begin
      ScreenShotsList[I].BeginCapture;
      try
        for J := 0 to ScreenShotsList[I].Count - 1 do
        begin
          SceneAnimation.ResetTime(ScreenShotsList[I].UseTime(J));
          Image := RenderAndSaveScreen(TRGBImage, ReadBuffer);
          try
            SaveImage(Image, ScreenShotsList[I].UseURL(J));
          finally FreeAndNil(Image) end;
        end;
        ScreenShotsList[I].EndCapture(true);
      except
        ScreenShotsList[I].EndCapture(false);
        raise;
      end;
    end;

  finally
    Progress.UserInterface := OldProgressUserInterface;
    SceneAnimation.ResetTime(OldTime);
  end;
end;

{ Make all the screenshots, preferably using FBO.
  This allows the window to be hidden. }
procedure MakeAllScreenShotsFBO;
var
  ScreenshotRender: TGLRenderToTexture;
begin
  ScreenshotRender := TGLRenderToTexture.Create(Window.Width, Window.Height);
  try
    ScreenshotRender.Buffer := tbNone;
    ScreenshotRender.GLContextOpen;
    ScreenshotRender.RenderBegin;
    MakeAllScreenShots(ScreenshotRender.ColorBuffer);
    ScreenshotRender.RenderEnd;
  finally FreeAndNil(ScreenshotRender) end;
end;

{ TGamePlaceholdersRemover --------------------------------------------------- }

type
  TGamePlaceholdersRemover = class
    Count: Cardinal;
    procedure Remove(ParentNode: TX3DNode; var Node: TX3DNode);
  end;

procedure TGamePlaceholdersRemover.Remove(
  ParentNode: TX3DNode; var Node: TX3DNode);

  function IsPlaceholder(const Prefix: string): boolean;
  begin
    Result := IsPrefix(Prefix, Node.NodeName) or
              IsPrefix('OB_' + Prefix, Node.NodeName);
  end;

begin
  if IsPlaceholder('CasMoveLimit') or
     IsPlaceholder('CasWater') or
     IsPlaceholder('CasRes') or
     IsPlaceholder('CasWaypoint') or
     IsPlaceholder('CasSector') or
     { Below are special only on specific castle1 levels, see GameLevelSpecific.
       For historical reasons, they don't use 'Cas' prefix. }
     (Node.NodeName = 'LevelExitBox') or
     IsPrefix('WerewolfAppear_', Node.NodeName) or
     (Node.NodeName = 'GateExitBox') or
     (Node.NodeName = 'Teleport1Box') or
     (Node.NodeName = 'Teleport2Box') or
     (Node.NodeName = 'SacrilegeBox') or
     IsPrefix('SacrilegeGhost_', Node.NodeName) or
     IsPrefix('SwordGhost_', Node.NodeName) or
     (Node.NodeName = 'Elevator49DownBox') or
     (Node.NodeName = 'Elev9a9bPickBox') then
  begin
    Node := nil;
    Inc(Count);
  end;
end;

{ menu things ------------------------------------------------------------ }

const
  DisplayApplicationName = 'view3dscene';

procedure UpdateStatusToolbarVisible; forward;

procedure ScreenShotImage(const Caption: string; const Transparency: boolean);

  function CaptureScreen: TCastleImage;
  var
    ReadBuffer: TColorBuffer;
    Fbo: TGLRenderToTexture;
    ImageClass: TCastleImageClass;
  begin
    Assert(Window.DoubleBuffer); { view3dscene always has double buffer }

    if Transparency then
    begin
      Fbo := TGLRenderToTexture.Create(Window.Width, Window.Height);
      Fbo.Buffer := tbNone;
      Fbo.ColorBufferAlpha := true;
      Fbo.GLContextOpen;
      Fbo.RenderBegin;
      ReadBuffer := Fbo.ColorBuffer;
      ImageClass := TRGBAlphaImage;
      Inc(DisableBackground);

      if glGetInteger(GL_ALPHA_BITS) = 0 then
        { In case FBO is not available, and main context doesn't have alpha
          bits either. }
        OnWarning(wtMinor, 'OpenGL', 'We did not manage to create a render buffer with alpha channel. This means that screenshot will not capture the transparency. You need a better GPU for this to work.');
    end else
    begin
      ReadBuffer := Window.SaveScreenBuffer;
      ImageClass := TRGBImage;
    end;

    try
      Window.Container.EventBeforeRender;
      Result := RenderAndSaveScreen(ImageClass, ReadBuffer);
    finally
      if Transparency then
      begin
        Fbo.RenderEnd;
        FreeAndNil(Fbo);
        Dec(DisableBackground);
      end;
    end;
  end;

var
  ScreenShotName: string;
  Image: TCastleImage;
begin
  { Note that we capture screen *first*, and ask about where to save it later.
    That's because in some situations we capture directly from the back buffer,
    without FBO, and this means that our main window cannot be covered by
    another window. Capturing right after Window.FileDialog at least under Linux
    may mean that OpenGL context underneath the dialog is not really updated
    (so we capture controls (toolbar, lights editor etc.) underneath the dialog). }

  Image := CaptureScreen;
  try
    if SceneURL <> '' then
      ScreenShotName := ChangeURIExt(ExtractURIName(SceneURL), '_%d.png') else
      ScreenShotName := 'view3dscene_screen_%d.png';
    ScreenShotName := FileNameAutoInc(ScreenShotName);
    { Below is a little expanded version of TCastleWindowCustom.SaveScreenDialog.
      Expanded, to allow Transparency: boolean parameter,
      that in turn causes FBO rendering (as we need alpha channel in color buffer). }
    if Window.FileDialog(Caption, ScreenShotName, false, SaveImage_FileFilters) then
    try
      SaveImage(Image, ScreenShotName);
    except
      on E: Exception do Window.MessageOK('Unable to save screen: ' + E.Message, mtError);
    end;
  finally FreeAndNil(Image) end;
end;

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);

  procedure ChangeGravityUp;
  var Answer: string;
      NewUp: TVector3Single;
  begin
   if Camera.NavigationClass = ncWalk then
   begin
    Answer := '';
    if MessageInputQuery(Window,
      'Input new camera up vector (three float values).' +nl+nl+
      'This vector will be used as new gravity upward vector. ' +
      'This vector must not be zero vector.', Answer) then
    begin

     try
      NewUp := Vector3SingleFromStr(Answer);
     except
      on E: EConvertError do
      begin
       MessageOK(Window, 'Incorrect vector value : '+E.Message);
       Exit;
      end;
     end;

     Camera.Walk.GravityUp := NewUp;
     Window.Invalidate;
    end;
   end else
    MessageOK(Window, SNavigationClassWalkNeeded);
  end;

  procedure ChangeMoveSpeed;
  var
    MoveSpeed: Single;
  begin
    if Camera.NavigationClass = ncWalk then
    begin
      MoveSpeed := Camera.Walk.MoveSpeed;
      if MessageInputQuery(Window, 'New move speed (units per second):', MoveSpeed) then
      begin
        Camera.Walk.MoveSpeed := MoveSpeed;
        Window.Invalidate;
      end;
    end else
      MessageOK(Window, SNavigationClassWalkNeeded);
  end;

  procedure ShowAndWrite(const S: string);
  begin
    Writeln(S);
    MessageOK(Window, S);
  end;

  procedure ChangePointSize;
  var
    Value: Single;
  begin
    Value := SceneAnimation.Attributes.PointSize;
    if MessageInputQuery(Window, 'Change point size:', Value) then
      SceneAnimation.Attributes.PointSize := Max(Value, 0.01);
  end;

  procedure ChangeLineWidth;
  var
    Value: Single;
  begin
    Value := SceneAnimation.Attributes.LineWidth;
    if MessageInputQuery(Window, 'Change line width:', Value) then
      SceneAnimation.Attributes.LineWidth := Max(Value, 0.01);
  end;

  procedure ChangeAnimationTimeSpeed;
  var
    S: Single;
  begin
    S := SceneAnimation.TimePlayingSpeed;
    if MessageInputQuery(Window,
      'Playing speed 1.0 means that 1 time unit is 1 second.' +nl+
      '0.5 makes playing animation two times slower,' +nl+
      '2.0 makes it two times faster etc.' +nl+
      nl+
      'Note that this is the "on display" playing speed.' +nl+
      nl+
      '- For pracalculated ' +
      'animations (like from Kanim or MD3 files), this means ' +
      'that internally number of precalculated animation frames ' +
      'doesn''t change. Which means that slowing this speed too much ' +
      'leads to noticeably "jagged" animations.' +nl+
      nl+
      '- For interactive animations (played and calculated from a single ' +
      'VRML / X3D file, e.g. by interpolators) this is perfect, ' +
      'animation always remains smooth.' +nl+
      nl+
      'New "on display" playing speed:', S) then
      SceneAnimation.TimePlayingSpeed := S;
  end;

  procedure ChangeAnimationTimeSpeedWhenLoading;
  begin
    MessageInputQuery(Window,
      'Playing speed 1.0 means that 1 time unit is 1 second.' +nl+
      '0.5 makes playing animation two times slower,' +nl+
      '2.0 makes it two times faster etc.' +nl+
      nl+
      'Note that this is the "on loading" playing speed. Which means ' +
      'it''s only applied when loading animation from file ' +
      '(you can use "File -> Reopen" command to apply this to currently ' +
      'loaded animation).' +nl+
      nl+
      '- For pracalculated ' +
      'animations (like from Kanim or MD3 files), changing this actually changes ' +
      'the density of precalculated animation frames. Which means that ' +
      'this is the more resource-consuming, but also better ' +
      'method of changing animation speed: even if you slow down ' +
      'this playing speed much, the animation will remain smooth.' +nl+
      nl+
      '- For interactive animations (played and calculated from a single ' +
      'VRML / X3D file, e.g. by interpolators) this has no effect, ' +
      'as no frames are precalculated at loading. Use "on display" playing speed ' +
      'instead.' +nl+
      nl+
      'New "on loading" playing speed:', AnimationTimeSpeedWhenLoading);
  end;

  procedure SelectedShowInformation;
  var
    s, TextureDescription: string;
    VCOver, TCOver, VCNotOver, TCNotOver: Cardinal;
    M1: TMaterialNode_1;
    M2: TMaterialNode;
    SelectedShape: TShape;
    SelectedGeometry: TAbstractGeometryNode;
    Tex: TAbstractTextureNode;
  begin
    if SelectedItem = nil then
    begin
      s := 'Nothing selected.';
    end else
    begin
      SelectedShape := TShape(SelectedItem^.Shape);
      SelectedGeometry := SelectedShape.Geometry;
      s := Format(
           'Selected point %s from triangle %s (triangle id: %s).' +nl+
           nl+
           'This triangle is part of the '+
           'node named "%s" (original type %s, rendered through type %s). Node''s bounding box is %s. ',
           [VectorToNiceStr(SelectedPointWorld),
            TriangleToNiceStr(SelectedItem^.World.Triangle),
            PointerToStr(SelectedItem),
            SelectedGeometry.NodeName,
            SelectedShape.OriginalGeometry.NodeTypeName,
            SelectedGeometry.NodeTypeName,
            SelectedShape.BoundingBox.ToNiceStr]);

      if (SelectedItem^.Face.IndexBegin <> -1) and
         (SelectedItem^.Face.IndexEnd <> -1) then
      begin
        S += Format('Face containing the selected triangle spans from %d to' +
          ' %d coordIndex entries. ',
          [ SelectedItem^.Face.IndexBegin,
            SelectedItem^.Face.IndexEnd ]);
      end;

      VCNotOver := SelectedShape.VerticesCount(false);
      TCNotOver := SelectedShape.TrianglesCount(false);
      VCOver := SelectedShape.VerticesCount(true);
      TCOver := SelectedShape.TrianglesCount(true);

      if (VCOver = VCNotOver) and (TCOver = TCNotOver) then
      begin
       s += Format(
              'Node has %d vertices and %d triangles '+
              '(with and without over-triangulating).',
              [VCNotOver, TCNotOver]);
      end else
      begin
       s += Format(
              'When we don''t use over-triangulating (e.g. for raytracing and '+
              'collision-detection) node has %d vertices and %d triangles. '+
              'When we use over-triangulating (e.g. for real-time rendering) '+
              'node has %d vertices and %d triangles.',
              [VCNotOver, TCNotOver, VCOver, TCOver]);
      end;

      { calculate Tex }
      Tex := SelectedItem^.State.Texture;

      { calculate TextureDescription }
      if Tex = nil then
        TextureDescription := 'none' else
        TextureDescription := Tex.TextureDescription;

      S += Format(nl +nl+ 'Node''s texture : %s.', [TextureDescription]);

      S += nl+ nl;
      if SelectedItem^.State.ShapeNode <> nil then
      begin
        { This is VRML 2.0 / X3D node }
        M2 := SelectedItem^.State.ShapeNode.Material;
        if M2 <> nil then
        begin
          S += Format(
                 'Material (VRML 2.0 / X3D):' +nl+
                 '  name : %s' +nl+
                 '  ambientIntensity : %s' +nl+
                 '  diffuseColor : %s' +nl+
                 '  specular : %s' +nl+
                 '  shininess : %s' +nl+
                 '  transparency : %s',
                 [ M2.NodeName,
                   FloatToNiceStr(M2.FdAmbientIntensity.Value),
                   VectorToNiceStr(M2.FdDiffuseColor.Value),
                   VectorToNiceStr(M2.FdSpecularColor.Value),
                   FloatToNiceStr(M2.FdShininess.Value),
                   FloatToNiceStr(M2.FdTransparency.Value) ]);
        end else
          S += 'Material: NULL';
      end else
      begin
        M1 := SelectedItem^.State.LastNodes.Material;
        S += Format(
            'Material (VRML 1.0 / Inventor):' +nl+
            '  name : %s' +nl+
            '  ambientColor[0] : %s' +nl+
            '  diffuseColor[0] : %s' +nl+
            '  specularColor[0] : %s' +nl+
            '  shininess[0] : %s' +nl+
            '  transparency[0] : %s',
            [ M1.NodeName,
              VectorToNiceStr(M1.AmbientColor3Single(0)),
              VectorToNiceStr(M1.DiffuseColor3Single(0)),
              VectorToNiceStr(M1.SpecularColor3Single(0)),
              FloatToNiceStr(M1.Shininess(0)),
              FloatToNiceStr(M1.Transparency(0)) ]);
      end;
    end;
    ShowAndWrite(S);
  end;

  procedure SelectedShowLightsInformation;
  var
    i: integer;
    ShadowingItem: PTriangle;
    S: string;
    Lights: TLightInstancesList;
    C: Integer;
  begin
    if SelectedItem = nil then
    begin
      s := 'Nothing selected.';
    end else
    begin
      Lights := SelectedItem^.State.Lights;

      if Lights <> nil then
        C := Lights.Count else
        C := 0;

      S := Format('Total %d lights active on selected object:', [C]);

      if Lights <> nil then
        for i := 0 to Lights.Count - 1 do
        begin
         s += NL + NL + Format('Light %d: %s: ',
           [ I, Lights.L[i].Node.NiceName ]);

         ShadowingItem := SceneOctreeCollisions.SegmentCollision(
           SelectedPointWorld, Lights.L[i].Location,
             false, SelectedItem, true, nil);

         if ShadowingItem <> nil then
         begin
          s += Format('no, this light is blocked by triangle %s from shape %s.',
            [ TriangleToNiceStr(ShadowingItem^.World.Triangle),
              TShape(ShadowingItem^.Shape).NiceName ])
         end else
          s += 'yes, no object blocks this light, it shines on selected point.';
        end;
    end;

    ShowAndWrite(S);
  end;

  procedure RemoveSelectedShape;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.Geometry, so this is only for the frame where
        octree is available. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).');
      Exit;
    end;

    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
    end else
    begin
      Scene.RemoveShape(TShape(SelectedItem^.Shape));
    end;
  end;

  procedure RemoveSelectedFace;

    function MFNonEmpty(Field: TLongIntList): boolean;
    begin
      Result := (Field <> nil) and (Field.Count > 0) and
        { Single "-1" value in an MF field is the VRML 1.0 default
          weird value for normalIndex, materialIndex and textureCoordIndex
          fields. We treat it like an empty field, otherwise we wouldn't
          be able to process most VRML 1.0 files. }
        (not ((Field.Count = 1) and (Field[0] = -1)));
    end;

  var
    Geometry: TAbstractGeometryNode;
    Colors, Coords, Materials, Normals, TexCoords: TLongIntList;
    CoordsField, TexCoordsField: TMFLong;
    IndexBegin, IndexCount: Integer;
  begin
    { TODO: for now, we work with OriginalGeometry.
      So it doesn't work on Cone, Cylinder etc. that are converted
      to IndexedFaceSet in Proxy. Reason: well, after changing the node
      the proxy is recreated, so any changes to it are lost. }

    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.OriginalGeometry, so this is only for the frame where
        octree is available. Moreover, we call
        Scene.ChangedField. }
      MessageOK(Window, 'This function is not available when you deal with precalculated animations (like from Kanim or MD3 files).');
      Exit;
    end;

    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
      Exit;
    end;

    if (SelectedItem^.Face.IndexBegin = -1) or
       (SelectedItem^.Face.IndexEnd = -1) then
    begin
      MessageOK(Window, 'The selected triangle is not part of IndexedFaceSet or IndexedTriangleSet node.');
      Exit;
    end;

    Geometry := TShape(SelectedItem^.Shape).OriginalGeometry;

    if Geometry is TIndexedFaceSetNode_1 then
    begin
      Colors := nil;
      CoordsField := TIndexedFaceSetNode_1(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := TIndexedFaceSetNode_1(Geometry).FdMaterialIndex.Items;
      Normals := TIndexedFaceSetNode_1(Geometry).FdNormalIndex.Items;
      TexCoordsField := TIndexedFaceSetNode_1(Geometry).FdTextureCoordIndex;
      TexCoords := TexCoordsField.Items;
    end else
    if Geometry is TIndexedFaceSetNode then
    begin
      Colors := TIndexedFaceSetNode(Geometry).FdColorIndex.Items;
      CoordsField := TIndexedFaceSetNode(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := nil;
      Normals := TIndexedFaceSetNode(Geometry).FdNormalIndex.Items;
      TexCoordsField := TIndexedFaceSetNode(Geometry).FdTexCoordIndex;
      TexCoords := TexCoordsField.Items;
    end else
    if Geometry is TIndexedTriangleSetNode then
    begin
      Colors := nil; { colorIndex not available, index is used }
      CoordsField := TIndexedTriangleSetNode(Geometry).FdIndex;
      Coords := CoordsField.Items;
      Materials := nil;
      Normals := nil; { normalIndex not available, index is used }
      TexCoordsField := nil; { texCoordIndex not available, index is used }
      TexCoords := nil;
    end else
    if Geometry is TIndexedTriangleMeshNode_1 then
    begin
      Colors := nil;
      CoordsField := TIndexedTriangleMeshNode_1(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := TIndexedTriangleMeshNode_1(Geometry).FdMaterialIndex.Items;
      Normals := TIndexedTriangleMeshNode_1(Geometry).FdNormalIndex.Items;
      TexCoordsField := TIndexedTriangleMeshNode_1(Geometry).FdTextureCoordIndex;
      TexCoords := TexCoordsField.Items;
    end else
    begin
      MessageOK(Window, Format('Cannot remove faces from "%s" node.', [Geometry.NodeTypeName]));
      Exit;
    end;

    if MFNonEmpty(Colors) or MFNonEmpty(Materials) or MFNonEmpty(Normals) then
    begin
      MessageOK(Window, 'Removing faces from a geometry node with colorIndex, materialIndex or normalIndex not implemented yet.');
      Exit;
    end;

    Inc(DisableAutoDynamicGeometry);
    try
      { calculate IndexBegin and IndexCount. Remember that after
        1st call, SelectedItem pointer may become invalid. }
      IndexBegin := SelectedItem^.Face.IndexBegin;
      IndexCount := SelectedItem^.Face.IndexEnd -
                    SelectedItem^.Face.IndexBegin + 1;

      Coords.DeleteRange(IndexBegin, IndexCount);
      Scene.ChangedField(CoordsField);

      { Texture coordinates, if not empty, have always (both in VRML 1.0
        and VRML 2.0 / X3D IndexedFaceSet nodes, and in IndexedTriangleMesh
        from Inventor) the same ordering as coordIndex.
        So we can remove equivalent texture coords in the same manner
        as we removed coords. }
      if TexCoords <> nil then
      begin
        TexCoords.DeleteRange(IndexBegin, IndexCount);
        Scene.ChangedField(TexCoordsField);
      end;
    finally Dec(DisableAutoDynamicGeometry) end;
  end;

  { Returns @true and sets M1 and M2 (exactly one to @nil, one to non-nil)
    if success. Produces message to user and returns @false on failure.

    Note that SelectedItem is not necessarily correct anymore. Use only
    M1 and M2 pointers after this. }
  function ChangeMaterialInit(
    out M1: TMaterialNode_1;
    out M2: TMaterialNode): boolean;
  var
    Shape: TAbstractShapeNode;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem.State, so this is only for the frame where
        octree is available. Moreover, we call
        Scene.ChangedField. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).');
      Exit(false);
    end;

    if (SelectedItem = nil) then
    begin
      ShowAndWrite('Nothing selected.');
      Exit(false);
    end;

    M1 := nil;
    M2 := nil;
    Shape := SelectedItem^.State.ShapeNode;
    if Shape <> nil then
    begin
      M2 := Shape.Material;
      if M2 = nil then
      begin
        if MessageYesNo(Window, 'No material present. Add material to this node and then edit it?') then
        begin
          { Note that this may remove old Shape.FdAppearance.Value,
            but only if Shape.Appearance = nil, indicating that
            something wrong was specified for "appearance" field.

            Similar, it may remove old Shape.Appearance.FdMaterial.Value,
            but only if Shape.Material was nil, and together
            this indicates that something incorrect was placed in "material"
            field. }

          M2 := TMaterialNode.Create('', Shape.BaseUrl);
          Shape.Material := M2;
          Scene.ChangedAll;
        end else
          Exit(false);
      end;
    end else
    begin
      M1 := SelectedItem^.State.LastNodes.Material;
    end;

    Result := true;
  end;

  procedure ChangeMaterialDiffuse;
  var
    M1: TMaterialNode_1;
    M2: TMaterialNode;
    Color: TVector3Single;
  begin
    if not ChangeMaterialInit(M1, M2) then Exit;

    if M2 <> nil then
      Color := M2.FdDiffuseColor.Value else
    begin
      Assert(M1 <> nil);
      if M1.FdDiffuseColor.Count > 0 then
        Color := M1.FdDiffuseColor.Items.L[0] else
        Color := DefaultMaterialDiffuseColor;
    end;

    if Window.ColorDialog(Color) then
    begin
      if M2 <> nil then
      begin
        M2.FdDiffuseColor.Send(Color);
      end else
      begin
        Assert(M1 <> nil);
        M1.FdDiffuseColor.Send([Color]);
      end;
    end;
  end;

  procedure ChangeMaterialSpecular;
  var
    M1: TMaterialNode_1;
    M2: TMaterialNode;
    Color: TVector3Single;
  begin
    if not ChangeMaterialInit(M1, M2) then Exit;

    if M2 <> nil then
      Color := M2.FdSpecularColor.Value else
    begin
      Assert(M1 <> nil);
      if M1.FdSpecularColor.Count > 0 then
        Color := M1.FdSpecularColor.Items.L[0] else
        Color := DefaultMaterialSpecularColor;
    end;

    if Window.ColorDialog(Color) then
    begin
      if M2 <> nil then
      begin
        M2.FdSpecularColor.Send(Color);
      end else
      begin
        Assert(M1 <> nil);
        M1.FdSpecularColor.Send([Color]);
      end;
    end;
  end;

  procedure LoadMaterialProperties;
  var
    URL: string;
  begin
    URL := ExtractURIPath(SceneURL);
    if Window.FileDialog('Open material_properties.xml file', URL, true,
      'All Files|*|*XML files|*.xml') then
    try
      MaterialProperties.URL := URL;
    except
      on E: Exception do MessageOK(Window,
        'Error while loading material properties: ' + E.Message);
    end;
  end;

  procedure CleanMaterialProperties;
  begin
    MaterialProperties.URL := '';
  end;

  procedure ChangeLightModelAmbient;
  var
    C: TVector3Single;
  begin
    C := GlobalAmbient;
    if Window.ColorDialog(C) then
      GlobalAmbient := C;
  end;

  procedure SetViewpointForWholeScene(
    const WantedDirection, WantedUp: Integer;
    const WantedDirectionPositive, WantedUpPositive: boolean);
  var
    Position, Direction, Up, GravityUp: TVector3Single;
  begin
    CameraViewpointForWholeScene(SceneAnimation.BoundingBox,
      WantedDirection, WantedUp,
      WantedDirectionPositive, WantedUpPositive,
      Position, Direction, Up, GravityUp);
    Scene.CameraTransition(Camera, Position, Direction, Up, GravityUp);
  end;

  procedure RemoveNodesWithMatchingName;
  var
    Wildcard: string;
    RemovedNumber, RemovedNumberOther: Cardinal;
    I: Integer;
  begin
    Wildcard := '';
    if MessageInputQuery(Window,
      'Input node name to be removed. You can use wildcards (* and ?) in ' +
      'the expression below to match many node names. The input is ' +
      'case sensitive (like all VRML/X3D).', Wildcard) then
    begin
      SceneAnimation.BeforeNodesFree;

      RemovedNumber := Scene.RootNode.
        RemoveChildrenWithMatchingName(Wildcard, false);
      for I := 1 to SceneAnimation.ScenesCount - 1 do
      begin
        RemovedNumberOther := SceneAnimation.Scenes[I].RootNode.
          RemoveChildrenWithMatchingName(Wildcard, false);
        Assert(RemovedNumberOther = RemovedNumber);
      end;

      SceneAnimation.ChangedAll;

      MessageOK(Window, Format('Removed %d node instances.', [RemovedNumber]));
    end;
  end;

  { Remove special "stub" nodes, for castle creatures, items etc.
    This is purely for testing purposes (to view castle levels
    in view3dscene), in actual game you want to remove them more intelligently
    (actually adding creatures, items, etc. at designated places). }
  procedure RemoveGamePlaceholders;
  var
    R: TGamePlaceholdersRemover;
  begin
    if SceneAnimation.ScenesCount <> 1 then
    begin
      MessageOK(Window, 'This is not possible with a precalculated animation (like loaded from Kanim or MD3 file).');
      Exit;
    end;

    R := TGamePlaceholdersRemover.Create;
    try
      SceneAnimation.BeforeNodesFree;
      SceneAnimation.FirstScene.RootNode.EnumerateReplaceChildren(@R.Remove);
      SceneAnimation.ChangedAll;
      MessageOK(Window, Format('Removed %d nodes.', [R.Count]));
    finally FreeAndNil(R) end;
  end;

  procedure PrintRayhunterCommand;
  var
    S: string;
  begin
    S := Format(
       'Call rayhunter like this to render this view :' +nl+
       '  rayhunter classic %d %d %d "%s" "%s" \' +nl+
       '    --camera-pos %s \' +nl+
       '    --camera-dir %s \' +nl+
       '    --camera-up %s \' +nl+
       '    --scene-bg-color %f %f %f \' +nl,
       [ DefaultRaytracerDepth,
         Window.Width, Window.Height,
         SceneURL,
         ChangeURIExt(ExtractURIName(SceneURL), '-rt.png'),
         VectorToRawStr(Camera.Walk.Position),
         VectorToRawStr(Camera.Walk.Direction),
         VectorToRawStr(Camera.Walk.Up),
         BGColor[0], BGColor[1], BGColor[2] ]);

    case SceneManager.Projection.ProjectionType of
      ptPerspective:
        S += Format('    --view-angle-x %f',
          [SceneManager.Projection.PerspectiveAngles[0]]);
      ptOrthographic:
        S += Format('    --ortho %f %f %f %f', [
          SceneManager.Projection.OrthoDimensions[0],
          SceneManager.Projection.OrthoDimensions[1],
          SceneManager.Projection.OrthoDimensions[2],
          SceneManager.Projection.OrthoDimensions[3] ]);
      else raise EInternalError.Create('PrintRayhunterCommand:ProjectionType?');
    end;

    Writeln(S);
  end;

  procedure WritelnCameraSettings(const Version: TX3DCameraVersion;
    const Xml: boolean);
  var
    Pos, Dir, Up, GravityUp: TVector3Single;
  begin
    SceneManager.Camera.GetView(Pos, Dir, Up, GravityUp);
    Writeln(MakeCameraStr(Version, Xml, Pos, Dir, Up, GravityUp));
  end;

  procedure WriteBoundingBox(const Box: TBox3D);
  begin
    if Box.IsEmpty then
      MessageOK(Window, 'The bounding box is empty.') else
    begin
      Writeln(Format(
        '# ----------------------------------------' +nl+
        '# BoundingBox %s:' +nl+
        '# Version for VRML 1.0' +nl+
        'DEF BoundingBox Separator {' +nl+
        '  Translation {' +nl+
        '    translation %s' +nl+
        '  }' +nl+
        '  Cube {' +nl+
        '    width %s' +nl+
        '    height %s' +nl+
        '    depth %s' +nl+
        '  } }' +nl+
        nl+
        '# Version for VRML 2.0 / X3D' +nl+
        'DEF BoundingBox Transform {' +nl+
        '  translation %1:s' +nl+
        '  children Shape {' +nl+
        '    geometry Box {' +nl+
        '      size %2:s %3:s %4:s' +nl+
        '    } } }',
        [ Box.ToNiceStr,
          VectorToRawStr(Box.Middle),
          FloatToRawStr(Box.Data[1, 0] - Box.Data[0, 0]),
          FloatToRawStr(Box.Data[1, 1] - Box.Data[0, 1]),
          FloatToRawStr(Box.Data[1, 2] - Box.Data[0, 2]) ]));
    end;
  end;

  procedure SetFillMode(Value: TFillMode);
  begin
    FillMode := Value;
    { For fmSilhouetteBorderEdges, these things can remain as they were
      previously. }
    if FillMode <> fmSilhouetteBorderEdges then
    begin
      SceneAnimation.Attributes.WireframeEffect := FillModes[FillMode].WireframeEffect;
      SceneAnimation.Attributes.WireframeColor  := FillModes[FillMode].WireframeColor;
      SceneAnimation.Attributes.Mode            := FillModes[FillMode].Mode;
    end;
  end;

  procedure ScreenShotToVideo;
  var
    TimeBegin, TimeStep: TFloatTime;
    FramesCount: Cardinal;
    URLPattern: string;
    Range: TRangeScreenShot;
  begin
    TimeBegin := SceneAnimation.Time;
    TimeStep := 0.04;
    FramesCount := 25;
    URLPattern := 'image@counter(4).png';

    if MessageInputQuery(Window, 'Input start time for recording movie:', TimeBegin) then
      if MessageInputQuery(Window, 'Time step between capturing movie frames:' +NL+NL+
        'Note that if you later choose to record to a single movie file, like "output.avi", then we''ll generate a movie with 25 frames per second. ' +
        'So if you want your movie to play with the same speed as animation in view3dscene then the default value, 1/25, is good.' +NL+NL+
        'Input time step between capturing movie frames:', TimeStep) then
        if MessageInputQueryCardinal(Window, 'Input frames count to capture:', FramesCount) then
          if Window.FileDialog('Images pattern or movie file to save', URLPattern, false) then
          begin
            { ScreenShotsList should always be empty in interactive mode
              (otherwise some rendering behaves differently when
              MakingScreenShot = true) }
            Assert(ScreenShotsList.Count = 0);

            Range := TRangeScreenShot.Create;
            Range.TimeBegin := TimeBegin;
            Range.TimeStep := TimeStep;
            Range.FramesCount := FramesCount;
            Range.URLPattern := URLPattern;
            ScreenShotsList.Add(Range);

            try
              MakeAllScreenShots(Window.SaveScreenBuffer);
            except
              on E: EInvalidScreenShotURL do
                MessageOk(Window, 'Making screenshot failed: ' + NL + NL + E.Message);
            end;

            ScreenShotsList.Clear;
          end;
  end;

  procedure PrecalculateAnimationFromEvents;
  var
    ScenesPerTime: Cardinal;
    TimeBegin, TimeEnd: Single;
    RootNode: TX3DRootNode;
  const
    EqualityEpsilon = 0.0001;
  begin
    if SceneAnimation.ScenesCount <> 1 then
    begin
      MessageOK(Window, 'This is not possible when you already have a precalculated animation (like loaded from Kanim or MD3 file).');
      Exit;
    end;

    TimeBegin := 0;
    TimeEnd := 10;
    ScenesPerTime := 25;

    if MessageInputQuery(Window, 'This will "record" an interactive animation (done by VRML/X3D events, interpolators, sensors etc.) into a non-interactive precalculated animation.' + ' This allows an animation to be played ultra-fast, although may also be memory-consuming for long ranges of time.' +nl+
         nl+
         'World BEGIN time of recording:', TimeBegin) and
       MessageInputQuery(Window,
         'World END time of recording:', TimeEnd) and
       MessageInputQueryCardinal(Window,
         'Scenes per second (higher values make animation smoother but also more memory-consuming):', ScenesPerTime) then
    begin
      { Note: there's an inherent problem here since RootNode starts
        with state from current Time. This includes
        TimeDependentNodeHandler state like IsActive, etc., but also
        the rest of VRML/X3D graph (e.g. if some events change some geometry
        or materials). While LoadFromEvents takes care to call
        SceneAnimation.ResetTime, this only resets time-dependent nodes and routes
        and the like, but it cannot at the same time deactivate-and-then-activate
        time-dependent nodes in the same timestamp (so e.g. TimeSensor just
        remains active, if it was active currently and is determined to be
        active during animation, without a pair of Active.Send(false) +
        Active.Send(true)). And it cannot revert whole VRML/X3D graph state.

        This is inherent to the fact that we take current RootNode,
        not the loaded one, so it cannot really be fixed --- we would have
        to just reload RootNode from file, since we cannot keep RootNode
        copy just for this purpose.

        So I just treat it silently as non-fixable in view3dscene,
        you have to load model with ProcessEvents = initially false
        to safely do LoadFromEvents. }

      { Extract RootNode. OwnsFirstRootNode set to false, to avoid
        freeing it when current animation is closed (which is done implicitly
        at the beginning of LoadFromEvents). }
      SceneAnimation.OwnsFirstRootNode := false;
      RootNode := Scene.RootNode;

      { Using LoadFromEvents will also Close the previous scene.
        Before doing this, we must always free our octrees
        (as Scene keeps references to our octrees). }
      SceneOctreeFree;

      { Root node will be owned by LoadFromEvents, so it will be freed }
      SceneAnimation.LoadFromEvents(RootNode, true,
        TimeBegin, TimeEnd, ScenesPerTime, EqualityEpsilon,
        'Precalculating animation');

      { Otherwise, current time is huge and it doesn't work reliably
        with division inside TCastlePrecalculatedAnimation.Scene.
        Do it *before* setting MainScene, as even setting MainScene
        may cause TCastlePrecalculatedAnimation.VisibleChangeNotification, which
        already requires TCastlePrecalculatedAnimation.Scene. }
      SceneAnimation.ResetTimeAtLoad;

      { Closing the scene freed SceneManager.MainScene (it's set to nil
        automagically by free notification). Set it correctly now. }
      SceneManager.MainScene := Scene;

      { Since we just destroyed RootNode, and replaced it with completely
        different scene, we have to recalculate many things.
        Recalculate octree.
        GeometryChanged takes care of invalidating SelectedItem and such. }
      SceneOctreeCreate;
      THelper.GeometryChanged(nil, true, nil);
      THelper.ViewpointsChanged(Scene);
    end;
  end;

  procedure SelectedShapeOctreeStat;
  var
    Shape: TShape;
  begin
    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.');
    end else
    begin
      Shape := TShape(SelectedItem^.Shape);
      if Shape.OctreeTriangles = nil then
        MessageOk(Window, 'No collision octree was initialized for this shape.') else
      begin
        Writeln(Shape.OctreeTriangles.Statistics);
      end;
    end;
  end;

  const
    DefaultCubeMapSize = 256;

  procedure ScreenShotToCubeMap;
  var
    Side: TCubeMapSide;
    CubeMapImg: TCubeMapImages;
    URLPattern: string;
    Orientation: char;
    Size: Cardinal;

    procedure SaveSide(const Image: TCastleImage; const SideName: string);
    begin
      SaveImage(Image, StringReplace(URLPattern, '@side', SideName, [rfReplaceAll]));
    end;

  begin
    Orientation := MessageChoice(Window,
      'This function will save six separate image files that show cube map environment around you.' + NL +
      NL +
      'In a moment you will be asked to choose directory and base filename for saving these images, right now you have to decide how the cube map faces will be oriented and named.' + NL +
      NL +
      '[B] : VRML/X3D Background orientation (left/right/...)' + NL +
      '[O] : OpenGL orientation (positive/negative x/y/z)' + NL +
      '[D] : DirectX (and DDS) orientation (positive/negative x/y/z, in left-handed coord system)',
      ['VRML/X3D Background', 'OpenGL', 'DirectX', 'Cancel (Esc)'],
      ['b', 'o', 'd', CharEscape]);

    if Orientation <> CharEscape then
    begin
      if SceneURL <> '' then
        URLPattern := ChangeURIExt(ExtractURIName(SceneURL), '_cubemap_@side.png') else
        URLPattern := 'view3dscene_cubemap_@side.png';

      if Window.FileDialog('Image name template to save', URLPattern, false) then
      begin
        Size := DefaultCubeMapSize;

        if MessageInputQueryCardinal(Window, 'Size of cube map images', Size) then
        begin
          for Side := Low(Side) to High(Side) do
            CubeMapImg[Side] := TRGBImage.Create(Size, Size);

          GLCaptureCubeMapImages(CubeMapImg, SceneManager.Camera.GetPosition,
            @SceneManager.RenderFromViewEverything,
            SceneManager.Projection.ProjectionNear,
            SceneManager.Projection.ProjectionFar);
          glViewport(Window.Rect);

          case Orientation of
            'b':
              begin
                CubeMapImg[csPositiveX].Rotate(2);
                CubeMapImg[csNegativeX].Rotate(2);
                CubeMapImg[csPositiveZ].Rotate(2);
                CubeMapImg[csNegativeZ].Rotate(2);
                SaveSide(CubeMapImg[csPositiveX], 'right');
                SaveSide(CubeMapImg[csNegativeX], 'left');
                SaveSide(CubeMapImg[csPositiveY], 'top');
                SaveSide(CubeMapImg[csNegativeY], 'bottom');
                SaveSide(CubeMapImg[csPositiveZ], 'back');
                SaveSide(CubeMapImg[csNegativeZ], 'front');
              end;
            'o':
              begin
                { This is the most natural Orientation,
                  our csXxx names match OpenGL names and orientation. }
                SaveSide(CubeMapImg[csPositiveX], 'positive_x');
                SaveSide(CubeMapImg[csNegativeX], 'negative_x');
                SaveSide(CubeMapImg[csPositiveY], 'positive_y');
                SaveSide(CubeMapImg[csNegativeY], 'negative_y');
                SaveSide(CubeMapImg[csPositiveZ], 'positive_z');
                SaveSide(CubeMapImg[csNegativeZ], 'negative_z');
              end;
            'd':
              begin
                { Swap positive/negative y, since DirectX is left-handed. }
                SaveSide(CubeMapImg[csPositiveX], 'positive_x');
                SaveSide(CubeMapImg[csNegativeX], 'negative_x');
                SaveSide(CubeMapImg[csNegativeY], 'positive_y');
                SaveSide(CubeMapImg[csPositiveY], 'negative_y');
                SaveSide(CubeMapImg[csPositiveZ], 'positive_z');
                SaveSide(CubeMapImg[csNegativeZ], 'negative_z');
              end;
            else EInternalError.Create('orient?');
          end;

          for Side := Low(Side) to High(Side) do
            FreeAndNil(CubeMapImg[Side]);
        end;
      end;
    end;
  end;

  procedure ScreenShotToCubeMapDDS;
  var
    DDS: TDDSImage;
    URL: string;
    Size: Cardinal;
  begin
    if SceneURL <> '' then
      URL := ChangeURIExt(ExtractURIName(SceneURL), '_cubemap.dds') else
      URL := 'view3dscene_cubemap.dds';

    if Window.FileDialog('Save image to file', URL, false) then
    begin
      Size := DefaultCubeMapSize;

      if MessageInputQueryCardinal(Window, 'Size of cube map images', Size) then
      begin
        DDS := GLCaptureCubeMapDDS(Size, SceneManager.Camera.GetPosition,
          @SceneManager.RenderFromViewEverything,
          SceneManager.Projection.ProjectionNear,
          SceneManager.Projection.ProjectionFar);
        try
          glViewport(Window.Rect);
          DDS.SaveToFile(URL);
        finally FreeAndNil(DDS) end;
      end;
    end;
  end;

  procedure ScreenShotDepthToImage;

    procedure DoSave(const URL: string);
    var
      PackData: TPackNotAlignedData;
      Image: TGrayscaleImage;
    begin
      { Just like TCastleWindowCustom.SaveScreen, we have to force redisplay now
        (otherwise we could be left here with random buffer contents from
        other window obscuring us, or we could have depth buffer from
        other drawing routine (like "frozen screen" drawn under FileDialog). }
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;

      Image := TGrayscaleImage.Create(Window.Width, Window.Height);
      try
        BeforePackImage(PackData, Image);
        try
          glReadPixels(0, 0, Window.Width, Window.Height, GL_DEPTH_COMPONENT,
            ImageGLType(Image), Image.RawPixels);
        finally AfterPackImage(PackData, Image) end;

        SaveImage(Image, URL);
      finally FreeAndNil(Image) end;
    end;

  var
    URL: string;
  begin
    if SceneURL <> '' then
      URL := ChangeURIExt(ExtractURIName(SceneURL), '_depth_%d.png') else
      URL := 'view3dscene_depth_%d.png';
    URL := FileNameAutoInc(URL);

    if Window.FileDialog('Save depth to a file', URL, false, SaveImage_FileFilters) then
      DoSave(URL);
  end;

  procedure Raytrace;
  var
    Pos, Dir, Up: TVector3Single;
  begin
    SceneManager.Camera.GetView(Pos, Dir, Up);
    RaytraceToWin(SceneManager.BaseLights, Scene,
      Pos, Dir, Up, SceneManager.Projection, BGColor);
  end;

  procedure MergeCloseVertexes;
  var
    Shape: TShape;
    Coord: TMFVec3f;
    MergeDistance: Single;
    MergedCount: Cardinal;
  begin
    { TODO: for now, we work with OriginalGeometry and OriginalState.
      So it doesn't work on Cone, Cylinder etc. that are converted
      to IndexedFaceSet in Proxy. Reason: well, Coord.Changed
      makes TCastleSceneCore.ChangedShapeFields which releases the very Coordinate
      node and fields that were changed... }

    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.Shape.GeometryOriginal, so this is only for the frame where
        octree is available. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).');
      Exit;
    end;

    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.');
      Exit;
    end;

    Shape := TShape(SelectedItem^.Shape);

    if not Shape.OriginalGeometry.Coord(Shape.OriginalState, Coord) then
    begin
      MessageOK(Window, 'Selected geometry node doesn''t have a coordinate field. Nothing to merge.');
      Exit;
    end;

    if Coord = nil then
    begin
      MessageOK(Window, 'Selected geometry node''s has an empty coordinate field. Nothing to merge.');
      Exit;
    end;

    MergeDistance := 0.01;
    if MessageInputQuery(Window, 'Input merge distance. Vertexes closer than this will be set to be exactly equal.',
      MergeDistance, '0.01') then
    begin
      Inc(DisableAutoDynamicGeometry);
      try
        MergedCount := Coord.Items.MergeCloseVertexes(MergeDistance);
        if MergedCount <> 0 then
          Coord.Changed;
      finally Dec(DisableAutoDynamicGeometry) end;
      MessageOK(Window, Format('Merged %d vertexes.', [MergedCount]));
    end;
  end;

  procedure VisualizeHumanoids;
  var
    Vis: THumanoidVisualization;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).');
      Exit;
    end;

    Vis := THumanoidVisualization.Create;
    try
      Vis.JointVisualizationSize := Scene.BoundingBox.AverageSize(false, 1) / 20;
      if MessageInputQuery(Window, 'Joint Visualization Size (default based on scene size):',
        Vis.JointVisualizationSize) then
      begin
        Scene.RootNode.EnumerateNodes(THAnimHumanoidNode,
          @Vis.VisualizeHumanoid, false);
        MessageOK(Window, Format('%d H-Anim Humanoids (%d Joints inside) processed.',
          [Vis.HumanoidsProcessed, Vis.JointsProcessed]));
        if Vis.HumanoidsProcessed <> 0 then
          Scene.ChangedAll;
      end;
    finally FreeAndNil(Vis) end;
  end;

  procedure SaveAs(const Encoding: TX3DEncoding; const MessageTitle: string;
    const ForceConvertingToX3D: boolean);
  var
    ProposedSaveName, Extension, FileFilters: string;
    SaveVersion: TX3DVersion;
    Convertion: boolean;
  begin
    SaveVersion := Save3DVersion(Scene.RootNode);
    Convertion := Save3DWillConvertToX3D(SaveVersion, Encoding, ForceConvertingToX3D);

    if SceneAnimation.ScenesCount > 1 then
      if Convertion then
      begin
        MessageOK(Window, 'This is a precalculated animation (like from Kanim or MD3 file). Converting it from VRML to X3D is not supported.');
        Exit;
      end else
        MessageOK(Window, 'Warning: this is a precalculated animation (like from Kanim or MD3 file). Saving it as VRML/X3D will only save it''s first frame.');

    Extension := SaveVersion.FileExtension(Encoding, ForceConvertingToX3D);
    FileFilters := SaveVersion.FileFilters(Encoding, ForceConvertingToX3D);
    ProposedSaveName := ChangeURIExt(SceneURL, Extension);

    if Window.FileDialog(MessageTitle, ProposedSaveName, false, FileFilters) then
    try
      if Convertion then
        Scene.BeforeNodesFree;

      Save3D(Scene.RootNode, ProposedSaveName, SaveGenerator,
        ExtractURIName(SceneURL), SaveVersion, Encoding, ForceConvertingToX3D);

      if Convertion then
        Scene.ChangedAll;
    except
      on E: Exception do
      begin
        MessageOK(Window, 'Error while saving scene to "' +ProposedSaveName+
          '": ' + E.Message);
      end;
    end;
  end;

  procedure SetLimitFPS;
  var
    F: Single;
  begin
    F := Application.LimitFPS;
    if MessageInputQuery(Window,
      'Set approximate FPS (Frames Per Second) limit.' +NL+
      NL+
      'Smaller values give OS and CPU some rest, to run other applications or conserve laptop battery. Your monitor has a fixed refresh rate anyway, so limiting FPS doesn''t necessarily mean worse visuals (unless you set it to some really small value).' +NL+
      NL+
      'This is only an approximation of desired FPS. Usually it''s quite precise, but on some systems some values may be actually capped by monitor refresh rate.' +NL+
      NL+
      'Special value 0 means "do not limit FPS".',
      F, FloatToNiceStr(F)) then
      Application.LimitFPS := Max(F, 0.0);
  end;

  procedure SwitchScreenSpaceAmbientOcclusion;
  var
    I: Integer;
  begin
    with SceneManager do
      ScreenSpaceAmbientOcclusion := not ScreenSpaceAmbientOcclusion;
    for I := Low(Viewports) to High(Viewports) do
      with Viewports[I] do
        ScreenSpaceAmbientOcclusion := not ScreenSpaceAmbientOcclusion;
  end;

  procedure OpenSceneURL;
  var
    URL, CopyStr, CutStr, PasteStr: string;
  begin
    URL := SceneURL;
    Check(KeyString(CtrlC, K_None, [], CopyStr));
    Check(KeyString(CtrlX, K_None, [], CutStr));
    Check(KeyString(CtrlV, K_None, [], PasteStr));
    if MessageInputQuery(Window,
      'Open 3D model from given URL.' + NL + NL +
      'Note that by default "http" is disabled (because the downloads are blocking for now, and every 3D model may reference additional resources like textures). Enable http by checking "Preferences -> Download Resources From Network".' + NL + NL +
      'You can copy (' + CopyStr +
      '), cut (' + CutStr +
      ') and paste (' + PasteStr +
      ') here, for example to easily paste URL from/to your web browser.' + NL + NL +
      'URL:', URL) then
      LoadScene(URL, [], 0.0, true);
  end;

var
  C: Cardinal;
begin
 case MenuItem.IntData of
  10: THelper.OpenButtonClick(nil);
  11: OpenSceneURL;

  12: Window.Close;

  15: begin
        { When reopening, then InitializeCamera parameter is false.
          In fact, this was the purpose of this InitializeCamera
          parameter: to set it to false when reopening, as this makes
          reopening more useful. }
        LoadScene(SceneURL, [], 0.0, false);
      end;

  900: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), false);
  905: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), true);
  910: SaveAs(xeXML    , SRemoveMnemonics(MenuItem.Caption), true { doesn't matter });

  21: WarningsButton.DoClick;

  31: ChangeAnimation([scNoNormals], SceneAnimation);
  32: ChangeAnimation([scNoSolidObjects], SceneAnimation);
  33: ChangeAnimation([scNoConvexFaces], SceneAnimation);

  34: RemoveNodesWithMatchingName;
  38: RemoveGamePlaceholders;

  42: VisualizeHumanoids;

  3500: with SceneAnimation do ShadowMaps := not ShadowMaps;
  3510..3519: SceneAnimation.Attributes.ShadowSampling :=
    TShadowSampling(Ord(MenuItem.IntData) - 3510);
  3520: with SceneAnimation.Attributes do VisualizeDepthMap := not VisualizeDepthMap;
  3530:
    begin
      C := SceneAnimation.ShadowMapsDefaultSize;
      if MessageInputQueryCardinal(Window, 'Input default shadow map size :' + NL + '(should be a power of 2)', C) then
      begin
        SceneAnimation.ShadowMapsDefaultSize := C;
      end;
    end;

  36: RemoveSelectedShape;
  37: RemoveSelectedFace;

  51: Scene.CameraTransition(Camera,
        DefaultX3DCameraPosition[cvVrml1_Inventor],
        DefaultX3DCameraDirection,
        DefaultX3DCameraUp,
        DefaultX3DGravityUp);
  52: Scene.CameraTransition(Camera,
        DefaultX3DCameraPosition[cvVrml2_X3d],
        DefaultX3DCameraDirection,
        DefaultX3DCameraUp,
        DefaultX3DGravityUp);

  53: SetViewpointForWholeScene(2, 1, false, true);
  54: SetViewpointForWholeScene(2, 1, true , true);
  55: SetViewpointForWholeScene(0, 1, false, true);
  56: SetViewpointForWholeScene(0, 1, true , true);

  57: SetViewpointForWholeScene(0, 2, false, true);
  58: SetViewpointForWholeScene(0, 2, true , true);
  59: SetViewpointForWholeScene(1, 2, false, true);
  60: SetViewpointForWholeScene(1, 2, true , true);

  65: Viewpoints.Initial(SceneManager);
  66: Viewpoints.Previous(SceneManager);
  67: Viewpoints.Next(SceneManager);
  68: Viewpoints.Final(SceneManager);

  82: ShowBBox := not ShowBBox;
  84: if Window.ColorDialog(BGColor) then BGColorChanged(SceneManager);
  86: with SceneAnimation.Attributes do Blending := not Blending;
  88: with SceneAnimation.Attributes do UseOcclusionQuery := not UseOcclusionQuery;
  450: SceneAnimation.Attributes.BlendingSort := bsNone;
  452: SceneAnimation.Attributes.BlendingSort := bs3D;
  454: SceneAnimation.Attributes.BlendingSort := bs2D;
  90: with SceneAnimation.Attributes do UseHierarchicalOcclusionQuery := not UseHierarchicalOcclusionQuery;
  891: with SceneAnimation.Attributes do DebugHierOcclusionQueryResults := not DebugHierOcclusionQueryResults;

  91: with SceneAnimation.Attributes do Lighting := not Lighting;
  92: with Scene do HeadLightOn := not HeadLightOn;
  93: with SceneAnimation.Attributes do UseSceneLights := not UseSceneLights;
  94: with SceneAnimation.Attributes do EnableTextures := not EnableTextures;
  95: ChangeLightModelAmbient;
  96: ShowFrustum := not ShowFrustum;
  180: ShowFrustumAlwaysVisible := not ShowFrustumAlwaysVisible;

  97: OctreeTrianglesDisplay.DoMenuToggleWhole;
  98: OctreeTrianglesDisplay.DoMenuIncDepth;
  99: OctreeTrianglesDisplay.DoMenuDecDepth;

  190: OctreeVisibleShapesDisplay.DoMenuToggleWhole;
  191: OctreeVisibleShapesDisplay.DoMenuIncDepth;
  192: OctreeVisibleShapesDisplay.DoMenuDecDepth;

  195: OctreeCollidableShapesDisplay.DoMenuToggleWhole;
  196: OctreeCollidableShapesDisplay.DoMenuIncDepth;
  197: OctreeCollidableShapesDisplay.DoMenuDecDepth;

  100: SelectedShapeOctreeStat;
  101: if SceneOctreeCollisions <> nil then
         Writeln(SceneOctreeCollisions.Statistics) else
         MessageOk(Window, SOnlyWhenOctreeAvailable);
  103: if SceneOctreeRendering <> nil then
         Writeln(SceneOctreeRendering.Statistics) else
         MessageOk(Window, SOnlyWhenOctreeAvailable);

  105: PrintRayhunterCommand;

  106: WritelnCameraSettings(cvVrml1_Inventor, false);
  107: WritelnCameraSettings(cvVrml2_X3d, false);
  108: WritelnCameraSettings(cvVrml2_X3d, true);

{ Only for debugging:
         Writeln(
           'Current camera frustum planes :' +nl+
           '((A, B, C, D) means a plane given by equation A*x + B*y + C*z + D = 0.)' +nl+
           '  Left   : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpLeft]) +nl+
           '  Right  : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpRight]) +nl+
           '  Bottom : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpBottom]) +nl+
           '  Top    : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpTop]) +nl+
           '  Near   : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpNear]));
         if SceneManager.Camera.Frustum.ZFarInfinity then
           Writeln(
           '  Far    : (No frustum far plane. That is, far plane is "at infinity".)') else
           Writeln(
           '  Far    : ' + VectorToRawStr(SceneManager.Camera.Frustum.Planes[fpFar]));
       end;
}

  109: WriteBoundingBox(SceneAnimation.BoundingBox);
  110: WriteBoundingBox(SceneAnimation.CurrentScene.BoundingBox);

  111: begin
         if Camera.NavigationType = High(TNavigationType) then
           Camera.NavigationType := Low(TNavigationType) else
         begin
           Camera.NavigationType := Succ(Camera.NavigationType);
           { skip over navigation types that are not stable }
           if not (Camera.NavigationType in StableNavigationType) then
             Camera.NavigationType := Succ(Camera.NavigationType);
         end;
         ViewportsSetNavigationType(Camera.NavigationType);
         UpdateCameraUI;
       end;

  121: begin
         ShowAndWrite(
           'Scene "' + SceneURL + '" information:' + NL + NL +
           SceneAnimation.Info(true, true, false));
       end;
  122: begin
         ShowStatus := not ShowStatus;
         UpdateStatusToolbarVisible;
       end;
  123: SetCollisions(not SceneAnimation.Collides, false);
  124: ChangeGravityUp;
  125: Raytrace;
  150: ScreenShotImage(SRemoveMnemonics(MenuItem.Caption), false);
  151: ScreenShotImage(SRemoveMnemonics(MenuItem.Caption), true);
  128: Camera.Walk.MouseLook := not Camera.Walk.MouseLook;
  129: begin
         ShowAndWrite(SceneAnimation.Info(false, false, true));
         SceneAnimation.FreeResources([frManifoldAndBorderEdges]);
       end;

  131: begin
         ShowAndWrite(
           'view3dscene: VRML / X3D browser and full-featured viewer of other 3D models.' +nl+
           'Formats: X3D, VRML 1.0 and 2.0 (aka VRML 97), 3DS, MD3, Wavefront OBJ, Collada.' + NL +
           'Version ' + Version + '.' + NL +
           'By Michalis Kamburelis.' + NL +
           NL +
           '[' + View3dsceneURL + ']' + NL +
           NL +
           'Compiled with ' + SCompilerDescription +'.');
       end;
  132: if not OpenURL(View3dsceneURL) then
         Window.MessageOk(SCannotOpenURL, mtError);
  134: if not OpenURL(DonateURL) then
         Window.MessageOk(SCannotOpenURL, mtError);

  171: SelectedShowInformation;
  172: SelectedShowLightsInformation;
  173: ShowAndWrite(GLInformationString);

  182: ChangePointSize;

  201: begin
         Camera.Walk.Gravity := not Camera.Walk.Gravity;
         UpdateCameraNavigationTypeUI;
       end;
  202: begin
         Camera.Walk.PreferGravityUpForRotations := not Camera.Walk.PreferGravityUpForRotations;
         UpdateCameraNavigationTypeUI;
       end;
  203: begin
         Camera.Walk.PreferGravityUpForMoving := not Camera.Walk.PreferGravityUpForMoving;
         UpdateCameraNavigationTypeUI;
       end;
  205: ChangeMoveSpeed;

  220: begin
         AnimationTimePlaying := not AnimationTimePlaying;
         SceneAnimation.TimePlaying := AnimationTimePlaying and ProcessEventsWanted;
       end;
  221: SceneAnimation.ResetTimeAtLoad(true);
  222: ChangeAnimationTimeSpeed;
  223: ChangeAnimationTimeSpeedWhenLoading;

  224: begin
         ProcessEventsWanted := not ProcessEventsWanted;
         SceneAnimation.TimePlaying := AnimationTimePlaying and ProcessEventsWanted;
         UpdateProcessEvents;
       end;

  225: PrecalculateAnimationFromEvents;

  300: JumpToViewpoint(SceneManager, (MenuItem as TMenuItemViewpoint).Viewpoint);

  340: SwitchScreenSpaceAmbientOcclusion;
  350..370:
    begin
      ScreenEffects.ActiveEffectsRecalculate;
      { All that is needed to actually render with the new effect is to
        actually redisplay. }
      Window.Invalidate;
    end;

  400..419: SceneAnimation.Attributes.BlendingSourceFactor :=
    BlendingFactors[MenuItem.IntData - 400].Value;
  420..439: SceneAnimation.Attributes.BlendingDestinationFactor :=
    BlendingFactors[MenuItem.IntData - 420].Value;

  500..519:
    begin
      SetFillMode(MenuItem.IntData - 500);
      { appropriate Checked will be set automatically }
    end;
  520:
    begin
      SetFillMode((FillMode + 1) mod (High(FillMode) + 1));
      FillModesMenu[FillMode].Checked := true;
    end;

  530: ChangeLineWidth;

  540: ScreenShotToVideo;
  550: ScreenShotToCubeMap;
  555: ScreenShotToCubeMapDDS;
  560: ScreenShotDepthToImage;
  570: ControlsOnScreenshot := not ControlsOnScreenshot;

  600..649: Window.AntiAliasing := TAntiAliasing(MenuItem.IntData - 600);

  710: ChangeMaterialDiffuse;
  720: ChangeMaterialSpecular;
  722: LoadMaterialProperties;
  723: CleanMaterialProperties;
  725: if LightsEditorIsOpen then
         LightsEditorClose else
         LightsEditorOpen(SceneManager, V3DSceneWindow.Window, ToolbarPanel.Height);
  730: MergeCloseVertexes;

  750: ShadowVolumes := not ShadowVolumes;
  760: ShadowVolumesRender := not ShadowVolumesRender;

  770: InitialShowBBox := not InitialShowBBox;
  771: InitialShowStatus := not InitialShowStatus;

  801: SoundEngine.Enable := not SoundEngine.Enable;
  810..850: SoundEngine.Device :=
    SoundEngine.Devices[MenuItem.IntData - 810].Name;

  2000: SetLimitFPS;
  2010: EnableNetwork := not EnableNetwork;

  1100..1199: SetMinificationFilter(
    TMinificationFilter  (MenuItem.IntData-1100), SceneAnimation);
  1200..1299: SetMagnificationFilter(
    TMagnificationFilter  (MenuItem.IntData-1200), SceneAnimation);
  1300..1399:
    begin
      Camera.NavigationType := TNavigationType(MenuItem.IntData - 1300);
      ViewportsSetNavigationType(Camera.NavigationType);
      UpdateCameraUI;
    end;
  1400..1499: SceneAnimation.Attributes.BumpMapping :=
    TBumpMapping(MenuItem.IntData - 1400);
  3600..3610: SetViewportsConfig(TViewportsConfig(MenuItem.IntData - 3600),
    V3DSceneWindow.Window, SceneManager);
  4000..4010: SceneAnimation.Attributes.Shaders :=
    TShadersRendering(MenuItem.IntData - 4000);
  else raise EInternalError.Create('not impl menu item');
 end;

 { This may be called when headlight on / off state changes,
   so prVisibleSceneNonGeometry is possible.
   For safety, pass also prVisibleSceneGeometry now. }
 SceneAnimation.CurrentScene.VisibleChangeHere(
   [vcVisibleGeometry, vcVisibleNonGeometry]);
end;

function CreateMainMenu: TMenu;

  procedure AppendNavigationTypes(M: TMenu);
  var
    Mode: TNavigationType;
    Group: TMenuItemRadioGroup;
  begin
    Group := M.AppendRadioGroup(CameraNames, 1300, Ord(Camera.NavigationType), true);
    for Mode := Low(Mode) to High(Mode) do
      CameraRadios[Mode] := Group[Ord(Mode)];
  end;

  procedure AppendBlendingFactors(M: TMenu; Source: boolean;
    BaseIntData: Cardinal);
  var
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
    I: Cardinal;
    Caption: string;
    IsDefault: boolean;
  begin
    RadioGroup := nil;

    for I := Low(BlendingFactors) to High(BlendingFactors) do
      if (Source and BlendingFactors[I].ForSource) or
         ((not Source) and BlendingFactors[I].ForDestination) then
      begin
        if Source then
          IsDefault := BlendingFactors[I].Value = TSceneRenderingAttributes.DefaultBlendingSourceFactor else
          IsDefault := BlendingFactors[I].Value = TSceneRenderingAttributes.DefaultBlendingDestinationFactor;
        Caption := SQuoteMenuEntryCaption(BlendingFactors[I].Name);
        if IsDefault then
          Caption += ' (Default)';
        Radio := TMenuItemRadio.Create(Caption, BaseIntData + I, IsDefault, true);
        if RadioGroup = nil then
          RadioGroup := Radio.Group else
          Radio.Group := RadioGroup;
        M.Append(Radio);
      end;
  end;

  procedure MenuAppendSoundDevices(M: TMenu; BaseIntData: Cardinal);
  var
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
    I: Integer;
  begin
    { When making screenshot, we will disable sound always.
      Do not try to read SoundEngine.Devices,
      and even this may initialize some OpenAL stuff, printing messages
      about ALSA. }
    if MakingScreenShot then Exit;

    RadioGroup := nil;
    for I := 0 to SoundEngine.Devices.Count - 1 do
    begin
      Radio := TMenuItemRadio.Create(
        SQuoteMenuEntryCaption(SoundEngine.Devices[I].NiceName),
        BaseIntData + I, SoundEngine.Devices[I].Name = SoundEngine.Device, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
    end;
  end;

var
  M, M2, M3: TMenu;
  NextRecentMenuItem: TMenuEntry;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('_Open ...',         10, CtrlO));
    M.Append(TMenuItem.Create('_Open URL ...',     11, CtrlL));
    MenuReopen := TMenuItem.Create('_Reopen',      15);
    MenuReopen.Enabled := false;
    M.Append(MenuReopen);
    M.Append(TMenuItem.Create('_Save As VRML/X3D (classic encoding) ...', 900, CtrlS));
    M.Append(TMenuItem.Create('Save As X3D (_classic encoding; converts VRML to X3D) ...', 905));
    M.Append(TMenuItem.Create('Save As X3D (_XML encoding; converts VRML to X3D) ...', 910));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('View _Warnings About Current Scene', 21));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('_Preferences');
      M3 := TMenu.Create('_Anti Aliasing (Restart view3dscene to Apply)');
        MenuAppendAntiAliasing(M3, 600);
        M2.Append(M3);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItemChecked.Create('Show Bounding Box at Start', 770,
        InitialShowBBox, true));
      M2.Append(TMenuItemChecked.Create('Show Status and Toolbar at Start', 771,
        InitialShowStatus, true));
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItemChecked.Create('Sound', 801, SoundEngine.Enable, true));
      M3 := TMenu.Create('Sound Device');
        MenuAppendSoundDevices(M3, 810);
        M2.Append(M3);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItem.Create('Point Size ...', 182));
      M2.Append(TMenuItem.Create('Line Width ...', 530));
      M2.Append(TMenuItem.Create('Default Background Color ...', 84));
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItem.Create('Frames Per Second Limit ...', 2000));
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItemChecked.Create('Download Resources From Network', 2010,
        EnableNetwork, true));
      M.Append(M2);
    NextRecentMenuItem := TMenuSeparator.Create;
    M.Append(NextRecentMenuItem);
    RecentMenu.NextMenuItem := NextRecentMenuItem;
    M.Append(TMenuItem.Create('_Exit',             12, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('_Bounding Box Visible',      82, CtrlB,
      ShowBBox, true));
    M.Append(TMenuItemChecked.Create('Status and Toolbar Visible',  122, K_F1,
      ShowStatus, true));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('_Fill Mode');
      MenuAppendFillModes(M2, 500);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItem.Create('Next _Fill Mode', 520, CtrlF));
      M.Append(M2);
    M.Append(ScreenEffects.Menu);
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('Shaders');
      M2.AppendRadioGroup(['Disable', 'Enable When Required', 'Enable For Everything'],
        4000, Ord(SceneAnimation.Attributes.Shaders), true);
      M.Append(M2);
    M2 := TMenu.Create('Bump mapping');
      M2.AppendRadioGroup(BumpMappingNames, 1400,
        Ord(SceneAnimation.Attributes.BumpMapping), true);
      M.Append(M2);
    M2 := TMenu.Create('Shadow Maps');
      M2.Append(TMenuItemChecked.Create('Enable', 3500, SceneAnimation.ShadowMaps, true));
      M2.Append(TMenuSeparator.Create);
      M2.AppendRadioGroup(ShadowSamplingNames, 3510,
        Ord(SceneAnimation.Attributes.ShadowSampling), true);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItemChecked.Create('Visualize Depths', 3520, SceneAnimation.Attributes.VisualizeDepthMap, true));
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItem.Create('Set Default Shadow Map Size ...', 3530));
      M.Append(M2);
    M2 := TMenu.Create('Shadow Volumes');
      M2.Append(TMenuItemChecked.Create('Enable (Requires Light With shadowVolumesMain)', 750,
        ShadowVolumes, true));
      M2.Append(TMenuItemChecked.Create('Render shadow volumes', 760,
        ShadowVolumesRender, true));
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      '_Lighting Calculation',         91,
      SceneAnimation.Attributes.Lighting, true));
    MenuHeadlight := TMenuItemChecked.Create('_Headlight', 92, CtrlH,
      (Scene <> nil) and Scene.HeadlightOn, true);
    M.Append(MenuHeadlight);
    M.Append(TMenuItemChecked.Create('Use Scene Lights',    93,
      SceneAnimation.Attributes.UseSceneLights, true));
    M.Append(TMenuItem.Create('Light Global Ambient Color ...',  95));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Textures',           94, CtrlT,
      SceneAnimation.Attributes.EnableTextures, true));
    M2 := TMenu.Create('Texture Minification Method');
      MenuAppendMinificationFilters(M2, 1100);
      M.Append(M2);
    M2 := TMenu.Create('Texture Magnification Method');
      MenuAppendMagnificationFilters(M2, 1200);
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Blending',                86,
      SceneAnimation.Attributes.Blending, true));
    M2 := TMenu.Create('Blending Source Factor');
      AppendBlendingFactors(M2, true, 400);
      M.Append(M2);
    M2 := TMenu.Create('Blending Destination Factor');
      AppendBlendingFactors(M2, false, 420);
      M.Append(M2);
    M.Append(TMenuItem.Create('No sorting of transparent shapes', 450));
    M.Append(TMenuItem.Create('Sort transparent shapes (3D)', 452));
    M.Append(TMenuItem.Create('Sort transparent shapes (2D, based on Z)', 454));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Use Occlusion Query', 88,
      SceneAnimation.Attributes.UseOcclusionQuery, true));
    M.Append(TMenuItemChecked.Create('Use Hierarchical Occlusion Query', 90,
      SceneAnimation.Attributes.UseHierarchicalOcclusionQuery, true));
    M.Append(TMenuItemChecked.Create('Debug Last Hierarchical Occlusion Query Results', 891,
      SceneAnimation.Attributes.DebugHierOcclusionQueryResults, true));
    M2 := TMenu.Create('Frustum visualization');
      M2.Append(TMenuItemChecked.Create('Show Walk frustum in Examine mode', 96,
        ShowFrustum, true));
      M2.Append(TMenuItemChecked.Create('When Showing Frustum, ' +
        'Show it Over All Other Objects (no depth test)', 180,
        ShowFrustumAlwaysVisible, true));
      M.Append(M2);
    M2 := TMenu.Create('Octree visualization');
      OctreeTrianglesDisplay.MenuWhole :=
        TMenuItemChecked.Create('Show Whole Collisions (Triangle) Octrees',
        97, OctreeTrianglesDisplay.Whole, true);
      M2.Append(OctreeTrianglesDisplay.MenuWhole);
      M2.Append(TMenuItem.Create('Show _Upper Level of Collisions (Triangle) Octrees', 98, CtrlU));
      M2.Append(TMenuItem.Create('Show _Lower Level of Collisions (Triangle) Octrees', 99, CtrlD));
      M2.Append(TMenuSeparator.Create);
      OctreeVisibleShapesDisplay.MenuWhole :=
        TMenuItemChecked.Create('Show Whole Visible Shapes Octree',
        190, OctreeVisibleShapesDisplay.Whole, true);
      M2.Append(OctreeVisibleShapesDisplay.MenuWhole);
      M2.Append(TMenuItem.Create('Show _Upper Level of Visible Shapes Octree', 191));
      M2.Append(TMenuItem.Create('Show _Lower Level of Visible Shapes Octree', 192));
      M2.Append(TMenuSeparator.Create);
      OctreeCollidableShapesDisplay.MenuWhole :=
        TMenuItemChecked.Create('Show Whole Collidable Shapes Octree',
        195, OctreeCollidableShapesDisplay.Whole, true);
      M2.Append(OctreeCollidableShapesDisplay.MenuWhole);
      M2.Append(TMenuItem.Create('Show _Upper Level of Collidable Shapes Octree', 196));
      M2.Append(TMenuItem.Create('Show _Lower Level of Collidable Shapes Octree', 197));
      M.Append(M2);
    Result.Append(M);
  M := TMenu.Create('_Navigation');
    Viewpoints.Recalculate(Scene);
    M.Append(Viewpoints);
    M.Append(TMenuSeparator.Create);
    AppendNavigationTypes(M);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Next Navigation Type', 111, CtrlN));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('Walk and Fly Settings');
      M2.Append(TMenuItemChecked.Create(
        '_Use Mouse Look',                       128, CtrlM,
          Camera.Walk.MouseLook, true));
      MenuGravity := TMenuItemChecked.Create(
        '_Gravity',                              201, CtrlG,
        Camera.Walk.Gravity, true);
      M2.Append(MenuGravity);
      MenuPreferGravityUpForRotations := TMenuItemChecked.Create(
        'Rotate with Respect to Gravity Vector',      202,
        Camera.Walk.PreferGravityUpForRotations, true);
      M2.Append(MenuPreferGravityUpForRotations);
      MenuPreferGravityUpForMoving := TMenuItemChecked.Create(
        'Move with Respect to Gravity Vector',          203,
        Camera.Walk.PreferGravityUpForMoving, true);
      M2.Append(MenuPreferGravityUpForMoving);
      M2.Append(TMenuItem.Create('Change Gravity Up Vector ...',  124));
      M2.Append(TMenuItem.Create('Change Move Speed...', 205));
      M.Append(M2);
    MenuCollisions := TMenuItemChecked.Create(
      '_Collision Detection and Picking',                123, CtrlC,
        SceneAnimation.Collides, true);
    M.Append(MenuCollisions);
    Result.Append(M);
  M := TMenu.Create('_Animation');
    MenuAnimationTimePlaying := TMenuItemChecked.Create(
      '_Playing / Paused',   220, CtrlP, AnimationTimePlaying, true);
    M.Append(MenuAnimationTimePlaying);
    M.Append(TMenuItem.Create('Rewind to Beginning', 221));
    M.Append(TMenuSeparator.Create);
    CreateMenuNamedAnimations;
    M.Append(MenuNamedAnimations);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Playing Speed Slower or Faster (on display) ...', 222));
    M.Append(TMenuItem.Create('Playing Speed Slower or Faster (on loading) ...', 223));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Process VRML/X3D Events ("off" pauses also animation)', 224, ProcessEventsWanted, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Precalculate Animation from VRML/X3D Events ...', 225));
    Result.Append(M);
  M := TMenu.Create('_Edit');
    MenuRemoveSelectedShape :=
      TMenuItem.Create('Remove _Shape (containing selected triangle)', 36);
    M.Append(MenuRemoveSelectedShape);
    MenuRemoveSelectedFace :=
      TMenuItem.Create('Remove _Face (containing selected triangle)', 37);
    M.Append(MenuRemoveSelectedFace);
    M.Append(TMenuItem.Create(
      'Remove VRML/X3D Nodes with Name Matching ...', 34));
    M.Append(TMenuItem.Create(
      'Remove Special Placeholders from "Castle Game Engine" Levels', 38));
    M.Append(TMenuSeparator.Create);
    MenuMergeCloseVertexes := TMenuItem.Create(
      'Merge Close Vertexes (of node with selected triangle) ...', 730);
    M.Append(MenuMergeCloseVertexes);
    M.Append(TMenuSeparator.Create);
    MenuEditMaterial := TMenu.Create('_Edit Material (of node with selected triangle)');
      MenuEditMaterial.Append(TMenuItem.Create('Diffuse Color ...' , 710));
      MenuEditMaterial.Append(TMenuItem.Create('Specular Color ...', 720));
    M.Append(MenuEditMaterial);
    M.Append(TMenuItem.Create('Load material properties from external file ...', 722));
    M.Append(TMenuItem.Create('Clear loaded material properties', 723));
    M.Append(TMenuSeparator.Create);
    MenuLightsEditor := TMenuItemChecked.Create('Lights Editor', 725,
      LightsEditorIsOpen, false);
    M.Append(MenuLightsEditor);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create(
      'Remove Normals Info from Scene (forces normals to be calculated)',
       31));
    M.Append(TMenuItem.Create('Mark All Shapes as '+
      'non-solid (disables any backface culling)', 32));
    M.Append(TMenuItem.Create('Mark All Faces as '+
      'non-convex (forces faces to be triangulated carefully)', 33));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Add Humanoids Joints Visualization ...', 42));
    Result.Append(M);
  M := TMenu.Create('_Console');
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 1.0)',   106));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 2.0, X3D classic)', 107));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (X3D XML)', 108));
    M.Append(TMenuItem.Create('Print _rayhunter Command-line to Render This View', 105));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Print _Bounding Box (of whole animation)', 109));
    M.Append(TMenuItem.Create('Print Bounding Box (of current _animation frame)', 110));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Print Statistics of Top _Collisions Octree (Based on Shapes)', 101));
    MenuSelectedOctreeStat := TMenuItem.Create('Print Statistics of _Collisions Octree Of Selected Shape (Based on Triangles)', 100);
    M.Append(MenuSelectedOctreeStat);
    M.Append(TMenuItem.Create('Print Statistics of Rendering Octree (Based on Shapes)', 103));
    Result.Append(M);
  M := TMenu.Create('_Display');
    M.AppendRadioGroup(ViewportsConfigNames, 3600, Ord(ViewportsConfig), true, false);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemToggleFullScreen.Create(Window.FullScreen));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Screenshot to Image ...',                  150, K_F5));
    M.Append(TMenuItem.Create('Screenshot to Image (Transparent Background) ...', 151));
    M.Append(TMenuItem.Create('Screenshot to Video / Multiple Images ...', 540));
    M.Append(TMenuItem.Create('Screenshot to _Cube Map (environment around camera position) ...',  550));
    M.Append(TMenuItem.Create('Screenshot to Cube Map DDS (environment around camera position) ...',  555));
    M.Append(TMenuItem.Create('Screenshot Depth to Grayscale Image ...', 560));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Show Controls on Screenshots', 570,
      ControlsOnScreenshot, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Raytrace !',                   125, CtrlR));
    Result.Append(M);
  M := TMenu.Create('_Help');
    M.Append(TMenuItem.Create('Scene Information',                  121));
    M.Append(TMenuItem.Create('Manifold Edges Information',         129));
    MenuSelectedInfo :=
      TMenuItem.Create('Selected Object Information',               171);
    M.Append(MenuSelectedInfo);
    MenuSelectedLightsInfo :=
      TMenuItem.Create('Selected Object Lights Information',        172);
    UpdateSelectedEnabled;
    M.Append(MenuSelectedLightsInfo);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('OpenGL Information',                 173));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Visit view3dscene website',          132));
    M.Append(TMenuItem.Create('Donate to view3dscene',              134));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About view3dscene',                  131));
    Result.Append(M);
end;

{ toolbar -------------------------------------------------------------------- }

var
  OpenButton, ScreenshotButton: TCastleButton;

{ call when ShowStatus or MakingScreenShot changed }
procedure UpdateStatusToolbarVisible;
var
  NT: TNavigationType;
  Vis: boolean;
begin
  Vis := ShowStatus and not MakingScreenShot;

  if ToolbarPanel <> nil then // check was CreateToolbar called already?
  begin
    ToolbarPanel.Exists := Vis;
    OpenButton.Exists := Vis;
    CollisionsButton.Exists := Vis;
    ScreenshotButton.Exists := Vis;

    { Note that WarningsButton ignores the Vis.
      This is by design --- always signal warnings. }

    for NT := Low(NT) to High(NT) do
      if CameraButtons[NT] <> nil then
        CameraButtons[NT].Exists := Vis;

    { Call Resize after changing ToolbarPanel.Exists, as warnings button
      position is calculated differently based on it }
    if not Window.Closed then Window.Container.EventResize;
  end;

  if StatusText <> nil then
    StatusText.Exists := Vis;
end;

procedure CreateStatusToolbar;
var
  NT: TNavigationType;
const
  MinImageHeight = 22;
begin
  ToolbarPanel := TCastlePanel.Create(Application);
  ToolbarPanel.VerticalSeparators.Count := 2;
  Window.Controls.InsertFront(ToolbarPanel);

  StatusText := TExtendedStatusText.Create(Application);
  Window.Controls.InsertFront(StatusText);

  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open';
  OpenButton.OnClick := @THelper(nil).OpenButtonClick;
  OpenButton.Image := V3DSceneImages.Open;
  OpenButton.ImageAlphaTest := true;
  OpenButton.MinImageHeight := MinImageHeight;
  Window.Controls.InsertFront(OpenButton);

  CollisionsButton := TCastleButton.Create(Application);
  CollisionsButton.Caption := 'Collisions';
  CollisionsButton.OnClick := @THelper(nil).CollisionsButtonClick;
  CollisionsButton.MinImageHeight := MinImageHeight;
  CollisionsButton.Toggle := true;
  if SceneAnimation <> nil then
    CollisionsButton.Pressed := SceneAnimation.Collides else
    CollisionsButton.Pressed := true { default value };
  Window.Controls.InsertFront(CollisionsButton);

  ScreenshotButton := TCastleButton.Create(Application);
  ScreenshotButton.Caption := 'Screenshot';
  ScreenshotButton.OnClick := @THelper(nil).ScreenshotButtonClick;
  ScreenshotButton.Image := V3DSceneImages.Screenshot;
  ScreenshotButton.ImageAlphaTest := true;
  ScreenshotButton.MinImageHeight := MinImageHeight;
  Window.Controls.InsertFront(ScreenShotButton);

  WarningsButton := TCastleButton.Create(Application);
  WarningsButton.Caption := 'Warnings';
  WarningsButton.OnClick := @THelper(nil).WarningsButtonClick;
  WarningsButton.Image := Warning_icon;
  WarningsButton.MinImageHeight := MinImageHeight;
  Window.Controls.InsertFront(WarningsButton);

  if SceneWarnings <> nil then
    UpdateWarningsButton else
    WarningsButton.Exists := false; { at least initialize Exists }

  for NT := Low(NT) to High(NT) do
    { Don't show button for ntNone or not StableNavigationType.
      For ntNone it's confusing for new user.
      The "none" navigation type is visible in menu. }
    if (NT <> ntNone) and (NT in StableNavigationType) then
    begin
      CameraButtons[NT] := TNavigationTypeButton.Create(Application, NT);
      CameraButtons[NT].Caption := CameraNames[NT];
      CameraButtons[NT].OnClick := @THelper(nil).NavigationTypeButtonClick;
      CameraButtons[NT].Toggle := true;
      CameraButtons[NT].MinImageHeight := MinImageHeight;
      Window.Controls.InsertFront(CameraButtons[NT]);
    end;

  CameraButtons[ntExamine].Image := V3DSceneImages.Examine;
  CameraButtons[ntWalk].Image := V3DSceneImages.Walk;
  CameraButtons[ntFly].Image := V3DSceneImages.Fly;

  UpdateStatusToolbarVisible;
end;

procedure Resize(Container: TUIContainer);
const
  ToolbarMargin = 5;  {< between buttons and toolbar panel }
  ButtonsMargin = 8; {< between buttons }
  ButtonsSeparatorsMargin = 8; {< between buttons and separators }
var
  NT: TNavigationType;
  NextLeft, ButtonsHeight, ButtonsBottom: Integer;
begin
  ButtonsHeight := Max(
    CameraButtons[ntExamine { any button }].Height,
    WarningsButton.Height);
  ButtonsBottom := Window.Height - ButtonsHeight - ToolbarMargin;

  NextLeft := ToolbarMargin;

  if ToolbarPanel.Exists then
  begin
    ToolbarPanel.Left := 0;
    ToolbarPanel.Width := Window.Width;
    ToolbarPanel.Height := ButtonsHeight + ToolbarMargin * 2;
    ToolbarPanel.Bottom := Window.Height - ToolbarPanel.Height;

    { Now place buttons, in left-to-right order }

    OpenButton.Left := NextLeft;
    OpenButton.Bottom := ButtonsBottom;
    NextLeft += OpenButton.Width + ButtonsSeparatorsMargin;

    ToolbarPanel.VerticalSeparators[0] := NextLeft;
    NextLeft += ToolbarPanel.SeparatorSize + ButtonsSeparatorsMargin;

    for NT := Low(NT) to High(NT) do
      { check with <> nil, since for ntNone or not StableNavigationType
        we don't show button }
      if CameraButtons[NT] <> nil then
      begin
        CameraButtons[NT].Left := NextLeft;
        CameraButtons[NT].Bottom := ButtonsBottom;
        NextLeft += CameraButtons[NT].Width + ButtonsMargin;
      end;
    NextLeft += -ButtonsMargin + ButtonsSeparatorsMargin;

    ToolbarPanel.VerticalSeparators[1] := NextLeft;
    NextLeft += ToolbarPanel.SeparatorSize + ButtonsSeparatorsMargin;

    CollisionsButton.Left := NextLeft;
    CollisionsButton.Bottom := ButtonsBottom;
    NextLeft += CollisionsButton.Width + ButtonsMargin;

    ScreenshotButton.Left := NextLeft;
    ScreenshotButton.Bottom := ButtonsBottom;
    NextLeft += ScreenshotButton.Width + ButtonsMargin;
  end;

  WarningsButton.Left := Max(NextLeft,
    Window.Width - WarningsButton.Width - ToolbarMargin);
  WarningsButton.Bottom := ButtonsBottom;

  ResizeViewports(V3DSceneWindow.Window, SceneManager);
end;

class procedure THelper.OpenButtonClick(Sender: TObject);
var
  URL: string;
begin
  URL := SceneURL;
  if Window.FileDialog('Open file', URL, true, Load3DSequence_FileFilters) then
    LoadScene(URL, [], 0.0, true);
end;

class procedure THelper.NavigationTypeButtonClick(Sender: TObject);
begin
  Camera.NavigationType := (Sender as TNavigationTypeButton).NavigationType;
  ViewportsSetNavigationType(Camera.NavigationType);
  UpdateCameraUI;
end;

class procedure THelper.CollisionsButtonClick(Sender: TObject);
begin
  SetCollisions(not SceneAnimation.Collides, true);
end;

class procedure THelper.ScreenshotButtonClick(Sender: TObject);
begin
  ScreenShotImage('Screenshot to Image', false);
end;

{ Try to lower anti-aliasing (multi-sampling) and shadows (stencil buffer)
  requirements and initialize worse GL context. }
function RetryOpen(Window: TCastleWindowCustom): boolean;
begin
  if Window.AntiAliasing <> aaNone then
  begin
    Window.AntiAliasing := aaNone;
    if AntiAliasingMenu[Window.AntiAliasing] <> nil then
      AntiAliasingMenu[Window.AntiAliasing].Checked := true;
    Writeln('OpenGL context cannot be initialized. Multi-sampling (anti-aliasing) turned off, trying to initialize once again.');
    Result := true;
  end else
  if Window.StencilBits > 0 then
  begin
    Window.StencilBits := 0;
    Writeln('OpenGL context cannot be initialized. Stencil buffer (shadow volumes) turned off, trying to initialize once again.');
    Result := true;
  end else
    Result := false;
end;

{ main --------------------------------------------------------------------- }

var
  Param_CameraRadius: Single = 0.0;
  WasParam_Write: boolean = false;
  Param_WriteEncoding: TX3DEncoding = xeClassic;
  Param_WriteForceX3D: boolean = false;
  WasParam_SceneURL: boolean = false;
  Param_SceneURL: string;
  Param_SceneChanges: TSceneChanges = [];
  Param_HideMenu: boolean = false;

const
  Options: array [0..18] of TOption =
  (
    (Short:  #0; Long: 'camera-radius'; Argument: oaRequired),
    (Short:  #0; Long: 'scene-change-no-normals'; Argument: oaNone),
    (Short:  #0; Long: 'scene-change-no-solid-objects'; Argument: oaNone),
    (Short:  #0; Long: 'scene-change-no-convex-faces'; Argument: oaNone),
    (Short:  #0; Long: 'write-to-vrml'; Argument: oaNone),
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'screenshot'; Argument: oaRequired2Separate),
    (Short:  #0; Long: 'screenshot-range'; Argument: oaRequired4Separate),
    (Short:  #0; Long: 'debug-log'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-changes'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-cache'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-shaders'; Argument: oaNone),
    (Short:  #0; Long: 'anti-alias'; Argument: oaRequired),
    (Short: 'H'; Long: 'hide-extras'; Argument: oaNone),
    (Short:  #0; Long: 'write'; Argument: oaNone),
    (Short:  #0; Long: 'write-encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'write-force-x3d'; Argument: oaNone),
    (Short:  #0; Long: 'hide-menu'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  var
    SingleScreenShot: TSingleScreenShot;
    RangeScreenShot: TRangeScreenShot;
  begin
    case OptionNum of
      0 : Param_CameraRadius := StrToFloat(Argument);
      1 : Include(Param_SceneChanges, scNoNormals);
      2 : Include(Param_SceneChanges, scNoSolidObjects);
      3 : Include(Param_SceneChanges, scNoConvexFaces);
      4 : begin
            WasParam_Write := true;
            Param_WriteEncoding := xeClassic;
          end;
      5 : begin
           InfoWrite(
             'view3dscene: VRML / X3D browser, and a viewer for other 3D formats.' +NL+
             'You can navigate in the (possibly animated and interactive) 3D scene,' +NL+
             'with collision-checking, gravity, and a wealth of graphic effects.' +NL+
             'You can also convert models in other formats to VRML/X3D.' +NL+
             NL+
             'Call as' +NL+
             '  view3dscene [OPTIONS]... [FILE-NAME-TO-OPEN]' +NL+
             NL+
             'You can provide FILE-NAME-TO-OPEN on the command-line.' +NL+
             'As usual, dash (-) means that standard input will be read' +NL+
             '(in this case the input must be in Inventor / VRML / X3D (classic) format).' +NL+
             NL+
             'Available options are:' +NL+
             HelpOptionHelp +NL+
             VersionOptionHelp +NL+
             '  -H / --hide-extras    Do not show anything extra (like status text' +NL+
             '                        or toolbar or bounding box) when program starts.' +NL+
             '                        Show only the 3D world.' +NL+
             '  --hide-menu           Hide menu bar.' +NL+
             '  --write               Load the scene,'+NL+
             '                        optionally process by --scene-change-xxx,' +NL+
             '                        save it as VRML/X3D to the standard output,' +NL+
             '                        exit. Use --write-encoding to choose encoding.' +NL+
             '  --write-encoding classic|xml' +NL+
             '                        Choose X3D encoding to use with --write option.' +NL+
             '                        Default is "classic".' +NL+
             '  --write-force-x3d     Force convertion from VRML to X3D with --write option.' +NL+
             '                        Note that if you choose XML encoding' +NL+
             '                        (by --write-encoding=xml), this is automatic.' +NL+
             '                        Note that this works sensibly only for VRML 2.0' +NL+
             '                        (not for older Inventor/VRML 1.0,' +NL+
             '                        we cannot convert them to valid X3D for now).' +NL+
             '  --screenshot TIME IMAGE-FILE-NAME' +NL+
             '                        Take a screenshot of the loaded scene' +NL+
             '                        at given TIME, and save it to IMAGE-FILE-NAME.' +NL+
             '                        You most definitely want to pass 3D model' +NL+
             '                        file to load at command-line too, otherwise' +NL+
             '                        we''ll just make a screenshot of the default' +NL+
             '                        black scene.' +NL+
             '  --screenshot-range TIME-BEGIN TIME-STEP FRAMES-COUNT FILE-NAME' +NL+
             '                        Take a FRAMES-COUNT number of screenshots from' +NL+
             '                        TIME-BEGIN by step TIME-STEP. Save them to' +NL+
             '                        a single movie file (like .avi) (ffmpeg must' +NL+
             '                        be installed and available on $PATH for this)' +NL+
             '                        or to a sequence of image files (FILE-NAME' +NL+
             '                        must then be specified like image@counter(4).png).' +NL+
             '  --viewpoint NAME      Use the viewpoint with given name or index as initial.' +NL+
             '                        Especially useful to make a screenshot from given viewpoint.' +NL+
             '  --anti-alias AMOUNT   Use full-screen anti-aliasing.' +NL+
             '                        Argument AMOUNT is an integer >= 0.' +NL+
             '                        Exact 0 means "no anti-aliasing",' +NL+
             '                        this is the default. Each successive integer' +NL+
             '                        generally makes method one step better.' +NL+
             '                        Especially useful to make a screenshot with' +NL+
             '                        anti-aliasing quality.' +NL+
             SoundEngine.ParseParametersHelp + NL+
             NL+
             TCastleWindowCustom.ParseParametersHelp(StandardParseOptions, true) +NL+
             NL+
             'Debug options:' +NL+
             '  --debug-log           Write log info to stdout.' +NL+
             '  --debug-log-cache     Write log info, including cache.' +nl+
             '  --debug-log-shaders   Write log info, including shader source and log.' +nl+
             '  --debug-log-changes   Write log info, including VRML/X3D graph changes.' +nl+
             NL+
             'Deprecated options:' +NL+
             '  --scene-change-no-normals' +NL+
             '                        Remove normals information from the loaded scene.' +NL+
             '                        Forces automatic calculation of normal vectors.' +NL+
             '                        Deprecated, doing this from command-line is not' +NL+
             '                        usually useful.' +NL+
             '  --scene-change-no-solid-objects' +NL+
             '                        Make all shapes not solid in the loaded scene.' +NL+
             '                        Disables backface culling.' +NL+
             '                        Deprecated, doing this from command-line is not' +NL+
             '                        usually useful.' +NL+
             '  --scene-change-no-convex-faces' +NL+
             '                        Treat all faces as potentially concave in the loaded scene.' +NL+
             '                        Deprecated, doing this from command-line is not' +NL+
             '                        usually useful.' +NL+
             '  --write-to-vrml       Deprecated, shortcut for "--write --write-encoding=classic".' +NL+
             '  --camera-radius RADIUS' +NL+
             '                        Set camera sphere radius used for collisions' +NL+
             '                        and determinig moving speed.' +NL+
             '                        Deprecated, consider using NavigationInfo node' +NL+
             '                        inside your scene instead.' +NL+
             '  --navigation EXAMINE|WALK|FLY...'+nl+
             '                        Set initial navigation style.' +NL+
             '                        Deprecated, consider using NavigationInfo node' +NL+
             '                        inside your scene instead.' +NL+
             X3DNodesDetailOptionsHelp +NL+
             NL+
             SCastleEngineProgramHelpSuffix(DisplayApplicationName, Version, true));
           ProgramBreak;
          end;
      6 : begin
           Writeln(Version);
           ProgramBreak;
          end;
      7 : begin
            SingleScreenShot := TSingleScreenShot.Create;
            SingleScreenShot.Time := StrToFloat(SeparateArgs[1]);
            SingleScreenShot.URLPattern := SeparateArgs[2];
            ScreenShotsList.Add(SingleScreenShot);
          end;
      8 : begin
            RangeScreenShot := TRangeScreenShot.Create;
            RangeScreenShot.TimeBegin := StrToFloat(SeparateArgs[1]);
            RangeScreenShot.TimeStep := StrToFloat(SeparateArgs[2]);
            RangeScreenShot.FramesCount := StrToInt(SeparateArgs[3]);
            RangeScreenShot.URLPattern := SeparateArgs[4];
            ScreenShotsList.Add(RangeScreenShot);
          end;
      9 : InitializeLog(Version);
      10: begin
            InitializeLog(Version);
            LogChanges := true;
          end;
      11: begin
            InitializeLog(Version);
            LogRendererCache := true;
          end;
      12: begin
            InitializeLog(Version);
            LogShaders := true;
          end;
      13: begin
            Window.AntiAliasing := TAntiAliasing(Clamped(StrToInt(Argument),
              Ord(Low(TAntiAliasing)), Ord(High(TAntiAliasing))));
            if AntiAliasingMenu[Window.AntiAliasing] <> nil then
              AntiAliasingMenu[Window.AntiAliasing].Checked := true;
          end;
      14: begin
            ShowBBox := false;
            ShowStatus := false;
            UpdateStatusToolbarVisible;
          end;
      15: WasParam_Write := true;
      16: if SameText(Argument, 'classic') then
            Param_WriteEncoding := xeClassic else
          if SameText(Argument, 'xml') then
            Param_WriteEncoding := xeXML else
            raise EInvalidParams.CreateFmt('Invalid --write-encoding argument "%s"', [Argument]);
      17: Param_WriteForceX3D := true;
      18: Param_HideMenu := true;
      else raise EInternalError.Create('OptionProc');
    end;
  end;

begin
  Window := TCastleWindowCustom.Create(Application);

  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { initialize RecentMenu, before Config.Load }
  RecentMenu := TWindowRecentFiles.Create(nil);
  RecentMenu.OnOpenRecent := @THelper(nil).OpenRecent;

  { initialize SoundEngine, before Config.Load }
  SoundEngine;

  { load config, before SoundEngine.ParseParameters
    (that may change Enable by --no-sound) and
    after creating Window (various OnLoad / OnSave may use Window instance). }
  Config.Load;

  { parse parameters }
  Window.ParseParameters(StandardParseOptions);
  SoundEngine.ParseParameters;
  CamerasParseParameters;
  ViewpointsParseParameters;
  X3DNodesDetailOptionsParse;
  Parameters.Parse(Options, @OptionProc, nil);
  { the most important param : URL to load }
  if Parameters.High > 1 then
   raise EInvalidParams.Create('Excessive command-line parameters. '+
     'Expected at most one URL to load') else
  if Parameters.High = 1 then
  begin
    WasParam_SceneURL := true;
    Param_SceneURL := Parameters[1];
  end;

  SceneManager := TV3DSceneManager.Create(nil);
  Window.Controls.Add(SceneManager);
  SceneManager.OnBoundViewpointChanged := @THelper(nil).BoundViewpointChanged;
  SceneManager.OnBoundNavigationInfoChanged := @THelper(nil).BoundNavigationInfoChanged;

  InitializeViewports(TV3DViewport);
  BGColorChanged(SceneManager);

  CreateStatusToolbar;

  Window.Controls.Add(ScreenEffects);

  SceneWarnings := TSceneWarnings.Create;
  try
    OnWarning := @OnWarningHandle;

    if WasParam_Write then
    begin
      if not WasParam_SceneURL then
        raise EInvalidParams.Create('You used --write option, '+
          'this means that you want to convert some 3D model file to VRML/X3D. ' +
          'But you didn''t provide any URL on command-line to load.');
      WriteModel(Param_SceneURL, Param_SceneChanges, Param_WriteEncoding,
        Param_WriteForceX3D);
      Exit;
    end;

    { This is for loading default clean scene.
      LoadClearScene should be lighting fast always,
      so progress should not be needed in this case anyway
      (and we don't want to clutter stdout). }
    Progress.UserInterface := ProgressNullInterface;

    { init "scene global variables" to initial empty values }
    SceneAnimation := TCastlePrecalculatedAnimation.Create(nil);
    try
      SceneAnimation.TryFirstSceneDynamic := true;
      AttributesLoadFromConfig(SceneAnimation.Attributes);
      SceneManager.Items.Add(SceneAnimation);

      InitCameras(SceneManager);
      InitTextureFilters(SceneAnimation);

      { init "scene global variables" to non-null values }
      LoadClearScene;
      try
        Theme.MessageInputTextColor := Vector4Single(0, 0.4, 0, 1.0);
        Theme.MessageTextColor := Black;
        Theme.Images[tiWindow] := WindowGray;
        Theme.Images[tiLabel] := FrameYellowBlack;

        Window.GtkIconName := 'view3dscene';
        Window.MainMenu := CreateMainMenu;
        Window.MainMenuVisible := not Param_HideMenu;
        Window.OnMenuClick := @MenuClick;
        Window.OnResize := @Resize;
        Window.OnPress := @Press;
        Window.OnDropFiles := @DropFiles;

        if MakingScreenShot then
        begin
          { --geometry must work as reliably as possible in this case.
            This is only in case FBO will not be available,
            TGLRenderToTexture will then use normal window contents. }
          Window.ResizeAllowed := raNotAllowed;

          { Do not show window on the screen, since we're working in batch mode. }
          Window.Visible := false;

          { Don't make sound when rendering in batch mode. }
          SoundEngine.Enable := false;
          SoundEngine.EnableSaveToConfig := false;
        end;

        Window.FpsShowOnCaption := true;
        Window.StencilBits := 8;

        Window.Open(@RetryOpen);

        if WasParam_SceneURL then
          LoadScene(Param_SceneURL, Param_SceneChanges, Param_CameraRadius, true) else
          LoadWelcomeScene;

        if MakingScreenShot then
        begin
          MakeAllScreenShotsFBO;
          Exit;
        end;

        Application.Run;
      finally FreeScene end;

      AttributesSaveToConfig(SceneAnimation.Attributes);

      Config.Save;
    finally
      FreeAndNil(SceneAnimation);
      FreeAndNil(RecentMenu);
    end;
  finally
    FreeAndNil(SceneWarnings);
    FreeAndNil(SceneManager);
  end;
end.
