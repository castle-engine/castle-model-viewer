{
  Copyright 2002-2011 Michalis Kamburelis.

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
  See [http://vrmlengine.sourceforge.net/view3dscene.php] for user
  documentation.

  The actual name of this program is "Kambi VRML game engine
  swiss army knife" :) This program shows and uses a lot
  of our engine features. It's basically a giant GUI to load everything,
  and tweak every option from the menu and toolbar.

  If you want to find out how to use "Kambi VRML game engine",
  this isn't the best place to study. Look instead at simple examples
  in engine sources, like
    ../kambi_vrml_game_engine/examples/vrml/simplest_vrml_browser.lpr
    ../kambi_vrml_game_engine/examples/vrml/scene_manager_demos.lpr
    ../kambi_vrml_game_engine/examples/vrml/tovrmlx3d.lpr
    (the last one is an example of command-line converter from
    Collada, OBJ, 3DS (and all other model formats we can read) to VRML).

  Basic components of this program:
  - use LoadVRMLSequence to load any format to VRML scene.
    This converts any known (to our engine) 3D model format to VRML.
    This convertion doesn't lose anything because VRML is able to
    express everything that is implemented in other 3D formats readers.
    And we gain the simplicity of this program (we just treat everything
    as VRML scene, actually VRML animation),
    optimization (vbo, OpenGL renderer cache inside VRML renderer),
    functionality (like automatic normals generation based on creaseAngle).
  - render scene using TVRMLGLScene (actually TVRMLGLAnimation
    and TVRMLGLScene is inside)
  - use Cameras and TGLUIWindow to let user navigate
    over the scene using various navigation modes
    (Examine, Walk) and with optional gravity
  - build TVRMLTriangleOctree to allow collision detection for
    Walk camera and to allow raytracer
  - build TVRMLShapeOctree to allow frustum culling using
    octree by TVRMLGLScene
  - use VRMLRayTracer embedded in RaytraceToWindow module to allow
    viewing raytraced image
  - allow some kind of object picking with mouse left button
    (for VRML sensors) and right button (to select for editing).
    This uses simple one-primary-ray casting.
}

program view3dscene;

{$ifdef MSWINDOWS}
  {$R windows/view3dscene.res}
{$endif MSWINDOWS}

uses KambiUtils, SysUtils, VectorMath, Boxes3D, Classes, KambiClassUtils,
  BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  ParseParametersUnit, ProgressUnit, Cameras,
  KambiStringUtils, KambiFilesUtils, KambiTimeUtils,
  DataErrors, KambiLog, ProgressConsole, DateUtils, Frustum,
  Images, CubeMap, DDS, Base3D, ALSoundEngine, UIControls,
  { OpenGL related units: }
  GL, GLWindow, KambiGLUtils, OpenGLBmpFonts,
  GLWinMessages, GLProgress, GLWindowRecentFiles, GLImages,
  GLAntiAliasing, GLVersionUnit, GLCubeMap, GLControls, GLShaders,
  { VRML (and possibly OpenGL) related units: }
  VRMLFields, VRMLShapeOctree,
  VRMLNodes, Object3DAsVRML, VRMLGLScene, VRMLTriangle,
  VRMLScene, VRMLNodesDetailOptions,
  VRMLCameraUtils, VRMLErrors, VRMLGLAnimation,
  VRMLGLRenderer, VRMLShape, RenderingCameraUnit, VRMLShadowMaps, KambiSceneManager,
  { view3dscene-specific units: }
  V3DSceneTextureFilters, V3DSceneLights, V3DSceneRaytrace,
  V3DSceneNavigationTypes, V3DSceneSceneChanges, V3DSceneBGColors, V3DSceneViewpoints,
  V3DSceneConfig, V3DSceneBlending, V3DSceneWarnings, V3DSceneFillMode,
  V3DSceneAntiAliasing, V3DSceneScreenShot,
  V3DSceneShadows, V3DSceneOctreeVisualize, V3DSceneMiscConfig, V3DSceneImages,
  V3DSceneScreenEffects, V3DSceneHAnim, V3DSceneViewports;

var
  Window: TGLUIWindow;

  ShowFrustum: boolean = false;
  ShowFrustumAlwaysVisible: boolean = false;

  MenuCollisions: TMenuItemChecked;

  { Initialized in Open, finalized in Close. }
  StatusFont: TGLBitmapFont;

  RecentMenu: TGLRecentFiles;

  { These are so-called "scene global variables".
    Modified only by LoadSceneCore (and all using it Load*Scene* procedures)
    and FreeScene.
    Also note that Window.Caption (and FPSBaseCaption) also should be modified
    only by those procedures.

    In this program's comments I often talk about "null values" of these
    variables, "null values" mean that these variables have some *defined
    but useless* values, i.e.
      SceneAnimation.Loaded = false
      SceneFileName = '' }
  { Note that only one SceneAnimation object is created and present for the whole
    lifetime of this program, i.e. when I load new scene (from "Open"
    menu item) I DO NOT free and create new SceneAnimation object.
    Instead I'm only freeing and creating underlying scenes
    (by Close / Load of TVRMLGLAnimation).
    This way I'm preserving values of all Attributes.Xxx when opening new scene
    from "Open" menu item. }
  SceneAnimation: TVRMLGLAnimation;
  SceneFilename: string;

  SelectedItem: PVRMLTriangle;
  { SelectedPoint* always lies on SelectedItem item,
    and it's meaningless when SelectedItem = nil.
    World is in world coords,
    local in local shape (SelectedItem^.State.Transform) coords. }
  SelectedPointWorld, SelectedPointLocal: TVector3Single;
  { SelectedShape is always SelectedItem^.Shape.
    Except in GeometryChanged, when SelectedItem may be already freed,
    and then SelectedShape variable is useful to compare with OnlyShapeChanged. }
  SelectedShape: TVRMLShape;

  { Set to non-nil by CreateMainMenu. }
  MenuSelectedOctreeStat: TMenuItem;
  MenuSelectedInfo: TMenuItem;
  MenuSelectedLightsInfo: TMenuItem;
  MenuRemoveSelectedGeometry: TMenuItem;
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

  { If WarningsButton.Exists is allowed. If false, then WarningsButton.Exists
    should be false, regardless of warnings count. }
  WarningsButtonEnabled: boolean = true;
  WarningsButton: TKamGLButton;

  ToolbarPanel: TKamPanel;
  CollisionsButton: TKamGLButton;

  AnimationTimeSpeedWhenLoading: TKamTime = 1.0;
  AnimationTimePlaying: boolean = true;
  MenuAnimationTimePlaying: TMenuItemChecked;

  { These are set by Draw right after rendering a SceneAnimation frame. }
  LastRender_RenderedShapesCount: Cardinal;
  LastRender_BoxesOcclusionQueriedCount: Cardinal;
  LastRender_VisibleShapesCount: Cardinal;

{ Helper class ---------------------------------------------------------------

  Some callbacks here must be methods (procedure of class),
  so we use this dummy class to contain them. }

type
  THelper = class
    class procedure OpenRecent(const FileName: string);
    class procedure GeometryChanged(Scene: TVRMLScene;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TVRMLShape);
    class procedure ViewpointsChanged(Scene: TVRMLScene);
    class procedure BoundViewpointChanged(Sender: TObject);
    class procedure BoundNavigationInfoChanged(Sender: TObject);
    class procedure PointingDeviceSensorsChange(Sender: TObject);
    class procedure HeadlightOnChanged(Sender: TObject);
    class procedure WarningsButtonClick(Sender: TObject);
    class procedure NavigationTypeButtonClick(Sender: TObject);
    class procedure OpenButtonClick(Sender: TObject);
    class procedure CollisionsButtonClick(Sender: TObject);
  end;

{ SceneManager and viewport ------------------------------------------------ }

type
  TV3DSceneManager = class(TV3DShadowsSceneManager)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
    procedure Render3D(const Params: TRenderParams); override;
  public
    procedure BeforeDraw; override;
    procedure Draw; override;
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function ScreenEffectsCount: Integer; override;
    function ScreenEffectsNeedDepth: boolean; override;
  end;

  TV3DViewport = class(TV3DShadowsViewport)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
    procedure Render3D(const Params: TRenderParams); override;
  public
    procedure BeforeDraw; override;
    procedure Draw; override;
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function ScreenEffectsCount: Integer; override;
    function ScreenEffectsNeedDepth: boolean; override;
  end;

var
  SceneManager: TV3DSceneManager;

procedure ViewportProperties(Viewport: TKamAbstractViewport);
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

{ Helper functions ----------------------------------------------------------- }

{ First animation scene. Always present when loaded,
  nil if not (although our SceneAnimation tries to always stay loaded).
  We use it for VRML/X3D events, and all other stuff where
  a single scene is needed. }
function Scene: TVRMLGLScene;
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
  if MenuRemoveSelectedGeometry <> nil then
    MenuRemoveSelectedGeometry.Enabled := SelectedItem <> nil;
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
function SceneOctreeCollisions: TVRMLBaseTrianglesOctree;
begin
  if (SceneAnimation <> nil) and
     (Scene <> nil) and
     (Scene.OctreeCollisions <> nil) then
    Result := Scene.OctreeCollisions else
    Result := nil;
end;

function SceneOctreeRendering: TVRMLShapeOctree;
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

function ProjectionType: TProjectionType; forward;
function ViewpointNode: TVRMLViewpointNode; forward;

{ TStatusText ---------------------------------------------------------------- }

type
  TStatusText = class(TUIControl)
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TStatusText.DrawStyle: TUIControlDrawStyle;
begin
  if Exists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TStatusText.Draw;
const
  StatusInsideCol: TVector4f = (0, 0, 0, 0.7);
  StatusBorderCol: TVector4f = (0, 1, 0, 1);
  StatusTextCol  : TVector4f = (1, 1, 0, 1);
var
  strs: TStringList;

  { Describe pointing-device sensors (under the mouse, and also active
    one (if any)). }
  procedure DescribeSensors;

    function DescribeSensor(Sensor: TVRMLNode): string;
    var
      Desc: string;
      J: Integer;
    begin
      Result := Format('%s', [Sensor.NiceName]);

      if Sensor is TNodeX3DPointingDeviceSensorNode then
        Desc := TNodeX3DPointingDeviceSensorNode(Sensor).FdDescription.Value else
      if Sensor is TNodeAnchor then
      begin
        Desc := TNodeAnchor(Sensor).FdDescription.Value;
        for J := 0 to TNodeAnchor(Sensor).FdUrl.Count - 1 do
        begin
          if J = 0 then
          begin
            if Desc <> '' then Desc += ' ';
            Desc += '[';
          end else
            Desc += ', ';
          Desc += TNodeAnchor(Sensor).FdUrl.Items.Items[J];
        end;
        if TNodeAnchor(Sensor).FdUrl.Count <> 0 then
          Desc += ']';
      end else
        Desc := '';

      Desc := SForCaption(Desc);
      if Desc <> '' then
        Result += ' ' + Desc;
    end;

  var
    Sensors: TPointingDeviceSensorsList;
    I: Integer;
  begin
    if SceneAnimation.ScenesCount = 1 then
    begin
      if Scene.PointingDeviceOverItem <> nil then
      begin
        Sensors := Scene.PointingDeviceSensors;
        for I := 0 to Sensors.Count - 1 do
          if Sensors.Enabled(I) then
            Strs.Append('Over enabled sensor: ' + DescribeSensor(Sensors[I]));
      end;
      for I := 0 to Scene.PointingDeviceActiveSensors.Count - 1 do
        Strs.Append('Active sensor: ' +
          DescribeSensor(Scene.PointingDeviceActiveSensors[I]));
    end;

    if Strs.Count <> 0 then
      Strs.Append(''); { a blank line, separating from the rest of status }
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
begin
  if not Exists then Exit;

  glLoadIdentity;
  glTranslatef(5, 5, 0);

  strs := TStringList.Create;
  try
    DescribeSensors;

    { S := Format('Collision detection: %s', [ BoolToStrOO[SceneAnimation.Collides] ]);
    if SceneOctreeCollisions = nil then
      S += ' (octree resources released)';
    strs.Append(S); }

    if Camera.NavigationClass = ncWalk then
    begin
      strs.Append(Format('Camera: pos %s, dir %s, up %s',
        [ VectorToNiceStr(Camera.Walk.Position),
          VectorToNiceStr(Camera.Walk.Direction),
          VectorToNiceStr(Camera.Walk.Up) ]));
      strs.Append(Format('Move speed (per sec) : %f, Avatar height: %f (last height above the ground: %s)',
        [ Camera.Walk.MoveSpeed,
          Camera.Walk.CameraPreferredHeight,
          CurrentAboveHeight ]));
    end;

    { if SceneLightsCount = 0 then
     s := '(useless, scene has no lights)' else
     s := BoolToStrOO[SceneAnimation.Attributes.UseSceneLights];
    strs.Append(Format('Use scene lights: %s', [s])); }

    if SceneAnimation.Attributes.UseOcclusionQuery or
       SceneAnimation.Attributes.UseHierarchicalOcclusionQuery then
      S := Format(' (+ %d boxes to occl query)', [LastRender_BoxesOcclusionQueriedCount]) else
      S := '';
    strs.Append(Format('Rendered Shapes : %d%s of %d ',
      [ LastRender_RenderedShapesCount, S,
        LastRender_VisibleShapesCount ]) + OctreeDisplayStatus);

    if SceneAnimation.TimeAtLoad = 0.0 then
      S := Format('World time: %d', [Trunc(SceneAnimation.Time)]) else
      S := Format('World time: load time + %d = %d',
        [Trunc(SceneAnimation.Time - SceneAnimation.TimeAtLoad),
         Trunc(SceneAnimation.Time)]);
    if not AnimationTimePlaying then
      S += ' (paused)';
    if not ProcessEventsWanted then
      S += ' (paused, not processing VRML events)';
    strs.Append(S);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

      statusFont.printStringsBox(strs, 0,
        StatusInsideCol, StatusBorderCol, StatusTextCol,
        nil, 5, 1, 1);

    glDisable(GL_BLEND);
  finally strs.Free end;
end;

var
  StatusText: TStatusText;

{ TGLWindow callbacks --------------------------------------------------------- }

procedure Open(Window: TGLWindow);
begin
 statusFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);

 { We want to be able to render any scene --- so we have to be prepared
   that fog interpolation has to be corrected for perspective. }
 glHint(GL_FOG_HINT, GL_NICEST);

 GLProgressInterface.Window := Window;
 Progress.UserInterface := GLProgressInterface;

 BGColorChanged;

 ShadowsGLOpen;

 AntiAliasingGLOpen;
 AntiAliasingEnable;
end;

procedure Close(Window: TGLWindow);
begin
  ShadowsGLClose;
  FreeAndNil(statusFont);

  Progress.UserInterface := ProgressNullInterface;
end;

{ Render visualization of various stuff, like bounding box, octree and such. }
procedure RenderVisualizations;

  procedure DrawFrustum(AlwaysVisible: boolean);
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
  end;

begin
  if RenderingCamera.Target = rtScreen then
  begin
    { Visualization below depends on DEPTH_TEST enabled
      (and after rendering scene, it may be disabled, if not PreserveOpenGLState) }
    glEnable(GL_DEPTH_TEST);

    { Use SceneAnimation.Attributes.LineWidth for our visualizations as well }
    glLineWidth(SceneAnimation.Attributes.LineWidth);

    OctreeDisplay(SceneAnimation);

    if showBBox and (not MakingScreenShot) then
    begin
      { Display current bounding box only if there's a chance that it's
        different than whole animation BoundingBox --- this requires that animation
        has at least two frames. }
      if SceneAnimation.ScenesCount > 1 then
      begin
        glColorv(Red3Single);
        if not IsEmptyBox3D(SceneAnimation.CurrentScene.BoundingBox) then
          glDrawBox3DWire(SceneAnimation.CurrentScene.BoundingBox);
      end;

      glColorv(Green3Single);
      if not IsEmptyBox3D(SceneAnimation.BoundingBox) then
        glDrawBox3DWire(SceneAnimation.BoundingBox);
    end;

    { Note that there is no sense in showing viewing frustum in
      Camera.NavigationClass <> ncExamine, since viewing frustum should
      be never visible then (or should be just at the exact borders
      or visibility, so it's actually unspecified whether OpenGL would
      show it or not). }
    if ShowFrustum and ((Camera as TUniversalCamera).NavigationClass = ncExamine) then
     DrawFrustum(ShowFrustumAlwaysVisible);

    if SelectedItem <> nil then
    begin
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
    end;
  end;
end;

procedure TV3DSceneManager.Render3D(const Params: TRenderParams);
begin
  inherited;
  { RenderVisualizations are opaque, so they should be renderer here
    to correctly mix with partially transparent 3D scenes. }
  if Params.TransparentGroup in [tgAll, tgOpaque] then
    RenderVisualizations;
end;

procedure TV3DSceneManager.RenderFromView3D(const Params: TRenderParams);
begin
  { Although TKamAbstractViewport is ready for MainScene = nil case,
    this RenderFromView3D below is not. Doesn't seem needed,
    but better to secure for this case, since any TKamAbstractViewport
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
    LastRender_RenderedShapesCount := MainScene.LastRender_RenderedShapesCount;
    LastRender_BoxesOcclusionQueriedCount := MainScene.LastRender_BoxesOcclusionQueriedCount;
    LastRender_VisibleShapesCount  := MainScene.LastRender_VisibleShapesCount;
  end;
end;

procedure TV3DSceneManager.BeforeDraw;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DSceneManager.Draw;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DViewport.Render3D(const Params: TRenderParams);
begin
  inherited;
  { RenderVisualizations are opaque, so they should be renderer here
    to correctly mix with partially transparent 3D scenes. }
  if Params.TransparentGroup in [tgAll, tgOpaque] then
    RenderVisualizations;
end;

procedure TV3DViewport.RenderFromView3D(const Params: TRenderParams);
begin
  { Although TKamAbstractViewport is ready for MainScene = nil case,
    this RenderFromView3D below is not. Doesn't seem needed,
    but better to secure for this case, since any TKamAbstractViewport
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
    { Custom viewports don't set LastRender_* }
  end;
end;

procedure TV3DViewport.BeforeDraw;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

procedure TV3DViewport.Draw;
begin
  { Make sure to call ViewportProperties before inherited. }
  ViewportProperties(Self);
  inherited;
end;

const
  SNavigationClassWalkNeeded =
    'You must be in "Walk", "Fly" or "None" navigation types to use this function.';

  SOnlyWhenOctreeAvailable = 'This is not possible when octree is not generated. Turn on "Navigation -> Collision Detection" to make it available.';

procedure MouseDown(Window: TGLWindow; Button: TMouseButton);
begin
  if (Button = mbRight) and
     { Support selecting item by right button click only in Walk mode.

       In Examine mode, it would collide with zooming in / out,
       making  user select item on each zoom in / out by dragging
       right mouse button. Mouse clicks start dragging, so don't do
       anything here (not even a warning about SNavigationClassWalkNeeded,
       since it would only interfere with navigation). }
     (Camera.NavigationClass <> ncExamine) then
  begin
    SelectedItem := Scene.PointingDeviceOverItem;
    SelectedPointWorld := Scene.PointingDeviceOverPoint;

    { calculate SelectedPointLocal }
    if SelectedItem <> nil then
    begin
      SelectedShape := TVRMLShape(SelectedItem^.Shape);
      try
        SelectedPointLocal := MatrixMultPoint(
          SelectedItem^.State.InvertedTransform, SelectedPointWorld);
      except
        on ETransformedResultInvalid do
          SelectedItem := nil;
      end;
    end;

    UpdateSelectedEnabled;

    Window.PostRedisplay;
  end;
end;

class procedure THelper.PointingDeviceSensorsChange(Sender: TObject);
begin
  { Our status text displays current sensors (under the mouse,
    and currently active (if any)), so we have to redisplay. }
  Window.PostRedisplay;
end;

{ Setting viewpoint ---------------------------------------------------------- }

function NavigationNode: TNodeNavigationInfo;
begin
  if Scene <> nil then
    Result := Scene.NavigationInfoStack.Top as TNodeNavigationInfo else
    Result := nil;
end;

function ViewpointNode: TVRMLViewpointNode;
begin
  if Scene <> nil then
    Result := Scene.ViewpointStack.Top as TVRMLViewpointNode else
    Result := nil;
end;

function ProjectionType: TProjectionType;
var
  Viewpoint: TVRMLViewpointNode;
begin
  Viewpoint := ViewpointNode;
  if Viewpoint <> nil then
    Result := Viewpoint.ProjectionType else
    Result := ptPerspective;
end;

class procedure THelper.ViewpointsChanged(Scene: TVRMLScene);
begin
  Viewpoints.Recalculate(Scene);
end;

class procedure THelper.BoundViewpointChanged(Sender: TObject);
var
  V: TVRMLViewpointNode;
begin
  V := SceneManager.MainScene.ViewpointStack.Top as TVRMLViewpointNode;
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

var
  DebugLogVRMLChanges: boolean = false;

{ Call when WarningsButtonEnabled or SceneWarnings.Count changes
  or when window sizes change. }
procedure UpdateWarningsButton;
begin
  WarningsButton.Caption := Format('%d warnings', [SceneWarnings.Count]);
  WarningsButton.Exists := WarningsButtonEnabled and (SceneWarnings.Count <> 0);
  { When window is closed, width/height may be incorrect (even negative,
    because of GLWindowDefaultSize). Do not call EventResize then.
    May happen when you used --write, and some warning occurs. }
  if not Window.Closed then
    Window.EventResize; { update WarningsButton.Left }
end;

procedure DoVRMLWarning(const WarningType: TVRMLWarningType; const s: string);
begin
  { Write to ErrOutput, not normal Output, since when --write is used,
    we write to output VRML/X3D contents. }
  Writeln(ErrOutput, ProgramName + ': VRML/X3D Warning: ' + S);
  SceneWarnings.Add(S);
  UpdateWarningsButton;
  if Window <> nil then
    Window.PostRedisplay;
end;

procedure DoDataWarning(const s: string);
begin
  { Write to ErrOutput, not normal Output, since when --write is used,
    we write to output VRML/X3D contents. }
  Writeln(ErrOutput, ProgramName + ': Data Warning: ' + S);
  SceneWarnings.Add(S);
  UpdateWarningsButton;
end;

procedure SceneOctreeCreate;
var
  OldDraw, OldBeforeDraw: TDrawFunc;
begin
  { Do not create octrees when SceneAnimation.Collides = false. This makes
    setting SceneAnimation.Collides to false an optimization: octree doesn't have to
    be recomputed when animation frame changes, or new scene is loaded etc. }

  if SceneAnimation.Collides then
  begin
    { Beware: constructing octrees will cause progress drawing,
      and progress drawing may cause FlushRedisplay,
      and FlushRedisplay may cause OnDraw and OnBeforeDraw to be called.
      That's why we simply turn normal Draw/BeforeDraw temporarily off. }
    OldDraw := Window.OnDraw;
    OldBeforeDraw := Window.OnBeforeDraw;
    Window.OnDraw := nil;
    Window.OnBeforeDraw := nil;
    try
      { For now we construct and store octrees only for the 1st animation frame. }

      Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
      Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
      Scene.Spatial := [ssRendering, ssDynamicCollisions];
    finally
      Window.OnDraw := OldDraw;
      Window.OnBeforeDraw := OldBeforeDraw;
    end;
  end;
end;

procedure SceneOctreeFree;
begin
  if Scene <> nil then
  begin
    { Since we destroy our PVRMLTriangles, make sure Scene
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

  SceneFileName := '';

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
  { Always disable ProcessEvents for TVRMLGLAnimation consisting of many models. }
  Value := ProcessEventsWanted and (SceneAnimation.ScenesCount = 1);

  for I := 0 to SceneAnimation.ScenesCount - 1 do
    SceneAnimation.Scenes[I].ProcessEvents := Value;
end;

procedure LoadClearScene; forward;

{ Calls FreeScene and then inits "scene global variables".
  Pass here ACameraRadius = 0.0 to say that CameraRadius should be
  somehow calculated (guessed) based on loaded Scene data.

  Camera settings for scene are inited from VRML defaults and
  from camera node in scene.

  Exceptions: if this function will raise any exception you should assume
  that scene loading failed for some reason and "scene global variables"
  are set to their "null values". I.e. everything is in a "clean" state
  like after FreeScene.

  This procedure does not really open any file
  (so ASceneFileName need not be a name of existing file,
  in fact it does not need to even be a valid filename).
  Instead in uses already created RootNode to init
  "scene global variables".

  Note that all RootNodes[] will be owned by Scene.
  So do not Free RootNodes[] items after using this procedure
  (still, you should use RootNodes list itself).

  Note that this may change the value of Times list.

  Note that there is one "scene global variable" that will
  not be completely handled by this procedure:
  SceneWarnings. During this procedure some VRML warnings may
  occur and be appended to SceneWarnings. You have to take care
  about the rest of issues with the SceneWarnings, like clearing
  them before calling LoadSceneCore.

  ASceneFileName is not const parameter, to allow you to pass
  SceneFileName as ASceneFileName. If ASceneFileName would be const
  we would have problem, because FreeScene modifies SceneFileName
  global variable, and this would change ASceneFileName value
  (actually, making it possibly totally invalid pointer,
  pointing at some other place of memory). That's always the
  problem with passing pointers to global variables
  (ASceneFileName is a pointer) as local vars.

  If UseInitialNavigationType then we will use and reset
  InitialNavigationType. This should be @false if you're only loading
  temporary scene, like LoadClearScene. }
procedure LoadSceneCore(
  RootNodes: TVRMLNodesList;
  ATimes: TDynSingleArray;
  ScenesPerTime: Cardinal;
  const EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;

  ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  InitializeCamera: boolean;

  UseInitialNavigationType: boolean = true);

  procedure ScaleAll(A: TDynSingleArray; const Value: Single);
  var
    I: Integer;
  begin
    for I := 0 to A.High do
      A.Items[I] *= Value;
  end;

var
  NewCaption: string;
  I: Integer;
  ForceNavigationType: string;
begin
  FreeScene;

  try
    SceneFileName := ASceneFileName;

    if AnimationTimeSpeedWhenLoading <> 1.0 then
      ScaleAll(ATimes, 1 / AnimationTimeSpeedWhenLoading);

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

    if UseInitialNavigationType then
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
      NewCaption := ExtractFileName(SceneFilename);
    NewCaption := SForCaption(NewCaption) + ' - view3dscene';
    if Window.Closed then
      Window.Caption := NewCaption else
      Window.FPSBaseCaption := NewCaption;

    UpdateCameraUI;

    SceneOctreeCreate;

    for I := 0 to SceneAnimation.ScenesCount - 1 do
    begin
      { Order is somewhat important here: first turn DebugLogVRMLChanges on,
        then turn events on, otherwise events on initialize() of scripts
        will not be logged. }
      SceneAnimation.Scenes[I].LogChanges := DebugLogVRMLChanges;

      SceneAnimation.Scenes[I].OnGeometryChanged := @THelper(nil).GeometryChanged;
      SceneAnimation.Scenes[I].OnViewpointsChanged := @THelper(nil).ViewpointsChanged;
      SceneAnimation.Scenes[I].OnPointingDeviceSensorsChange := @THelper(nil).PointingDeviceSensorsChange;

      { regardless of ProcessEvents, we may change the vrml graph,
        e.g. by Edit->Material->... }
      SceneAnimation.Scenes[I].Static := SceneAnimation.ScenesCount <> 1;
    end;

    UpdateProcessEvents;

    { Make initial CameraChanged to make initial events to
      ProximitySensor, if user is within. }
    Scene.CameraChanged(SceneManager.Camera, SceneManager.CameraToChanges);

    if not Window.Closed then
    begin
      { call EventResize to adjust zNear/zFar of our projection to the size
        of Scene.BoundingBox }
      Window.EventResize;
      Scene.VisibleChangeHere([]);
    end;

    if MenuReopen <> nil then
      MenuReopen.Enabled := SceneFileName <> '';
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
    MessageOK(Window, S, taLeft) else
    Writeln(ErrOutput, 'view3dscene: ', S);
end;

{ This loads the scene from file (using LoadVRMLSequence) and
  then inits our scene variables by LoadSceneCore.

  If it fails, it tries to preserve current scene
  (if it can't preserve current scene, only then it resets it to clear scene).
  Also, it shows the error message using MessageOK
  (so Glw must be already open).

  It may seem that ASceneFileName could be constant parameter here.
  Yes, it could. However, you will sometimes want to pass here
  SceneFileName global value and this would cause memory havoc
  (parameter is passed as const, however when global variable
  SceneFileName is changed then the parameter value implicitly
  changes, it may even cause suddenly invalid pointer --- yeah,
  I experienced it). }
procedure LoadScene(ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  InitializeCamera: boolean);

{ It's useful to undefine it only for debug purposes:
  FPC dumps then backtrace of where exception happened,
  which is often enough to trace the error.
  In release versions this should be defined to produce a nice
  message box in case of errors (instead of just a crash). }
{$define CATCH_EXCEPTIONS}

var
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;
  SavedSceneWarnings: TSceneWarnings;
begin
  RootNodes := TVRMLNodesList.Create;
  Times := TDynSingleArray.Create;
  try
    { We have to clear SceneWarnings here (not later)
      to catch also all warnings raised during parsing the VRML file.
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
        LoadVRMLSequence(ASceneFileName, true,
          RootNodes, Times, ScenesPerTime, EqualityEpsilon,
          TimeLoop, TimeBackwards);
      {$ifdef CATCH_EXCEPTIONS}
      except
        on E: Exception do
        begin
          LoadErrorMessage('Error while loading scene from "' +ASceneFileName+ '": ' +
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
        ASceneFileName, SceneChanges, ACameraRadius, InitializeCamera);
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
          our Draw routine works OK when it's called to draw background
          under MessageOK. }
        LoadClearScene;
        LoadErrorMessage('Error while loading scene from "' + ASceneFileName + '": ' +
          E.Message);
        Exit;
      end;
    end;
    {$endif CATCH_EXCEPTIONS}

    { For batch operation (making screenshots), do not save the scene
      on "recent files" menu. This also applies when using view3dscene
      as a thumbnailer. }
    if not MakingScreenShot then
      RecentMenu.Add(ASceneFileName);

    { We call PrepareResources to make SceneAnimation.PrepareResources to gather
      VRML warnings (because some warnings, e.g. invalid texture filename,
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
  This loads a scene directly from TVRMLNode, and assumes that
  LoadSceneCore will not fail. }
procedure LoadSimpleScene(Node: TVRMLNode;
  UseInitialNavigationType: boolean = true);
var
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  ScenesPerTime: Cardinal;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;
begin
  RootNodes := TVRMLNodesList.Create;
  Times := TDynSingleArray.Create;
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
      UseInitialNavigationType);
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
  { As a clear scene, I'm simply loading an empty VRML file.
    This way everything seems normal: SceneAnimation is Loaded,
    FirstScene is available and FirstScene.RootNode is non-nil.

    The other idea was to use some special state like Loaded = @false
    to indicate clear scene, but this would only complicate code
    with checks for "if Loaded" everywhere.

    Also, non-empty clear scene allows me to put there WorldInfo with a title.
    This way clear scene has an effect on view3dscene window's title,
    and at the same time I don't have to set SceneFileName to something
    dummy.

    I'm not constructing here RootNode in code (i.e. Pascal).
    This would allow a fast implementation, but it's easier for me to
    design scene in pure VRML and then auto-generate
    xxx_scene.inc file to load VRML scene from a simple string. }
  LoadSimpleScene(LoadVRMLClassicFromString({$I clear_scene.inc}, ''), false);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
begin
  LoadSimpleScene(LoadVRMLClassicFromString({$I welcome_scene.inc}, ''));
end;

const
  SaveGenerator = 'view3dscene, http://vrmlengine.sourceforge.net/view3dscene.php';

{ Load model from ASceneFileName ('-' means stdin),
  do SceneChanges, and write it as VRML/X3D to stdout.
  This is used to handle --write command-line option. }
procedure WriteModel(const ASceneFileName: string;
  const SceneChanges: TSceneChanges; const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean);
var
  Node: TVRMLNode;
begin
  Node := LoadVRML(ASceneFileName, true);
  try
    Node := ChangeNode(SceneChanges, Node);
    SaveVRML(Node, StdOutStream, SaveGenerator,
      ExtractFileName(ASceneFileName), Encoding, ForceConvertingToX3D);
  finally FreeAndNil(Node) end;
end;

class procedure THelper.OpenRecent(const FileName: string);
begin
  LoadScene(FileName, [], 0.0, true);
end;

class procedure THelper.GeometryChanged(Scene: TVRMLScene;
  const SomeLocalGeometryChanged: boolean;
  OnlyShapeChanged: TVRMLShape);
begin
  if SomeLocalGeometryChanged then
  begin
    if (OnlyShapeChanged = nil) or
       (OnlyShapeChanged = SelectedShape) then
      { Our PVRMLTriangle pointer is possibly invalid now. }
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
      [ SceneWarnings.Count, SceneFileName ]));
    S.Append('');
    S.AddStrings(SceneWarnings.Items);
    S.Append('');
    S.Append('You can always see the console or use File->"View warnings" menu command to view these warnings again.');
    MessageOK(Window, S, taLeft);
    WarningsButtonEnabled := false;
    UpdateWarningsButton;
  finally FreeAndNil(S) end;
end;

procedure AttributesLoadFromConfig(Attributes: TVRMLRenderingAttributes);
begin
  Attributes.LineWidth := ConfigFile.GetFloat('video_options/line_width',
    DefaultLineWidth);
  Attributes.PointSize := ConfigFile.GetFloat('video_options/point_size',
    DefaultPointSize);
end;

procedure AttributesSaveToConfig(Attributes: TVRMLRenderingAttributes);
begin
  ConfigFile.SetDeleteFloat('video_options/line_width',
    Attributes.LineWidth, DefaultLineWidth);
  ConfigFile.SetDeleteFloat('video_options/point_size',
    Attributes.PointSize, DefaultPointSize);
end;

{ make screen shots ---------------------------------------------------------- }

{ This performs all screenshot takes, as specified in ScreenShotsList.
  It is used both for batch mode screenshots (--screenshot, --screenshot-range)
  and interactive (menu items about screenshots) operation. }
procedure MakeAllScreenShots(ReadBuffer: TGLenum);
var
  I, J: Integer;
  OldProgressUserInterface: TProgressUserInterface;
  OldTime: TKamTime;
  Image: TRGBImage;
begin
  { Save global things that we change, to restore them later.
    This isn't needed for batch mode screenshots, but it doesn't hurt
    to be clean. }
  OldProgressUserInterface := Progress.UserInterface;
  OldTime := SceneAnimation.Time;
  try
    SceneManager.BeforeDraw;

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
          ViewportsDraw;
          SceneManager.Draw;
          glFlush();
          Image := SaveScreen_NoFlush(0, 0, Window.Width, Window.Height, ReadBuffer);
          try
            SaveImage(Image, ScreenShotsList[I].UseFileName(J));
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

{ menu things ------------------------------------------------------------ }

const
  Version = '3.10.0';
  DisplayProgramName = 'view3dscene';

type
  TShaderAdder = class
    ProgramNode: TNodeComposedShader;
    Added: boolean;
    procedure AddShader(Node: TVRMLNode);
  end;

procedure TShaderAdder.AddShader(Node: TVRMLNode);
var
  ShadersField: TMFNode;
begin
  ShadersField := (Node as TNodeAppearance).FdShaders;
  if not ((ShadersField.Count = 1) and (ShadersField.Items[0] = ProgramNode)) then
  begin
    ShadersField.Clear;
    ShadersField.Add(ProgramNode);
    Added := true;
  end;
end;

procedure UpdateStatusToolbarVisible; forward;

procedure MenuCommand(Window: TGLWindow; MenuItem: TMenuItem);

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
      'This vector must not be zero vector.',
      Answer, taLeft) then
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
     Window.PostRedisplay;
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
      if MessageInputQuery(Window, 'New move speed (units per second):', MoveSpeed, taLeft) then
      begin
        Camera.Walk.MoveSpeed := MoveSpeed;
        Window.PostRedisplay;
      end;
    end else
      MessageOK(Window, SNavigationClassWalkNeeded);
  end;

  procedure ShowAndWrite(const S: string);
  begin
    Writeln(S);
    MessageOK(Window, S, taLeft);
  end;

  procedure ChangePointSize;
  var
    Value: Single;
  begin
    Value := SceneAnimation.Attributes.PointSize;
    if MessageInputQuery(Window, 'Change point size:', Value, taLeft) then
      SceneAnimation.Attributes.PointSize := Value;
  end;

  procedure ChangeLineWidth;
  var
    Value: Single;
  begin
    Value := SceneAnimation.Attributes.LineWidth;
    if MessageInputQuery(Window, 'Change line width:', Value, taLeft) then
      SceneAnimation.Attributes.LineWidth := Value;
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
      'VRML / X3D file, e.g. by VRML interpolators) this is perfect, ' +
      'animation always remains smooth.' +nl+
      nl+
      'New "on display" playing speed:',
      S, taLeft) then
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
      'VRML / X3D file, e.g. by VRML interpolators) this has no effect, ' +
      'as no frames are precalculated at loading. Use "on display" playing speed ' +
      'instead.' +nl+
      nl+
      'New "on loading" playing speed:',
      AnimationTimeSpeedWhenLoading, taLeft);
  end;

  procedure SelectedShowInformation;
  var
    s, TextureDescription: string;
    VCOver, TCOver, VCNotOver, TCNotOver: Cardinal;
    M1: TNodeMaterial_1;
    M2: TNodeMaterial;
    SelectedShape: TVRMLShape;
    SelectedGeometry: TVRMLGeometryNode;
    Tex: TNodeX3DTextureNode;
  begin
    if SelectedItem = nil then
    begin
      s := 'Nothing selected.';
    end else
    begin
      SelectedShape := TVRMLShape(SelectedItem^.Shape);
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
            Box3DToNiceStr(SelectedShape.BoundingBox)]);

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
        { This is VRML 2.0 node }
        M2 := SelectedItem^.State.ShapeNode.Material;
        if M2 <> nil then
        begin
          S += Format(
                 'Material (VRML >= 2.0):' +nl+
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
            'Material (VRML <= 1.0):' +nl+
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
    ShadowingItem: PVRMLTriangle;
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
           [ I, Lights.Items[i].Node.NiceName ]);

         ShadowingItem := SceneOctreeCollisions.SegmentCollision(
           SelectedPointWorld, Lights.Items[i].Location,
             false, SelectedItem, true, nil);

         if ShadowingItem <> nil then
         begin
          s += Format('no, this light is blocked by triangle %s from shape %s.',
            [ TriangleToNiceStr(ShadowingItem^.World.Triangle),
              TVRMLShape(ShadowingItem^.Shape).NiceName ])
         end else
          s += 'yes, no object blocks this light, it shines on selected point.';
        end;
    end;

    ShowAndWrite(S);
  end;

  procedure RemoveSelectedGeometry;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.Geometry, so this is only for the frame where
        octree is available. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).', taLeft);
      Exit;
    end;

    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
    end else
    begin
      Scene.RemoveShapeGeometry(TVRMLShape(SelectedItem^.Shape));
    end;
  end;

  procedure RemoveSelectedFace;

    function MFNonEmpty(Field: TDynLongIntArray): boolean;
    begin
      Result := (Field <> nil) and (Field.Count > 0) and
        { Single "-1" value in an MF field is the VRML 1.0 default
          weird value for normalIndex, materialIndex and textureCoordIndex
          fields. We treat it like an empty field, otherwise we wouldn't
          be able to process most VRML 1.0 files. }
        (not ((Field.Count = 1) and (Field.Items[0] = -1)));
    end;

  var
    Geometry: TVRMLGeometryNode;
    Colors, Coords, Materials, Normals, TexCoords: TDynLongIntArray;
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
      MessageOK(Window, 'The selected triangle is not part of IndexedFaceSet node.');
      Exit;
    end;

    Geometry := TVRMLShape(SelectedItem^.Shape).OriginalGeometry;

    if Geometry is TNodeIndexedFaceSet_1 then
    begin
      Colors := nil;
      CoordsField := TNodeIndexedFaceSet_1(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := TNodeIndexedFaceSet_1(Geometry).FdMaterialIndex.Items;
      Normals := TNodeIndexedFaceSet_1(Geometry).FdNormalIndex.Items;
      TexCoordsField := TNodeIndexedFaceSet_1(Geometry).FdTextureCoordIndex;
      TexCoords := TexCoordsField.Items;
    end else
    if Geometry is TNodeIndexedFaceSet then
    begin
      Colors := TNodeIndexedFaceSet(Geometry).FdColorIndex.Items;
      CoordsField := TNodeIndexedFaceSet(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := nil;
      Normals := TNodeIndexedFaceSet(Geometry).FdNormalIndex.Items;
      TexCoordsField := TNodeIndexedFaceSet(Geometry).FdTexCoordIndex;
      TexCoords := TexCoordsField.Items;
    end else
    if Geometry is TNodeIndexedTriangleMesh_1 then
    begin
      Colors := nil;
      CoordsField := TNodeIndexedTriangleMesh_1(Geometry).FdCoordIndex;
      Coords := CoordsField.Items;
      Materials := TNodeIndexedTriangleMesh_1(Geometry).FdMaterialIndex.Items;
      Normals := TNodeIndexedTriangleMesh_1(Geometry).FdNormalIndex.Items;
      TexCoordsField := TNodeIndexedTriangleMesh_1(Geometry).FdTextureCoordIndex;
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

    { calculate IndexBegin and IndexCount. Remember that after
      1st call, SelectedItem pointer may become invalid. }
    IndexBegin := SelectedItem^.Face.IndexBegin;
    IndexCount := SelectedItem^.Face.IndexEnd -
                  SelectedItem^.Face.IndexBegin + 1;

    Coords.Delete(IndexBegin, IndexCount);
    Scene.ChangedField(CoordsField);

    { Texture coordinates, if not empty, have always (both in VRML 1.0
      and VRML 2.0 IndexedFaceSet nodes, and in IndexedTriangleMesh
      from Inventor) the same ordering as coordIndex.
      So we can remove equivalent texture coords in the same manner
      as we removed coords. }
    if TexCoords <> nil then
    begin
      TexCoords.Delete(IndexBegin, IndexCount);
      Scene.ChangedField(TexCoordsField);
    end;
  end;

  { Returns @true and sets M1 and M2 (exactly one to @nil, one to non-nil)
    if success. Produces message to user and returns @false on failure.

    Note that SelectedItem is not necessarily correct anymore. Use only
    M1 and M2 pointers after this. }
  function ChangeMaterialInit(
    out M1: TNodeMaterial_1;
    out M2: TNodeMaterial): boolean;
  var
    Shape: TNodeX3DShapeNode;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem.State, so this is only for the frame where
        octree is available. Moreover, we call
        Scene.ChangedField. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).', taLeft);
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
        if MessageYesNo(Window, 'No material present. Add material to this node and then edit it?', taLeft) then
        begin
          { Note that this may remove old Shape.FdAppearance.Value,
            but only if Shape.Appearance = nil, indicating that
            something wrong was specified for "appearance" field.

            Similar, it may remove old Shape.Appearance.FdMaterial.Value,
            but only if Shape.Material was nil, and together
            this indicates that something incorrect was placed in "material"
            field. }

          M2 := TNodeMaterial.Create('', Shape.WWWBasePath);
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
    M1: TNodeMaterial_1;
    M2: TNodeMaterial;
    Color: TVector3Single;
  begin
    if not ChangeMaterialInit(M1, M2) then Exit;

    if M2 <> nil then
      Color := M2.FdDiffuseColor.Value else
    begin
      Assert(M1 <> nil);
      if M1.FdDiffuseColor.Count > 0 then
        Color := M1.FdDiffuseColor.Items.Items[0] else
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
    M1: TNodeMaterial_1;
    M2: TNodeMaterial;
    Color: TVector3Single;
  begin
    if not ChangeMaterialInit(M1, M2) then Exit;

    if M2 <> nil then
      Color := M2.FdSpecularColor.Value else
    begin
      Assert(M1 <> nil);
      if M1.FdSpecularColor.Count > 0 then
        Color := M1.FdSpecularColor.Items.Items[0] else
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

  procedure ChangeLightModelAmbient;
  begin
    if Window.ColorDialog(LightModelAmbient) then LightModelAmbientChanged;
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
      'case sensitive (like all VRML).',
      Wildcard, taLeft) then
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

      MessageOK(Window, Format('Removed %d node instances.', [RemovedNumber]),
        taLeft);
    end;
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
       [ DEF_RAYTRACE_DEPTH,
         Window.Width, Window.Height,
         SceneFilename,
         ExtractOnlyFileName(SceneFilename) + '-rt.png',
         VectorToRawStr(Camera.Walk.Position),
         VectorToRawStr(Camera.Walk.Direction),
         VectorToRawStr(Camera.Walk.Up),
         BGColor[0], BGColor[1], BGColor[2] ]);
    if SceneManager.PerspectiveView then
      S += Format('    --view-angle-x %f', [SceneManager.PerspectiveViewAngles[0]]) else
      S += Format('    --ortho %f %f %f %f', [
        SceneManager.OrthoViewDimensions[0],
        SceneManager.OrthoViewDimensions[1],
        SceneManager.OrthoViewDimensions[2],
        SceneManager.OrthoViewDimensions[3] ]);
    Writeln(S);
  end;

  procedure WritelnCameraSettings(const Version: TVRMLCameraVersion;
    const Xml: boolean);
  var
    Pos, Dir, Up, GravityUp: TVector3Single;
  begin
    SceneManager.Camera.GetView(Pos, Dir, Up, GravityUp);
    Writeln(MakeVRMLCameraStr(Version, Xml, Pos, Dir, Up, GravityUp));
  end;

  procedure WriteBoundingBox(const Box: TBox3D);
  begin
    if IsEmptyBox3D(Box) then
      MessageOK(Window, 'The bounding box is empty.', taLeft) else
    begin
      Writeln(Format(
        '# ----------------------------------------' +nl+
        '# BoundingBox %s expressed in VRML:' +nl+
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
        [ Box3DToNiceStr(Box),
          VectorToRawStr(Box3DMiddle(Box)),
          FloatToRawStr(Box[1, 0] - Box[0, 0]),
          FloatToRawStr(Box[1, 1] - Box[0, 1]),
          FloatToRawStr(Box[1, 2] - Box[0, 2]) ]));
    end;
  end;

  procedure AssignGLSLShader;
  const
    VS_FileFilters =
    'All files|*|' +
    '*Vertex shader (*.vs)|*.vs';
    FS_FileFilters =
    'All files|*|' +
    '*Fragment shader (*.fs)|*.fs';
  var
    FragmentShaderUrl, VertexShaderUrl: string;
    ProgramNode: TNodeComposedShader;
    ShaderPart: TNodeShaderPart;
    ShaderAdder: TShaderAdder;
    I: Integer;
  begin
    VertexShaderUrl := '';
    if Window.FileDialog('Open vertex shader file', VertexShaderUrl, true,
      VS_FileFilters) then
    begin
      { We guess that FragmentShaderUrl will be in the same dir as vertex shader }
      FragmentShaderUrl := ExtractFilePath(VertexShaderUrl);
      if Window.FileDialog('Open fragment shader file', FragmentShaderUrl, true,
        FS_FileFilters) then
      begin
        ProgramNode := TNodeComposedShader.Create(
          { any name that has a chance to be unique }
          'view3dscene_shader_' + IntToStr(Random(1000)), '');
        ProgramNode.FdLanguage.Value := 'GLSL';

        ShaderPart := TNodeShaderPart.Create('', '');
        ProgramNode.FdParts.Add(ShaderPart);
        ShaderPart.FdType.Value := 'VERTEX';
        ShaderPart.FdUrl.Items.Add(VertexShaderUrl);

        ShaderPart := TNodeShaderPart.Create('', '');
        ProgramNode.FdParts.Add(ShaderPart);
        ShaderPart.FdType.Value := 'FRAGMENT';
        ShaderPart.FdUrl.Items.Add(FragmentShaderUrl);

        ShaderAdder := TShaderAdder.Create;
        try
          ShaderAdder.ProgramNode := ProgramNode;
          ShaderAdder.Added := false;

          SceneAnimation.BeforeNodesFree; { AddShader may remove old shader nodes }

          for I := 0 to SceneAnimation.ScenesCount - 1 do
          begin
            SceneAnimation.Scenes[I].RootNode.EnumerateNodes(TNodeAppearance,
              @ShaderAdder.AddShader, false);
          end;

          SceneAnimation.ChangedAll;

          if not ShaderAdder.Added then
          begin
            FreeAndNil(ProgramNode);
            MessageOK(Window, 'No shaders added.' +NL+
              'Hint: this feature adds shaders to Apperance.shaders field. ' +
              'So it requires VRML >= 2.0 models with Appearance nodes present, ' +
              'otherwise nothing will be added.',
              taLeft);
          end;
        finally FreeAndNil(ShaderAdder); end;
      end;
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
      SceneAnimation.Attributes.PureGeometry    := FillModes[FillMode].PureGeometry;
    end;
  end;

  procedure ScreenShotToVideo;
  var
    TimeBegin, TimeStep: TKamTime;
    FramesCount: Cardinal;
    FileNamePattern: string;
    Range: TRangeScreenShot;
  begin
    TimeBegin := SceneAnimation.Time;
    TimeStep := 0.04;
    FramesCount := 25;
    FileNamePattern := 'image%d.png';

    if MessageInputQuery(Window, 'Input start time for recording movie:',
      TimeBegin, taLeft) then
      if MessageInputQuery(Window, 'Time step between capturing movie frames:' +NL+NL+
        'Note that if you later choose to record to a single movie file, like "output.avi", then we''ll generate a movie with 25 frames per second. ' +
        'So if you want your movie to play with the same speed as animation in view3dscene then the default value, 1/25, is good.' +NL+NL+
        'Input time step between capturing movie frames:', TimeStep, taLeft) then
        if MessageInputQueryCardinal(Window, 'Input frames count to capture:', FramesCount, taLeft) then
          if Window.FileDialog('Images pattern or movie filename to save', FileNamePattern, false) then
          begin
            { ScreenShotsList should always be empty in interactive mode
              (otherwise some rendering behaves differently when
              MakingScreenShot = true) }
            Assert(ScreenShotsList.Count = 0);

            Range := TRangeScreenShot.Create;
            Range.TimeBegin := TimeBegin;
            Range.TimeStep := TimeStep;
            Range.FramesCount := FramesCount;
            Range.FileNamePattern := FileNamePattern;
            ScreenShotsList.Add(Range);

            try
              MakeAllScreenShots(GL_BACK);
            except
              on E: EInvalidScreenShotFileName do
                MessageOk(Window, 'Making screenshot failed: ' +NL+NL+ E.Message, taLeft);
            end;

            ScreenShotsList.FreeContents;
          end;
  end;

  procedure PrecalculateAnimationFromVRMLEvents;
  var
    ScenesPerTime: Cardinal;
    TimeBegin, TimeEnd: Single;
    RootNode: TVRMLNode;
  const
    EqualityEpsilon = 0.0001;
  begin
    if SceneAnimation.ScenesCount <> 1 then
    begin
      MessageOK(Window, 'This is not possible when you already have a precalculated animation (like loaded from Kanim or MD3 file).', taLeft);
      Exit;
    end;

    TimeBegin := 0;
    TimeEnd := 10;
    ScenesPerTime := 25;

    if MessageInputQuery(Window, 'This will "record" an interactive animation (done by VRML events, interpolators, sensors etc.) into a non-interactive precalculated animation. This allows an animation to be played ultra-fast, although may also be memory-consuming for long ranges of time.' +nl+
         nl+
         'World BEGIN time of recording:', TimeBegin, taLeft) and
       MessageInputQuery(Window,
         'World END time of recording:', TimeEnd, taLeft) and
       MessageInputQueryCardinal(Window,
         'Scenes per second (higher values make animation smoother but also more memory-consuming):', ScenesPerTime, taLeft) then
    begin
      { Note: there's an inherent problem here since RootNode starts
        with state from current Time. This includes
        TimeDependentNodeHandler state like IsActive, etc., but also
        the rest of VRML graph (e.g. if some events change some geometry
        or materials). While LoadFromVRMLEvents takes care to call
        SceneAnimation.ResetTime, this only resets time-dependent nodes and routes
        and the like, but it cannot at the same time deactivate-and-then-activate
        time-dependent nodes in the same timestamp (so e.g. TimeSensor just
        remains active, if it was active currently and is determined to be
        active during animation, without a pair of Active.Send(false) +
        Active.Send(true)). And it cannot revert whole VRML graph state.

        This is inherent to the fact that we take current RootNode,
        not the loaded one, so it cannot really be fixed --- we would have
        to just reload RootNode from file, since we cannot keep RootNode
        copy just for this purpose.

        So I just treat it silently as non-fixable in view3dscene,
        you have to load model with ProcessEvents = initially false
        to safely do LoadFromVRMLEvents. }

      { Extract RootNode. OwnsFirstRootNode set to false, to avoid
        freeing it when current animation is closed (which is done implicitly
        at the beginning of LoadFromVRMLEvents). }
      SceneAnimation.OwnsFirstRootNode := false;
      RootNode := Scene.RootNode;

      { Using LoadFromVRMLEvents will also Close the previous scene.
        Before doing this, we must always free our octrees
        (as Scene keeps references to our octrees). }
      SceneOctreeFree;

      { Root node will be owned by LoadFromVRMLEvents, so it will be freed }
      SceneAnimation.LoadFromVRMLEvents(RootNode, true,
        TimeBegin, TimeEnd, ScenesPerTime, EqualityEpsilon,
        'Precalculating animation');

      { Otherwise, current time is huge and it doesn't work reliably
        with division inside TVRMLGLAnimation.SceneFromTime.
        Do it *before* setting MainScene, as even setting MainScene
        may cause TVRMLGLAnimation.VisibleChangeNotification, which
        already requires TVRMLGLAnimation.SceneFromTime. }
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
    Shape: TVRMLShape;
  begin
    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.', taLeft);
    end else
    begin
      Shape := TVRMLShape(SelectedItem^.Shape);
      if Shape.OctreeTriangles = nil then
        MessageOk(Window, 'No collision octree was initialized for this shape.', taLeft) else
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
    FileNamePattern: string;
    Orientation: char;
    Size: Cardinal;
  begin
    Orientation := MessageChar(Window,
      'This function will save six separate image files that show cube map environment around you.' + NL +
      NL +
      'In a moment you will be asked to choose directory and base filename for saving these images, right now you have to decide how the cube map faces will be oriented and named. ("Names" of cube map faces will be placed instead of "%s" in image file pattern.)' + NL +
      NL +
      '[B] : VRML/X3D Background orientation (left/right/...)' + NL +
      '[O] : OpenGL orientation (positive/negative x/y/z)' + NL +
      '[D] : DirectX (and DDS) orientation (positive/negative x/y/z, in left-handed coord system)' + NL +
      NL +
      '[Escape] Cancel',
      ['b', 'o', 'd', CharEscape],
      'Press [B], [O], [D] or [Escape]',
      taLeft, true);

    if Orientation <> CharEscape then
    begin
      if SceneFileName <> '' then
        FileNamePattern := ExtractOnlyFileName(SceneFileName) + '_cubemap_%s.png' else
        FileNamePattern := 'view3dscene_cubemap_%s.png';

      if Window.FileDialog('Image name template to save', FileNamePattern, false) then
      begin
        Size := DefaultCubeMapSize;

        if MessageInputQueryCardinal(Window, 'Size of cube map images', Size, taLeft) then
        begin
          for Side := Low(Side) to High(Side) do
            CubeMapImg[Side] := TRGBImage.Create(Size, Size);

          GLCaptureCubeMapImages(CubeMapImg, SceneManager.Camera.GetPosition,
            @SceneManager.RenderFromViewEverything,
            SceneManager.ProjectionNear, SceneManager.ProjectionFar,
            true, 0, 0);
          glViewport(0, 0, Window.Width, Window.Height);

          case Orientation of
            'b':
              begin
                CubeMapImg[csPositiveX].Rotate(2);
                CubeMapImg[csNegativeX].Rotate(2);
                CubeMapImg[csPositiveZ].Rotate(2);
                CubeMapImg[csNegativeZ].Rotate(2);
                SaveImage(CubeMapImg[csPositiveX], Format(FileNamePattern, ['right']));
                SaveImage(CubeMapImg[csNegativeX], Format(FileNamePattern, ['left']));
                SaveImage(CubeMapImg[csPositiveY], Format(FileNamePattern, ['top']));
                SaveImage(CubeMapImg[csNegativeY], Format(FileNamePattern, ['bottom']));
                SaveImage(CubeMapImg[csPositiveZ], Format(FileNamePattern, ['back']));
                SaveImage(CubeMapImg[csNegativeZ], Format(FileNamePattern, ['front']));
              end;
            'o':
              begin
                { This is the most natural Orientation,
                  our csXxx names match OpenGL names and orientation. }
                SaveImage(CubeMapImg[csPositiveX], Format(FileNamePattern, ['positive_x']));
                SaveImage(CubeMapImg[csNegativeX], Format(FileNamePattern, ['negative_x']));
                SaveImage(CubeMapImg[csPositiveY], Format(FileNamePattern, ['positive_y']));
                SaveImage(CubeMapImg[csNegativeY], Format(FileNamePattern, ['negative_y']));
                SaveImage(CubeMapImg[csPositiveZ], Format(FileNamePattern, ['positive_z']));
                SaveImage(CubeMapImg[csNegativeZ], Format(FileNamePattern, ['negative_z']));
              end;
            'd':
              begin
                { Swap positive/negative y, since DirectX is left-handed. }
                SaveImage(CubeMapImg[csPositiveX], Format(FileNamePattern, ['positive_x']));
                SaveImage(CubeMapImg[csNegativeX], Format(FileNamePattern, ['negative_x']));
                SaveImage(CubeMapImg[csNegativeY], Format(FileNamePattern, ['positive_y']));
                SaveImage(CubeMapImg[csPositiveY], Format(FileNamePattern, ['negative_y']));
                SaveImage(CubeMapImg[csPositiveZ], Format(FileNamePattern, ['positive_z']));
                SaveImage(CubeMapImg[csNegativeZ], Format(FileNamePattern, ['negative_z']));
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
    FileName: string;
    Size: Cardinal;
  begin
    if SceneFileName <> '' then
      FileName := ExtractOnlyFileName(SceneFileName) + '_cubemap.dds' else
      FileName := 'view3dscene_cubemap.dds';

    if Window.FileDialog('Save image to file', FileName, false) then
    begin
      Size := DefaultCubeMapSize;

      if MessageInputQueryCardinal(Window, 'Size of cube map images', Size, taLeft) then
      begin
        DDS := GLCaptureCubeMapDDS(Size, SceneManager.Camera.GetPosition,
          @SceneManager.RenderFromViewEverything,
          SceneManager.ProjectionNear, SceneManager.ProjectionFar,
          true, 0, 0);
        try
          glViewport(0, 0, Window.Width, Window.Height);
          DDS.SaveToFile(FileName);
        finally FreeAndNil(DDS) end;
      end;
    end;
  end;

  procedure ScreenShotDepthToImage;

    procedure DoSave(const FileName: string);
    var
      PackData: TPackNotAlignedData;
      Image: TGrayscaleImage;
    begin
      { Just like TGLWindow.SaveScreen, we have to force redisplay now
        (otherwise we could be left here with random buffer contents from
        other window obscuring us, or we could have depth buffer from
        other drawing routine (like "frozen screen" drawn under FileDialog). }
      Window.EventBeforeDraw;
      Window.EventDraw;

      Image := TGrayscaleImage.Create(Window.Width, Window.Height);
      try
        BeforePackImage(PackData, Image);
        try
          glReadPixels(0, 0, Window.Width, Window.Height, GL_DEPTH_COMPONENT,
            ImageGLType(Image), Image.RawPixels);
        finally AfterPackImage(PackData, Image) end;

        SaveImage(Image, FileName);
      finally FreeAndNil(Image) end;
    end;

  var
    FileName: string;
  begin
    if SceneFileName <> '' then
      FileName := ExtractOnlyFileName(SceneFileName) + '_depth_%d.png' else
      FileName := 'view3dscene_depth_%d.png';
    FileName := FileNameAutoInc(FileName);

    if Window.FileDialog('Save depth to a file', FileName, false,
      SaveImage_FileFilters) then
      DoSave(FileName);
  end;

  procedure Raytrace;
  var
    Pos, Dir, Up: TVector3Single;
  begin
    SceneManager.Camera.GetView(Pos, Dir, Up);
    RaytraceToWin(Window, SceneManager.BaseLights, Scene,
      Pos, Dir, Up,
      SceneManager.PerspectiveView, SceneManager.PerspectiveViewAngles,
      SceneManager.OrthoViewDimensions, BGColor,
      Scene.FogNode);
  end;

  procedure MergeCloseVertexes;
  var
    Shape: TVRMLShape;
    Coord: TMFVec3f;
    MergeDistance: Single;
    MergedCount: Cardinal;
  begin
    { TODO: for now, we work with OriginalGeometry and OriginalState.
      So it doesn't work on Cone, Cylinder etc. that are converted
      to IndexedFaceSet in Proxy. Reason: well, Coord.Changed
      makes TVRMLScene.ChangedShapeFields which releases the very Coordinate
      node and fields that were changed... }

    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.Shape.GeometryOriginal, so this is only for the frame where
        octree is available. }
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).', taLeft);
      Exit;
    end;

    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.', taLeft);
      Exit;
    end;

    Shape := TVRMLShape(SelectedItem^.Shape);

    if not Shape.OriginalGeometry.Coord(Shape.OriginalState, Coord) then
    begin
      MessageOK(Window, 'Selected geometry node doesn''t have a coordinate field. Nothing to merge.', taLeft);
      Exit;
    end;

    if Coord = nil then
    begin
      MessageOK(Window, 'Selected geometry node''s has an empty coordinate field. Nothing to merge.', taLeft);
      Exit;
    end;

    MergeDistance := 0.01;
    if MessageInputQuery(Window, 'Input merge distance. Vertexes closer than this will be set to be exactly equal.',
      MergeDistance, taLeft, '0.01') then
    begin
      MergedCount := Coord.Items.MergeCloseVertexes(MergeDistance);
      if MergedCount <> 0 then
        Coord.Changed;
      MessageOK(Window, Format('Merged %d vertexes.', [MergedCount]), taLeft);
    end;
  end;

  procedure JumpToViewpoint(Viewpoint: TVRMLViewpointNode);
  var
    Pos, Dir, Up, GravityUp: TVector3Single;
  begin
    if Viewpoint = Scene.ViewpointStack.Top then
    begin
      { Sending set_bind = true works fine if it's not current viewpoint,
        otherwise nothing happens... So just explicitly go to viewpoint
        position. }
      Viewpoint.GetView(Pos, Dir, Up, GravityUp);
      Scene.CameraTransition(Camera, Pos, Dir, Up, GravityUp);
    end else
      Viewpoint.EventSet_Bind.Send(true, Scene.Time);
  end;

  procedure VisualizeHumanoids;
  var
    Vis: THumanoidVisualization;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      MessageOK(Window, 'This function is not available when you deal with ' +
        'precalculated animations (like from Kanim or MD3 files).', taLeft);
      Exit;
    end;

    Vis := THumanoidVisualization.Create;
    try
      Vis.JointVisualizationSize := Box3DAvgSize(Scene.BoundingBox, false, 1) / 20;
      Scene.RootNode.EnumerateNodes(TNodeHAnimHumanoid,
        @Vis.VisualizeHumanoid, false);
      MessageOK(Window, Format('%d H-Anim Humanoids (%d Joints inside) processed.',
        [Vis.HumanoidsProcessed, Vis.JointsProcessed]), taLeft);
      if Vis.HumanoidsProcessed <> 0 then
        Scene.ChangedAll;
    finally FreeAndNil(Vis) end;
  end;

  procedure SaveAs(const Encoding: TX3DEncoding; const MessageTitle: string;
    const ForceConvertingToX3D: boolean);
  var
    ProposedSaveName, Extension, FileFilters: string;
    SaveVersion: TVRMLVersion;
    Convertion: boolean;
  begin
    SaveVersion := SaveVRMLVersion(Scene.RootNode);
    Convertion := SaveVRMLWillConvertToX3D(SaveVersion, Encoding, ForceConvertingToX3D);

    if SceneAnimation.ScenesCount > 1 then
      if Convertion then
      begin
        MessageOK(Window, 'This is a precalculated animation (like from Kanim or MD3 file). Converting it from VRML to X3D is not supported.', taLeft);
        Exit;
      end else
        MessageOK(Window, 'Warning: this is a precalculated animation (like from Kanim or MD3 file). Saving it as VRML/X3D will only save it''s first frame.', taLeft);

    Extension := SaveVersion.FileExtension(Encoding, ForceConvertingToX3D);
    FileFilters := SaveVersion.FileFilters(Encoding, ForceConvertingToX3D);
    ProposedSaveName := ChangeFileExt(SceneFileName, Extension);

    if Window.FileDialog(MessageTitle, ProposedSaveName, false, FileFilters) then
    try
      if Convertion then
        Scene.BeforeNodesFree;

      SaveVRML(Scene.RootNode, ProposedSaveName,
        SaveGenerator, ExtractFileName(SceneFileName), SaveVersion, Encoding,
        ForceConvertingToX3D);

      if Convertion then
        Scene.ChangedAll;
    except
      on E: Exception do
      begin
        MessageOK(Window, 'Error while saving scene to "' +ProposedSaveName+
          '": ' + E.Message, taLeft);
      end;
    end;
  end;

var
  ProposedScreenShotName: string;
  C: Cardinal;
begin
 case MenuItem.IntData of
  10: THelper.OpenButtonClick(nil);

  12: Window.Close;

  15: begin
        { When reopening, then InitializeCamera parameter is false.
          In fact, this was the purpose of this InitializeCamera
          parameter: to set it to false when reopening, as this makes
          reopening more useful. }
        LoadScene(SceneFileName, [], 0.0, false);
      end;

  900: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), false);
  905: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), true);
  910: SaveAs(xeXML    , SRemoveMnemonics(MenuItem.Caption), true { doesn't matter });

  21: WarningsButton.DoClick;

  31: ChangeAnimation([scNoNormals], SceneAnimation);
  32: ChangeAnimation([scNoSolidObjects], SceneAnimation);
  33: ChangeAnimation([scNoConvexFaces], SceneAnimation);

  34: RemoveNodesWithMatchingName;

  42: VisualizeHumanoids;

  3500: with SceneAnimation do ShadowMaps := not ShadowMaps;
  3510..3519: SceneAnimation.Attributes.PercentageCloserFiltering :=
    TPercentageCloserFiltering(Ord(MenuItem.IntData) - 3510);
  3520: with SceneAnimation.Attributes do VisualizeDepthMap := not VisualizeDepthMap;
  3530:
    begin
      C := SceneAnimation.ShadowMapsDefaultSize;
      if MessageInputQueryCardinal(Window, 'Input default shadow map size :' + NL + '(should be a power of 2)', C, taLeft) then
      begin
        SceneAnimation.ShadowMapsDefaultSize := C;
      end;
    end;
  3540: with SceneAnimation.Attributes do VarianceShadowMaps := not VarianceShadowMaps;

  36: RemoveSelectedGeometry;
  37: RemoveSelectedFace;

  41: AssignGLSLShader;

  51: Scene.CameraTransition(Camera,
        DefaultVRMLCameraPosition[1],
        DefaultVRMLCameraDirection,
        DefaultVRMLCameraUp,
        DefaultVRMLGravityUp);
  52: Scene.CameraTransition(Camera,
        DefaultVRMLCameraPosition[2],
        DefaultVRMLCameraDirection,
        DefaultVRMLCameraUp,
        DefaultVRMLGravityUp);

  53: SetViewpointForWholeScene(2, 1, false, true);
  54: SetViewpointForWholeScene(2, 1, true , true);
  55: SetViewpointForWholeScene(0, 1, false, true);
  56: SetViewpointForWholeScene(0, 1, true , true);

  57: SetViewpointForWholeScene(0, 2, false, true);
  58: SetViewpointForWholeScene(0, 2, true , true);
  59: SetViewpointForWholeScene(1, 2, false, true);
  60: SetViewpointForWholeScene(1, 2, true , true);

  82: ShowBBox := not ShowBBox;
  84: if Window.ColorDialog(BGColor) then BGColorChanged;
  86: with SceneAnimation.Attributes do Blending := not Blending;
  88: with SceneAnimation.Attributes do UseOcclusionQuery := not UseOcclusionQuery;
  89: with SceneAnimation.Attributes do BlendingSort := not BlendingSort;
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
         MessageOk(Window, SOnlyWhenOctreeAvailable, taLeft);
  103: if SceneOctreeRendering <> nil then
         Writeln(SceneOctreeRendering.Statistics) else
         MessageOk(Window, SOnlyWhenOctreeAvailable, taLeft);
  102: SceneAnimation.WritelnInfoNodes;

  105: PrintRayhunterCommand;

  106: WritelnCameraSettings(1, false);
  107: WritelnCameraSettings(2, false);
  108: WritelnCameraSettings(2, true);

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
         if Camera.NavigationType = High(TCameraNavigationType) then
           Camera.NavigationType := Low(TCameraNavigationType) else
           Camera.NavigationType := Succ(Camera.NavigationType);
         ViewportsSetNavigationType(Camera.NavigationType);
         UpdateCameraUI;
       end;

  121: begin
         ShowAndWrite(
           'Scene "' + SceneFilename + '" information:' + NL + NL +
           SceneAnimation.Info(true, true, false));
       end;
  122: begin
         ShowStatus := not ShowStatus;
         UpdateStatusToolbarVisible;
       end;
  123: SetCollisions(not SceneAnimation.Collides, false);
  124: ChangeGravityUp;
  125: Raytrace;
  126: (Window as TGLUIWindow).SwapFullScreen;
  127: begin
         if SceneFileName <> '' then
           ProposedScreenShotName := ExtractOnlyFileName(SceneFileName) + '_%d.png' else
           ProposedScreenShotName := 'view3dscene_screen_%d.png';
         Window.SaveScreenDialog(FileNameAutoInc(ProposedScreenShotName));
       end;
  128: begin
         Camera.Walk.MouseLook := not Camera.Walk.MouseLook;

         if Camera.Walk.MouseLook then
         begin
           Camera.Walk.Input_LeftStrafe.AssignFromDefault(Camera.Walk.Input_LeftRot);
           Camera.Walk.Input_RightStrafe.AssignFromDefault(Camera.Walk.Input_RightRot);
           Camera.Walk.Input_LeftRot.AssignFromDefault(Camera.Walk.Input_LeftStrafe);
           Camera.Walk.Input_RightRot.AssignFromDefault(Camera.Walk.Input_RightStrafe);
         end else
         begin
           Camera.Walk.Input_LeftStrafe.MakeDefault;
           Camera.Walk.Input_RightStrafe.MakeDefault;
           Camera.Walk.Input_LeftRot.MakeDefault;
           Camera.Walk.Input_RightRot.MakeDefault;
         end;
       end;

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
           '[http://vrmlengine.sourceforge.net/view3dscene.php]' + NL +
           NL +
           'Compiled with ' + SCompilerDescription +'.');
       end;

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
  210: begin
         Camera.IgnoreAllInputs := not Camera.IgnoreAllInputs;
         UpdateCameraNavigationTypeUI;
       end;

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

  225: PrecalculateAnimationFromVRMLEvents;

  300: JumpToViewpoint((MenuItem as TMenuItemViewpoint).Viewpoint);

  350..370:
    begin
      ScreenEffects.ActiveEffectsRecalculate;
      { All that is needed to actually render with the new effect is to
        actually redisplay. }
      Window.PostRedisplay;
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

  600..649: AntiAliasing := MenuItem.IntData - 600;

  710: ChangeMaterialDiffuse;
  720: ChangeMaterialSpecular;
  730: MergeCloseVertexes;

  740: ShadowsPossibleWanted := not ShadowsPossibleWanted;
  750: ShadowsOn := not ShadowsOn;
  760: DrawShadowVolumes := not DrawShadowVolumes;

  770: InitialShowBBox := not InitialShowBBox;
  771: InitialShowStatus := not InitialShowStatus;

  801: SoundEngine.Enable := not SoundEngine.Enable;
  810..850: SoundEngine.Device :=
    SoundEngine.Devices[MenuItem.IntData - 810].Name;

  1100..1199: SetTextureMinFilter(
    TTextureMinFilter  (MenuItem.IntData-1100), SceneAnimation);
  1200..1299: SetTextureMagFilter(
    TTextureMagFilter  (MenuItem.IntData-1200), SceneAnimation);
  1300..1399:
    begin
      Camera.NavigationType := TCameraNavigationType(MenuItem.IntData - 1300);
      ViewportsSetNavigationType(Camera.NavigationType);
      UpdateCameraUI;
    end;
  1400..1499: SceneAnimation.Attributes.BumpMapping :=
    TBumpMapping(MenuItem.IntData - 1400);
  1600..1699: SetTextureModeRGB(
    TTextureMode(MenuItem.IntData-1600), SceneAnimation);
  3600..3610: SetViewportsConfig(TViewportsConfig(MenuItem.IntData - 3600),
    View3DScene.Window, SceneManager);
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
    Mode: TCameraNavigationType;
    Group: TMenuItemRadioGroup;
  begin
    Group := M.AppendRadioGroup(CameraNames, 1300, Ord(Camera.NavigationType), true);
    for Mode := Low(Mode) to High(Mode) do
      CameraRadios[Mode] := Group.Items[Ord(Mode)];
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
          IsDefault := BlendingFactors[I].Value = DefaultBlendingSourceFactor else
          IsDefault := BlendingFactors[I].Value = DefaultBlendingDestinationFactor;
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
     M2.Append(TMenuItemChecked.Create('_Shadow Volumes Possible (Restart view3dscene to Apply)',
       740, ShadowsPossibleWanted, true));
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
     M2.Append(TMenuItem.Create('Default Background Color ...',    84));
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
     M2.AppendRadioGroup(PCFNames, 3510,
       Ord(SceneAnimation.Attributes.PercentageCloserFiltering), true);
     M2.Append(TMenuSeparator.Create);
     M2.Append(TMenuItemChecked.Create('Visualize Depths', 3520, SceneAnimation.Attributes.VisualizeDepthMap, true));
     M2.Append(TMenuSeparator.Create);
     M2.Append(TMenuItem.Create('Set Default Shadow Map Size ...', 3530));
     M2.Append(TMenuSeparator.Create);
     M2.Append(TMenuItemChecked.Create('Variance Shadow Maps (Experimental)', 3540, SceneAnimation.Attributes.VarianceShadowMaps, true));
     M.Append(M2);
   MenuShadowVolumes := TMenu.Create('Shadow Volumes');
     MenuShadowVolumes.Enabled := ShadowsPossibleCurrently;
     MenuShadowVolumes.Append(TMenuItemChecked.Create('Enable (Requires Light With kambiShadowsMain)', 750,
       ShadowsOn, true));
     MenuShadowVolumes.Append(TMenuItemChecked.Create('Draw shadow volumes', 760,
       DrawShadowVolumes, true));
     M.Append(MenuShadowVolumes);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create(
     '_Lighting Calculation',         91, CtrlL,
     SceneAnimation.Attributes.Lighting, true));
   MenuHeadlight := TMenuItemChecked.Create('_Head Light', 92, CtrlH,
     (Scene <> nil) and Scene.HeadlightOn, true);
   M.Append(MenuHeadlight);
   M.Append(TMenuItemChecked.Create('Use Scene Lights',    93,
     SceneAnimation.Attributes.UseSceneLights, true));
   M.Append(TMenuItem.Create('Light Global Ambient Color ...',  95));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('_Textures',           94, CtrlT,
     SceneAnimation.Attributes.EnableTextures, true));
   M2 := TMenu.Create('Texture Minification Method');
     MenuAppendTextureMinFilters(M2, 1100);
     M.Append(M2);
   M2 := TMenu.Create('Texture Magnification Method');
     MenuAppendTextureMagFilters(M2, 1200);
     M.Append(M2);
   M2 := TMenu.Create('RGB Textures Color Mode');
     MenuAppendTextureModeRGB(M2, 1600);
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
   M.Append(TMenuItemChecked.Create('Sort transparent shapes', 89,
     SceneAnimation.Attributes.BlendingSort, true));
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
   M.Append(TMenuItem.Create('Playing Speed Slower or Faster (on display) ...', 222));
   M.Append(TMenuItem.Create('Playing Speed Slower or Faster (on loading) ...', 223));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('Process VRML/X3D Events ("off" pauses also animation)', 224, ProcessEventsWanted, true));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Precalculate Animation from VRML/X3D Events ...', 225));
   Result.Append(M);
 M := TMenu.Create('_Edit');
   MenuRemoveSelectedGeometry :=
     TMenuItem.Create('Remove _Geometry Node (containing selected triangle)', 36);
   M.Append(MenuRemoveSelectedGeometry);
   MenuRemoveSelectedFace :=
     TMenuItem.Create('Remove _Face (containing selected triangle)', 37);
   M.Append(MenuRemoveSelectedFace);
   M.Append(TMenuItem.Create(
     'Remove VRML/X3D Nodes with Name Matching ...', 34));
   M.Append(TMenuSeparator.Create);
   MenuMergeCloseVertexes := TMenuItem.Create(
     'Merge Close Vertexes (of node with selected triangle) ...', 730);
   M.Append(MenuMergeCloseVertexes);
   M.Append(TMenuSeparator.Create);
   MenuEditMaterial := TMenu.Create('_Edit Material (of node with selected triangle)');
     MenuEditMaterial.Append(TMenuItem.Create('Diffuse Color ...' , 710));
     MenuEditMaterial.Append(TMenuItem.Create('Specular Color ...', 720));
   M.Append(MenuEditMaterial);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create(
     'Remove Normals Info from Scene (forces normals to be calculated)',
      31));
   M.Append(TMenuItem.Create('Mark All Shapes as '+
     'non-solid (disables any backface culling)', 32));
   M.Append(TMenuItem.Create('Mark All Faces as '+
     'non-convex (forces faces to be triangulated carefully)', 33));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create(
     'Simply Assign GLSL Shader to All Objects ...', 41));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Add Humanoids Joints Visualization', 42));
   Result.Append(M);
 M := TMenu.Create('_Console');
   M.Append(TMenuItem.Create('Print VRML _Info nodes',        102));
   M.Append(TMenuSeparator.Create);
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
   M.Append(TMenuItemChecked.Create('_Full Screen',           126, K_F11,
     Window.FullScreen, true));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('_Screenshot to Image ...',         127, K_F5));
   M.Append(TMenuItem.Create('Screenshot to Video / Multiple Images ...', 540));
   M.Append(TMenuItem.Create('Screenshot to _Cube Map (environment around camera position) ...',  550));
   M.Append(TMenuItem.Create('Screenshot to Cube Map DDS (environment around camera position) ...',  555));
   M.Append(TMenuItem.Create('Screenshot Depth to Grayscale Image ...', 560));
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
   M.Append(TMenuItem.Create('About view3dscene',                  131));
   Result.Append(M);
end;

{ toolbar -------------------------------------------------------------------- }

var
  OpenButton: TKamGLButton;

{ call when ShowStatus or MakingScreenShot changed }
procedure UpdateStatusToolbarVisible;
var
  NT: TCameraNavigationType;
  Vis: boolean;
begin
  Vis := ShowStatus and not MakingScreenShot;

  if ToolbarPanel <> nil then // check was CreateToolbar called already?
  begin
    ToolbarPanel.Exists := Vis;
    OpenButton.Exists := Vis;
    CollisionsButton.Exists := Vis;

    { Note that WarningsButton ignores the Vis.
      This is by design --- always signal warnings. }

    for NT := Low(NT) to High(NT) do
      if CameraButtons[NT] <> nil then
        CameraButtons[NT].Exists := Vis;

    { Call Resize after changing ToolbarPanel.Exists, as warnings button
      position is calculated differently based on it }
    if not Window.Closed then Window.EventResize;
  end;

  if StatusText <> nil then
    StatusText.Exists := Vis;
end;

procedure CreateStatusToolbar;
var
  NT: TCameraNavigationType;
const
  MinImageHeight = 22;
begin
  ToolbarPanel := TKamPanel.Create(Application);
  ToolbarPanel.Opacity := 0.8;
  ToolbarPanel.VerticalSeparators.Count := 2;
  Window.Controls.Insert(0, ToolbarPanel);

  StatusText := TStatusText.Create(Application);
  Window.Controls.Insert(0, StatusText);

  OpenButton := TKamGLButton.Create(Application);
  OpenButton.Caption := 'Open';
  OpenButton.OnClick := @THelper(nil).OpenButtonClick;
  OpenButton.Image := V3DSceneImages.Open;
  OpenButton.ImageAlphaTest := true;
  OpenButton.MinImageHeight := MinImageHeight;
  Window.Controls.Insert(0, OpenButton);

  CollisionsButton := TKamGLButton.Create(Application);
  CollisionsButton.Caption := 'Collisions';
  CollisionsButton.OnClick := @THelper(nil).CollisionsButtonClick;
  CollisionsButton.MinImageHeight := MinImageHeight;
  CollisionsButton.Toggle := true;
  if SceneAnimation <> nil then
    CollisionsButton.Pressed := SceneAnimation.Collides else
    CollisionsButton.Pressed := true { default value };
  Window.Controls.Insert(0, CollisionsButton);

  WarningsButton := TKamGLButton.Create(Application);
  WarningsButton.Caption := 'Warnings';
  WarningsButton.OnClick := @THelper(nil).WarningsButtonClick;
  WarningsButton.Image := Warning_icon;
  WarningsButton.MinImageHeight := MinImageHeight;
  Window.Controls.Insert(0, WarningsButton);

  if SceneWarnings <> nil then
    UpdateWarningsButton else
    WarningsButton.Exists := false; { at least initialize Exists }

  for NT := Low(NT) to High(NT) do
    { Don't show button for ntNone navigation type, it's confusing for new user.
      The "none" navigation type is visible in menu. }
    if NT <> ntNone then
    begin
      CameraButtons[NT] := TNavigationTypeButton.Create(Application, NT);
      CameraButtons[NT].Caption := CameraNames[NT];
      CameraButtons[NT].OnClick := @THelper(nil).NavigationTypeButtonClick;
      CameraButtons[NT].Toggle := true;
      CameraButtons[NT].MinImageHeight := MinImageHeight;
      Window.Controls.Insert(0, CameraButtons[NT]);
    end;

  CameraButtons[ntExamine].Image := V3DSceneImages.Examine;
  CameraButtons[ntWalk].Image := V3DSceneImages.Walk;
  CameraButtons[ntFly].Image := V3DSceneImages.Fly;

  UpdateStatusToolbarVisible;
end;

procedure Resize(Window: TGLWindow);
const
  ToolbarMargin = 5;  {< between buttons and toolbar panel }
  ButtonsMargin = 8; {< between buttons }
  ButtonsSeparatorsMargin = 8; {< between buttons and separators }
var
  NT: TCameraNavigationType;
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
      { check with <> nil, since for ntNone we don't show button now }
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
  end;

  WarningsButton.Left := Max(NextLeft,
    Window.Width - WarningsButton.Width - ToolbarMargin);
  WarningsButton.Bottom := ButtonsBottom;

  ResizeViewports(View3DScene.Window, SceneManager);
end;

class procedure THelper.OpenButtonClick(Sender: TObject);
var
  S: string;
begin
  S := ExtractFilePath(SceneFilename);
  if Window.FileDialog('Open file', s, true,
    LoadVRMLSequence_FileFilters) then
    LoadScene(s, [], 0.0, true);
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

{ initializing GL context --------------------------------------------------- }

procedure MultiSamplingOff(Window: TGLWindow; const FailureMessage: string);
begin
  AntiAliasing := 0;
  if AntiAliasingMenu[AntiAliasing] <> nil then
    AntiAliasingMenu[AntiAliasing].Checked := true;
  Writeln(FailureMessage);
end;

procedure StencilOff(Window: TGLWindow; const FailureMessage: string);
begin
  ShadowsPossibleCurrently := false;
  Writeln(FailureMessage);
end;

{ Call Window.Open, when anti-aliasing (multi-sampling) and shadows (stencil
  buffer) are possibly allowed. If EGLContextNotPossible, will try to lower
  requirements and initialize worse GL context. }
procedure OpenContext;
begin
  Window.OpenOptionalMultiSamplingAndStencil(@MultiSamplingOff, @StencilOff);
end;

{ main --------------------------------------------------------------------- }

var
  Param_CameraRadius: Single = 0.0;
  WasParam_Write: boolean = false;
  Param_WriteEncoding: TX3DEncoding = xeClassic;
  Param_WriteForceX3D: boolean = false;
  WasParam_SceneFileName: boolean = false;
  Param_SceneFileName: string;
  Param_SceneChanges: TSceneChanges = [];

const
  Options: array[0..17] of TOption =
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
    (Short:  #0; Long: 'debug-log-vrml-changes'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-cache'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-shaders'; Argument: oaNone),
    (Short:  #0; Long: 'anti-alias'; Argument: oaRequired),
    (Short: 'H'; Long: 'hide-extras'; Argument: oaNone),
    (Short:  #0; Long: 'write'; Argument: oaNone),
    (Short:  #0; Long: 'write-encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'write-force-x3d'; Argument: oaNone)
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
           'You can also convert models in other formats to VRML.' +NL+
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
           '  --camera-radius RADIUS' +NL+
           '                        Set camera sphere radius used for collisions' +NL+
           '                        and determinig moving speed' +NL+
           '  --scene-change-no-normals ,' +NL+
           '  --scene-change-no-solid-objects ,' +NL+
           '  --scene-change-no-convex-faces' +NL+
           '                        Change scene somehow after loading' +NL+
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
           '                        (not for older Inventor/VRML 1.0).' +NL+
           '  --write-to-vrml       Obsolete shortcut for "--write --write-encoding=classic"' +NL+
           CamerasOptionsHelp +NL+
           VRMLNodesDetailOptionsHelp +NL+
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
           '                        must then be specified like image%d.png).' +NL+
           '  --anti-alias AMOUNT   Use full-screen anti-aliasing.' +NL+
           '                        Argument AMOUNT is an integer >= 0.' +NL+
           '                        Exact 0 means "no anti-aliasing",' +NL+
           '                        this is the default. Each successive integer' +NL+
           '                        generally makes method one step better.' +NL+
           SoundEngine.ParseParametersHelp + NL+
           NL+
           TGLWindow.ParseParametersHelp(StandardParseOptions, true) +NL+
           NL+
           'Debug options:' +NL+
           '  --debug-log           Write log info to stdout.' +NL+
           '  --debug-log-cache     Write log info, including cache.' +nl+
           '  --debug-log-shaders   Write log info, including shader source and log.' +nl+
           '  --debug-log-vrml-changes' +nl+
           '                        Write log info, including VRML graph changes.' +nl+
           '                        This shows how VRML events are optimized.' +nl+
           NL+
           SVrmlEngineProgramHelpSuffix(DisplayProgramName, Version, true));
         ProgramBreak;
        end;
    6 : begin
         Writeln(Version);
         ProgramBreak;
        end;
    7 : begin
          SingleScreenShot := TSingleScreenShot.Create;
          SingleScreenShot.Time := StrToFloat(SeparateArgs[1]);
          SingleScreenShot.FileNamePattern := SeparateArgs[2];
          ScreenShotsList.Add(SingleScreenShot);
        end;
    8 : begin
          RangeScreenShot := TRangeScreenShot.Create;
          RangeScreenShot.TimeBegin := StrToFloat(SeparateArgs[1]);
          RangeScreenShot.TimeStep := StrToFloat(SeparateArgs[2]);
          RangeScreenShot.FramesCount := StrToInt(SeparateArgs[3]);
          RangeScreenShot.FileNamePattern := SeparateArgs[4];
          ScreenShotsList.Add(RangeScreenShot);
        end;
    9 : InitializeLog(Version);
    10: begin
          InitializeLog(Version);
          DebugLogVRMLChanges := true;
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
          { for proper menu display, we have to keep AntiAliasing
            within 0..MaxAntiAliasing range (although GLAntiAliasing
            unit accepts any cardinal value). }
          AntiAliasing := Clamped(StrToInt(Argument), 0, MaxAntiAliasing);
          if AntiAliasingMenu[AntiAliasing] <> nil then
            AntiAliasingMenu[AntiAliasing].Checked := true;
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
    else raise EInternalError.Create('OptionProc');
   end;
  end;

begin
  Window := TGLUIWindow.Create(Application);

  { parse parameters }
  Window.ParseParameters(StandardParseOptions);
  SoundEngine.ParseParameters;
  CamerasParseParameters;
  VRMLNodesDetailOptionsParse;
  ParseParameters(Options, @OptionProc, nil);
  { the most important param : filename to load }
  if Parameters.High > 1 then
   raise EInvalidParams.Create('Excessive command-line parameters. '+
     'Expected at most one filename to load') else
  if Parameters.High = 1 then
  begin
    WasParam_SceneFileName := true;
    Param_SceneFileName := Parameters[1];
  end;

  SceneManager := TV3DSceneManager.Create(nil);
  Window.Controls.Add(SceneManager);
  SceneManager.OnBoundViewpointChanged := @THelper(nil).BoundViewpointChanged;
  SceneManager.OnBoundNavigationInfoChanged := @THelper(nil).BoundNavigationInfoChanged;

  InitializeViewports(TV3DViewport);

  CreateStatusToolbar;

  Window.Controls.Add(ScreenEffects);

  SceneWarnings := TSceneWarnings.Create;
  try
    VRMLWarning := @DoVRMLWarning;
    DataWarning := @DoDataWarning;

    if WasParam_Write then
    begin
      if not WasParam_SceneFileName then
        raise EInvalidParams.Create('You used --write option, '+
          'this means that you want to convert some 3D model file to VRML/X3D. ' +
          'But you didn''t provide any filename on command-line to load.');
      WriteModel(Param_SceneFileName, Param_SceneChanges, Param_WriteEncoding,
        Param_WriteForceX3D);
      Exit;
    end;

    { This is for loading default clean scene.
      LoadClearScene should be lighting fast always,
      so progress should not be needed in this case anyway
      (and we don't want to clutter stdout). }
    Progress.UserInterface := ProgressNullInterface;

    { init "scene global variables" to null values }
    SceneAnimation := TVRMLGLAnimation.Create(nil);
    try
      AttributesLoadFromConfig(SceneAnimation.Attributes);
      SceneManager.Items.Add(SceneAnimation);

      InitCameras(SceneManager);
      InitTextureFilters(SceneAnimation);

      RecentMenu := TGLRecentFiles.Create(nil);
      RecentMenu.LoadFromConfig(ConfigFile, 'recent_files');
      RecentMenu.OnOpenRecent := @THelper(nil).OpenRecent;

      { init "scene global variables" to non-null values }
      LoadClearScene;
      try
        GLWinMessagesTheme := GLWinMessagesTheme_TypicalGUI;

        Window.GtkIconName := 'view3dscene';
        Window.MainMenu := CreateMainMenu;
        Window.OnMenuCommand := @MenuCommand;
        Window.OnOpen := @Open;
        Window.OnClose := @Close;
        Window.OnResize := @Resize;
        Window.OnMouseDown := @MouseDown;

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

        Window.SetDemoOptions(K_None, #0, true);

        if ShadowsPossibleWanted then
        begin
          Window.StencilBufferBits := 8;
          { Assignment below essentially copies
            ShadowsPossibleWanted to ShadowsPossibleCurrently.
            ShadowsPossibleCurrently may be eventually turned to @false
            by OpenContext. }
          ShadowsPossibleCurrently := true;
        end;
        Assert(ShadowsPossibleCurrently = ShadowsPossibleWanted);

        Window.MultiSampling := AntiAliasingGlwMultiSampling;

        OpenContext;

        if WasParam_SceneFileName then
          LoadScene(Param_SceneFileName, Param_SceneChanges, Param_CameraRadius, true) else
          LoadWelcomeScene;

        if MakingScreenShot then
        begin
          MakeAllScreenShotsFBO;
          Exit;
        end;

        Application.Run;
      finally FreeScene end;

      AttributesSaveToConfig(SceneAnimation.Attributes);
    finally
      FreeAndNil(SceneAnimation);
      if RecentMenu <> nil then
        RecentMenu.SaveToConfig(ConfigFile, 'recent_files');
      FreeAndNil(RecentMenu);
    end;
  finally
    FreeAndNil(SceneWarnings);
    FreeAndNil(SceneManager);
  end;
end.

{
  Local Variables:
  kam-compile-release-command-unix:    "./compile.sh && mv -fv view3dscene      ~/bin/"
  kam-compile-release-command-windows: "./compile.sh && mv -fv view3dscene.exe c:\\\\bin\\\\"
  End:
}
