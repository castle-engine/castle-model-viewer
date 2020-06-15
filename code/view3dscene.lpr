{
  Copyright 2002-2018 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ view3dscene, a VRML/X3D browser and general 3D model viewer.
  See [https://castle-engine.io/view3dscene.php] for user
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

{$I v3dsceneconf.inc}

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

{ Icon for Mac OS X. .res file managed by Lazarus. }
{$ifdef DARWIN}
  {$R *.res}
{$endif}

uses SysUtils, Math, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  { CGE units }
  CastleUtils, CastleVectors, CastleBoxes, CastleClassUtils,
  CastleTriangles, CastleApplicationProperties,
  CastleParameters, CastleProgress, CastleCameras, CastleOpenDocument, CastleConfig,
  CastleStringUtils, CastleFilesUtils, CastleTimeUtils,
  CastleLog, DateUtils, CastleFrustum,
  CastleImages, CastleCubeMaps, CastleCompositeImage, CastleTransform, CastleSoundEngine,
  CastleUIControls, CastleColors, CastleKeysMouse, CastleDownload, CastleURIUtils,
  CastleRays, CastleProjection, CastleVideos, CastleTextureImages,
  CastleLoadGltf,
  { OpenGL related units: }
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleWindow, CastleGLUtils, CastleMessages, CastleWindowProgress,
  CastleWindowRecentFiles, CastleGLImages, CastleGLCubeMaps, CastleComponentSerialize,
  CastleControls, CastleGLShaders, CastleControlsImages, CastleGLBoxes,
  { VRML/X3D (and possibly OpenGL) related units: }
  X3DFields, CastleInternalShapeOctree, X3DNodes, X3DLoad, CastleScene, X3DTriangles,
  CastleSceneCore, X3DCameraUtils, CastleInternalBackground,
  CastleRenderer, CastleShapes, CastleViewport,
  CastleMaterialProperties, CastleRendererBaseTypes,
  { view3dscene-specific units: }
  V3DSceneTextureFilters, V3DSceneLights, V3DSceneRaytrace,
  V3DSceneNavigationTypes, V3DSceneSceneChanges, V3DSceneBGColors, V3DSceneViewpoints,
  V3DSceneWarnings, V3DSceneFillMode,
  V3DSceneAntiAliasing, V3DSceneScreenShot, V3DSceneCaptions,
  V3DSceneShadows, V3DSceneOctreeVisualize, V3DSceneMiscConfig, V3DSceneImages,
  V3DSceneScreenEffects, V3DSceneSkeletonVisualize, V3DSceneViewports, V3DSceneVersion,
  V3DSceneLightsEditor, V3DSceneWindow, V3DSceneStatus, V3DSceneNamedAnimations,
  V3DSceneBoxes, V3DSceneInternalScenes, V3DSceneDialogBox;

var
  ShowFrustum: boolean = false;
  ShowFrustumAlwaysVisible: boolean = false;

  MenuCollisions: TMenuItemChecked;

  RecentMenu: TWindowRecentFiles;

  { Scene global variables.
    Modified only by LoadSceneCore (and all using it Load*Scene* procedures)
    and FreeScene.
    Note that Window.Caption also should be modified only by those procedures.

    Note that only one Scene object is created and present for the whole
    lifetime of this program. When we load new scene (from "Open"
    menu item) we DO NOT free and create new Scene object.
    Instead we only free and create underlying root node (TCastleScene.Load).
    This way we're preserving values of all Attributes.Xxx when opening new scene
    from "Open" menu item. }
  Scene: TCastleScene;
  SceneBoundingBox: TBoundingBoxScene;
  SceneDebugEdges: TDebugEdgesScene;
  SceneURL: string;

  SelectedItem: PTriangle;
  { SelectedPoint* always lies on SelectedItem item,
    and it's meaningless when SelectedItem = nil.
    World is in world coords,
    local in local shape (SelectedItem^.State.Transform) coords. }
  SelectedPointWorld, SelectedPointLocal: TVector3;
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
  MenuHideSelectedShape: TMenuItem;
  MenuEditMaterial: TMenu;
  MenuMergeCloseVertexes: TMenuItem;
  MenuHeadlight, MenuGravity, MenuMouseLook: TMenuItemChecked;
  MenuPreferGravityUpForRotations: TMenuItemChecked;
  MenuPreferGravityUpForMoving: TMenuItemChecked;
  MenuReopen: TMenuItem;
  MenuNamedAnimations: TMenuItemChecked;

  SceneWarnings: TSceneWarnings;

  { Does user want to process VRML/X3D events? Set by menu item.

    When false, this also makes Time stopped (just like
    AnimationTimePlaying = @false. IOW, always
    Scene.TimePlaying := AnimationTimePlaying and ProcessEventsWanted.)

    This is simpler for user --- ProcessEventsWanted=false is something
    stricly "stronger" than AnimationTimePlaying=false. Actually, the engine
    *could* do something otherwise, if we would allow Time to pass
    with ProcessEventsWanted=false: time-dependent  MovieTexture
    would still play, as this is not using events (also precalculated
    animation would play). }
  ProcessEventsWanted: boolean = true;

  { If ButtonWarnings.GetExists is allowed. If false, then ButtonWarnings.Exists
    should be false, regardless of warnings count. }
  ButtonWarningsEnabled: boolean = true;
  ButtonWarnings: TCastleButton;

  ToolbarPanel: TCastleUserInterface;
  ButtonCollisions: TCastleButton;
  ButtonAnimations: TCastleButton;

  AnimationTimePlaying: boolean = true;
  MenuAnimationTimePlaying: TMenuItemChecked;

  ControlsOnScreenshot: boolean = false;
  HideExtraScenesForScreenshot: boolean = false;

{ Helper class ---------------------------------------------------------------

  Some callbacks here must be methods (procedure of class),
  so we use this dummy class to contain them. }

type
  THelper = class
    class procedure OpenRecent(const URL: string);
    class procedure GeometryChanged(Scene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);
    class procedure ViewpointsChanged(Sender: TObject);
    class procedure BoundViewpointChanged(Sender: TObject);
    class procedure BoundNavigationInfoChanged(Sender: TObject);
    class procedure PointingDeviceSensorsChange(Sender: TObject);
    class procedure HeadlightOnChanged(Sender: TObject);
    class procedure ClickButtonWarnings(Sender: TObject);
    class procedure ClickNavigationTypeButton(Sender: TObject);
    class procedure ClickButtonOpen(Sender: TObject);
    class procedure ClickButtonCollisions(Sender: TObject);
    class procedure ClickButtonScreenshot(Sender: TObject);
    class procedure ClickButtonAnimations(Sender: TObject);
    class procedure OnWarningHandle(const Category, S: string);
  end;

{ Custom viewport class ------------------------------------------------ }

type
  TV3DViewport = class(TV3DShadowsViewport)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
    procedure Render3D(const Params: TRenderParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeRender; override;
    procedure Render; override;
    function GetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function ScreenEffectsCount: Integer; override;
    function ScreenEffectsNeedDepth: boolean; override;
    function Background: TBackground; override;
    function BaseLightsForRaytracer: TLightInstancesList;
  end;

procedure ViewportProperties(Viewport: TCastleViewport);
begin
  ViewportShadowsProperties(Viewport);
  Viewport.BackgroundWireframe := FillModes[FillMode].BackgroundWireframe;
end;

constructor TV3DViewport.Create(AOwner: TComponent);
begin
  inherited;
  PreventInfiniteFallingDown := true;
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

function TV3DViewport.BaseLightsForRaytracer: TLightInstancesList;
begin
  Result := TLightInstancesList.Create;
  InitializeLights(Result);
end;

{ Helper functions ----------------------------------------------------------- }

{ Show a multi-line message, allow to copy it to clipboard etc. }
procedure MessageReport(const S: string);
var
  Answer: char;
begin
  Answer := MessageChoice(Window, S,
    ['Copy To Clipboard (Ctrl + C)', 'Close (Enter)'],
    [CtrlC, {CtrlW,} CharEnter], hpLeft, false, true);
  if Answer = CtrlC then
    Clipboard.AsText := S + NL;
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
  if MenuHideSelectedShape <> nil then
    MenuHideSelectedShape.Enabled := SelectedItem <> nil;
  if MenuEditMaterial <> nil then
    MenuEditMaterial.Enabled := SelectedItem <> nil;
  if MenuMergeCloseVertexes <> nil then
    MenuMergeCloseVertexes.Enabled := SelectedItem <> nil;
end;

{ Currently used TCastleWalkNavigation,
  or @nil if we don't use TCastleWalkNavigation now. }
function CurrentWalkNavigation: TCastleWalkNavigation;
begin
  if (MainViewport <> nil) and
     (MainViewport.Navigation is TCastleWalkNavigation) then
    Result := TCastleWalkNavigation(MainViewport.Navigation)
  else
    Result := nil;
end;

{ Update menu items and buttons that reflect Camera properties }
procedure UpdateCameraUI;
var
  WalkNavigation: TCastleWalkNavigation;
begin
  UpdateCameraNavigationTypeUI;

  WalkNavigation := CurrentWalkNavigation;

  if MenuMouseLook <> nil then
    MenuMouseLook.Checked :=
      (WalkNavigation <> nil) and WalkNavigation.MouseLook;
  if MenuGravity <> nil then
    MenuGravity.Checked :=
      (WalkNavigation <> nil) and WalkNavigation.Gravity;
  if MenuPreferGravityUpForRotations <> nil then
    MenuPreferGravityUpForRotations.Checked :=
      (WalkNavigation <> nil) and WalkNavigation.PreferGravityUpForRotations;
  if MenuPreferGravityUpForMoving <> nil then
    MenuPreferGravityUpForMoving.Checked :=
      (WalkNavigation <> nil) and WalkNavigation.PreferGravityUpForMoving;
end;

{ Return currently used collisions octree.

  Note: When Scene.Collides = true, octree is always initialized
  (SceneOctreeCreate is called, and corresponding SceneOctreeDestroy
  was not).
  Otherwise, when Scene.Collides = false, octree *may* be available
  but doesn't have to. When setting Scene.Collides to false we do not
  immediately destroy the octree (in case user will just go back
  to Scene.Collides = true next), but it will be destroyed on next
  rebuild of octree (when we will just destroy old and not recreate new).
}
function SceneOctreeCollisions: TBaseTrianglesOctree;
begin
  if (Scene <> nil) and
     (Scene.InternalOctreeCollisions <> nil) then
    Result := Scene.InternalOctreeCollisions else
    Result := nil;
end;

function SceneOctreeRendering: TShapeOctree;
begin
  if (Scene <> nil) and
     (Scene.InternalOctreeRendering <> nil) then
    Result := Scene.InternalOctreeRendering else
    Result := nil;
end;

{ This calls MainViewport.PrepareResources
  (that causes Scene.PrepareResources).
  Additionally, if AllowProgess and some other conditions are met,
  this shows progress of operation.

  Remember that you can call this only when gl context is already active
  (Scene.PrepareResources requires this) }
procedure PrepareResources(AllowProgress: boolean);
begin
  if AllowProgress then
    MainViewport.PrepareResources('Preparing animation')
  else
    MainViewport.PrepareResources;
end;

procedure SceneOctreeCreate; forward;

procedure SetCollisions(const Value: boolean;
  const NeedMenuUpdate: boolean = true);
begin
  if Scene.Collides <> Value then
  begin
    Scene.Collides := Value;
    ButtonCollisions.Pressed := Value;
    if NeedMenuUpdate then
      MenuCollisions.Checked := Value;
    if Scene.Collides and (Scene.InternalOctreeCollisions = nil) then
      SceneOctreeCreate;
  end;
end;

procedure ToggleNamedAnimationsUi;
begin
  NamedAnimationsUiExists := not NamedAnimationsUiExists;
  MenuNamedAnimations.Checked := NamedAnimationsUiExists;
  ButtonAnimations.Pressed := NamedAnimationsUiExists;
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
  ValueColor = 'ffffff';

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
        Description := Trim(TAbstractPointingDeviceSensorNode(Sensor).Description);
        if Description <> '' then
          Result := Description;
      end else
      if Sensor is TAnchorNode then
      begin
        Anchor := TAnchorNode(Sensor);
        { use description instead, if any provided }
        Description := Trim(Anchor.Description);
        if Description <> '' then
          Result := Description;
        if Anchor.FdUrl.Count <> 0 then
        begin
          Result := Result + (' [' + URIDisplay(Anchor.FdUrl.Items[0]));
          for J := 1 to Anchor.FdUrl.Count - 1 do
            Result := Result + (', ' + URIDisplay(Anchor.FdUrl.Items[J]));
          Result := Result + ']';
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

  function CurrentAboveHeight(WalkNavigation: TCastleWalkNavigation): string;
  begin
    if SceneOctreeCollisions = nil then
      Result := 'no collisions'
    else
    if not WalkNavigation.Gravity then
      Result := 'no gravity'
    else
    if not WalkNavigation.IsAbove then
      Result := 'no ground beneath'
    else
      Result := Format('%f', [WalkNavigation.AboveHeight]);
  end;

var
  s: string;
  Statistics: TRenderStatistics;
  Pos, Dir, Up: TVector3;
  WalkNavigation: TCastleWalkNavigation;
begin
  inherited;

  Statistics := MainViewport.Statistics;

  DescribeSensors;

  { S := Format('Collision detection: %s', [ BoolToStrOO[Scene.Collides] ]);
  if SceneOctreeCollisions = nil then
    S := S + ' (octree resources released)';
  Text.Append(S); }

  MainViewport.Camera.GetView(Pos, Dir, Up);
  Text.Append(Format('Camera: pos <font color="#%s">%s</font>, dir <font color="#%s">%s</font>, up <font color="#%s">%s</font>',
    [ ValueColor, Pos.ToString,
      ValueColor, Dir.ToString,
      ValueColor, Up.ToString ]));

  WalkNavigation := CurrentWalkNavigation;
  if WalkNavigation <> nil then
  begin
    Text.Append(Format('Move speed (per sec) : <font color="#%s">%f</font>, Avatar height: <font color="#%s">%f</font> (last height above the ground: <font color="#%s">%s</font>)',
      [ ValueColor, WalkNavigation.MoveSpeed,
        ValueColor, WalkNavigation.PreferredHeight,
        ValueColor, CurrentAboveHeight(WalkNavigation) ]));
  end;

  { if SceneLightsCount = 0 then
   s := '(useless, scene has no lights)' else
   s := BoolToStrOO[Scene.Attributes.UseSceneLights];
  Text.Append(Format('Use scene lights: %s', [s])); }

  Text.Append(Format('Rendered: <font color="#%s">%s</font>', [
    ValueColor,
    Statistics.ToString
  ]) + OctreeDisplayStatus);

  if Scene.TimeAtLoad = 0.0 then
    S := Format('World time: <font color="#%s">%d</font>',
      [ValueColor, Trunc(Scene.Time)]) else
    S := Format('World time: <font color="#%s">load time + %d</font>',
      [ValueColor, Trunc(Scene.Time - Scene.TimeAtLoad)]);
  if not AnimationTimePlaying then
    S := S + ' (paused)';
  if not ProcessEventsWanted then
    S := S + (' (paused, not processing VRML/X3D events)');
  Text.Append(S);

(*// nice to debug ShadowVolumeRenderer optimizations.
  // Too cryptic to show normal users:)
  S := Format('No shadow %d + zpass %d + zfail (no l cap) %d + zfail (l cap) %d = all %d',
    [ MainViewport.ShadowVolumeRenderer.CountShadowsNotVisible,
      MainViewport.ShadowVolumeRenderer.CountZPass,
      MainViewport.ShadowVolumeRenderer.CountZFailNoLightCap,
      MainViewport.ShadowVolumeRenderer.CountZFailAndLightCap,
      MainViewport.ShadowVolumeRenderer.CountCasters ]);
  Text.Append(S);
*)

{
  Text.Append(Format('Projection: near %f far %f', [
    MainViewport.Camera.EffectiveProjectionNear,
    MainViewport.Camera.EffectiveProjectionFar
  ]));
}
end;

{ TCastleWindowBase callbacks --------------------------------------------------------- }

{ Update SceneBoundingBox look. }
procedure SceneBoundingBoxUpdate(const RenderingCamera: TRenderingCamera);
begin
  SceneBoundingBox.Exists :=
    (RenderingCamera.Target = rtScreen) and
    (not HideExtraScenesForScreenshot) and
    ShowBBox;
  if SceneBoundingBox.Exists then
  begin
    { Use Scene.Attributes.LineWidth for our visualizations as well }
    SceneBoundingBox.Attributes.LineWidth := Scene.Attributes.LineWidth;
    SceneBoundingBox.UpdateBox(Scene.BoundingBox);
  end;
end;

{ Render visualization of various stuff, like octree and such. }
procedure RenderVisualizations(const RenderingCamera: TRenderingCamera);

  { TODO:
    Rendering below for now uses OpenGL fixed-function pipeline,
    and it requires fixed-function matrix set up. }
  {$ifndef OpenGLES}
  procedure PushMatrix;
  var
    CameraMatrix: PMatrix4;
  begin
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadMatrix(RenderContext.ProjectionMatrix);

    if RenderingCamera.RotationOnly then
      CameraMatrix := @RenderingCamera.RotationMatrix
    else
      CameraMatrix := @RenderingCamera.Matrix;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrix(CameraMatrix^);
  end;

  procedure PopMatrix;
  begin
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  end;

  procedure RenderFrustum(AlwaysVisible: boolean);
  var
    FrustumPoints: TFrustumPoints;
  begin
    if AlwaysVisible then
    begin
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
    end;
    try
      {$warnings off} // using deprecated, but for now this is easiest for view3dscene
      MainViewport.InternalWalkNavigation.Camera.Frustum.CalculatePoints(FrustumPoints);
      {$warnings on}
      glColor3f(1, 1, 1);
      glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(4, GL_FLOAT, 0, @FrustumPoints);
        glDrawElements(GL_LINES, 12 * 2, GL_UNSIGNED_INT,
          @FrustumPointsLinesIndexes);
      glDisableClientState(GL_VERTEX_ARRAY);
    finally
      if AlwaysVisible then glPopAttrib;
    end;
  end;
  {$endif not OpenGLES}

begin
  if (RenderingCamera.Target = rtScreen) and (not HideExtraScenesForScreenshot) then
  begin
    {$ifndef OpenGLES}
    PushMatrix;

    { Visualization below depends on DEPTH_TEST enabled
      (and after rendering scene, it is disabled by TGLRenderer.RenderCleanState) }
    glEnable(GL_DEPTH_TEST);

    { Use Scene.Attributes.LineWidth for our visualizations as well }
    RenderContext.LineWidth := Scene.Attributes.LineWidth;

    OctreeDisplay(Scene);

    { Note that there is no sense in showing viewing frustum if
      Camera is TCastleWalkNavigation, since InternalWalkNavigation.Frustum should not
      be visible then (as it's just the *currently used* frustum in this case). }
    if ShowFrustum and not (Navigation is TCastleWalkNavigation) then
      RenderFrustum(ShowFrustumAlwaysVisible);

    if SelectedItem <> nil then
    begin
      SelectedShape := TShape(SelectedItem^.Shape);
      glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
        glDisable(GL_DEPTH_TEST); { saved by GL_ENABLE_BIT }
        glColorv(Vector4(0.5, 0.3, 0.3, 1));
        glDrawCornerMarkers(SelectedShape.BoundingBox);   // draw red selection corner markers, visible through geometry
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); { saved by GL_COLOR_BUFFER_BIT }
        glEnable(GL_BLEND);
        glEnable(GL_CULL_FACE);  { saved by GL_ENABLE_BIT }
        glColorv(Vector4(0.5, 0.3, 0.3, 0.5));
        glBegin(GL_TRIANGLES);   // draw face back in red, visible through geometry
          glVertexv(SelectedItem^.World.Triangle.Data[0]);
          glVertexv(SelectedItem^.World.Triangle.Data[2]);
          glVertexv(SelectedItem^.World.Triangle.Data[1]);
        glEnd;
        glColorv(Vector4(0.4, 0.4, 1, 0.4));
        glBegin(GL_TRIANGLES);  // draw face front in blue,  visible through geometry
          glVertexv(SelectedItem^.World.Triangle.Data[0]);
          glVertexv(SelectedItem^.World.Triangle.Data[1]);
          glVertexv(SelectedItem^.World.Triangle.Data[2]);
        glEnd;

        glEnable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        glColorv(Vector4(1, 1, 1, 1));

        RenderContext.LineWidth := 2.0;
        glBegin(GL_LINE_LOOP);   // draw face outline in white from front only, not visible through geometry.
          glVertexv(SelectedItem^.World.Triangle.Data[0]);
          glVertexv(SelectedItem^.World.Triangle.Data[1]);
          glVertexv(SelectedItem^.World.Triangle.Data[2]);
        glEnd;

        RenderContext.PointSize := 5.0;
        glDisable(GL_DEPTH_TEST);
        glBegin(GL_POINTS);     // draw hit point, visible through
          glVertexv(SelectedPointWorld);
        glEnd;
      glPopAttrib;
      // draw blue corner markers, these overwrite the red markers, but only when infront
      // so corners infront are blue, those behind are red.
      // gives depth impression and is generally visible against various geometry .
      glColorv(Vector4(0.2, 0.2, 1, 1));
      glDrawCornerMarkers(SelectedShape.BoundingBox);
    end;

    PopMatrix;
    {$endif not OpenGLES}
  end else
  begin
    SceneBoundingBox.Exists := false;
  end;
end;

procedure TV3DViewport.Render3D(const Params: TRenderParams);
begin
  SceneBoundingBoxUpdate(Params.RenderingCamera);
  inherited;
  { RenderVisualizations are opaque, so they should be rendered here
    to correctly mix with partially transparent 3D scenes.
    Render as ShadowVolumesReceivers=true to make selected triangle
    drawn last (on top), to be clearly and always visible. }
  if (not Params.Transparent) and
     (true in Params.ShadowVolumesReceivers) then
    RenderVisualizations(Params.RenderingCamera);
end;

procedure TV3DViewport.RenderFromView3D(const Params: TRenderParams);
begin
  { Although TCastleViewport is ready for Items.MainScene = nil case,
    this RenderFromView3D below is not. Doesn't seem needed,
    but better to secure for this case, since any TCastleViewport
    should always work with MainScene = nil. }
  if Items.MainScene = nil then Exit;

  { do not render Items.MainScene if SceneDebugEdges is to be visible }
  Items.MainScene.Visible := FillMode <> fmSilhouetteBorderEdges;
  SceneDebugEdges.Exists := FillMode = fmSilhouetteBorderEdges;
  if SceneDebugEdges.Exists then
    SceneDebugEdges.UpdateEdges(Scene);

  inherited;
  { inherited will call Render3D that will call RenderVisualizations }
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
        SelectedPointLocal :=
          SelectedItem^.State.Transformation.InverseTransform.MultPoint(SelectedPointWorld);
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

class procedure THelper.ViewpointsChanged(Sender: TObject);
begin
  Viewpoints.Recalculate(Scene);
end;

class procedure THelper.BoundViewpointChanged(Sender: TObject);
var
  V: TAbstractViewpointNode;
begin
  V := Scene.ViewpointStack.Top;
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

{ Call when ButtonWarningsEnabled or SceneWarnings.Count changes
  or when window sizes change. }
procedure UpdateButtonWarnings;
begin
  ButtonWarnings.Caption := Format('%d warnings', [SceneWarnings.Count]);
  ButtonWarnings.Exists := ButtonWarningsEnabled and (SceneWarnings.Count <> 0);
  { When window is closed, width/height may be incorrect (even negative,
    because of WindowDefaultSize). Do not call EventResize then.
    May happen when you used --write, and some warning occurs. }
  if not Window.Closed then
    Window.Container.EventResize; { update ButtonWarnings.Left }
end;

class procedure THelper.OnWarningHandle(const Category, S: string);
begin
  { It is possible that SceneWarnings = nil now,
    in case on macOS we use
    ".../view3dscene .../dynamic_world.x3dv --screenshot 0 output_2d_screenshot.png"
    and get warning
    "Freeing form failed with EAccessViolation, this is unfortunately possible on macOS with Carbon widgetset".
    The ButtonWarnings is invalid (already freed) at this point too. }
  if SceneWarnings <> nil then
  begin
    if Category <> '' then
      SceneWarnings.Add(Category + ': ' + S)
    else
      SceneWarnings.Add(S);
    UpdateButtonWarnings;
  end;

  if Window <> nil then
    Window.Invalidate;
end;

procedure SceneOctreeCreate;
var
  OldRender, OldBeforeRender: TContainerEvent;
begin
  { Do not create octrees when Scene.Collides = false. This makes
    setting Scene.Collides to false an optimization: octree doesn't have to
    be recomputed when animation frame changes, or new scene is loaded etc. }

  if Scene.Collides then
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

{ Frees (and sets to some null values) "scene global variables". }
procedure FreeScene;
begin
  SceneOctreeFree;

  // Scene.Close;

  Viewpoints.Recalculate(nil);
  RefreshNamedAnimationsUi(Window, nil, ToolbarPanel.EffectiveHeight);

  SceneURL := '';

  if MenuReopen <> nil then
    MenuReopen.Enabled := false;

  Unselect;
end;

procedure LoadClearScene; forward;

type
  TLoadSceneOption = (lsoCommandLineCustomization);
  TLoadSceneOptions = set of TLoadSceneOption;

{ Calls FreeScene and then inits "scene global variables".

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

  If lsoCommandLineCustomization in Options,
  then we will use (and clear) the command-line customizations
  of viewpoint. This should be @false if you're only loading
  temporary scene, like LoadClearScene. }
procedure LoadSceneCore(
  RootNode: TX3DRootNode;
  ASceneURL: string;
  const SceneChanges: TSceneChanges;
  const Options: TLoadSceneOptions);
var
  NewCaption: string;
  I: Integer;
begin
  FreeScene;

  try
    SceneURL := ASceneURL;

    { set InitialViewpoint* before Scene.Load }
    SetInitialViewpoint(Scene, lsoCommandLineCustomization in Options);

    Scene.Load(RootNode, true);

    ChangeScene(SceneChanges, Scene);

    { calculate Viewpoints, including MenuJumpToViewpoint. }
    Viewpoints.Recalculate(Scene);

    MainViewport.AssignDefaultCamera;
    MainViewport.AssignDefaultNavigation;
    // UpdateCameraUI, called below, will make UI reflect current navigation

    for I := 0 to High(ExtraViewports) do
      AssignCameraAndNavigation(ExtraViewports[I], MainViewport);
    Viewpoints.BoundViewpoint := Viewpoints.ItemOf(ViewpointNode);

    SceneInitLights(Scene, NavigationNode);

    { update MenuHeadlight.Checked now, and make it always updated. }
    THelper.HeadlightOnChanged(Scene);
    Scene.OnHeadlightOnChanged :=
      {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).HeadlightOnChanged;

    NewCaption := Scene.Caption;
    if NewCaption = '' then
      NewCaption := URICaption(SceneURL);
    NewCaption := SForCaption(NewCaption) + ' - view3dscene';
    Window.Caption := NewCaption;

    UpdateCameraUI;

    SceneOctreeCreate;

    Scene.OnGeometryChanged :=
      {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).GeometryChanged;
    Scene.OnViewpointsChanged :=
      {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ViewpointsChanged;
    Scene.OnPointingDeviceSensorsChange :=
      {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).PointingDeviceSensorsChange;
    Scene.ProcessEvents := ProcessEventsWanted;

    RefreshNamedAnimationsUi(Window, Scene, ToolbarPanel.EffectiveHeight);

    if not Window.Closed then
      Window.Invalidate;

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
    MessageOK(Window, S)
  else
    ErrorWrite(S);
end;

{ This loads the scene from file (using LoadNode) and
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
  const SceneChanges: TSceneChanges);

{ It's useful to undefine it only for debug purposes:
  FPC dumps then backtrace of where exception happened,
  which is often enough to trace the error.
  In release versions this should be defined to produce a nice
  message box in case of errors (instead of just a crash). }
{$define CATCH_EXCEPTIONS}

var
  RootNode: TX3DRootNode;
  SavedSceneWarnings: TSceneWarnings;
  StartTime, TimeLoadX3D, TimeLoadScene, TimePrepareResources: TProcessTimerResult;
begin
  StartTime := ProcessTimer;

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
    // since we just modified SceneWarnings.Count, refresh ButtonWarnings visibility
    UpdateButtonWarnings;

    {$ifdef CATCH_EXCEPTIONS}
    try
    {$endif CATCH_EXCEPTIONS}
      RootNode := LoadNode(ASceneURL);
    {$ifdef CATCH_EXCEPTIONS}
    except
      on E: Exception do
      begin
        LoadErrorMessage('Error while loading scene from "' +ASceneURL+ '": ' +
          E.Message);
        { In this case we can preserve current scene. }
        SceneWarnings.Assign(SavedSceneWarnings);
        // since we just modified SceneWarnings.Count, refresh ButtonWarnings visibility
        UpdateButtonWarnings;
        Exit;
      end;
    end;
    {$endif CATCH_EXCEPTIONS}
  finally FreeAndNil(SavedSceneWarnings) end;

  TimeLoadX3D := ProcessTimer;

  {$ifdef CATCH_EXCEPTIONS}
  try
  {$endif CATCH_EXCEPTIONS}
    LoadSceneCore(RootNode, ASceneURL, SceneChanges, [lsoCommandLineCustomization]);
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

  TimeLoadScene := ProcessTimer;

  { For batch operation (making screenshots), do not save the scene
    on "recent files" menu. This also applies when using view3dscene
    as a thumbnailer. }
  if not MakingScreenShot then
    RecentMenu.Add(ASceneURL);

  { We call PrepareResources to make Scene.PrepareResources to gather
    warnings (because some warnings, e.g. invalid texture URL,
    are reported only from Scene.PrepareResources).
    Also, this allows us to show first PrepareResources with progress bar. }
  PrepareResources(true);
  ButtonWarningsEnabled := true;
  UpdateButtonWarnings;

  TimePrepareResources := ProcessTimer;

  WritelnLogMultiline('Loading', Format(
    'Loaded "%s" with dependencies in %f seconds:' + NL +
    '  %f to load from disk (create X3D graph)' + NL +
    '  %f to initialize scene (initialize shapes tree, collisions...)' + NL +
    '  %f to prepare resources (load textures, prepare OpenGL resources...)',
    [ URIDisplay(ASceneURL),
      ProcessTimerSeconds(TimePrepareResources, StartTime),
      ProcessTimerSeconds(TimeLoadX3D, StartTime),
      ProcessTimerSeconds(TimeLoadScene, TimeLoadX3D),
      ProcessTimerSeconds(TimePrepareResources, TimeLoadScene)
    ]));
end;

{ Load special "clear" and "welcome" scenes.
  This loads a scene directly from TX3DNode, and assumes that
  LoadSceneCore will not fail. }
procedure LoadSimpleScene(Node: TX3DRootNode;
  const Options: TLoadSceneOptions);
begin
  SceneWarnings.Clear;
  // since we just modified SceneWarnings.Count, refresh ButtonWarnings visibility
  UpdateButtonWarnings;

  LoadSceneCore(Node, '', [], Options);
end;

{ This works like LoadScene, but loaded scene is an empty scene.
  More specifically, this calls FreeScene, and then inits
  "scene global variables" to some non-null values. }
procedure LoadClearScene;
begin
  { As a clear scene, I'm simply loading an empty 3D model.
    This way everything seems normal: Scene is Loaded,
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
  LoadSimpleScene(LoadX3DClassicFromString({$I ../embedded_data/scenes/clear_scene.inc}, ''), []);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
begin
  LoadSimpleScene(LoadX3DClassicFromString({$I ../embedded_data/scenes/welcome_scene.inc}, ''), []);
end;

const
  SaveGenerator = 'view3dscene, https://castle-engine.io/view3dscene.php';

{ Load model from ASceneURL ('-' means stdin),
  do SceneChanges, and write it as VRML/X3D to stdout.
  This is used to handle --write command-line option. }
procedure WriteModel(const ASceneURL: string;
  const SceneChanges: TSceneChanges; const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean);
var
  Node: TX3DRootNode;
begin
  Node := LoadNode(ASceneURL);
  try
    ChangeNode(SceneChanges, Node);
    Save3D(Node, StdOutStream, SaveGenerator,
      ExtractURIName(ASceneURL), Encoding, ForceConvertingToX3D);
  finally FreeAndNil(Node) end;
end;

class procedure THelper.OpenRecent(const URL: string);
begin
  LoadScene(URL, []);
end;

procedure DropFiles(Container: TUIContainer; const FileNames: array of string);
var
  URL: string;
begin
  if High(FileNames) >= 0 then
  begin
    URL := FilenameToURISafe(FileNames[0]);
    if URL <> '' then
      LoadScene(URL, []);
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
      SelectedPointLocal to world coords by new transform.
      This is the main reason why we keep SelectedPointLocal recorded. }
    try
      SelectedPointWorld := SelectedItem^.State.Transformation.Transform.MultPoint(
        SelectedPointLocal);
    except
      on ETransformedResultInvalid do
        Unselect;
    end;

  end;
end;

class procedure THelper.ClickButtonWarnings(Sender: TObject);
begin
  MessageReport(
    Format('%d warnings:', [SceneWarnings.Count]) + NL +
    NL +
    TrimRight(SceneWarnings.Items.Text) + NL +
    NL +
    'Scene URL: "' + URIDisplay(SceneURL) + '".' + NL +
    'Use "File->View Warnings" menu to view these warnings again.'
  );
  ButtonWarningsEnabled := false;
  UpdateButtonWarnings;
end;

procedure AttributesLoadFromConfig(Attributes: TRenderingAttributes);
begin
  Attributes.LineWidth := UserConfig.GetFloat('video_options/line_width',
    Attributes.DefaultLineWidth);
  Attributes.PointSize := UserConfig.GetFloat('video_options/point_size',
    Attributes.DefaultPointSize);
end;

procedure AttributesSaveToConfig(Attributes: TRenderingAttributes);
begin
  UserConfig.SetDeleteFloat('video_options/line_width',
    Attributes.LineWidth, Attributes.DefaultLineWidth);
  UserConfig.SetDeleteFloat('video_options/point_size',
    Attributes.PointSize, Attributes.DefaultPointSize);
end;

{ make screen shots ---------------------------------------------------------- }

procedure SaveScreenRender;
begin
  if ControlsOnScreenshot then
  begin
    Window.Container.EventBeforeRender;
    Window.Container.EventRender;
  end else
  begin
    { Many controls are hidden simply because ViewportsRender doesn't render them.
      But for extra 3D scenes within scene manager, we need to make sure
      nearest SceneBoundingBoxUpdate will hide them. }
    HideExtraScenesForScreenshot := true;
    ViewportsRender(Window.Container);
    HideExtraScenesForScreenshot := false;
  end;
end;

{ Make all the screenshots, using FBO.
  This allows the window to be hidden when taking screenshots. }
procedure MakeAllScreenShotsFBO(const Transparency: boolean);
var
  ScreenshotRender: TGLRenderToTexture;

  { This performs all screenshot takes, as specified in ScreenShotsList.
    It is used both for batch mode screenshots (--screenshot, --screenshot-range)
    and interactive (menu items about screenshots) operation. }
  procedure MakeAllScreenShots(const ImageClass: TCastleImageClass);
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
    OldTime := Scene.Time;
    try
      MainViewport.BeforeRender;

      { TRangeScreenShot cannot display progress on TCastleWindow,
        since this would mess rendered image.
        Besides, in the future GL window may be hidden during rendering. }
      Progress.UserInterface := ProgressNullInterface;

      ScreenShotsList.BeginCapture;

      for I := 0 to ScreenShotsList.Count - 1 do
      begin
        ScreenShotsList[I].BeginCapture;
        try
          for J := 0 to ScreenShotsList[I].Count - 1 do
          begin
            Scene.ResetTime(ScreenShotsList[I].UseTime(J));
            SaveScreenRender;
            Image := ScreenshotRender.SaveScreen(ImageClass, Window.Rect);
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
      Scene.ResetTime(OldTime);
    end;
  end;

var
  ImageClass: TCastleImageClass;
begin
  ScreenshotRender := TGLRenderToTexture.Create(Window.Width, Window.Height);
  try
    ScreenshotRender.Buffer := tbNone;
    if Transparency then
    begin
      ScreenshotRender.ColorBufferAlpha := true;
      ImageClass := TRGBAlphaImage;
    end else
      ImageClass := TRGBImage;
    ScreenshotRender.Stencil := Window.StencilBits > 0;
    ScreenshotRender.MultiSampling := Window.MultiSampling;
    ScreenshotRender.GLContextOpen;
    ScreenshotRender.RenderBegin;

    if Transparency then
    begin
      BackgroundTransparent;
      if glGetInteger(GL_ALPHA_BITS) = 0 then
        { In case FBO is not available, and main context doesn't have alpha
          bits either. }
        WritelnWarning('OpenGL', 'We did not manage to create a render buffer with alpha channel. This means that screenshot will not capture the transparency. You need a better GPU for this to work.');
    end;

    try
      MakeAllScreenShots(ImageClass);
    finally
      ScreenshotRender.RenderEnd;
      if Transparency then
        BackgroundOpaque;
    end;
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
    Result := IsPrefix(Prefix, Node.X3DName) or
              IsPrefix('OB_' + Prefix, Node.X3DName);
  end;

begin
  if IsPlaceholder('CasMoveLimit') or
     IsPlaceholder('CasWater') or
     IsPlaceholder('CasRes') or
     IsPlaceholder('CasWaypoint') or
     IsPlaceholder('CasSector') or
     { Below are special only on specific castle1 levels, see GameLevelSpecific.
       For historical reasons, they don't use 'Cas' prefix. }
     (Node.X3DName = 'LevelExitBox') or
     IsPrefix('WerewolfAppear_', Node.X3DName) or
     (Node.X3DName = 'GateExitBox') or
     (Node.X3DName = 'Teleport1Box') or
     (Node.X3DName = 'Teleport2Box') or
     (Node.X3DName = 'SacrilegeBox') or
     IsPrefix('SacrilegeGhost_', Node.X3DName) or
     IsPrefix('SwordGhost_', Node.X3DName) or
     (Node.X3DName = 'Elevator49DownBox') or
     (Node.X3DName = 'Elev9a9bPickBox') then
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
    Fbo: TGLRenderToTexture;
    ImageClass: TCastleImageClass;
  begin
    Assert(Window.DoubleBuffer); { view3dscene always has double buffer }

    if Transparency then
    begin
      Fbo := TGLRenderToTexture.Create(Window.Width, Window.Height);
      Fbo.Buffer := tbNone;
      Fbo.ColorBufferAlpha := true;
      Fbo.Stencil := Window.StencilBits > 0;
      Fbo.MultiSampling := Window.MultiSampling;
      Fbo.GLContextOpen;
      Fbo.RenderBegin;
      ImageClass := TRGBAlphaImage;
      BackgroundTransparent;

      if glGetInteger(GL_ALPHA_BITS) = 0 then
        { In case FBO is not available, and main context doesn't have alpha
          bits either. }
        WritelnWarning('OpenGL', 'We did not manage to create a render buffer with alpha channel. This means that screenshot will not capture the transparency. You need a better GPU for this to work.');
    end else
    begin
      Fbo := nil;
      ImageClass := TRGBImage;
    end;

    try
      Window.Container.EventBeforeRender;
      SaveScreenRender;

      if Fbo <> nil then
      begin
        { We need to use TGLRenderToTexture.SaveScreen
          to work in case of multi-sampling. }
        Result := Fbo.SaveScreen(ImageClass, Window.Rect);
      end else
        Result := SaveScreen_NoFlush(ImageClass, Window.Rect, Window.SaveScreenBuffer);
    finally
      if Transparency then
      begin
        Fbo.RenderEnd;
        FreeAndNil(Fbo);
        BackgroundOpaque;
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
    { Below is a little expanded version of TCastleWindowBase.SaveScreenDialog.
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
var
  WalkNavigation: TCastleWalkNavigation;

  procedure MakeGravityUp(const NewUp: TVector3);
  var
    Pos, Dir, Up, GravityUp: TVector3;
  begin
    MainViewport.Camera.GetView(Pos, Dir, Up, GravityUp);
    if VectorsParallel(Dir, NewUp) then
      Dir := AnyOrthogonalVector(NewUp);
    Up := NewUp;
    GravityUp := NewUp;
    MainViewport.Camera.SetView(Pos, Dir, Up, GravityUp, false);
  end;

  procedure ChangeMoveSpeed;
  var
    MoveSpeed: Single;
  begin
    MoveSpeed := Navigation.MoveSpeed;
    if MessageInputQuery(Window, 'New move speed (units per second):', MoveSpeed) then
    begin
      Navigation.MoveSpeed := MoveSpeed;
      Window.Invalidate;
    end;
  end;

  procedure ChangePointSize;
  var
    Value: Single;
  begin
    Value := Scene.Attributes.PointSize;
    if MessageInputQuery(Window, 'Change point size:', Value) then
      Scene.Attributes.PointSize := Max(Value, 0.01);
  end;

  procedure ChangeLineWidth;
  var
    Value: Single;
  begin
    Value := Scene.Attributes.LineWidth;
    if MessageInputQuery(Window, 'Change line width:', Value) then
      Scene.Attributes.LineWidth := Max(Value, 0.01);
  end;

  procedure ChangeTimeSpeed;
  var
    S: Single;
  begin
    S := Scene.TimePlayingSpeed;
    if MessageInputQuery(Window,
      'Adjust the playing speed to slow down, or make faster, the animation. For example, "0.5" makes playing animation two times slower,"2.0" makes it two times faster.' + NL +
      NL +
      'Note: Using this to slow down a "baked animation" (from castle-anim-frames or MD3 files) may look bad, as the same number of static frames will be just played slower. Increase "Baked Animation Smoothness" and reload the animation to counteract this.' + NL +
      NL +
      'New playing speed:', S) then
      Scene.TimePlayingSpeed := S;
  end;

  procedure ChangeBakedAnimationSmoothness;
  var
    S: Single;
  begin
    S := BakedAnimationSmoothness;
    if MessageInputQuery(Window,
      'Adjust the number of static frames generated when loading a "baked animation" (from castle-anim-frames or MD3 files). ' + NL +
      NL +
      'Increase this to improve the quality of baked animation (even when it''s slowed down by changing "playing speed"). But it costs memory, and the loading time will increase too. Lower this value to have faster loading, but worse quality.' + NL +
      NL +
      'This is only applied when loading animation from file. You can use "File -> Reopen" command to apply this to the currently loaded animation.' + NL +
      NL +
      'This has no effect on non-baked animations, like played from X3D or Spine JSON.' + NL+
      NL +
      'New baked animation smoothness:', S) then
      BakedAnimationSmoothness := S;
  end;

  procedure SelectedShowInformation;
  var
    S, TextureDescription: string;
    VCOver, TCOver, VCNotOver, TCNotOver: Cardinal;
    M: TMaterialInfo;
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
           'Selected point %s from triangle'+ NL +
           '%s.' +nl+
           nl+
           'This triangle is part of the geometry node named "%s" (original geometry type %s, rendered through type %s). Parent node name: "%s", grand-parent node name: "%s", grand-grand-parent node name: "%s".' +nl+
           nl+
           'Node''s bounding box is %s. ',
           [SelectedPointWorld.ToString,
            SelectedItem^.World.Triangle.ToString,
            SelectedGeometry.X3DName,
            SelectedShape.OriginalGeometry.X3DType,
            SelectedGeometry.X3DType,
            SelectedShape.GeometryParentNodeName,
            SelectedShape.GeometryGrandParentNodeName,
            SelectedShape.GeometryGrandGrandParentNodeName,
            SelectedShape.BoundingBox.ToString]);

      if (SelectedItem^.Face.IndexBegin <> -1) and
         (SelectedItem^.Face.IndexEnd <> -1) then
      begin
        S := S + Format('Face containing the selected triangle spans from %d to' +
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
       s := s + Format(
              'Node has %d vertices and %d triangles '+
              '(with and without over-triangulating).',
              [VCNotOver, TCNotOver]);
      end else
      begin
       s := s + Format(
              'When we don''t use over-triangulating (e.g. for raytracing and '+
              'collision-detection) node has %d vertices and %d triangles. '+
              'When we use over-triangulating (e.g. for real-time rendering) '+
              'node has %d vertices and %d triangles.',
              [VCNotOver, TCNotOver, VCOver, TCOver]);
      end;

      { calculate Tex }
      Tex := SelectedItem^.State.MainTexture;

      { calculate TextureDescription }
      if Tex = nil then
        TextureDescription := 'none' else
        TextureDescription := Tex.TextureDescription;

      S := S + Format(nl +nl+ 'Node''s texture : %s.', [TextureDescription]);

      S := S + nl+ nl;
      M := SelectedItem^.State.MaterialInfo;
      if M <> nil then
      begin
        S := S + Format(
          'Material parameters (lighting equation): %s' + NL +
          '  Main color: %s' + NL +
          '  Transparency : %f', [
          M.ClassName,
          M.MainColor.ToString,
          M.Transparency
        ]);
      end else
      begin
        S := S + 'Material not assigned (unlit white material).';
      end;
    end;
    MessageReport(S);
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
         S := S + NL + NL + Format('Light %d: %s: ',
           [ I, Lights.List^[i].Node.NiceName ]);

         ShadowingItem := SceneOctreeCollisions.SegmentCollision(
           SelectedPointWorld, Lights.List^[i].Location,
             false, SelectedItem, true, nil);

         if ShadowingItem <> nil then
         begin
          s := s + Format('no, this light is blocked by triangle %s from shape %s.',
            [ ShadowingItem^.World.Triangle.ToString,
              TShape(ShadowingItem^.Shape).NiceName ])
         end else
          s := s + 'yes, no object blocks this light, it shines on selected point.';
        end;
    end;

    MessageReport(S);
  end;

  procedure RemoveSelectedShape;
  begin
    if SelectedItem = nil then
    begin
      Window.MessageOK('Nothing selected.', mtError);
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

    if SelectedItem = nil then
    begin
      Window.MessageOK('Nothing selected.', mtError);
      Exit;
    end;

    if (SelectedItem^.Face.IndexBegin = -1) or
       (SelectedItem^.Face.IndexEnd = -1) then
    begin
      Window.MessageOK('The selected triangle is not part of IndexedFaceSet or IndexedTriangleSet node.', mtError);
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
      MessageOK(Window, Format('Cannot remove faces from "%s" node.', [Geometry.X3DType]));
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
      CoordsField.Changed;

      { Texture coordinates, if not empty, have always (both in VRML 1.0
        and VRML 2.0 / X3D IndexedFaceSet nodes, and in IndexedTriangleMesh
        from Inventor) the same ordering as coordIndex.
        So we can remove equivalent texture coords in the same manner
        as we removed coords. }
      if TexCoords <> nil then
      begin
        TexCoords.DeleteRange(IndexBegin, IndexCount);
        TexCoordsField.Changed;
      end;
    finally Dec(DisableAutoDynamicGeometry) end;
  end;

  { Returns @true and sets MatInfo on success.
    Produces message to user and returns @false on failure.

    Note that SelectedItem is not necessarily correct anymore.
    Use only MatInfo pointers after this.

    TODO: This only works with Phong materials. }
  function ChangeMaterialInit(out MatInfo: TPhongMaterialInfo): boolean;
  var
    Shape: TAbstractShapeNode;
    AnyMatInfo: TMaterialInfo;
  begin
    if (SelectedItem = nil) then
    begin
      Window.MessageOK('Nothing selected.', mtError);
      Exit(false);
    end;

    AnyMatInfo := SelectedItem^.State.MaterialInfo;
    if AnyMatInfo is TPhongMaterialInfo then
      MatInfo := TPhongMaterialInfo(AnyMatInfo)
    else
      MatInfo := nil;

    Shape := SelectedItem^.State.ShapeNode;

    if MatInfo = nil then
    begin
      if Shape <> nil then
      begin
        if MessageYesNo(Window, 'No Material (or similar node indicating Phong material) present. Add material to this node and then edit it?') then
        begin
          { Note that this may remove old Shape.FdAppearance.Value,
            but only if Shape.Appearance = nil, indicating that
            something wrong was specified for "appearance" field.

            Similar, it may remove old Shape.Appearance.FdMaterial.Value,
            but only if Shape.MaterialInfo was nil. }
          Shape.Material := TMaterialNode.Create('', Shape.BaseUrl);
          Scene.ChangedAll;
        end else
          Exit(false);
      end else
      begin
        MessageOK(Window, 'Cannot add Material to VRML 1.0.');
        Exit(false);
      end;
    end;

    Result := true;
  end;

  procedure ChangeMaterialDiffuse;
  var
    MatInfo: TPhongMaterialInfo;
    Color: TVector3;
  begin
    if not ChangeMaterialInit(MatInfo) then Exit;

    Color := MatInfo.DiffuseColor;
    if Window.ColorDialog(Color) then
      MatInfo.DiffuseColor := Color;
  end;

  procedure ChangeMaterialSpecular;
  var
    MatInfo: TPhongMaterialInfo;
    Color: TVector3;
  begin
    if not ChangeMaterialInit(MatInfo) then Exit;

    Color := MatInfo.SpecularColor;
    if Window.ColorDialog(Color) then
      MatInfo.SpecularColor := Color;
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
    C: TVector3;
  begin
    C := RenderContext.GlobalAmbient;
    if Window.ColorDialog(C) then
      RenderContext.GlobalAmbient := C;
  end;

  procedure SetViewpointForWholeScene(
    const WantedDirection, WantedUp: Integer;
    const WantedDirectionPositive, WantedUpPositive: boolean);
  var
    Position, Direction, Up, GravityUp: TVector3;
  begin
    CameraViewpointForWholeScene(Scene.BoundingBox, WantedDirection, WantedUp,
      WantedDirectionPositive, WantedUpPositive,
      Position, Direction, Up, GravityUp);
    Scene.CameraTransition(MainViewport.Camera, Position, Direction, Up, GravityUp);
  end;

  procedure RemoveNodesWithMatchingName;
  var
    Wildcard: string;
    RemovedNumber: Cardinal;
  begin
    Wildcard := '';
    if MessageInputQuery(Window,
      'Input node name to be removed. You can use wildcards (* and ?) in ' +
      'the expression below to match many node names. The input is ' +
      'case sensitive (like all VRML/X3D).', Wildcard) then
    begin
      Scene.BeforeNodesFree;
      RemovedNumber := Scene.RootNode.
        RemoveChildrenWithMatchingName(Wildcard, false);
      Scene.ChangedAll;
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
    R := TGamePlaceholdersRemover.Create;
    try
      Scene.BeforeNodesFree;
      Scene.RootNode.EnumerateReplaceChildren(
        {$ifdef CASTLE_OBJFPC}@{$endif} R.Remove);
      Scene.ChangedAll;
      MessageOK(Window, Format('Removed %d nodes.', [R.Count]));
    finally FreeAndNil(R) end;
  end;

  procedure PrintRayhunterCommand;
  var
    S: string;
    Pos, Dir, Up: TVector3;
  begin
    MainViewport.Camera.GetView(Pos, Dir, Up);

    S := FormatDot(
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
         Pos.ToRawString,
         Dir.ToRawString,
         Up.ToRawString,
         BGColor[0], BGColor[1], BGColor[2] ]);

    {$warnings off} // for now, this knowingly uses deprecated MainViewport.Projection
    case MainViewport.Projection.ProjectionType of
      ptPerspective:
        S := S + FormatDot('    --view-angle-x %f',
          [MainViewport.Projection.PerspectiveAngles[0]]);
      ptOrthographic:
        S := S + FormatDot('    --ortho %f %f %f %f', [
          MainViewport.Projection.Dimensions.Left,
          MainViewport.Projection.Dimensions.Bottom,
          MainViewport.Projection.Dimensions.Right,
          MainViewport.Projection.Dimensions.Top ]);
      else raise EInternalError.Create('PrintRayhunterCommand:ProjectionType?');
    end;
    {$warnings on}

    MessageReport(S);
  end;

  procedure PrintCameraSettingsPascal;

    function Vector3ToPascal(const V: TVector3): String;
    begin
      Result := FormatDot('Vector3(%f, %f, %f)', [V.Data[0], V.Data[1], V.Data[2]]);
    end;

  var
    Pos, Dir, Up, GravityUp: TVector3;
  begin
    MainViewport.Camera.GetView(Pos, Dir, Up, GravityUp);
    MessageReport(Format('// Set camera vectors using Castle Game Engine.' + NL +
      'Viewport.Camera.SetView(' + NL +
      '  %s, // position' + NL +
      '  %s, // direction' + NL +
      '  %s, // up (current)' + NL +
      '  %s // gravity up' + NL +
      ');',
      [ Vector3ToPascal(Pos),
        Vector3ToPascal(Dir),
        Vector3ToPascal(Up),
        Vector3ToPascal(GravityUp)
      ]));
  end;

  procedure PrintCameraSettings(const Version: TX3DCameraVersion;
    const Xml: boolean);
  var
    Pos, Dir, Up, GravityUp: TVector3;
  begin
    MainViewport.Camera.GetView(Pos, Dir, Up, GravityUp);
    MessageReport(MakeCameraStr(Version, Xml, Pos, Dir, Up, GravityUp));
  end;

  procedure WriteBoundingBox(const Box: TBox3D);
  var
    S1, S2, S3, S4: string;
  begin
    if Box.IsEmpty then
      MessageOK(Window, 'The bounding box is empty.') else
    begin
      { Workarounding FPC 3.1.1 internal error 200211262 in view3dscene.lpr }
      (*
      MessageReport(Format(
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
          Box.Middle.ToRawString,
          FloatToRawStr(Box.Data[1].Data[0] - Box.Data[0].Data[0]),
          FloatToRawStr(Box.Data[1].Data[1] - Box.Data[0].Data[1]),
          FloatToRawStr(Box.Data[1].Data[2] - Box.Data[0].Data[2]) ]));
      *)

      S1 := Box.Center.ToRawString;
      S2 := FormatDot('%g', [Box.Data[1].Data[0] - Box.Data[0].Data[0]]);
      S3 := FormatDot('%g', [Box.Data[1].Data[1] - Box.Data[0].Data[1]]);
      S4 := FormatDot('%g', [Box.Data[1].Data[2] - Box.Data[0].Data[2]]);
      MessageReport(Format(
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
        [Box.ToString, S1, S2, S3, S4]));
    end;
  end;

  procedure SetFillMode(Value: TFillMode);
  begin
    FillMode := Value;
    { For fmSilhouetteBorderEdges, these things can remain as they were
      previously. }
    if FillMode <> fmSilhouetteBorderEdges then
    begin
      Scene.Attributes.WireframeEffect := FillModes[FillMode].WireframeEffect;
      Scene.Attributes.WireframeColor  := FillModes[FillMode].WireframeColor;
      Scene.Attributes.Mode            := FillModes[FillMode].Mode;
    end;
  end;

  procedure ScreenShotToVideo(const Transparency: boolean);
  var
    TimeBegin, TimeStep: TFloatTime;
    FramesCount: Cardinal;
    URLPattern: string;
    Range: TRangeScreenShot;
  begin
    TimeBegin := Scene.Time;
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
              MakeAllScreenShotsFBO(Transparency);
            except
              on E: EInvalidScreenShotURL do
                MessageOk(Window, 'Making screenshot failed: ' + NL + NL + E.Message);
            end;

            ScreenShotsList.Clear;
          end;
  end;

(*
  TODO: Reimplement this without TCastlePrecalculatedAnimation.

  procedure PrecalculateAnimationFromEvents;
  var
    ScenesPerTime: Cardinal;
    TimeBegin, TimeEnd: Single;
    RootNode: TX3DRootNode;
  const
    Epsilon = 0.0001;
  begin
    if SceneAnimation.ScenesCount <> 1 then
    begin
      MessageOK(Window, 'This is not possible when you already have a precalculated animation (like loaded from castle-anim-frames or MD3 file).');
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
        time-dependent nodes state like isActive, etc., but also
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
        TimeBegin, TimeEnd, ScenesPerTime, Epsilon,
        'Precalculating animation');

      { Otherwise, current time is huge and it doesn't work reliably
        with division inside TCastlePrecalculatedAnimation.Scene.
        Do it *before* setting MainScene, as even setting MainScene
        may cause TCastlePrecalculatedAnimation.VisibleChangeNotification, which
        already requires TCastlePrecalculatedAnimation.Scene. }
      SceneAnimation.ResetTimeAtLoad;

      { Closing the scene freed MainViewport.MainScene (it's set to nil
        automagically by free notification). Set it correctly now. }
      MainViewport.MainScene := Scene;

      { Since we just destroyed RootNode, and replaced it with completely
        different scene, we have to recalculate many things.
        Recalculate octree.
        GeometryChanged takes care of invalidating SelectedItem and such. }
      SceneOctreeCreate;
      THelper.GeometryChanged(nil, true, nil);
      THelper.ViewpointsChanged(Scene);
    end;
  end;
*)

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
      if Shape.InternalOctreeTriangles = nil then
        MessageOk(Window, 'No collision octree was initialized for this shape.') else
      begin
        MessageReport(Shape.InternalOctreeTriangles.Statistics);
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

          GLCaptureCubeMapImages(CubeMapImg, MainViewport.Camera.Position,
            {$ifdef CASTLE_OBJFPC}@{$endif} TV3DViewport(MainViewport).RenderFromViewEverything,
            MainViewport.Camera.EffectiveProjectionNear,
            MainViewport.Camera.EffectiveProjectionFar);
          RenderContext.Viewport := Window.Rect;

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
            else raise EInternalError.Create('orient?');
          end;

          for Side := Low(Side) to High(Side) do
            FreeAndNil(CubeMapImg[Side]);
        end;
      end;
    end;
  end;

  procedure ScreenShotToCubeMapComposite;
  var
    Composite: TCompositeImage;
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
        Composite := GLCaptureCubeMapComposite(Size, MainViewport.Camera.Position,
          {$ifdef CASTLE_OBJFPC}@{$endif} TV3DViewport(MainViewport).RenderFromViewEverything,
          MainViewport.Camera.EffectiveProjectionNear,
          MainViewport.Camera.EffectiveProjectionFar);
        try
          RenderContext.Viewport := Window.Rect;
          Composite.SaveToFile(URL);
        finally FreeAndNil(Composite) end;
      end;
    end;
  end;

  procedure ScreenShotDepthToImage;

    procedure DoSave(const URL: string);
    var
      Image: TGrayscaleImage;
    begin
      { Just like TCastleWindowBase.SaveScreen, we have to force redisplay now
        (otherwise we could be left here with random buffer contents from
        other window obscuring us, or we could have depth buffer from
        other drawing routine (like "frozen screen" drawn under FileDialog). }
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;

      Image := TGrayscaleImage.Create(Window.Width, Window.Height);
      try
        BeforePackImage(Image);
        try
          glReadPixels(0, 0, Window.Width, Window.Height, GL_DEPTH_COMPONENT,
            ImageGLType(Image), Image.RawPixels);
        finally AfterPackImage(Image) end;

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
    Pos, Dir, Up: TVector3;
    BaseLights: TLightInstancesList;
  begin
    MainViewport.Camera.GetView(Pos, Dir, Up);
    BaseLights := TV3DViewport(MainViewport).BaseLightsForRaytracer;
    try
      {$warnings off} // using deprecated MainViewport.Projection, for now this is simplest
      RaytraceToWin(BaseLights, Scene, Pos, Dir, Up, MainViewport.Projection, BGColor);
      {$warnings on}
    finally FreeAndNil(BaseLights) end;
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

    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.');
      Exit;
    end;

    Shape := TShape(SelectedItem^.Shape);

    if not Shape.OriginalGeometry.InternalCoord(Shape.OriginalState, Coord) then
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
    Vis: TSkeletonVisualize;
  begin
    Vis := TSkeletonVisualize.Create;
    try
      Vis.JointVisualizationSize := Scene.BoundingBox.AverageSize(false, 1) / 20;
      if MessageInputQuery(Window, 'Joint Visualization Size (default based on scene size):',
        Vis.JointVisualizationSize) then
      begin
        Vis.VisualizeAllHumanoids(Scene.RootNode);
        MessageOK(Window, Format('%d H-Anim Humanoids (%d Joints inside) processed.', [
          Vis.HumanoidsProcessed,
          Vis.JointsProcessed
        ]));
        if Vis.HumanoidsProcessed <> 0 then
          Scene.ChangedAll;
      end;
    finally FreeAndNil(Vis) end;
  end;

  procedure VisualizeTransformations;
  var
    Vis: TSkeletonVisualize;
  begin
    Vis := TSkeletonVisualize.Create;
    try
      Vis.JointVisualizationSize := Scene.BoundingBox.AverageSize(false, 1) / 20;
      if MessageInputQuery(Window, 'Joint Visualization Size (default based on scene size):',
        Vis.JointVisualizationSize) then
      begin
        Vis.VisualizeAllTransformations(Scene.RootNode);
        MessageOK(Window, Format('%d Joints processed.', [
          Vis.JointsProcessed
        ]));
        if Vis.JointsProcessed <> 0 then
          Scene.ChangedAll;
      end;
    finally FreeAndNil(Vis) end;
  end;

  procedure SaveAs(const Encoding: TX3DEncoding; const MessageTitle: string;
    const ForceConvertingToX3D: boolean);
  var
    ProposedSaveName, Extension, FileFilters: string;
    SaveVersion: TX3DVersion;
    Conversion: boolean;
  begin
    SaveVersion := Save3DVersion(Scene.RootNode);
    Conversion := Save3DWillConvertToX3D(SaveVersion, Encoding, ForceConvertingToX3D);

    Extension := SaveVersion.FileExtension(Encoding, ForceConvertingToX3D);
    FileFilters := SaveVersion.FileFilters(Encoding, ForceConvertingToX3D);
    ProposedSaveName := ChangeURIExt(SceneURL, Extension);

    if Window.FileDialog(MessageTitle, ProposedSaveName, false, FileFilters) then
    try
      if Conversion then
        Scene.BeforeNodesFree;

      Save3D(Scene.RootNode, ProposedSaveName, SaveGenerator,
        ExtractURIName(SceneURL), SaveVersion, Encoding, ForceConvertingToX3D);

      if Conversion then
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
    F := ApplicationProperties.LimitFPS;
    if MessageInputQuery(Window,
      'Set approximate FPS (Frames Per Second) limit.' +NL+
      NL+
      'Smaller values give OS and CPU some rest, to run other applications or conserve laptop battery. Your monitor has a fixed refresh rate anyway, so limiting FPS doesn''t necessarily mean worse visuals (unless you set it to some really small value).' +NL+
      NL+
      'This is only an approximation of desired FPS. Usually it''s quite precise, but on some systems some values may be actually capped by monitor refresh rate.' +NL+
      NL+
      'Special value 0 means "do not limit FPS".',
      F, Format('%f', [F])) then
      ApplicationProperties.LimitFPS := Max(F, 0.0);
  end;

  procedure SwitchScreenSpaceAmbientOcclusion;
  var
    I: Integer;
  begin
    with MainViewport do
      ScreenSpaceAmbientOcclusion := not ScreenSpaceAmbientOcclusion;
    for I := Low(ExtraViewports) to High(ExtraViewports) do
      with ExtraViewports[I] do
        ScreenSpaceAmbientOcclusion := not ScreenSpaceAmbientOcclusion;
  end;

  procedure OpenSceneURL;
  var
    URL, CopyStr, CutStr, PasteStr: string;
  begin
    URL := SceneURL;
    Check(KeyToString(CtrlC, K_None, [], CopyStr));
    Check(KeyToString(CtrlX, K_None, [], CutStr));
    Check(KeyToString(CtrlV, K_None, [], PasteStr));
    if MessageInputQuery(Window,
      'Open 3D model from given URL.' + NL + NL +
      'Note that by default "http" is disabled (because the downloads are blocking for now, and every 3D model may reference additional resources like textures). Enable http by checking "Preferences -> Download Resources From Network".' + NL + NL +
      'You can copy (' + CopyStr +
      '), cut (' + CutStr +
      ') and paste (' + PasteStr +
      ') here, for example to easily paste URL from/to your web browser.' + NL + NL +
      'URL:', URL) then
      LoadScene(URL, []);
  end;

  procedure Reopen;
  var
    Pos, Dir, Up: TVector3;
    SavedNavigationType: TUserNavigationType;
  begin
    { reopen saves/restores camera view and navigation type,
      this makes it more useful }
    SavedNavigationType := NavigationType;
    MainViewport.Camera.GetView(Pos, Dir, Up{, GravityUp});

    LoadScene(SceneURL, []);

    { restore view, without GravityUp (trying to preserve it goes wrong
      in case we're in Examine mode, then "reopen", then switch to "Walk"
      --- original scene's gravity is then lost) }
    MainViewport.Camera.SetView(Pos, Dir, Up{, GravityUp});
    { restore NavigationType }
    SetNavigationType(SavedNavigationType);
    UpdateCameraUI;
  end;

  function SceneVertexTriangleInfo(const Scene: TCastleScene): string;
  const
    SSceneInfoTriVertCounts_Same = 'Scene contains %d triangles and %d ' +
      'vertices (with and without over-triangulating).';
    SSceneInfoTriVertCounts_1 =
      'When we don''t use over-triangulating (e.g. when we do collision '+
      'detection or ray tracing) scene has %d triangles and %d vertices.';
    SSceneInfoTriVertCounts_2 =
      'When we use over-triangulating (e.g. when we do OpenGL rendering) '+
      'scene has %d triangles and %d vertices.';
  begin
    if (Scene.VerticesCount(false) = Scene.VerticesCount(true)) and
       (Scene.TrianglesCount(false) = Scene.TrianglesCount(true)) then
      Result := Format(SSceneInfoTriVertCounts_Same,
        [Scene.TrianglesCount(false), Scene.VerticesCount(false)]) + NL else
    begin
      Result :=
        Format(SSceneInfoTriVertCounts_1,
          [Scene.TrianglesCount(false), Scene.VerticesCount(false)]) + NL +
        Format(SSceneInfoTriVertCounts_2,
          [Scene.TrianglesCount(true), Scene.VerticesCount(true)]) + NL;
    end;
  end;

  function SceneBoundingBoxInfo(const Scene: TCastleScene): string;
  var
    BBox: TBox3D;
  begin
    BBox := Scene.BoundingBox;
    Result := 'Bounding box : ' + BBox.ToString;
    if not BBox.IsEmpty then
    begin
      Result := Result + Format(', average size : %f', [BBox.AverageSize]);
    end;
    Result := Result + NL;
  end;

  function ManifoldEdgesInfo(const Scene: TCastleScene): string;
  var
    ManifoldEdges, BorderEdges: Cardinal;
  begin
    Scene.EdgesCount(ManifoldEdges, BorderEdges);
    Result := Format('Edges detection: all edges split into %d manifold edges and %d border edges. Remember that for shadow volumes, only the shapes that are perfect manifold (have zero border edges) can cast shadows.',
      [ManifoldEdges, BorderEdges]);
    Scene.FreeResources([frShadowVolume]);
  end;

  procedure HideSelectedShape;
  begin
    if SelectedItem = nil then
    begin
      MessageOk(Window, 'Nothing selected.');
      Exit;
    end;
    if SelectedItem^.Shape.Node = nil then
    begin
      MessageOk(Window, 'Cannot hide VRML 1.0 shape.');
      Exit;
    end;
    SelectedItem^.Shape.Node.Render := false;
  end;

  procedure RevealAllHiddenShapes;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Scene.Shapes.TraverseList({ OnlyActive } false);
    for Shape in ShapeList do
      if Shape.Node <> nil then
        Shape.Node.Render := true;
  end;

var
  C: Cardinal;
begin
  WalkNavigation := CurrentWalkNavigation;

  case MenuItem.IntData of
    10: THelper.ClickButtonOpen(nil);
    11: OpenSceneURL;

    12: Window.Close;

    15: Reopen;

    900: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), false);
    905: SaveAs(xeClassic, SRemoveMnemonics(MenuItem.Caption), true);
    910: SaveAs(xeXML    , SRemoveMnemonics(MenuItem.Caption), true { doesn't matter });

    21: ButtonWarnings.DoClick;

    31: ChangeScene([scNoNormals], Scene);
    32: ChangeScene([scNoSolidObjects], Scene);
    33: ChangeScene([scNoConvexFaces], Scene);

    34: RemoveNodesWithMatchingName;
    38: RemoveGamePlaceholders;

    42: VisualizeHumanoids;
    45: VisualizeTransformations;

    3500: with Scene do ShadowMaps := not ShadowMaps;
    3510..3519: Scene.Attributes.ShadowSampling :=
      TShadowSampling(Ord(MenuItem.IntData) - 3510);
    3520: with Scene.Attributes do VisualizeDepthMap := not VisualizeDepthMap;
    3530:
      begin
        C := Scene.ShadowMapsDefaultSize;
        if MessageInputQueryCardinal(Window, 'Input default shadow map size :' + NL + '(should be a power of 2)', C) then
        begin
          Scene.ShadowMapsDefaultSize := C;
        end;
      end;

    36: RemoveSelectedShape;
    37: RemoveSelectedFace;

    51: Scene.CameraTransition(MainViewport.Camera,
          DefaultX3DCameraPosition[cvVrml1_Inventor],
          DefaultX3DCameraDirection,
          DefaultX3DCameraUp,
          DefaultX3DGravityUp);
    52: Scene.CameraTransition(MainViewport.Camera,
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

    65: Viewpoints.Initial(MainViewport);
    66: Viewpoints.Previous(MainViewport);
    67: Viewpoints.Next(MainViewport);
    68: Viewpoints.Final(MainViewport);

    82: ShowBBox := not ShowBBox;
    84: if Window.ColorDialog(BGColor) then BGColorChanged;
    83: DynamicBatching := not DynamicBatching;
    86: with Scene.Attributes do Blending := not Blending;
    87: with Scene.Attributes do OcclusionSort := not OcclusionSort;
    88: with Scene.Attributes do UseOcclusionQuery := not UseOcclusionQuery;
    90: with Scene.Attributes do UseHierarchicalOcclusionQuery := not UseHierarchicalOcclusionQuery;
    891: with Scene.Attributes do DebugHierOcclusionQueryResults := not DebugHierOcclusionQueryResults;

    91: with Scene.Attributes do Lighting := not Lighting;
    92: with Scene do HeadLightOn := not HeadLightOn;
    93: with Scene.Attributes do UseSceneLights := not UseSceneLights;
    94: with Scene.Attributes do EnableTextures := not EnableTextures;
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
           MessageReport(SceneOctreeCollisions.Statistics)
         else
           MessageOk(Window, SOnlyWhenOctreeAvailable);
    103: if SceneOctreeRendering <> nil then
           MessageReport(SceneOctreeRendering.Statistics)
         else
           MessageOk(Window, SOnlyWhenOctreeAvailable);

    105: PrintRayhunterCommand;

    104: PrintCameraSettingsPascal;
    106: PrintCameraSettings(cvVrml1_Inventor, false);
    107: PrintCameraSettings(cvVrml2_X3d, false);
    108: PrintCameraSettings(cvVrml2_X3d, true);

  { Only for debugging:
           WritelnLog(
             'Current camera frustum planes :' +nl+
             '((A, B, C, D) means a plane given by equation A*x + B*y + C*z + D = 0.)' +nl+
             '  Left   : ' + MainViewport.Camera.Frustum.Planes[fpLeft].ToRawString +nl+
             '  Right  : ' + MainViewport.Camera.Frustum.Planes[fpRight].ToRawString +nl+
             '  Bottom : ' + MainViewport.Camera.Frustum.Planes[fpBottom].ToRawString +nl+
             '  Top    : ' + MainViewport.Camera.Frustum.Planes[fpTop].ToRawString +nl+
             '  Near   : ' + MainViewport.Camera.Frustum.Planes[fpNear].ToRawString);
           if MainViewport.Camera.Frustum.ZFarInfinity then
             WritelnLog(
             '  Far    : (No frustum far plane. That is, far plane is "at infinity".)') else
             WritelnLog(
             '  Far    : ' + MainViewport.Camera.Frustum.Planes[fpFar].ToRawString);
         end;
  }

    109: WriteBoundingBox(Scene.BoundingBox);
    110: WriteBoundingBox(Scene.BoundingBox);

    111: begin
           if NavigationType = High(NavigationType) then
             SetNavigationType(Low(NavigationType))
           else
             SetNavigationType(Succ(NavigationType));
           UpdateCameraUI;
         end;

    120: MessageReport(TextureMemoryProfiler.Summary);

    121: MessageReport('Scene "' + SceneURL + '" information:' + NL + NL +
           SceneVertexTriangleInfo(Scene) + NL +
           SceneBoundingBoxInfo(Scene));
    122: begin
           ShowStatus := not ShowStatus;
           UpdateStatusToolbarVisible;
         end;
    123: SetCollisions(not Scene.Collides, false);
    125: Raytrace;
    150: ScreenShotImage(SRemoveMnemonics(MenuItem.Caption), false);
    151: ScreenShotImage(SRemoveMnemonics(MenuItem.Caption), true);
    128: if WalkNavigation <> nil then
         begin
           WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
           UpdateCameraUI;
         end;
    129: MessageReport(ManifoldEdgesInfo(Scene));

    131: begin
           MessageReport(
             'view3dscene: VRML / X3D browser and full-featured viewer of other 3D models.' + NL +
             NL +
             'Supported formats:' + NL +
             '- X3D' + NL +
             '- glTF 2.0' + NL +
             '- Spine JSON' + NL +
             '- VRML 2.0 (aka VRML 97) and 1.0' + NL +
             '- castle-anim-frames' + NL +
             '- OpenInventor (IV)' + NL +
             '- 3DS' + NL +
             '- MD3' + NL +
             '- Wavefront OBJ' + NL +
             '- Collada' + NL +
             '- STL' + NL +
             NL +
             'Version ' + Version + '.' + NL +
             'By Michalis Kamburelis.' + NL +
             NL +
             'See ' + View3dsceneURL + ' .' + NL +
             NL +
             'Created using Castle Game Engine ( https://castle-engine.io/ ) version ' + CastleEngineVersion + '.' + NL +
             'Compiled with ' + SCompilerDescription + '.');
         end;
    132: if not OpenURL(View3dsceneURL) then
           Window.MessageOk(SCannotOpenURL, mtError);
    134: if not OpenURL(SupportURL) then
           Window.MessageOk(SCannotOpenURL, mtError);

    171: SelectedShowInformation;
    172: SelectedShowLightsInformation;
    173: MessageReport(GLInformationString);

    182: ChangePointSize;

    201: if WalkNavigation <> nil then
         begin
           WalkNavigation.Gravity := not WalkNavigation.Gravity;
           UpdateCameraUI;
         end;
    202: if WalkNavigation <> nil then
         begin
           WalkNavigation.PreferGravityUpForRotations := not WalkNavigation.PreferGravityUpForRotations;
           UpdateCameraUI;
         end;
    203: if WalkNavigation <> nil then
         begin
           WalkNavigation.PreferGravityUpForMoving := not WalkNavigation.PreferGravityUpForMoving;
           UpdateCameraUI;
         end;
    205: ChangeMoveSpeed;

    220: begin
           AnimationTimePlaying := not AnimationTimePlaying;
           Scene.TimePlaying := AnimationTimePlaying and ProcessEventsWanted;
         end;
    222: ChangeTimeSpeed;
    223: ChangeBakedAnimationSmoothness;

    224: begin
           ProcessEventsWanted := not ProcessEventsWanted;
           Scene.TimePlaying := AnimationTimePlaying and ProcessEventsWanted;
           Scene.ProcessEvents := ProcessEventsWanted;
         end;

  //  225: PrecalculateAnimationFromEvents;

    22800: Scene.AnimateSkipTicks := 0;
    22810: Scene.AnimateSkipTicks := 1;
    22820: Scene.AnimateSkipTicks := 2;

    230: ToggleNamedAnimationsUi;

    300: JumpToViewpoint(MainViewport, (MenuItem as TMenuItemViewpoint).Viewpoint);

    340: SwitchScreenSpaceAmbientOcclusion;
    350..370:
      begin
        ScreenEffects.ActiveEffectsRecalculate;
        { All that is needed to actually render with the new effect is to
          actually redisplay. }
        Window.Invalidate;
      end;

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

    540: ScreenShotToVideo(false);
    542: ScreenShotToVideo(true);
    550: ScreenShotToCubeMap;
    555: ScreenShotToCubeMapComposite;
    560: ScreenShotDepthToImage;
    570: ControlsOnScreenshot := not ControlsOnScreenshot;

    600..649: Window.AntiAliasing := TAntiAliasing(MenuItem.IntData - 600);

    710: ChangeMaterialDiffuse;
    720: ChangeMaterialSpecular;
    722: LoadMaterialProperties;
    723: CleanMaterialProperties;
    725: if LightsEditorIsOpen then
           LightsEditorClose
         else
           LightsEditorOpen(MainViewport, V3DSceneWindow.Window, ToolbarPanel.EffectiveHeight);
    730: MergeCloseVertexes;

    750: ShadowVolumes := not ShadowVolumes;
    760: ShadowVolumesRender := not ShadowVolumesRender;

    770: InitialShowBBox := not InitialShowBBox;
    771: InitialShowStatus := not InitialShowStatus;

    801: SoundEngine.Enabled := not SoundEngine.Enabled;
    810..850: SoundEngine.Device :=
      SoundEngine.Devices[MenuItem.IntData - 810].Name;

    2000: SetLimitFPS;
    2010: EnableNetwork := not EnableNetwork;

    3010: MakeGravityUp(Vector3(0, 1, 0));
    3020: MakeGravityUp(Vector3(0, 0, 1));

    1100..1199: SetMinificationFilter(
      TMinificationFilter  (MenuItem.IntData-1100), Scene);
    1200..1299: SetMagnificationFilter(
      TMagnificationFilter  (MenuItem.IntData-1200), Scene);
    1300..1399:
      begin
        SetNavigationType(TUserNavigationType(MenuItem.IntData - 1300));
        UpdateCameraUI;
      end;
    1400..1499: Scene.Attributes.BumpMapping :=
      TBumpMapping(MenuItem.IntData - 1400);
    3600..3610: SetViewportsConfig(TViewportsConfig(MenuItem.IntData - 3600),
      V3DSceneWindow.Window, MainViewport);
    4000: Scene.Attributes.PhongShading := not Scene.Attributes.PhongShading;
    5000..5099: GammaCorrection := TGammaCorrection(MenuItem.IntData - 5000);
    5100..5199: ToneMapping := TToneMapping(MenuItem.IntData - 5100);
    5200: GltfForcePhongMaterials := not GltfForcePhongMaterials;
    4100: HideSelectedShape;
    4110: RevealAllHiddenShapes;
    else raise EInternalError.Create('not impl menu item');
  end;

  { This may be called when headlight on / off state changes,
    so prVisibleSceneNonGeometry is possible.
    For safety, pass also prVisibleSceneGeometry now. }
  Scene.VisibleChangeHere(
    [vcVisibleGeometry, vcVisibleNonGeometry]);
end;

function CreateMainMenu: TMenu;

  procedure AppendNavigationTypes(M: TMenu);
  var
    NT: TUserNavigationType;
    Group: TMenuItemRadioGroup;
  begin
    Group := M.AppendRadioGroup(NavigationNames, 1300, Ord(NavigationType), true);
    for NT := Low(NT) to High(NT) do
      CameraRadios[NT] := Group[Ord(NT)];
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
        SQuoteMenuEntryCaption(SoundEngine.Devices[I].Caption),
        BaseIntData + I, SoundEngine.Devices[I].Name = SoundEngine.Device, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
    end;
  end;

const
  ToneMappingNames: array [TToneMapping] of String = (
    'None',
    'Uncharted',
    'Hejl-Richard',
    'ACES'
  );
  GammaCorrectionNames: array [TGammaCorrection] of String = (
    'None',
    'On Physical Materials',
    'Always'
  );

var
  M, M2, M3: TMenu;
  NextRecentMenuItem: TMenuEntry;
  WalkNavigation: TCastleWalkNavigation;
begin
  WalkNavigation := CurrentWalkNavigation;

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
      M2.Append(TMenuItemChecked.Create('Sound', 801, SoundEngine.Enabled, true));
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
    M.Append(TMenuItemChecked.Create('Phong Shading on Everything', 4000, Scene.Attributes.PhongShading, true));
    M2 := TMenu.Create('Gamma Correction');
      M2.AppendRadioGroup(GammaCorrectionNames, 5000, Ord(GammaCorrection), true);
      M.Append(M2);
    M2 := TMenu.Create('Tone Mapping');
      M2.AppendRadioGroup(ToneMappingNames, 5100, Ord(ToneMapping), true);
      M.Append(M2);
    M2 := TMenu.Create('Bump Mapping');
      M2.AppendRadioGroup(BumpMappingNames, 1400, Ord(Scene.Attributes.BumpMapping), true);
      M.Append(M2);
    M2 := TMenu.Create('Shadow Maps');
      M2.Append(TMenuItemChecked.Create('Enable', 3500, Scene.ShadowMaps, true));
      M2.Append(TMenuSeparator.Create);
      M2.AppendRadioGroup(ShadowSamplingNames, 3510,
        Ord(Scene.Attributes.ShadowSampling), true);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItemChecked.Create('Visualize Depths', 3520, Scene.Attributes.VisualizeDepthMap, true));
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
      'Load glTF materials as Phong (Faster, Requires Reload)', 5200,
      GltfForcePhongMaterials, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      '_Lighting Calculation',         91,
      Scene.Attributes.Lighting, true));
    MenuHeadlight := TMenuItemChecked.Create('_Headlight', 92, CtrlH,
      (Scene <> nil) and Scene.HeadlightOn, true);
    M.Append(MenuHeadlight);
    M.Append(TMenuItemChecked.Create('Use Scene Lights',    93,
      Scene.Attributes.UseSceneLights, true));
    M.Append(TMenuItem.Create('Light Global Ambient Color ...',  95));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Textures',           94, CtrlT,
      Scene.Attributes.EnableTextures, true));
    M2 := TMenu.Create('Texture Minification Method');
      MenuAppendMinificationFilters(M2, 1100);
      M.Append(M2);
    M2 := TMenu.Create('Texture Magnification Method');
      MenuAppendMagnificationFilters(M2, 1200);
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Blending',                86,
      Scene.Attributes.Blending, true));
    M.Append(TMenuItemChecked.Create('Dynamic Batching',        83,
      DynamicBatching, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Occlusion Sort', 87,
      Scene.Attributes.OcclusionSort, true));
    M.Append(TMenuItemChecked.Create('_Use Occlusion Query', 88,
      Scene.Attributes.UseOcclusionQuery, true));
    M.Append(TMenuItemChecked.Create('Use Hierarchical Occlusion Query', 90,
      Scene.Attributes.UseHierarchicalOcclusionQuery, true));
    M.Append(TMenuItemChecked.Create('Debug Last Hierarchical Occlusion Query Results', 891,
      Scene.Attributes.DebugHierOcclusionQueryResults, true));
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
      MenuMouseLook := TMenuItemChecked.Create(
        '_Use Mouse Look',                       128, CtrlM,
        (WalkNavigation <> nil) and WalkNavigation.MouseLook, false);
      M2.Append(MenuMouseLook);
      MenuGravity := TMenuItemChecked.Create(
        '_Gravity',                              201, CtrlG,
        (WalkNavigation <> nil) and WalkNavigation.Gravity, false);
      M2.Append(MenuGravity);
      MenuPreferGravityUpForRotations := TMenuItemChecked.Create(
        'Rotate with Respect to Gravity Vector',      202,
        (WalkNavigation <> nil) and WalkNavigation.PreferGravityUpForRotations, false);
      M2.Append(MenuPreferGravityUpForRotations);
      MenuPreferGravityUpForMoving := TMenuItemChecked.Create(
        'Move with Respect to Gravity Vector',          203,
        (WalkNavigation <> nil) and WalkNavigation.PreferGravityUpForMoving, false);
      M2.Append(MenuPreferGravityUpForMoving);
      M2.Append(TMenuItem.Create('Change Move Speed...', 205));
      M.Append(M2);
    M.Append(TMenuItem.Create('Set Up (and Gravity Up) +Y',  3010));
    M.Append(TMenuItem.Create('Set Up (and Gravity Up) +Z',  3020));
    MenuCollisions := TMenuItemChecked.Create(
      '_Collision Detection and Picking',                123, CtrlC,
        Scene.Collides, true);
    M.Append(MenuCollisions);
    Result.Append(M);
  M := TMenu.Create('_Animation');
    MenuNamedAnimations := TMenuItemChecked.Create('Animations Panel', 230, CtrlA, NamedAnimationsUiExists, false);
    M.Append(MenuNamedAnimations);
    M.Append(TMenuSeparator.Create);
    MenuAnimationTimePlaying := TMenuItemChecked.Create(
      '_Playing / Paused',   220, CtrlP, AnimationTimePlaying, true);
    M.Append(MenuAnimationTimePlaying);
    M.Append(TMenuItem.Create('Playing Speed...', 222));
    M.Append(TMenuItem.Create('Baked Animation Smoothness ...', 223));
    M.Append(TMenuItemChecked.Create('Process VRML/X3D Events ("off" pauses also animation)', 224, ProcessEventsWanted, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Do Not Optimize (Update Every Frame)', 22800));
    M.Append(TMenuItem.Create('Optimize 1 (Skip Updating for 1 Frame Between)', 22810));
    M.Append(TMenuItem.Create('Optimize 2 (Skip Updating for 2 Frames Between)', 22820));
    // M.Append(TMenuItem.Create('Precalculate Animation from VRML/X3D Events ...', 225));
    Result.Append(M);
  M := TMenu.Create('_Edit');
    MenuHideSelectedShape :=
      TMenuItem.Create('Hide _Shape (containing selected triangle)', 4100);
    M.Append(MenuHideSelectedShape);
    M.Append(TMenuItem.Create('Reveal All Hidden Shapes', 4110));
    M.Append(TMenuSeparator.Create);
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
    M.Append(TMenuItem.Create('Add Joints Visualization ...', 45));
    M.Append(TMenuItem.Create('Add H-Anim Joints Visualization ...', 42));
    Result.Append(M);
  M := TMenu.Create('_Clipboard');
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (X3D XML)', 108));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 2.0, X3D classic)', 107));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 1.0)',   106));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (Pascal)',   104));
    M.Append(TMenuItem.Create('Print _rayhunter Command-line to Render This View', 105));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Print _Bounding Box (of whole animation)', 109));
    M.Append(TMenuItem.Create('Print Bounding Box (of current _animation frame)', 110));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Print Texture Memory Usage (run with --debug-texture-memory)', 120));
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
    M.Append(TMenuItem.Create('Screenshot to Video / Multiple Images (Transparent Background) ...', 542));
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
    M.Append(TMenuItem.Create('Support the development of view3dscene', 134));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About view3dscene',                  131));
    Result.Append(M);
end;

{ toolbar -------------------------------------------------------------------- }

var
  ButtonOpen, ButtonScreenshot: TCastleButton;

{ call when ShowStatus or MakingScreenShot changed }
procedure UpdateStatusToolbarVisible;
var
  Vis: boolean;
begin
  Vis := ShowStatus and not MakingScreenShot;

  if ToolbarPanel <> nil then // check was CreateMainUserInterface called already?
  begin
    ToolbarPanel.Exists := Vis;

    { Note that ButtonWarnings ignores the Vis.
      This is by design --- always signal warnings. }
  end;

  if StatusText <> nil then
    StatusText.Exists := Vis;
end;

{ Initialize StatusText and ToolbarPanel and various buttons instances }
procedure CreateMainUserInterface;
var
  ToolbarHorizGroup: TCastleHorizontalGroup;
  NT: TUserNavigationType;
  ToolbarBackground, PanelSeparator1, PanelSeparator2: TCastleImageControl;
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
const
  MinImageHeight = 22;
begin
  StatusText := TExtendedStatusText.Create(Application);
  Window.Controls.InsertFront(StatusText);

  UiOwner := TComponent.Create(Application);
  Ui := StringToComponent({$I ../embedded_data/designs/main.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  Window.Controls.InsertFront(Ui);

  ToolbarPanel := UiOwner.FindRequiredComponent('ToolbarPanel') as TCastleUserInterface;

  { Note that we need to assign all images,
    because we embed all images in view3dscene binary. }

  ButtonOpen := UiOwner.FindRequiredComponent('ButtonOpen') as TCastleButton;
  ButtonOpen.OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickButtonOpen;
  ButtonOpen.Image.Image := V3DSceneImages.Open;
  ButtonOpen.Image.OwnsImage := false;
  ButtonOpen.Image.AlphaChannel := acTest;
  ButtonOpen.MinImageHeight := MinImageHeight;

  ButtonCollisions := UiOwner.FindRequiredComponent('ButtonCollisions') as TCastleButton;
  ButtonCollisions.OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickButtonCollisions;
  ButtonCollisions.MinImageHeight := MinImageHeight;
  { When Scene = nil, make Pressed = true,
    because it means the Scene will be soon created with Scene.Collides = default true. }
  ButtonCollisions.Pressed := (Scene = nil) or Scene.Collides;

  ButtonScreenshot := UiOwner.FindRequiredComponent('ButtonScreenshot') as TCastleButton;
  ButtonScreenshot.OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickButtonScreenshot;
  ButtonScreenshot.Image.Image := V3DSceneImages.Screenshot;
  ButtonScreenshot.Image.OwnsImage := false;
  ButtonScreenshot.Image.AlphaChannel := acTest;
  ButtonScreenshot.MinImageHeight := MinImageHeight;

  ButtonAnimations := UiOwner.FindRequiredComponent('ButtonAnimations') as TCastleButton;
  ButtonAnimations.OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickButtonAnimations;
  ButtonAnimations.Toggle := true;
  ButtonAnimations.Image.Image := V3DSceneImages.Animations;
  ButtonAnimations.Image.OwnsImage := false;
  ButtonAnimations.MinImageHeight := MinImageHeight;

  ButtonWarnings := UiOwner.FindRequiredComponent('ButtonWarnings') as TCastleButton;
  ButtonWarnings.OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickButtonWarnings;
  ButtonWarnings.Image.Image := Warning_icon;
  ButtonWarnings.Image.OwnsImage := false;
  ButtonWarnings.MinImageHeight := MinImageHeight;

  ToolbarBackground := UiOwner.FindRequiredComponent('ToolbarBackground') as TCastleImageControl;
  ToolbarBackground.Image := Panel;
  ToolbarBackground.OwnsImage := false;

  PanelSeparator1 := UiOwner.FindRequiredComponent('PanelSeparator1') as TCastleImageControl;
  PanelSeparator1.Image := PanelSeparator;
  PanelSeparator1.OwnsImage := false;

  PanelSeparator2 := UiOwner.FindRequiredComponent('PanelSeparator2') as TCastleImageControl;
  PanelSeparator2.Image := PanelSeparator;
  PanelSeparator2.OwnsImage := false;

  if SceneWarnings <> nil then
    UpdateButtonWarnings
  else
    ButtonWarnings.Exists := false; { at least initialize Exists }

  ToolbarHorizGroup := UiOwner.FindRequiredComponent('ToolbarHorizGroup') as TCastleHorizontalGroup;

  for NT := Low(NT) to High(NT) do
    { Don't show button for ntNone.
      For ntNone it's confusing for new user.
      The "none" navigation type is visible in menu. }
    if NT <> untNone then
    begin
      CameraButtons[NT] := TNavigationTypeButton.Create(Application, NT);
      CameraButtons[NT].Caption := NavigationNames[NT];
      CameraButtons[NT].OnClick := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).ClickNavigationTypeButton;
      CameraButtons[NT].Toggle := true;
      CameraButtons[NT].MinImageHeight := MinImageHeight;
      ToolbarHorizGroup.InsertControl(2 + Ord(NT), CameraButtons[NT]);
    end;

  CameraButtons[untExamine].Image.Image := V3DSceneImages.Examine;
  CameraButtons[untExamine].Image.OwnsImage := false;
  CameraButtons[untFly].Image.Image := V3DSceneImages.Fly;
  CameraButtons[untFly].Image.OwnsImage := false;
  CameraButtons[untWalk].Image.Image := V3DSceneImages.Walk;
  CameraButtons[untWalk].Image.OwnsImage := false;

  UpdateStatusToolbarVisible;
end;

procedure Resize(Container: TUIContainer);
begin
  ResizeViewports(V3DSceneWindow.Window, MainViewport);
end;

class procedure THelper.ClickButtonOpen(Sender: TObject);
var
  URL: string;
begin
  URL := SceneURL;
  if Window.FileDialog('Open file', URL, true, LoadScene_FileFilters) then
    LoadScene(URL, []);
end;

class procedure THelper.ClickNavigationTypeButton(Sender: TObject);
begin
  SetNavigationType((Sender as TNavigationTypeButton).NavigationType);
  UpdateCameraUI;
end;

class procedure THelper.ClickButtonCollisions(Sender: TObject);
begin
  SetCollisions(not Scene.Collides, true);
end;

class procedure THelper.ClickButtonScreenshot(Sender: TObject);
begin
  ScreenShotImage('Screenshot to Image', false);
end;

class procedure THelper.ClickButtonAnimations(Sender: TObject);
begin
  ToggleNamedAnimationsUi;
end;

{ Try to lower anti-aliasing (multi-sampling) and shadows (stencil buffer)
  requirements and initialize worse GL context. }
function RetryOpen(Window: TCastleWindowBase): boolean;
begin
  if Window.AntiAliasing <> aaNone then
  begin
    Window.AntiAliasing := aaNone;
    if AntiAliasingMenu[Window.AntiAliasing] <> nil then
      AntiAliasingMenu[Window.AntiAliasing].Checked := true;
    WritelnLog('OpenGL context cannot be initialized. Multi-sampling (anti-aliasing) turned off, trying to initialize once again.');
    Result := true;
  end else
  if Window.StencilBits > 0 then
  begin
    Window.StencilBits := 0;
    WritelnLog('OpenGL context cannot be initialized. Stencil buffer (shadow volumes) turned off, trying to initialize once again.');
    Result := true;
  end else
    Result := false;
end;

{ main --------------------------------------------------------------------- }

var
  WasParam_Write: boolean = false;
  Param_WriteEncoding: TX3DEncoding = xeClassic;
  Param_WriteForceX3D: boolean = false;
  WasParam_SceneURL: boolean = false;
  Param_SceneURL: string;
  Param_SceneChanges: TSceneChanges = [];
  Param_HideMenu: boolean = false;
  Param_ScreenshotTransparent: boolean = false;
  Param_EnableFixedFunction: boolean = false;

const
  Options: array [0..23] of TOption =
  (
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
    (Short:  #0; Long: 'debug-log-videos'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log-textures'; Argument: oaNone),
    (Short:  #0; Long: 'anti-alias'; Argument: oaRequired),
    (Short: 'H'; Long: 'hide-extras'; Argument: oaNone),
    (Short:  #0; Long: 'write'; Argument: oaNone),
    (Short:  #0; Long: 'write-encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'write-force-x3d'; Argument: oaNone),
    (Short:  #0; Long: 'hide-menu'; Argument: oaNone),
    (Short:  #0; Long: 'debug-texture-memory'; Argument: oaNone),
    (Short:  #0; Long: 'screenshot-transparent'; Argument: oaNone),
    (Short:  #0; Long: 'debug-enable-fixed-function'; Argument: oaNone),
    (Short:  #0; Long: 'project'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  { Set ApplicationDataOverride to make castle-data:/ protocol work with respect
    to given project directory (or CastleEngineManifest.xml file). }
  procedure SetProject(const DirOrManifestFile: String);
  var
    DirOrManifestFileUrl, ManifestUrl, DirUrl: String;
  begin
    DirOrManifestFileUrl := AbsoluteURI(DirOrManifestFile);
    if ExtractURIName(DirOrManifestFileUrl) = 'CastleEngineManifest.xml' then
    begin
      ManifestUrl := DirOrManifestFileUrl;
      DirUrl := ExtractURIPath(DirOrManifestFileUrl)
    end else
    begin
      DirUrl := URIIncludeSlash(DirOrManifestFileUrl);
      ManifestUrl := DirUrl + 'CastleEngineManifest.xml';
    end;

    if not URIFileExists(ManifestUrl) then
      raise EInvalidParams.CreateFmt('Castle Game Engine project not recognized in this directory, "%s" does not exist', [
        ManifestUrl
      ]);

    ApplicationDataOverride := DirUrl + 'data/';
    if not (URIExists(URIExcludeSlash(ApplicationDataOverride)) in [ueDirectory, ueUnknown]) then
      WritelnWarning('Castle Game Engine project found, but "data" subdirectory does not exist ("%s"). Effectively, nothing will resolve to "castle-data:/" protocol.', [
        ApplicationDataOverride
      ]);
  end;

var
  SingleScreenShot: TSingleScreenShot;
  RangeScreenShot: TRangeScreenShot;
  S: String;
begin
  case OptionNum of
    0 : Include(Param_SceneChanges, scNoNormals);
    1 : Include(Param_SceneChanges, scNoSolidObjects);
    2 : Include(Param_SceneChanges, scNoConvexFaces);
    3 : begin
          WasParam_Write := true;
          Param_WriteEncoding := xeClassic;
        end;
    4 : begin
          S :=
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
            '  --write-force-x3d     Force conversion from VRML to X3D with --write option.' +NL+
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
            '  --screenshot-transparent' +NL+
            '                        Screenshots background is transparent.' +NL+
            '                        Useful only together' +NL+
            '                        with --screenshot-range or --screenshot options.' +NL+
            '  --viewpoint NAME      Use the viewpoint with given name or index as initial.' +NL+
            '                        Especially useful to make a screenshot from given viewpoint.' +NL+
            '  --anti-alias AMOUNT   Use full-screen anti-aliasing.' +NL+
            '                        Argument AMOUNT is an integer >= 0.' +NL+
            '                        Exact 0 means "no anti-aliasing",' +NL+
            '                        this is the default. Each successive integer' +NL+
            '                        generally makes method one step better.' +NL+
            '                        Especially useful to make a screenshot with' +NL+
            '                        anti-aliasing quality.' +NL+
            '  --project DIR         Point view3dscene to Castle Game Engine' +NL+
            '                        project directory (or CastleEngineManifest.xml file)' +NL+
            '                        to resolve the "castle-data:/" URLs in files.' +NL+
            SoundEngine.ParseParametersHelp + NL+
            NL+
            TCastleWindowBase.ParseParametersHelp(StandardParseOptions, true) +NL+
            NL+
            'Debug options:' +NL+
            '  --debug-log           Deprecated. We now log by default.' +NL+
            '  --debug-log-cache     Write log info, including cache.' +nl+
            '  --debug-log-shaders   Write log info, including shader source and log.' +nl+
            '  --debug-log-changes   Write log info, including VRML/X3D graph changes.' +nl+
            '  --debug-log-videos    Write log info, including videos loading and cache.' +nl+
            '  --debug-texture-memory Profile GPU texture memory usage.' +nl+
            OptionDescription('--debug-enable-fixed-function', 'Enable OpenGL fixed-function pipeline for some rendering.') +NL+
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
            NL+
            SCastleEngineProgramHelpSuffix(DisplayApplicationName, Version, true);

          if IsConsole then
            Writeln(S)
          else
            WindowMessageOK(S);
          Halt;
        end;
    5 : begin
          InfoWrite(Version);
          Halt;
        end;
    6 : begin
          SingleScreenShot := TSingleScreenShot.Create;
          SingleScreenShot.Time := StrToFloat(SeparateArgs[1]);
          SingleScreenShot.URLPattern := SeparateArgs[2];
          ScreenShotsList.Add(SingleScreenShot);
        end;
    7 : begin
          RangeScreenShot := TRangeScreenShot.Create;
          RangeScreenShot.TimeBegin := StrToFloat(SeparateArgs[1]);
          RangeScreenShot.TimeStep := StrToFloat(SeparateArgs[2]);
          RangeScreenShot.FramesCount := StrToInt(SeparateArgs[3]);
          RangeScreenShot.URLPattern := SeparateArgs[4];
          ScreenShotsList.Add(RangeScreenShot);
        end;
    8 : ; // we now call InitializeLog always
    9 : LogChanges := true;
    10: LogRendererCache := true;
    11: LogShaders := true;
    12: LogVideosCache := true;
    13: LogTextureCache := true;
    14: begin
          Window.AntiAliasing := TAntiAliasing(Clamped(StrToInt(Argument),
            Ord(Low(TAntiAliasing)), Ord(High(TAntiAliasing))));
          if AntiAliasingMenu[Window.AntiAliasing] <> nil then
            AntiAliasingMenu[Window.AntiAliasing].Checked := true;
        end;
    15: begin
          ShowBBox := false;
          ShowStatus := false;
          UpdateStatusToolbarVisible;
        end;
    16: WasParam_Write := true;
    17: if SameText(Argument, 'classic') then
          Param_WriteEncoding := xeClassic else
        if SameText(Argument, 'xml') then
          Param_WriteEncoding := xeXML else
          raise EInvalidParams.CreateFmt('Invalid --write-encoding argument "%s"', [Argument]);
    18: Param_WriteForceX3D := true;
    19: Param_HideMenu := true;
    20: TextureMemoryProfiler.Enabled := true;
    21: Param_ScreenshotTransparent := true;
    22: Param_EnableFixedFunction := true;
    23: SetProject(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

procedure Run;
begin
  { Set nice application name.
    The default application name is determined from ParamStr(0),
    but that is sometimes not optimal:
    - Under Windows ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unceratain for Unixes -- it contains whatever caller set.
    - ApplicationName is used for Config.URL by ApplicationConfig, so it better be reliable. }
  ApplicationProperties.ApplicationName := 'view3dscene';
  ApplicationProperties.Version := Version;

  // Initialize log as early as possible, but avoid messing --help/--version output
  if not Parameters.IsPresent([
        '-h',
        '--help',
        '-v',
        '--version'
      ]) then
    InitializeLog;

  Window := TCastleWindowBase.Create(Application);

  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { load config, before SoundEngine.ParseParameters
    (that may change SoundEngine.Enable by --no-sound) and
    after creating Window (various OnLoad / OnSave listeners in V3DSceneXxx units
    may use Window instance). }
  UserConfig.Load;
  SoundEngine.LoadFromConfig(UserConfig);

  { initialize RecentMenu }
  RecentMenu := TWindowRecentFiles.Create(nil);
  RecentMenu.OnOpenRecent := {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).OpenRecent;
  RecentMenu.LoadFromConfig(UserConfig);

  { parse parameters }
  Window.ParseParameters(StandardParseOptions);
  SoundEngine.ParseParameters;
  ViewpointsParseParameters;
  Parameters.Parse(Options, @OptionProc, nil);
  { the most important param : URL to load }
  if Parameters.High > 1 then
    raise EInvalidParams.Create('Excessive command-line parameters. Expected at most one URL to load') else
  if Parameters.High = 1 then
  begin
    WasParam_SceneURL := true;
    Param_SceneURL := Parameters[1];
  end;

  MainViewport := TV3DViewport.Create(nil);
  MainViewport.FullSize := true;
  MainViewport.AutoCamera := true;
  MainViewport.AutoNavigation := true;
  { do not use lights from MainViewport.Items.MainScene on other scenes }
  MainViewport.UseGlobalLights := false;
  Window.Controls.InsertBack(MainViewport);
  MainViewport.OnBoundViewpointChanged :=
    {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).BoundViewpointChanged;
  MainViewport.OnBoundNavigationInfoChanged :=
    {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).BoundNavigationInfoChanged;

  InitializeViewports(TV3DViewport);
  BGColorChanged;

  CreateMainUserInterface;

  Window.Controls.InsertBack(ScreenEffects);

  SceneWarnings := TSceneWarnings.Create;
  try
    ApplicationProperties.OnWarning.Add(
      {$ifdef CASTLE_OBJFPC}@{$endif} THelper(nil).OnWarningHandle);

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
    Scene := TCastleScene.Create(nil);
    try
      AttributesLoadFromConfig(Scene.Attributes);
      MainViewport.Items.Add(Scene);
      MainViewport.Items.MainScene := Scene;

      SceneBoundingBox := TBoundingBoxScene.Create(Scene);
      MainViewport.Items.Add(SceneBoundingBox);

      SceneDebugEdges := TDebugEdgesScene.Create(Scene);
      MainViewport.Items.Add(SceneDebugEdges);

      InitNavigation(MainViewport);
      InitTextureFilters(Scene);

      { init "scene global variables" to non-null values }
      LoadClearScene;
      try
        Theme.DialogsLight;

        Window.GtkIconName := 'view3dscene';
        Window.MainMenu := CreateMainMenu;
        Window.MainMenuVisible := not Param_HideMenu;
        Window.OnMenuClick := @MenuClick;
        Window.OnResize := @Resize;
        Window.OnPress := @Press;
        Window.OnDropFiles := @DropFiles;
        Window.AutoRedisplay := false;

        if MakingScreenShot then
        begin
          { --geometry must work as reliably as possible in this case.
            This is only in case FBO will not be available,
            TGLRenderToTexture will then use normal window contents. }
          Window.ResizeAllowed := raNotAllowed;

          { Do not show window on the screen, since we're working in batch mode. }
          Window.Visible := false;

          { Don't make sound when rendering in batch mode. }
          SoundEngine.Enabled := false;
          SoundEngine.EnableSaveToConfig := false;
        end;

        Window.FpsShowOnCaption := true;
        Window.StencilBits := 8;

        Window.Open(@RetryOpen);

        if WasParam_SceneURL then
          LoadScene(Param_SceneURL, Param_SceneChanges) else
          LoadWelcomeScene;

        if Param_EnableFixedFunction then
          GLFeatures.EnableFixedFunction := true;

        if MakingScreenShot then
        begin
          MakeAllScreenShotsFBO(Param_ScreenshotTransparent);
          Exit;
        end;

        Application.Run;
      finally FreeScene end;

      AttributesSaveToConfig(Scene.Attributes);

      SoundEngine.SaveToConfig(UserConfig);
      RecentMenu.SaveToConfig(UserConfig);
      UserConfig.Save;
    finally
      FreeAndNil(Scene);
      FreeAndNil(RecentMenu);
    end;
  finally
    FreeAndNil(SceneWarnings);
    FreeAndNil(MainViewport);
  end;
end;

begin
  try
    Run;
  except
    on E: TObject do
    begin
      { In case of exception, write nice message and exit with non-zero status,
        without dumping any stack trace (because it's normal to
        exit with exception in case of project/environment error, not a bug,
        and the stack trace is mostly useless for end-users in -dRELEASE mode). }
      ErrorWrite(ExceptMessage(E));
      Halt(1);
    end;
  end;
end.
