{
  Copyright 2002-2024 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Model viewer for all scene formats supported by Castle Game Engine
  (glTF, X3D, Spine JSON, sprite sheets... ).
  See https://castle-engine.io/creating_data_model_formats.php for a list of supported formats.
  See https://castle-engine.io/castle-model-viewer for user documentation.

  Note: If you want to learn how to use "Castle Game Engine",
  the castle-model-viewer source code isn't the best place to study.
  It's long and uses some obscure CGE features sometimes.
  Look instead at Castle Game Engine examples and manual:
  https://castle-engine.io/manual_intro.php .
}

program castle_model_viewer;

{$I v3dsceneconf.inc}

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

{ Icon for Mac OS X. .res file managed by Lazarus. }
{$ifdef DARWIN}
  {$R *.res}
{$endif}

{ Catch exceptions at loading and saving, and display them as nice messages to user.
  This should be defined for users.
  For debug purposes you may want to not define it.
  Then our standard exception handler will show, with a useful backtrace. }
{$define CATCH_EXCEPTIONS}

uses SysUtils, Math, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  { CGE units }
  CastleUtils, CastleVectors, CastleBoxes, CastleClassUtils,
  CastleTriangles, CastleApplicationProperties, CastleParameters, CastleCameras,
  CastleOpenDocument, CastleConfig, CastleStringUtils, CastleFilesUtils,
  CastleTimeUtils, CastleLog, DateUtils, CastleFrustum,
  CastleImages, CastleInternalCubeMaps, CastleInternalCompositeImage, CastleTransform, CastleSoundEngine,
  CastleUIControls, CastleColors, CastleKeysMouse, CastleDownload, CastleUriUtils,
  CastleProjection, CastleVideos, CastleTextureImages, CastleLoadGltf,
  CastleWindow, CastleGLUtils, CastleMessages, CastleRenderPrimitives,
  CastleWindowRecentFiles, CastleGLImages, CastleInternalGLCubeMaps, CastleComponentSerialize,
  CastleControls, CastleGLShaders, CastleInternalControlsImages, CastleRenderContext,
  X3DFields, CastleInternalShapeOctree, X3DNodes, X3DLoad, CastleScene,
  CastleInternalBaseTriangleOctree, CastleFileFilters,
  X3DLoadInternalUtils, CastleSceneCore, X3DCameraUtils,
  CastleRenderOptions, CastleShapes, CastleViewport,
  CastleInternalRenderer,
  { castle-model-viewer-specific units: }
  V3DSceneTextureFilters, V3DSceneLights, V3DSceneRaytrace,
  V3DSceneNavigationTypes, V3DSceneSceneChanges, V3DSceneBGColors, V3DSceneViewpoints,
  V3DSceneWarnings, V3DSceneFillMode,
  V3DSceneAntiAliasing, V3DSceneScreenShot, V3DSceneCaptions,
  V3DSceneShadows, V3DSceneOctreeVisualize, V3DSceneMiscConfig, V3DSceneImages,
  V3DSceneScreenEffects, V3DSceneSkeletonVisualize, V3DSceneViewports, V3DSceneVersion,
  V3DSceneLightsEditor, V3DSceneWindow, V3DSceneStatus, V3DSceneNamedAnimations,
  V3DSceneBoxes, V3DSceneInternalScenes, V3DSceneDialogBox, V3DSceneFonts;

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
    This way we're preserving values of all RenderOptions.Xxx when opening new scene
    from "Open" menu item. }
  Scene: TCastleScene;
  SceneBoundingBox: TBoundingBoxScene;
  SceneDebugEdges: TDebugEdgesScene;
  SceneUrl: String;

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

  { ToolbarPanel is displayed if user wants to see the toolbar.
    Otherwise we display ToolbarPanelShorter --- which only shows the warnings button,
    see https://github.com/castle-engine/castle-model-viewer/issues/53 . }
  ToolbarPanel, ToolbarPanelShorter,
    ToolbarHorizGroup, ToolbarHorizGroupShorter: TCastleUserInterface;
  ButtonCollisions: TCastleButton;
  ButtonAnimations: TCastleButton;
  ButtonPatreon: TCastleButton;

  AnimationTimePlaying: boolean = true;
  MenuAnimationTimePlaying: TMenuItemChecked;

  ControlsOnScreenshot: boolean = false;
  HideExtraScenesForScreenshot: boolean = false;

  NavigationUi: TNavigationUi;

{ TEventsHandler -------------------------------------------------------------

  TODO: Change TEventsHandler to "TViewMain = class(TCastleView)",
  as is standard in CGE applications. }

type
  { Handle various events of the application. }
  TEventsHandler = class(TComponent)
  public
    procedure OpenRecent(const Url: String);
    procedure GeometryChanged(Scene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);
    procedure ViewpointsChanged(Sender: TObject);
    procedure BoundViewpointChanged(Sender: TObject);
    procedure BoundNavigationInfoChanged(Sender: TObject);
    procedure PointingDeviceSensorsChange(Sender: TObject);
    procedure HeadlightOnChanged(Sender: TObject);
    procedure ClickButtonWarnings(Sender: TObject);
    procedure ClickButtonPatreon(Sender: TObject);
    procedure ClickNavigationTypeButton(Sender: TObject);
    procedure ClickButtonOpen(Sender: TObject);
    procedure ClickButtonCollisions(Sender: TObject);
    procedure ClickButtonScreenshot(Sender: TObject);
    procedure ClickButtonAnimations(Sender: TObject);
    procedure OnWarningHandle(const Category, S: string);
    procedure Press(const Sender: TCastleUserInterface;
      const Event: TInputPressRelease; var Handled: boolean);
    procedure MenuClick(const MenuItem: TMenuItem);
  end;

var
  EventsHandler: TEventsHandler;

{ Custom viewport class ------------------------------------------------ }

type
  TV3DViewport = class(TV3DShadowsViewport)
  protected
    procedure RenderFromView3D(const Params: TRenderParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    procedure BeforeRender; override;
    procedure Render; override;
    function BaseLightsForRaytracer: TLightInstancesList;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
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

function TV3DViewport.BaseLightsForRaytracer: TLightInstancesList;
begin
  Result := TLightInstancesList.Create;
  InitializeGlobalLights(Result);
end;

procedure TV3DViewport.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Set Cursor = mcHand when we're over or keeping active
    some pointing-device sensors. }
  if (Scene <> nil) and
     ( ( (Scene.PointingDeviceSensors <> nil) and
         (Scene.PointingDeviceSensors.EnabledCount <> 0)
       ) or
       (Scene.PointingDeviceActiveSensors.Count <> 0)
     ) then
    Cursor := mcHand
  else
    Cursor := mcDefault;
end;

procedure TV3DViewport.Resize;
begin
  inherited;
  // Call ResizeViewports to change size of all viewports, when container size changed
  ResizeViewports(V3DSceneWindow.Window, MainViewport);
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

{ Update menu items and buttons that reflect Camera properties }
procedure UpdateCameraUI;
var
  WalkNav: TCastleWalkNavigation;
begin
  UpdateCameraNavigationTypeUI;

  WalkNav := WalkNavigation;

  if MenuMouseLook <> nil then
    MenuMouseLook.Checked := PersistentMouseLook;
  if MenuGravity <> nil then
    MenuGravity.Checked :=
      (WalkNav <> nil) and WalkNav.Gravity;
  if MenuPreferGravityUpForRotations <> nil then
    MenuPreferGravityUpForRotations.Checked :=
      (WalkNav <> nil) and WalkNav.PreferGravityUpForRotations;
  if MenuPreferGravityUpForMoving <> nil then
    MenuPreferGravityUpForMoving.Checked :=
      (WalkNav <> nil) and WalkNav.PreferGravityUpForMoving;
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
          Result := Result + (' [' + UriDisplay(Anchor.FdUrl.Items[0]));
          for J := 1 to Anchor.FdUrl.Count - 1 do
            Result := Result + (', ' + UriDisplay(Anchor.FdUrl.Items[J]));
          Result := Result + ']';
        end;
      end;

      Result := SForCaption(Result);
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
begin
  inherited;

  Statistics := MainViewport.Statistics;

  DescribeSensors;

  { S := Format('Collision detection: %s', [ BoolToStrOO[Scene.Collides] ]);
  if SceneOctreeCollisions = nil then
    S := S + ' (octree resources released)';
  Text.Append(S); }

  MainViewport.Camera.GetWorldView(Pos, Dir, Up);
  Text.Append(Format('Camera: pos <font color="#%s">%s</font>, dir <font color="#%s">%s</font>, up <font color="#%s">%s</font>',
    [ ValueColor, Pos.ToString,
      ValueColor, Dir.ToString,
      ValueColor, Up.ToString ]));

  if WalkNavigation <> nil then
  begin
    Text.Append(Format('Avatar height: <font color="#%s">%f</font> (last height above the ground: <font color="#%s">%s</font>)',
      [ ValueColor, WalkNavigation.PreferredHeight,
        ValueColor, CurrentAboveHeight(WalkNavigation) ]));
  end;

  { if SceneLightsCount = 0 then
   s := '(useless, scene has no lights)' else
   s := BoolToStrOO[Scene.RenderOptions.UseSceneLights];
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

{ TCastleWindow callbacks --------------------------------------------------------- }

{ Update SceneBoundingBox look. }
procedure SceneBoundingBoxUpdate(const RenderingCamera: TRenderingCamera);
begin
  SceneBoundingBox.Exists :=
    (RenderingCamera.Target = rtScreen) and
    (not HideExtraScenesForScreenshot) and
    ShowBBox;
  if SceneBoundingBox.Exists then
  begin
    { Use Scene.RenderOptions.LineWidth for our visualizations as well }
    SceneBoundingBox.RenderOptions.LineWidth := Scene.RenderOptions.LineWidth;
    SceneBoundingBox.UpdateBox(Scene.BoundingBox);
  end;
end;

var
  HasWalkFrustum: Boolean; //< Secures from case when user never used Walk navigation, and so WalkFrustum is undefined
  WalkFrustum: TFrustum;

{ Render visualization of various stuff, like octree and such. }
procedure RenderVisualizations(const RenderingCamera: TRenderingCamera);

  procedure RenderFrustum(const AlwaysVisible: boolean);
  var
    FrustumPoints: TFrustumPoints;
    SavedDepthTest: Boolean;
    Mesh: TCastleRenderUnlitMesh;
  begin
    if not HasWalkFrustum then
      Exit;

    if AlwaysVisible then
    begin
      SavedDepthTest := RenderContext.DepthTest;
      RenderContext.DepthTest := false;
    end;
    try
      WalkFrustum.CalculatePoints(FrustumPoints);

      Mesh := TCastleRenderUnlitMesh.Create(true);
      try
        Mesh.ModelViewProjection := RenderContext.ProjectionMatrix * RenderingCamera.CurrentMatrix;
        Mesh.SetVertexes(@FrustumPoints, High(FrustumPoints) + 1, false);
        Mesh.SetIndexes(@FrustumPointsLinesIndexes, (High(FrustumPointsLinesIndexes) + 1) * 2);
        Mesh.Render(pmLines);
      finally FreeAndNil(Mesh) end;
    finally
      if AlwaysVisible then
        RenderContext.DepthTest := SavedDepthTest;
    end;
  end;

  procedure RenderSelected(const Box: TBox3D; const Triangle: TTriangle3; const Point: TVector3);
  var
    SavedDepthTest, SavedCullFace: Boolean;
    ModelViewProjection: TMatrix4;
    Mesh: TCastleRenderUnlitMesh;
    SavedLineWidth, SavedPointSize: Single;
  begin
    SavedDepthTest := RenderContext.DepthTest;
    SavedCullFace := RenderContext.CullFace;
    SavedLineWidth := RenderContext.LineWidth;
    SavedPointSize := RenderContext.PointSize;

    RenderContext.DepthTest := false; // draw stuff visible through other geometry here
    RenderContext.CullFace := true; // note: it doesn't matter for lines, but matters for triangles below
    RenderContext.LineWidth := 2.0;
    RenderContext.PointSize := 5.0;

    ModelViewProjection := RenderContext.ProjectionMatrix * RenderingCamera.CurrentMatrix;

    // draw red selection corner markers
    glDrawCornerMarkers(Box, Vector4(0.5, 0.3, 0.3, 1), ModelViewProjection);

    Mesh := TCastleRenderUnlitMesh.Create(true);
    try
      Mesh.ModelViewProjection := ModelViewProjection;

      RenderContext.BlendingEnable(bsSrcAlpha, bdOneMinusSrcAlpha);

      Mesh.Color := Vector4(0.5, 0.3, 0.3, 0.5); // draw face back in red
      Mesh.SetVertexes([
        Vector4(Triangle.Data[0], 1),
        Vector4(Triangle.Data[2], 1),
        Vector4(Triangle.Data[1], 1)
      ], false);
      Mesh.Render(pmTriangles);

      Mesh.Color := Vector4(0.4, 0.4, 1, 0.4); // draw face front in blue
      Mesh.SetVertexes([
        Vector4(Triangle.Data[0], 1),
        Vector4(Triangle.Data[1], 1),
        Vector4(Triangle.Data[2], 1)
      ], false);
      Mesh.Render(pmTriangles);

      RenderContext.BlendingDisable;

      // draw face outline in white
      Mesh.Color := White;
      Mesh.Render(pmLineLoop); // note: we use vertexes set previously

      // draw hit point
      Mesh.SetVertexes([Vector4(Point, 1)], false);
      Mesh.Render(pmPoints);
    finally FreeAndNil(Mesh) end;

    { Draw blue corner markers, these overwrite the red markers, but only when in front.
      So corners in front are blue, those behind are red.
      Gives depth impression and is generally visible against various geometry. }
    glDrawCornerMarkers(Box, Vector4(0.2, 0.2, 1, 1), ModelViewProjection);

    RenderContext.DepthTest := SavedDepthTest;
    RenderContext.CullFace := SavedCullFace;
    RenderContext.LineWidth := SavedLineWidth;
    RenderContext.PointSize := SavedPointSize;
  end;

begin
  { Save WalkFrustum for future RenderFrustum rendering. }
  if (RenderingCamera.Target = rtScreen) and (WalkNavigation <> nil) then
  begin
    HasWalkFrustum := true;
    WalkFrustum := MainViewport.Camera.Frustum;
  end;

  if (RenderingCamera.Target = rtScreen) and (not HideExtraScenesForScreenshot) then
  begin
    { Visualization below depends on depth test enabled }
    RenderContext.DepthTest := true;

    { Use Scene.RenderOptions.LineWidth for our visualizations as well }
    RenderContext.LineWidth := Scene.RenderOptions.LineWidth;

    OctreeDisplay(Scene, RenderContext.ProjectionMatrix * RenderingCamera.CurrentMatrix);

    { Note that there is no sense in showing WalkFrustum if WalkNavigation <> nil
      since then the WalkFrustum matches currently used frustum. }
    if ShowFrustum and (WalkNavigation = nil) then
      RenderFrustum(ShowFrustumAlwaysVisible);

    if SelectedItem <> nil then
      { Note that this assumes that Scene transformation is identity.
        We only transform selected stuff into scene coordinate-system. }
      RenderSelected(
        TShape(SelectedItem^.Shape).BoundingBox,
        SelectedItem^.SceneSpace.Triangle,
        SelectedPointWorld
      );
  end;
end;

procedure TV3DViewport.RenderFromView3D(const Params: TRenderParams);
begin
  SceneBoundingBoxUpdate(Params.RenderingCamera);

  Scene.Visible := FillMode <> fmSilhouetteBorderEdges;
  SceneDebugEdges.Exists := FillMode = fmSilhouetteBorderEdges;
  if SceneDebugEdges.Exists then
    SceneDebugEdges.UpdateEdges(Scene);
  inherited;

  { Draw visualizations after viewport contents, this is important for
    "selected" visualization that doesn't use depth test and assumes it is
    just drawn on top of everything. }
  RenderVisualizations(Params.RenderingCamera);
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

procedure SetViewpointForWholeScene(
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean);
var
  Position, Direction, Up, GravityUp: TVector3;
begin
  CameraViewpointForWholeScene(Scene.BoundingBox, WantedDirection, WantedUp,
    WantedDirectionPositive, WantedUpPositive,
    Position, Direction, Up, GravityUp);
  MainViewport.Camera.AnimateTo(Position, Direction, Up, CameraTransitionTime);
  MainViewport.Camera.GravityUp := GravityUp;
end;

procedure SetViewpointTop;
begin
  SetViewpointForWholeScene(1, 2, false, false);
end;

procedure SetViewpointBottom;
begin
  SetViewpointForWholeScene(1, 2, true , false);
end;

procedure SetViewpointFront;
begin
  SetViewpointForWholeScene(2, 1, false, true);
end;

procedure SetViewpointBack;
begin
  SetViewpointForWholeScene(2, 1, true , true);
end;

procedure SetViewpointRight;
begin
  SetViewpointForWholeScene(0, 1, false, true);
end;

procedure SetViewpointLeft;
begin
  SetViewpointForWholeScene(0, 1, true , true);
end;

procedure TEventsHandler.Press(const Sender: TCastleUserInterface;
  const Event: TInputPressRelease; var Handled: boolean);
begin
  { Although some of these shortcuts are also assigned to menu items,
    catching them here is more reliable -- allows to handle also Ctrl+number
    combinations, and capture both numpad and non-numpad versions. }
  if Event.IsKey(key7) or Event.IsKey(keyNumPad7) then
  begin
    if Event.ModifiersDown = [] then
      SetViewpointTop
    else
    if Event.ModifiersDown = [mkCtrl] then
      SetViewpointBottom;
  end;
  if Event.IsKey(key1) or Event.IsKey(keyNumPad1) then
  begin
    if Event.ModifiersDown = [] then
      SetViewpointFront
    else
    if Event.ModifiersDown = [mkCtrl] then
      SetViewpointBack;
  end;
  if Event.IsKey(key3) or Event.IsKey(keyNumPad3) then
  begin
    if Event.ModifiersDown = [] then
      SetViewpointRight
    else
    if Event.ModifiersDown = [mkCtrl] then
      SetViewpointLeft;
  end;

  { Support selecting item by ctrl + right button click. }
  if Event.IsMouseButton(buttonRight) and
     (mkCtrl in Window.Container.Pressed.Modifiers) then
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

procedure TEventsHandler.PointingDeviceSensorsChange(Sender: TObject);
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

procedure TEventsHandler.ViewpointsChanged(Sender: TObject);
begin
  Viewpoints.Recalculate(Scene);
end;

procedure TEventsHandler.BoundViewpointChanged(Sender: TObject);
var
  V: TAbstractViewpointNode;
begin
  V := Scene.ViewpointStack.Top;
  Viewpoints.BoundViewpoint := Viewpoints.ItemOf(V);
end;

procedure TEventsHandler.BoundNavigationInfoChanged(Sender: TObject);
begin
  UpdateCameraUI;
end;

procedure TEventsHandler.HeadlightOnChanged(Sender: TObject);
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

procedure TEventsHandler.OnWarningHandle(const Category, S: string);
begin
  { It is possible that SceneWarnings = nil now,
    in case on macOS we use
    ".../castle-model-viewer .../dynamic_world.x3dv --screenshot 0 output_2d_screenshot.png"
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
begin
  { Do not create octrees when Scene.Collides = false. This makes
    setting Scene.Collides to false an optimization: octree doesn't have to
    be recomputed when animation frame changes, or new scene is loaded etc. }

  if Scene.Collides then
    Scene.PreciseCollisions := true;
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

    Scene.PreciseCollisions := false;
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

  SceneUrl := '';

  if MenuReopen <> nil then
    MenuReopen.Enabled := false;

  Unselect;
end;

procedure LoadClearScene; forward;

type
  TLoadSceneOption = (lsoCommandLineCustomization);
  TLoadSceneOptions = set of TLoadSceneOption;

{ Set proper Camera.ProjectionNear and Navigation.Radius based on predicted
  bounding box we need to display.

  Navigation may be @nil.

  CGE since commit 050dc126a4f0ac0a0211d929f1e1f8d7f96a88f9 no longer does it
  automatically based on box. }
procedure UpdateRadiusProjectionNear(const Camera: TCastleCamera;
  const Navigation: TCastleNavigation;
  const Box: TBox3D);
const
  WorldBoxSizeToRadius = 0.005;
var
  Radius: Single;
begin
  Radius := Box.AverageSize(false, 1.0) * WorldBoxSizeToRadius;
  { Make Radius at most DefaultCameraRadius?
    Commented out, not necessary and could be troublesome -- we want the autocalculate
    it to be have good near (good near clipping plane) and good depth precision. }
  // MaxVar(Radius, DefaultCameraRadius);
  if Navigation <> nil then
    Navigation.Radius := Radius;
  Camera.ProjectionNear := Radius * RadiusToProjectionNear;
  WritelnLog('Auto-calculated Radius %.8f, ProjectionNear %.8f', [
    Radius,
    Camera.ProjectionNear
  ]);
end;

{ Calls FreeScene and then inits "scene global variables".

  Camera settings for scene are inited from VRML/X3D defaults and
  from camera node in scene.

  Exceptions: if this function will raise any exception you should assume
  that scene loading failed for some reason and "scene global variables"
  are set to their "null values". I.e. everything is in a "clean" state
  like after FreeScene.

  This procedure does not really open any file
  (so ASceneUrl need not be a name of existing file/URL).
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

  ASceneUrl is not const parameter, to allow you to pass
  SceneUrl as ASceneUrl. If ASceneUrl would be const
  we would have problem, because FreeScene modifies SceneUrl
  global variable, and this would change ASceneUrl value
  (actually, making it possibly totally invalid pointer,
  pointing at some other place of memory). That's always the
  problem with passing pointers to global variables
  (ASceneUrl is a pointer) as local vars.

  If lsoCommandLineCustomization in Options,
  then we will use (and clear) the command-line customizations
  of viewpoint. This should be @false if you're only loading
  temporary scene, like LoadClearScene. }
procedure LoadSceneCore(
  RootNode: TX3DRootNode;
  ASceneUrl: String;
  const Options: TLoadSceneOptions);
var
  NewCaption: string;
  I: Integer;
begin
  FreeScene;

  try
    SceneUrl := ASceneUrl;

    { set InitialViewpoint* before Scene.Load }
    SetInitialViewpoint(Scene, lsoCommandLineCustomization in Options);

    Scene.Load(RootNode, true);

    { calculate Viewpoints, including MenuJumpToViewpoint. }
    Viewpoints.Recalculate(Scene);

    MainViewport.AssignDefaultCamera;
    MainViewport.AssignDefaultNavigation;
    // UpdateCameraUI, called below, will make UI reflect current navigation

    { Set proper Camera.ProjectionNear and Navigation.Radius,
      before AssignCameraAndNavigation for other viewports (as they will copy us).
      CGE since commit 050dc126a4f0ac0a0211d929f1e1f8d7f96a88f9 no longer does it
      automatically based on box. }
    UpdateRadiusProjectionNear(MainViewport.Camera, Navigation, Scene.LocalBoundingBox);

    for I := 0 to High(ExtraViewports) do
      AssignCameraAndNavigation(ExtraViewports[I], MainViewport);
    Viewpoints.BoundViewpoint := Viewpoints.ItemOf(ViewpointNode);

    SceneInitLights(Scene, NavigationNode);

    { update MenuHeadlight.Checked now, and make it always updated. }
    EventsHandler.HeadlightOnChanged(Scene);
    Scene.OnHeadlightOnChanged :=
      {$ifdef FPC}@{$endif} EventsHandler.HeadlightOnChanged;

    NewCaption := Scene.Caption;
    if NewCaption = '' then
      NewCaption := UriCaption(SceneUrl);
    NewCaption := SForCaption(NewCaption) + ' - castle-model-viewer';
    Window.Caption := NewCaption;

    UpdateCameraUI;

    SceneOctreeCreate;

    Scene.OnGeometryChanged :=
      {$ifdef FPC}@{$endif} EventsHandler.GeometryChanged;
    Scene.OnViewpointsChanged :=
      {$ifdef FPC}@{$endif} EventsHandler.ViewpointsChanged;
    Scene.OnPointingDeviceSensorsChange :=
      {$ifdef FPC}@{$endif} EventsHandler.PointingDeviceSensorsChange;
    Scene.ProcessEvents := ProcessEventsWanted;

    RefreshNamedAnimationsUi(Window, Scene, ToolbarPanel.EffectiveHeight);

    if not Window.Closed then
      Window.Invalidate;

    if MenuReopen <> nil then
      MenuReopen.Enabled := SceneUrl <> '';

    { Set blending sort following "NavigationInfo.blendingSort" info from scene.
      This means we use 2D sorting e.g. for Spine models by default. }
    if (Scene.NavigationInfoStack.Top <> nil) and
       (Scene.NavigationInfoStack.Top.BlendingSort <> sortAuto) then
      MainViewport.BlendingSort := Scene.NavigationInfoStack.Top.BlendingSort
    else
      MainViewport.BlendingSort := sortAuto;
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

  It may seem that ASceneUrl could be constant parameter here.
  Yes, it could. However, you will sometimes want to pass here
  SceneUrl global value and this would cause memory havoc
  (parameter is passed as const, however when global variable
  SceneUrl is changed then the parameter value implicitly
  changes, it may even cause suddenly invalid pointer --- yeah,
  I experienced it). }
procedure LoadScene(ASceneUrl: String);
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
      RootNode := LoadNode(ASceneUrl);
    {$ifdef CATCH_EXCEPTIONS}
    except
      on E: Exception do
      begin
        LoadErrorMessage('Error while loading scene from "' +ASceneUrl+ '": ' +
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
    LoadSceneCore(RootNode, ASceneUrl, [lsoCommandLineCustomization]);
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
      LoadErrorMessage('Error while loading scene from "' + ASceneUrl + '": ' +
        E.Message);
      Exit;
    end;
  end;
  {$endif CATCH_EXCEPTIONS}

  TimeLoadScene := ProcessTimer;

  { For batch operation (making screenshots), do not save the scene
    on "recent files" menu. This also applies when using castle-model-viewer
    as a thumbnailer. }
  if not MakingScreenShot then
    RecentMenu.Add(ASceneUrl);

  { We call MainViewport.PrepareResources to make Scene.PrepareResources to gather
    warnings (because some warnings, e.g. invalid texture URL,
    are reported only from Scene.PrepareResources). }
  MainViewport.PrepareResources;
  ButtonWarningsEnabled := true;
  UpdateButtonWarnings;

  TimePrepareResources := ProcessTimer;

  WritelnLogMultiline('Loading', FormatDot(
    'Loaded "%s" with dependencies in %f seconds:' + NL +
    '  %f to load from disk (create X3D graph)' + NL +
    '  %f to initialize scene (initialize shapes tree, collisions...)' + NL +
    '  %f to prepare resources (load textures, prepare OpenGL resources...)',
    [ UriDisplay(ASceneUrl),
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

  LoadSceneCore(Node, '', Options);
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
    This way clear scene has an effect on castle-model-viewer window's title,
    and at the same time I don't have to set SceneUrl to something dummy.

    I'm not constructing here RootNode in code (i.e. Pascal).
    This would allow a fast implementation, but it's easier for me to
    design scene in pure VRML/X3D and then auto-generate
    xxx_scene.inc file to load scene from a simple string. }
  LoadSimpleScene(LoadX3DClassicFromString({$I embedded_data/scenes/clear_scene.inc}, ''), []);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of castle-model-viewer. }
procedure LoadWelcomeScene;
begin
  LoadSimpleScene(LoadX3DClassicFromString({$I embedded_data/scenes/welcome_scene.inc}, ''), []);
end;

const
  SaveGenerator = 'castle-model-viewer, https://castle-engine.io/castle-model-viewer';

{ Load model from ASceneUrl,
  do SceneChanges, and write it as VRML/X3D to stdout.
  This is used to handle --write command-line option. }
procedure WriteModel(const ASceneUrl: String;
  const Encoding: TX3DEncoding);
var
  Node: TX3DRootNode;
  OutputMime: String;
begin
  Node := LoadNode(ASceneUrl);
  try
    if StdOutStream = nil then
      raise EInvalidParams.Create('Standard output is not available. This most likely means you used --write option on Windows and you didn''t redirect the output.' + NL + NL + 'The proper usage from the command-line looks like "castle-model-viewer input.gltf --write > output.x3dv", see https://castle-engine.io/castle-model-viewer#section_converting .');
    case Encoding of
      xeXML    : OutputMime := 'model/x3d+xml';
      xeClassic: OutputMime := 'model/x3d+vrml';
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Saving Encoding?');
      {$endif}
    end;
    SaveNode(Node, StdOutStream, OutputMime, SaveGenerator, ExtractURIName(ASceneUrl));
  finally FreeAndNil(Node) end;
end;

procedure TEventsHandler.OpenRecent(const Url: String);
begin
  LoadScene(Url);
end;

procedure DropFiles(Container: TCastleContainer; const FileNames: array of string);
var
  Url: String;
begin
  if High(FileNames) >= 0 then
  begin
    Url := FilenameToUriSafe(FileNames[0]);
    if Url <> '' then
      LoadScene(Url);
  end;
end;

procedure TEventsHandler.GeometryChanged(Scene: TCastleSceneCore;
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
    SelectedItem^.UpdateSceneSpace;
    // update to SelectedItem^.UpdateSceneSpace for new

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

procedure TEventsHandler.ClickButtonWarnings(Sender: TObject);
begin
  MessageReport(
    Format('%d warnings:', [SceneWarnings.Count]) + NL +
    NL +
    TrimRight(SceneWarnings.Items.Text) + NL +
    NL +
    'Scene URL: "' + UriDisplay(SceneUrl) + '".' + NL +
    'Use "File->View Warnings" menu to view these warnings again.'
  );
  ButtonWarningsEnabled := false;
  UpdateButtonWarnings;
end;

procedure TEventsHandler.ClickButtonPatreon(Sender: TObject);
begin
  if not OpenUrl(SupportUrl) then
    Window.MessageOk(SCannotOpenUrl, mtError);
end;

procedure AttributesLoadFromConfig(RenderOptions: TCastleRenderOptions);
begin
  RenderOptions.LineWidth := UserConfig.GetFloat('video_options/line_width',
    RenderOptions.DefaultLineWidth);
  RenderOptions.PointSize := UserConfig.GetFloat('video_options/point_size',
    RenderOptions.DefaultPointSize);
end;

procedure AttributesSaveToConfig(RenderOptions: TCastleRenderOptions);
begin
  UserConfig.SetDeleteFloat('video_options/line_width',
    RenderOptions.LineWidth, RenderOptions.DefaultLineWidth);
  UserConfig.SetDeleteFloat('video_options/point_size',
    RenderOptions.PointSize, RenderOptions.DefaultPointSize);
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
    OldTime: TFloatTime;
    Image: TCastleImage;
  begin
    { Save global things that we change, to restore them later.
      This isn't needed for batch mode screenshots, but it doesn't hurt
      to be clean. }
    OldTime := Scene.Time;
    try
      MainViewport.BeforeRender;

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
              SaveImage(Image, ScreenShotsList[I].UseUrl(J));
            finally FreeAndNil(Image) end;
          end;
          ScreenShotsList[I].EndCapture(true);
        except
          ScreenShotsList[I].EndCapture(false);
          raise;
        end;
      end;

    finally
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
      ViewportsSetTransparent(true);
      if GLFeatures.AlphaBits = 0 then
        { In case FBO is not available, and main context doesn't have alpha bits either.
          TODO: Above only queries for alpha bits in default color buffer, not in FBO. }
        WritelnWarning('OpenGL', 'We did not manage to create a render buffer with alpha channel. This means that screenshot will not capture the transparency. You need a better GPU for this to work.');
    end;

    try
      MakeAllScreenShots(ImageClass);
    finally
      ScreenshotRender.RenderEnd;
      if Transparency then
        ViewportsSetTransparent(false);
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

procedure UpdateStatusToolbarVisible; forward;

procedure ScreenShotImage(const Caption: string; const Transparency: boolean);

  function CaptureScreen: TCastleImage;
  var
    Fbo: TGLRenderToTexture;
    ImageClass: TCastleImageClass;
  begin
    Assert(Window.DoubleBuffer); { castle-model-viewer always has double buffer }

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
      ViewportsSetTransparent(true);

      if GLFeatures.AlphaBits = 0 then
        { In case FBO is not available, and main context doesn't have alpha bits either.
          TODO: Above only queries for alpha bits in default color buffer, not in FBO. }
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
        ViewportsSetTransparent(false);
      end;
    end;
  end;

var
  ScreenShotNameUrlPrefix, ScreenShotName: string;
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
    if SceneUrl <> '' then
      { Without protocol, ScreenShotNameUrlPrefix is treated like relative
        filename now. So make sure to convert %20 to spaces,
        to later save screenshot with spaces when scene file had spaces. }
      ScreenShotNameUrlPrefix := InternalUriUnescape(DeleteURIExt(ExtractURIName(SceneUrl)))
    else
      ScreenShotNameUrlPrefix := 'castle-model-viewer_screenshot';
    { We use FileNameAutoInc with 2 params,
      UrlPrefix, UrlSuffixWithPattern,
      since URL (and/or filename) may also contain % sign.
      See https://github.com/castle-engine/castle-model-viewer/issues/61 . }
    ScreenShotName := FileNameAutoInc(ScreenShotNameUrlPrefix, '_%d.png');
    { Below is a little expanded version of TCastleWindow.SaveScreenDialog.
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

procedure TEventsHandler.MenuClick(const MenuItem: TMenuItem);

  procedure MakeGravityUp(const NewUp: TVector3);
  var
    Pos, Dir, Up, GravityUp: TVector3;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);
    GravityUp := MainViewport.Camera.GravityUp;

    if VectorsParallel(Dir, NewUp) then
      Dir := AnyOrthogonalVector(NewUp);
    Up := NewUp;
    GravityUp := NewUp;

    MainViewport.Camera.SetWorldView(Pos, Dir, Up, false);
    MainViewport.Camera.GravityUp := GravityUp;
  end;

  procedure ChangeMoveSpeed;
  var
    MoveSpeed: Single;
  begin
    { TODO: before merging new-cameras branch, this is not entirely correct,
      when switching navigations the examine navigation MoveSpeed may override
      InternalWalkNavigation.MoveSpeed. }
    MoveSpeed := MainViewport.InternalWalkNavigation.MoveSpeed;
    if MessageInputQuery(Window, 'New move speed (units per second) when walking or flying:', MoveSpeed) then
    begin
      MainViewport.InternalWalkNavigation.MoveSpeed := MoveSpeed;
      Window.Invalidate;
    end;
  end;

  procedure ChangePointSize;
  var
    Value: Single;
  begin
    Value := Scene.RenderOptions.PointSize;
    if MessageInputQuery(Window, 'Change point size:', Value) then
      Scene.RenderOptions.PointSize := Max(Value, 0.01);
  end;

  procedure ChangeLineWidth;
  var
    Value: Single;
  begin
    Value := Scene.RenderOptions.LineWidth;
    if MessageInputQuery(Window, 'Change line width:', Value) then
      Scene.RenderOptions.LineWidth := Max(Value, 0.01);
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
      'This has no effect on non-baked animations, like played from X3D or Spine JSON.' + NL +
      NL +
      'New baked animation smoothness:', S) then
      BakedAnimationSmoothness := S;
  end;

  procedure SelectedShowInformation;

    { Node nice name, or '' if Node = @nil. }
    function OptionalNodeNiceName(const Node: TX3DNode): String;
    begin
      if Node <> nil then
        Result := Node.NiceName
      else
        Result := '';
    end;

  var
    S, TextureDescription: string;
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
      S := Format(
             'Selected point %s from triangle'+ NL +
             '%s.' + NL +
             NL +
             'This triangle is part of the geometry node named "%s" (original geometry type %s, rendered through type %s). Parent node: "%s", grand-parent node: "%s", grand-grand-parent node: "%s".' + NL +
             NL +
             'Node''s bounding box is %s. ',
           [
             SelectedPointWorld.ToString,
             SelectedItem^.SceneSpace.Triangle.ToString,
             SelectedGeometry.X3DName,
             SelectedShape.OriginalGeometry.X3DType,
             SelectedGeometry.X3DType,
             OptionalNodeNiceName(SelectedShape.GeometryParentNode),
             OptionalNodeNiceName(SelectedShape.GeometryGrandParentNode),
             OptionalNodeNiceName(SelectedShape.GeometryGrandGrandParentNode),
             SelectedShape.BoundingBox.ToString
           ]);

      if (SelectedItem^.Face.IndexBegin <> -1) and
         (SelectedItem^.Face.IndexEnd <> -1) then
      begin
        S := S + Format('Face containing the selected triangle spans from %d to' +
          ' %d coordIndex entries. ',
          [ SelectedItem^.Face.IndexBegin,
            SelectedItem^.Face.IndexEnd ]);
      end;

      s := s + Format('Node has %d vertices and %d triangles.', [
        SelectedShape.VerticesCount,
        SelectedShape.TrianglesCount
      ]);

      { calculate Tex }
      Tex := SelectedItem^.State.MainTexture;

      { calculate TextureDescription }
      if Tex = nil then
        TextureDescription := 'none' else
        TextureDescription := Tex.TextureDescription;

      S := S + Format(nl + NL + 'Node''s texture : %s.', [TextureDescription]);

      S := S + NL + NL;
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
           [ I, Lights.L[i].Node.NiceName ]);

         ShadowingItem := SceneOctreeCollisions.SegmentCollision(
           SelectedPointWorld, Lights.L[i].Location,
             false, SelectedItem, true, nil);

         if ShadowingItem <> nil then
         begin
          s := s + Format('no, this light is blocked by triangle %s from shape %s.',
            [ ShadowingItem^.SceneSpace.Triangle.ToString,
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

    function MFNonEmpty(Field: TInt32List): boolean;
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
    Colors, Coords, Materials, Normals, TexCoords: TInt32List;
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

  procedure ChangeMaterialEmissiveColor;
  var
    MatInfo: TMaterialInfo;
    Color: TVector3;
  begin
    if SelectedItem = nil then
    begin
      Window.MessageOK('Nothing selected.', mtError);
      Exit;
    end;

    MatInfo := SelectedItem^.State.MaterialInfo;
    if MatInfo = nil then
    begin
      Window.MessageOK('No material assigned to this shape. Create a new material using one of "Reset To Default ... Material" menu items.', mtError);
      Exit;
    end;

    Color := MatInfo.EmissiveColor;
    if Window.ColorDialog(Color) then
      MatInfo.EmissiveColor := Color;
  end;

  procedure ChangeMaterialMainColor;
  var
    MatInfo: TMaterialInfo;
    Color: TVector3;
  begin
    if SelectedItem = nil then
    begin
      Window.MessageOK('Nothing selected.', mtError);
      Exit;
    end;

    MatInfo := SelectedItem^.State.MaterialInfo;
    if MatInfo = nil then
    begin
      Window.MessageOK('No material assigned to this shape. Create a new material using one of "Reset To Default ... Material" menu items.', mtError);
      Exit;
    end;

    Color := MatInfo.MainColor;
    if Window.ColorDialog(Color) then
      MatInfo.MainColor := Color;
  end;

  procedure ResetMaterial(const NewMaterialNode: TAbstractMaterialNode);
  var
    Shape: TAbstractShapeNode;
  begin
    if SelectedItem = nil then
    begin
      Window.MessageOK('Nothing selected.', mtError);
      Exit;
    end;

    Shape := SelectedItem^.State.ShapeNode;
    if Shape = nil then // this can happen if you selected VRML 1 / Inventor node
    begin
      Window.MessageOK('No Shape node selected.', mtError);
      Exit;
    end;

    if Shape.Appearance = nil then
      Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Material := NewMaterialNode;
  end;

  procedure ChangeLightModelAmbient;
  var
    C: TVector3;
  begin
    C := RenderContext.GlobalAmbient;
    if Window.ColorDialog(C) then
      RenderContext.GlobalAmbient := C;
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
    in castle-model-viewer), in actual game you want to remove them more intelligently
    (actually adding creatures, items, etc. at designated places). }
  procedure RemoveGamePlaceholders;
  var
    R: TGamePlaceholdersRemover;
  begin
    R := TGamePlaceholdersRemover.Create;
    try
      Scene.BeforeNodesFree;
      Scene.RootNode.EnumerateReplaceChildren(
        {$ifdef FPC}@{$endif} R.Remove);
      Scene.ChangedAll;
      MessageOK(Window, Format('Removed %d nodes.', [R.Count]));
    finally FreeAndNil(R) end;
  end;

  procedure PrintRayhunterCommand;
  var
    S: string;
    Pos, Dir, Up: TVector3;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);

    S := FormatDot(
       'Call rayhunter like this to render this view :' + NL +
       '  rayhunter classic %d %d %d "%s" "%s" \' + NL +
       '    --camera-pos %s \' + NL +
       '    --camera-dir %s \' + NL +
       '    --camera-up %s \' + NL +
       '    --scene-bg-color %f %f %f \' +nl,
       [ DefaultRaytracerDepth,
         Window.Width, Window.Height,
         SceneUrl,
         ChangeURIExt(ExtractURIName(SceneUrl), '-rt.png'),
         Pos.ToRawString,
         Dir.ToRawString,
         Up.ToRawString,
         BGColor[0], BGColor[1], BGColor[2] ]);

    {$warnings off} // for now, this knowingly uses deprecated MainViewport.Projection
    case MainViewport.Projection.ProjectionType of
      ptPerspective:
        S := S + FormatDot('    --view-angle-x %f',
          [RadToDeg(MainViewport.Projection.PerspectiveAnglesRad[0])]);
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
      Result := FormatDot('Vector3(%f, %f, %f)', [V[0], V[1], V[2]]);
    end;

  var
    Pos, Dir, Up, GravityUp: TVector3;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);
    GravityUp := MainViewport.Camera.GravityUp;
    MessageReport(Format('// Set camera vectors using Castle Game Engine.' + NL +
      'Viewport.Camera.SetWorldView(' + NL +
      '  %s, // position' + NL +
      '  %s, // direction' + NL +
      '  %s  // up (current)' + NL +
      ');' + NL +
      'Viewport.Camera.GravityUp := %s;',
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
    Make: TMakeX3DViewpoint;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);
    GravityUp := MainViewport.Camera.GravityUp;

    Make := TMakeX3DViewpoint.Create;
    try
      Make.Version := Version;
      Make.Xml := Xml;
      Make.Position := Pos;
      Make.Direction := Dir;
      Make.Up := Up;
      Make.GravityUp := GravityUp;
      if Navigation is TCastleExamineNavigation then
      begin
        Make.AutoCenterOfRotation := TCastleExamineNavigation(Navigation).AutoCenterOfRotation;
        if not TCastleExamineNavigation(Navigation).AutoCenterOfRotation then
          Make.CenterOfRotation := TCastleExamineNavigation(Navigation).CenterOfRotation;
      end;
      {$warnings off} // for now, this knowingly uses deprecated MainViewport.Projection
      case MainViewport.Projection.ProjectionType of
        ptPerspective:
          begin
            Make.PerspectiveFieldOfView := MainViewport.Projection.PerspectiveAnglesRad.Y;
            Make.ProjectionType := ptPerspective;
          end;
        ptOrthographic:
          begin
            // TODO: TMakeX3DViewpoint actually doesn't support orthographic projection yet.
            Make.OrthographicFieldOfView := MainViewport.Projection.Dimensions;
            Make.ProjectionType := ptOrthographic;
          end;
      end;
      {$warnings on}
      MessageReport(Make.ToString);
    finally FreeAndNil(Make) end;
  end;

  procedure WriteBoundingBox(const Box: TBox3D);
  var
    S1, S2, S3, S4: string;
  begin
    if Box.IsEmpty then
      MessageOK(Window, 'The bounding box is empty.') else
    begin
      { Workarounding FPC 3.1.1 internal error 200211262 }
      (*
      MessageReport(Format(
        '# ----------------------------------------' + NL +
        '# BoundingBox %s:' + NL +
        '# Version for VRML 1.0' + NL +
        'DEF BoundingBox Separator {' + NL +
        '  Translation {' + NL +
        '    translation %s' + NL +
        '  }' + NL +
        '  Cube {' + NL +
        '    width %s' + NL +
        '    height %s' + NL +
        '    depth %s' + NL +
        '  } }' + NL +
        NL +
        '# Version for VRML 2.0 / X3D' + NL +
        'DEF BoundingBox Transform {' + NL +
        '  translation %1:s' + NL +
        '  children Shape {' + NL +
        '    geometry Box {' + NL +
        '      size %2:s %3:s %4:s' + NL +
        '    } } }',
        [ Box.ToNiceStr,
          Box.Middle.ToRawString,
          FloatToRawStr(Box.Data[1][0] - Box.Data[0][0]),
          FloatToRawStr(Box.Data[1][1] - Box.Data[0][1]),
          FloatToRawStr(Box.Data[1][2] - Box.Data[0][2]) ]));
      *)

      S1 := Box.Center.ToRawString;
      S2 := FormatDot('%g', [Box.Data[1][0] - Box.Data[0][0]]);
      S3 := FormatDot('%g', [Box.Data[1][1] - Box.Data[0][1]]);
      S4 := FormatDot('%g', [Box.Data[1][2] - Box.Data[0][2]]);
      MessageReport(Format(
        '# ----------------------------------------' + NL +
        '# BoundingBox %s:' + NL +
        '# Version for VRML 1.0' + NL +
        'DEF BoundingBox Separator {' + NL +
        '  Translation {' + NL +
        '    translation %s' + NL +
        '  }' + NL +
        '  Cube {' + NL +
        '    width %s' + NL +
        '    height %s' + NL +
        '    depth %s' + NL +
        '  } }' + NL +
        NL +
        '# Version for VRML 2.0 / X3D' + NL +
        'DEF BoundingBox Transform {' + NL +
        '  translation %1:s' + NL +
        '  children Shape {' + NL +
        '    geometry Box {' + NL +
        '      size %2:s %3:s %4:s' + NL +
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
      Scene.RenderOptions.WireframeEffect := FillModes[FillMode].WireframeEffect;
      Scene.RenderOptions.WireframeColor  := FillModes[FillMode].WireframeColor;
      Scene.RenderOptions.Mode            := FillModes[FillMode].Mode;
    end;
  end;

  procedure ScreenShotToVideo(const Transparency: boolean);
  var
    TimeBegin, TimeStep: TFloatTime;
    FramesCount: Cardinal;
    UrlPattern: string;
    Range: TRangeScreenShot;
  begin
    TimeBegin := Scene.Time;
    TimeStep := 0.04;
    FramesCount := 25;
    UrlPattern := 'image@counter(4).png';

    if MessageInputQuery(Window, 'Input start time for recording movie:', TimeBegin) then
      if MessageInputQuery(Window, 'Time step between capturing movie frames:' + NL +
        NL +
        'Note that if you later choose to record to a single movie file, like "output.avi", then we''ll generate a movie with 25 frames per second. ' +
        'So if you want your movie to play with the same speed as animation in castle-model-viewer then the default value, 1/25, is good.' + NL +
        NL +
        'Input time step between capturing movie frames:', TimeStep) then
        if MessageInputQueryCardinal(Window, 'Input frames count to capture:', FramesCount) then
          if Window.FileDialog('Images pattern or movie file to save', UrlPattern, false) then
          begin
            { ScreenShotsList should always be empty in interactive mode
              (otherwise some rendering behaves differently when
              MakingScreenShot = true) }
            Assert(ScreenShotsList.Count = 0);

            Range := TRangeScreenShot.Create;
            Range.TimeBegin := TimeBegin;
            Range.TimeStep := TimeStep;
            Range.FramesCount := FramesCount;
            Range.UrlPattern := UrlPattern;
            ScreenShotsList.Add(Range);

            try
              MakeAllScreenShotsFBO(Transparency);
            except
              on E: EInvalidScreenShotUrl do
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

    if MessageInputQuery(Window, 'This will "record" an interactive animation (done by VRML/X3D events, interpolators, sensors etc.) into a non-interactive precalculated animation.' + ' This allows an animation to be played ultra-fast, although may also be memory-consuming for long ranges of time.' + NL +
         NL +
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

        So I just treat it silently as non-fixable in castle-model-viewer,
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
      EventsHandler.GeometryChanged(nil, true, nil);
      EventsHandler.ViewpointsChanged(Scene);
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
    UrlPattern: string;
    Orientation: char;
    Size: Cardinal;

    procedure SaveSide(const Image: TCastleImage; const SideName: string);
    begin
      SaveImage(Image, StringReplace(UrlPattern, '@side', SideName, [rfReplaceAll]));
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
      if SceneUrl <> '' then
        UrlPattern := ChangeURIExt(ExtractURIName(SceneUrl), '_cubemap_@side.png')
      else
        UrlPattern := 'castle-model-viewer_cubemap_@side.png';

      if Window.FileDialog('Image name template to save', UrlPattern, false) then
      begin
        Size := DefaultCubeMapSize;

        if MessageInputQueryCardinal(Window, 'Size of cube map images', Size) then
        begin
          for Side := Low(Side) to High(Side) do
            CubeMapImg[Side] := TRGBImage.Create(Size, Size);

          GLCaptureCubeMapImages(CubeMapImg, MainViewport.Camera.Translation,
            {$ifdef FPC}@{$endif} TV3DViewport(MainViewport).RenderFromViewEverything,
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
    Url: String;
    Size: Cardinal;
  begin
    if SceneUrl <> '' then
      Url := ChangeURIExt(ExtractURIName(SceneUrl), '_cubemap.dds')
    else
      Url := 'castle-model-viewer_cubemap.dds';

    if Window.FileDialog('Save image to file', Url, false) then
    begin
      Size := DefaultCubeMapSize;

      if MessageInputQueryCardinal(Window, 'Size of cube map images', Size) then
      begin
        Composite := GLCaptureCubeMapComposite(Size, MainViewport.Camera.Translation,
          {$ifdef FPC}@{$endif} TV3DViewport(MainViewport).RenderFromViewEverything,
          MainViewport.Camera.EffectiveProjectionNear,
          MainViewport.Camera.EffectiveProjectionFar);
        try
          RenderContext.Viewport := Window.Rect;
          Composite.SaveToFile(Url);
        finally FreeAndNil(Composite) end;
      end;
    end;
  end;

  procedure ScreenShotDepthToImage;

    procedure DoSave(const Url: String);
    var
      Image: TGrayscaleImage;
    begin
      { Just like TCastleWindow.SaveScreen, we have to force redisplay now
        (otherwise we could be left here with random buffer contents from
        other window obscuring us, or we could have depth buffer from
        other drawing routine (like "frozen screen" drawn under FileDialog). }
      Window.Container.EventBeforeRender;
      Window.Container.EventRender;
      Image := SaveScreenDepth_NoFlush(Window.Rect);
      try
        SaveImage(Image, Url);
      finally FreeAndNil(Image) end;
    end;

  var
    Url: String;
  begin
    if SceneUrl <> '' then
      Url := ChangeURIExt(ExtractURIName(SceneUrl), '_depth_%d.png')
    else
      Url := 'castle-model-viewer_depth_%d.png';
    Url := FileNameAutoInc(Url);

    if Window.FileDialog('Save depth to a file', Url, false, SaveImage_FileFilters) then
      DoSave(Url);
  end;

  procedure Raytrace;
  var
    Pos, Dir, Up: TVector3;
    BaseLights: TLightInstancesList;
  begin
    MainViewport.Camera.GetWorldView(Pos, Dir, Up);
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

  procedure SaveAs;
  var
    ProposedSaveName: string;
  begin
    ProposedSaveName := SceneUrl;

    // if we cannot save to SceneUrl format, propose to save as X3D
    if not TFileFilterList.Matches(SaveNode_FileFilters, ProposedSaveName) then
      ProposedSaveName := ChangeURIExt(ProposedSaveName, '.x3d');

    if Window.FileDialog('Save As...', ProposedSaveName, false, SaveNode_FileFilters) then
    begin
      {$ifdef CATCH_EXCEPTIONS}
      try
      {$endif}

        SaveNode(Scene.RootNode, ProposedSaveName, SaveGenerator,
          ExtractURIName(SceneUrl));

      {$ifdef CATCH_EXCEPTIONS}
      except
        on E: Exception do
        begin
          MessageOK(Window, 'Error while saving scene to "' +ProposedSaveName+
            '": ' + E.Message);
        end;
      end;
      {$endif}
    end;
  end;

  procedure SetLimitFPS;
  var
    F: Single;
  begin
    F := ApplicationProperties.LimitFPS;
    if MessageInputQuery(Window,
      'Set approximate FPS (Frames Per Second) limit.' + NL +
      NL +
      'Smaller values give OS and CPU some rest, to run other applications or conserve laptop battery. Your monitor has a fixed refresh rate anyway, so limiting FPS doesn''t necessarily mean worse visuals (unless you set it to some really small value).' + NL +
      NL +
      'This is only an approximation of desired FPS. Usually it''s quite precise, but on some systems some values may be actually capped by monitor refresh rate.' + NL +
      NL +
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

  procedure SwitchScreenSpaceReflections;
  var
    I: Integer;
  begin
    with MainViewport do
      ScreenSpaceReflections := not ScreenSpaceReflections;
    for I := Low(ExtraViewports) to High(ExtraViewports) do
      with ExtraViewports[I] do
        ScreenSpaceReflections := not ScreenSpaceReflections;
  end;

  procedure OpenSceneUrl;
  var
    Url, CopyStr, CutStr, PasteStr: string;
  begin
    Url := SceneUrl;
    Check(KeyToString(CtrlC, keyNone, [], CopyStr));
    Check(KeyToString(CtrlX, keyNone, [], CutStr));
    Check(KeyToString(CtrlV, keyNone, [], PasteStr));
    if MessageInputQuery(Window,
      'Open 3D model from given URL.' + NL + NL +
      'Note that by default "http" is disabled (because the downloads are blocking for now, and every 3D model may reference additional resources like textures). Enable http by checking "Preferences -> Download Resources From Network".' + NL + NL +
      'You can copy (' + CopyStr +
      '), cut (' + CutStr +
      ') and paste (' + PasteStr +
      ') here, for example to easily paste URL from/to your web browser.' + NL + NL +
      'URL:', Url) then
      LoadScene(Url);
  end;

  procedure Reopen;
  var
    Pos, Dir, Up: TVector3;
    SavedNavigationType: TUserNavigationType;
  begin
    { reopen saves/restores camera view and navigation type,
      this makes it more useful }
    SavedNavigationType := NavigationType;
    MainViewport.Camera.GetWorldView(Pos, Dir, Up{, GravityUp});

    LoadScene(SceneUrl);

    { restore view, without GravityUp (trying to preserve it goes wrong
      in case we're in Examine mode, then "reopen", then switch to "Walk"
      --- original scene's gravity is then lost) }
    MainViewport.Camera.SetWorldView(Pos, Dir, Up{, GravityUp});
    { restore NavigationType }
    SetNavigationType(SavedNavigationType);
    UpdateCameraUI;
  end;

  function SceneVertexTriangleInfo(const Scene: TCastleScene): string;
  begin
    Result := Format('Scene contains %d triangles and %d vertices.',
      [Scene.TrianglesCount, Scene.VerticesCount]) + NL;
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
    SelectedItem^.Shape.Node.Visible := false;
  end;

  procedure RevealAllHiddenShapes;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    ShapeList := Scene.Shapes.TraverseList({ OnlyActive } false);
    for Shape in ShapeList do
      if Shape.Node <> nil then
        Shape.Node.Visible := true;
  end;

  procedure ChangeMaxLights;
  var
    L: Cardinal;
  begin
    L := Scene.RenderOptions.MaxLightsPerShape;
    if MessageInputQueryCardinal(Window, 'Maximum Number of Lights (that can affect a single shape):', L) then
      Scene.RenderOptions.MaxLightsPerShape := L;
  end;

var
  C: Cardinal;
begin
  case MenuItem.IntData of
    10: ClickButtonOpen(nil);
    11: OpenSceneUrl;
    12: Window.Close;
    15: Reopen;
    900: SaveAs;
    21: ButtonWarnings.DoClick;

    31: ChangeScene([scNoNormals], Scene);
    32: ChangeScene([scNoSolidObjects], Scene);
    33: ChangeScene([scNoConvexFaces], Scene);
    33000: ChangeScene([scConvertInlines], Scene);

    34: RemoveNodesWithMatchingName;
    38: RemoveGamePlaceholders;

    42: VisualizeHumanoids;
    45: VisualizeTransformations;

    3500: with Scene do ShadowMaps := not ShadowMaps;
    3510..3519: Scene.RenderOptions.ShadowSampling :=
      TShadowSampling(Ord(MenuItem.IntData) - 3510);
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

    51: begin
          MainViewport.Camera.AnimateTo(
            DefaultX3DCameraPosition[cvVrml1_Inventor],
            DefaultX3DCameraDirection,
            DefaultX3DCameraUp,
            CameraTransitionTime);
          MainViewport.Camera.GravityUp := DefaultX3DGravityUp;
        end;
    52: begin
          MainViewport.Camera.AnimateTo(
            DefaultX3DCameraPosition[cvVrml2_X3d],
            DefaultX3DCameraDirection,
            DefaultX3DCameraUp,
            CameraTransitionTime);
          MainViewport.Camera.GravityUp := DefaultX3DGravityUp;
        end;

    53: SetViewpointTop;
    54: SetViewpointBottom;
    57: SetViewpointFront;
    58: SetViewpointBack;
    59: SetViewpointRight;
    60: SetViewpointLeft;

    65: Viewpoints.Initial(MainViewport);
    66: Viewpoints.Previous(MainViewport);
    67: Viewpoints.Next(MainViewport);
    68: Viewpoints.Final(MainViewport);

    82: ShowBBox := not ShowBBox;
    84: if Window.ColorDialog(BGColor) then BGColorChanged;
    83: MainViewport.DynamicBatching := not MainViewport.DynamicBatching;
    86: with Scene.RenderOptions do Blending := not Blending;
    87: if MainViewport.OcclusionSort = sortAuto then
          MainViewport.OcclusionSort := sort3D
        else
          MainViewport.OcclusionSort := sortAuto;
    88: MainViewport.OcclusionCulling := not MainViewport.OcclusionCulling;

    91: with Scene.RenderOptions do Lighting := not Lighting;
    92: with Scene do HeadLightOn := not HeadLightOn;
    93: with Scene.RenderOptions do ReceiveSceneLights := not ReceiveSceneLights;
    940: ChangeMaxLights;
    950: with Scene.RenderOptions do RobustNegativeScale := not RobustNegativeScale;
    94: with Scene.RenderOptions do Textures := not Textures;
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
             'Current camera frustum planes :' + NL +
             '((A, B, C, D) means a plane given by equation A*x + B*y + C*z + D = 0.)' + NL +
             '  Left   : ' + MainViewport.Camera.Frustum.Planes[fpLeft].ToRawString + NL +
             '  Right  : ' + MainViewport.Camera.Frustum.Planes[fpRight].ToRawString + NL +
             '  Bottom : ' + MainViewport.Camera.Frustum.Planes[fpBottom].ToRawString + NL +
             '  Top    : ' + MainViewport.Camera.Frustum.Planes[fpTop].ToRawString + NL +
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

    121: MessageReport('Scene "' + SceneUrl + '" information:' + NL + NL +
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
    128: begin
           PersistentMouseLook := not PersistentMouseLook;
           UpdateCameraUI;
         end;
    129: MessageReport(ManifoldEdgesInfo(Scene));

    131: begin
           MessageReport(
             'viewer for all 3D and 2D model formats supported by Castle Game Engine.' + NL +
             NL +
             'Supported formats:' + NL +
             '- glTF 2.0' + NL +
             '- X3D' + NL +
             '- VRML 2.0 (aka VRML 97) and 1.0' + NL +
             '- Spine JSON' + NL +
             '- Sprite sheets in Starling and Cocos2d formats' + NL +
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
             'See ' + ViewerUrl + ' .' + NL +
             NL +
             'Created using Castle Game Engine ( https://castle-engine.io/ ) version ' + CastleEngineVersion + '.' + NL +
             'Compiled with ' + SCompilerDescription + '.');
         end;
    132: if not OpenUrl(ViewerUrl) then
           Window.MessageOk(SCannotOpenUrl, mtError);
    134: ClickButtonPatreon(nil);

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
    342: SwitchScreenSpaceReflections;
    350..370:
      begin
        ScreenEffects.ActiveEffectsApply(MainViewport, 0);
        ScreenEffects.ActiveEffectsApply(ExtraViewports[0], 1);
        ScreenEffects.ActiveEffectsApply(ExtraViewports[1], 2);
        ScreenEffects.ActiveEffectsApply(ExtraViewports[2], 3);
        { Redisplay to actually render with the new effect }
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

    710: ChangeMaterialMainColor;
    711: ChangeMaterialEmissiveColor;
    715: ResetMaterial(TPhysicalMaterialNode.Create);
    716: ResetMaterial(TUnlitMaterialNode.Create);
    717: ResetMaterial(TMaterialNode.Create);

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
    2010: EnableBlockingDownloads := not EnableBlockingDownloads;

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
    1400..1499: Scene.RenderOptions.BumpMapping :=
      TBumpMapping(MenuItem.IntData - 1400);
    3600..3610: SetViewportsConfig(TViewportsConfig(MenuItem.IntData - 3600),
      V3DSceneWindow.Window, MainViewport);
    4000: Scene.RenderOptions.PhongShading := not Scene.RenderOptions.PhongShading;
    5000..5099: ColorSpace := TColorSpace(MenuItem.IntData - 5000);
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
  ColorSpaceNames: array [TColorSpace] of String = (
    'sRGB (Gamma Correction = No)',
    'Linear On Physical Materials (Gamma Correction = when PBR)',
    'Linear Always (Gamma Correction = Yes)'
  );

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
    M.Append(TMenuItem.Create('_Save As...', 900, CtrlS));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('View _Warnings About Current Scene', 21));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('_Preferences');
      M3 := TMenu.Create('_Anti Aliasing (Restart Application to Apply)');
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
        EnableBlockingDownloads, true));
      M.Append(M2);
    NextRecentMenuItem := TMenuSeparator.Create;
    M.Append(NextRecentMenuItem);
    RecentMenu.NextMenuItem := NextRecentMenuItem;
    M.Append(TMenuItem.Create('_Exit',             12, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('_Bounding Box Visible',      82, CtrlB,
      ShowBBox, true));
    M.Append(TMenuItemChecked.Create('Status and Toolbar Visible',  122, keyF1,
      ShowStatus, true));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('_Fill Mode');
      MenuAppendFillModes(M2, 500);
      M2.Append(TMenuSeparator.Create);
      M2.Append(TMenuItem.Create('Next _Fill Mode', 520, CtrlF));
      M.Append(M2);
    M.Append(ScreenEffects.Menu);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Phong Shading on Everything', 4000, Scene.RenderOptions.PhongShading, true));
    M2 := TMenu.Create('Color Space');
      M2.AppendRadioGroup(ColorSpaceNames, 5000, Ord(ColorSpace), true);
      M.Append(M2);
    M2 := TMenu.Create('Tone Mapping');
      M2.AppendRadioGroup(ToneMappingNames, 5100, Ord(ToneMapping), true);
      M.Append(M2);
    M2 := TMenu.Create('Bump Mapping');
      M2.AppendRadioGroup(BumpMappingNames, 1400, Ord(Scene.RenderOptions.BumpMapping), true);
      M.Append(M2);
    M2 := TMenu.Create('Shadow Maps');
      M2.Append(TMenuItemChecked.Create('Enable', 3500, Scene.ShadowMaps, true));
      M2.Append(TMenuSeparator.Create);
      M2.AppendRadioGroup(ShadowSamplingNames, 3510,
        Ord(Scene.RenderOptions.ShadowSampling), true);
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
      Scene.RenderOptions.Lighting, true));
    MenuHeadlight := TMenuItemChecked.Create('_Headlight', 92, CtrlH,
      (Scene <> nil) and Scene.HeadlightOn, true);
    M.Append(MenuHeadlight);
    M.Append(TMenuItemChecked.Create('Receive Scene Lights',    93,
      Scene.RenderOptions.ReceiveSceneLights, true));
    M.Append(TMenuItem.Create('Max Lights Per Shape ...'  , 940));
    M.Append(TMenuItem.Create('Light Global Ambient Color ...',  95));
    M.Append(TMenuItemChecked.Create('Robust Negative Scale',    950,
      Scene.RenderOptions.RobustNegativeScale, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('_Textures',           94, CtrlT,
      Scene.RenderOptions.Textures, true));
    M2 := TMenu.Create('Texture Minification Method');
      MenuAppendMinificationFilters(M2, 1100);
      M.Append(M2);
    M2 := TMenu.Create('Texture Magnification Method');
      MenuAppendMagnificationFilters(M2, 1200);
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Blending',                86,
      Scene.RenderOptions.Blending, true));
    M.Append(TMenuItemChecked.Create('Dynamic Batching',        83,
      MainViewport.DynamicBatching, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create('Occlusion Sort', 87,
      MainViewport.OcclusionSort <> sortAuto, true));
    M.Append(TMenuItemChecked.Create('_Use Occlusion Culling', 88,
      MainViewport.OcclusionCulling, true));
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
        PersistentMouseLook, false);
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
      MenuEditMaterial.Append(TMenuItem.Create('Change Main (Emissive / Diffuse / Base) Color ...' , 710));
      MenuEditMaterial.Append(TMenuItem.Create('Change Emissive Color ...' , 711));
      MenuEditMaterial.Append(TMenuSeparator.Create);
      MenuEditMaterial.Append(TMenuItem.Create('Reset to Default Physical Material', 715));
      MenuEditMaterial.Append(TMenuItem.Create('Reset to Default Unlit Material', 716));
      MenuEditMaterial.Append(TMenuItem.Create('Reset to Default Phong Material', 717));
    M.Append(MenuEditMaterial);
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
    M.Append(TMenuItem.Create('Convert Inline to Group (pulls external content into this model)', 33000));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Add Joints Visualization ...', 45));
    M.Append(TMenuItem.Create('Add H-Anim Joints Visualization ...', 42));
    Result.Append(M);
  M := TMenu.Create('_Clipboard');
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (X3D XML)', 108));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 2.0, X3D classic)', 107));
    M.Append(TMenuItem.Create('Print Current Camera (Viewpoint) (VRML 1.0, deprecated)',   106));
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
    M.Append(TMenuItem.Create('_Screenshot to Image ...',                  150, keyF5));
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
    M.Append(TMenuItem.Create('Castle Model Viewer Website',        132));
    M.Append(TMenuItem.Create('Support us', 134));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About Castle Model Viewer',                  131));
    Result.Append(M);
end;

{ toolbar -------------------------------------------------------------------- }

var
  ButtonOpen, ButtonScreenshot: TCastleButton;

{ call when ShowStatus or MakingScreenShot changed }
procedure UpdateStatusToolbarVisible;

  procedure EnsureParent(const Child, WantedParent: TCastleUserInterface);
  begin
    if Child.Parent <> WantedParent then
    begin
      if Child.Parent <> nil then
        Child.Parent.RemoveControl(Child);
      WantedParent.InsertFront(Child);
    end;
    Assert(Child.Parent = WantedParent);
  end;

var
  Vis, VisShorter: boolean;
begin
  Vis := ShowStatus and (not MakingScreenShot);
  VisShorter := (not ShowStatus) and (not MakingScreenShot);

  if ToolbarPanel <> nil then // check was CreateMainUserInterface called already?
    ToolbarPanel.Exists := Vis;

  if ToolbarPanelShorter <> nil then
    ToolbarPanelShorter.Exists := VisShorter;

  if StatusText <> nil then
    StatusText.Exists := Vis;

  if ButtonPatreon <> nil then
    ButtonPatreon.Exists := Vis;

  { make sure ButtonWarnings.Parent is correct }
  if (ButtonWarnings <> nil) and
     (ToolbarHorizGroupShorter <> nil) and
     (ToolbarHorizGroupShorter <> nil) then
  begin
    if ShowStatus then
      EnsureParent(ButtonWarnings, ToolbarHorizGroup)
    else
      EnsureParent(ButtonWarnings, ToolbarHorizGroupShorter);
  end;
end;

{ Initialize StatusText and ToolbarPanel and various buttons instances }
procedure CreateMainUserInterface;
var
  NavigationButtonsGroup: TCastleHorizontalGroup;
  NT: TUserNavigationType;
  ToolbarBackground: TCastleImageControl;
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  ImagePatreon: TCastleImageControl;
const
  MinImageHeight = 22;
begin
  StatusText := TExtendedStatusText.Create(Application);
  Window.Controls.InsertFront(StatusText);

  UiOwner := TComponent.Create(Application);
  Ui := StringToComponent({$I embedded_data/designs/main.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;
  Window.Controls.InsertFront(Ui);

  ToolbarPanel := UiOwner.FindRequiredComponent('ToolbarPanel') as TCastleUserInterface;
  ToolbarPanelShorter := UiOwner.FindRequiredComponent('ToolbarPanelShorter') as TCastleUserInterface;
  ToolbarHorizGroup := UiOwner.FindRequiredComponent('ToolbarHorizGroup') as TCastleUserInterface;
  ToolbarHorizGroupShorter := UiOwner.FindRequiredComponent('ToolbarHorizGroupShorter') as TCastleUserInterface;

  { Note that we need to assign all images,
    because we embed all images in castle-model-viewer binary. }

  ButtonOpen := UiOwner.FindRequiredComponent('ButtonOpen') as TCastleButton;
  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonOpen;
  ButtonOpen.Image.Image := V3DSceneImages.Open;
  ButtonOpen.Image.OwnsImage := false;
  ButtonOpen.MinImageHeight := MinImageHeight;

  ButtonCollisions := UiOwner.FindRequiredComponent('ButtonCollisions') as TCastleButton;
  ButtonCollisions.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonCollisions;
  ButtonCollisions.MinImageHeight := MinImageHeight;
  { When Scene = nil, make Pressed = true,
    because it means the Scene will be soon created with Scene.Collides = default true. }
  ButtonCollisions.Pressed := (Scene = nil) or Scene.Collides;

  ButtonScreenshot := UiOwner.FindRequiredComponent('ButtonScreenshot') as TCastleButton;
  ButtonScreenshot.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonScreenshot;
  ButtonScreenshot.Image.Image := V3DSceneImages.Screenshot;
  ButtonScreenshot.Image.OwnsImage := false;
  ButtonScreenshot.MinImageHeight := MinImageHeight;

  ButtonAnimations := UiOwner.FindRequiredComponent('ButtonAnimations') as TCastleButton;
  ButtonAnimations.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonAnimations;
  ButtonAnimations.Toggle := true;
  ButtonAnimations.Image.Image := V3DSceneImages.Animations;
  ButtonAnimations.Image.OwnsImage := false;
  ButtonAnimations.MinImageHeight := MinImageHeight;

  ButtonWarnings := UiOwner.FindRequiredComponent('ButtonWarnings') as TCastleButton;
  ButtonWarnings.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonWarnings;
  ButtonWarnings.Image.Image := Warning_icon;
  ButtonWarnings.Image.OwnsImage := false;
  ButtonWarnings.MinImageHeight := MinImageHeight;

  ButtonPatreon := UiOwner.FindRequiredComponent('ButtonPatreon') as TCastleButton;
  ButtonPatreon.OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickButtonPatreon;
  ButtonPatreon.CustomBackgroundNormal.Image := Castle_Game_Engine_Icon;
  ButtonPatreon.CustomBackgroundNormal.OwnsImage := false;
  ButtonPatreon.CustomBackgroundDisabled.Image := Castle_Game_Engine_Icon;
  ButtonPatreon.CustomBackgroundDisabled.OwnsImage := false;
  ButtonPatreon.CustomBackgroundPressed.Image := Castle_Game_Engine_Icon;
  ButtonPatreon.CustomBackgroundPressed.OwnsImage := false;
  ButtonPatreon.CustomBackgroundFocused.Image := Castle_Game_Engine_Icon;
  ButtonPatreon.CustomBackgroundFocused.OwnsImage := false;
  ButtonPatreon.MinImageHeight := MinImageHeight;

  ImagePatreon := UiOwner.FindRequiredComponent('ImagePatreon') as TCastleImageControl;
  ImagePatreon.Image := Cge_Heart;
  ImagePatreon.OwnsImage := false;

  ToolbarBackground := UiOwner.FindRequiredComponent('ToolbarBackground') as TCastleImageControl;
  ToolbarBackground.Image := Panel;
  ToolbarBackground.OwnsImage := false;

  if SceneWarnings <> nil then
    UpdateButtonWarnings
  else
    ButtonWarnings.Exists := false; { at least initialize Exists }

  NavigationButtonsGroup := UiOwner.FindRequiredComponent('NavigationButtonsGroup') as TCastleHorizontalGroup;

  for NT := Low(NT) to High(NT) do
    { Don't show button for ntNone.
      For ntNone it's confusing for new user.
      The "none" navigation type is visible in menu. }
    if NT <> untNone then
    begin
      NavigationButtons[NT] := TNavigationTypeButton.Create(Application, NT);
      NavigationButtons[NT].Caption := NavigationNames[NT];
      NavigationButtons[NT].OnClick := {$ifdef FPC}@{$endif} EventsHandler.ClickNavigationTypeButton;
      NavigationButtons[NT].Toggle := true;
      NavigationButtons[NT].MinImageHeight := MinImageHeight;
      NavigationButtonsGroup.InsertFront(NavigationButtons[NT]);
    end;

  NavigationButtons[untExamine].Image.Image := V3DSceneImages.Examine;
  NavigationButtons[untExamine].Image.OwnsImage := false;
  NavigationButtons[untFly].Image.Image := V3DSceneImages.Fly;
  NavigationButtons[untFly].Image.OwnsImage := false;
  NavigationButtons[untWalk].Image.Image := V3DSceneImages.Walk;
  NavigationButtons[untWalk].Image.OwnsImage := false;
  NavigationButtons[unt2D].Image.Image := V3DSceneImages.Nav2d;
  NavigationButtons[unt2D].Image.OwnsImage := false;

  UpdateStatusToolbarVisible;

  NavigationUi := TNavigationUi.Create(Application);
  NavigationUi.LabelMoveSpeedContainer := UiOwner.FindRequiredComponent('LabelMoveSpeedContainer') as TCastleRectangleControl;
  NavigationUi.LabelMoveSpeed := UiOwner.FindRequiredComponent('LabelMoveSpeed') as TCastleLabel;
  Window.Controls.InsertFront(NavigationUi);
end;

procedure TEventsHandler.ClickButtonOpen(Sender: TObject);
var
  Url: String;
begin
  Url := SceneUrl;
  if Window.FileDialog('Open file', Url, true, LoadScene_FileFilters) then
    LoadScene(Url);
end;

procedure TEventsHandler.ClickNavigationTypeButton(Sender: TObject);
begin
  SetNavigationType((Sender as TNavigationTypeButton).NavigationType);
  UpdateCameraUI;
end;

procedure TEventsHandler.ClickButtonCollisions(Sender: TObject);
begin
  SetCollisions(not Scene.Collides, true);
end;

procedure TEventsHandler.ClickButtonScreenshot(Sender: TObject);
begin
  ScreenShotImage('Screenshot to Image', false);
end;

procedure TEventsHandler.ClickButtonAnimations(Sender: TObject);
begin
  ToggleNamedAnimationsUi;
end;

{ Try to lower anti-aliasing (multi-sampling) and shadows (stencil buffer)
  requirements and initialize worse GL context. }
function RetryOpen(Window: TCastleWindow): boolean;
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
  WasParam_SceneUrl: boolean = false;
  Param_SceneUrl: String;
  Param_HideMenu: boolean = false;
  Param_ScreenshotTransparent: boolean = false;

const
  Options: array [0..19] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'screenshot'; Argument: oaRequired2Separate),
    (Short:  #0; Long: 'screenshot-range'; Argument: oaRequired4Separate),
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
    (Short:  #0; Long: 'capabilities'; Argument: oaRequired),
    (Short:  #0; Long: 'project'; Argument: oaRequired),
    (Short:  #0; Long: 'no-x3d-extensions'; Argument: oaNone)
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
    0 : begin
          S :=
            'Viewer for all 3D and 2D model formats supported by Castle Game Engine:' + NL +
            'glTF, X3D, VRML, Spine, sprite sheets and more.' + NL +
            'You can navigate in the (animated and interactive) scene,' + NL +
            'with collision-checking, gravity, and a wealth of graphic effects.' + NL +
            'You can also convert models in other formats to X3D.' + NL +
            NL +
            'Call as' + NL +
            '  castle-model-viewer [OPTIONS]... [FILE-NAME-TO-OPEN]' + NL +
            NL +
            'You can provide FILE-NAME-TO-OPEN on the command-line.' + NL +
            NL +
            'Available options:' + NL +
            OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
            OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
            OptionDescription('-H / --hide-extras', 'Do not show anything extra (like status text or toolbar or bounding box) when program starts. Show only the loaded model.') + NL +
            OptionDescription('--hide-menu', 'Hide menu bar.') + NL +
            OptionDescription('--no-x3d-extensions', 'Do not use Castle Game Engine extensions to X3D. Particularly useful when combined with --write, to have X3D valid in all browsers (but less functional).')  + NL +
            OptionDescription('--screenshot TIME IMAGE-FILE-NAME', 'Take a screenshot of the loaded scene at given TIME, and save it to IMAGE-FILE-NAME. You most definitely want to pass 3D model file to load at command-line too, otherwise we''ll just make a screenshot of the default black scene.')  + NL +
            OptionDescription('--screenshot-range TIME-BEGIN TIME-STEP FRAMES-COUNT FILE-NAME', 'Take a FRAMES-COUNT number of screenshots from TIME-BEGIN by step TIME-STEP. Save them to a single movie file (like .avi) (ffmpeg must be installed and available on $PATH for this) or to a sequence of image files (FILE-NAME must then be specified like image@counter(4).png).')  + NL +
            OptionDescription('--screenshot-transparent', 'Screenshots background is transparent. Useful only together with --screenshot-range or --screenshot options.')  + NL +
            OptionDescription('--viewpoint NAME', 'Use the viewpoint with given name or index as initial. Especially useful to make a screenshot from given viewpoint.')  + NL +
            OptionDescription('--anti-alias AMOUNT', 'Use full-screen anti-aliasing. Argument AMOUNT is an integer >= 0. Exact 0 means "no anti-aliasing", this is the default. Each successive integer generally makes method one step better. Especially useful to make a screenshot with anti-aliasing quality.')  + NL +
            OptionDescription('--project DIR', 'Point to Castle Game Engine project directory (or CastleEngineManifest.xml file) to resolve the "castle-data:/" URLs in files.')  + NL +
            NL +
            'Sound options:' + NL +
            SoundEngine.ParseParametersHelp + NL +
            NL +
            'Window options:' + NL +
            TCastleWindow.ParseParametersHelp + NL +
            NL +
            'Debug options:' + NL +
            OptionDescription('--debug-log-cache', 'Write log info, including cache.')  + NL +
            OptionDescription('--debug-log-shaders', 'Write log info, including shader source and log.')  + NL +
            OptionDescription('--debug-log-changes', 'Write log info, including VRML/X3D graph changes.')  + NL +
            OptionDescription('--debug-log-videos', 'Write log info, including videos loading and cache.')  + NL +
            OptionDescription('--debug-texture-memory', 'Profile GPU texture memory usage.')  + NL +
            OptionDescription('--capabilities automatic|force-fixed-function|force-modern', 'Force OpenGL context to have specific capabilities, to test rendering on modern or ancient GPUs.') + NL +
            NL +
            'Deprecated options:' + NL +
            OptionDescription('--write', 'DEPRECATED. Load the model and save it as X3D or VRML to the standard output, exit. Use --write-encoding to choose encoding. Use instead castle-model-converter for command-line conversions, it has more functionality with simpler options.') + NL +
            OptionDescription('--write-encoding classic|xml', 'DEPRECATED. Choose X3D encoding to use with --write option. Default is "classic". Use instead castle-model-converter for command-line conversions, it has more functionality with simpler options.') + NL +
            OptionDescription('--write-force-x3d', 'DEPRECATED. Ignored now.')  + NL +
            NL +
            ApplicationProperties.Description;

          if IsConsole then
            Writeln(S)
          else
            WindowMessageOK(S);
          Halt;
        end;
    1 : begin
          InfoWrite(Version);
          Halt;
        end;
    2 : begin
          SingleScreenShot := TSingleScreenShot.Create;
          SingleScreenShot.Time := StrToFloat(SeparateArgs[1]);
          SingleScreenShot.UrlPattern := SeparateArgs[2];
          ScreenShotsList.Add(SingleScreenShot);
        end;
    3 : begin
          RangeScreenShot := TRangeScreenShot.Create;
          RangeScreenShot.TimeBegin := StrToFloat(SeparateArgs[1]);
          RangeScreenShot.TimeStep := StrToFloat(SeparateArgs[2]);
          RangeScreenShot.FramesCount := StrToInt(SeparateArgs[3]);
          RangeScreenShot.UrlPattern := SeparateArgs[4];
          ScreenShotsList.Add(RangeScreenShot);
        end;
    4 : LogChanges := true;
    5 : LogRendererCache := true;
    6 : LogShaders := true;
    7 : LogVideosCache := true;
    8 : LogTextureCache := true;
    9 : begin
          Window.AntiAliasing := TAntiAliasing(Clamped(StrToInt(Argument),
            Ord(Low(TAntiAliasing)), Ord(High(TAntiAliasing))));
          if AntiAliasingMenu[Window.AntiAliasing] <> nil then
            AntiAliasingMenu[Window.AntiAliasing].Checked := true;
        end;
    10: begin
          ShowBBox := false;
          ShowStatus := false;
          UpdateStatusToolbarVisible;
        end;
    11: begin
          WritelnWarning('Option --write is deprecated. Use castle-model-converter for command-line model conversion, it has more features and simpler command-line options.');
          WasParam_Write := true;
        end;
    12: begin
          WritelnWarning('Option --write-encoding is deprecated. Use castle-model-converter for command-line model conversion, it has more features and simpler command-line options.');
          if SameText(Argument, 'classic') then
            Param_WriteEncoding := xeClassic
          else
          if SameText(Argument, 'xml') then
            Param_WriteEncoding := xeXML
          else
            raise EInvalidParams.CreateFmt('Invalid --write-encoding argument "%s"', [Argument]);
        end;
    13: WritelnWarning('Option --write-force-x3d is deprecated and ignored now. Use castle-model-converter for command-line model conversion, it has more features and simpler command-line options.');
    14: Param_HideMenu := true;
    15: TextureMemoryProfiler.Enabled := true;
    16: Param_ScreenshotTransparent := true;
    { We can change TGLFeatures.RequestCapabilities immediately,
      during parsing of command-line options. In fact it's good --
      we should not change TGLFeatures.RequestCapabilities once the context is open. }
    17: TGLFeatures.RequestCapabilities := StrToCapabilities(Argument);
    18: SetProject(Argument);
    19: CastleX3dExtensions := false;
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
    - ApplicationName is used for Config.Url by ApplicationConfig, so it better be reliable. }
  ApplicationProperties.ApplicationName := 'castle-model-viewer';
  ApplicationProperties.Version := Version;

  EventsHandler := TEventsHandler.Create(Application);

  // Initialize log as early as possible, but avoid messing --help/--version/--write output
  LogEnableStandardOutput := false;
  InitializeLog;

  FontsInitialize;

  Window := TCastleWindow.Create(Application);
  Window.Container.UIScaling := usDpiScale;

  { Enable F8 even in -dRELEASE: this is castle-model-viewer,
    let's make debug tools available always. }
  Window.Container.InputInspector.Key := keyF8;

  Application.MainWindow := Window;

  Theme.ImagesPersistent[tiButtonNormal].Image := ButtonNormal;
  Theme.ImagesPersistent[tiButtonNormal].OwnsImage := false;
  Theme.ImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonPressed].Image := ButtonPressed;
  Theme.ImagesPersistent[tiButtonPressed].OwnsImage := false;
  Theme.ImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonFocused].Image := ButtonFocused;
  Theme.ImagesPersistent[tiButtonFocused].OwnsImage := false;
  Theme.ImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonDisabled].Image := ButtonDisabled;
  Theme.ImagesPersistent[tiButtonDisabled].OwnsImage := false;
  Theme.ImagesPersistent[tiButtonDisabled].ProtectedSides.AllSides := 2;

  UserConfig.Load;

  SoundEngine.DistanceModel := dmLinear; // corresponds to X3D spec closely

  { initialize RecentMenu }
  RecentMenu := TWindowRecentFiles.Create(nil);
  RecentMenu.OnOpenRecent := {$ifdef FPC}@{$endif} EventsHandler.OpenRecent;
  RecentMenu.LoadFromConfig(UserConfig);

  { parse parameters }
  Window.ParseParameters;
  SoundEngine.ParseParameters;
  ViewpointsParseParameters;
  Parameters.Parse(Options, @OptionProc, nil);
  { the most important param : URL to load }
  if Parameters.High > 1 then
    raise EInvalidParams.Create('Excessive command-line parameters. Expected at most one URL to load') else
  if Parameters.High = 1 then
  begin
    WasParam_SceneUrl := true;
    Param_SceneUrl := Parameters[1];
  end;

  MainViewport := TV3DViewport.Create(nil);
  MainViewport.FullSize := true;
  // Usign deprecated AutoCamera, castle-model-viewer needs this now to have
  // X3D Viewpoint animation affect camera.
  {$warnings off}
  MainViewport.AutoCamera := true;
  {$warnings on}
  MainViewport.AutoNavigation := true;
  Window.Controls.InsertBack(MainViewport);
  MainViewport.OnBoundViewpointChanged :=
    {$ifdef FPC}@{$endif} EventsHandler.BoundViewpointChanged;
  MainViewport.OnBoundNavigationInfoChanged :=
    {$ifdef FPC}@{$endif} EventsHandler.BoundNavigationInfoChanged;
  { Note: MainViewport.InternalWalkNavigation
    will be adjusted later, by InitializeViewportsAndDefaultNavigation }

  InitializeViewportsAndDefaultNavigation(TV3DViewport);
  BGColorChanged;

  CreateMainUserInterface;

  Window.Controls.InsertBack(ScreenEffects);

  SceneWarnings := TSceneWarnings.Create;
  try
    ApplicationProperties.OnWarning.Add(
      {$ifdef FPC}@{$endif} EventsHandler.OnWarningHandle);

    if WasParam_Write then
    begin
      if not WasParam_SceneUrl then
        raise EInvalidParams.Create('You used --write option, '+
          'this means that you want to convert some 3D model file to VRML/X3D. ' +
          'But you didn''t provide any URL on command-line to load.');
      WriteModel(Param_SceneUrl, Param_WriteEncoding);
      Exit;
    end;

    { init "scene global variables" to initial empty values }
    Scene := TCastleScene.Create(nil);
    try
      { makes X3D KeySensor and StringSensor working }
      Scene.ListenPressRelease := true;
      { do not use lights from Scene on other scenes }
      Scene.CastGlobalLights := false;

      AttributesLoadFromConfig(Scene.RenderOptions);
      InitializeViewportsMainScene(Scene);

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

        Window.GtkIconName := 'castle-model-viewer';
        Window.MainMenu := CreateMainMenu;
        Window.MainMenuVisible := not Param_HideMenu;
        Window.OnMenuItemClick := {$ifdef FPC}@{$endif} EventsHandler.MenuClick;
        { TODO: Use TCastleView to handle these events,
          and inside that TCastleView should be a viewport.
          Currently we abuse MainViewport for it. }
        MainViewport.OnPress := {$ifdef FPC}@{$endif} EventsHandler.Press;
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
        end;

        Window.FpsShowOnCaption := true;
        Window.StencilBits := 8;

        Window.Open(@RetryOpen);

        if WasParam_SceneUrl then
          LoadScene(Param_SceneUrl)
        else
          LoadWelcomeScene;

        if MakingScreenShot then
        begin
          MakeAllScreenShotsFBO(Param_ScreenshotTransparent);
          Exit;
        end;

        Application.Run;
      finally FreeScene end;

      AttributesSaveToConfig(Scene.RenderOptions);

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

{ Forces using a dedicated (faster) GPU on laptops with multiple GPUs.
  See https://castle-engine.io/dedicated_gpu }
{$if (not defined(CASTLE_NO_FORCE_DEDICATED_GPU)) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64)) and (defined(MSWINDOWS) or defined(Linux))}
    {$ifdef fpc}
     {$asmmode intel}
    {$endif}

    procedure NvOptimusEnablement; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    procedure AmdPowerXpressRequestHighPerformance; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    exports
      NvOptimusEnablement,
      AmdPowerXpressRequestHighPerformance;
{$ifend}

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
