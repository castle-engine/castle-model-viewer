{
  Copyright 2002-2007 Michalis Kamburelis.

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
}

{ WWW page of this program, with user documentation,
  is here: [http://vrmlengine.sourceforge.net/view3dscene.php].

  If you want to find out how to use my units to use VRML and 3DS
  models in your own programs, this program may look
  too complex and filled with irrelevant details.
  Instead you should look at simple example program in
  kambi_vrml_game_engine/3dmodels.gl/examples/simpleViewModel.dpr.

  Also kambi_vrml_game_engine/3dmodels/examples/many2vrml.dpr is an example
  how to write simple command-line converter from 3DS (and all other model
  formats I can read) to VRML.

  This is 3d scene viewer. Basic components are :
  - use LoadAsVRMLSequence to load any format to VRML scene.
    This converts any known (to our engine) 3D model format to VRML.
    This convertion doesn't lose anything because VRML is able to
    express everything that is implemented in other 3D formats readers.
    And we gain the simplicity of this program (we just treat everything
    as VRML scene, actually VRML animation),
    optimization (display lists optimizations,
    OpenGL renderer cache inside VRML renderer), functionality
    (like automatic normals generation based on creaseAngle).
  - render scene using TVRMLFlatSceneGL (actually TVRMLGLAnimation
    and TVRMLFlatSceneGL is inside)
  - use MatrixNavigation and TGLWindowNavigated to let user navigate
    over the scene using various navigation modes
    (Examine, Walk) and with optional gravity
  - build TVRMLTriangleOctree to allow collision detection for
    Walk navigation and to allow raytracer
  - build TVRMLShapeStateOctree to allow frustum culling using
    octree by TVRMLFlatSceneGL
  - use VRMLRayTracer embedded in RaytraceToWindow module to allow
    viewing raytraced image
  - allow some kind of object picking with mouse left button (using
    simple one-primary-ray casting)
}

program view3dscene;

uses
  KambiUtils, SysUtils, VectorMath, Boxes3d, Classes, KambiClassUtils,
  BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  ParseParametersUnit, ProgressUnit, MatrixNavigation, RaysWindow,
  KambiStringUtils, KambiFilesUtils, Math, KambiTimeUtils,
  { OpenGL related units: }
  OpenGLh, GLWindow, GLW_Navigated, KambiGLUtils, OpenGLBmpFonts,
  GLWinMessages, ProgressGL, GLWindowRecentMenu,
  { VRML (and possibly OpenGL) related units: }
  VRMLFields, KambiOctree, VRMLTriangleOctree, VRMLShapeStateOctree,
  VRMLNodes, Object3dAsVRML, VRMLFlatSceneGL,
  VRMLFlatScene, VRMLRayTracer, BackgroundGL, VRMLNodesDetailOptions,
  VRMLCameraUtils, VRMLErrors, VRMLGLHeadLight, VRMLGLAnimation,
  { view3dscene-specific units: }
  TextureFilters, ColorModulators, V3DSceneLights, RaytraceToWindow,
  MultiNavigators, SceneChangesUnit, BGColors, V3DSceneCamera,
  V3DSceneConfig, V3DSceneBlending;

var
  Wireframe: boolean = false;
  ShowStatus: boolean = true;
  ShowBBox: boolean = true;

  ShowFrustum: boolean = false;
  ShowFrustumAlwaysVisible: boolean = false;

  OctreeDisplayWhole: boolean = false;
  { This is meaningful only if OctreeDisplayWhole = false.
    -1 means "don't display octree". }
  OctreeDisplayDepth: integer = -1;

  { ponizsze zmienne istotne tylko w trybach nawigacji ktore robia
    wykrywanie kolizji: }
  CollisionCheck: boolean = true;

  { ustalane w Init, finalizowane w Close }
  StatusFont: TGLBitmapFont;

  AngleOfViewX: Single = 60;

  RecentMenu: TGLRecentMenu;

  { These are so-called "scene global variables".
    Modified only by LoadSceneCore (and all using it Load*Scene* procedures)
    and FreeScene.
    Also note that Glw.Caption (and FPSBaseCaption) also should be modified
    only by those procedures.

    In this program's comments I often talk about "null values" of these
    variables, "null values" mean that these variables have some *defined
    but useless* values, i.e.
      SceneAnimation.Loaded = false
      SceneTriangleOctree = nil
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
  { must always be > 0 }
  CameraKind: TVRMLCameraKind;
  { vars below istotne tylko w trybach nawigacji ktore robia wykrywanie kolizji: }
  CameraRadius: Single = 0.0;
  { 0.0 means "no limit". }
  VisibilityLimit: Single = 0.0;
  NavigationNode: TNodeNavigationInfo;
  ViewpointNode: TNodeGeneralViewpoint;
  SceneHeadlight: TVRMLGLHeadlight;

  { (SelectedItem = nil) if and only if (SelectedItemIndex = NoItemIndex). }
  SelectedItem: POctreeItem;
  SelectedItemIndex: Integer;
  { SelectedPoint always lies on SelectedItem item,
    and it's meaningless when SelectedItem = nil. }
  SelectedPoint: TVector3Single;
  MenuSelectedInfo: TMenuItem;
  MenuSelectedLightsInfo: TMenuItem;
  MenuRemoveSelectedGeometry: TMenuItem;
  MenuRemoveSelectedFace: TMenuItem;

  SceneWarnings: TStringList;

  { TODO: for now set only by command-line param, should be configurable at runtime. }
  RendererOptimization: TGLRendererOptimization = roSeparateShapeStates;

{ ogolne pomocnicze funkcje -------------------------------------------------- }

type
  { World time, used for animations.

    "Single" type is *not* enough for this.
    Proof: open rotate.kanim (from kambi_vrml_test_suite).
    Change "on display" time pass to 1000, wait a couple seconds
    (world time will reach a couple of thousands),
    change "on display" time pass back to 1.
    Result: with TAnimationTime as Single, animation becomes jagged.
    Reason: the precision loss of Single time, and the fact that
    Draw is not called all the time (because AutoRedisplay is false,
    and model is in Examine mode and is still (not rotating)),
    so incrementation steps of AnimationTime are very very small.

    Setting AutoRedisplay to true workarounds the problem too, but that's
    1. unacceptable to eat 100% CPU without a reason for utility like
    view3dscene 2. that's asking for trouble, after all even with
    AutoRedisplay = true the precision loss is there, it's just not
    noticeable... using better precision feels much safer. }
  TAnimationTime = Float;

var
  AnimationTime: TAnimationTime = 0.0;
  AnimationTimeSpeed: TAnimationTime = 1.0;
  AnimationTimeSpeedWhenLoading: TAnimationTime = 1.0;
  AnimationTimePaused: boolean = false;

  { These are set by Draw right after rendering a SceneAnimation frame. }
  LastRender_RenderedShapeStatesCount: Cardinal;
  LastRender_AllShapeStatesCount: Cardinal;

function AngleOfViewY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(
    AngleOfViewX, Glw.Height / Glw.Width);
end;

function WalkProjectionNear: Single;
begin
 Result := CameraRadius * 0.6;
end;

function WalkProjectionFar: Single;
var
  Box: TBox3d;
begin
  if VisibilityLimit <> 0.0 then
    Result := VisibilityLimit else
  begin
    Box := SceneAnimation.BoundingBoxSum;

    if IsEmptyBox3d(Box) then
      { When IsEmptyBox3d, Result is not simply "any dummy value".
        It must be appropriately larger than WalkProjectionNear
        to provide sufficient space for rendering Background node. }
      Result := WalkProjectionNear * 10 else
      Result := Box3dAvgSize(Box) * 20.0;
  end;
end;

procedure UpdateSelectedEnabled;
begin
  if MenuSelectedInfo <> nil then
    MenuSelectedInfo.Enabled := SelectedItem <> nil;
  if MenuSelectedLightsInfo <> nil then
    MenuSelectedLightsInfo.Enabled := SelectedItem <> nil;
  if MenuRemoveSelectedGeometry <> nil then
    MenuRemoveSelectedGeometry.Enabled := SelectedItem <> nil;
  if MenuRemoveSelectedFace <> nil then
    MenuRemoveSelectedFace.Enabled := SelectedItem <> nil;
end;

function SceneTriangleOctree: TVRMLTriangleOctree;
begin
  if (SceneAnimation <> nil) and
     (SceneAnimation.ScenesCount <> 0) and
     (SceneAnimation.FirstScene.DefaultTriangleOctree <> nil) then
    Result := SceneAnimation.FirstScene.DefaultTriangleOctree else
    Result := nil;
end;

function SceneShapeStateOctree: TVRMLShapeStateOctree;
begin
  if (SceneAnimation <> nil) and
     (SceneAnimation.ScenesCount <> 0) and
     (SceneAnimation.FirstScene.DefaultShapeStateOctree <> nil) then
    Result := SceneAnimation.FirstScene.DefaultShapeStateOctree else
    Result := nil;
end;

{ This calls SceneAnimation.PrepareRender.
  Additionally, if AllowProgess and some other conditions are met,
  this show progress of operation.

  Remember that you can call this only when gl context is already active
  (SceneAnimation.PrepareRender requires this) }
procedure PrepareRender(AllowProgress: boolean);
begin
  AllowProgress := AllowProgress and
    (SceneAnimation.ScenesCount > 1);

  if AllowProgress then
    Progress.Init(SceneAnimation.ScenesCount, 'Preparing animation');
  try
    SceneAnimation.PrepareRender([tgAll], [prBackground, prBoundingBox],
      AllowProgress, false);
  finally
    if AllowProgress then
      Progress.Fini;
  end;
end;

{ TGLWindow callbacks --------------------------------------------------------- }

procedure Init(glwin: TGLWindow);
begin
 statusFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);

 { normalize normals because we will scale our objects in Examiner navigation;
   chwilowo i tak w Scene.Render zawsze jest wlaczane glEnable(GL_NORMALIZE)
   ale to nie zawsze bedzie prawdziwe.  }
 glEnable(GL_NORMALIZE);
 glEnable(GL_DEPTH_TEST);

 { We want to be able to render any scene --- so we have to be prepared
   that fog interpolation has to be corrected for perspective. }
 glHint(GL_FOG_HINT, GL_NICEST);

 ProgressGLInterface.Window := Glw;
 Progress.UserInterface := ProgressGLInterface;

 BGColorChanged;
end;

procedure Close(glwin: TGLWindow);
begin
  if SceneAnimation <> nil then
    SceneAnimation.CloseGL;
  FreeAndNil(statusFont);
end;

procedure DrawStatus(data: Pointer);

  function OctreeDisplayToStr: string;
  begin
    if OctreeDisplayWhole then
      Result := 'whole' else
    if OctreeDisplayDepth <> -1 then
      Result := 'depth ' + IntToStr(OctreeDisplayDepth) else
      Result := 'none';
  end;

const BoolToStrOO: array[boolean] of string=('OFF','ON');
var strs: TStringList;
    s: string;
begin
 glLoadIdentity;
 glTranslatef(5, 5, 0);

 strs := TStringList.Create;
 try
  strs.Append(Format('Navigation mode: %s', [NavigatorNames[NavigatorKind]]));

  if Glw.Navigator is TMatrixWalker then
  begin
   strs.Append(Format('Collision check %s, CamPos %s, CamDir %s',
     [ BoolToStrOO[collisionCheck],
       VectorToNiceStr(Glw.NavWalker.CameraPos),
       VectorToNiceStr(Glw.NavWalker.CameraDir)]));
   strs.Append(Format('CamUp %s, Move speed : %f',
     [ VectorToNiceStr(Glw.NavWalker.CameraUp), Glw.NavWalker.MoveSpeed ]));
  end else
  begin
   strs.Append(Format('Rotation : %s, Move : %s, Scale : %f',
     [ VectorToNiceStr(Glw.NavExaminer.RotationsAngle),
       VectorToNiceStr(Glw.NavExaminer.MoveAmount),
       Glw.NavExaminer.ScaleFactor ]));
  end;

  strs.Append(Format('Camera Kind : %s, Octree display : %s',
    [ VRMLCameraKindToStr[CameraKind],
      OctreeDisplayToStr ]));

  if SceneLightsCount = 0 then
   s := '(useless, scene has no lights)' else
   s := BoolToStrOO[SceneAnimation.Attributes.UseLights];
  strs.Append(Format('Use scene lights: %s', [s]));

  { Note: there's no sense in showing here Glw.FpsRealTime,
    since it would force me to constantly render new frames just
    to show constantly changing Glw.FpsRealTime ...
    this makes no sense, of course.
    I can sensibly show here only Glw.FpsFrameTime.
    User will be able to see Glw.FpsRealTime only on window's Caption. }
  strs.Append(Format('Rendered ShapeStates : %d of %d. FPS : %f',
    [ LastRender_RenderedShapeStatesCount,
      LastRender_AllShapeStatesCount,
      Glw.FpsFrameTime ]));

  S := Format('World time : %f', [AnimationTime]);
  if AnimationTimePaused then
    S += ' (paused)';
  strs.Append(S);

  {statusFont.printStringsBorderedRect(strs, 0, Brown4f, Yellow4f, Black4f,
    nil, 5, 1, 1);}
  statusFont.printStringsBorderedRect(strs, 0,
    Black4Single, Green4Single, Yellow4Single,
    @HalftoneStipple, 5, 1, 1);
 finally strs.Free end;
end;

procedure BeforeDraw(glwin: TGLWindow);
begin
  PrepareRender(false);
end;

procedure Draw(glwin: TGLWindow);

  procedure DisplayOctreeDepth(octreenode: TOctreeNode; octreeDisplayDepth: integer);
  var
    b0, b1, b2: boolean;
  begin
    with octreenode do
      if Depth = octreeDisplayDepth then
      begin
        if not (IsLeaf and (ItemsCount = 0)) then
          glDrawBox3dWire(Box);
      end else
      if not IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeDepth(TreeSubNodes[b0, b1, b2], octreeDisplayDepth);
      end;
  end;

  procedure DisplayOctreeWhole(OctreeNode: TOctreeNode);
  var
    b0, b1, b2: boolean;
  begin
    with OctreeNode do
    begin
      if not (IsLeaf and (ItemsCount = 0)) then
        glDrawBox3dWire(Box);

      if not IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeWhole(TreeSubNodes[b0, b1, b2]);
      end;
    end;
  end;

  procedure DrawFrustum(AlwaysVisible: boolean);
  var FrustumPoints: TFrustumPointsDouble;
  begin
   if AlwaysVisible then
   begin
    glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
   end;
   try
    CalculateFrustumPoints(FrustumPoints, MatrixWalker.Frustum);
    glColor3f(1, 1, 1);
    glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_DOUBLE, 0, @FrustumPoints);
      glDrawElements(GL_LINES,
        SizeOf(FrustumPointsLinesIndexes) div SizeOf(LongWord),
        GL_UNSIGNED_INT, @FrustumPointsLinesIndexes);
    glDisableClientState(GL_VERTEX_ARRAY);
   finally
    if AlwaysVisible then glPopAttrib;
   end;
  end;

var
  Scene: TVRMLFlatSceneGL;
begin
 glClear(GL_DEPTH_BUFFER_BIT);

 Scene := SceneAnimation.SceneFromTime(AnimationTime);

 if Scene.Background <> nil then
 begin
  { jezeli Wireframe i mamy Backgr to rysujemy Backgr w trybie wireframe (czyszczac
    poprzednio teren przez glClear(GL_COLOR_BUFFER_BIT)) - nawet jezeli nie jest
    zdefiniowane czy Backgr.Render nie bedzie takze uzywalo glClear do wyczyszczenia
    ekranu. W rezultacie wynik renderowania jest nieco niezdefiniowany (zalezy
    od tego jak bedzie dzialac Backgr.Render - czy ekran bedzie pokazywal szkielet
    jakiejs kuli i za nim kolor BGColor czy tez moze bedzie wypelniony kolorem
    Backgr.FdSkyColors[0]) ale to nic - to co widac w trybie wireframe nigdy nie mialo
    byc jasno zdefiniowane i zawsze bylo w tym widac sporo implementation-specific
    rzeczy.

    Nie zdecydowalem sie na prostsze rozwiazanie (i dajace lepiej zdefiniowany
    rezultat renderowania) aby zignorowac Wireframe przy rysowaniu Backgr.
    Zrobilem to dlatego ze ogladanie nieba w trybie wireframe jest pomocne -
    czasem wygodnie jest wiedziec gdzie przebiegaja krawedzie szescianu nieba
    aby zobaczyc czy niebo tam dobrze pasuje. Jest to tez pomocne mi abym mogl
    sprawdzic czy BackgroundGL dziala dobrze.
  }
  if Wireframe then
  begin
   glClear(GL_COLOR_BUFFER_BIT);
   glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  end;
  try
   glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
   Scene.Background.Render;
  finally
   if Wireframe then glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
 end else
  glClear(GL_COLOR_BUFFER_BIT);

 { Set properties of headlight. Actual enabled state of headlight will be
   set later by BeginRenderSceneWithLights. }
 if SceneHeadlight <> nil then
   SceneHeadlight.Render(0, false);

 glLoadMatrix(Glw.Navigator.Matrix);

 { Draw may be called in various situations even when Scene
   is not really ready (e.g. when showing errors after scene loading),
   so we have to check here whether some things are initialized as they
   should. }
 if (SceneTriangleOctree <> nil) and
    (SceneTriangleOctree.TreeRoot <> nil) then
 if OctreeDisplayWhole then
 begin
   glColorv(Yellow3Single);
   DisplayOctreeWhole(SceneTriangleOctree.TreeRoot);
 end else
 if OctreeDisplayDepth >= 0 then
 begin
   glColorv(Yellow3Single);
   DisplayOctreeDepth(SceneTriangleOctree.TreeRoot, OctreeDisplayDepth);
 end;

 if showBBox then
 begin
   { Display current bounding box only if there's a chance that it's
     different than whole BoundingBoxSum --- this requires that animation
     has at least two frames. }
   if SceneAnimation.ScenesCount > 1 then
   begin
     glColorv(Red3Single);
     if not IsEmptyBox3d(Scene.BoundingBox) then
       glDrawBox3dWire(Scene.BoundingBox);
   end;

   glColorv(Green3Single);
   if not IsEmptyBox3d(SceneAnimation.BoundingBoxSum) then
     glDrawBox3dWire(SceneAnimation.BoundingBoxSum);
 end;

 if Wireframe then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
 try
   BeginRenderSceneWithLights;

     if (Glw.Navigator is TMatrixWalker) then
     begin
       { For animations, DefaultShapeStateOctree is constructed only
         for the 1st scene. For the rest, we have to call normal
         RenderFrustum. }
       if Scene.DefaultShapeStateOctree <> nil then
         Scene.RenderFrustumOctree(Glw.NavWalker.Frustum, tgAll) else
         Scene.RenderFrustum(Glw.NavWalker.Frustum, tgAll);
     end else
       Scene.Render(nil, tgAll);

     LastRender_RenderedShapeStatesCount :=
       Scene.LastRender_RenderedShapeStatesCount;
     LastRender_AllShapeStatesCount :=
       Scene.LastRender_AllShapeStatesCount;

   EndRenderSceneWithLights;
 finally
   if Wireframe then glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
 end;

 { Note that there is no sense in showing viewing frustum in
   NavigatorKind <> nkExaminer, since viewing frustum should
   be never visible then (or should be just at the exact borders
   or visibility, so it's actually unspecified whether OpenGL would
   show it or not). }
 if ShowFrustum and (NavigatorKind = nkExaminer) then
  DrawFrustum(ShowFrustumAlwaysVisible);

 if SelectedItem <> nil then
 begin
   glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_POINT_BIT);
     glDisable(GL_DEPTH_TEST); { saved by GL_ENABLE_BIT }
     glColorv(White3Single);

     glLineWidth(3.0); { saved by GL_LINE_BIT }
     glBegin(GL_LINE_LOOP);
       glVertexv(SelectedItem^.Triangle[0]);
       glVertexv(SelectedItem^.Triangle[1]);
       glVertexv(SelectedItem^.Triangle[2]);
     glEnd;

     glPointSize(5.0); { saved by GL_POINT_BIT }
     glBegin(GL_POINTS);
       glVertexv(SelectedPoint);
     glEnd;
   glPopAttrib;
 end;

 if ShowStatus then
 begin
  { Note that DrawStatus changes current modelview matrix,
    so you want to always leave drawing status at the end of this Draw
    procedure. }
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glProjectionPushPopOrtho2D(@DrawStatus, nil, 0, Glwin.Width, 0, Glwin.Height);
  glPopAttrib;
 end;
end;

procedure Resize(glwin: TGLWindow);

  procedure UpdateNavigatorProjectionMatrix;
  var ProjectionMatrix: TMatrix4f;
  begin
   glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
   MatrixWalker.ProjectionMatrix := ProjectionMatrix;
  end;

var
  MaxSize, zNear, zFar: TGLdouble;
  FieldOfView: Single;
  Box: TBox3d;
begin
 glViewport(0, 0, glwin.Width, glwin.Height);

 Box := SceneAnimation.BoundingBoxSum;

 { Calculate AngleOfViewX, basing on ViewpointNode and Glw.Width,Height }
 if (ViewpointNode <> nil) and (ViewpointNode is TNodeViewpoint) then
   FieldOfView := TNodeViewpoint(ViewpointNode).FdFieldOfView.Value else
   FieldOfView := Pi / 4 { VRML Viewpoint.fieldOfView default value };
 AngleOfViewX := RadToDeg(TNodeViewpoint.ViewpointAngleOfView(
   FieldOfView, Glw.Width / Glw.Height));
 { Tests:
   Writeln(Format('Angle of view: x %f, y %f', [AngleOfViewX, AngleOfViewY])); }

 { Zeby zminimalizowac bledy depth buffera trzeba uzaleznic zNear/zFar
   od wielkosci obiektu.

   W trybie Examiner mozna uzywac duzo wiekszych near,
   uzywam tu Box3dAvgSize(Box) * 0.1 i to daje dobra dokladnosc,
   w Walk uzywam cameraRadius * 0.6 co standardowo
   daje Box3dAvgSize(Box)*0.006 a wiec prawie 20 razy mniej !
   Przy tak malym perspective near w trybie Examiner czesto byloby widac
   bledy z-bufora, patrz np. KingsHead.wrl. }
 if (Glw.Navigator is TMatrixExaminer) and
    (not IsEmptyBox3d(Box)) then
  zNear := Box3dAvgSize(Box) * 0.1 else
  zNear := WalkProjectionNear;
 zFar := WalkProjectionFar;

 if CameraKind = ckPerspective then
  ProjectionGLPerspective(AngleOfViewY, glwin.Width/glwin.Height,
    zNear, zFar) else
 begin
  if IsEmptyBox3d(Box) then
   MaxSize := 1.0 { any dummy value } else
   MaxSize := Box3dMaxSize(Box);
  ProjectionGLOrtho(-MaxSize/2, MaxSize/2,
                    -MaxSize/2, MaxSize/2,
                    zNear, zFar);
 end;

 UpdateNavigatorProjectionMatrix;
end;

const
  SOnlyInWalker = 'You must be in ''Walk'' navigation mode '+
    'to use this function.';

procedure MouseDown(glwin: TGLWindow; btn: TMouseButton);
var
  Ray0, RayVector: TVector3Single;
begin
  if btn = mbLeft then
  begin
    if not (Glw.Navigator is TMatrixWalker) then
    begin
      MessageOK(glwin, SOnlyInWalker);
      Exit;
    end;

    Glw.MousePickedRay(AngleOfViewX, AngleOfViewY, Ray0, RayVector);

    SceneTriangleOctree.DirectCollisionTestsCounter := 0;

    SelectedItemIndex :=
      SceneTriangleOctree.RayCollision(
        SelectedPoint, Ray0, RayVector, true, NoItemIndex, false, nil);

    { DirectCollisionTestsCounter is not recorded,
      so I may write it now on console in case it will be useful.
      For now it's commented out --- not interesting to typical user.
    Writeln(Format('%d tests for collisions between ray ' +
      'and triangles were needed to learn this.',
      [ SceneTriangleOctree.DirectCollisionTestsCounter ])); }

    if SelectedItemIndex = NoItemIndex then
      SelectedItem := nil else
      SelectedItem := SceneTriangleOctree.OctreeItems.
        Pointers[SelectedItemIndex];

    UpdateSelectedEnabled;

    Glw.PostRedisplay;
  end;
end;

procedure Idle(glwin: TGLWindow);
var
  OldAnimationTime: TAnimationTime;
begin
  if not AnimationTimePaused then
  begin
    OldAnimationTime := AnimationTime;
    AnimationTime += AnimationTimeSpeed * Glwin.IdleCompSpeed / 50;

    { Call PostRedisplay only if the displayed animation frame actually changed.
      This way, we avoid wasting CPU cycles if the loaded scene is actually
      still, or if the animation stopped running. }
    if SceneAnimation.SceneFromTime(OldAnimationTime) <>
       SceneAnimation.SceneFromTime(AnimationTime) then
      Glwin.PostRedisplay;
  end;
end;

{ TMatrixWalker collision detection using SceneTriangleOctree --------------- }

type
  THelper = class
    class function MoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    class procedure GetCameraHeight(Navigator: TMatrixWalker;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

    class procedure OpenRecent(const FileName: string);
  end;

class function THelper.MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if CollisionCheck then
  begin
    SceneTriangleOctree.DirectCollisionTestsCounter := 0;
    Result := SceneTriangleOctree.MoveAllowed(
      Navigator.CameraPos, ProposedNewPos, NewPos, CameraRadius,
      NoItemIndex, nil);
    {tests:
     Writeln('Octree efficiency : ',SceneTriangleOctree.DirectCollisionTestsCounter,
       ' direct tests with single triangles were needed to know that move is'
       +BoolToStrNNN[result]+'allowed');
    }
  end else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;

  if Result and BecauseOfGravity then
    { TODO: instead of setting Result to false, this should
      actually move NewPos so that it's *exactly* on the border
      of bounding box. }
    Result := Box3dPointInside(NewPos, SceneAnimation.BoundingBoxSum);
end;

class procedure THelper.GetCameraHeight(Navigator: TMatrixWalker;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
var
  GroundItemIndex: Integer;
begin
  SceneTriangleOctree.GetCameraHeight(
    TMatrixWalker(Navigator).CameraPos,
    TMatrixWalker(Navigator).GravityUp,
    IsAboveTheGround, SqrHeightAboveTheGround, GroundItemIndex,
    NoItemIndex, nil);
end;

{ Setting viewpoint ---------------------------------------------------------- }

{ This does what all SetViewpointXxx should do, except that you
  have to set ViewpointNode and CameraKind before calling this.
  Note that the length of InitialCameraDir doesn't matter. }
procedure SetViewpointCore(
  const InitialCameraPos: TVector3Single;
  InitialCameraDir: TVector3Single;
  const InitialCameraUp: TVector3Single;
  const GravityUp: TVector3Single);
begin
  { zmien dlugosc InitialCameraDir,
    na podstawie CameraRadius i NavigationNode.FdSpeed }
  InitialCameraDir := VectorAdjustToLength(InitialCameraDir, CameraRadius * 0.4);
  if (NavigationNode <> nil) and (NavigationNode.FdSpeed.Value <> 0.0) then
    VectorScaleTo1st(InitialCameraDir, NavigationNode.FdSpeed.Value);

  MatrixWalker.Init(InitialCameraPos, InitialCameraDir, InitialCameraUp,
    GravityUp, MatrixWalker.CameraPreferredHeight, CameraRadius);

  if not Glw.Closed then
  begin
    Glw.EventResize;
    Glw.PostRedisplay;
  end;
end;

{ This sets perspective viewpoint that doesn't come from
  any VRML viewpoint node. }
procedure SetViewpoint(
  const InitialCameraPos, InitialCameraDir, InitialCameraUp,
    GravityUp: TVector3Single); overload;
begin
  ViewpointNode := nil;
  CameraKind := ckPerspective;
  SetViewpointCore(InitialCameraPos, InitialCameraDir, InitialCameraUp, GravityUp);
end;

{ This jumps to 1st viewpoint on ViewpointsList
  (or to the default VRML cam setting if no viewpoints in the list).

  Sets CameraKind.
  Uses CameraRadius, NavigationNode, so make sure these are already
  set as needed }
procedure SetViewpoint(Index: Integer); overload;
var
  InitialCameraPos: TVector3Single;
  InitialCameraDir: TVector3Single;
  InitialCameraUp: TVector3Single;
  GravityUp: TVector3Single;
begin
  ViewpointNode := ViewpointsList.GetViewpoint(Index, CameraKind,
    InitialCameraPos, InitialCameraDir, InitialCameraUp, GravityUp);
  SetViewpointCore(InitialCameraPos, InitialCameraDir, InitialCameraUp, GravityUp);
end;

{ Scene operations ---------------------------------------------------------- }

const
  V3DSDefTriangleOctreeMaxDepth = 10;
  V3DSDefTriangleOctreeMaxLeafItemsCount = 64;

var
  { These are parameters for Load*Scene* telling how to construct octree.
    For description of their meaning, see VRMLOctree. }
  TriangleOctreeMaxDepth: Integer = V3DSDefTriangleOctreeMaxDepth;
  TriangleOctreeMaxLeafItemsCount: integer =
    V3DSDefTriangleOctreeMaxLeafItemsCount;

  ShapeStateOctreeMaxDepth: Integer = DefShapeStateOctreeMaxDepth;
  ShapeStateOctreeMaxLeafItemsCount: Integer =
    DefShapeStateOctreeMaxLeafItemsCount;

  { This is set to non-nil by CreateMainMenu.
    It is used there and it is also used from LoadSceneCore, since
    loading a scene may change value of HeadLight so we have to update
    MenuHeadlight.Checked state.
    Same for MenuGravity. }
  MenuHeadlight: TMenuItemChecked;
  MenuGravity: TMenuItemChecked;
  MenuPreferGravityUpForRotations: TMenuItemChecked;
  MenuPreferGravityUpForMoving: TMenuItemChecked;
  MenuReopen: TMenuItem;

procedure VRMLNonFatalError_Warning(const s: string);
begin
  Writeln(ProgramName + ': WARNING: ' + S);
  SceneWarnings.Append(S);
end;

procedure SceneOctreeCreate;
begin
  { Beware: constructing octrees will cause progress drawing,
    and progress drawing may cause FlushRedisplay,
    and FlushRedisplay may cause OnDraw and OnBeforeDraw to be called. }
  Glw.OnDraw := nil;
  Glw.OnBeforeDraw := nil;
  try
    { For now we construct and store octrees only for the 1st animation frame. }
    SceneAnimation.FirstScene.OwnsDefaultTriangleOctree := false;
    SceneAnimation.FirstScene.DefaultTriangleOctree :=
      SceneAnimation.FirstScene.CreateTriangleOctree(
        TriangleOctreeMaxDepth, TriangleOctreeMaxLeafItemsCount,
        'Building triangle octree');

    SceneAnimation.FirstScene.OwnsDefaultShapeStateOctree := false;
    SceneAnimation.FirstScene.DefaultShapeStateOctree :=
      SceneAnimation.FirstScene.CreateShapeStateOctree(
        ShapeStateOctreeMaxDepth, ShapeStateOctreeMaxLeafItemsCount,
        'Building ShapeState octree');
  finally
    Glw.OnDraw := @Draw;
    Glw.OnBeforeDraw := @BeforeDraw;
  end;
end;

procedure SceneOctreeFree;
begin
  if SceneAnimation.ScenesCount <> 0 then
  begin
    SceneAnimation.FirstScene.DefaultTriangleOctree.Free;
    SceneAnimation.FirstScene.DefaultTriangleOctree := nil;

    SceneAnimation.FirstScene.DefaultShapeStateOctree.Free;
    SceneAnimation.FirstScene.DefaultShapeStateOctree := nil;
  end;
end;

procedure Unselect;
begin
  SelectedItem := nil;
  SelectedItemIndex := NoItemIndex;
  UpdateSelectedEnabled;
end;

{ Frees (and sets to some null values) "scene global variables".

  Note about OpenGL context: remember that changing calling Close
  on SceneAnimation also calls CloseGL  that closes all connections
  of Scene to OpenGL context. This means that:
  1) SceneAnimation must not be Loaded, or Scene must not have any
     connections with OpenGL context (like e.g. after calling
     SceneAnimation.CloseGL)
  2) or you must call FreeScene in the same OpenGL context that the
     SceneAnimation is connected to. }
procedure FreeScene;
begin
  SceneOctreeFree;

  SceneAnimation.Close;

  ViewpointsList.Count := 0;
  if ViewpointsList.MenuJumpToViewpoint <> nil then
    ViewpointsList.MakeMenuJumpToViewpoint;

  NavigationNode := nil;
  ViewpointNode := nil;

  SceneFileName := '';

  if MenuReopen <> nil then
    MenuReopen.Enabled := false;

  Unselect;

  FreeAndNil(SceneHeadlight);
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
  (ASceneFileName is a pointer) as local vars. }
procedure LoadSceneCore(
  RootNodes: TVRMLNodesList;
  ATimes: TDynSingleArray;
  ScenesPerTime: Cardinal;
  AOptimization: TGLRendererOptimization;
  const EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;

  ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  JumpToInitialViewpoint: boolean);

  procedure ScaleAll(A: TDynSingleArray; const Value: Single);
  var
    I: Integer;
  begin
    for I := 0 to A.High do
      A.Items[I] *= Value;
  end;

var
  NewCaption: string;
  CameraPreferredHeight: Single;
  WorldInfoNode: TNodeWorldInfo;
  I: Integer;
  SavedCameraPos, SavedCameraDir, SavedCameraUp, SavedGravityUp: TVector3Single;
begin
  FreeScene;

  try
    SceneFileName := ASceneFileName;

    if AnimationTimeSpeedWhenLoading <> 1.0 then
      ScaleAll(ATimes, 1 / AnimationTimeSpeedWhenLoading);

    SceneAnimation.Load(RootNodes, true, ATimes,
      ScenesPerTime, AOptimization, EqualityEpsilon);
    SceneAnimation.TimeLoop := TimeLoop;
    SceneAnimation.TimeBackwards := TimeBackwards;

    AnimationTime := SceneAnimation.TimeBegin;

    NavigationNode := SceneAnimation.FirstScene.RootNode.TryFindNode(
      TNodeNavigationInfo, true) as TNodeNavigationInfo;

    ChangeSceneAnimation(SceneChanges, SceneAnimation);

    { calculate CameraRadius }
    CameraRadius := ACameraRadius;
    if CameraRadius = 0.0 then
    begin
      if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
        CameraRadius := NavigationNode.FdAvatarSize.Items[0];
      if CameraRadius = 0.0 then
        if IsEmptyBox3d(SceneAnimation.BoundingBoxSum) then
          CameraRadius := 1.0 { any non-zero dummy value } else
          CameraRadius := Box3dAvgSize(SceneAnimation.BoundingBoxSum) * 0.01;
    end;

    { calculate CameraPreferredHeight }
    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      CameraPreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      CameraPreferredHeight := CameraRadius * 2;

    { calculate VisibilityLimit }
    if NavigationNode <> nil then
      VisibilityLimit := NavigationNode.FdVisibilityLimit.Value else
      VisibilityLimit := 0;

    if not JumpToInitialViewpoint then
    begin
      SavedCameraPos := MatrixWalker.CameraPos;
      SavedCameraDir := MatrixWalker.CameraDir;
      SavedCameraUp := MatrixWalker.CameraUp;
      SavedGravityUp := MatrixWalker.GravityUp;
    end;

    SceneInitMultiNavigators(SceneAnimation.BoundingBoxSum,
      StdVRMLCamPos[1], StdVRMLCamDir, StdVRMLCamUp, StdVRMLGravityUp,
      CameraPreferredHeight, CameraRadius);

    { calculate ViewpointsList, MenuJumpToViewpoint,
      and jump to 1st viewpoint (or to the default cam settings). }
    SceneAnimation.FirstScene.EnumerateViewpoints(
      @ViewpointsList.AddNodeTransform);
    if ViewpointsList.MenuJumpToViewpoint <> nil then
      ViewpointsList.MakeMenuJumpToViewpoint;
    SetViewpoint(0);

    if not JumpToInitialViewpoint then
    begin
      MatrixWalker.CameraPos := SavedCameraPos;
      MatrixWalker.CameraDir := SavedCameraDir;
      MatrixWalker.CameraUp := SavedCameraUp;
      MatrixWalker.GravityUp := SavedGravityUp;
    end;

    SceneInitLights(SceneAnimation, NavigationNode);
    SceneHeadlight := SceneAnimation.FirstScene.CreateHeadLight;

    { SceneInitLights could change HeadLight value.
      So update MenuHeadlight.Checked now. }
    if MenuHeadlight <> nil then
      MenuHeadlight.Checked := HeadLight;

    WorldInfoNode := SceneAnimation.FirstScene.RootNode.TryFindNode(
      TNodeWorldInfo, true)
      as TNodeWorldInfo;
    if (WorldInfoNode <> nil) and (WorldInfoNode.FdTitle.Value <> '') then
      NewCaption := SForCaption(WorldInfoNode.FdTitle.Value) else
      NewCaption := ExtractFileName(SceneFilename);
    NewCaption += ' - view3dscene';
    if Glw.Closed then
      Glw.Caption := NewCaption else
      Glw.FPSBaseCaption := NewCaption;

    { calculate Scene.BackgroundSkySphereRadius (musi byc najpierw ustalone
      Scene.BoundingBox i CameraRadius, sa potrzebne do WalkProjectionNear/Far) }
    SceneAnimation.BackgroundSkySphereRadius :=
      TBackgroundGL.NearFarToSkySphereRadius(
        WalkProjectionNear, WalkProjectionFar);

    { Find recognized NavigationNode.FdType }
    if NavigationNode <> nil then
      for I := 0 to NavigationNode.FdType.Count - 1 do
        if NavigationNode.FdType.Items[I] = 'WALK' then
        begin
          SetNavigatorKind(Glw, nkWalker);
          MatrixWalker.PreferGravityUpForRotations := true;
          MatrixWalker.PreferGravityUpForMoving := true;
          MatrixWalker.Gravity := true;
          Break;
        end else
        if NavigationNode.FdType.Items[I] = 'FLY' then
        begin
          SetNavigatorKind(Glw, nkWalker);
          MatrixWalker.PreferGravityUpForRotations := true;
          MatrixWalker.PreferGravityUpForMoving := false;
          MatrixWalker.Gravity := false;
          Break;
        end else
        if NavigationNode.FdType.Items[I] = 'EXAMINE' then
        begin
          SetNavigatorKind(Glw, nkExaminer);
          Break;
        end;

    if MenuGravity <> nil then
      MenuGravity.Checked := MatrixWalker.Gravity;
    if MenuPreferGravityUpForRotations <> nil then
      MenuPreferGravityUpForRotations.Checked := MatrixWalker.PreferGravityUpForRotations;
    if MenuPreferGravityUpForMoving <> nil then
      MenuPreferGravityUpForMoving.Checked := MatrixWalker.PreferGravityUpForMoving;

    SceneOctreeCreate;

    if not Glw.Closed then
    begin
      { call EventResize to adjust zNear/zFar of our projection to the size
        of Scene.BoundingBox }
      Glw.EventResize;
      Glw.PostRedisplay;
    end;

    if MenuReopen <> nil then
      MenuReopen.Enabled := SceneFileName <> '';

  except
    FreeScene;
    raise;
  end;
end;

{ This loads the scene from file (using LoadAsVRMLSequence) and
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
  JumpToInitialViewpoint: boolean);

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
  Optimization: TGLRendererOptimization;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;

  SavedSceneWarnings: TStringList;
begin
  RootNodes := TVRMLNodesList.Create;
  Times := TDynSingleArray.Create;
  try
    { TODO: Show to user that Optimization for kanim is from kanim file,
      not current setting of RendererOptimization ? }
    Optimization := RendererOptimization;

    { We have to clear SceneWarnings here (not later)
      to catch also all warnings raised during parsing the VRML file.
      This causes a potential problem: if loading the scene will fail,
      we should restore the old warnings (if the old scene will be
      preserved) or clear them (if the clear scene will be loaded
      --- LoadSceneClear will clear them). }
    SavedSceneWarnings := TStringList.Create;
    try
      SavedSceneWarnings.Assign(SceneWarnings);
      SceneWarnings.Clear;

      {$ifdef CATCH_EXCEPTIONS}
      try
      {$endif CATCH_EXCEPTIONS}
        LoadAsVRMLSequence(ASceneFileName, true,
          RootNodes, Times,
          ScenesPerTime, Optimization, EqualityEpsilon,
          TimeLoop, TimeBackwards);
      {$ifdef CATCH_EXCEPTIONS}
      except
        on E: Exception do
        begin
          MessageOK(glw, 'Error while loading scene from "' +ASceneFileName+ '": ' +
            E.Message, taLeft);
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
        ScenesPerTime, Optimization, EqualityEpsilon,
        TimeLoop, TimeBackwards,
        ASceneFileName, SceneChanges, ACameraRadius, JumpToInitialViewpoint);
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
        MessageOK(glw, 'Error while loading scene from "' + ASceneFileName + '": ' +
          E.Message, taLeft);
        Exit;
      end;
    end;
    {$endif CATCH_EXCEPTIONS}

    RecentMenu.Add(ASceneFileName);

    { We call PrepareRender to make SceneAnimation.PrepareRender to gather
      VRML warnings (because some warnings, e.g. invalid texture filename,
      are reported only from SceneAnimation.PrepareRender).
      Also, this allows us to show first PrepareRender with progress bar. }
    PrepareRender(true);
    if SceneWarnings.Count <> 0 then
      MessageOK(Glw, Format('Note that there were %d warnings while loading ' +
        'this scene. See the console or use File->"View warnings" ' +
        'menu command to view them all.', [SceneWarnings.Count]), taLeft);
  finally
    FreeAndNil(RootNodes);
    FreeAndNil(Times);
  end;
end;

{ This should be used to load special "clear" and "welcome" scenes.
  This loads a scene directly from TVRMLNode, and assumes that
  LoadSceneCore will not fail. }
procedure LoadSimpleScene(Node: TVRMLNode);
var
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  ScenesPerTime: Cardinal;
  Optimization: TGLRendererOptimization;
  EqualityEpsilon: Single;
  TimeLoop, TimeBackwards: boolean;
begin
  RootNodes := TVRMLNodesList.Create;
  Times := TDynSingleArray.Create;
  try
    Optimization := RendererOptimization;

    RootNodes.Add(Node);
    Times.AppendItem(0);

    ScenesPerTime := 1;      { doesn't matter }
    EqualityEpsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }

    SceneWarnings.Clear;
    LoadSceneCore(
      RootNodes, Times,
      ScenesPerTime, Optimization, EqualityEpsilon,
      TimeLoop, TimeBackwards,
      '', [], 1.0, true);
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
  LoadSimpleScene(ParseVRMLFileFromString({$I clear_scene.inc}, ''));
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
begin
  LoadSimpleScene(ParseVRMLFileFromString({$I welcome_scene.inc}, ''));
end;

function SavedVRMLPrecedingComment(const SourceFileName: string): string;
begin
 Result := 'VRML generated by view3dscene from ' +SourceFileName +
   ' on ' +DateTimeToAtStr(Now);
end;

{ Load model from ASceneFileName ('-' means stdin),
  do SceneChanges, and write it as VRML to stdout.
  This is simply the function to handle --write-to-vrml command-line option. }
procedure WriteToVRML(const ASceneFileName: string;
  const SceneChanges: TSceneChanges);
var Scene: TVRMLFlatScene;
begin
 Scene := TVRMLFlatScene.Create(LoadAsVRML(ASceneFileName, true), true);
 try
  ChangeScene(SceneChanges, Scene);
  SaveToVRMLFile(Scene.RootNode, StdOutStream,
    SavedVRMLPrecedingComment(ASceneFileName));
 finally Scene.Free end;
end;

class procedure THelper.OpenRecent(const FileName: string);
begin
  LoadScene(FileName, [], 0.0, true);
end;

{ menu things ------------------------------------------------------------ }

const
  Version = '2.2.1';
  DisplayProgramName = 'view3dscene';

var
  MenuOctreeDisplayWhole: TMenuItemChecked;

type
  THelperSpecialCastleNodes = class
    procedure Remove(ParentNode, Node: TVRMLNode; var RemoveNode: boolean);
  end;

procedure THelperSpecialCastleNodes.Remove(
  ParentNode, Node: TVRMLNode; var RemoveNode: boolean);
begin
  RemoveNode :=
    (Node.NodeName = 'LevelBox') or
    (Node.NodeName = 'WaterBox') or
    IsPrefix('Crea', Node.NodeName) or
    IsPrefix('Item', Node.NodeName) or
    IsPrefix('Waypoint', Node.NodeName) or
    IsPrefix('Sector', Node.NodeName) or
    { Actually below are special only on specific levels. Oh, well... there's
      no way to solve it in view3dscene (and I obviously don't want to
      make view3dscene depend on any "The Castle" units --- view3dscene
      is completely independent). }
    (Node.NodeName = 'LevelExitBox') or
    IsPrefix('WerewolfAppear_', Node.NodeName) or
    (Node.NodeName = 'GateExitBox') or
    (Node.NodeName = 'Teleport1Box') or
    (Node.NodeName = 'Teleport2Box') or
    (Node.NodeName = 'SacrilegeBox') or
    IsPrefix('SacrilegeGhost_', Node.NodeName) or
    IsPrefix('SwordGhost_', Node.NodeName) or
    (Node.NodeName = 'Elevator49DownBox') or
    (Node.NodeName = 'Elev9a9bPickBox');
end;

procedure MenuCommand(glwin: TGLWindow; MenuItem: TMenuItem);

  procedure ChangeGravityUp;
  var Answer: string;
      NewUp: TVector3Single;
  begin
   if Glw.Navigator is TMatrixWalker then
   begin
    Answer := '';
    if MessageInputQuery(glwin,
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
       MessageOK(glwin, 'Incorrect vector value : '+E.Message);
       Exit;
      end;
     end;

     Glw.NavWalker.GravityUp := NewUp;
     Glw.PostRedisplay;
    end;
   end else
    MessageOK(glwin, SOnlyInWalker);
  end;

  procedure ShowAndWrite(const S: string);
  begin
    Writeln(S);
    MessageOK(Glw, S, taLeft);
  end;

  procedure ViewSceneWarnings;
  var
    S: TStringList;
  begin
    S := TStringList.Create;
    try
      S.Append(Format('Total %d warnings about current scene "%s":',
        [ SceneWarnings.Count, SceneFileName ]));
      S.Append('');
      S.AddStrings(SceneWarnings);
      MessageOK(Glw, S, taLeft);
    finally FreeAndNil(S) end;
  end;

  procedure ChangePointSize;
  var
    Value: Single;
  begin
    Value := SceneAnimation.Attributes.PointSize;
    if MessageInputQuery(Glwin, 'Change point size:',
      Value, taLeft) then
      SceneAnimation.Attributes.PointSize := Value;
  end;

  procedure ChangeAnimationTimeSpeed;
  begin
    MessageInputQuery(Glwin,
      'Time pass speed 1.0 means that 1 time unit is 1 second.' +nl+
      '0.5 makes time pass two times slower,' +nl+
      '2.0 makes it pass two times faster etc.' +nl+
      nl+
      'Note that this is only "on display" time speed. Which means ' +
      'that internally number of precalculated animation frames ' +
      'doesn''t change. Which means that slowing this time pass too much ' +
      'leads to noticeably "jagged" animations.' +nl+
      nl+
      'New "on display" time pass speed:',
      AnimationTimeSpeed, taLeft);
  end;

  procedure ChangeAnimationTimeSpeedWhenLoading;
  begin
    MessageInputQuery(Glwin,
      'Time pass speed 1.0 means that 1 time unit is 1 second.' +nl+
      '0.5 makes time pass two times slower,' +nl+
      '2.0 makes it pass two times faster etc.' +nl+
      nl+
      'Note that this is the "on loading" time speed. Which means ' +
      'that it''s only applied when loading animation from file ' +
      '(you can use "File -> Reopen" command to apply this to currently ' +
      'loaded animation). Moreover, changing this actually changes ' +
      'the density of precalculated animation frames. Which means that ' +
      'this is the more resource-consuming, but also better ' +
      'method of changing animation speed: even if you slow down ' +
      'this time pass much, the animation should not become ' +
      'more jagged.' +nl+
      nl+
      'New "on loading" time pass speed:',
      AnimationTimeSpeedWhenLoading, taLeft);
  end;

  function NodeNiceName(node: TVRMLNode): string;
  begin
   result := ''''+node.NodeName+''' (class '''+node.NodeTypeName+''')';
  end;

  procedure SelectedShowInformation;
  var
    s, TextureDescription: string;
    VCOver, TCOver, VCNotOver, TCNotOver: Cardinal;
    M1: TNodeMaterial_1;
    M2: TNodeMaterial_2;
    SelectedShape: TNodeGeneralShape;
  begin
    if SelectedItemIndex = NoItemIndex then
    begin
      s := 'Nothing selected.';
    end else
    begin
      SelectedShape := SelectedItem^.ShapeNode;
      s := Format(
           'Selected point %s from triangle %s.' +nl+
           nl+
           'This triangle is part of the '+
           'node named %s. Node''s bounding box is %s. ',
           [VectorToNiceStr(SelectedPoint),
            TriangleToNiceStr(SelectedItem^.Triangle),
            NodeNiceName(SelectedShape),
            Box3dToNiceStr(SelectedShape.BoundingBox(SelectedItem^.State))]);

      if (SelectedItem^.FaceCoordIndexBegin <> -1) and
         (SelectedItem^.FaceCoordIndexEnd <> -1) then
      begin
        S += Format('Face containing the selected triangle spans from %d to' +
          ' %d coordIndex entries. ',
          [ SelectedItem^.FaceCoordIndexBegin,
            SelectedItem^.FaceCoordIndexEnd ]);
      end;

      VCNotOver := SelectedShape.VerticesCount(SelectedItem^.State, false);
      TCNotOver := SelectedShape.TrianglesCount(SelectedItem^.State, false);
      VCOver := SelectedShape.VerticesCount(SelectedItem^.State, true);
      TCOver := SelectedShape.TrianglesCount(SelectedItem^.State, true);

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

      if SelectedItem^.State.Texture = nil then
        TextureDescription := 'none' else
        TextureDescription := SelectedItem^.State.Texture.TextureDescription;
      S += Format(nl +nl+
            'Node''s texture : %s.', [TextureDescription]);

      S += nl+ nl;
      if SelectedItem^.State.ParentShape <> nil then
      begin
        { This is VRML 2.0 node }
        M2 := SelectedItem^.State.ParentShape.Material;
        if M2 <> nil then
        begin
          S += Format(
                 'Material:' +nl+
                 '  name : %s' +nl+
                 '  ambientIntensity[0] : %s' +nl+
                 '  diffuseColor[0] : %s' +nl+
                 '  specular[0] : %s' +nl+
                 '  shininess[0] : %s' +nl+
                 '  transparency[0] : %s',
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
            'Material:' +nl+
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
    ShadowingItem: POctreeItem;
    ShadowingItemIndex: Integer;
    S: string;
  begin
    if SelectedItemIndex = NoItemIndex then
    begin
      s := 'Nothing selected.';
    end else
    begin
      S := Format('Total %d lights active for selected object.',
        [SelectedItem^.State.ActiveLights.Count]);

      for i := 0 to SelectedItem^.State.ActiveLights.Count - 1 do
      begin
       s += nl+ nl + Format('Light %d (node %s) possibly affects selected point ... ',
         [ I, NodeNiceName(SelectedItem^.State.ActiveLights.Items[i].LightNode) ]);

       ShadowingItemIndex := SceneTriangleOctree.SegmentCollision(
         SelectedPoint, SelectedItem^.State.ActiveLights.Items[i].TransfLocation,
           false, SelectedItemIndex, true, nil);

       if ShadowingItemIndex <> NoItemIndex then
       begin
        ShadowingItem := SceneTriangleOctree.OctreeItems.
          Pointers[ShadowingItemIndex];
        s += Format('but no, this light is blocked by triangle %s from node %s.',
          [ TriangleToNiceStr(ShadowingItem^.Triangle),
            NodeNiceName(ShadowingItem^.ShapeNode) ])
       end else
        s += 'hmm, yes ! No object blocks this light here.';
      end;
    end;

    ShowAndWrite(S);
  end;

  procedure WholeSceneChanged;
  begin
    Unselect;
    SceneOctreeFree;
    SceneAnimation.ChangedAll;
    SceneOctreeCreate;
  end;

  procedure RemoveSelectedGeometry;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.ShapeNode, so this is only for the frame where
        octree is available. }
      MessageOK(Glwin, 'This function is not available when you deal with ' +
        'animations.', taLeft);
      Exit;
    end;

    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
    end else
    begin
      SelectedItem^.ShapeNode.FreeRemovingFromAllParents;
      WholeSceneChanged;
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
    ShapeNode: TNodeGeneralShape;
    Colors, Coords, Materials, Normals, TexCoords: TDynLongIntArray;
  begin
    if SceneAnimation.ScenesCount > 1 then
    begin
      { We can't do this for animations, because we use
        SelectedItem^.ShapeNode, so this is only for the frame where
        octree is available. Moreover, we call
        SceneAnimation.FirstScene.ChangedFields. }
      MessageOK(Glwin, 'This function is not available when you deal with ' +
        'animations.', taLeft);
      Exit;
    end;

    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
      Exit;
    end;

    if (SelectedItem^.FaceCoordIndexBegin = -1) or
       (SelectedItem^.FaceCoordIndexEnd = -1) then
    begin
      ShowAndWrite('The selected triangle is not part of IndexedFaceSet ' +
        'or IndexedTriangleMesh nodes.');
      Exit;
    end;

    ShapeNode := SelectedItem^.ShapeNode;

    if ShapeNode is TNodeIndexedFaceSet_1 then
    begin
      Colors := nil;
      Coords := TNodeIndexedFaceSet_1(ShapeNode).FdCoordIndex.Items;
      Materials := TNodeIndexedFaceSet_1(ShapeNode).FdMaterialIndex.Items;
      Normals := TNodeIndexedFaceSet_1(ShapeNode).FdNormalIndex.Items;
      TexCoords := TNodeIndexedFaceSet_1(ShapeNode).FdTextureCoordIndex.Items;
    end else
    if ShapeNode is TNodeIndexedFaceSet_2 then
    begin
      Colors := TNodeIndexedFaceSet_2(ShapeNode).FdColorIndex.Items;
      Coords := TNodeIndexedFaceSet_2(ShapeNode).FdCoordIndex.Items;
      Materials := nil;
      Normals := TNodeIndexedFaceSet_2(ShapeNode).FdNormalIndex.Items;
      TexCoords := TNodeIndexedFaceSet_2(ShapeNode).FdTexCoordIndex.Items;
    end else
    if ShapeNode is TNodeIndexedTriangleMesh_1 then
    begin
      Colors := nil;
      Coords := TNodeIndexedTriangleMesh_1(ShapeNode).FdCoordIndex.Items;
      Materials := TNodeIndexedTriangleMesh_1(ShapeNode).FdMaterialIndex.Items;
      Normals := TNodeIndexedTriangleMesh_1(ShapeNode).FdNormalIndex.Items;
      TexCoords := TNodeIndexedTriangleMesh_1(ShapeNode).FdTextureCoordIndex.Items;
    end else
    begin
      ShowAndWrite('Internal error: cannot get the coordIndex field.');
      Exit;
    end;

    if MFNonEmpty(Colors) or MFNonEmpty(Materials) or MFNonEmpty(Normals) then
    begin
      ShowAndWrite('Removing faces from a geometry node with colorIndex, ' +
        'materialIndex or normalIndex not implemented yet.');
      Exit;
    end;

    Coords.Delete(SelectedItem^.FaceCoordIndexBegin,
      SelectedItem^.FaceCoordIndexEnd -
      SelectedItem^.FaceCoordIndexBegin + 1);

    { Texture coordinates, if not empty, have always (both in VRML 1.0
      and VRML 2.0 IndexedFaceSet nodes, and in IndexedTriangleMesh
      from Inventor) the same ordering as coordIndex.
      So we can remove equivalent texture coords in the same manner
      as we removed coords. }
    if TexCoords <> nil then
      TexCoords.Delete(SelectedItem^.FaceCoordIndexBegin,
        SelectedItem^.FaceCoordIndexEnd -
        SelectedItem^.FaceCoordIndexBegin + 1);

    Unselect;
    SceneOctreeFree;
    SceneAnimation.FirstScene.ChangedFields(ShapeNode);
    SceneOctreeCreate;
  end;

  procedure ChangeLightModelAmbient;
  begin
    if glwin.ColorDialog(LightModelAmbient) then LightModelAmbientChanged;
  end;

  procedure SetViewpointForWholeScene;
  var
    CameraPos, CameraDir, CameraUp, GravityUp: TVector3Single;
  begin
    CameraViewpointForWholeScene(SceneAnimation.BoundingBoxSum,
      CameraPos, CameraDir, CameraUp, GravityUp);
    SetViewpoint(CameraPos, CameraDir, CameraUp, GravityUp);
  end;

  procedure RemoveNodesWithMatchingName;
  var
    Wildcard: string;
    RemovedNumber, RemovedNumberOther: Cardinal;
    I: Integer;
  begin
    Wildcard := '';
    if MessageInputQuery(Glwin,
      'Input node name to be removed. You can use wildcards (* and ?) in ' +
      'the expression below to match many node names. The input is ' +
      'case sensitive (like all VRML).',
      Wildcard, taLeft) then
    begin
      RemovedNumber := SceneAnimation.Scenes[0].RootNode.
        RemoveChildrenWithMatchingName(Wildcard, false);
      for I := 1 to SceneAnimation.ScenesCount - 1 do
      begin
        RemovedNumberOther := SceneAnimation.Scenes[I].RootNode.
          RemoveChildrenWithMatchingName(Wildcard, false);
        Assert(RemovedNumberOther = RemovedNumber);
      end;

      if RemovedNumber <> 0 then
      begin
        { TODO: we should set these to nil only if
          it's actually removed by RemoveChildrenWithMatchingName above. }
        ViewpointNode := nil;
        NavigationNode := nil;

        WholeSceneChanged;
      end;

      MessageOK(Glwin, Format('Removed %d node instances.', [RemovedNumber]),
        taLeft);
    end;
  end;

  procedure RemoveSpecialCastleNodes;
  var
    RemovedNumber, RemovedNumberOther: Cardinal;
    I: Integer;
    HelperSpecialCastleNodes: THelperSpecialCastleNodes;
  begin
    RemovedNumber := SceneAnimation.Scenes[0].RootNode.
      EnumerateRemoveChildren(@HelperSpecialCastleNodes.Remove);
    for I := 1 to SceneAnimation.ScenesCount - 1 do
    begin
      RemovedNumberOther := SceneAnimation.Scenes[I].RootNode.
        EnumerateRemoveChildren(@HelperSpecialCastleNodes.Remove);
      Assert(RemovedNumberOther = RemovedNumber);
    end;

    if RemovedNumber <> 0 then
      WholeSceneChanged;
    MessageOK(Glwin, Format('Removed %d node instances.', [RemovedNumber]),
      taLeft);
  end;

  procedure WritelnCameraSettings(Version: TVRMLCameraVersion);
  begin
    Writeln(MakeVRMLCameraStr(Version,
      MatrixWalker.CameraPos,
      MatrixWalker.CameraDir,
      MatrixWalker.CameraUp,
      MatrixWalker.GravityUp));
  end;

  procedure WriteBoundingBox(const Box: TBox3d);
  begin
    if IsEmptyBox3d(Box) then
      MessageOK(Glw, 'The bounding box is empty.', taLeft) else
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
        '# Version for VRML 2.0' +nl+
        'DEF BoundingBox Transform {' +nl+
        '  translation %1:s' +nl+
        '  children Shape {' +nl+
        '    geometry Box {' +nl+
        '      size %2:s %3:s %4:s' +nl+
        '    } } }',
        [ Box3dToNiceStr(Box),
          VectorToRawStr(Box3dMiddle(Box)),
          FloatToRawStr(Box[1, 0] - Box[0, 0]),
          FloatToRawStr(Box[1, 1] - Box[0, 1]),
          FloatToRawStr(Box[1, 2] - Box[0, 2]) ]));
    end;
  end;

var
  S: string;
begin
 case MenuItem.IntData of
  10: begin
       s := ExtractFilePath(SceneFilename);
       if glwin.FileDialog('Open file', s, true) then
         LoadScene(s, [], 0.0, true);
      end;

  12: Glw.Close;

  15: begin
        { When reopening, then JumpToInitialViewpoint parameter is false.
          In fact, this was the purpose of this JumpToInitialViewpoint
          parameter: to set it to false when reopening, as this makes
          reopening more useful. }
        LoadScene(SceneFileName, [], 0.0, false);
      end;

  20: begin
        if SceneAnimation.ScenesCount > 1 then
          MessageOK(Glwin, 'Warning: this is an animation. Saving it as VRML ' +
            'will only save it''s first frame.', taLeft);

        { TODO: this filename gen is stupid, it leads to names like
          _2, _2_2, _2_2_2... while it should lead to _2, _3, _4 etc.... }
        if AnsiSameText(ExtractFileExt(SceneFilename), '.wrl') then
          s := AppendToFileName(SceneFilename, '_2') else
          s := ChangeFileExt(SceneFilename, '.wrl');
        if glwin.FileDialog('Save as VRML file', s, false) then
        try
          SaveToVRMLFile(SceneAnimation.FirstScene.RootNode, s,
            SavedVRMLPrecedingComment(SceneFileName));
        except
          on E: Exception do
          begin
            MessageOK(glw, 'Error while saving scene to "' +S+ '": ' +
              E.Message, taLeft);
          end;
        end;
      end;

  21: ViewSceneWarnings;

  31: ChangeSceneAnimation([scNoNormals], SceneAnimation);
  32: ChangeSceneAnimation([scNoSolidObjects], SceneAnimation);
  33: ChangeSceneAnimation([scNoConvexFaces], SceneAnimation);

  34: RemoveNodesWithMatchingName;
  35: RemoveSpecialCastleNodes;

  36: RemoveSelectedGeometry;
  37: RemoveSelectedFace;

  { Before all calls to SetViewpoint below, we don't really have to
    swith to nkWalker. But user usually wants to switch to nkWalker ---
    in nkExamine SetViewpoint result is not visible at all. }
  51: begin
        SetNavigatorKind(Glw, nkWalker);
        SetViewpoint(StdVRMLCamPos[1], StdVRMLCamDir, StdVRMLCamUp, StdVRMLGravityUp);
      end;
  52: begin
        SetNavigatorKind(Glw, nkWalker);
        SetViewpoint(StdVRMLCamPos[2], StdVRMLCamDir, StdVRMLCamUp, StdVRMLGravityUp);
      end;
  53: begin
        SetNavigatorKind(Glw, nkWalker);
        SetViewpointForWholeScene;
      end;

  81: Wireframe := not Wireframe;
  82: ShowBBox := not ShowBBox;
  83: with SceneAnimation do Attributes.SmoothShading := not Attributes.SmoothShading;
  84: if glwin.ColorDialog(BGColor) then BGColorChanged;
  85: with SceneAnimation do Attributes.UseFog := not Attributes.UseFog;
  86: with SceneAnimation do Attributes.Blending := not Attributes.Blending;

  91: LightCalculate := not LightCalculate;
  92: HeadLight := not HeadLight;
  93: with SceneAnimation do Attributes.UseLights := not Attributes.UseLights;
  94: with SceneAnimation do Attributes.EnableTextures := not Attributes.EnableTextures;
  95: ChangeLightModelAmbient;
  96: ShowFrustum := not ShowFrustum;
  180: ShowFrustumAlwaysVisible := not ShowFrustumAlwaysVisible;

  97: begin
        if OctreeDisplayWhole then
        begin
          OctreeDisplayWhole := false;
          MenuOctreeDisplayWhole.Checked := OctreeDisplayWhole;
          OctreeDisplayDepth := -1;
        end;

        Inc(octreeDisplayDepth);
        if octreeDisplayDepth > SceneTriangleOctree.MaxDepth then
          octreeDisplayDepth := -1;
      end;

  98: begin
        if OctreeDisplayWhole then
        begin
          OctreeDisplayWhole := false;
          MenuOctreeDisplayWhole.Checked := OctreeDisplayWhole;
          OctreeDisplayDepth := -1;
        end;

        Dec(octreeDisplayDepth);
        if octreeDisplayDepth < -1 then
          octreeDisplayDepth := SceneTriangleOctree.MaxDepth;
      end;

  99: begin
        OctreeDisplayWhole := not OctreeDisplayWhole;
        if not OctreeDisplayWhole then
          OctreeDisplayDepth := -1;
      end;

  101: Writeln(SceneTriangleOctree.Statistics);
  103: Writeln(SceneShapeStateOctree.Statistics);
  102: SceneAnimation.WritelnInfoNodes;

  105: Writeln(Format(
         'Call rayhunter like this to render this view :' +nl+
         'rayhunter classic %d %d %d "%s" "%s" \' +nl+
         '  --camera-pos %s \' +nl+
         '  --camera-dir %s \' +nl+
         '  --camera-up %s \' +nl+
         '  --view-angle-x %s --scene-bg-color %f %f %f',
         [ DEF_RAYTRACE_DEPTH,
           Glw.Width, Glw.Height,
           SceneFilename,
           ExtractOnlyFileName(SceneFilename) + '-rt.png',
           VectorToRawStr(MatrixWalker.CameraPos),
           VectorToRawStr(MatrixWalker.CameraDir),
           VectorToRawStr(MatrixWalker.CameraUp),
           FloatToStr(AngleOfViewX),
           BGColor[0], BGColor[1], BGColor[2] ]));

  106: WritelnCameraSettings(1);
  107: WritelnCameraSettings(2);

  108: Writeln(
         'Current viewing frustum planes :' +nl+
         '((A, B, C, D) means a plane given by equation A*x + B*y + C*z + D = 0.)' +nl+
         '  Left   : ' + VectorToRawStr(MatrixWalker.Frustum[fpLeft]) +nl+
         '  Right  : ' + VectorToRawStr(MatrixWalker.Frustum[fpRight]) +nl+
         '  Bottom : ' + VectorToRawStr(MatrixWalker.Frustum[fpBottom]) +nl+
         '  Top    : ' + VectorToRawStr(MatrixWalker.Frustum[fpTop]) +nl+
         '  Near   : ' + VectorToRawStr(MatrixWalker.Frustum[fpNear]) +nl+
         '  Far    : ' + VectorToRawStr(MatrixWalker.Frustum[fpFar]));

  109: WriteBoundingBox(SceneAnimation.BoundingBoxSum);
  110: WriteBoundingBox(SceneAnimation.SceneFromTime(AnimationTime).BoundingBox);

  111: ChangeNavigatorKind(glw, +1);

  121: ShowAndWrite(
         'Scene "' + SceneFilename + '" information:' + NL + NL +
         SceneAnimation.Info(true));
  122: ShowStatus := not ShowStatus;
  123: CollisionCheck := not CollisionCheck;
  124: ChangeGravityUp;
  125: if Glw.Navigator is TMatrixWalker then
        RaytraceToWin(glwin, SceneTriangleOctree,
          Glw.NavWalker.CameraPos,
          Glw.NavWalker.CameraDir, Glw.NavWalker.CameraUp,
          AngleOfViewX, AngleOfViewY, BGColor,
          SceneAnimation.FirstScene.FogNode,
          SceneAnimation.FirstScene.FogDistanceScaling) else
        MessageOK(glwin, SOnlyInWalker);
  126: Glw.SwapFullScreen;
  127: glwin.SaveScreenDialog(FNameAutoInc('view3dscene_screen_%d.png'));
  128: begin
         MatrixWalker.MouseLook := not MatrixWalker.MouseLook;
         Glw.UpdateMouseLook;

         if MatrixWalker.MouseLook then
         begin
           MatrixWalker.Input_LeftStrafe.AssignFromDefault(MatrixWalker.Input_LeftRot);
           MatrixWalker.Input_RightStrafe.AssignFromDefault(MatrixWalker.Input_RightRot);
           MatrixWalker.Input_LeftRot.AssignFromDefault(MatrixWalker.Input_LeftStrafe);
           MatrixWalker.Input_RightRot.AssignFromDefault(MatrixWalker.Input_RightStrafe);
         end else
         begin
           MatrixWalker.Input_LeftStrafe.MakeDefault;
           MatrixWalker.Input_RightStrafe.MakeDefault;
           MatrixWalker.Input_LeftRot.MakeDefault;
           MatrixWalker.Input_RightRot.MakeDefault;
         end;
       end;

  131: begin
         ShowAndWrite(
           'view3dscene: full-featured VRML, 3DS, MD3 viewer.' + NL +
           'Version ' + Version + '.' + NL +
           'By Michalis Kamburelis.' + NL +
           NL +
           '[http://vrmlengine.sourceforge.net/view3dscene.php]' + NL +
           NL +
           'Compiled with ' + SCompilerDescription +'.');
       end;

  171: SelectedShowInformation;
  172: SelectedShowLightsInformation;
  173: ShowAndWrite(GLCapsString);

  182: ChangePointSize;

  201: MatrixWalker.Gravity := not MatrixWalker.Gravity;
  202: MatrixWalker.PreferGravityUpForRotations := not MatrixWalker.PreferGravityUpForRotations;
  203: MatrixWalker.PreferGravityUpForMoving := not MatrixWalker.PreferGravityUpForMoving;

  220: AnimationTimePaused := not AnimationTimePaused;
  221: AnimationTime := SceneAnimation.TimeBegin;
  222: ChangeAnimationTimeSpeed;
  223: ChangeAnimationTimeSpeedWhenLoading;

  300..399:
    begin
      { We could just SetViewpoint, without swithing to nkWalker.
        But user usually wants to switch to nkWalker --- in nkExamine
        SetViewpoint is not visible at all. }
      SetNavigatorKind(Glw, nkWalker);
      SetViewpoint(MenuItem.IntData - 300);
    end;

  400..419: SceneAnimation.Attributes.BlendingSourceFactor :=
    BlendingFactors[MenuItem.IntData - 400].Value;
  420..439: SceneAnimation.Attributes.BlendingDestinationFactor :=
    BlendingFactors[MenuItem.IntData - 420].Value;

  1000..1099: SetColorModulatorType(
    TColorModulatorType(MenuItem.IntData-1000), SceneAnimation);
  1100..1199: SetTextureMinFilter(
    TTextureMinFilter  (MenuItem.IntData-1100), SceneAnimation);
  1200..1299: SetTextureMagFilter(
    TTextureMagFilter  (MenuItem.IntData-1200), SceneAnimation);
  1300..1399: SetNavigatorKind(glw,
    TNavigatorKind(     MenuItem.IntData-1300));

  else raise EInternalError.Create('not impl menu item');
 end;

 glwin.PostRedisplay;
end;

function CreateMainMenu: TMenu;

  procedure AppendColorModulators(M: TMenu);
  var
    Cmt: TColorModulatorType;
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
  begin
    RadioGroup := nil;
    for Cmt := Low(Cmt) to High(Cmt) do
    begin
      Radio := TMenuItemRadio.Create(
        SQuoteMenuEntryCaption(ColorModulatorInfos[Cmt].Name),
        Ord(Cmt) + 1000, ColorModulatorType = Cmt, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
    end;
  end;

  procedure AppendTextureMinFilters(M: TMenu);
  var
    TexMin: TTextureMinFilter;
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
  begin
    RadioGroup := nil;
    for TexMin := Low(TexMin) to High(TexMin) do
    begin
      Radio := TMenuItemRadio.Create(
        SQuoteMenuEntryCaption(TextureMinFilterNames[TexMin]),
        Ord(TexMin) + 1100, TexMin = TextureMinFilter, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
    end;
  end;

  procedure AppendTextureMagFilters(M: TMenu);
  var
    TexMag: TTextureMagFilter;
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
  begin
    RadioGroup := nil;
    for TexMag := Low(TexMag) to High(TexMag) do
    begin
      Radio := TMenuItemRadio.Create(
        SQuoteMenuEntryCaption(TextureMagFilterNames[TexMag]),
        Ord(TexMag) + 1200, TexMag = TextureMagFilter, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
    end;
  end;

  procedure AppendNavigationTypes(M: TMenu);
  var
    NavKind: TNavigatorKind;
    Radio: TMenuItemRadio;
    RadioGroup: TMenuItemRadioGroup;
  begin
    RadioGroup := nil;
    for NavKind := Low(NavKind) to High(NavKind) do
    begin
      Radio := TMenuItemRadio.Create(
        SQuoteMenuEntryCaption(NavigatorNames[NavKind]),
        Ord(NavKind) + 1300, NavKind = NavigatorKind, true);
      if RadioGroup = nil then
        RadioGroup := Radio.Group else
        Radio.Group := RadioGroup;
      M.Append(Radio);
      NavigatorRadios[NavKind] := Radio;
    end;
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
          Caption += ' (default)';
        Radio := TMenuItemRadio.Create(Caption, BaseIntData + I, IsDefault, true);
        if RadioGroup = nil then
          RadioGroup := Radio.Group else
          Radio.Group := RadioGroup;
        M.Append(Radio);
      end;
  end;

var
  M, M2: TMenu;
  NextRecentMenuItem: TMenuEntry;
begin
 Result := TMenu.Create('Main menu');
 M := TMenu.Create('_File');
   M.Append(TMenuItem.Create('_Open ...',         10, CtrlO));
   MenuReopen := TMenuItem.Create('_Reopen',      15);
   MenuReopen.Enabled := false;
   M.Append(MenuReopen);
   M.Append(TMenuItem.Create('_Save as VRML ...', 20));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('View _Warnings About Current Scene', 21));
   NextRecentMenuItem := TMenuSeparator.Create;
   M.Append(NextRecentMenuItem);
   RecentMenu.NextMenuItem := NextRecentMenuItem;
   M.Append(TMenuItem.Create('_Exit',             12, CharEscape));
   Result.Append(M);
 M := TMenu.Create('_View');
   M.Append(TMenuItemChecked.Create('_Wireframe',              81, 'w',
     Wireframe, true));
   M.Append(TMenuItemChecked.Create('Show _Bounding Box',      82, 'b',
     ShowBBox, true));
   M.Append(TMenuItemChecked.Create('_Smooth Shading',         83, 's',
     SceneAnimation.Attributes.SmoothShading, true));
   M.Append(TMenuItem.Create('Change Background Color ...',    84));
   M.Append(TMenuItemChecked.Create('_Fog',                    85, 'f',
     SceneAnimation.Attributes.UseFog, true));
   M.Append(TMenuItemChecked.Create('Blending',                86, CtrlB,
     SceneAnimation.Attributes.Blending, true));
   M2 := TMenu.Create('Blending Source Factor');
     AppendBlendingFactors(M2, true, 400);
     M.Append(M2);
   M2 := TMenu.Create('Blending Destination Factor');
     AppendBlendingFactors(M2, false, 420);
     M.Append(M2);
   M2 := TMenu.Create('Change Scene Colors');
     AppendColorModulators(M2);
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create(
     '_Lighting Calculate (GL__LIGHTING enabled)',         91, 'l',
     LightCalculate, true));
   MenuHeadlight := TMenuItemChecked.Create('_Head Light', 92, 'h',
     Headlight, true);
   M.Append(MenuHeadlight);
   M.Append(TMenuItemChecked.Create('Use Scene Lights',    93,
     SceneAnimation.Attributes.UseLights, true));
   M.Append(TMenuItem.Create('Light Global Ambient Color ...',  95));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('_Textures',           94, 't',
     SceneAnimation.Attributes.EnableTextures, true));
   M2 := TMenu.Create('Texture Minification Method');
     AppendTextureMinFilters(M2);
     M.Append(M2);
   M2 := TMenu.Create('Texture Magnification Method');
     AppendTextureMagFilters(M2);
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('Show in Examine mode Camera Frustum', 96,
     ShowFrustum, true));
   M.Append(TMenuItemChecked.Create('When Showing Camera Frustum, ' +
     'Show it Over All Other Objects (no depth test)', 180,
     ShowFrustumAlwaysVisible, true));
   M.Append(TMenuSeparator.Create);
   MenuOctreeDisplayWhole := TMenuItemChecked.Create('Show Whole Octree',
     99, OctreeDisplayWhole, true);
   M.Append(MenuOctreeDisplayWhole);
   M.Append(TMenuItem.Create('Show _Upper Level of Octree Based on Triangles', 97, CtrlU));
   M.Append(TMenuItem.Create('Show _Lower Level of Octree Based on Triangles', 98, CtrlD));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Set Point Size ...', 182));
   Result.Append(M);
 M := TMenu.Create('_Navigation');
   ViewpointsList.MenuJumpToViewpoint := TMenu.Create('Jump to Viewpoint');
     ViewpointsList.MakeMenuJumpToViewpoint;
     M.Append(ViewpointsList.MenuJumpToViewpoint);
   M2 := TMenu.Create('Navigation Mode');
     AppendNavigationTypes(M2);
     M2.Append(TMenuSeparator.Create);
     M2.Append(TMenuItem.Create('Switch to Next',                      111, 'v'));
     M.Append(M2);
   M2 := TMenu.Create('Walk mode Settings');
     M2.Append(TMenuItemChecked.Create(
       '_Use Mouse Look',                       128, 'm',
         MatrixWalker.MouseLook, true));
     M2.Append(TMenuItemChecked.Create(
       '_Collision Checking',                   123, 'c',
         CollisionCheck, true));
     MenuGravity := TMenuItemChecked.Create(
       '_Gravity',                              201, 'g',
       MatrixWalker.Gravity, true);
     M2.Append(MenuGravity);
     MenuPreferGravityUpForRotations := TMenuItemChecked.Create(
       'Rotate with Respect to Stable (Gravity) Camera Up',      202,
       MatrixWalker.PreferGravityUpForRotations, true);
     M2.Append(MenuPreferGravityUpForRotations);
     MenuPreferGravityUpForMoving := TMenuItemChecked.Create(
       'Move with Respect to Stable (Gravity) Camera Up',          203,
       MatrixWalker.PreferGravityUpForMoving, true);
     M2.Append(MenuPreferGravityUpForMoving);
     M2.Append(TMenuItem.Create('Change Gravity Up Vector ...',  124));
     M.Append(M2);
   Result.Append(M);
 M := TMenu.Create('_Animation');
   M.Append(TMenuItem.Create('Pause / Play', 220, CtrlP));
   M.Append(TMenuItem.Create('Rewind to Beginning', 221));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Make Time Pass Slower or Faster (on display) ...', 222));
   M.Append(TMenuItem.Create('Make Time Pass Slower or Faster (on loading) ...', 223));
   Result.Append(M);
 M := TMenu.Create('_Edit');
   MenuRemoveSelectedGeometry :=
     TMenuItem.Create('Remove _Geometry Node (containing selected triangle)', 36);
   M.Append(MenuRemoveSelectedGeometry);
   MenuRemoveSelectedFace :=
     TMenuItem.Create('Remove _Face (containing selected triangle)', 37);
   M.Append(MenuRemoveSelectedFace);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create(
     'Remove VRML Nodes with Name Matching ...', 34));
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
     'Remove Special Nodes on "The Castle" Level', 35));
   Result.Append(M);
 M := TMenu.Create('_Console');
   M.Append(TMenuItem.Create('Print Statistics of Octree Based on _Triangles', 101));
   M.Append(TMenuItem.Create('Print Statistics of Octree Based on _ShapeStates', 103));
   M.Append(TMenuItem.Create('Print VRML _Info nodes',        102));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print _rayhunter Command-line to Render This View',
     105, CtrlR));
   M.Append(TMenuItem.Create('Print Current Camera _Node (VRML 1.0)',   106));
   M.Append(TMenuItem.Create('Print Current Camera Node (VRML 2.0)',    107, CtrlC));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print Current Camera _Frustum', 108));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print _Bounding Box (of whole animation)', 109));
   M.Append(TMenuItem.Create('Print Bounding Box (of current animation frame)', 110));
   Result.Append(M);
 M := TMenu.Create('_Other');
   M.Append(TMenuItem.Create('_Raytrace !',                   125, 'r'));
   M.Append(TMenuItemChecked.Create('_Full Screen',           126, K_F11,
     Glw.FullScreen, true));
   M.Append(TMenuItem.Create('_Save Screen ...',              127, K_F5));
   Result.Append(M);
 M := TMenu.Create('_Help');
   M.Append(TMenuItemChecked.Create('Show Status _Text',           122, K_F1,
      ShowStatus, true));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Scene Information',                  121));
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

{ main --------------------------------------------------------------------- }

var
  Param_CameraRadius: Single = 0.0;
  WasParam_WriteToVRML: boolean = false;

  WasParam_SceneFileName: boolean = false;
  Param_SceneFileName: string;
  Param_SceneChanges: TSceneChanges = [];

const
  Options: array[0..10] of TOption =
  (
    (Short:  #0; Long: 'triangle-octree-max-depth'; Argument: oaRequired),
    (Short:  #0; Long: 'triangle-octree-max-leaf-items-count'; Argument: oaRequired),
    (Short:  #0; Long: 'camera-radius'; Argument: oaRequired),
    (Short:  #0; Long: 'scene-change-no-normals'; Argument: oaNone),
    (Short:  #0; Long: 'scene-change-no-solid-objects'; Argument: oaNone),
    (Short:  #0; Long: 'scene-change-no-convex-faces'; Argument: oaNone),
    (Short:  #0; Long: 'write-to-vrml'; Argument: oaNone),
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'ss-octree-max-depth'; Argument: oaRequired),
    (Short:  #0; Long: 'ss-octree-max-leaf-items-count'; Argument: oaRequired)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
   case OptionNum of
    0 : TriangleOctreeMaxDepth := StrToInt(Argument);
    1 : TriangleOctreeMaxLeafItemsCount := StrToInt(Argument);
    2 : Param_CameraRadius := StrToFloat(Argument);
    3 : Include(Param_SceneChanges, scNoNormals);
    4 : Include(Param_SceneChanges, scNoSolidObjects);
    5 : Include(Param_SceneChanges, scNoConvexFaces);
    6 : WasParam_WriteToVRML := true;
    7 : begin
         InfoWrite(
           'view3dscene: VRML 1.0 (basic VRML 97 under development), ' +nl+
           '  3DS, OBJ and GEO viewer. ' +nl+
           '  You can move in the scene, possibly with collision-checking.' +nl+
           '  It can also be used to convert 3DS, OBJ and GEO files to VRML.' +nl+
           '  Also it has built-in raytracer, similiar to that available in ' +nl+
           '  my other program, rayhunter.' +nl+
           nl+
           'Call as' +nl+
           '  view3dscene [OPTIONS]... FILE-TO-VIEW' +nl+
           'Available options are:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           '  --camera-radius RADIUS' +nl+
           '                        Set camera sphere radius used for collisions' +nl+
           '                        and determinig moving speed' +nl+
           '  --scene-change-no-normals ,' +nl+
           '  --scene-change-no-solid-objects ,' +nl+
           '  --scene-change-no-convex-faces' +nl+
           '                        Change scene somehow after loading' +nl+
           '  --write-to-vrml       After loading (and changing) scene, write it' +nl+
           '                        as VRML 1.0 to the standard output' +nl+
           MultiNavigatorsOptionsHelp +nl+
           LightsOptionsHelp +nl+
           VRMLNodesDetailOptionsHelp +nl+
           RendererOptimizationOptionsHelp +nl+
           '  --triangle-octree-max-depth DEPTH ,' +nl+
           '  --triangle-octree-max-leaf-items-count COUNT' +nl+
           '                        Set the parameters of generated octree.' +nl+
           '                        These concern octree based on triangles.' +nl+
           '                        Defaults are: max-depth = '+
             IntToStr(V3DSDefTriangleOctreeMaxDepth) + ',' +nl+
           '                        max-leaf-items-count = '+
             IntToStr(V3DSDefTriangleOctreeMaxLeafItemsCount) + '.' +nl+
           '  --ss-octree-max-depth DEPTH ,' +nl+
           '  --ss-octree-max-leaf-items-count COUNT' +nl+
           '                        Set the parameters of generated octree.' +nl+
           '                        These concern octree based on ShapeStates.' +nl+
           '                        Defaults are: max-depth = '+
             IntToStr(DefShapeStateOctreeMaxDepth) + ',' +nl+
           '                        max-leaf-items-count = '+
             IntToStr(DefShapeStateOctreeMaxLeafItemsCount) + '.' +nl+
           nl+
           TGLWindow.ParseParametersHelp(StandardParseOptions, true) +nl+
           nl+
           SVrmlEngineProgramHelpSuffix(DisplayProgramName, Version, true));
         ProgramBreak;
        end;
    8 : begin
         Writeln(Version);
         ProgramBreak;
        end;
    9 : ShapeStateOctreeMaxDepth := StrToInt(Argument);
    10: ShapeStateOctreeMaxLeafItemsCount := StrToInt(Argument);
    else raise EInternalError.Create('OptionProc');
   end;
  end;

var
  Helper: THelper;
begin
 { parse parameters }
 { glw params }
 Glw.ParseParameters(StandardParseOptions);
 { our params }
 MultiNavigatorsParseParameters;
 LightsParseParameters;
 VRMLNodesDetailOptionsParse;
 RendererOptimizationOptionsParse(RendererOptimization);
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

 VRMLNonFatalError := @VRMLNonFatalError_Warning;

 if WasParam_WriteToVRML then
 begin
  if not WasParam_SceneFileName then
   raise EInvalidParams.Create('You used --write-to-vrml option, '+
     'this means that you want to convert some 3d model file to VRML. ' +
     'But you didn''t provide any filename on command-line to load.');
  WriteToVRML(Param_SceneFileName, Param_SceneChanges);
  Exit;
 end;

 { This is for loading default clean scene.
   LoadClearScene should be lighting fast always,
   so progress should not be needed in this case anyway
   (and we don't want to clutter stdout). }
 Progress.UserInterface := ProgressNullInterface;

 Helper := nil;
 InitMultiNavigators(glw, @Helper.MoveAllowed, @Helper.GetCameraHeight);

 SceneWarnings := TStringList.Create;

 { init "scene global variables" to null values }
 SceneAnimation := TVRMLGLAnimation.Create;
 try
  { in view3dscene SceneAnimation.Attributes.UseLights default value is true }
  SceneAnimation.Attributes.UseLights := true;
  InitColorModulator(SceneAnimation);
  InitTextureFilters(SceneAnimation);

  RecentMenu := TGLRecentMenu.Create;
  RecentMenu.LoadFromConfig(ConfigFile, 'recent_files');
  RecentMenu.OnOpenRecent := @Helper.OpenRecent;

  { init "scene global variables" to non-null values }
  LoadClearScene;
  try

   GLWinMessagesTheme := GLWinMessagesTheme_TypicalGUI;

   Glw.MainMenu := CreateMainMenu;
   Glw.OnMenuCommand := @MenuCommand;
   Glw.OnResize := @Resize;
   Glw.OnInit := @Init;
   Glw.OnClose := @Close;
   Glw.OnMouseDown := @MouseDown;
   Glw.OnBeforeDraw := @BeforeDraw;
   Glw.OnDraw := @Draw;
   Glw.OnIdle := @Idle;

   Glw.SetDemoOptions(K_None, #0, true);

   Glw.Init;

   if WasParam_SceneFileName then
     LoadScene(Param_SceneFileName, Param_SceneChanges, Param_CameraRadius, true) else
     LoadWelcomeScene;

   Glwm.Loop;
  finally FreeScene end;
 finally
   FreeAndNil(SceneAnimation);
   FreeAndNil(SceneWarnings);
   if RecentMenu <> nil then
     RecentMenu.SaveToConfig(ConfigFile, 'recent_files');
   FreeAndNil(RecentMenu);
 end;
end.

{
  Local Variables:
  kam-compile-release-command-unix:  "./compile.sh && mv -fv view3dscene      ~/bin/"
  kam-compile-release-command-win32: "./compile.sh && mv -fv view3dscene.exe c:/bin/"
  End:
}
