{
  Copyright 2002-2006 Michalis Kamburelis.

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

program view3dscene;

{$I openglmac.inc}

{ WWW page of this program, with user documentation,
  is here: [http://camelot.homedns.org/~michalis/view3dscene.php].

  If you want to find out how to use my units to use VRML and 3DS
  models in your own programs, this program may look
  too complex and filled with irrelevant details.
  Instead you should look at simple example program in
  units/3dmodels.gl/examples/simpleViewModel.dpr.

  Also units/3dmodels/examples/many2vrml.dpr is an example how to write
  simple command-line converter from 3DS (and all other model formats
  I can read) to VRML.

  This is 3d scene viewer. Basic components are :
  - use LoadAsVRMLScene to load any format to VRML scene
  - render scene using TVRMLFlatSceneGL
  - use MatrixNavigation and TGLWindowNavigated to let user navigate
    over the scene using various navigation methods
    (Examine, Walk) and with optional gravity
  - build TVRMLTriangleOctree to allow collision detection for
    Walk navigation and to allow raytracer
  - build TVRMLShapeStateOctree to allow frustum culling using
    octree by TVRMLFlatSceneGL
  - use VRMLRayTracer embedded in RaytraceToWindow module to allow
    viewing raytraced image
  - allow some kind of object picking with mouse left button (using
    simple one-primary-ray casting)

  Ten program to nic wielkiego - skladamy tutaj po prostu razem
  wiele rzeczy z modulow 3dmodels.gl/, 3dmodels/, 3dgraph/ i opengl/.

  Ladujemy modele przez LoadAsVRML ktore konwertuje wszystkie znane nam formaty
  do VRMLa. Nie tracimy przez to nic bo VRML obejmuje wszystko co jest
  zaimplementowane w tamtych formatach a zyskujemy prostote i szybkosc
  w zapisie niniejszego programu - operujemy zawsze na TVRMLFlatSceneGL i mozemy
  wykorzystac jej funkcjonalnosc (chocby automatyczne generowanie wektorow
  normalnych uwzgledniajac creaseAngle i mozliwosc przestawienia na generowanie
  normali flat) i rozne optymalizacje pod katem wyswietlania w OpenGL'u.
}

uses
  KambiUtils, SysUtils, VectorMath, Boxes3d, Classes, KambiClassUtils,
  BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  ParseParametersUnit, ProgressUnit, MatrixNavigation, RaysWindow,
  KambiStringUtils, KambiFilesUtils, Math,
  { OpenGL related units: }
  OpenGLh, GLWindow, GLW_Navigated, KambiGLUtils, OpenGLBmpFonts,
  GLWinMessages, ProgressGL, GLWindowRecentMenu,
  { VRML (and possibly OpenGL) related units: }
  VRMLFields, KambiOctree, VRMLTriangleOctree, VRMLShapeStateOctree,
  VRMLNodes, Object3dAsVRML, VRMLFlatSceneGL,
  VRMLFlatScene, VRMLRayTracer, BackgroundGL, VRMLNodesDetailOptions,
  VRMLCameraUtils, VRMLErrors,
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
    Scene.RootNode = nil,
    Scene.Default(Triangle/ShapeState)Octree = nil,
    SceneFileName = '' etc. }
  { Note that only one Scene object is created and present for the whole
    lifetime of this program, i.e. when I load new scene (from "Open"
    menu item) I DO NOT free and create new Scene object.
    Instead I'm only freeing and creating new value for Scene.RootNode.
    This way I'm preserving values of all Attributes.Xxx when opening new scene
    from "Open" menu item. }
  Scene: TVRMLFlatSceneGL;
  SceneFilename: string;
  { must always be > 0 }
  CameraKind: TVRMLCameraKind;
  { vars below istotne tylko w trybach nawigacji ktore robia wykrywanie kolizji: }
  CameraRadius: Single = 0.0;
  { 0.0 means "no limit". }
  VisibilityLimit: Single = 0.0;
  NavigationNode: TNodeNavigationInfo;
  ViewpointNode: TNodeGeneralViewpoint;

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

{ ogolne pomocnicze funkcje -------------------------------------------------- }

function AngleOfViewY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(
    AngleOfViewX, Glw.Height/Glw.Width);
end;

function WalkProjectionNear: Single;
begin
 Result := CameraRadius * 0.6;
end;

function WalkProjectionFar: Single;
begin
 if VisibilityLimit <> 0.0 then
  Result := VisibilityLimit else
 if IsEmptyBox3d(Scene.BoundingBox) then
  { When IsEmptyBox3d, Result is not simply "any dummy value".
    It must be appropriately larger than WalkProjectionNear
    to provide sufficient space for rendering Background node
    (see Scene.BackgroundSkySphereRadius evaluated in Init). }
  Result := WalkProjectionNear * 10 else
  Result := Box3dAvgSize(Scene.BoundingBox) * 20.0;
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

{ TGLWindow callbacks --------------------------------------------------------- }

procedure Init(glwin: TGLWindow);
begin
 statusFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);

 { normalize normals because we will scale our objects in Examiner navigation;
   chwilowo i tak w Scene.Render zawsze jest wlaczane glEnable(GL_NORMALIZE)
   ale to nie zawsze bedzie prawdziwe.  }
 glEnable(GL_NORMALIZE);
 glEnable(GL_DEPTH_TEST);

 ProgressGLInterface.Window := Glw;
 Progress.UserInterface := ProgressGLInterface;

 BGColorChanged;
end;

procedure Close(glwin: TGLWindow);
begin
  if Scene <> nil then
    Scene.CloseGL;
  FreeAndNil(statusFont);
end;

procedure DrawStatus(data: integer);

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
  strs.Append(Format('Navigation style : %s', [NavigatorNames[NavigatorKind]]));

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
   s := BoolToStrOO[Scene.Attributes.UseLights];
  strs.Append(Format('Use scene lights: %s', [s]));

  { Note: there's no sense in showing here Glw.FpsRealTime,
    since it would force me to constantly render new frames just
    to show constantly changing Glw.FpsRealTime ...
    this makes no sense, of course.
    I can sensibly show here only Glw.FpsFrameTime.
    User will be able to see Glw.FpsRealTime only on window's Caption. }
  strs.Append(Format('Rendered ShapeStates : %d of %d. FPS : %f',
    [ Scene.LastRender_RenderedShapeStatesCount,
      Scene.LastRender_AllShapeStatesCount,
      Glw.FpsFrameTime ]));

  {statusFont.printStringsBorderedRect(strs, 0, Brown4f, Yellow4f, Black4f,
    nil, 5, 1, 1);}
  statusFont.printStringsBorderedRect(strs, 0,
    Black4Single, Green4Single, Yellow4Single,
    @HalftoneStipple, 5, 1, 1);
 finally strs.Free end;
end;

procedure BeforeDraw(glwin: TGLWindow);
begin
 Scene.PrepareRender(true, true, false, false);
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

begin
 glClear(GL_DEPTH_BUFFER_BIT);

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

 glLoadMatrix(Glw.Navigator.Matrix);

 { Draw may be called in various situations even when Scene
   is not really ready (e.g. when showing errors after scene loading),
   so we have to check here whether some things are initialized as they
   should. }
 if (Scene.DefaultTriangleOctree <> nil) and
    (Scene.DefaultTriangleOctree.TreeRoot <> nil) then
 if OctreeDisplayWhole then
 begin
   glColorv(Yellow3Single);
   DisplayOctreeWhole(Scene.DefaultTriangleOctree.TreeRoot);
 end else
 if OctreeDisplayDepth >= 0 then
 begin
   glColorv(Yellow3Single);
   DisplayOctreeDepth(Scene.DefaultTriangleOctree.TreeRoot, OctreeDisplayDepth);
 end;

 if showBBox then
 begin
  glColorv(Green3Single);
  if not IsEmptyBox3d(Scene.BoundingBox) then
    glDrawBox3dWire(Scene.BoundingBox);
 end;

 if Wireframe then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
 try
  BeginRenderSceneWithLights;
    if (Glw.Navigator is TMatrixWalker) then
     Scene.RenderFrustumOctree(Glw.NavWalker.Frustum) else
     Scene.Render(nil);
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
    glProjectionPushPopOrtho2D(@DrawStatus, 0, 0, Glwin.Width, 0, Glwin.Height);
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
begin
 glViewport(0, 0, glwin.Width, glwin.Height);

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
   uzywam tu Box3dAvgSize(Scene.BoundingBox)*0.1 i to daje dobra dokladnosc,
   w Walkerze uzywam cameraRadius*0.6 co standardowo
   daje Box3dAvgSize(Scene.BoundingBox)*0.006 a wiec prawie 20 razy mniej !
   Przy tak malym perspective near w trybie Examiner czesto byloby widac
   bledy z-bufora, patrz np. KingsHead.wrl. }
 if (Glw.Navigator is TMatrixExaminer) and
    (not IsEmptyBox3d(Scene.BoundingBox)) then
  zNear := Box3dAvgSize(Scene.BoundingBox) * 0.1 else
  zNear := WalkProjectionNear;
 zFar := WalkProjectionFar;

 if CameraKind = ckPerspective then
  ProjectionGLPerspective(AngleOfViewY, glwin.Width/glwin.Height,
    zNear, zFar) else
 begin
  if IsEmptyBox3d(Scene.BoundingBox) then
   MaxSize := 1.0 { any dummy value } else
   MaxSize := Box3dMaxSize(Scene.BoundingBox);
  ProjectionGLOrtho(-MaxSize/2, MaxSize/2,
                    -MaxSize/2, MaxSize/2,
                    zNear, zFar);
 end;

 UpdateNavigatorProjectionMatrix;
end;

const
  SOnlyInWalker = 'You must be in ''Walk'' navigation style '+
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

    Scene.DefaultTriangleOctree.DirectCollisionTestsCounter := 0;

    SelectedItemIndex :=
      Scene.DefaultTriangleOctree.RayCollision(
        SelectedPoint, Ray0, RayVector, true, NoItemIndex, false, nil);

    { DirectCollisionTestsCounter is not recorded,
      so I may write it now on console in case it will be useful.
      For now it's commented out --- not interesting to typical user.
    Writeln(Format('%d tests for collisions between ray ' +
      'and triangles were needed to learn this.',
      [ Scene.DefaultTriangleOctree.DirectCollisionTestsCounter ])); }

    if SelectedItemIndex = NoItemIndex then
      SelectedItem := nil else
      SelectedItem := Scene.DefaultTriangleOctree.OctreeItems.
        Pointers[SelectedItemIndex];

    UpdateSelectedEnabled;

    Glw.PostRedisplay;
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
    Scene.DefaultTriangleOctree.DirectCollisionTestsCounter := 0;
    Result := Scene.DefaultTriangleOctree.MoveAllowed(
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
    Result := Box3dPointInside(NewPos, Scene.BoundingBox);
end;

class procedure THelper.GetCameraHeight(Navigator: TMatrixWalker;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
begin
  Scene.DefaultTriangleOctree.GetCameraHeight(
    TMatrixWalker(Navigator).CameraPos,
    TMatrixWalker(Navigator).HomeCameraUp,
    IsAboveTheGround, SqrHeightAboveTheGround,
    NoItemIndex, nil);
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
  MenuPreferHomeUpForRotations: TMenuItemChecked;
  MenuPreferHomeUpForMoving: TMenuItemChecked;
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
    Scene.OwnsDefaultTriangleOctree := false;
    Scene.DefaultTriangleOctree := Scene.CreateTriangleOctree(
      TriangleOctreeMaxDepth, TriangleOctreeMaxLeafItemsCount,
      'Building triangle octree');

    Scene.OwnsDefaultShapeStateOctree := false;
    Scene.DefaultShapeStateOctree := Scene.CreateShapeStateOctree(
      ShapeStateOctreeMaxDepth, ShapeStateOctreeMaxLeafItemsCount,
      'Building ShapeState octree');
  finally
    Glw.OnDraw := @Draw;
    Glw.OnBeforeDraw := @BeforeDraw;
  end;
end;

procedure SceneOctreeFree;
begin
  Scene.DefaultTriangleOctree.Free;
  Scene.DefaultTriangleOctree := nil;

  Scene.DefaultShapeStateOctree.Free;
  Scene.DefaultShapeStateOctree := nil;
end;

procedure Unselect;
begin
  SelectedItem := nil;
  SelectedItemIndex := NoItemIndex;
  UpdateSelectedEnabled;
end;

{ Frees (and sets to some null values) "scene global variables".

  Note about OpenGL context: remember that changing Scene.RootNode
  requires calling Scene.ChangedSthg that closes all connections
  of Scene to OpenGL context. This means that:
  1) Scene.RootNode must not be initialized, or Scene must not have any
     connections with OpenGL context (like e.g. after calling Scene.CloseGL)
  2) or you must call FreeScene in the same OpenGL context that the
     Scene is connected to. }
procedure FreeScene;
begin
  SceneOctreeFree;

  Scene.RootNode.Free;
  Scene.RootNode := nil;

  Scene.ChangedAll;

  ViewpointsList.Count := 0;
  if ViewpointsList.MenuJumpToViewpoint <> nil then
    ViewpointsList.MakeMenuJumpToViewpoint;

  NavigationNode := nil;
  ViewpointNode := nil;

  SceneFileName := '';

  if MenuReopen <> nil then
    MenuReopen.Enabled := false;

  Unselect;
end;

{ This jumps to 1st viewpoint on ViewpointsList
  (or to the default VRML cam setting if no viewpoints in the list).

  Sets CameraKind.
  Uses CameraRadius, NavigationNode, so make sure these are already
  set as needed }
procedure SetViewpoint(Index: Integer);
var
  HomeCameraPos: TVector3Single;
  HomeCameraDir: TVector3Single;
  HomeCameraUp: TVector3Single;
begin
  ViewpointNode := ViewpointsList.GetViewpoint(Index, CameraKind,
    HomeCameraPos, HomeCameraDir, HomeCameraUp);

  { zmien dlugosc HomeCameraDir,
    na podstawie CameraRadius i NavigationNode.FdSpeed }
  HomeCameraDir := VectorAdjustToLength(HomeCameraDir, CameraRadius * 0.4);
  if (NavigationNode <> nil) and (NavigationNode.FdSpeed.Value <> 0.0) then
    VectorScaleTo1st(HomeCameraDir, NavigationNode.FdSpeed.Value);

  MatrixWalker.Init(HomeCameraPos, HomeCameraDir,
    HomeCameraUp, MatrixWalker.CameraPreferredHeight, CameraRadius);

  if not Glw.Closed then
  begin
    Glw.EventResize;
    Glw.PostRedisplay;
  end;
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

  Note that RootNode object will be owned by Scene.
  So do not Free RootNode after using this procedure.

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
procedure LoadSceneCore(RootNode: TVRMLNode;
  ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  JumpToInitialViewpoint: boolean);
var
  NewCaption: string;
  CameraPreferredHeight: Single;
  WorldInfoNode: TNodeWorldInfo;
  I: Integer;
  SavedCameraPos, SavedCameraDir, SavedCameraUp: TVector3Single;
begin
  FreeScene;

  try
    SceneFileName := ASceneFileName;
    Scene.RootNode := RootNode;
    Scene.ChangedAll;

    NavigationNode := Scene.RootNode.TryFindNode(TNodeNavigationInfo, true)
      as TNodeNavigationInfo;

    ChangeScene(SceneChanges, Scene);

    { calculate CameraRadius }
    CameraRadius := ACameraRadius;
    if CameraRadius = 0.0 then
    begin
      if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
        CameraRadius := NavigationNode.FdAvatarSize.Items[0];
      if CameraRadius = 0.0 then
        if IsEmptyBox3d(Scene.BoundingBox) then
          CameraRadius := 1.0 { any non-zero dummy value } else
          CameraRadius := Box3dAvgSize(Scene.BoundingBox)*0.01;
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
    end;

    SceneInitMultiNavigators(Scene.BoundingBox,
      StdVRMLCamPos_1, StdVRMLCamDir, StdVRMLCamUp,
      CameraPreferredHeight, CameraRadius);

    { calculate ViewpointsList, MenuJumpToViewpoint,
      and jump to 1st viewpoint (or to the default cam settings). }
    Scene.EnumerateViewpoints(@ViewpointsList.AddNodeTransform);
    if ViewpointsList.MenuJumpToViewpoint <> nil then
      ViewpointsList.MakeMenuJumpToViewpoint;
    SetViewpoint(0);

    if not JumpToInitialViewpoint then
    begin
      MatrixWalker.CameraPos := SavedCameraPos;
      MatrixWalker.CameraDir := SavedCameraDir;
      MatrixWalker.CameraUp := SavedCameraUp;
    end;

    SceneInitLights(Scene, NavigationNode);

    { SceneInitLights could change HeadLight value.
      So update MenuHeadlight.Checked now. }
    if MenuHeadlight <> nil then
      MenuHeadlight.Checked := HeadLight;

    WorldInfoNode := Scene.RootNode.TryFindNode(TNodeWorldInfo, true)
      as TNodeWorldInfo;
    if (WorldInfoNode <> nil) and (WorldInfoNode.FdTitle.Value <> '') then
      NewCaption := SForCaption(WorldInfoNode.FdTitle.Value) else
      NewCaption := ExtractFileName(SceneFilename);
    NewCaption += ' - view3dscene';
    if Glw.Closed then
      Glw.Caption := NewCaption else
      Glw.FPSBaseCaption := NewCaption;

    { evaluate Scene.BackgroundSkySphereRadius (musi byc najpierw ustalone
      Scene.BoundingBox i CameraRadius, sa potrzebne do WalkProjectionNear/Far) }
    Scene.BackgroundSkySphereRadius := TBackgroundGL.NearFarToSkySphereRadius(
      WalkProjectionNear, WalkProjectionFar);

    { Find recognized NavigationNode.FdType }
    if NavigationNode <> nil then
      for I := 0 to NavigationNode.FdType.Count - 1 do
        if NavigationNode.FdType.Items[I] = 'WALK' then
        begin
          SetNavigatorKind(Glw, nkWalker);
          MatrixWalker.PreferHomeUpForRotations := true;
          MatrixWalker.PreferHomeUpForMoving := true;
          MatrixWalker.Gravity := true;
          Break;
        end else
        if NavigationNode.FdType.Items[I] = 'FLY' then
        begin
          SetNavigatorKind(Glw, nkWalker);
          MatrixWalker.PreferHomeUpForRotations := true;
          MatrixWalker.PreferHomeUpForMoving := false;
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
    if MenuPreferHomeUpForRotations <> nil then
      MenuPreferHomeUpForRotations.Checked := MatrixWalker.PreferHomeUpForRotations;
    if MenuPreferHomeUpForMoving <> nil then
      MenuPreferHomeUpForMoving.Checked := MatrixWalker.PreferHomeUpForMoving;

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

{ This loads the scene from file (using LoadAsVRML) and
  then inits our scene variables by LoadSceneCore.

  If it fails, it tries to preserve current scene
  (if it can't preserve current scene, only then it resets it to clear scene).
  Also, it shows the error message using MessageOK
  (so Glw must be already open). }
procedure LoadScene(const ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single;
  JumpToInitialViewpoint: boolean);

{ It's useful to undefine it only for debug purposes:
  FPC dumps then backtrace of where exception happened,
  which is often enough to trace the error.
  In release versions this should be defined to produce a nice
  message box in case of errors (instead of just a crash). }
{$define CATCH_EXCEPTIONS}

var
  RootNode: TVRMLNode;
  SavedSceneWarnings: TStringList;
begin
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
      RootNode := LoadAsVRML(ASceneFileName, true);
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
    LoadSceneCore(RootNode,
      ASceneFileName, SceneChanges, ACameraRadius, JumpToInitialViewpoint);
  {$ifdef CATCH_EXCEPTIONS}
  except
    on E: Exception do
    begin
      MessageOK(glw, 'Error while loading scene from "' +ASceneFileName+ '": ' +
        E.Message, taLeft);
      { In this case we cannot preserve old scene, because
        LoadSceneCore does FreeScene when it exits with exception
        (and that's because LoadSceneCore modifies some global scene variables
        when it works --- so when something fails inside LoadSceneCore,
        we are left with some partially-initiaized state,
        that is not usable; actually, LoadSceneCore
        also does FreeScene when it starts it's work --- to start
        with a clean state). }
      LoadClearScene;
      Exit;
    end;
  end;
  {$endif CATCH_EXCEPTIONS}

  RecentMenu.Add(ASceneFileName);

  { We call EventBeforeDraw to make Scene.PrepareRender to gather
    VRML warnings (because some warnings, e.g. invalid texture filename,
    are reported only from Scene.PrepareRender) }
  Glw.EventBeforeDraw;
  if SceneWarnings.Count <> 0 then
    MessageOK(Glw, Format('Note that there were %d warnings while loading ' +
      'this scene. See the console or use File->"View warnings" ' +
      'menu command to view them all.', [SceneWarnings.Count]), taLeft);
end;

{ This works like LoadScene, but loaded scene is an empty scene.
  More specifically, this calls FreeScene, and then inits
  "scene global variables" to some non-null values. }
procedure LoadClearScene;
begin
  SceneWarnings.Clear;

  { Note: Once I had an idea to use RootNode = nil for clear scene.
    This is not so entirely bad idea since I implemented TVRMLFlatScene
    in such way that RootNode = nil is allowed and behaves sensible.
    Disadvantages:
    - Many things in view3dscene
      use Scene.RootNode (e.g. when saving model to VRML using
      appropriate menu item). So using here RootNode = nil would make
      implementation of view3dscene more diffucult (I would have
      to be careful in many places and check whether Scene.RootNode <> nil),
      without any gains in functionality.
    - non-empty clear scene allows me to put there WorldInfo with a title.
      This way clear scene has an effect on view3dscene window's title,
      and at the same time I don't have to set SceneFileName to something
      dummy.

    I'm not constructing here RootNode in code (i.e. Pascal).
    This would allow a fast implementation, but it's easier for me to
    design scene in pure VRML and then auto-generate
    xxx_scene.inc file to load VRML scene from a simple string. }

  LoadSceneCore(
    ParseVRMLFileFromString({$I clear_scene.inc}, ''), '', [], 1.0, true);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
begin
  SceneWarnings.Clear;

  { See comments at LoadClearScene }
  LoadSceneCore(
    ParseVRMLFileFromString({$I welcome_scene.inc}, ''), '', [], 1.0, true);
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
  Version = '2.0.1';
  DisplayProgramName = 'view3dscene';

var
  MenuOctreeDisplayWhole: TMenuItemChecked;

procedure MenuCommand(glwin: TGLWindow; MenuItem: TMenuItem);

  procedure ChangeCameraUp;
  var Answer: string;
      NewUp, NewDir: TVector3Single;
  begin
   if Glw.Navigator is TMatrixWalker then
   begin
    Answer := '';
    if MessageInputQuery(glwin,
      'Input new camera home up vector (three float values) :'+nl+
      '(new vector must not be zero vector; '+
      'we will also change home direction to some other correct value; '+
      'we will also change current up and direction vectors to their '+
      '"home" values)',
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

     { evaluate NewDir; to kiepski pomysl zeby starac sie tutaj utrzymac
       NewDir takie jakie jest i np. zmieniac je tylko gdy
       VectorsParallel(NewUp, NewDir) - to dlatego ze wtedy w Glw.NavWalker.Init
       wektor up zostanie dostosowany do wektora dir, a tego nie chcemy. Musimy
       wiec sami dostosowac NewDir do NewUp tak zeby byly prostopadle.

       Wszystko co mozemy zrobic to zachowac dlugosc Glw.NavWalker.CameraDir,
       ktora determinuje przeciez szybkosc poruszania sie. }
     NewDir := Glw.NavWalker.HomeCameraDir;
     if not VectorsPerp(NewUp, NewDir) then
      NewDir := VectorAdjustToLength( AnyPerpVector(NewUp),
        VectorLen(Glw.NavWalker.CameraDir));

     { ustaw NewDir/Up. Ustaw tez aktualne Up i Dir na NewUp/Dir
       (zeby nie martwic sie o to jak sie ma aktualne dir to NewUp,
       czy nie sa rownolegle) }
     Glw.NavWalker.Init(Glw.NavWalker.CameraPos, NewDir, NewUp,
       Glw.NavWalker.CameraPreferredHeight,
       0.0 { CameraPreferredHeight is already corrected });
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
    Value := Scene.Attributes.PointSize;
    if MessageInputQuerySingle(Glwin, 'Change point size:',
      Value, taLeft) then
      Scene.Attributes.PointSize := Value;
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

       ShadowingItemIndex := Scene.DefaultTriangleOctree.SegmentCollision(
         SelectedPoint, SelectedItem^.State.ActiveLights.Items[i].TransfLocation,
           false, SelectedItemIndex, true, nil);

       if ShadowingItemIndex <> NoItemIndex then
       begin
        ShadowingItem := Scene.DefaultTriangleOctree.OctreeItems.
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

  procedure RemoveSelectedGeometry;
  begin
    if SelectedItem = nil then
    begin
      ShowAndWrite('Nothing selected.');
    end else
    begin
      SelectedItem^.ShapeNode.FreeRemovingFromAllParentNodes;

      Unselect;
      SceneOctreeFree;
      Scene.ChangedAll;
      SceneOctreeCreate;
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
    Scene.ChangedFields(ShapeNode);
    SceneOctreeCreate;
  end;

  procedure ChangeLightModelAmbient;
  begin
    if glwin.ColorDialog(LightModelAmbient) then LightModelAmbientChanged;
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
       if AnsiSameText(ExtractFileExt(SceneFilename), '.wrl') then
        s := AppendToFileName(SceneFilename, '_2') else
        s := ChangeFileExt(SceneFilename, '.wrl');
       if glwin.FileDialog('Save as VRML file', s, false) then
       try
        SaveToVRMLFile(Scene.RootNode, s,
          SavedVRMLPrecedingComment(SceneFileName));
       except
        on E: Exception do
        begin
         MessageOK(glw, 'Error while saving scene to "' +S+ '": '
            +E.Message, taLeft);
        end;
       end;
      end;

  21: ViewSceneWarnings;

  31: ChangeScene(scNoNormals, Scene);
  32: ChangeScene(scNoSolidObjects, Scene);
  33: ChangeScene(scNoConvexFaces, Scene);

  36: RemoveSelectedGeometry;
  37: RemoveSelectedFace;

  81: Wireframe := not Wireframe;
  82: ShowBBox := not ShowBBox;
  83: with Scene do Attributes.SmoothShading := not Attributes.SmoothShading;
  84: if glwin.ColorDialog(BGColor) then BGColorChanged;
  85: with Scene do Attributes.UseFog := not Attributes.UseFog;
  86: with Scene do Attributes.Blending := not Attributes.Blending;

  91: LightCalculate := not LightCalculate;
  92: HeadLight := not HeadLight;
  93: with Scene do Attributes.UseLights := not Attributes.UseLights;
  94: with Scene do Attributes.EnableTextures := not Attributes.EnableTextures;
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
        if octreeDisplayDepth > Scene.DefaultTriangleOctree.MaxDepth then
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
          octreeDisplayDepth := Scene.DefaultTriangleOctree.MaxDepth;
      end;

  99: begin
        OctreeDisplayWhole := not OctreeDisplayWhole;
        if not OctreeDisplayWhole then
          OctreeDisplayDepth := -1;
      end;

  101: Writeln(Scene.DefaultTriangleOctree.Statistics);
  103: Writeln(Scene.DefaultShapeStateOctree.Statistics);
  102: Scene.WritelnInfoNodes;

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

  106: Writeln(Format(
         'Current camera settings are :' +nl+
         'Viewpoint  {' +nl+
         '  position %s' +nl+
         '  orientation %s' +nl+
         '  # alternative representation of "orientation" :' +nl+
         '  # direction %s' +nl+
         '  # up %s' +nl+
         '}',
         [ VectorToRawStr(MatrixWalker.CameraPos),
           VectorToRawStr( CamDirUp2Orient(MatrixWalker.CameraDir,
             MatrixWalker.CameraUp) ),
           VectorToRawStr(MatrixWalker.CameraDir),
           VectorToRawStr(MatrixWalker.CameraUp) ]));

  108: Writeln(
         'Current viewing frustum planes :' +nl+
         '((A, B, C, D) means a plane given by equation A*x + B*y + C*z + D = 0.)' +nl+
         '  Left   : ' + VectorToRawStr(MatrixWalker.Frustum[fpLeft]) +nl+
         '  Right  : ' + VectorToRawStr(MatrixWalker.Frustum[fpRight]) +nl+
         '  Bottom : ' + VectorToRawStr(MatrixWalker.Frustum[fpBottom]) +nl+
         '  Top    : ' + VectorToRawStr(MatrixWalker.Frustum[fpTop]) +nl+
         '  Near   : ' + VectorToRawStr(MatrixWalker.Frustum[fpNear]) +nl+
         '  Far    : ' + VectorToRawStr(MatrixWalker.Frustum[fpFar]));

  110: if IsEmptyBox3d(Scene.BoundingBox) then
         MessageOK(Glw, 'The scene''s bounding box is empty.', taLeft) else
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
           [ Box3dToNiceStr(Scene.BoundingBox),
             VectorToRawStr(Box3dMiddle(Scene.BoundingBox)),
             FloatToRawStr(Scene.BoundingBox[1, 0] - Scene.BoundingBox[0, 0]),
             FloatToRawStr(Scene.BoundingBox[1, 1] - Scene.BoundingBox[0, 1]),
             FloatToRawStr(Scene.BoundingBox[1, 2] - Scene.BoundingBox[0, 2]) ]));
       end;

  111: ChangeNavigatorKind(glw, +1);

  121: ShowAndWrite(
         'Scene "' + SceneFilename + '" information:' + NL + NL +
          Scene.Info(true, true));
  122: ShowStatus := not ShowStatus;
  123: CollisionCheck := not CollisionCheck;
  124: ChangeCameraUp;
  125: if Glw.Navigator is TMatrixWalker then
        RaytraceToWin(glwin, Scene.DefaultTriangleOctree,
          Glw.NavWalker.CameraPos,
          Glw.NavWalker.CameraDir, Glw.NavWalker.CameraUp,
          AngleOfViewX, AngleOfViewY, BGColor,
          Scene.FogNode, Scene.FogDistanceScaling) else
        MessageOK(glwin, SOnlyInWalker);
  126: Glw.SwapFullScreen;
  127: glwin.SaveScreenDialog(FNameAutoInc('view3dscene_screen_%d.png'));
  128: begin
         MatrixWalker.MouseLook := not MatrixWalker.MouseLook;
         Glw.UpdateMouseLook;

         if MatrixWalker.MouseLook then
         begin
           MatrixWalker.Key_LeftStrafe := WalkerDefaultKey_LeftRot;
           MatrixWalker.Key_RightStrafe := WalkerDefaultKey_RightRot;
           MatrixWalker.Key_LeftRot := WalkerDefaultKey_LeftStrafe;
           MatrixWalker.Key_RightRot := WalkerDefaultKey_RightStrafe;
         end else
         begin
           MatrixWalker.Key_LeftStrafe := WalkerDefaultKey_LeftStrafe;
           MatrixWalker.Key_RightStrafe := WalkerDefaultKey_RightStrafe;
           MatrixWalker.Key_LeftRot := WalkerDefaultKey_LeftRot;
           MatrixWalker.Key_RightRot := WalkerDefaultKey_RightRot;
         end;
       end;

  131: begin
         ShowAndWrite(
           'view3dscene - extensive VRML and 3DS viewer.' + NL +
           'Version ' + Version + '.' + NL +
           'By Michalis Kamburelis.' + NL +
           NL +
           '[http://www.camelot.homedns.org/~michalis/view3dscene.php]' + NL +
           NL +
           'Compiled with ' + SCompilerDescription +'.');
       end;

  171: SelectedShowInformation;
  172: SelectedShowLightsInformation;

  182: ChangePointSize;

  201: MatrixWalker.Gravity := not MatrixWalker.Gravity;
  202: MatrixWalker.PreferHomeUpForRotations := not MatrixWalker.PreferHomeUpForRotations;
  203: MatrixWalker.PreferHomeUpForMoving := not MatrixWalker.PreferHomeUpForMoving;

  300..399:
    begin
      { We could just SetViewpoint, without swithing to nkWalker.
        But user usually wants to switch to nkWalker --- in nkExamine
        SetViewpoint is not visible at all. }
      SetNavigatorKind(Glw, nkWalker);
      SetViewpoint(MenuItem.IntData - 300);
    end;

  400..419: Scene.Attributes.BlendingSourceFactor :=
    BlendingFactors[MenuItem.IntData - 400].Value;
  420..439: Scene.Attributes.BlendingDestinationFactor :=
    BlendingFactors[MenuItem.IntData - 420].Value;

  1000..1099: SetColorModulatorType(
    TColorModulatorType(MenuItem.IntData-1000), Scene);
  1100..1199: SetTextureMinFilter(
    TTextureMinFilter  (MenuItem.IntData-1100), Scene);
  1200..1299: SetTextureMagFilter(
    TTextureMagFilter  (MenuItem.IntData-1200), Scene);
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
   M.Append(TMenuItem.Create('View _warnings about current scene', 21));
   NextRecentMenuItem := TMenuSeparator.Create;
   M.Append(NextRecentMenuItem);
   RecentMenu.NextMenuItem := NextRecentMenuItem;
   M.Append(TMenuItem.Create('_Exit',             12, CharEscape));
   Result.Append(M);
 M := TMenu.Create('_View');
   M.Append(TMenuItemChecked.Create('_Wireframe',              81, 'w',
     Wireframe, true));
   M.Append(TMenuItemChecked.Create('Show _bounding box',      82, 'b',
     ShowBBox, true));
   M.Append(TMenuItemChecked.Create('_Smooth shading',         83, 's',
     Scene.Attributes.SmoothShading, true));
   M.Append(TMenuItem.Create('Change background color ...',    84));
   M.Append(TMenuItemChecked.Create('_Fog',                    85, 'f',
     Scene.Attributes.UseFog, true));
   M.Append(TMenuItemChecked.Create('Blending',                86, CtrlB,
     Scene.Attributes.Blending, true));
   M2 := TMenu.Create('Blending source factor');
     AppendBlendingFactors(M2, true, 400);
     M.Append(M2);
   M2 := TMenu.Create('Blending destination factor');
     AppendBlendingFactors(M2, false, 420);
     M.Append(M2);
   M2 := TMenu.Create('Change scene colors');
     AppendColorModulators(M2);
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create(
     '_Lighting calculate (GL__LIGHTING enabled)',         91, 'l',
     LightCalculate, true));
   MenuHeadlight := TMenuItemChecked.Create('_Head light', 92, 'h',
     Headlight, true);
   M.Append(MenuHeadlight);
   M.Append(TMenuItemChecked.Create('Use scene lights',    93,
     Scene.Attributes.UseLights, true));
   M.Append(TMenuItem.Create('Light global ambient color ...',  95));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('_Textures',           94, 't',
     Scene.Attributes.EnableTextures, true));
   M2 := TMenu.Create('Change texture minification method');
     AppendTextureMinFilters(M2);
     M.Append(M2);
   M2 := TMenu.Create('Change texture magnification method');
     AppendTextureMagFilters(M2);
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('Show in Examine mode camera frustum', 96,
     ShowFrustum, true));
   M.Append(TMenuItemChecked.Create('When showing camera frustum, ' +
     'show it over all other objects (no depth test)', 180,
     ShowFrustumAlwaysVisible, true));
   M.Append(TMenuSeparator.Create);
   MenuOctreeDisplayWhole := TMenuItemChecked.Create('Show whole octree',
     99, OctreeDisplayWhole, true);
   M.Append(MenuOctreeDisplayWhole);
   M.Append(TMenuItem.Create('Show _upper level of octree based on triangles', 97, CtrlU));
   M.Append(TMenuItem.Create('Show _lower level of octree based on triangles', 98, CtrlD));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Set point size ...', 182));
   Result.Append(M);
 M := TMenu.Create('_Navigation');
   ViewpointsList.MenuJumpToViewpoint := TMenu.Create('Jump to viewpoint');
     ViewpointsList.MakeMenuJumpToViewpoint;
     M.Append(ViewpointsList.MenuJumpToViewpoint);
   M2 := TMenu.Create('Change navigation method');
     AppendNavigationTypes(M2);
     M2.Append(TMenuSeparator.Create);
     M2.Append(TMenuItem.Create('Next',                      111, 'v'));
     M.Append(M2);
  M.Append(TMenuItemChecked.Create(
     '_Use mouse look (in Walk mode)',                       128, 'm',
       MatrixWalker.MouseLook, true));
   M.Append(TMenuItemChecked.Create(
     '_Collision checking (in Walk mode)',                   123, 'c',
       CollisionCheck, true));
   MenuGravity := TMenuItemChecked.Create(
     '_Gravity (in Walk mode)',                              201, 'g',
     MatrixWalker.Gravity, true);
   M.Append(MenuGravity);
   MenuPreferHomeUpForRotations := TMenuItemChecked.Create(
     'Rotations are versus stable (initial) camera up',      202,
     MatrixWalker.PreferHomeUpForRotations, true);
   M.Append(MenuPreferHomeUpForRotations);
   MenuPreferHomeUpForMoving := TMenuItemChecked.Create(
     'Moving is versus stable (initial) camera up',          203,
     MatrixWalker.PreferHomeUpForMoving, true);
   M.Append(MenuPreferHomeUpForMoving);
   M.Append(TMenuItem.Create('Change camera up vector ...',  124));
   Result.Append(M);
 M := TMenu.Create('_Edit');
   M2 := TMenu.Create('_Whole scene changes');
   M2.Append(TMenuItem.Create(
     'Remove normals info from scene (forces normals to be calculated)',
      31));
   M2.Append(TMenuItem.Create('Mark all shapes as '+
     'non-solid (disables any backface culling)', 32));
   M2.Append(TMenuItem.Create('Mark all faces as '+
     'non-convex (forces faces to be triangulated carefully)', 33));
   M.Append(M2);
   MenuRemoveSelectedGeometry :=
     TMenuItem.Create('Remove _geometry node (containing selected triangle)', 36);
   M.Append(MenuRemoveSelectedGeometry);
   MenuRemoveSelectedFace :=
     TMenuItem.Create('Remove _face (containing selected triangle)', 37);
   M.Append(MenuRemoveSelectedFace);
   Result.Append(M);
 M := TMenu.Create('_Console');
   M.Append(TMenuItem.Create('Print statistics of octree based on _triangles', 101));
   M.Append(TMenuItem.Create('Print statistics of octree based on _ShapeStates', 103));
   M.Append(TMenuItem.Create('Print VRML _Info nodes',        102));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print _rayhunter command-line to render this view',
     105, CtrlR));
   M.Append(TMenuItem.Create('Print current camera _node',    106, CtrlC));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print current camera _frustum', 108));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Print scene _bounding box as VRML node', 110));
   Result.Append(M);
 M := TMenu.Create('_Other');
   M.Append(TMenuItem.Create('_Raytrace !',                   125, 'r'));
   M.Append(TMenuItemChecked.Create('_FullScreen',            126, K_F11,
     Glw.FullScreen, true));
   M.Append(TMenuItem.Create('_Save screen to PNG',           127, K_F5));
   Result.Append(M);
 M := TMenu.Create('_Help');
   M.Append(TMenuItemChecked.Create('Show status _text',           122, K_F1,
      ShowStatus, true));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Scene information',                  121));
   MenuSelectedInfo :=
     TMenuItem.Create('Selected object information',               171);
   M.Append(MenuSelectedInfo);
   MenuSelectedLightsInfo :=
     TMenuItem.Create('Selected object lights information',        172);
   UpdateSelectedEnabled;
   M.Append(MenuSelectedLightsInfo);
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
           SCamelotProgramHelpSuffix(DisplayProgramName, Version, true));
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
  Param_RendererOptimization: TGLRendererOptimization = roSeparateShapeStates;
  Helper: THelper;
begin
 { parse parameters }
 { glw params }
 Glw.ParseParameters(StandardParseOptions);
 { our params }
 MultiNavigatorsParseParameters;
 LightsParseParameters;
 VRMLNodesDetailOptionsParse;
 RendererOptimizationOptionsParse(Param_RendererOptimization);
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
 Scene := TVRMLFlatSceneGL.Create(nil, false, Param_RendererOptimization);
 try
  { in view3dscene Scene.Attributes.UseLights default value is true }
  Scene.Attributes.UseLights := true;
  InitColorModulator(Scene);
  InitTextureFilters(Scene);

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

   Glw.SetDemoOptions(K_None, #0, true);

   Glw.Init;

   if WasParam_SceneFileName then
     LoadScene(Param_SceneFileName, Param_SceneChanges, Param_CameraRadius, true) else
     LoadWelcomeScene;

   Glwm.Loop;
  finally FreeScene end;
 finally
   FreeAndNil(Scene);
   FreeAndNil(SceneWarnings);
   if RecentMenu <> nil then
     RecentMenu.SaveToConfig(ConfigFile, 'recent_files');
   FreeAndNil(RecentMenu);
 end;
end.

{
  Local Variables:
  kam-compile-release-command-win32: "clean_glwindow_unit; fpcreleaseb"
  kam-compile-release-command-unix: "clean_glwindow_unit; fpcreleaseb"
  End:
}
