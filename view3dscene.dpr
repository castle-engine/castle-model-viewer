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
  GLWinMessages, ProgressGL,
  { VRML (and possibly OpenGL) related units: }
  VRMLFields, KambiOctree, VRMLTriangleOctree, VRMLShapeStateOctree,
  VRMLNodes, Object3dAsVRML, VRMLFlatSceneGL,
  VRMLFlatScene, VRMLRayTracer, BackgroundGL, VRMLNodesDetailOptions,
  VRMLCameraUtils, VRMLErrors,
  { view3dscene-specific units: }
  TextureFilters, ColorModulators, V3DSceneLights, RaytraceToWindow,
  MultiNavigators, SceneChangesUnit, BGColors, V3DSceneCamera;

var
  Wireframe: boolean = false;
  OctreeDisplayDepth: integer = -1; { -1 means "don't display octree" }
  ShowStatus: boolean = true;
  ShowBBox: boolean = true;

  ShowFrustum: boolean = false;
  ShowFrustumAlwaysVisible: boolean = false;

  PickingMessageShowTexture: boolean = false;
  PickingMessageShowMaterial: boolean = false;
  PickingMessageShowShadows: boolean = false;

  { ponizsze zmienne istotne tylko w trybach nawigacji ktore robia
    wykrywanie kolizji: }
  CollisionCheck: boolean = true;

  { ustalane w Init, finalizowane w Close }
  StatusFont: TGLBitmapFont;

  AngleOfViewX: Single = 60;

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

  function OctreeDisplayDepthToStr: string;
  begin
   if OctreeDisplayDepth <> -1 then
    result := IntToStr(OctreeDisplayDepth) else
    result := 'none';
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

  strs.Append(Format('Camera Kind : %s, Octree display depth : %s',
    [ VRMLCameraKindToStr[CameraKind],
      OctreeDisplayDepthToStr ]));

  if SceneLightsCount = 0 then
   s := '(useless, scene has no lights)' else
   s := BoolToStrOO[Scene.Attributes.UseLights];
  strs.Append(Format('Use scene lights: %s, Color modulator : %s',
    [ s, ColorModulatorInfos[ColorModulatorType].Name ]));

  strs.Append(Format('Texture min filter : %s, mag filter %s',
    [ TextureMinFilterNames[TextureMinFilter],
      TextureMagFilterNames[TextureMagFilter] ]));

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

  procedure DisplayOctree(octreenode: TOctreeNode; octreeDisplayDepth: integer);
  var b0, b1, b2: boolean;
  begin
   with octreenode do
   if Depth = octreeDisplayDepth then
   begin
    if not (IsLeaf and (ItemsCount = 0)) then DrawGLBoxWire(Box, 0, 0, 0, true);
   end else
   if not IsLeaf then
   begin
    for b0 := false to true do
     for b1 := false to true do
      for b2 := false to true do
       DisplayOctree(TreeSubNodes[b0, b1, b2], octreeDisplayDepth);
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

 if octreeDisplayDepth >= 0 then
 begin
  glColorv(Yellow3Single);
  DisplayOctree(Scene.DefaultTriangleOctree.TreeRoot, octreeDisplayDepth);
 end;

 if showBBox then
 begin
  glColorv(Green3Single);
  if not IsEmptyBox3d(Scene.BoundingBox) then
   DrawGLBoxWire(Scene.BoundingBox, 0, 0, 0, true);
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

 if ShowStatus then
 begin
  { Note that DrawStatus changes current modelview matrix,
    so you want to always leave drawing status at the end of this Draw
    procedure. }
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glProjectionPushPopOrtho2D(DrawStatus, 0, 0, Glwin.Width, 0, Glwin.Height);
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

  function NodeNiceName(node: TVRMLNode): string;
  begin
   result := ''''+node.NodeName+''' (class '''+node.NodeTypeName+''')';
  end;

var Ray0, RayVector: TVector3Single;
    IntersectItemIndex: integer;
    IntersectItem: POctreeItem;
    Intersection: TVector3Single;
    s, TextureDescription: string;
    VCOver, TCOver, VCNotOver, TCNotOver: Cardinal;

    { used only when PickingMessageShowShadows = true }
    i: integer;
    ShadowingItem: POctreeItem;
    ShadowingItemIndex: Integer;

begin
 if btn = mbLeft then
 begin
  if not (Glw.Navigator is TMatrixWalker) then
   begin MessageOK(glwin, SOnlyInWalker); Exit end;

  Glw.MousePickedRay(AngleOfViewX, AngleOfViewY, Ray0, RayVector);

  Scene.DefaultTriangleOctree.DirectCollisionTestsCounter := 0;

  IntersectItemIndex :=
    Scene.DefaultTriangleOctree.RayCollision(
      Intersection, Ray0, RayVector, true, NoItemIndex, false, nil);

  if IntersectItemIndex = NoItemIndex then
   s := 'No object picked.' else
  begin
   IntersectItem := Scene.DefaultTriangleOctree.OctreeItems.
     Pointers[IntersectItemIndex];
   s := Format(
        'You picked point %s from triangle %s.' +nl+
        nl+
        'This triangle is part of the '+
        'node named %s. Node''s bounding box is %s. ',
        [VectorToNiceStr(Intersection),
         TriangleToNiceStr(IntersectItem.Triangle),
         NodeNiceName(IntersectItem.ShapeNode),
         Box3dToNiceStr(IntersectItem.ShapeNode.BoundingBox(IntersectItem.State))]);

   VCNotOver := IntersectItem.ShapeNode.VerticesCount(IntersectItem.State, false);
   TCNotOver := IntersectItem.ShapeNode.TrianglesCount(IntersectItem.State, false);
   VCOver := IntersectItem.ShapeNode.VerticesCount(IntersectItem.State, true);
   TCOver := IntersectItem.ShapeNode.TrianglesCount(IntersectItem.State, true);

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

   if PickingMessageShowTexture then
   begin
     if IntersectItem.State.Texture = nil then
       TextureDescription := 'none' else
       TextureDescription := IntersectItem.State.Texture.TextureDescription;
     S += Format(nl +nl+
           'Node''s texture : %s.', [TextureDescription]);
   end;

   if PickingMessageShowMaterial then
   begin
    { TODO: this is VRML 1.0-specific, fix }
    s += Format(nl +nl+
           'Material name : %s' +nl+
           'Material ambient[0] : %s' +nl+
           'Material diffuse[0] : %s' +nl+
           'Material specular[0] : %s' +nl+
           'Material shininess[0] : %s' +nl+
           'Material transparency[0] : %s',
           [IntersectItem.State.LastNodes.Material.NodeName,
            VectorToNiceStr(IntersectItem.State.LastNodes.Material.AmbientColor3Single(0)),
            VectorToNiceStr(IntersectItem.State.LastNodes.Material.DiffuseColor3Single(0)),
            VectorToNiceStr(IntersectItem.State.LastNodes.Material.SpecularColor3Single(0)),
            FloatToNiceStr(IntersectItem.State.LastNodes.Material.ShininessExp(0)),
            FloatToNiceStr(IntersectItem.State.LastNodes.Material.Transparency(0)) ]);
   end;

   if PickingMessageShowShadows then
   begin
    for i := 0 to IntersectItem.State.ActiveLights.Count - 1 do
    begin
     s += nl+ nl+ Format('Light node %s possibly affects node ... ',
       [ NodeNiceName(IntersectItem.State.ActiveLights[i].LightNode) ]);

     ShadowingItemIndex := Scene.DefaultTriangleOctree.SegmentCollision(
       Intersection, IntersectItem.State.ActiveLights[i].TransfLocation,
         false, IntersectItemIndex, true, nil);

     if ShadowingItemIndex <> NoItemIndex then
     begin
      ShadowingItem := Scene.DefaultTriangleOctree.OctreeItems.
        Pointers[ShadowingItemIndex];
      s += Format('but no, this light is blocked by triangle %s from node %s.',
        [ TriangleToNiceStr(ShadowingItem.Triangle),
          NodeNiceName(ShadowingItem.ShapeNode) ])
     end else
      s += 'hmm, yes ! No object blocks this light here.';
    end;
   end;

  end;
  s += Format(nl+ nl+
    '%d tests for collisions between ray and triangles were needed to learn this.',
    [Scene.DefaultTriangleOctree.DirectCollisionTestsCounter]);
  MessageOK(glwin, s, taLeft);
 end;
end;

{ TMatrixWalker collision detection using SceneTriangleOctree --------------- }

type
  TDummy = class
    class function MoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    class procedure GetCameraHeight(Navigator: TMatrixWalker;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
  end;

class function TDummy.MoveAllowed(Navigator: TMatrixWalker;
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

class procedure TDummy.GetCameraHeight(Navigator: TMatrixWalker;
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

procedure VRMLNonFatalError_Warning(const s: string);
begin
  Writeln(ProgramName + ': WARNING: ' + S);
  SceneWarnings.Append(S);
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
 Scene.DefaultTriangleOctree.Free;
 Scene.DefaultTriangleOctree := nil;

 Scene.DefaultShapeStateOctree.Free;
 Scene.DefaultShapeStateOctree := nil;

 Scene.RootNode.Free;
 Scene.RootNode := nil;

 Scene.ChangedAll;

 ViewpointsList.Count := 0;
 if ViewpointsList.MenuJumpToViewpoint <> nil then
   ViewpointsList.MakeMenuJumpToViewpoint;

 NavigationNode := nil;
 ViewpointNode := nil;

 SceneFileName := '';

 SceneWarnings.Clear;
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
  So do not Free RootNode after using this procedure. }
procedure LoadSceneCore(RootNode: TVRMLNode;
  const ASceneFileName: string;
  const SceneChanges: TSceneChanges; const ACameraRadius: Single);
var
  NewCaption: string;
  CameraPreferredHeight: Single;
  WorldInfoNode: TNodeWorldInfo;
  I: Integer;
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

    SceneInitMultiNavigators(Scene.BoundingBox,
      StdVRMLCamPos_1, StdVRMLCamDir, StdVRMLCamUp,
      CameraPreferredHeight, CameraRadius);

    { calculate ViewpointsList, MenuJumpToViewpoint,
      and jump to 1st viewpoint (or to the default cam settings). }
    Scene.EnumerateViewpoints(ViewpointsList.AddNodeTransform);
    if ViewpointsList.MenuJumpToViewpoint <> nil then
      ViewpointsList.MakeMenuJumpToViewpoint;
    SetViewpoint(0);

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
      Glw.OnDraw := Draw;
      Glw.OnBeforeDraw := BeforeDraw;
    end;

    if not Glw.Closed then
    begin
      { call EventResize to adjust zNear/zFar of our projection to the size
        of Scene.BoundingBox }
      Glw.EventResize;
      Glw.PostRedisplay;
    end;

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
  const SceneChanges: TSceneChanges; const ACameraRadius: Single);

{ It's useful to undefine it only for debug purposes:
  FPC dumps then backtrace of where exception happened,
  which is often enough to trace the error.
  In release versions this should be defined to produce a nice
  message box in case of errors (instead of just a crash). }
{$define CATCH_EXCEPTIONS}

var
  RootNode: TVRMLNode;
begin
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
      Exit;
    end;
  end;
  {$endif CATCH_EXCEPTIONS}

  {$ifdef CATCH_EXCEPTIONS}
  try
  {$endif CATCH_EXCEPTIONS}
    LoadSceneCore(RootNode,
      ASceneFileName, SceneChanges, ACameraRadius);
  {$ifdef CATCH_EXCEPTIONS}
  except
    on E: Exception do
    begin
      MessageOK(glw, 'Error while loading scene from "' +ASceneFileName+ '": ' +
        E.Message, taLeft);
      LoadClearScene;
      Exit;
    end;
  end;
  {$endif CATCH_EXCEPTIONS}

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
var RootNode: TVRMLNode;
begin
 { Note: once I had an idea to use here RootNode = nil.
   This is not so entirely bad idea since I implemented TVRMLFlatScene
   in such way that RootNode = nil is allowed and behaves sensible.
   But it's also not so good idea because many things in view3dscene
   use Scene.RootNode (e.g. when saving model to VRML using
   appropriate menu item). So using here RootNode = nil would make
   implementation of view3dscene more diffucult (I would have
   to be careful in many places and check whether Scene.RootNode <> nil),
   without any gains in functionality. }
 RootNode := TNodeGroup_1.Create('', '');
 LoadSceneCore(RootNode, 'clear_scene.wrl', [], 1.0);
end;

{ like LoadClearScene, but this loads a little more complicated scene.
  It's a "welcome scene" of view3dscene. }
procedure LoadWelcomeScene;
var RootNode: TVRMLNode;
    Stream: TPeekCharStream;
begin
 { I'm not constructing here RootNode in code (i.e. Pascal).
   This would allow a fast implementation, but it's easier for me to
   design welcome_scene.wrl in pure VRML and then auto-generate
   welcome_scene.inc file to load VRML scene from a simple string. }
 Stream := TSimplePeekCharStream.Create(
   TStringStream.Create({$I welcome_scene.inc}), true);
 try
  RootNode := ParseVRMLFile(Stream, '');
 finally FreeAndNil(Stream) end;

 LoadSceneCore(RootNode, 'welcome_scene.wrl', [], 1.0);
end;

function SavedVRMLPrecedingComment(const SourceFileName: string): string;
begin
 Result := 'VRML generated by view3dscene by Kambi from ' +SourceFileName +
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

{ menu things ------------------------------------------------------------ }

const
  Version = '1.2.5';
  DisplayProgramName = 'view3dscene';

var
  SceneChanges: TSceneChanges = [];

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

  procedure InvertSceneChanges(SC: TSceneChange);
  begin
   if SC in SceneChanges then
    Exclude(SceneChanges, SC) else
    Include(SceneChanges, SC);
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
      S.Assign(SceneWarnings);
      S.Append(Format('Total %d warnings about current scene "%s":',
        [ SceneWarnings.Count, SceneFileName ]));
      S.Append('');
      S.AddStrings(SceneWarnings);
      MessageOK(Glw, S, taLeft);
    finally FreeAndNil(S) end;
  end;

var s: string;
begin
 case MenuItem.IntData of
  10: begin
       s := ExtractFilePath(SceneFilename);
       if glwin.FileDialog('Open file', s, true) then
         LoadScene(s, SceneChanges, 0.0);
      end;
  11: begin
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
  12: Glw.Close;

  21: ViewSceneWarnings;

  31: InvertSceneChanges(scNoNormals);
  32: InvertSceneChanges(scNoSolidObjects);
  33: InvertSceneChanges(scNoConvexFaces);

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
  96: ShowFrustum := not ShowFrustum;
  180: ShowFrustumAlwaysVisible := not ShowFrustumAlwaysVisible;
  97: begin
       Inc(octreeDisplayDepth);
       if octreeDisplayDepth > Scene.DefaultTriangleOctree.MaxDepth then
        octreeDisplayDepth := -1;
      end;
  98: begin
       Dec(octreeDisplayDepth);
       if octreeDisplayDepth < -1 then
        octreeDisplayDepth := Scene.DefaultTriangleOctree.MaxDepth;
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
           'DEF BoundingBox Separator {' +nl+
           '  Translation {' +nl+
           '    translation %s' +nl+
           '  }' +nl+
           '  Cube {' +nl+
           '    width %s' +nl+
           '    height %s' +nl+
           '    depth %s' +nl+
           '  }' +nl+
           '}',
           [ VectorToRawStr(Box3dMiddle(Scene.BoundingBox)),
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

  161: PickingMessageShowTexture := not PickingMessageShowTexture;
  162: PickingMessageShowMaterial := not PickingMessageShowMaterial;
  163: PickingMessageShowShadows := not PickingMessageShowShadows;

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
var
  M, M2: TMenu;
  Cmt: TColorModulatorType;
  TexMin: TTextureMinFilter;
  TexMag: TTextureMagFilter;
  NavKind: TNavigatorKind;
begin
 Result := TMenu.Create('Main menu');
 M := TMenu.Create('_File');
   M.Append(TMenuItem.Create('_Open ...',         10, CtrlO));
   M.Append(TMenuItem.Create('_Save as VRML ...', 11));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('View _warnings about current scene', 21));
   M.Append(TMenuSeparator.Create);
   M2 := TMenu.Create('_Configure scene loading');
     M2.Append(TMenuItemChecked.Create(
       'Remove normals info from scene (no-_normals)',  31,
       scNoNormals in SceneChanges, true));
     M2.Append(TMenuItemChecked.Create('Force all shapes to be marked as '+
       'possibly non-solid (no-_solid-objects)',        32,
       scNoSolidObjects in SceneChanges, true));
     M2.Append(TMenuItemChecked.Create('Force all faces to be marked as '+
       'possibly non-convex (no-_convex-faces)',        33,
       scNoConvexFaces in SceneChanges, true));
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
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
   M2 := TMenu.Create('Change scene colors');
     for Cmt := Low(Cmt) to High(Cmt) do
      M2.Append(TMenuItem.Create(
        SQuoteMenuEntryCaption(ColorModulatorInfos[Cmt].Name),
        Ord(Cmt) + 1000));
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
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('_Textures',           94, 't',
     Scene.Attributes.EnableTextures, true));
   M2 := TMenu.Create('Change texture minification method');
     for TexMin := Low(TexMin) to High(TexMin) do
      M2.Append(TMenuItem.Create(
        SQuoteMenuEntryCaption(TextureMinFilterNames[TexMin]),
        Ord(TexMin) + 1100));
     M.Append(M2);
   M2 := TMenu.Create('Change texture magnification method');
     for TexMag := Low(TexMag) to High(TexMag) do
      M2.Append(TMenuItem.Create(
        SQuoteMenuEntryCaption(TextureMagFilterNames[TexMag]),
        Ord(TexMag) + 1200));
     M.Append(M2);
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItemChecked.Create('Show in Examine mode camera frustum', 96,
     ShowFrustum, true));
   M.Append(TMenuItemChecked.Create('When showing camera frustum, ' +
     'show it over all other objects (no depth test)', 180,
     ShowFrustumAlwaysVisible, true));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('Show _upper level of octree based on triangles', 97, CtrlU));
   M.Append(TMenuItem.Create('Show _lower level of octree based on triangles', 98, CtrlD));
   Result.Append(M);
 M := TMenu.Create('_Navigation');
   ViewpointsList.MenuJumpToViewpoint := TMenu.Create('Jump to viewpoint');
     ViewpointsList.MakeMenuJumpToViewpoint;
     M.Append(ViewpointsList.MenuJumpToViewpoint);
   M2 := TMenu.Create('Change navigation method');
     for NavKind := Low(NavKind) to High(NavKind) do
      M2.Append(TMenuItem.Create(NavigatorNames[NavKind],
        Ord(NavKind) + 1300));
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
   M.Append(TMenuItem.Create('Scene information',                  121));
   M.Append(TMenuItemChecked.Create('Show status _text',           122, K_F1,
      ShowStatus, true));
   M2 := TMenu.Create('When picking with left mouse button, show ...');
     M2.Append(TMenuItemChecked.Create('_Texture info',            161,
       PickingMessageShowTexture, true));
     M2.Append(TMenuItemChecked.Create('_Material info',           162,
       PickingMessageShowMaterial, true));
     M2.Append(TMenuItemChecked.Create('_Lights and shadows info', 163,
       PickingMessageShowShadows, true));
     M.Append(M2);
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
    3 : Include(SceneChanges, scNoNormals);
    4 : Include(SceneChanges, scNoSolidObjects);
    5 : Include(SceneChanges, scNoConvexFaces);
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
begin
 { parse parameters }
 { glw params }
 Glw.ParseParameters(StandardParseOptions);
 { our params }
 MultiNavigatorsParseParameters;
 LightsParseParameters;
 VRMLNodesDetailOptionsParse;
 RendererOptimizationOptionsParse(Param_RendererOptimization);
 ParseParameters(Options, OptionProc, nil);
 { the most important param : filename to load }
 if Parameters.High > 1 then
  raise EInvalidParams.Create('Excessive command-line parameters. '+
    'Expected at most one filename to load') else
 if Parameters.High = 1 then
 begin
  WasParam_SceneFileName := true;
  Param_SceneFileName := Parameters[1];
 end;

 VRMLNonFatalError := VRMLNonFatalError_Warning;

 if WasParam_WriteToVRML then
 begin
  if not WasParam_SceneFileName then
   raise EInvalidParams.Create('You used --write-to-vrml option, '+
     'this means that you want to convert some 3d model file to VRML. ' +
     'But you didn''t provide any filename on command-line to load.');
  WriteToVRML(Param_SceneFileName, SceneChanges);
  Exit;
 end;

 { This is for loading default clean scene.
   LoadClearScene should be lighting fast always,
   so progress should not be needed in this case anyway
   (and we don't want to clutter stdout). }
 Progress.UserInterface := ProgressNullInterface;

 InitMultiNavigators(glw, TDummy.MoveAllowed, TDummy.GetCameraHeight);

 SceneWarnings := TStringList.Create;

 { init "scene global variables" to null values }
 Scene := TVRMLFlatSceneGL.Create(nil, false, Param_RendererOptimization);
 try
  { in view3dscene Scene.Attributes.UseLights default value is true }
  Scene.Attributes.UseLights := true;
  InitColorModulator(Scene);
  InitTextureFilters(Scene);

  { init "scene global variables" to non-null values }
  LoadClearScene;
  try

   GLWinMessagesTheme := GLWinMessagesTheme_TypicalGUI;

   Glw.MainMenu := CreateMainMenu;
   Glw.OnMenuCommand := MenuCommand;
   Glw.OnResize := Resize;
   Glw.OnInit := Init;
   Glw.OnClose := Close;
   Glw.OnMouseDown := MouseDown;
   Glw.OnBeforeDraw := BeforeDraw;
   Glw.OnDraw := Draw;

   Glw.SetDemoOptions(K_None, #0, true);

   Glw.Init;

   if WasParam_SceneFileName then
     LoadScene(Param_SceneFileName, SceneChanges, Param_CameraRadius) else
     LoadWelcomeScene;

   Glwm.Loop;
  finally FreeScene end;
 finally
   FreeAndNil(Scene);
   FreeAndNil(SceneWarnings);
 end;
end.

{
  Local Variables:
  kam-compile-release-command-win32: "clean_glwindow_unit; fpcreleaseb"
  kam-compile-release-command-unix: "clean_glwindow_unit; fpcreleaseb"
  End:
}
