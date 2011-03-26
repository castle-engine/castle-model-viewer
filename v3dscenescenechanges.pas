{
  Copyright 2003-2011 Michalis Kamburelis.

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

{ }
unit V3DSceneSceneChanges;

{ This unit honours KAMBI_HAS_NURBS symbol, avoiding using nurbs nodes
  if not available. But note that *view3dscene (including this unit)
  is still GPL-only (strict), not LGPL*. So it could actually safely
  assume that nurbs nodes are available, and KAMBI_HAS_NURBS is always defined...

  The possibility to work without KAMBI_HAS_NURBS is purely for testing. }
{$I kambiconf.inc}

interface

uses VRMLScene, VRMLNodes, KambiUtils, VRMLGLAnimation;

type
  { When adding new item to TSceneChange you can also consider adding
    new option to view3dscene (--scene-change-).
    All other necessary changes after extending TSceneChange will be enforced
    by the compiler.

    See [http://vrmlengine.sourceforge.net/view3dscene.php#section_command_line_options]
    for detailed spec what these scene changes do. }
  TSceneChange = (
    scNoNormals,
    scNoSolidObjects,
    scNoConvexFaces);
  TSceneChanges = set of TSceneChange;

{ Do all scene changes in SceneChanges.
  Changes will be done in the order they are declared in type TSceneChange. }
procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLScene);

procedure ChangeSceneAnimation(SceneChanges: TSceneChanges;
  Scene: TVRMLGLAnimation);

implementation

uses SysUtils;

{ TSceneChangesDo -------------------------------------------------- }

type
  TSceneChangesDo = class
  public
    procedure NoNormal_Indexed_1(node: TVRMLNode);
    procedure NoNormal_IFS_2(node: TVRMLNode);
    procedure NoNormal_ElevationGrid(node: TVRMLNode);

    procedure NoSolid_ShapeHints(node: TVRMLNode);
    procedure NoSolid_X3DComposedGeometry(node: TVRMLNode);
    procedure NoSolid_Extrusion(node: TVRMLNode);
    procedure NoSolid_ElevationGrid(node: TVRMLNode);
    procedure NoSolid_Box(node: TVRMLNode);
    procedure NoSolid_Cone(node: TVRMLNode);
    procedure NoSolid_Cylinder(node: TVRMLNode);
    procedure NoSolid_Sphere(node: TVRMLNode);
    procedure NoSolid_Text(node: TVRMLNode);
    procedure NoSolid_Text3D(node: TVRMLNode);
    {$ifdef KAMBI_HAS_NURBS}
    procedure NoSolid_X3DNurbsSurfaceGeometryNode(Node: TVRMLNode);
    procedure NoSolid_NurbsSweptSurface(Node: TVRMLNode);
    procedure NoSolid_NurbsSwungSurface(Node: TVRMLNode);
    procedure NoSolid_NurbsSurface(Node: TVRMLNode);
    {$endif KAMBI_HAS_NURBS}

    procedure NoConvex_ShapeHints(node: TVRMLNode);
    procedure NoConvex_IFS_2(node: TVRMLNode);
    procedure NoConvex_Extrusion(node: TVRMLNode);
  end;

procedure TSceneChangesDo.NoNormal_Indexed_1(node: TVRMLNode);
begin
  (Node as TVRMLIndexedNode_1).FdNormalIndex.Items.SetLength(0);
end;

procedure TSceneChangesDo.NoNormal_IFS_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdNormal.Value := nil;
end;

procedure TSceneChangesDo.NoNormal_ElevationGrid(node: TVRMLNode);
begin
  (Node as TNodeElevationGrid).FdNormal.Value := nil;
end;

procedure TSceneChangesDo.NoSolid_ShapeHints(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdShapeType.Value := SHTYPE_UNKNOWN;
end;

procedure TSceneChangesDo.NoSolid_X3DComposedGeometry(node: TVRMLNode);
begin
  (Node as TNodeX3DComposedGeometryNode).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Extrusion(node: TVRMLNode);
begin
  (Node as TNodeExtrusion).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_ElevationGrid(Node: TVRMLNode);
begin
  (Node As TNodeElevationGrid).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Box(Node: TVRMLNode);
begin
  (Node As TNodeBox).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Cone(Node: TVRMLNode);
begin
  (Node As TNodeCone_2).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Cylinder(Node: TVRMLNode);
begin
  (Node As TNodeCylinder_2).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Sphere(Node: TVRMLNode);
begin
  (Node As TNodeSphere_2).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Text(Node: TVRMLNode);
begin
  (Node As TNodeText).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Text3D(Node: TVRMLNode);
begin
  (Node As TNodeText3D).FdSolid.Value := false;
end;

{$ifdef KAMBI_HAS_NURBS}

procedure TSceneChangesDo.NoSolid_X3DNurbsSurfaceGeometryNode(Node: TVRMLNode);
begin
  (Node As TNodeX3DNurbsSurfaceGeometryNode).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_NurbsSweptSurface(Node: TVRMLNode);
begin
  (Node As TNodeNurbsSweptSurface).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_NurbsSwungSurface(Node: TVRMLNode);
begin
  (Node As TNodeNurbsSwungSurface).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_NurbsSurface(Node: TVRMLNode);
begin
  (Node As TNodeNurbsSurface).FdSolid.Value := false;
end;

{$endif KAMBI_HAS_NURBS}

procedure TSceneChangesDo.NoConvex_ShapeHints(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdFaceType.Value := FACETYPE_UNKNOWN;
end;

procedure TSceneChangesDo.NoConvex_IFS_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdConvex.Value := false;
end;

procedure TSceneChangesDo.NoConvex_Extrusion(node: TVRMLNode);
begin
  (Node as TNodeExtrusion).FdConvex.Value := false;
end;

{ SceneChange_Xxx functions ---------------------------------------- }

procedure RemoveNodeClass(RootNode: TVRMLNode;
  NodeClass: TVRMLNodeClass; onlyFromActivePart: boolean);
var node: TVRMLNode;
begin
 {usuwania node'ow ze sceny lepiej nie robic przez EnumNodes -
  nie mozemy zaburzac hierarchii sceny w czasie jej przegladania.}
 repeat
  node := RootNode.TryFindNode(NodeClass, onlyFromActivePart);
  if node = nil then break;
  node.FreeRemovingFromAllParents;
 until false;
end;

{ obie procedury SceneChange_RemoveNormals i SceneChange_MakeNoObjectsSolid
  moglyby zostac napisane tak zeby nie wywolywac od razu scene.ChangedAll
  tylko jakies bardziej konkretne Changed* (a wiec tez powodujace mniejsza
  strate czasu na odtworzenie precalculated czesci sceny).

  I byc moze kiedys to zrobie. Ale na razie o to nie dbam i nawet nie jest
  to dla mnie sor-ry bo do niczego mi szybkosc ponizszych procedur nie jest
  potrzebna. }

procedure SceneChange_NoNormals(scene: TVRMLScene);
const onlyFromActivePart = false;
var
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    Scene.RootNode.EnumerateNodes(TVRMLIndexedNode_1,
      @DoChanges.NoNormal_Indexed_1, onlyFromActivePart);
    Scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      @DoChanges.NoNormal_IFS_2, onlyFromActivePart);
    Scene.RootNode.EnumerateNodes(TNodeElevationGrid,
      @DoChanges.NoNormal_ElevationGrid, onlyFromActivePart);
  finally FreeAndNil(DoChanges) end;

  { Do this at the end.
    Note that for VRML >= 2.0, Normal nodes were already removed by
    NoNormal_IFS_2 (in more intelligent way). }
  RemoveNodeClass(scene.RootNode, TNodeNormal, onlyFromActivePart);
  RemoveNodeClass(scene.RootNode, TNodeNormalBinding, onlyFromActivePart);
end;

procedure SceneChange_NoSolidObjects(scene: TVRMLScene);
var
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    scene.RootNode.EnumerateNodes(TNodeShapeHints,
      @DoChanges.NoSolid_ShapeHints, false);
    scene.RootNode.EnumerateNodes(TNodeX3DComposedGeometryNode,
      @DoChanges.NoSolid_X3DComposedGeometry, false);
    scene.RootNode.EnumerateNodes(TNodeExtrusion,
      @DoChanges.NoSolid_Extrusion, false);
    scene.RootNode.EnumerateNodes(TNodeElevationGrid,
      @DoChanges.NoSolid_ElevationGrid, false);
    scene.RootNode.EnumerateNodes(TNodeBox,
      @DoChanges.NoSolid_Box, false);
    scene.RootNode.EnumerateNodes(TNodeCone_2,
      @DoChanges.NoSolid_Cone, false);
    scene.RootNode.EnumerateNodes(TNodeCylinder_2,
      @DoChanges.NoSolid_Cylinder, false);
    scene.RootNode.EnumerateNodes(TNodeSphere_2,
      @DoChanges.NoSolid_Sphere, false);
    scene.RootNode.EnumerateNodes(TNodeText,
      @DoChanges.NoSolid_Text, false);
    scene.RootNode.EnumerateNodes(TNodeText3D,
      @DoChanges.NoSolid_Text3D, false);

    {$ifdef KAMBI_HAS_NURBS}
    scene.RootNode.EnumerateNodes(TNodeX3DNurbsSurfaceGeometryNode,
      @DoChanges.NoSolid_X3DNurbsSurfaceGeometryNode, false);
    scene.RootNode.EnumerateNodes(TNodeNurbsSweptSurface,
      @DoChanges.NoSolid_NurbsSweptSurface, false);
    scene.RootNode.EnumerateNodes(TNodeNurbsSwungSurface,
      @DoChanges.NoSolid_NurbsSwungSurface, false);
    scene.RootNode.EnumerateNodes(TNodeNurbsSurface,
      @DoChanges.NoSolid_NurbsSurface, false);
    {$endif KAMBI_HAS_NURBS}

  finally FreeAndNil(DoChanges) end;
end;

procedure SceneChange_NoConvexFaces(scene: TVRMLScene);
var
  newRootNode: TVRMLNode;
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    scene.RootNode.EnumerateNodes(TNodeShapeHints,
      @DoChanges.NoConvex_ShapeHints, false);
    scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      @DoChanges.NoConvex_IFS_2, false);
    scene.RootNode.EnumerateNodes(TNodeExtrusion,
      @DoChanges.NoConvex_Extrusion, false);
  finally FreeAndNil(DoChanges) end;

  if scene.RootNode.TryFindNode(TVRMLGeometryNode_1, false) <> nil then
  begin
    (* stworz newRootNode : Group, jego chidren to
      ShapeHints { faceType UNKNOWN_FACE_TYPE }
      i dotychczasowy root node sceny. *)
    newRootNode := TNodeGroup_1.Create('', Scene.RootNode.WWWBasePath, Scene.Cache);
    newRootNode.VRML1ChildAdd(TNodeShapeHints.Create('', Scene.RootNode.WWWBasePath, Scene.Cache));
    TNodeShapeHints(newRootNode.VRML1Children[0]).FdFaceType.Value := FACETYPE_UNKNOWN;
    newRootNode.VRML1ChildAdd(scene.RootNode);

    { podmien scene.RootNode na newRootNode }
    scene.RootNode := newRootNode;
  end;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure(Scene: TVRMLScene);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces );

procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLScene);

  procedure DoIt(SceneChange: TSceneChange; Scene: TVRMLScene);
  begin
    SCFunctions[SceneChange](Scene);
  end;

var
  sc: TSceneChange;
begin
  Scene.BeforeNodesFree;
  for sc := Low(sc) to High(sc) do
    if sc in SceneChanges then
      DoIt(SC, Scene);
  Scene.ChangedAll;
end;

procedure ChangeSceneAnimation(SceneChanges: TSceneChanges;
  Scene: TVRMLGLAnimation);

  procedure DoIt(SceneChange: TSceneChange; Scene: TVRMLGLAnimation);
  var
    I: Integer;
  begin
    for I := 0 to Scene.ScenesCount - 1 do
      SCFunctions[SceneChange](Scene.Scenes[I]);
  end;

var
  sc: TSceneChange;
begin
  Scene.BeforeNodesFree;
  for sc := Low(sc) to High(sc) do
    if sc in SceneChanges then
      DoIt(SC, Scene);
  Scene.ChangedAll;
end;

end.
