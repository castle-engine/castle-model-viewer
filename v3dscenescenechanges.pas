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

{ Transforms scene root node as appropriate.
  Returns new root node. (Old root node is either included here,
  or already freed.)

  When you use this on a TVRMLScene class, remember to surround
  by TVRMLScene.BeforeNodesFree and TVRMLScene.ChangedAll. }
function ChangeNode(SceneChanges: TSceneChanges; Node: TVRMLNode): TVRMLNode;

{ Transforms all scenes inside the animation as appropriate. }
procedure ChangeAnimation(SceneChanges: TSceneChanges; Scene: TVRMLGLAnimation);

implementation

uses SysUtils;

{ TSceneChangesDo -------------------------------------------------- }

type
  TSceneChangesDo = class
  public
    class procedure NoNormal_Indexed_1(node: TVRMLNode);
    class procedure NoNormal_IFS(node: TVRMLNode);
    class procedure NoNormal_ElevationGrid(node: TVRMLNode);

    class procedure NoSolid_ShapeHints(node: TVRMLNode);
    class procedure NoSolid_X3DComposedGeometry(node: TVRMLNode);
    class procedure NoSolid_Extrusion(node: TVRMLNode);
    class procedure NoSolid_ElevationGrid(node: TVRMLNode);
    class procedure NoSolid_Box(node: TVRMLNode);
    class procedure NoSolid_Cone(node: TVRMLNode);
    class procedure NoSolid_Cylinder(node: TVRMLNode);
    class procedure NoSolid_Sphere(node: TVRMLNode);
    class procedure NoSolid_Text(node: TVRMLNode);
    class procedure NoSolid_Text3D(node: TVRMLNode);
    class procedure NoSolid_X3DNurbsSurfaceGeometryNode(Node: TVRMLNode);
    class procedure NoSolid_NurbsSweptSurface(Node: TVRMLNode);
    class procedure NoSolid_NurbsSwungSurface(Node: TVRMLNode);
    class procedure NoSolid_NurbsSurface(Node: TVRMLNode);

    class procedure NoConvex_ShapeHints(node: TVRMLNode);
    class procedure NoConvex_IFS(node: TVRMLNode);
    class procedure NoConvex_Extrusion(node: TVRMLNode);
  end;

class procedure TSceneChangesDo.NoNormal_Indexed_1(node: TVRMLNode);
begin
  (Node as TVRMLIndexedNode_1).FdNormalIndex.Items.SetLength(0);
end;

class procedure TSceneChangesDo.NoNormal_IFS(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoNormal_ElevationGrid(node: TVRMLNode);
begin
  (Node as TNodeElevationGrid).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoSolid_ShapeHints(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdShapeType.Value := SHTYPE_UNKNOWN;
end;

class procedure TSceneChangesDo.NoSolid_X3DComposedGeometry(node: TVRMLNode);
begin
  (Node as TNodeX3DComposedGeometryNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Extrusion(node: TVRMLNode);
begin
  (Node as TNodeExtrusion).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_ElevationGrid(Node: TVRMLNode);
begin
  (Node as TNodeElevationGrid).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Box(Node: TVRMLNode);
begin
  (Node as TNodeBox).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Cone(Node: TVRMLNode);
begin
  (Node as TNodeCone).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Cylinder(Node: TVRMLNode);
begin
  (Node as TNodeCylinder).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Sphere(Node: TVRMLNode);
begin
  (Node as TNodeSphere).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Text(Node: TVRMLNode);
begin
  (Node as TNodeText).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Text3D(Node: TVRMLNode);
begin
  (Node as TNodeText3D).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_X3DNurbsSurfaceGeometryNode(Node: TVRMLNode);
begin
  (Node as TNodeX3DNurbsSurfaceGeometryNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSweptSurface(Node: TVRMLNode);
begin
  (Node as TNodeNurbsSweptSurface).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSwungSurface(Node: TVRMLNode);
begin
  (Node as TNodeNurbsSwungSurface).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSurface(Node: TVRMLNode);
begin
  (Node as TNodeNurbsSurface).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoConvex_ShapeHints(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdFaceType.Value := FACETYPE_UNKNOWN;
end;

class procedure TSceneChangesDo.NoConvex_IFS(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet).FdConvex.Value := false;
end;

class procedure TSceneChangesDo.NoConvex_Extrusion(node: TVRMLNode);
begin
  (Node as TNodeExtrusion).FdConvex.Value := false;
end;

{ SceneChange_Xxx functions ---------------------------------------- }

procedure RemoveNodeClass(RootNode: TVRMLNode;
  NodeClass: TVRMLNodeClass; onlyFromActivePart: boolean);
var
  Node: TVRMLNode;
begin
  { usuwania node'ow ze sceny lepiej nie robic przez EnumNodes -
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

function SceneChange_NoNormals(Node: TVRMLNode): TVRMLNode;
const
  onlyFromActivePart = false;
begin
  Result := Node;

  Result.EnumerateNodes(TVRMLIndexedNode_1,
    @TSceneChangesDo(nil).NoNormal_Indexed_1, onlyFromActivePart);
  Result.EnumerateNodes(TNodeIndexedFaceSet,
    @TSceneChangesDo(nil).NoNormal_IFS, onlyFromActivePart);
  Result.EnumerateNodes(TNodeElevationGrid,
    @TSceneChangesDo(nil).NoNormal_ElevationGrid, onlyFromActivePart);

  { Do this at the end.
    Note that for VRML >= 2.0, Normal nodes were already removed by
    NoNormal_IFS (in more intelligent way). }
  RemoveNodeClass(Result, TNodeNormal, onlyFromActivePart);
  RemoveNodeClass(Result, TNodeNormalBinding, onlyFromActivePart);
end;

function SceneChange_NoSolidObjects(Node: TVRMLNode): TVRMLNode;
begin
  Result := Node;

  Result.EnumerateNodes(TNodeShapeHints,
    @TSceneChangesDo(nil).NoSolid_ShapeHints, false);
  Result.EnumerateNodes(TNodeX3DComposedGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DComposedGeometry, false);
  Result.EnumerateNodes(TNodeExtrusion,
    @TSceneChangesDo(nil).NoSolid_Extrusion, false);
  Result.EnumerateNodes(TNodeElevationGrid,
    @TSceneChangesDo(nil).NoSolid_ElevationGrid, false);
  Result.EnumerateNodes(TNodeBox,
    @TSceneChangesDo(nil).NoSolid_Box, false);
  Result.EnumerateNodes(TNodeCone,
    @TSceneChangesDo(nil).NoSolid_Cone, false);
  Result.EnumerateNodes(TNodeCylinder,
    @TSceneChangesDo(nil).NoSolid_Cylinder, false);
  Result.EnumerateNodes(TNodeSphere,
    @TSceneChangesDo(nil).NoSolid_Sphere, false);
  Result.EnumerateNodes(TNodeText,
    @TSceneChangesDo(nil).NoSolid_Text, false);
  Result.EnumerateNodes(TNodeText3D,
    @TSceneChangesDo(nil).NoSolid_Text3D, false);
  Result.EnumerateNodes(TNodeX3DNurbsSurfaceGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DNurbsSurfaceGeometryNode, false);
  Result.EnumerateNodes(TNodeNurbsSweptSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSweptSurface, false);
  Result.EnumerateNodes(TNodeNurbsSwungSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSwungSurface, false);
  Result.EnumerateNodes(TNodeNurbsSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSurface, false);
end;

function SceneChange_NoConvexFaces(Node: TVRMLNode): TVRMLNode;
var
  SH: TNodeShapeHints;
begin
  Node.EnumerateNodes(TNodeShapeHints,
    @TSceneChangesDo(nil).NoConvex_ShapeHints, false);
  Node.EnumerateNodes(TNodeIndexedFaceSet,
    @TSceneChangesDo(nil).NoConvex_IFS, false);
  Node.EnumerateNodes(TNodeExtrusion,
    @TSceneChangesDo(nil).NoConvex_Extrusion, false);

  Result := Node;

  if Result.TryFindNode(TVRMLGeometryNode_1, false) <> nil then
  begin
    Result := TNodeGroup_1.Create('', Node.WWWBasePath);
    SH := TNodeShapeHints.Create('', Node.WWWBasePath);
    SH.FdFaceType.Value := FACETYPE_UNKNOWN;
    Result.VRML1ChildAdd(SH);
    Result.VRML1ChildAdd(Node);
  end;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = function (Node: TVRMLNode): TVRMLNode;
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces );

function ChangeNode(SceneChanges: TSceneChanges; Node: TVRMLNode): TVRMLNode;
var
  SC: TSceneChange;
begin
  Result := Node;
  for SC := Low(SC) to High(SC) do
    if SC in SceneChanges then
      Result := SCFunctions[SC](Result);
end;

procedure ChangeAnimation(SceneChanges: TSceneChanges; Scene: TVRMLGLAnimation);
var
  I: Integer;
begin
  Scene.BeforeNodesFree;
  for I := 0 to Scene.ScenesCount - 1 do
    Scene.Scenes[I].RootNode := ChangeNode(SceneChanges, Scene.Scenes[I].RootNode);
  Scene.ChangedAll;
end;

end.
