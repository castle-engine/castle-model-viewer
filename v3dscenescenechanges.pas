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
  When you use this on a TVRMLScene class, remember to surround
  by TVRMLScene.BeforeNodesFree and TVRMLScene.ChangedAll. }
procedure ChangeNode(SceneChanges: TSceneChanges; Node: TVRMLRootNode);

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
  (Node as TVRMLIndexedNode_1).FdNormalIndex.Items.Clear;
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
  { TODO: do this by EnumerateReplaceNodes.
    Actually, most usage of FreeRemovingFromAllParents in engine should
    be replaced by faster and cleaner EnumerateReplaceNodes?
    usuwania node'ow ze sceny lepiej nie robic przez EnumNodes -
    nie mozemy zaburzac hierarchii sceny w czasie jej przegladania.}
  repeat
    node := RootNode.TryFindNode(NodeClass, onlyFromActivePart);
    if node = nil then break;
    node.FreeRemovingFromAllParents;
  until false;
end;

{ TODO: maybe avoid calling costly ChangedAll after scene changes. }

procedure SceneChange_NoNormals(Node: TVRMLRootNode);
begin
  Node.EnumerateNodes(TVRMLIndexedNode_1,
    @TSceneChangesDo(nil).NoNormal_Indexed_1, false);
  Node.EnumerateNodes(TNodeIndexedFaceSet,
    @TSceneChangesDo(nil).NoNormal_IFS, false);
  Node.EnumerateNodes(TNodeElevationGrid,
    @TSceneChangesDo(nil).NoNormal_ElevationGrid, false);

  { Do this at the end.
    Note that for VRML >= 2.0, Normal nodes were already removed by
    NoNormal_IFS (in more intelligent way). }
  RemoveNodeClass(Node, TNodeNormal, false);
  RemoveNodeClass(Node, TNodeNormalBinding, false);
end;

procedure SceneChange_NoSolidObjects(Node: TVRMLRootNode);
begin
  Node.EnumerateNodes(TNodeShapeHints,
    @TSceneChangesDo(nil).NoSolid_ShapeHints, false);
  Node.EnumerateNodes(TNodeX3DComposedGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DComposedGeometry, false);
  Node.EnumerateNodes(TNodeExtrusion,
    @TSceneChangesDo(nil).NoSolid_Extrusion, false);
  Node.EnumerateNodes(TNodeElevationGrid,
    @TSceneChangesDo(nil).NoSolid_ElevationGrid, false);
  Node.EnumerateNodes(TNodeBox,
    @TSceneChangesDo(nil).NoSolid_Box, false);
  Node.EnumerateNodes(TNodeCone,
    @TSceneChangesDo(nil).NoSolid_Cone, false);
  Node.EnumerateNodes(TNodeCylinder,
    @TSceneChangesDo(nil).NoSolid_Cylinder, false);
  Node.EnumerateNodes(TNodeSphere,
    @TSceneChangesDo(nil).NoSolid_Sphere, false);
  Node.EnumerateNodes(TNodeText,
    @TSceneChangesDo(nil).NoSolid_Text, false);
  Node.EnumerateNodes(TNodeText3D,
    @TSceneChangesDo(nil).NoSolid_Text3D, false);
  Node.EnumerateNodes(TNodeX3DNurbsSurfaceGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DNurbsSurfaceGeometryNode, false);
  Node.EnumerateNodes(TNodeNurbsSweptSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSweptSurface, false);
  Node.EnumerateNodes(TNodeNurbsSwungSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSwungSurface, false);
  Node.EnumerateNodes(TNodeNurbsSurface,
    @TSceneChangesDo(nil).NoSolid_NurbsSurface, false);
end;

procedure SceneChange_NoConvexFaces(Node: TVRMLRootNode);
var
  SH: TNodeShapeHints;
begin
  Node.EnumerateNodes(TNodeShapeHints,
    @TSceneChangesDo(nil).NoConvex_ShapeHints, false);
  Node.EnumerateNodes(TNodeIndexedFaceSet,
    @TSceneChangesDo(nil).NoConvex_IFS, false);
  Node.EnumerateNodes(TNodeExtrusion,
    @TSceneChangesDo(nil).NoConvex_Extrusion, false);

  if Node.TryFindNode(TVRMLGeometryNode_1, false) <> nil then
  begin
    SH := TNodeShapeHints.Create('', Node.WWWBasePath);
    SH.FdFaceType.Value := FACETYPE_UNKNOWN;
    Node.FdChildren.Add(0, SH);
  end;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure (Node: TVRMLRootNode);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces );

procedure ChangeNode(SceneChanges: TSceneChanges; Node: TVRMLRootNode);
var
  SC: TSceneChange;
begin
  for SC := Low(SC) to High(SC) do
    if SC in SceneChanges then
      SCFunctions[SC](Node);
end;

procedure ChangeAnimation(SceneChanges: TSceneChanges; Scene: TVRMLGLAnimation);
var
  I: Integer;
begin
  { Check SceneChanges, to avoid expensive ChangedAll (that also invalidates
    all data prepared in shapes, like octrees) when not needed. }
  if SceneChanges <> [] then
  begin
    Scene.BeforeNodesFree;
    for I := 0 to Scene.ScenesCount - 1 do
      ChangeNode(SceneChanges, Scene.Scenes[I].RootNode);
    Scene.ChangedAll;
  end;
end;

end.
