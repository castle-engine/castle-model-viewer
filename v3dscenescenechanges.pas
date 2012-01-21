{
  Copyright 2003-2012 Michalis Kamburelis.

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

uses CastleSceneCore, X3DNodes, CastleUtils, PrecalculatedAnimation;

type
  { When adding new item to TSceneChange you can also consider adding
    new option to view3dscene (--scene-change-).
    All other necessary changes after extending TSceneChange will be enforced
    by the compiler.

    See [http://castle-engine.sourceforge.net/view3dscene.php#section_command_line_options]
    for detailed spec what these scene changes do. }
  TSceneChange = (
    scNoNormals,
    scNoSolidObjects,
    scNoConvexFaces);
  TSceneChanges = set of TSceneChange;

{ Transforms scene root node as appropriate.
  When you use this on a TCastleSceneCore class, remember to surround
  by TCastleSceneCore.BeforeNodesFree and TCastleSceneCore.ChangedAll. }
procedure ChangeNode(SceneChanges: TSceneChanges; Node: TX3DRootNode);

{ Transforms all scenes inside the animation as appropriate. }
procedure ChangeAnimation(SceneChanges: TSceneChanges; Scene: TCastlePrecalculatedAnimation);

implementation

uses SysUtils;

{ TSceneChangesDo -------------------------------------------------- }

type
  TSceneChangesDo = class
  public
    class procedure NoNormal_Indexed_1(node: TX3DNode);
    class procedure NoNormal_IFS(node: TX3DNode);
    class procedure NoNormal_ElevationGrid(node: TX3DNode);

    class procedure NoSolid_ShapeHints(node: TX3DNode);
    class procedure NoSolid_X3DComposedGeometry(node: TX3DNode);
    class procedure NoSolid_Extrusion(node: TX3DNode);
    class procedure NoSolid_ElevationGrid(node: TX3DNode);
    class procedure NoSolid_Box(node: TX3DNode);
    class procedure NoSolid_Cone(node: TX3DNode);
    class procedure NoSolid_Cylinder(node: TX3DNode);
    class procedure NoSolid_Sphere(node: TX3DNode);
    class procedure NoSolid_Text(node: TX3DNode);
    class procedure NoSolid_Text3D(node: TX3DNode);
    class procedure NoSolid_X3DNurbsSurfaceGeometryNode(Node: TX3DNode);
    class procedure NoSolid_NurbsSweptSurface(Node: TX3DNode);
    class procedure NoSolid_NurbsSwungSurface(Node: TX3DNode);
    class procedure NoSolid_NurbsSurface(Node: TX3DNode);

    class procedure NoConvex_ShapeHints(node: TX3DNode);
    class procedure NoConvex_IFS(node: TX3DNode);
    class procedure NoConvex_Extrusion(node: TX3DNode);
  end;

class procedure TSceneChangesDo.NoNormal_Indexed_1(node: TX3DNode);
begin
  (Node as TAbstractIndexedNode_1).FdNormalIndex.Items.Clear;
end;

class procedure TSceneChangesDo.NoNormal_IFS(node: TX3DNode);
begin
  (Node as TIndexedFaceSetNode).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoNormal_ElevationGrid(node: TX3DNode);
begin
  (Node as TElevationGridNode).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoSolid_ShapeHints(node: TX3DNode);
begin
  (Node as TShapeHintsNode_1).FdShapeType.Value := SHTYPE_UNKNOWN;
end;

class procedure TSceneChangesDo.NoSolid_X3DComposedGeometry(node: TX3DNode);
begin
  (Node as TAbstractComposedGeometryNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Extrusion(node: TX3DNode);
begin
  (Node as TExtrusionNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_ElevationGrid(Node: TX3DNode);
begin
  (Node as TElevationGridNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Box(Node: TX3DNode);
begin
  (Node as TBoxNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Cone(Node: TX3DNode);
begin
  (Node as TConeNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Cylinder(Node: TX3DNode);
begin
  (Node as TCylinderNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Sphere(Node: TX3DNode);
begin
  (Node as TSphereNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Text(Node: TX3DNode);
begin
  (Node as TTextNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_Text3D(Node: TX3DNode);
begin
  (Node as TText3DNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_X3DNurbsSurfaceGeometryNode(Node: TX3DNode);
begin
  (Node as TAbstractNurbsSurfaceGeometryNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSweptSurface(Node: TX3DNode);
begin
  (Node as TNurbsSweptSurfaceNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSwungSurface(Node: TX3DNode);
begin
  (Node as TNurbsSwungSurfaceNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoSolid_NurbsSurface(Node: TX3DNode);
begin
  (Node as TNurbsSurfaceNode).FdSolid.Value := false;
end;

class procedure TSceneChangesDo.NoConvex_ShapeHints(node: TX3DNode);
begin
  (Node as TShapeHintsNode_1).FdFaceType.Value := FACETYPE_UNKNOWN;
end;

class procedure TSceneChangesDo.NoConvex_IFS(node: TX3DNode);
begin
  (Node as TIndexedFaceSetNode).FdConvex.Value := false;
end;

class procedure TSceneChangesDo.NoConvex_Extrusion(node: TX3DNode);
begin
  (Node as TExtrusionNode).FdConvex.Value := false;
end;

{ SceneChange_Xxx functions ---------------------------------------- }

procedure RemoveNodeClass(RootNode: TX3DNode;
  NodeClass: TX3DNodeClass; onlyFromActivePart: boolean);
var
  Node: TX3DNode;
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

procedure SceneChange_NoNormals(Node: TX3DRootNode);
begin
  Node.EnumerateNodes(TAbstractIndexedNode_1,
    @TSceneChangesDo(nil).NoNormal_Indexed_1, false);
  Node.EnumerateNodes(TIndexedFaceSetNode,
    @TSceneChangesDo(nil).NoNormal_IFS, false);
  Node.EnumerateNodes(TElevationGridNode,
    @TSceneChangesDo(nil).NoNormal_ElevationGrid, false);

  { Do this at the end.
    Note that for VRML >= 2.0, Normal nodes were already removed by
    NoNormal_IFS (in more intelligent way). }
  RemoveNodeClass(Node, TNormalNode, false);
  RemoveNodeClass(Node, TNormalBindingNode_1, false);
end;

procedure SceneChange_NoSolidObjects(Node: TX3DRootNode);
begin
  Node.EnumerateNodes(TShapeHintsNode_1,
    @TSceneChangesDo(nil).NoSolid_ShapeHints, false);
  Node.EnumerateNodes(TAbstractComposedGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DComposedGeometry, false);
  Node.EnumerateNodes(TExtrusionNode,
    @TSceneChangesDo(nil).NoSolid_Extrusion, false);
  Node.EnumerateNodes(TElevationGridNode,
    @TSceneChangesDo(nil).NoSolid_ElevationGrid, false);
  Node.EnumerateNodes(TBoxNode,
    @TSceneChangesDo(nil).NoSolid_Box, false);
  Node.EnumerateNodes(TConeNode,
    @TSceneChangesDo(nil).NoSolid_Cone, false);
  Node.EnumerateNodes(TCylinderNode,
    @TSceneChangesDo(nil).NoSolid_Cylinder, false);
  Node.EnumerateNodes(TSphereNode,
    @TSceneChangesDo(nil).NoSolid_Sphere, false);
  Node.EnumerateNodes(TTextNode,
    @TSceneChangesDo(nil).NoSolid_Text, false);
  Node.EnumerateNodes(TText3DNode,
    @TSceneChangesDo(nil).NoSolid_Text3D, false);
  Node.EnumerateNodes(TAbstractNurbsSurfaceGeometryNode,
    @TSceneChangesDo(nil).NoSolid_X3DNurbsSurfaceGeometryNode, false);
  Node.EnumerateNodes(TNurbsSweptSurfaceNode,
    @TSceneChangesDo(nil).NoSolid_NurbsSweptSurface, false);
  Node.EnumerateNodes(TNurbsSwungSurfaceNode,
    @TSceneChangesDo(nil).NoSolid_NurbsSwungSurface, false);
  Node.EnumerateNodes(TNurbsSurfaceNode,
    @TSceneChangesDo(nil).NoSolid_NurbsSurface, false);
end;

procedure SceneChange_NoConvexFaces(Node: TX3DRootNode);
var
  SH: TShapeHintsNode_1;
begin
  Node.EnumerateNodes(TShapeHintsNode_1,
    @TSceneChangesDo(nil).NoConvex_ShapeHints, false);
  Node.EnumerateNodes(TIndexedFaceSetNode,
    @TSceneChangesDo(nil).NoConvex_IFS, false);
  Node.EnumerateNodes(TExtrusionNode,
    @TSceneChangesDo(nil).NoConvex_Extrusion, false);

  if Node.TryFindNode(TAbstractGeometryNode_1, false) <> nil then
  begin
    SH := TShapeHintsNode_1.Create('', Node.WWWBasePath);
    SH.FdFaceType.Value := FACETYPE_UNKNOWN;
    Node.FdChildren.Add(0, SH);
  end;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure (Node: TX3DRootNode);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces );

procedure ChangeNode(SceneChanges: TSceneChanges; Node: TX3DRootNode);
var
  SC: TSceneChange;
begin
  for SC := Low(SC) to High(SC) do
    if SC in SceneChanges then
      SCFunctions[SC](Node);
end;

procedure ChangeAnimation(SceneChanges: TSceneChanges; Scene: TCastlePrecalculatedAnimation);
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
