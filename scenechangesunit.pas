{
  Copyright 2003-2008 Michalis Kamburelis.

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

{ }
unit SceneChangesUnit;

interface

uses VRMLFlatScene, VRMLNodes, KambiUtils, VRMLGLAnimation;

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
procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLFlatScene);

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
    procedure NoSolid_IFS_2(node: TVRMLNode);
    procedure NoSolid_Extrusion(node: TVRMLNode);

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

procedure TSceneChangesDo.NoSolid_IFS_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdSolid.Value := false;
end;

procedure TSceneChangesDo.NoSolid_Extrusion(node: TVRMLNode);
begin
  (Node as TNodeExtrusion).FdSolid.Value := false;
end;

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

procedure SceneChange_NoNormals(scene: TVRMLFlatScene);
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

procedure SceneChange_NoSolidObjects(scene: TVRMLFlatScene);
var
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    scene.RootNode.EnumerateNodes(TNodeShapeHints,
      @DoChanges.NoSolid_ShapeHints, false);
    scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      @DoChanges.NoSolid_IFS_2, false);
    scene.RootNode.EnumerateNodes(TNodeExtrusion,
      @DoChanges.NoSolid_Extrusion, false);
  finally FreeAndNil(DoChanges) end;
end;

procedure SceneChange_NoConvexFaces(scene: TVRMLFlatScene);
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
    newRootNode := TNodeGroup_1.Create('', scene.RootNode.WWWBasePath);
    newRootNode.AddChild(TNodeShapeHints.Create('', scene.RootNode.WWWBasePath));
    TNodeShapeHints(newRootNode.Children[0]).FdFaceType.Value := FACETYPE_UNKNOWN;
    newRootNode.AddChild(scene.RootNode);

    { podmien scene.RootNode na newRootNode }
    scene.RootNode := newRootNode;
  end;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure(Scene: TVRMLFlatScene);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces );

procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLFlatScene);

  procedure DoIt(SceneChange: TSceneChange; Scene: TVRMLFlatScene);
  begin
    SCFunctions[SceneChange](Scene);
  end;

var
  sc: TSceneChange;
begin
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
  for sc := Low(sc) to High(sc) do
    if sc in SceneChanges then
      DoIt(SC, Scene);
  Scene.ChangedAll;
end;

end.
