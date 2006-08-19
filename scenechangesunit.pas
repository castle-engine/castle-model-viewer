{
  Copyright 2003-2005 Michalis Kamburelis.

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

uses VRMLFlatScene, VRMLNodes, KambiUtils;

type
  { When adding new item to TSceneChange you can also consider adding
    new option to view3dscene (--scene-change-).
    All other necessary changes after extending TSceneChange will be enforced
    by the compiler.

    See [http://www.camelot.homedns.org/~michalis/view3dscene.php#section_command_line_options]
    for detailed spec what these scene changes do. }
  TSceneChange = (
    scNoNormals,
    scNoSolidObjects,
    scNoConvexFaces);
  TSceneChanges = set of TSceneChange;

procedure ChangeScene(SceneChange: TSceneChange;
  Scene: TVRMLFlatScene); overload;

{ Do all scene changes in SceneChanges.
  Changes will be done in the order they are declared in type TSceneChange. }
procedure ChangeScene(SceneChanges: TSceneChanges;
  Scene: TVRMLFlatScene); overload;

implementation

uses SysUtils;

{ TSceneChangesDo -------------------------------------------------- }

type
  TSceneChangesDo = class
  public
    procedure NormalIndexErase_1(node: TVRMLNode);
    procedure NormalErase_2(node: TVRMLNode);

    procedure MakeNoSolid_1(node: TVRMLNode);
    procedure MakeNoSolid_2(node: TVRMLNode);

    procedure MakeNoConvex_1(node: TVRMLNode);
    procedure MakeNoConvex_2(node: TVRMLNode);
  end;

procedure TSceneChangesDo.NormalIndexErase_1(node: TVRMLNode);
begin
  (Node as TNodeGeneralIndexed_1).FdNormalIndex.Items.SetLength(0);
end;

procedure TSceneChangesDo.NormalErase_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdNormal.Value := nil;
end;

procedure TSceneChangesDo.MakeNoSolid_1(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdShapeType.Value := SHTYPE_UNKNOWN;
end;

procedure TSceneChangesDo.MakeNoSolid_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdSolid.Value := false;
end;

procedure TSceneChangesDo.MakeNoConvex_1(node: TVRMLNode);
begin
  (Node as TNodeShapeHints).FdFaceType.Value := FACETYPE_UNKNOWN;
end;

procedure TSceneChangesDo.MakeNoConvex_2(node: TVRMLNode);
begin
  (Node as TNodeIndexedFaceSet_2).FdConvex.Value := false;
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
  node.FreeRemovingFromAllParentNodes;
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
    Scene.RootNode.EnumerateNodes(TNodeGeneralIndexed_1,
      DoChanges.NormalIndexErase_1, onlyFromActivePart);
    Scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      DoChanges.NormalErase_2, onlyFromActivePart);
  finally FreeAndNil(DoChanges) end;

  { Do this at the end --- for VRML 2.0, Normal nodes will be
    removed in more intelligent way. }
  RemoveNodeClass(scene.RootNode, TNodeNormal, onlyFromActivePart);
  RemoveNodeClass(scene.RootNode, TNodeNormalBinding, onlyFromActivePart);

  scene.ChangedAll;
end;

procedure SceneChange_NoSolidObjects(scene: TVRMLFlatScene);
var
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    scene.RootNode.EnumerateNodes(TNodeShapeHints,
      DoChanges.MakeNoSolid_1, false);
    scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      DoChanges.MakeNoSolid_2, false);
  finally FreeAndNil(DoChanges) end;

  scene.ChangedAll;
end;

procedure SceneChange_NoConvexFaces(scene: TVRMLFlatScene);
var
  newRootNode: TVRMLNode;
  DoChanges: TSceneChangesDo;
begin
  DoChanges := TSceneChangesDo.Create;
  try
    scene.RootNode.EnumerateNodes(TNodeShapeHints,
      DoChanges.MakeNoConvex_1, false);
    scene.RootNode.EnumerateNodes(TNodeIndexedFaceSet_2,
      DoChanges.MakeNoConvex_2, false);
  finally FreeAndNil(DoChanges) end;

  if scene.RootNode.TryFindNode(TNodeGeneralShape_1, false) <> nil then
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

  scene.ChangedAll;
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure(Scene: TVRMLFlatScene);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( SceneChange_NoNormals,
    SceneChange_NoSolidObjects,
    SceneChange_NoConvexFaces );

procedure ChangeScene(SceneChange: TSceneChange; Scene: TVRMLFlatScene);
begin
  SCFunctions[SceneChange](Scene);
end;

procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLFlatScene);
var sc: TSceneChange;
begin
 for sc := Low(sc) to High(sc) do
  if sc in SceneChanges then
   ChangeScene(SC, Scene);
end;

end.
