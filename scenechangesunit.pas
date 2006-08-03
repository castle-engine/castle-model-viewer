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

unit SceneChangesUnit;

interface

uses VRMLFlatScene, VRMLNodes, KambiUtils;

type
  { When adding new item to TSceneChange you can also consider adding
    new option to view3dscene (--scene-change-). 
    All other necessary changes after extending TSceneChange will be enforced
    by the compiler. }
  TSceneChange = (
    { usun ze sceny wszystkie node'y Normal, NormalBinding i wykasuj zawartosc
      wszystkich pol normalIndex w node'ach indexed.
      W rezultacie wymusi to na nas zawsze generowanie wlasnych normali
      (co moze byc przydatne np. gdy obiekt ma zle/niedoskonale smooth normale). }
    scNoNormals,
    { zamien wszedzie w ShapeHints ShapeType z SOLID na UNKNOWN. W ten sposob
      nie bedziemy uzywali backface culling. (pomocne jesli model ma blednie
      ustawione solid). }
    scNoSolidObjects,
    scNoConvexFaces);
  TSceneChanges = set of TSceneChange;

{ Do all scene changes in SceneChanges.
  Changes will be done in the order they are declared in type TSceneChange. }
procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLFlatScene);

implementation

{ SceneChange_Xxx functions ---------------------------------------- }

type
  TNormalIndexEraser = class
    procedure DoNode(node: TVRMLNode);
  end;
  procedure TNormalIndexEraser.DoNode(node: TVRMLNode);
  begin TNodeGeneralIndexed(node).FdNormalIndex.Items.SetLength(0) end;

type
  TNoSolidMaker = class
    procedure DoNode(node: TVRMLNode);
  end;
  procedure TNoSolidMaker.DoNode(node: TVRMLNode);
  begin TNodeShapeHints(node).FdShapeType.Value := SHTYPE_UNKNOWN end;

type
  TNoConvexMaker = class
    procedure DoNode(node: TVRMLNode);
  end;
  procedure TNoConvexMaker.DoNode(node: TVRMLNode);
  begin TNodeShapeHints(node).FdFaceType.Value := FACETYPE_UNKNOWN end;

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
begin
 RemoveNodeClass(scene.RootNode, TNodeNormal, onlyFromActivePart);
 RemoveNodeClass(scene.RootNode, TNodeNormalBinding, onlyFromActivePart);
 scene.RootNode.EnumNodes(TNodeGeneralIndexed, TNormalIndexEraser.DoNode, 
   onlyFromActivePart);
 scene.ChangedAll;
end;

procedure SceneChange_NoSolidObjects(scene: TVRMLFlatScene);
begin
 scene.RootNode.EnumNodes(TNodeShapeHints, TNoSolidMaker.DoNode, false);
 scene.ChangedAll;
end;

procedure SceneChange_NoConvexFaces(scene: TVRMLFlatScene);
var newRootNode: TVRMLNode;
begin
 scene.RootNode.EnumNodes(TNodeShapeHints, TNoConvexMaker.DoNode, false);

 (* stworz newRootNode : Group, jego chidren to
   ShapeHints { faceType UNKNOWN_FACE_TYPE }
   i dotychczasowy root node sceny. *)
 newRootNode := TNodeGroup_1.Create('', scene.RootNode.WWWBasePath);
 newRootNode.AddChild(TNodeShapeHints.Create('', scene.RootNode.WWWBasePath));
 TNodeShapeHints(newRootNode.Children[0]).FdFaceType.Value := FACETYPE_UNKNOWN;
 newRootNode.AddChild(scene.RootNode);

 { podmien scene.RootNode na newRootNode }
 scene.RootNode := newRootNode;

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

procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TVRMLFlatScene);
var sc: TSceneChange;
begin
 for sc := Low(sc) to High(sc) do 
  if sc in SceneChanges then
   SCFunctions[sc](Scene);
end;

end.
