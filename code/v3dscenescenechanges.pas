{
  Copyright 2003-2023 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Scene transformations (available through Edit menu). }
unit V3DSceneSceneChanges;

interface

uses X3DNodes, CastleUtils, CastleScene;

type
  { When adding new item to TSceneChange you can also consider adding
    new option to view3dscene (--scene-change-).
    All other necessary changes after extending TSceneChange will be enforced
    by the compiler.

    See [https://castle-engine.io/view3dscene.php#section_command_line_options]
    for detailed spec what these scene changes do. }
  TSceneChange = (
    scNoNormals,
    scNoSolidObjects,
    scNoConvexFaces,
    scConvertInlines
  );
  TSceneChanges = set of TSceneChange;

{ Transforms scene root node as appropriate.
  When you use this on a TCastleSceneCore class, remember to surround
  by TCastleSceneCore.BeforeNodesFree and TCastleSceneCore.ChangedAll. }
procedure ChangeNode(SceneChanges: TSceneChanges; Node: TX3DRootNode);

{ Transforms all scenes inside the animation as appropriate. }
procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TCastleScene);

implementation

uses SysUtils, X3DFields;

{ TSceneChangesDo -------------------------------------------------- }

type
  TSceneChangesDo = class
  public
    class procedure NoNormal_Indexed_1(node: TX3DNode);
    class procedure NoNormal_ComposedGeometryNode(node: TX3DNode);
    class procedure NoNormal_ElevationGrid(node: TX3DNode);

    class procedure NoSolid_ShapeHints(node: TX3DNode);
    class procedure NoSolid_AbstractGeometry(Node: TX3DNode);

    class procedure NoConvex_ShapeHints(node: TX3DNode);
    class procedure NoConvex_AbstractGeometry(node: TX3DNode);

    class procedure ConvertInlines(ParentNode: TX3DNode; var Node: TX3DNode);
  end;

class procedure TSceneChangesDo.NoNormal_Indexed_1(node: TX3DNode);
begin
  (Node as TAbstractIndexedNode_1).FdNormalIndex.Items.Clear;
end;

class procedure TSceneChangesDo.NoNormal_ComposedGeometryNode(node: TX3DNode);
begin
  (Node as TAbstractComposedGeometryNode).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoNormal_ElevationGrid(node: TX3DNode);
begin
  (Node as TElevationGridNode).FdNormal.Value := nil;
end;

class procedure TSceneChangesDo.NoSolid_ShapeHints(node: TX3DNode);
begin
  {$warnings off} // using deprecated to support VRML 1
  (Node as TShapeHintsNode_1).FdShapeType.Value := SHTYPE_UNKNOWN;
  {$warnings on}
end;

class procedure TSceneChangesDo.NoSolid_AbstractGeometry(Node: TX3DNode);
var
  F: TSFBool;
begin
  { We could also just use
      (Node as TAbstractGeometryNode).Value := false
    but that would be less optimal.
    (Better to let scene to call ChangedAll later.)  }
  F := (Node as TAbstractGeometryNode).SolidField;
  if F <> nil then F.Value := false;
end;

class procedure TSceneChangesDo.NoConvex_ShapeHints(node: TX3DNode);
begin
  {$warnings off} // using deprecated to support VRML 1
  (Node as TShapeHintsNode_1).FdFaceType.Value := FACETYPE_UNKNOWN;
  {$warnings on}
end;

class procedure TSceneChangesDo.NoConvex_AbstractGeometry(node: TX3DNode);
var
  F: TSFBool;
begin
  F := (Node as TAbstractGeometryNode).ConvexField;
  if F <> nil then F.Value := false;
end;

class procedure TSceneChangesDo.ConvertInlines(ParentNode: TX3DNode; var Node: TX3DNode);
var
  InlineNode: TInlineNode;
  NewGroup: TGroupNode;
begin
  if Node is TInlineNode then
  begin
    // replace Node with new TGroupNode
    InlineNode := TInlineNode(Node);
    NewGroup := TGroupNode.Create(InlineNode.X3DName, InlineNode.BaseUrl);
    Node := NewGroup;

    // put Inline contents into new group
    if InlineNode.Inlined <> nil then
    begin
      ChangeNode([scConvertInlines], InlineNode.Inlined);
      NewGroup.FdChildren.Assign(InlineNode.Inlined.FdChildren);
    end;
  end;
end;

{ SceneChange_Xxx functions ---------------------------------------- }

type
  TRemoveNodeClassHelper = class
    NodeClassToRemove: TX3DNodeClass;
    procedure DoIt(ParentNode: TX3DNode; var Node: TX3DNode);
  end;

procedure TRemoveNodeClassHelper.DoIt(
  ParentNode: TX3DNode; var Node: TX3DNode);
begin
  if Node.InheritsFrom(NodeClassToRemove) then
    Node := nil;
end;

procedure RemoveNodeClass(RootNode: TX3DNode;
  NodeClass: TX3DNodeClass);
var
  Helper: TRemoveNodeClassHelper;
begin
  Helper := TRemoveNodeClassHelper.Create;
  try
    Helper.NodeClassToRemove := NodeClass;
    RootNode.EnumerateReplaceChildren(@Helper.DoIt);
  finally Helper.Free end;
end;

{ TODO: maybe avoid calling costly ChangedAll after scene changes. }

procedure SceneChange_NoNormals(Node: TX3DRootNode);
begin
  Node.EnumerateNodes(TAbstractIndexedNode_1,
    @TSceneChangesDo(nil).NoNormal_Indexed_1, false);
  Node.EnumerateNodes(TAbstractComposedGeometryNode,
    @TSceneChangesDo(nil).NoNormal_ComposedGeometryNode, false);
  Node.EnumerateNodes(TElevationGridNode,
    @TSceneChangesDo(nil).NoNormal_ElevationGrid, false);

  { Do this at the end.
    Note that for VRML >= 2.0, most of the Normal nodes were already removed
    by NoNormal_ComposedGeometryNode and NoNormal_ElevationGrid anyway. }
  RemoveNodeClass(Node, TNormalNode);
  RemoveNodeClass(Node, TNormalBindingNode_1);
end;

procedure SceneChange_NoSolidObjects(Node: TX3DRootNode);
begin
  Node.EnumerateNodes(TShapeHintsNode_1,
    @TSceneChangesDo(nil).NoSolid_ShapeHints, false);
  Node.EnumerateNodes(TAbstractGeometryNode,
    @TSceneChangesDo(nil).NoSolid_AbstractGeometry, false);
end;

procedure SceneChange_NoConvexFaces(Node: TX3DRootNode);
var
  SH: TShapeHintsNode_1;
begin
  Node.EnumerateNodes(TShapeHintsNode_1,
    @TSceneChangesDo(nil).NoConvex_ShapeHints, false);
  Node.EnumerateNodes(TAbstractGeometryNode,
    @TSceneChangesDo(nil).NoConvex_AbstractGeometry, false);

  { To VRML 1, add ShapeHints node with FACETYPE_UNKNOWN. }
  if Node.HasForceVersion and
     (Node.ForceVersion.Major <= 1) then
  begin
    SH := TShapeHintsNode_1.Create('', Node.BaseUrl);
    {$warnings off} // using deprecated to support VRML 1
    SH.FdFaceType.Value := FACETYPE_UNKNOWN;
    {$warnings on}
    Node.FdChildren.Add(0, SH);
  end;
end;

procedure SceneChange_ConvertInlines(Node: TX3DRootNode);
begin
  Node.EnumerateReplaceChildren(@TSceneChangesDo(nil).ConvertInlines);
end;

{ ChangeScene --------------------------------------------------------------- }

type
  TSceneChangeFunction = procedure (Node: TX3DRootNode);
const
  SCFunctions: array[TSceneChange]of TSceneChangeFunction =
  ( @SceneChange_NoNormals,
    @SceneChange_NoSolidObjects,
    @SceneChange_NoConvexFaces,
    @SceneChange_ConvertInlines
  );

procedure ChangeNode(SceneChanges: TSceneChanges; Node: TX3DRootNode);
var
  SC: TSceneChange;
begin
  for SC := Low(SC) to High(SC) do
    if SC in SceneChanges then
      SCFunctions[SC](Node);
end;

procedure ChangeScene(SceneChanges: TSceneChanges; Scene: TCastleScene);
begin
  { Check SceneChanges, to avoid expensive ChangedAll (that also invalidates
    all data prepared in shapes, like octrees) when not needed. }
  if (SceneChanges <> []) and (Scene.RootNode <> nil) then
  begin
    Scene.BeforeNodesFree;
    ChangeNode(SceneChanges, Scene.RootNode);
    Scene.ChangedAll;
  end;
end;

end.
