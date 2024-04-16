{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Visualizing the octree. }
unit V3DSceneOctreeVisualize;

{$I v3dsceneconf.inc}

interface

uses CastleInternalOctree, CastleWindow, CastleScene, CastleVectors;

type
  TOctreeDisplay = object
  private
    procedure AddDisplayStatus(var S: string);
  public
    { Constructor, initially display is none (Whole = @false and Depth = -1). }
    constructor Init(const AName: string);
  public
    Whole: boolean;

    { Depth to dispay. Meaningful only if Whole = false.
      -1 means "don't display octree". }
    Depth: Integer;

    Name: string;

    MenuWhole: TMenuItemChecked;

    procedure DoMenuIncDepth;
    procedure DoMenuDecDepth;
    procedure DoMenuToggleWhole;
  end;

var
  OctreeTrianglesDisplay: TOctreeDisplay;
  OctreeVisibleShapesDisplay: TOctreeDisplay;
  OctreeCollidableShapesDisplay: TOctreeDisplay;

procedure OctreeDisplay(const Scene: TCastleScene; const ModelViewProjection: TMatrix4);

function OctreeDisplayStatus: string;

implementation

uses SysUtils,
  CastleColors, CastleGLUtils, CastleShapes, CastleBoxes, CastleRenderPrimitives;

{ TOctreeDisplay ------------------------------------------------------------- }

constructor TOctreeDisplay.Init(const AName: string);
begin
  Whole := false;
  Depth := -1;
  Name := AName;
end;

procedure TOctreeDisplay.DoMenuIncDepth;
begin
  if Whole then
  begin
    Whole := false;
    MenuWhole.Checked := Whole;
    Depth := -1;
  end;

  Inc(Depth);
end;

procedure TOctreeDisplay.DoMenuDecDepth;
begin
  if Whole then
  begin
    Whole := false;
    MenuWhole.Checked := Whole;
    Depth := -1;
  end;

  Dec(Depth);
  if Depth < -1 then Depth := -1;
end;

procedure TOctreeDisplay.DoMenuToggleWhole;
begin
  Whole := not Whole;
  if not Whole then
    Depth := -1;
end;

procedure TOctreeDisplay.AddDisplayStatus(var S: string);
begin
  if Whole then
    S += Format(', Octree %s display: whole', [Name]) else
  if Depth <> -1 then
    S += Format(', Octree %s display: depth %d', [Name, Depth]);
end;

{ ---------------------------------------------------------------------------- }

procedure AddBox(const Box: TBox3D; const Vertexes: TVector4List);

  procedure AddLoop(const V: array of TVector4);
  var
    I: Integer;
  begin
    for I := 0 to High(V) - 1 do
    begin
      Vertexes.Add(V[I]);
      Vertexes.Add(V[I + 1]);
    end;

    Vertexes.Add(V[High(V)]);
    Vertexes.Add(V[0]);
  end;

var
  X0, X1, Y0, Y1, Z0, Z1: Single;
begin
  // get box to easier variables
  X0 := Box.Data[0].X;
  X1 := Box.Data[1].X;
  Y0 := Box.Data[0].Y;
  Y1 := Box.Data[1].Y;
  Z0 := Box.Data[0].Z;
  Z1 := Box.Data[1].Z;

  // line loop on Z0
  AddLoop([
    Vector4(X0, Y0, Z0, 1),
    Vector4(X1, Y0, Z0, 1),
    Vector4(X1, Y1, Z0, 1),
    Vector4(X0, Y1, Z0, 1)
  ]);

  // line loop on Z1
  AddLoop([
    Vector4(X0, Y0, Z1, 1),
    Vector4(X1, Y0, Z1, 1),
    Vector4(X1, Y1, Z1, 1),
    Vector4(X0, Y1, Z1, 1)
  ]);

  // connect Z0 with Z1
  Vertexes.AddRange([
    Vector4(X0, Y0, Z0, 1),
    Vector4(X0, Y0, Z1, 1),

    Vector4(X1, Y0, Z0, 1),
    Vector4(X1, Y0, Z1, 1),

    Vector4(X1, Y1, Z0, 1),
    Vector4(X1, Y1, Z1, 1),

    Vector4(X0, Y1, Z0, 1),
    Vector4(X0, Y1, Z1, 1)
  ]);
end;

procedure OctreeDisplay(const Scene: TCastleScene; const ModelViewProjection: TMatrix4);
var
  Vertexes: TVector4List;
  Mesh: TCastleRenderUnlitMesh;

  { Reinitialize (clear and set) Vertexes to define the necessary mesh,
    use Mesh once to render them. }
  procedure DisplayOctreeDepth(const OctreeNode: TOctreeNode;
    const OctreeDisplayDepth: integer; const ModelViewProjection: TMatrix4);

    procedure DisplayOctreeDepthRecursion(const OctreeNode: TOctreeNode;
      const OctreeDisplayDepth: integer);
    var
      b0, b1, b2: boolean;
    begin
      if OctreeNode.Depth = OctreeDisplayDepth then
      begin
        if not (OctreeNode.IsLeaf and (OctreeNode.ItemsCount = 0)) then
          AddBox(OctreeNode.Box, Vertexes);
      end else
      if not OctreeNode.IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeDepthRecursion(OctreeNode.TreeSubNodes[b0, b1, b2], OctreeDisplayDepth);
      end;
    end;

  begin
    Vertexes.Clear;
    DisplayOctreeDepthRecursion(OctreeNode, OctreeDisplayDepth);
    Mesh.ModelViewProjection := ModelViewProjection;
    Mesh.SetVertexes(Vertexes, true);
    Mesh.Render(pmLines);
  end;

  procedure DisplayOctreeTrianglesDepth(const OctreeDisplayDepth: integer);
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    { Octree is not always ready, as it's recalculation during animations
      may hurt. Also, Draw may be called in various situations even when Scene
      is not really ready (e.g. when showing errors after scene loading).
      Also, octrees for particular shapes are not necessarily
      created, since some shapes may be not collidable.
      So we have to carefully check here whether appropriate things
      are initialized. }

    if Scene <> nil then
    begin
      ShapeList := Scene.Shapes.TraverseList(true);
      for Shape in ShapeList do
        if Shape.InternalOctreeTriangles <> nil then
          DisplayOctreeDepth(Shape.InternalOctreeTriangles.TreeRoot,
            OctreeDisplayDepth, ModelViewProjection * Shape.State.Transformation.Transform);
    end;
  end;

  { Reinitialize (clear and set) Vertexes to define the necessary mesh,
    use Mesh once to render them. }
  procedure DisplayOctreeWhole(const OctreeNode: TOctreeNode;
    const ModelViewProjection: TMatrix4);

    procedure DisplayOctreeWholeRecursion(const OctreeNode: TOctreeNode);
    var
      b0, b1, b2: boolean;
    begin
      if not (OctreeNode.IsLeaf and (OctreeNode.ItemsCount = 0)) then
        AddBox(OctreeNode.Box, Vertexes);

      if not OctreeNode.IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeWholeRecursion(OctreeNode.TreeSubNodes[b0, b1, b2]);
      end;
    end;

  begin
    Vertexes.Clear;
    DisplayOctreeWholeRecursion(OctreeNode);
    Mesh.ModelViewProjection := ModelViewProjection;
    Mesh.SetVertexes(Vertexes, true);
    Mesh.Render(pmLines);
  end;

  procedure DisplayOctreeTrianglesWhole;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    { Octree is not always ready, as it's recalculation during animations
      may hurt. Also, Draw may be called in various situations even when Scene
      is not really ready (e.g. when showing errors after scene loading).
      Also, octrees for particular shapes are not necessarily
      created, since some shapes may be not collidable.
      So we have to carefully check here whether appropriate things
      are initialized. }

    if Scene <> nil then
    begin
      ShapeList := Scene.Shapes.TraverseList(true);
      for Shape in ShapeList do
        if Shape.InternalOctreeTriangles <> nil then
          DisplayOctreeWhole(Shape.InternalOctreeTriangles.TreeRoot,
            ModelViewProjection * Shape.State.Transformation.Transform);
    end;
  end;

begin
  Vertexes := TVector4List.Create;
  try
    Mesh := TCastleRenderUnlitMesh.Create(true);
    try
      Mesh.Color := Yellow;
      if OctreeTrianglesDisplay.Whole then
      begin
        DisplayOctreeTrianglesWhole;
      end else
      if OctreeTrianglesDisplay.Depth >= 0 then
      begin
        DisplayOctreeTrianglesDepth(OctreeTrianglesDisplay.Depth);
      end;

      Mesh.Color := Blue;
      if OctreeVisibleShapesDisplay.Whole then
      begin
        DisplayOctreeWhole(Scene.InternalOctreeRendering.TreeRoot, ModelViewProjection);
      end else
      if OctreeVisibleShapesDisplay.Depth >= 0 then
      begin
        DisplayOctreeDepth(Scene.InternalOctreeRendering.TreeRoot,
          OctreeVisibleShapesDisplay.Depth, ModelViewProjection);
      end;

      Mesh.Color := Red;
      if OctreeCollidableShapesDisplay.Whole then
      begin
        DisplayOctreeWhole(Scene.InternalOctreeDynamicCollisions.TreeRoot, ModelViewProjection);
      end else
      if OctreeCollidableShapesDisplay.Depth >= 0 then
      begin
        DisplayOctreeDepth(Scene.InternalOctreeDynamicCollisions.TreeRoot,
          OctreeCollidableShapesDisplay.Depth, ModelViewProjection);
      end;
    finally FreeAndNil(Mesh) end;
  finally FreeAndNil(Vertexes) end;
end;

function OctreeDisplayStatus: string;
begin
  Result := '';
  OctreeTrianglesDisplay       .AddDisplayStatus(Result);
  OctreeVisibleShapesDisplay   .AddDisplayStatus(Result);
  OctreeCollidableShapesDisplay.AddDisplayStatus(Result);
end;

initialization
  OctreeTrianglesDisplay       .Init('triangles');
  OctreeVisibleShapesDisplay   .Init('visible shapes');
  OctreeCollidableShapesDisplay.Init('collidable shapes');
end.
