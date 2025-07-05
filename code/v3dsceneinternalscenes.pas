{
  Copyright 2006-2022 Michalis Kamburelis.

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

{ Internal castle-model-viewer scenes to visualize debug stuff. }
unit V3DSceneInternalScenes;

interface

uses Classes,
  CastleScene, X3DNodes, CastleBoxes, CastleVectors,
  { TDebugEdgesScene needs to access internal shape information to visualize it }
  CastleShapeInternalShadowVolumes;

type
  TInternalScene = class(TCastleScene)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBoundingBoxScene = class(TInternalScene)
  strict private
    TransformNode: TTransformNode;
    Box: TBoxNode;
    Shape: TShapeNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateBox(const ABox: TBox3D);
  end;

  TDebugEdgesScene = class(TInternalScene)
  strict private
    BorderLines, BorderMatchingLines, SilhouetteLines: TLineSetNode;
    BorderCoord, BorderMatchingCoord, SilhouetteCoord: TCoordinateNode;
    procedure AddSilhouetteEdges(const ObserverPos: TVector4;
      const ShapeTransform: TMatrix4;
      const ShapeShadowVolumes: TShapeShadowVolumes);
    procedure AddBorderEdges(
      const ShapeTransform: TMatrix4;
      const ShapeShadowVolumes: TShapeShadowVolumes);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateEdges(const SourceScene: TCastleScene);
  end;

implementation

uses SysUtils,
  CastleColors, CastleShapes, CastleTriangles, CastleUtils;

{ TInternalScene ------------------------------------------------------------- }

constructor TInternalScene.Create(AOwner: TComponent);
begin
  inherited;
  Collides := false;
  Pickable := false;
  CastShadows := false;
  { Otherwise bbox from previous scene would affect AssignDefaultCamera
    and AssignDefaultNavigation calls done right after new scene is loaded. }
  InternalExcludeFromParentBoundingVolume := true;
end;

{ TBoundingBoxScene ---------------------------------------------------------- }

constructor TBoundingBoxScene.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
begin
  inherited;

  Box := TBoxNode.Create;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := GreenRGB;

  Appearance := TAppearanceNode.Create;
  Appearance.ShadowCaster := false;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := Box;
  Shape.Shading := shWireframe;
  Shape.Appearance := Appearance;

  TransformNode := TTransformNode.Create;
  TransformNode.AddChildren(Shape);

  Root := TX3DRootNode.Create;
  Root.AddChildren(TransformNode);

  Load(Root, true);
end;

procedure TBoundingBoxScene.UpdateBox(const ABox: TBox3D);
begin
  Shape.Visible := not ABox.IsEmpty;
  if Shape.Visible then
  begin
    TransformNode.Translation := ABox.Center;
    Box.Size := ABox.Size;
  end;
end;

{ TDebugEdgesScene ----------------------------------------------------------- }

constructor TDebugEdgesScene.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
  Shape: TShapeNode;
  BorderAppearance,  BorderMatchingAppearance, SilhouetteAppearance: TAppearanceNode;
  BorderMaterial,  BorderMatchingMaterial, SilhouetteMaterial: TMaterialNode;
begin
  inherited;

  Root := TX3DRootNode.Create;

  // calculate BorderLines, BorderMaterial, BorderAppearance, BorderCoord

  BorderLines := TLineSetNode.Create;
  BorderLines.Mode := lmPair;

  BorderMaterial := TMaterialNode.Create;
  BorderMaterial.EmissiveColor := Vector3(0, 0, 1);

  BorderAppearance := TAppearanceNode.Create;
  BorderAppearance.Material := BorderMaterial;

  Shape := TShapeNode.Create;
  Shape.Geometry := BorderLines;
  Shape.Appearance := BorderAppearance;
  Root.AddChildren(Shape);

  Shape.Appearance.LineProperties := TLinePropertiesNode.Create;
  Shape.Appearance.LineProperties.LineWidthScaleFactor := 3;

  BorderCoord := TCoordinateNode.Create;
  BorderLines.Coord := BorderCoord;

  // calculate BorderMatchingLines, BorderMatchingMaterial, BorderMatchingAppearance, BorderMatchingCoord

  BorderMatchingLines := TLineSetNode.Create;
  BorderMatchingLines.Mode := lmPair;

  BorderMatchingMaterial := TMaterialNode.Create;
  BorderMatchingMaterial.EmissiveColor := Vector3(1, 1, 1);

  BorderMatchingAppearance := TAppearanceNode.Create;
  BorderMatchingAppearance.Material := BorderMatchingMaterial;

  Shape := TShapeNode.Create;
  Shape.Geometry := BorderMatchingLines;
  Shape.Appearance := BorderMatchingAppearance;
  Root.AddChildren(Shape);

  BorderMatchingCoord := TCoordinateNode.Create;
  BorderMatchingLines.Coord := BorderMatchingCoord;

  // calculate SilhouetteLines, SilhouetteMaterial, SilhouetteAppearance, SilhouetteCoord

  SilhouetteLines := TLineSetNode.Create;
  SilhouetteLines.Mode := lmPair;

  SilhouetteMaterial := TMaterialNode.Create;
  SilhouetteMaterial.EmissiveColor := Vector3(1, 1, 0);

  SilhouetteAppearance := TAppearanceNode.Create;
  SilhouetteAppearance.Material := SilhouetteMaterial;

  Shape := TShapeNode.Create;
  Shape.Geometry := SilhouetteLines;
  Shape.Appearance := SilhouetteAppearance;
  Root.AddChildren(Shape);

  SilhouetteCoord := TCoordinateNode.Create;
  SilhouetteLines.Coord := SilhouetteCoord;

  Load(Root, true);
end;

procedure TDebugEdgesScene.UpdateEdges(const SourceScene: TCastleScene);
var
  ObserverPos: TVector4;
  ShapeList: TShapeList;
  Shape: TShape;
begin
  if World.MainCamera = nil then Exit;
  ObserverPos := Vector4(World.MainCamera.Translation, 1);

  BorderCoord.FdPoint.Items.Clear;
  BorderMatchingCoord.FdPoint.Items.Clear;
  SilhouetteCoord.FdPoint.Items.Clear;

  WholeSceneManifoldDetectionForceAllEdges := true;
  { Force recalculation of manifold/border edges,
    to make them with WholeSceneManifoldDetectionForceAllEdges=true. }
  SourceScene.ChangedAll;

  { Make sure InternalDetectedWholeSceneManifold is calculated, to update
    border edges Triangles[1] values, which are used to determine
    how each border edge is rendered.

    Testcase when this is needed: open in Castle Model Viewer a model
    without light source casting shadow volumes, and (without using menu item
    "Help -> 2 Manifold Info...") switch to
    "View -> Fill Mode -> Silhouette and Border Edges". }
  SourceScene.InternalDetectedWholeSceneManifold;

  // restore WholeSceneManifoldDetectionForceAllEdges
  WholeSceneManifoldDetectionForceAllEdges := false;

  ShapeList := SourceScene.Shapes.TraverseList({ OnlyActive } true, { OnlyVisible } true);
  for Shape in ShapeList do
  begin
    AddSilhouetteEdges(ObserverPos, Shape.State.Transformation.Transform, Shape.InternalShadowVolumes);
    AddBorderEdges(Shape.State.Transformation.Transform, Shape.InternalShadowVolumes);
  end;

  ChangedAll;
end;

procedure TDebugEdgesScene.AddSilhouetteEdges(const ObserverPos: TVector4;
  const ShapeTransform: TMatrix4;
  const ShapeShadowVolumes: TShapeShadowVolumes);

{ This is actually a modified implementation of
  TCastleScene.RenderSilhouetteShadowQuads: instead of rendering
  shadow quad for each silhouette edge, the edge is simply added to list. }

var
  Triangles: TTriangle3List;
  EdgePtr: PEdge;

  procedure RenderEdge(
    const P0Index, P1Index: Cardinal);
  var
    V0, V1: TVector3;
    EdgeV0, EdgeV1: PVector3;
    TrianglePtr: PTriangle3;
  begin
    TrianglePtr := PTriangle3(Triangles.Ptr(EdgePtr^.Triangles[0]));
    EdgeV0 := @TrianglePtr^.Data[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^.Data[(EdgePtr^.VertexIndex + P1Index) mod 3];

    V0 := ShapeTransform.MultPoint(EdgeV0^);
    V1 := ShapeTransform.MultPoint(EdgeV1^);

    SilhouetteCoord.FdPoint.Items.AddRange([V0, V1]);
  end;

  function PlaneSide(const T: TTriangle3): boolean;
  var
    Plane: TVector4;
  begin
    Plane := TrianglePlane(
      ShapeTransform.MultPoint(T.Data[0]),
      ShapeTransform.MultPoint(T.Data[1]),
      ShapeTransform.MultPoint(T.Data[2]));
    Result := (Plane[0] * ObserverPos[0] +
               Plane[1] * ObserverPos[1] +
               Plane[2] * ObserverPos[2] +
               Plane[3] * ObserverPos[3]) > 0;
  end;

var
  I: Integer;
  TrianglePtr: PTriangle3;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TBooleanList;
  Edges: TEdgeList;
begin
  Triangles := ShapeShadowVolumes.TrianglesListShadowCasters;
  Edges := ShapeShadowVolumes.ManifoldEdges;

  TrianglesPlaneSide := TBooleanList.Create;
  try
    { calculate TrianglesPlaneSide array }
    TrianglesPlaneSide.Count := Triangles.Count;
    TrianglePtr := PTriangle3(Triangles.L);
    for I := 0 to Triangles.Count - 1 do
    begin
      TrianglesPlaneSide.L[I] := PlaneSide(TrianglePtr^);
      Inc(TrianglePtr);
    end;

    { for each edge, possibly render it's shadow quad }
    EdgePtr := PEdge(Edges.L);
    for I := 0 to Edges.Count - 1 do
    begin
      PlaneSide0 := TrianglesPlaneSide.L[EdgePtr^.Triangles[0]];
      PlaneSide1 := TrianglesPlaneSide.L[EdgePtr^.Triangles[1]];

      if PlaneSide0 <> PlaneSide1 then
        RenderEdge(0, 1);

      Inc(EdgePtr);
    end;

  finally FreeAndNil(TrianglesPlaneSide) end;
end;

procedure TDebugEdgesScene.AddBorderEdges(
  const ShapeTransform: TMatrix4;
  const ShapeShadowVolumes: TShapeShadowVolumes);
var
  Triangles: TTriangle3List;
  EdgePtr: PEdge;

  procedure RenderEdge;
  var
    V0, V1: TVector3;
    EdgeV0, EdgeV1: PVector3;
    TrianglePtr: PTriangle3;
  begin
    TrianglePtr := PTriangle3(Triangles.Ptr(EdgePtr^.Triangles[0]));
    EdgeV0 := @TrianglePtr^.Data[(EdgePtr^.VertexIndex + 0) mod 3];
    EdgeV1 := @TrianglePtr^.Data[(EdgePtr^.VertexIndex + 1) mod 3];

    V0 := ShapeTransform.MultPoint(EdgeV0^);
    V1 := ShapeTransform.MultPoint(EdgeV1^);

    if EdgePtr^.Triangles[1] = High(Cardinal) then
      BorderMatchingCoord.FdPoint.Items.AddRange([V0, V1])
    else
      BorderCoord.FdPoint.Items.AddRange([V0, V1]);
  end;

var
  I: Integer;
  Edges: TEdgeList;
begin
  Triangles := ShapeShadowVolumes.TrianglesListShadowCasters;
  Edges := ShapeShadowVolumes.BorderEdges;

  { for each edge, render it }
  EdgePtr := PEdge(Edges.L);
  for I := 0 to Edges.Count - 1 do
  begin
    RenderEdge;
    Inc(EdgePtr);
  end;
end;

end.
