{
  Copyright 2006-2020 Michalis Kamburelis.

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

{ Transformation visualization (to e.g. show skeleton joints),
  particularly useful to debug a skeleton of a skinned animation. }
unit V3DSceneSkeletonVisualize;

{$I v3dsceneconf.inc}

interface

uses X3DNodes;

type
  TSkeletonVisualize = class
  strict private
    { Assigned only between JointVisualizationBegin/End.
      @groupBegin }
    SphereShape: TShapeNode;
    SphereGeometry: TSphereNode;
    SphereAppearance: TAppearanceNode;
    SphereMaterial: TMaterialNode;
    { @groupEnd }
    procedure VisualizeHumanoid(Node: TX3DNode);
    procedure VisualizeTransformation(Node: TX3DNode);
    procedure JointVisualizationBegin;
    procedure JointVisualizationEnd;
    { In case of H-Anim Joint, it is relative to humanoid root,
      and so should be placed in humanoid root.
      In case of other transformations, they are relative to parent transformation,
      and so should be placed in parent transformation. }
    function JointVisualization(const Joint: TX3DNode): TTransformNode;
  public
    { Set before using VisualizeXxx }
    JointVisualizationSize: Single;
    { Informatio available after using VisualizeAllHumanoids. }
    HumanoidsProcessed: Cardinal;
    JointsProcessed: Cardinal;
    { Show H-Anim Humanoid joints. }
    procedure VisualizeAllHumanoids(const Node: TX3DNode);
    { Show all transformations. }
    procedure VisualizeAllTransformations(const Node: TX3DNode);
  end;

implementation

uses SysUtils,
  CastleUtils, X3DFields, CastleStringUtils;

procedure TSkeletonVisualize.JointVisualizationBegin;
const
  MatName = 'HumanoidJointVisualizeMat';
begin
  { create sphere Shape for joints.
    All joint visualizations share the same sphere Shape.
    This way they also share the same material, useful for view3dscene
    "Edit Material" menu item. }
  SphereShape := TShapeNode.Create;

  SphereGeometry := TSphereNode.Create;
  SphereGeometry.FdRadius.Value := JointVisualizationSize / 2;
  SphereShape.FdGeometry.Value := SphereGeometry;

  SphereAppearance := TAppearanceNode.Create;
  SphereShape.FdAppearance.Value := SphereAppearance;

  SphereMaterial := TMaterialNode.Create(MatName);
  SphereMaterial.FdTransparency.Value := 0.3;
  SphereAppearance.FdMaterial.Value := SphereMaterial;
end;

procedure TSkeletonVisualize.JointVisualizationEnd;
begin
  SphereShape.FreeIfUnused;
  SphereShape := nil;
end;

function TSkeletonVisualize.JointVisualization(const Joint: TX3DNode): TTransformNode;
var
  TextShape: TShapeNode;
  TextGeometry: TTextNode;
  FontStyle: TFontStyleNode;
  CenterRoute: TX3DRoute;
  JointCenter: TSFVec3f;
  JointName: String;
begin
  { Handle Joint being of THAnimJointNode or TTransformNode.
    TODO: Abstract this using TTransformFunctionality in CGE. }
  if Joint is THAnimJointNode then
  begin
    JointCenter := THAnimJointNode(Joint).FdCenter;
    JointName := THAnimJointNode(Joint).NameField;
    if JointName = '' then
      JointName := Joint.X3DName;
  end else
  if Joint is TTransformNode then
  begin
    JointCenter := TTransformNode(Joint).FdCenter;
    JointName := Joint.X3DName;
  end else
    raise EInternalError.CreateFmt('Cannot handle Joint node class %s', [Joint.ClassName]);

  Result := TTransformNode.Create(
    Format('JointVisualization_%d_%s', [JointsProcessed, JointName]));
  Inc(JointsProcessed);
  Result.Translation := JointCenter.Value;

  TextShape := TShapeNode.Create;
  Result.AddChildren(TextShape);

  TextGeometry := TTextNode.Create;
  TextGeometry.SetString([JointName]);
  TextShape.Geometry := TextGeometry;

  FontStyle := TFontStyleNode.Create;
  FontStyle.Size := JointVisualizationSize;
  TextGeometry.FontStyle := FontStyle;

  Result.AddChildren(SphereShape);

  CenterRoute := TX3DRoute.Create;
  CenterRoute.SetSourceDirectly(JointCenter);
  CenterRoute.SetDestinationDirectly(Result.FdTranslation);
  { Brutally force saving of routes *after* normal nodes.
    This is necessary, since the saved file is a mixture of stuff
    read from file and created by code. So we either try to
    set to zero all PositionInParent, or we try to set them good. }
  CenterRoute.PositionInParent := 1000 * 1000;
  Joint.AddRoute(CenterRoute);
end;

procedure TSkeletonVisualize.VisualizeHumanoid(Node: TX3DNode);

  { Change shape's material to be transparent.
    This doesn't guarantee that material is changed, as we don't want to
    change the way shape is displayed (so we don't add Appearance
    or Material if they didn't exist etc.) }
  procedure MakeShapeTransparent(const Shape: TShapeNode);
  var
    Mat: TMaterialNode;
  begin
    if Shape.Appearance <> nil then
    begin
      if (Shape.Appearance.FdMaterial.Value <> nil) and
         (Shape.Appearance.FdMaterial.Value is TMaterialNode) then
      begin
        Mat := TMaterialNode(Shape.Appearance.FdMaterial.Value);
        if Mat.FdTransparency.Value = 0 then
          Mat.FdTransparency.Value := 0.5;
      end;
    end;
  end;

var
  HumanoidNode: THAnimHumanoidNode;
  Joint: THAnimJointNode;
  JointVis: TTransformNode;
  I: Integer;
begin
  HumanoidNode := Node as THAnimHumanoidNode;
  Inc(HumanoidsProcessed);

  { make all existing skin shapes transparent.
    This helps to see joints and their names through }
  for I := 0 to HumanoidNode.FdSkin.Count - 1 do
    if HumanoidNode.FdSkin[I] is TShapeNode then
      MakeShapeTransparent(TShapeNode(HumanoidNode.FdSkin[I]));

  { for each joint, add it's visualization }
  for I := 0 to HumanoidNode.FdJoints.Count - 1 do
    if HumanoidNode.FdJoints[I] is THAnimJointNode then
    begin
      Joint := THAnimJointNode(HumanoidNode.FdJoints[I]);
      JointVis := JointVisualization(Joint);
      HumanoidNode.FdSkin.Add(JointVis);
    end;
end;

procedure TSkeletonVisualize.VisualizeAllHumanoids(const Node: TX3DNode);
begin
  JointVisualizationBegin;
  try
    Node.EnumerateNodes(THAnimHumanoidNode,
      {$ifdef CASTLE_OBJFPC}@{$endif} VisualizeHumanoid, false);
  finally JointVisualizationEnd end;
end;


procedure TSkeletonVisualize.VisualizeTransformation(Node: TX3DNode);
var
  Joint, JointVis: TTransformNode;
begin
  // ignore adding debug visualization to debug visualization
  if IsPrefix('JointVisualization', Node.X3DName, false) then
    Exit;

  Joint := Node as TTransformNode;
  JointVis := JointVisualization(Joint);
  Joint.AddChildren(JointVis);
end;

procedure TSkeletonVisualize.VisualizeAllTransformations(const Node: TX3DNode);
begin
  JointVisualizationBegin;
  try
    Node.EnumerateNodes(TTransformNode,
      {$ifdef CASTLE_OBJFPC}@{$endif} VisualizeTransformation, false);
  finally JointVisualizationEnd end;
end;

end.
