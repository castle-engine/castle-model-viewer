{
  Copyright 2006-2025 Michalis Kamburelis.

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

{ Transformation visualization (to e.g. show skeleton joints),
  particularly useful to debug a skeleton of a skinned animation. }
unit V3DSceneSkeletonVisualize;

{$I v3dsceneconf.inc}

interface

uses X3DNodes, CastleSceneCore;

type
  { Visualize skeleton inside TSkinNode or THAnimHumanoidNode. }
  TSkeletonVisualize = class
  strict private
    { Assigned only between JointVisualizationBegin/End.
      @groupBegin }
    SphereShape: TShapeNode;
    SphereGeometry: TSphereNode;
    SphereAppearance: TAppearanceNode;
    SphereMaterial: TMaterialNode;
    FontStyle: TFontStyleNode;
    { @groupEnd }
    procedure VisualizeHumanoid(Node: TX3DNode);
    procedure VisualizeHAnimJoint(Node: TX3DNode);
    procedure VisualizeSkin(Node: TX3DNode);
    procedure VisualizeTransformInSkin(Node: TX3DNode);
    procedure JointVisualizationBegin(const Scene: TCastleSceneCore);
    procedure JointVisualizationEnd(const Scene: TCastleSceneCore);
    { In case of H-Anim Joint, it is relative to humanoid root,
      and so should be placed in humanoid root.
      In case of other transformations, they are relative to parent transformation,
      and so should be placed in parent transformation. }
    function JointVisualization(const Joint: TX3DNode): TTransformNode;
    procedure MakeShapeTransparent(const Shape: TShapeNode);
  public
    { Set before using VisualizeAllHumanoids or VisualizeAllSkins. }
    JointVisualizationSize: Single;

    { Information available after using VisualizeAllHumanoids or VisualizeAllSkins.
      @groupBegin }
    SkinsProcessed: Cardinal;
    JointsProcessed: Cardinal;
    { @groupEnd }

    { Show H-Anim Humanoid joints. }
    procedure VisualizeAllHumanoids(const Scene: TCastleSceneCore);

    { Show Skin joints. }
    procedure VisualizeAllSkins(const Scene: TCastleSceneCore);
  end;

implementation

uses SysUtils, Math,
  CastleUtils, X3DFields, CastleStringUtils;

procedure TSkeletonVisualize.JointVisualizationBegin(const Scene: TCastleSceneCore);
const
  MatName = 'HumanoidJointVisualizeMat';
begin
  { create sphere Shape for joints.
    All joint visualizations share the same sphere Shape.
    This way they also share the same material, useful for castle-model-viewer
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

  FontStyle := TFontStyleNode.Create;
  FontStyle.Size := JointVisualizationSize;

  Scene.BeginChangesSchedule;
end;

procedure TSkeletonVisualize.JointVisualizationEnd(const Scene: TCastleSceneCore);
begin
  SphereShape.FreeIfUnused;
  SphereShape := nil;

  FontStyle.FreeIfUnused;
  FontStyle := nil;

  Scene.EndChangesSchedule;
end;

procedure TSkeletonVisualize.MakeShapeTransparent(const Shape: TShapeNode);
var
  MatInfo: TMaterialInfo;
begin
  // create Shape.Appearance if necessary
  if Shape.Appearance = nil then
    Shape.Appearance := TAppearanceNode.Create;
  Assert(Shape.Appearance <> nil);
  Shape.Appearance.AlphaMode := amBlend;

  // create Shape.Appearance.Material if necessary
  if Shape.Appearance.Material = nil then
    Shape.Appearance.Material := TMaterialNode.Create;
  Assert(Shape.Appearance.Material <> nil);

  // make transparency at least 0.5
  MatInfo := Shape.Appearance.Material.MaterialInfo;
  Assert(MatInfo <> nil);
  MatInfo.Transparency := Max(0.5, MatInfo.Transparency);
end;

function TSkeletonVisualize.JointVisualization(const Joint: TX3DNode): TTransformNode;
var
  TextShape: TShapeNode;
  TextGeometry: TTextNode;
  CenterRoute: TX3DRoute;
  JointCenter: TSFVec3f;
  JointName: String;
begin
  { Handle Joint being of THAnimJointNode or TTransformNode.
    Note: Not using TTransformFunctionality, as we need to differentiate
    name getting anyway. }
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
  TextGeometry.SetText([JointName]);
  TextShape.Geometry := TextGeometry;

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
var
  HumanoidNode: THAnimHumanoidNode;
  I: Integer;
begin
  HumanoidNode := Node as THAnimHumanoidNode;
  Inc(SkinsProcessed);

  { make all existing skin shapes transparent.
    This helps to see joints and their names through }
  for I := 0 to HumanoidNode.FdSkin.Count - 1 do
    if HumanoidNode.FdSkin[I] is TShapeNode then
      MakeShapeTransparent(TShapeNode(HumanoidNode.FdSkin[I]));
end;

procedure TSkeletonVisualize.VisualizeHAnimJoint(Node: TX3DNode);
var
  Joint: THAnimJointNode;
  JointVis: TTransformNode;
begin
  Joint := Node as THAnimJointNode;

  // avoid processing the same Joint many times
  if Joint.MetadataBoolean['JointVisualization'] then
    Exit;
  Joint.MetadataBoolean['JointVisualization'] := true;

  if Joint.Humanoid = nil then
    Exit; // cannot visualize, joint not part of HAnimHumanoid

  JointVis := JointVisualization(Joint);
  Joint.Humanoid.FdSkin.Add(JointVis);
end;

procedure TSkeletonVisualize.VisualizeAllHumanoids(const Scene: TCastleSceneCore);
begin
  JointVisualizationBegin(Scene);
  try
    Scene.RootNode.EnumerateNodes(THAnimHumanoidNode,
      {$ifdef FPC}@{$endif} VisualizeHumanoid, false);

    { Note: Don't depend on HumanoidNode.FdJoints list for this,
      as it became optional around X3D 4.1.
      So we need EnumerateNodes call for THAnimJointNode. }
    Scene.RootNode.EnumerateNodes(THAnimJointNode,
      {$ifdef FPC}@{$endif} VisualizeHAnimJoint, false);

    if (SkinsProcessed <> 0) or (JointsProcessed <> 0) then
      Scene.ChangedAll;
  finally JointVisualizationEnd(Scene) end;
end;

procedure TSkeletonVisualize.VisualizeTransformInSkin(Node: TX3DNode);
var
  Joint, JointVis: TTransformNode;
begin
  Joint := Node as TTransformNode;

  // ignore adding debug visualization to debug visualization
  if IsPrefix('JointVisualization', Joint.X3DName, false) then
    Exit;

  // avoid processing the same Joint many times, e.g. because it's in both Skin.skeleton and Skin.joints
  if Joint.MetadataBoolean['JointVisualization'] then
    Exit;
  Joint.MetadataBoolean['JointVisualization'] := true;

  JointVis := JointVisualization(Joint);
  Joint.AddChildren(JointVis);
end;

procedure TSkeletonVisualize.VisualizeSkin(Node: TX3DNode);
var
  SkinNode: TSkinNode;
  I: Integer;
begin
  SkinNode := Node as TSkinNode;
  Inc(SkinsProcessed);

  for I := 0 to SkinNode.FdShapes.Count - 1 do
    if SkinNode.FdShapes[I] is TShapeNode then
      MakeShapeTransparent(TShapeNode(SkinNode.FdShapes[I]));

  for I := 0 to SkinNode.FdJoints.Count - 1 do
    if SkinNode.FdJoints[I] is TTransformNode then
      VisualizeTransformInSkin(SkinNode.FdJoints[I]);
end;

procedure TSkeletonVisualize.VisualizeAllSkins(const Scene: TCastleSceneCore);
begin
  JointVisualizationBegin(Scene);
  try
    Scene.RootNode.EnumerateNodes(TSkinNode,
      {$ifdef FPC}@{$endif} VisualizeSkin, false);

    // No need to search for TTransformNode, VisualizeSkin will iterate over joints.
    // This is in contrast to VisualizeAllHumanoids, where we need to search
    // for THAnimJointNode.

    // make sure to call ChangedAll
    if (SkinsProcessed <> 0) or (JointsProcessed <> 0) then
      Scene.ChangedAll;
  finally JointVisualizationEnd(Scene) end;
end;

end.
