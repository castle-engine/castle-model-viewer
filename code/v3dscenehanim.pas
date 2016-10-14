{
  Copyright 2006-2016 Michalis Kamburelis.

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

{ H-Anim Humanoid visualization (shows joints and such), useful to debug
  your humanoid. }
unit V3DSceneHAnim;

interface

uses X3DNodes;

type
  THumanoidVisualization = class
    { Set before using VisualizeHumanoid }
    JointVisualizationSize: Single;
    { Read after using VisualizeHumanoid }
    HumanoidsProcessed: Cardinal;
    JointsProcessed: Cardinal;
    procedure VisualizeHumanoid(Node: TX3DNode);
  end;

implementation

uses SysUtils;

procedure THumanoidVisualization.VisualizeHumanoid(Node: TX3DNode);

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
  SphereShape, TextShape: TShapeNode;
  SphereGeometry: TSphereNode;
  SphereAppearance: TAppearanceNode;
  SphereMaterial: TMaterialNode;
  TextGeometry: TTextNode;
  FontStyle: TFontStyleNode;
  I: Integer;
  JointTransform: TTransformNode;
  CenterRoute: TX3DRoute;
const
  MatName = 'HumanoidJointVisualizeMat';
begin
  HumanoidNode := Node as THAnimHumanoidNode;
  Inc(HumanoidsProcessed);

  { make all existing skin shapes transparent.
    This helps to see joints and their names through }
  for I := 0 to HumanoidNode.FdSkin.Count - 1 do
    if HumanoidNode.FdSkin.Items[I] is TShapeNode then
      MakeShapeTransparent(TShapeNode(HumanoidNode.FdSkin.Items[I]));

  { create sphere Shape for joints.
    All joint visualizations share the same sphere Shape.
    This way they also share the same material, useful for view3dscene
    "Edit Material" menu item. }
  SphereShape := TShapeNode.Create('', HumanoidNode.BaseUrl);

  SphereGeometry := TSphereNode.Create('', HumanoidNode.BaseUrl);
  SphereGeometry.FdRadius.Value := JointVisualizationSize / 2;
  SphereShape.FdGeometry.Value := SphereGeometry;

  SphereAppearance := TAppearanceNode.Create('', HumanoidNode.BaseUrl);
  SphereShape.FdAppearance.Value := SphereAppearance;

  SphereMaterial := TMaterialNode.Create(MatName, HumanoidNode.BaseUrl);
  SphereMaterial.FdTransparency.Value := 0.3;
  SphereAppearance.FdMaterial.Value := SphereMaterial;

  { for each joint, add it's visualization }
  for I := 0 to HumanoidNode.FdJoints.Count - 1 do
    if HumanoidNode.FdJoints.Items[I] is THAnimJointNode then
    begin
      Joint := THAnimJointNode(HumanoidNode.FdJoints.Items[I]);
      Inc(JointsProcessed);

      JointTransform := TTransformNode.Create(
        Format('HAnimVisualization_JointTransform_%d_%s', [I, Joint.NodeName]),
          HumanoidNode.BaseUrl);
      JointTransform.FdTranslation.Value := Joint.FdCenter.Value;
      HumanoidNode.FdSkin.Add(JointTransform);

      CenterRoute := TX3DRoute.Create;
      CenterRoute.SetSourceDirectly(Joint.FdCenter);
      CenterRoute.SetDestinationDirectly(JointTransform.FdTranslation);
      { Brutally force saving of routes *after* normal nodes.
        This is necessary, since the saved file is a mixture of stuff
        read from file and created by code. So we either try to
        set to zero all PositionInParent, or we try to set them good. }
      CenterRoute.PositionInParent := 1000 * 1000 + I;
      HumanoidNode.AddRoute(CenterRoute);

      JointTransform.FdChildren.Add(SphereShape);

      TextShape := TShapeNode.Create('', HumanoidNode.BaseUrl);
      JointTransform.FdChildren.Add(TextShape);

      TextGeometry := TTextNode.Create('', HumanoidNode.BaseUrl);
      TextGeometry.FdString.Items.Add(Joint.FdName.Value);
      TextShape.FdGeometry.Value := TextGeometry;

      FontStyle := TFontStyleNode.Create('', HumanoidNode.BaseUrl);
      FontStyle.FdSize.Value := JointVisualizationSize;
      TextGeometry.FdFontStyle.Value := FontStyle;
    end;

  SphereShape.FreeIfUnused;
  SphereShape := nil;
end;

end.
