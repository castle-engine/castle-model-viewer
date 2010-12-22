{
  Copyright 2006-2010 Michalis Kamburelis.

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

{ H-Anim Humanoid visualization (shows joints and such), useful to debug
  your humanoid. }
unit V3DSceneHAnim;

interface

uses VRMLNodes;

type
  THumanoidVisualization = class
    { Set before using VisualizeHumanoid }
    JointVisualizationSize: Single;
    { Read after using VisualizeHumanoid }
    HumanoidsProcessed: Cardinal;
    JointsProcessed: Cardinal;
    procedure VisualizeHumanoid(Node: TVRMLNode);
  end;

implementation

procedure THumanoidVisualization.VisualizeHumanoid(Node: TVRMLNode);

  { Change shape's material to be transparent.
    This doesn't guarantee that material is changed, as we don't want to
    change the way shape is displayed (so we don't add Appearance
    or Material if they didn't exist etc.) }
  procedure MakeShapeTransparent(const Shape: TNodeShape);
  var
    Mat: TNodeMaterial_2;
  begin
    if Shape.Appearance <> nil then
    begin
      if (Shape.Appearance.FdMaterial.Value <> nil) and
         (Shape.Appearance.FdMaterial.Value is TNodeMaterial_2) then
      begin
        Mat := TNodeMaterial_2(Shape.Appearance.FdMaterial.Value);
        if Mat.FdTransparency.Value = 0 then
          Mat.FdTransparency.Value := 0.5;
      end;
    end;
  end;

var
  HumanoidNode: TNodeHAnimHumanoid;
  Joint: TNodeHAnimJoint;
  SphereShape, TextShape: TNodeShape;
  SphereGeometry: TNodeSphere_2;
  SphereAppearance: TNodeAppearance;
  SphereMaterial: TNodeMaterial_2;
  TextGeometry: TNodeText;
  FontStyle: TNodeFontStyle_2;
  I: Integer;
  JointTransform: TNodeTransform_2;
  CenterRoute: TVRMLRoute;
const
  MatName = 'HumanoidJointVisualizeMat';
begin
  HumanoidNode := Node as TNodeHAnimHumanoid;
  Inc(HumanoidsProcessed);

  { make all existing skin shapes transparent.
    This helps to see joints and their names through }
  for I := 0 to HumanoidNode.FdSkin.Count - 1 do
    if HumanoidNode.FdSkin.Items[I] is TNodeShape then
      MakeShapeTransparent(TNodeShape(HumanoidNode.FdSkin.Items[I]));

  { create sphere Shape for joints.
    All joint visualizations share the same sphere Shape.
    This way they also share the same material, useful for view3dscene
    "Edit Material" menu item. }
  SphereShape := TNodeShape.Create('', HumanoidNode.WWWBasePath);

  SphereGeometry := TNodeSphere_2.Create('', HumanoidNode.WWWBasePath);
  SphereGeometry.FdRadius.Value := JointVisualizationSize / 2;
  SphereShape.FdGeometry.Value := SphereGeometry;

  SphereAppearance := TNodeAppearance.Create('', HumanoidNode.WWWBasePath);
  SphereShape.FdAppearance.Value := SphereAppearance;

  SphereMaterial := TNodeMaterial_2.Create(MatName, HumanoidNode.WWWBasePath);
  SphereMaterial.FdTransparency.Value := 0.3;
  SphereAppearance.FdMaterial.Value := SphereMaterial;

  { for each joint, add it's visualization }
  for I := 0 to HumanoidNode.FdJoints.Count - 1 do
    if HumanoidNode.FdJoints.Items[I] is TNodeHAnimJoint then
    begin
      Joint := TNodeHAnimJoint(HumanoidNode.FdJoints.Items[I]);
      Inc(JointsProcessed);

      JointTransform := TNodeTransform_2.Create('', HumanoidNode.WWWBasePath);
      JointTransform.FdTranslation.Value := Joint.FdCenter.Value;
      HumanoidNode.FdSkin.AddItem(JointTransform);

      CenterRoute := TVRMLRoute.Create;
      CenterRoute.SetSourceDirectly(Joint.FdCenter);
      CenterRoute.SetDestinationDirectly(JointTransform.FdTranslation);
      HumanoidNode.Routes.Add(CenterRoute);

      JointTransform.FdChildren.AddItem(SphereShape);

      TextShape := TNodeShape.Create('', HumanoidNode.WWWBasePath);
      JointTransform.FdChildren.AddItem(TextShape);

      TextGeometry := TNodeText.Create('', HumanoidNode.WWWBasePath);
      TextGeometry.FdString.Items.Add(Joint.FdName.Value);
      TextShape.FdGeometry.Value := TextGeometry;

      FontStyle := TNodeFontStyle_2.Create('', HumanoidNode.WWWBasePath);
      FontStyle.FdSize.Value := JointVisualizationSize;
      TextGeometry.FdFontStyle.Value := FontStyle;
    end;

  SphereShape.FreeIfUnused;
  SphereShape := nil;
end;

end.
