{
  Copyright 2003-2010 Michalis Kamburelis.

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

{ Simple management of OpenGL lights and lights in VRML scene.
  You can freely set properties of any OpenGL lights (their properties
  and enabled state), this will not break anything in this unit. }

unit V3DSceneLights;

interface

uses VectorMath, SysUtils, VRMLGLAnimation, KambiUtils, VRMLNodes;

var
  SceneLightsCount: Cardinal;

{ Inits SceneLightsCount. }
procedure SceneInitLights(SceneAnimation: TVRMLGLAnimation;
  NavigationNode: TNodeNavigationInfo);

procedure BeginRenderSceneWithLights(SceneAnimation: TVRMLGLAnimation);
procedure EndRenderSceneWithLights;

var
  LightModelAmbient: TVector3Single;

procedure LightModelAmbientChanged;

implementation

uses GL, GLU, KambiGLUtils, ParseParametersUnit, V3DSceneFillMode;

procedure SceneInitLights(SceneAnimation: TVRMLGLAnimation;
  NavigationNode: TNodeNavigationInfo);
begin
  if not SceneAnimation.Loaded then
    SceneLightsCount := 0 else
    { If Loaded, then 1st scene exists and has RootNode <> nil
      (that's because loaded animation always has at least one RootNode) }
    SceneLightsCount := SceneAnimation.Scenes[0].RootNode.
      NodesCount(TVRMLLightNode, true);
end;

procedure BeginRenderSceneWithLights(SceneAnimation: TVRMLGLAnimation);
begin
  glPushAttrib(GL_LIGHTING_BIT);
    { Note that GL_LIGHTING is controlled by Attributes.Lighting
      (when @false, model will not be lit, as we do not enable OpenGL
      lighting ourselves anywhere.)

      Also Attributes.PureGeometry is correctly taken into account.
      (when PureGeometry = true, it's like Lighting = always false.) }
  if SceneAnimation.Attributes.PureGeometry then
    glColorv(PureGeometryColor);
end;

procedure EndRenderSceneWithLights;
begin
 glPopAttrib;
end;

procedure LightModelAmbientChanged;
begin
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(LightModelAmbient, 1.0));
end;

initialization
  LightModelAmbient := Vector3Single(
    GLDefaultLightModelAmbient[0],
    GLDefaultLightModelAmbient[1],
    GLDefaultLightModelAmbient[2]);
end.
