{
  Copyright 2003-2013 Michalis Kamburelis.

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

{$I castleconf.inc}

interface

uses CastleVectors, SysUtils, CastlePrecalculatedAnimation, CastleUtils, X3DNodes;

var
  SceneLightsCount: Cardinal;

{ Inits SceneLightsCount. }
procedure SceneInitLights(SceneAnimation: TCastlePrecalculatedAnimation;
  NavigationNode: TNavigationInfoNode);

var
  LightModelAmbient: TVector3Single;

procedure LightModelAmbientChanged;

implementation

uses CastleGL, CastleGLUtils, CastleParameters, V3DSceneFillMode;

procedure SceneInitLights(SceneAnimation: TCastlePrecalculatedAnimation;
  NavigationNode: TNavigationInfoNode);
begin
  if not SceneAnimation.Loaded then
    SceneLightsCount := 0 else
    { If Loaded, then 1st scene exists and has RootNode <> nil
      (that's because loaded animation always has at least one RootNode) }
    SceneLightsCount := SceneAnimation.Scenes[0].RootNode.
      NodesCount(TAbstractLightNode, true);
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
