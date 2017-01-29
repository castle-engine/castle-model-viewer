{
  Copyright 2003-2017 Michalis Kamburelis.

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

{ Simple management of OpenGL lights and lights in VRML scene.
  You can freely set properties of any OpenGL lights (their properties
  and enabled state), this will not break anything in this unit. }

unit V3DSceneLights;

{$I v3dsceneconf.inc}

interface

uses CastleVectors, SysUtils, CastleScene, CastleUtils, X3DNodes;

var
  SceneLightsCount: Cardinal;

{ Inits SceneLightsCount. }
procedure SceneInitLights(Scene: TCastleScene; NavigationNode: TNavigationInfoNode);

implementation

uses CastleGL, CastleGLUtils, CastleParameters, V3DSceneFillMode;

procedure SceneInitLights(Scene: TCastleScene; NavigationNode: TNavigationInfoNode);
begin
  if (Scene = nil) or (Scene.RootNode = nil) then
    SceneLightsCount := 0 else
    { If Loaded, then 1st scene exists and has RootNode <> nil
      (that's because loaded animation always has at least one RootNode) }
    SceneLightsCount := Scene.RootNode.NodesCount(TAbstractLightNode, true);
end;

end.
