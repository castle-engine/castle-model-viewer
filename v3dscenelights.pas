{
  Copyright 2003-2005 Michalis Kamburelis.

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
}

{ Simple management of OpenGL lights and lights in VRML scene.
  You can freely set properties of any OpenGL lights (their properties
  and enabled state), this will not break anything in this unit,
  with the following exception : properties (but not enabled state)
  of OpenGL light no 0 will determine HeadLight light. }

unit V3DSceneLights;

{$I openglmac.inc}

interface

uses SysUtils, VRMLFlatSceneGL, KambiUtils, VRMLNodes;

var
  LightCalculate: boolean = true;
  HeadLight: boolean = false;

  SceneLightsCount: Cardinal;

{ Inits SceneLightsCount.
  Inits HeadLight (to NavigationNode.FdHeadlight or SceneLightsCount = 0).
  Inits Scene.Attrib_FirstGLFreeLight (to 1). }
procedure SceneInitLights(Scene: TVRMLFlatSceneGL;
  NavigationNode: TNodeNavigationInfo);

procedure BeginRenderSceneWithLights;
procedure EndRenderSceneWithLights;

{ Possibly sets value of LightCalculate based on ParStrings. }
procedure LightsParseParameters;

const
  LightsOptionsHelp =
  '  --light-calculate on|off' +nl+
  '                        Should light calculations be performed ?';

implementation

uses OpenGLh, VectorMath, KambiGLUtils, ParseParametersUnit;

procedure SceneInitLights(scene: TVRMLFlatSceneGL;
  NavigationNode: TNodeNavigationInfo);
begin
  if Scene.RootNode = nil then
    SceneLightsCount := 0 else
    SceneLightsCount := Scene.RootNode.CountNodes(TNodeGeneralLight, true, true);

  if NavigationNode <> nil then
    HeadLight := NavigationNode.FdHeadlight.Value else
    HeadLight := SceneLightsCount = 0;

  Scene.Attributes.FirstGLFreeLight := 1;
end;

procedure BeginRenderSceneWithLights;
begin
 glPushAttrib(GL_LIGHTING_BIT);
   SetGLEnabled(GL_LIGHTING, LightCalculate);
   SetGLEnabled(GL_LIGHT0, HeadLight);
end;

procedure EndRenderSceneWithLights;
begin
 glPopAttrib;
end;

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
   Assert(OptionNum = 0);
   case ArrayPosStr(Argument, ['off', 'on']) of
    0: LightCalculate := false;
    1: LightCalculate := true;
    else raise EInvalidParams.CreateFmt('Invalid argument for option '+
      '--light-calculate. Must be "on" or "off", but is "%s"',
      [Argument]);
   end;
  end;

procedure LightsParseParameters;
const
  Options: array[0..0]of TOption =
  ((Short:#0; Long:'light-calculate'; Argument: oaRequired));
begin
 ParseParameters(Options, OptionProc, nil, true);
end;

end.
