{
  Copyright 2010-2010 Michalis Kamburelis.

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

{ Screen effects (for TKamAbstractViewport.ScreenEffects) available
  in view3dscene menu. }
unit V3DSceneScreenEffects;

interface

uses Classes, KambiUtils, UIControls, GLWindow, GLShaders;

type
  TScreenEffect = (seGrayscale, seNegative, seGamma22, seGamma4, seEdgeDetect,
    seRoundHeadLight);

  TScreenEffects = class(TUIControl)
  private
    MenuItems: array [TScreenEffect] of TMenuItemChecked;
    Shaders: array [TScreenEffect] of TGLSLProgram;
    FActiveEffectsCount: Integer;
  public
    Menu: TMenu;

    constructor Create(AOwner: TComponent); override;

    procedure GLContextInit; override;
    procedure GLContextClose; override;

    property ActiveEffectsCount: Integer read FActiveEffectsCount;
    procedure ActiveEffectsRecalculate;
    function ActiveEffects(const Index: Integer): TGLSLProgram;
  end;

var
  ScreenEffects: TScreenEffects;

implementation

uses SysUtils, KambiGLUtils, DataErrors, KambiLog;

const
  ScreenEffectsNames: array [TScreenEffect] of string =
  ('Grayscale', 'Negative', 'Gamma 2.2', 'Gamma 4.0', 'Edge Detect', 'Round HeadLight');

  ScreenEffectsCode: array [TScreenEffect] of string =
  ('#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
   '  gl_FragColor.r = (gl_FragColor.r + gl_FragColor.g + gl_FragColor.b) / 3.0;' +NL+
   '  gl_FragColor.g = gl_FragColor.r;' +NL+
   '  gl_FragColor.b = gl_FragColor.r;' +NL+
   '}',

   '#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
   '  gl_FragColor.rgb = vec3(1.0, 1.0, 1.0) - gl_FragColor.rgb;' +NL+
   '}',

   '#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
   '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/2.2, 1.0/2.2, 1.0/2.2));' +NL+
   '}',

   '#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
   '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/4.0, 1.0/4.0, 1.0/4.0));' +NL+
   '}',

   '#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  vec4 left   = texture2DRect(screen, vec2(gl_TexCoord[0].s - 1.0, gl_TexCoord[0].t));' +NL+
   '  vec4 right  = texture2DRect(screen, vec2(gl_TexCoord[0].s + 1.0, gl_TexCoord[0].t));' +NL+
   '  vec4 top    = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t + 1.0));' +NL+
   '  vec4 bottom = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t - 1.0));' +NL+
   '  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;' +NL+
   '}',

   '#extension GL_ARB_texture_rectangle : enable' +nl+
   'uniform sampler2DRect screen;' +NL+
   'uniform int screen_width;' +NL+
   'uniform int screen_height;' +NL+
   'void main (void)' +NL+
   '{' +NL+
   '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
   '  float dist = distance(gl_FragCoord.st, vec2(screen_width, screen_height) / 2.0);' +NL+
   '  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;' +NL+
   '  /* The magnificent Radeon fglrx crap refuses to correctly do "* 0.8" below */' +NL+
   '  float radius_in = 4.0 * radius_out / 5.0;' +NL+
   '  float p = mix(1.0 / 2.0, 1.0, smoothstep(radius_in, radius_out, dist));' +NL+
   '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));' +NL+
   '}'
  );

constructor TScreenEffects.Create(AOwner: TComponent);
var
  SE: TScreenEffect;
begin
  inherited;
  Menu := TMenu.Create('Screen Effects');
  for SE := Low(SE) to High(SE) do
  begin
    MenuItems[SE] := TMenuItemChecked.Create(
      SQuoteMenuEntryCaption(ScreenEffectsNames[SE]), 350, false, true);
    Menu.Append(MenuItems[SE]);
  end;
end;

procedure TScreenEffects.GLContextInit;
var
  SE: TScreenEffect;
begin
  inherited;
  for SE := Low(SE) to High(SE) do
    if Shaders[SE] = nil then
    begin
      if (TGLSLProgram.ClassSupport <> gsNone) and
         GL_ARB_texture_rectangle then
      begin
        try
          Shaders[SE] := TGLSLProgram.Create;
          Shaders[SE].AttachFragmentShader(ScreenEffectsCode[SE]);
          Shaders[SE].Link(true);
          Shaders[SE].UniformNotFoundAction := uaIgnore;
        except
          on E: EGLSLError do
          begin
            DataWarning('Error when initializing GLSL shader for ScreenEffect[' + ScreenEffectsNames[SE] + ']: ' + E.Message);
            FreeAndNil(Shaders[SE]);
          end;
        end;
      end;
    end;
  ActiveEffectsRecalculate;
end;

procedure TScreenEffects.GLContextClose;
var
  SE: TScreenEffect;
begin
  for SE := Low(SE) to High(SE) do
    FreeAndNil(Shaders[SE]);
  inherited;
end;

procedure TScreenEffects.ActiveEffectsRecalculate;
var
  SE: TScreenEffect;
begin
  FActiveEffectsCount := 0;
  for SE := Low(SE) to High(SE) do
    if MenuItems[SE].Checked and (Shaders[SE] <> nil) then
      Inc(FActiveEffectsCount);
end;

function TScreenEffects.ActiveEffects(const Index: Integer): TGLSLProgram;
var
  SE: TScreenEffect;
  I: Integer;
begin
  I := Index;
  for SE := Low(SE) to High(SE) do
    if MenuItems[SE].Checked and (Shaders[SE] <> nil) then
    begin
      if I = 0 then
        Exit(Shaders[SE]) else
        Dec(I);
    end;
end;

initialization
  ScreenEffects := TScreenEffects.Create(nil);
finalization
  FreeAndNil(ScreenEffects);
end.
