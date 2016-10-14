{
  Copyright 2010-2016 Michalis Kamburelis.

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

{ Screen effects (for TKamAbstractViewport.ScreenEffects) available
  in view3dscene menu. }
unit V3DSceneScreenEffects;

interface

uses Classes, CastleUtils, CastleUIControls, CastleWindow, CastleGLShaders,
  CastleSceneManager, CastleScreenEffects;

type
  { Screen effects predefined in view3dscene.
    The order below matters: that's the order in which they will be applied.
    Some findings:
    - seNegative looks best after at least seGamma*
    - seEdgeDetect looks best before gamma correction (including seRoundHeadLight)
    - seVisualizeDepth overrides color, so it's sensible to place it
      at the beginning (otherwise it just cancels any other effect)
  }
  TScreenEffect = (seVisualizeDepth, seGrayscale, seEdgeDetect,
    seGammaBrighten, seGammaBrightenMore, seGammaDarken, seGammaDarkenMore,
    seRoundHeadLight, seNegative);

  TScreenEffects = class(TUIControl)
  private
    MenuItems: array [TScreenEffect] of TMenuItemChecked;
    Shaders: array [TScreenEffect] of TGLSLScreenEffect;
    FActiveEffectsCount: Integer;
  public
    Menu: TMenu;

    constructor Create(AOwner: TComponent); override;

    procedure GLContextOpen; override;
    procedure GLContextClose; override;

    property ActiveEffectsCount: Integer read FActiveEffectsCount;
    procedure ActiveEffectsRecalculate;
    function ActiveEffects(const Index: Integer): TGLSLProgram;
    function ActiveEffectsNeedDepth: boolean;
  end;

var
  ScreenEffects: TScreenEffects;

implementation

uses SysUtils, CastleGLUtils, CastleLog, CastleRenderer, CastleKeysMouse;

const
  ScreenEffectsInfo: array [TScreenEffect] of record
    Name: string;
    Code: string;
    NeedsDepth: boolean;
  end = (
    (Name: 'Visualize Depth';
     Code:
       'ivec2 screen_position();' +NL+
       'float screen_get_depth(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  float d = pow(screen_get_depth(screen_position()), 20.0);' +NL+
       '  gl_FragColor = vec4(d, d, d, 1.0);' +NL+
       '}';
     NeedsDepth: true),
    (Name: 'Grayscale';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  /* Use integer (in 256 range) grayscale weights, to avoid crappy fglrx bugs with float consts */' +NL+
       '  gl_FragColor.r = (gl_FragColor.r * 54.0 + gl_FragColor.g * 183.0 + gl_FragColor.b * 19.0) / 256.0;' +NL+
       '  gl_FragColor.g = gl_FragColor.r;' +NL+
       '  gl_FragColor.b = gl_FragColor.r;' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Edge Detect';
     Code: {$I screen_effects_edge_detect.glsl.inc};
     NeedsDepth: false),
    (Name: 'Gamma 2.2 (Brighten)';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/2.2, 1.0/2.2, 1.0/2.2));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 4.0 (Brighten More)';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/4.0, 1.0/4.0, 1.0/4.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 1 / 1.5 (Darken)';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(15.0/10.0, 15.0/10.0, 15.0/10.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 1 / 2.2 (Darken More)';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(22.0/10.0, 22.0/10.0, 22.0/10.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Flashlight (Nice Headlight)';
     Code: {$I screen_effects_flashlight.glsl.inc};
     NeedsDepth: true),
    (Name: 'Negative';
     Code:
       'ivec2 screen_position();' +NL+
       'vec4 screen_get_color(ivec2 position);' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = screen_get_color(screen_position());' +NL+
       '  gl_FragColor.rgb = vec3(1.0, 1.0, 1.0) - gl_FragColor.rgb;' +NL+
       '}';
     NeedsDepth: false)
  );

constructor TScreenEffects.Create(AOwner: TComponent);
var
  SE: TScreenEffect;
begin
  inherited;
  Menu := TMenu.Create('Screen Effects');

  { add built-in screen effects }
  Menu.Append(TMenuItemChecked.Create('Screen Space Ambient Occlusion',
    340, TCastleAbstractViewport.DefaultScreenSpaceAmbientOcclusion, true));
  Menu.Append(TMenuSeparator.Create);

  { add custom screen effects handled in this unit }
  for SE := Low(SE) to High(SE) do
  begin
    MenuItems[SE] := TMenuItemChecked.Create(
      SQuoteMenuEntryCaption(ScreenEffectsInfo[SE].Name), 350, false, true);
    Menu.Append(MenuItems[SE]);
  end;
end;

procedure TScreenEffects.GLContextOpen;
var
  SE: TScreenEffect;
begin
  inherited;
  for SE := Low(SE) to High(SE) do
    if Shaders[SE] = nil then
    begin
      if TGLSLProgram.ClassSupport <> gsNone then
      begin
        try
          Shaders[SE] := TGLSLScreenEffect.Create;
          Shaders[SE].NeedsDepth := ScreenEffectsInfo[SE].NeedsDepth;
          Shaders[SE].ScreenEffectShader := ScreenEffectsInfo[SE].Code;
          Shaders[SE].Link;
        except
          on E: EGLSLError do
          begin
            WritelnWarning('GLSL', 'Error when initializing GLSL shader for ScreenEffect[' + ScreenEffectsInfo[SE].Name + ']: ' + E.Message);
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
  raise EInternalError.Create('TScreenEffects.ActiveEffects: Invalid index');
end;

function TScreenEffects.ActiveEffectsNeedDepth: boolean;
var
  SE: TScreenEffect;
begin
  for SE := Low(SE) to High(SE) do
    if MenuItems[SE].Checked and
       (Shaders[SE] <> nil) and
       ScreenEffectsInfo[SE].NeedsDepth then
      Exit(true);
  Result := false;
end;

initialization
  ScreenEffects := TScreenEffects.Create(nil);
finalization
  FreeAndNil(ScreenEffects);
end.
