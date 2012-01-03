{
  Copyright 2010-2011 Michalis Kamburelis.

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

uses Classes, CastleUtils, UIControls, CastleWindow, GLShaders;

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
    Shaders: array [TScreenEffect] of TGLSLProgram;
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

uses SysUtils, CastleGLUtils, CastleWarnings;

const
  ScreenEffectsInfo: array [TScreenEffect] of record
    Name: string;
    Code: string;
    NeedsDepth: boolean;
  end = (
    (Name: 'Visualize Depth';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       '/* Do not use screen_depth as sampler2DRectShadow with shadow2DRect[Proj],' +NL+
       '   instead just treat it as luminance texture. */' +NL+
       'uniform sampler2DRect screen_depth;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen_depth, gl_TexCoord[0].st);' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(20.0, 20.0, 20.0));' +NL+
       '}';
     NeedsDepth: true),
    (Name: 'Grayscale';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
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
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/2.2, 1.0/2.2, 1.0/2.2));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 4.0 (Brighten More)';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(1.0/4.0, 1.0/4.0, 1.0/4.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 1 / 1.5 (Darken)';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(15.0/10.0, 15.0/10.0, 15.0/10.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Gamma 1 / 2.2 (Darken More)';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
       '  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(22.0/10.0, 22.0/10.0, 22.0/10.0));' +NL+
       '}';
     NeedsDepth: false),
    (Name: 'Flashlight (Nice Headlight)';
     Code: {$I screen_effects_flashlight.glsl.inc};
     NeedsDepth: true),
    (Name: 'Negative';
     Code:
       '#extension GL_ARB_texture_rectangle : enable' +nl+
       'uniform sampler2DRect screen;' +NL+
       'void main (void)' +NL+
       '{' +NL+
       '  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' +NL+
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
      if (TGLSLProgram.ClassSupport <> gsNone) and
         GL_ARB_texture_rectangle then
      begin
        try
          Shaders[SE] := TGLSLProgram.Create;
          Shaders[SE].AttachFragmentShader(ScreenEffectsInfo[SE].Code);
          Shaders[SE].Link(true);
          Shaders[SE].UniformNotFoundAction := uaIgnore;
        except
          on E: EGLSLError do
          begin
            OnWarning(wtMinor, 'GLSL', 'Error when initializing GLSL shader for ScreenEffect[' + ScreenEffectsInfo[SE].Name + ']: ' + E.Message);
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
