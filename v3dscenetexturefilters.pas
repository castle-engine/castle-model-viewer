{
  Copyright 2003-2011 Michalis Kamburelis.

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


{ Simple management of TexMin/MagFilter properties of VRML scene/animation.
  Initialize by InitTextureFilters,
  change by SetTextureMin/MagFilter,
  description of currently set filters is in TextureMin/MagFilter. }

unit V3DSceneTextureFilters;

interface

uses GL, GLU, GLExt, KambiUtils, VectorMath, VRMLNodes,
  VRMLGLAnimation, KambiGLUtils, GLWindow;

type
  TTextureMinFilter = (tminNearest, tminLinear, tminNearestMipmapNearest,
    tminNearestMipmapLinear, tminLinearMipmapNearest, tminLinearMipmapLinear);
  TTextureMagFilter = (tmagNearest, tmagLinear);
  TTextureMode = (tmModulate, tmReplace);

const
  TextureMinFilterNames: array [TTextureMinFilter] of string =
  ( 'GL_NEAREST (fastest)',
    'GL_LINEAR',
    'GL_NEAREST_MIPMAP_NEAREST',
    'GL_NEAREST_MIPMAP_LINEAR',
    'GL_LINEAR_MIPMAP_NEAREST',
    'GL_LINEAR_MIPMAP_LINEAR (best looking)');

  TextureMagFilterNames: array [TTextureMagFilter] of string =
  ( 'GL_NEAREST (fastest)',
    'GL_LINEAR (best looking)');

  TextureModeRGBNames: array [TTextureMode] of string =
  ( 'GL_MODULATE (texture * light)',
    'GL_REPLACE (texture color overrides light)' );

procedure InitTextureFilters(SceneAnimation: TVRMLGLAnimation);

function TextureMinFilter: TTextureMinFilter;
procedure SetTextureMinFilter(Value: TTextureMinFilter; SceneAnimation: TVRMLGLAnimation);

function TextureMagFilter: TTextureMagFilter;
procedure SetTextureMagFilter(Value: TTextureMagFilter; SceneAnimation: TVRMLGLAnimation);

function TextureModeRGB: TTextureMode;
procedure SetTextureModeRGB(Value: TTextureMode; SceneAnimation: TVRMLGLAnimation);

procedure MenuAppendTextureMinFilters(M: TMenu; BaseIntData: Cardinal);
procedure MenuAppendTextureMagFilters(M: TMenu; BaseIntData: Cardinal);
procedure MenuAppendTextureModeRGB(M: TMenu; BaseIntData: Cardinal);

implementation

const
  TextureMinFilterToGL: array [TTextureMinFilter] of TGLint =
  ( GL_NEAREST, GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_LINEAR );

  TextureMagFilterToGL: array [TTextureMagFilter] of TGLint =
  ( GL_NEAREST, GL_LINEAR );

  TextureModeRGBToGL: array [TTextureMode] of TGLenum =
  ( GL_MODULATE, GL_REPLACE );

var
  FTextureMinFilter: TTextureMinFilter;
  FTextureMagFilter: TTextureMagFilter;
  FTextureModeRGB: TTextureMode;

procedure InitTextureFilters(SceneAnimation: TVRMLGLAnimation);
begin
 SetTextureMinFilter(tminLinearMipmapLinear, SceneAnimation);
 SetTextureMagFilter(tmagLinear, SceneAnimation);
 SetTextureModeRGB(tmModulate, SceneAnimation);
end;

function TextureMinFilter: TTextureMinFilter;
begin Result := FTextureMinFilter end;

procedure SetTextureMinFilter(Value: TTextureMinFilter; SceneAnimation: TVRMLGLAnimation);
begin
 FTextureMinFilter := value;
 SceneAnimation.Attributes.TextureMinFilter := TextureMinFilterToGL[Value];
end;

function TextureMagFilter: TTextureMagFilter;
begin Result := FTextureMagFilter end;

procedure SetTextureMagFilter(Value: TTextureMagFilter; SceneAnimation: TVRMLGLAnimation);
begin
 FTextureMagFilter := value;
 SceneAnimation.Attributes.TextureMagFilter := TextureMagFilterToGL[Value];
end;

function TextureModeRGB: TTextureMode;
begin Result := FTextureModeRGB end;

procedure SetTextureModeRGB(Value: TTextureMode; SceneAnimation: TVRMLGLAnimation);
begin
 FTextureModeRGB := value;
 SceneAnimation.Attributes.TextureModeRGB := TextureModeRGBToGL[Value];
end;

procedure MenuAppendTextureMinFilters(M: TMenu; BaseIntData: Cardinal);
begin
  M.AppendRadioGroup(TextureMinFilterNames, BaseIntData, Ord(TextureMinFilter), true);
end;

procedure MenuAppendTextureMagFilters(M: TMenu;
  BaseIntData: Cardinal);
begin
  M.AppendRadioGroup(TextureMagFilterNames, BaseIntData, Ord(TextureMagFilter), true);
end;

procedure MenuAppendTextureModeRGB(M: TMenu; BaseIntData: Cardinal);
begin
  M.AppendRadioGroup(TextureModeRGBNames, BaseIntData, Ord(TextureModeRGB), true);
end;

end.
