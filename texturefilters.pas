{
  Copyright 2003-2004 Michalis Kamburelis.

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


{ Simple management of TexMin/MagFilter properties of VRML scene/animation.
  Initialize by InitTextureFilters,
  change by SetTextureMin/MagFilter,
  description of currently set filters is in TextureMin/MagFilter. }

unit TextureFilters;

{$I openglmac.inc}

interface

uses OpenGLh, KambiUtils, VectorMath, VRMLNodes,
  VRMLGLAnimation, KambiGLUtils;

type
  TTextureMinFilter = (tminNearest, tminLinear, tminNearestMipmapNearest,
    tminNearestMipmapLinear, tminLinearMipmapNearest, tminLinearMipmapLinear);
  TTextureMagFilter = (tmagNearest, tmagLinear);

const
  TextureMinFilterNames: array[TTextureMinFilter]of string =
  ( 'GL_NEAREST (fastest)',
    'GL_LINEAR',
    'GL_NEAREST_MIPMAP_NEAREST',
    'GL_NEAREST_MIPMAP_LINEAR',
    'GL_LINEAR_MIPMAP_NEAREST',
    'GL_LINEAR_MIPMAP_LINEAR (best looking)');
  TextureMagFilterNames: array[TTextureMagFilter]of string =
  ( 'GL_NEAREST (fastest)',
    'GL_LINEAR (best looking)');

procedure InitTextureFilters(SceneAnimation: TVRMLGLAnimation);

function TextureMinFilter: TTextureMinFilter;
procedure SetTextureMinFilter(Value: TTextureMinFilter; SceneAnimation: TVRMLGLAnimation);

function TextureMagFilter: TTextureMagFilter;
procedure SetTextureMagFilter(Value: TTextureMagFilter; SceneAnimation: TVRMLGLAnimation);

implementation

const
  TextureMinFilterToGL: array[TTextureMinFilter]of TGLint =
  ( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR);
  TextureMagFilterToGL: array[TTextureMagFilter]of TGLint =
  ( GL_NEAREST, GL_LINEAR);

var
  FTextureMinFilter: TTextureMinFilter;
  FTextureMagFilter: TTextureMagFilter;

procedure InitTextureFilters(SceneAnimation: TVRMLGLAnimation);
begin
 SetTextureMinFilter(tminLinearMipmapLinear, SceneAnimation);
 SetTextureMagFilter(tmagLinear, SceneAnimation);
end;

procedure SetTextureMinFilter(Value: TTextureMinFilter; SceneAnimation: TVRMLGLAnimation);
begin
 FTextureMinFilter := value;
 SceneAnimation.Attributes.TextureMinFilter := TextureMinFilterToGL[Value];
end;

procedure SetTextureMagFilter(Value: TTextureMagFilter; SceneAnimation: TVRMLGLAnimation);
begin
 FTextureMagFilter := value;
 SceneAnimation.Attributes.TextureMagFilter := TextureMagFilterToGL[Value];
end;

function TextureMinFilter: TTextureMinFilter;
begin Result := FTextureMinFilter end;

function TextureMagFilter: TTextureMagFilter;
begin Result := FTextureMagFilter end;

end.
