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


{ Simple management of TexMin/MagFilter properties of VRML scene/animation.
  Initialize by InitTextureFilters,
  change by SetTextureMin/MagFilter,
  description of currently set filters is in TextureMin/MagFilter. }

unit V3DSceneTextureFilters;

{$I v3dsceneconf.inc}

interface

uses CastleUtils, CastleVectors, X3DNodes,
  CastleScene, CastleGLUtils, CastleGLImages, CastleWindow;

const
  MinificationFilterNames: array [TMinificationFilter] of string =
  ( 'Nearest (Fastest)',
    'Linear',
    'Nearest Pixel and Nearest Mipmap',
    'Nearest Pixel and Linear Mipmap',
    'Linear Pixel and Nearest Mipmap',
    'Linear Pixel and Linear Mimap (Trilinear; Best Quality)');

  MagnificationFilterNames: array [TMagnificationFilter] of string =
  ( 'Nearest (Fastest)',
    'Linear (Best Quality)');

procedure InitTextureFilters(Scene: TCastleScene);

function MinificationFilter: TMinificationFilter;
procedure SetMinificationFilter(Value: TMinificationFilter; Scene: TCastleScene);

function MagnificationFilter: TMagnificationFilter;
procedure SetMagnificationFilter(Value: TMagnificationFilter; Scene: TCastleScene);

procedure MenuAppendMinificationFilters(M: TMenu; BaseIntData: Cardinal);
procedure MenuAppendMagnificationFilters(M: TMenu; BaseIntData: Cardinal);

implementation

var
  FMinificationFilter: TMinificationFilter;
  FMagnificationFilter: TMagnificationFilter;

procedure InitTextureFilters(Scene: TCastleScene);
begin
  SetMinificationFilter(minLinearMipmapLinear, Scene);
  SetMagnificationFilter(magLinear, Scene);
end;

function MinificationFilter: TMinificationFilter;
begin Result := FMinificationFilter end;

procedure SetMinificationFilter(Value: TMinificationFilter; Scene: TCastleScene);
begin
  FMinificationFilter := value;
  Scene.Attributes.MinificationFilter := Value;
end;

function MagnificationFilter: TMagnificationFilter;
begin Result := FMagnificationFilter end;

procedure SetMagnificationFilter(Value: TMagnificationFilter; Scene: TCastleScene);
begin
  FMagnificationFilter := value;
  Scene.Attributes.MagnificationFilter := Value;
end;

procedure MenuAppendMinificationFilters(M: TMenu; BaseIntData: Cardinal);
begin
  M.AppendRadioGroup(MinificationFilterNames, BaseIntData, Ord(MinificationFilter), true);
end;

procedure MenuAppendMagnificationFilters(M: TMenu;
  BaseIntData: Cardinal);
begin
  M.AppendRadioGroup(MagnificationFilterNames, BaseIntData, Ord(MagnificationFilter), true);
end;

end.
