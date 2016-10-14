{
  Copyright 2002-2016 Michalis Kamburelis.

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

{ Changing BackgroundColor of all viewports. }
unit V3DSceneBGColors;

interface

uses CastleColors, CastleSceneManager, V3DSceneViewports;

const
  DefaultBGColor: TCastleColor = (0, 0, 0, 1);

var
  BGColor: TCastleColor;

  { Should be used by viewports, Background method should return nil
    if this is > 0. }
  DisableBackground: Cardinal = 0;

{ Call always after changing BGColor, call also once at the beginning
  after Viewports and SceneManager is ready. }
procedure BGColorChanged;

procedure BackgroundTransparent;
procedure BackgroundOpaque;

implementation

uses CastleConfig;

procedure BGColorChanged;
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    Viewports[I].BackgroundColor := BGColor;
  SceneManager.BackgroundColor := BGColor;
end;

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  BGColor := Config.GetVector4(
    'video_options/default_background_color', DefaultBGColor);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteVector4('video_options/default_background_color',
    BGColor, DefaultBGColor);
end;

procedure BackgroundTransparent;
begin
  Inc(DisableBackground);
  BGColor[3] := 0;
  BGColorChanged;
end;

procedure BackgroundOpaque;
begin
  Dec(DisableBackground);
  BGColor[3] := 1;
  BGColorChanged;
end;

initialization
  UserConfig.AddLoadListener(@TConfigOptions(nil).LoadFromConfig);
  UserConfig.AddSaveListener(@TConfigOptions(nil).SaveToConfig);
end.
