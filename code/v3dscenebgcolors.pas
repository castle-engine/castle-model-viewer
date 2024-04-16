{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Changing BackgroundColor of all viewports. }
unit V3DSceneBGColors;

interface

uses CastleColors, CastleViewport, V3DSceneViewports;

var
  BGColor: TCastleColor;

{ Call always after changing BGColor, call also once at the beginning
  after ExtraViewports and MainViewport is ready. }
procedure BGColorChanged;

implementation

uses CastleConfig;

procedure BGColorChanged;
var
  I: Integer;
begin
  for I := 0 to High(ExtraViewports) do
    ExtraViewports[I].BackgroundColor := BGColor;
  MainViewport.BackgroundColor := BGColor;
end;

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  BGColor := Config.GetVector4(
    'video_options/default_background_color', TCastleViewport.DefaultBackgroundColor);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteVector4('video_options/default_background_color',
    BGColor, TCastleViewport.DefaultBackgroundColor);
end;

initialization
  UserConfig.AddLoadListener(@TConfigOptions(nil).LoadFromConfig);
  UserConfig.AddSaveListener(@TConfigOptions(nil).SaveToConfig);
end.
