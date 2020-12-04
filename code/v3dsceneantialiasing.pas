{
  Copyright 2018-2020 Michalis Kamburelis.

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

{ Anti-aliasing UI. }
unit V3DSceneAntiAliasing;

interface

uses CastleWindow;

var
  AntiAliasingMenu: array [TAntiAliasing] of TMenuItemRadio;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);

implementation

uses CastleConfig, V3DSceneWindow;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);
var
  RadioGroup: TMenuItemRadioGroup;
  AA: TAntiAliasing;
begin
  RadioGroup := nil;

  for AA := Low(AA) to High(AA) do
  begin
    AntiAliasingMenu[AA] := TMenuItemRadio.Create(
      SQuoteMenuEntryCaption(AntiAliasingNames[AA]),
      BaseIntData + Ord(AA), AA = Window.AntiAliasing, true);
    if RadioGroup = nil then
      RadioGroup := AntiAliasingMenu[AA].Group else
      AntiAliasingMenu[AA].Group := RadioGroup;
    M.Append(AntiAliasingMenu[AA]);
  end;
end;

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  Window.AntiAliasing := TAntiAliasing(Config.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('video_options/anti_aliasing',
    Ord(Window.AntiAliasing), Ord(DefaultAntiAliasing));
end;

initialization
  UserConfig.AddLoadListener(@TConfigOptions(nil).LoadFromConfig);
  UserConfig.AddSaveListener(@TConfigOptions(nil).SaveToConfig);
end.
