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
  Config.AddLoadListener(@TConfigOptions(nil).LoadFromConfig);
  Config.AddSaveListener(@TConfigOptions(nil).SaveToConfig);
end.
