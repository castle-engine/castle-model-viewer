unit V3DSceneAntiAliasing;

interface

uses GLAntiAliasing, CastleWindow;

var
  AntiAliasingMenu: array [TAntiAliasing] of TMenuItemRadio;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);

implementation

uses CastleConfig;

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
      BaseIntData + Ord(AA), AA = AntiAliasing, true);
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
  AntiAliasing := TAntiAliasing(Config.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('video_options/anti_aliasing',
    Ord(AntiAliasing), Ord(DefaultAntiAliasing));
end;


initialization
  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
