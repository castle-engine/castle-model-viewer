unit V3DSceneAntiAliasing;

interface

uses GLAntiAliasing, CastleWindow;

var
  AntiAliasingMenu: array [TAntiAliasing] of TMenuItemRadio;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);

implementation

uses V3DSceneConfig;

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

initialization
  AntiAliasing := TAntiAliasing(ConfigFile.GetValue(
    'video_options/anti_aliasing', Ord(DefaultAntiAliasing)));
finalization
  if ConfigFile <> nil then
    ConfigFile.SetDeleteValue('video_options/anti_aliasing',
      Ord(AntiAliasing), Ord(DefaultAntiAliasing));
end.
