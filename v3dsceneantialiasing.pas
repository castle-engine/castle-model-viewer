unit V3DSceneAntiAliasing;

interface

uses GLAntiAliasing, GLWindow;

var
  AntiAliasingMenu: array [TAntiAliasingRange] of TMenuItemRadio;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);

implementation

uses V3DSceneConfig;

procedure MenuAppendAntiAliasing(M: TMenu; BaseIntData: Cardinal);
var
  RadioGroup: TMenuItemRadioGroup;
  AA: TAntiAliasingRange;
begin
  RadioGroup := nil;

  for AA := Low(AA) to High(AA) do
  begin
    AntiAliasingMenu[AA] := TMenuItemRadio.Create(
      SQuoteMenuEntryCaption(AntiAliasingToStr(AA)),
      BaseIntData + AA, AA = AntiAliasing, true);
    if RadioGroup = nil then
      RadioGroup := AntiAliasingMenu[AA].Group else
      AntiAliasingMenu[AA].Group := RadioGroup;
    M.Append(AntiAliasingMenu[AA]);
  end;
end;

initialization
  AntiAliasing := ConfigFile.GetValue(
    'video_options/anti_aliasing', DefaultAntiAliasing);
finalization
  ConfigFile.SetDeleteValue('video_options/anti_aliasing',
    AntiAliasing, DefaultAntiAliasing);
end.
