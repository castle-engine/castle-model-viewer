unit V3DSceneMiscConfig;

interface

const
  DefaultInitialShowBBox = true;
  DefaultInitialShowStatus = true;
  DefaultEnableBlockingDownloads = false;

var
  { Initial* are initialized at unit initialization from config file.
    Their changes will be recorded back to config file at finalization. }
  InitialShowBBox: boolean = DefaultInitialShowBBox;
  InitialShowStatus: boolean = DefaultInitialShowStatus;

  { Current values are initialized at unit initialization from Initial*.
    Their changes will not be recorded back. }
  ShowStatus: boolean;
  ShowBBox: boolean;

implementation

uses SysUtils,
  CastleConfig, CastleWindow, CastleDownload, CastleApplicationProperties;

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  InitialShowBBox := Config.GetValue(
    'video_options/initial_show_bbox', DefaultInitialShowBBox);
  InitialShowStatus := Config.GetValue(
    'video_options/initial_show_status', DefaultInitialShowStatus);
  ApplicationProperties.LimitFPS := Config.GetFloat('video_options/limit_fps',
    TCastleApplicationProperties.DefaultLimitFPS);
  EnableBlockingDownloads := Config.GetValue('network', DefaultEnableBlockingDownloads);

  ShowBBox := InitialShowBBox;
  ShowStatus := InitialShowStatus;
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('video_options/initial_show_bbox',
    InitialShowBBox  , DefaultInitialShowBBox);
  Config.SetDeleteValue('video_options/initial_show_status',
    InitialShowStatus, DefaultInitialShowStatus);
  Config.SetDeleteFloat('video_options/limit_fps',
    ApplicationProperties.LimitFPS, TCastleApplicationProperties.DefaultLimitFPS);
  Config.SetDeleteValue('network', EnableBlockingDownloads, DefaultEnableBlockingDownloads);
end;

initialization
  UserConfig.AddLoadListener({$ifdef FPC}@{$endif} TConfigOptions {$ifdef FPC}(nil){$endif}.LoadFromConfig);
  UserConfig.AddSaveListener({$ifdef FPC}@{$endif} TConfigOptions {$ifdef FPC}(nil){$endif}.SaveToConfig);
end.
