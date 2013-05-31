unit V3DSceneMiscConfig;

interface

const
  DefaultInitialShowBBox = true;
  DefaultInitialShowStatus = true;

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

uses SysUtils, CastleConfig, CastleWindow, CastleDownload;

function MyGetApplicationName: string;
begin
  Result := 'view3dscene';
end;

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
  Application.LimitFPS := Config.GetFloat('video_options/limit_fps',
    DefaultLimitFPS);
  EnableNetwork := Config.GetValue('network', DefaultEnableNetwork);

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
    Application.LimitFPS, DefaultLimitFPS);
  Config.SetDeleteValue('network', EnableNetwork, DefaultEnableNetwork);
end;

initialization
  { This is needed because
    - I sometimes display ApplicationName for user, and under Windows
      ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unsure for Unixes.
    - ApplicationName is used for Config.URL by ApplicationConfig. }
  OnGetApplicationName := @MyGetApplicationName;

  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
