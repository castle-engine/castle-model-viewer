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

uses V3DSceneConfig;

initialization
  InitialShowBBox := ConfigFile.GetValue(
    'video_options/initial_show_bbox', DefaultInitialShowBBox);
  InitialShowStatus := ConfigFile.GetValue(
    'video_options/initial_show_status', DefaultInitialShowStatus);

  ShowBBox := InitialShowBBox;
  ShowStatus := InitialShowStatus;
finalization
  if ConfigFile <> nil then
  begin
    ConfigFile.SetDeleteValue('video_options/initial_show_bbox',
      InitialShowBBox  , DefaultInitialShowBBox);
    ConfigFile.SetDeleteValue('video_options/initial_show_status',
      InitialShowStatus, DefaultInitialShowStatus);
  end;
end.
