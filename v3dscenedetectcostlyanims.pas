unit V3DSceneDetectCostlyAnims;

interface

uses VRMLRendererOptimization, KambiTimeUtils, GLWindow;

var
  DetectTimeConsumingAnimations: boolean = true;
  MenuDetectTimeConsumingAnimations: TMenuItemChecked;

  CurrentGeometryChangedTime: TKamTimerResult;
  LastGeometryChangedTime: TKamTimerResult;

implementation

uses V3DSceneConfig;

initialization
  DetectTimeConsumingAnimations :=
    ConfigFile.GetValue('detect_costly_anims', true);
finalization
  ConfigFile.SetDeleteValue('detect_costly_anims',
    DetectTimeConsumingAnimations, true);
end.
