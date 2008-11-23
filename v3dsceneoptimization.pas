unit V3DSceneOptimization;

interface

uses VRMLRendererOptimization, GLWindow;

var
  { When setting this, set also appropriate menu as Checked
    and SceneAnimation.Optimization.

    The natural exceptions, when you don't have to set them,
    is when you know that they are not yet initialized.
    Or, for menu, when given menu item was just clicked, you
    know menu is already updated (AutoCheckedToggle is true). }
  Optimization: TGLRendererOptimization;
  OptimizationMenu: array [TGLRendererOptimization] of TMenuItemRadio;

  OptimizationSaveConfig: boolean = true;

procedure MenuAppendOptimization(M: TMenu; BaseIntData: Cardinal);

implementation

uses V3DSceneConfig;

const
  DefaultOptimization = roSeparateShapes;

procedure MenuAppendOptimization(M: TMenu; BaseIntData: Cardinal);
var
  RadioGroup: TMenuItemRadioGroup;
  O: TGLRendererOptimization;
  S: string;
begin
  RadioGroup := nil;

  for O := Low(O) to High(O) do
  begin
    S := SQuoteMenuEntryCaption(RendererOptimizationNiceNames[O]);
    if O = DefaultOptimization then
      S += ' (Default)';
    OptimizationMenu[O] := TMenuItemRadio.Create(S,
      BaseIntData + Cardinal(Ord(O)), O = Optimization, true);
    if RadioGroup = nil then
      RadioGroup := OptimizationMenu[O].Group else
      OptimizationMenu[O].Group := RadioGroup;
    M.Append(OptimizationMenu[O]);
  end;
end;

initialization
  Optimization := TGLRendererOptimization(
    ConfigFile.GetValue('scene_optimization', Ord(DefaultOptimization)));
finalization
  if OptimizationSaveConfig then
    ConfigFile.SetDeleteValue('scene_optimization',
      Ord(Optimization), Ord(DefaultOptimization));
end.
