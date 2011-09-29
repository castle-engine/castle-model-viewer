unit V3DSceneShadows;

interface

uses CastleWindow, VRMLGLScene, Base3D, VectorMath, CastleSceneManager;

type
  { Takes care of settingshadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all view3dscene
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsSceneManager = class(TCastleSceneManager)
  protected
    procedure Render3D(const Params: TRenderParams); override;
  end;

  TV3DShadowsViewport = class(TKamViewport)
  protected
    procedure Render3D(const Params: TRenderParams); override;
  end;

var
  MenuShadowVolumes: TMenu;

const
  DefaultShadowsPossibleWanted = true;

var
  { Whether user wants to try next time to initialize
    ShadowsPossibleCurrently = true.
    This can change during runtime, but will be applied only on restart. }
  ShadowsPossibleWanted: boolean = DefaultShadowsPossibleWanted;

  { Whether we managed to initialize OpenGL context with stencil buffer
    (and set projection to infinity and initialized ShadowVolumeRenderer instance).
    This can be true only if ShadowsPossibleWanted was initially true. }
  ShadowsPossibleCurrently: boolean = false;

  ShadowsOn: boolean = true;
  DrawShadowVolumes: boolean = false;

procedure ShadowsGLOpen;
procedure ShadowsGLClose;

procedure ViewportShadowsProperties(Viewport: TKamAbstractViewport);

implementation

uses SysUtils, V3DSceneConfig, GL, CastleGLUtils, V3DSceneFillMode;

procedure ShadowsGLOpen;
begin
  MenuShadowVolumes.Enabled := ShadowsPossibleCurrently;
end;

procedure ShadowsGLClose;
begin
end;

procedure ViewportShadowsProperties(Viewport: TKamAbstractViewport);
begin
  Viewport.ShadowVolumesPossible := ShadowsPossibleCurrently;
  Viewport.ShadowVolumes := ShadowsOn;
  Viewport.ShadowVolumesDraw := DrawShadowVolumes;
end;

procedure Render3DShadowsBegin(Scene: T3DScene);
begin
  { Thanks to using PureGeometryShadowedColor, shadow is visible
    even when Attributes.PureGeometry. Note: no need to push current
    glColor value, VRML renderer doesn't preserve state anyway
    and caller is prepared to deal with it. }
  if Scene.Attributes.PureGeometry then
    glColorv(PureGeometryShadowedColor);
end;

procedure Render3DNoShadowsBegin(Scene: T3DScene);
begin
  if Scene.Attributes.PureGeometry then
    glColorv(PureGeometryColor);
end;

procedure TV3DShadowsSceneManager.Render3D(const Params: TRenderParams);
begin
  if Params.InShadow then
  begin
    Render3DShadowsBegin(MainScene);
    inherited;
  end else
  begin
    Render3DNoShadowsBegin(MainScene);
    inherited;
  end;
end;

procedure TV3DShadowsViewport.Render3D(const Params: TRenderParams);
begin
  if Params.InShadow then
  begin
    Render3DShadowsBegin(GetMainScene);
    inherited;
  end else
  begin
    Render3DNoShadowsBegin(GetMainScene);
    inherited;
  end;
end;

initialization
  ShadowsPossibleWanted := ConfigFile.GetValue(
    'video_options/shadows_possible_wanted', DefaultShadowsPossibleWanted);
finalization
  ConfigFile.SetDeleteValue('video_options/shadows_possible_wanted',
    ShadowsPossibleWanted, DefaultShadowsPossibleWanted);
end.
