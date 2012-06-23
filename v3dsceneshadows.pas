unit V3DSceneShadows;

interface

uses CastleWindow, CastleScene, Base3D, VectorMath, CastleSceneManager, GLRenderer;

type
  { Takes care of settingshadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all view3dscene
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsSceneManager = class(TCastleSceneManager)
  protected
    procedure Render3D(const Params: TRenderParams); override;
  end;

  TV3DShadowsViewport = class(TCastleViewport)
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

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);

implementation

uses SysUtils, CastleConfig, GL, CastleGLUtils, V3DSceneFillMode;

procedure ShadowsGLOpen;
begin
  MenuShadowVolumes.Enabled := ShadowsPossibleCurrently;
end;

procedure ShadowsGLClose;
begin
end;

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);
begin
  Viewport.ShadowVolumesPossible := ShadowsPossibleCurrently;
  Viewport.ShadowVolumes := ShadowsOn;
  Viewport.ShadowVolumesDraw := DrawShadowVolumes;
end;

procedure Render3DShadowsBegin(Scene: TCastleScene);
begin
  { Thanks to using PureGeometryShadowedColor, shadow is visible
    even when rmPureGeometry is used. Note: no need to push current
    glColor value, renderer doesn't preserve state anyway
    and caller is prepared to deal with it. }
  if Scene.Attributes.Mode = rmPureGeometry then
    glColorv(PureGeometryShadowedColor);
end;

procedure Render3DNoShadowsBegin(Scene: TCastleScene);
begin
  if Scene.Attributes.Mode = rmPureGeometry then
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

type
  TConfigOptions = class
    class procedure LoadFromConfig(const Config: TCastleConfig);
    class procedure SaveToConfig(const Config: TCastleConfig);
  end;

class procedure TConfigOptions.LoadFromConfig(const Config: TCastleConfig);
begin
  ShadowsPossibleWanted := Config.GetValue(
    'video_options/shadows_possible_wanted', DefaultShadowsPossibleWanted);
end;

class procedure TConfigOptions.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('video_options/shadows_possible_wanted',
    ShadowsPossibleWanted, DefaultShadowsPossibleWanted);
end;

initialization
  Config.OnLoad.Add(@TConfigOptions(nil).LoadFromConfig);
  Config.OnSave.Add(@TConfigOptions(nil).SaveToConfig);
end.
