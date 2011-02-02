unit V3DSceneShadows;

interface

uses GLWindow, VRMLGLScene, VectorMath, KambiSceneManager;

type
  { Takes care of settingshadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all view3dscene
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsSceneManager = class(TKamSceneManager)
  protected
    procedure Render3D(TransparentGroup: TTransparentGroup; InShadow: boolean); override;
  end;

  TV3DShadowsViewport = class(TKamViewport)
  protected
    procedure Render3D(TransparentGroup: TTransparentGroup; InShadow: boolean); override;
  end;

var
  MenuShadowsMenu: TMenu;

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

uses SysUtils, V3DSceneConfig, GL, KambiGLUtils, V3DSceneFillMode;

procedure ShadowsGLOpen;
begin
  MenuShadowsMenu.Enabled := ShadowsPossibleCurrently;
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

var
  SavedBMColor: TVector4Single;

procedure Render3DBegin(Scene: TVRMLGLScene);
begin
  { Thanks to using PureGeometryShadowedColor, shadow is visible
    even when Attributes.PureGeometry. Note: no need to push current
    glColor value, VRML renderer doesn't preserve state anyway
    and caller is prepared to deal with it. }
  if Scene.Attributes.PureGeometry then
    glColorv(PureGeometryShadowedColor);

  { Thanks to changing BumpMappingLightDiffuseColor, shadow is visible
    even when bump mapping is at work. }
  SavedBMColor := Scene.BumpMappingLightDiffuseColor;
  Scene.BumpMappingLightDiffuseColor := Black4Single;
end;

procedure Render3DEnd(Scene: TVRMLGLScene);
begin
  Scene.BumpMappingLightDiffuseColor := SavedBMColor;
end;

procedure TV3DShadowsSceneManager.Render3D(
  TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  if InShadow then
  begin
    Render3DBegin(MainScene);
    inherited;
    Render3DEnd(MainScene);
  end else
    inherited;
end;

procedure TV3DShadowsViewport.Render3D(
  TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  if InShadow then
  begin
    Render3DBegin(GetMainScene);
    inherited;
    Render3DEnd(GetMainScene);
  end else
    inherited;
end;

initialization
  ShadowsPossibleWanted := ConfigFile.GetValue(
    'video_options/shadows_possible_wanted', DefaultShadowsPossibleWanted);
finalization
  ConfigFile.SetDeleteValue('video_options/shadows_possible_wanted',
    ShadowsPossibleWanted, DefaultShadowsPossibleWanted);
end.
