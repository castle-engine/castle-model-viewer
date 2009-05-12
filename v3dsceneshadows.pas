unit V3DSceneShadows;

interface

uses ShadowVolumes, GLWindow, VRMLGLScene, VectorMath, Frustum,
  SceneManagerUnit;

type
  { TSceneManager descendant that takes care of setting
    TSceneManager shadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all view3dscene
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsSceneManager = class(TSceneManager)
  protected
    procedure InitShadowsProperties;
    procedure RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup); override;
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
    (and set projection to infinity and initialized ShadowVolumes instance).
    This can be true only if ShadowsPossibleWanted was initially true. }
  ShadowsPossibleCurrently: boolean = false;

  ShadowsOn: boolean = true;
  DrawShadowVolumes: boolean = false;

procedure ShadowsGLInit;
procedure ShadowsGLClose;

implementation

uses SysUtils, V3DSceneConfig, GL, KambiGLUtils, V3DSceneFillMode;

var
  SV: TShadowVolumes;

procedure ShadowsGLInit;
begin
  if ShadowsPossibleCurrently then
  begin
    SV := TShadowVolumes.Create;
    SV.InitGLContext;
  end;
  MenuShadowsMenu.Enabled := ShadowsPossibleCurrently;
end;

procedure ShadowsGLClose;
begin
  FreeAndNil(SV);
end;

procedure TV3DShadowsSceneManager.InitShadowsProperties;
begin
  ShadowVolumesPossible := ShadowsPossibleCurrently;
  ShadowVolumes := ShadowsOn;
  SV := V3DSceneShadows.SV;
  ShadowVolumesDraw := DrawShadowVolumes;
end;

procedure TV3DShadowsSceneManager.RenderScene(
  InShadow: boolean; TransparentGroup: TTransparentGroup);
var
  OldColor: TVector4Single;
begin
  if InShadow then
  begin
    { Thanks to using PureGeometryShadowedColor, shadow is visible
      even when Attributes.PureGeometry. }
    if Scene.Attributes.PureGeometry then
    begin
      glPushAttrib(GL_CURRENT_BIT);
      glColorv(PureGeometryShadowedColor);
    end;

    { Thanks to changing BumpMappingLightDiffuseColor, shadow is visible
      even when bump mapping is at work. }
    OldColor := Scene.BumpMappingLightDiffuseColor;
    Scene.BumpMappingLightDiffuseColor := Black4Single;

    inherited;

    Scene.BumpMappingLightDiffuseColor := OldColor;

    if Scene.Attributes.PureGeometry then
      glPopAttrib;
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
