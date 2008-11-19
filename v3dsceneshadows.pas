unit V3DSceneShadows;

interface

uses ShadowVolumes, GLWindow, VRMLGLScene, VectorMath;

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
procedure ShadowsRender(Scene: TVRMLGLscene;
  const Frustum: TFrustum; const MainLightPosition: TVector4Single);

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

type
  TRenderer = class
    Scene: TVRMLGLScene;
    Frustum: PFrustum;
    procedure RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
    procedure RenderShadowVolumes;
  end;

procedure TRenderer.RenderScene(InShadow: boolean; TransparentGroup: TTransparentGroup);
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

    Scene.RenderFrustum(Frustum^, TransparentGroup, @Scene.LightRenderInShadow);

    Scene.BumpMappingLightDiffuseColor := OldColor;

    if Scene.Attributes.PureGeometry then
      glPopAttrib;
  end else
    Scene.RenderFrustum(Frustum^, TransparentGroup, nil);
end;

procedure TRenderer.RenderShadowVolumes;
begin
  Scene.InitAndRenderShadowVolume(SV, true, IdentityMatrix4Single);
end;

procedure ShadowsRender(Scene: TVRMLGLscene;
  const Frustum: TFrustum; const MainLightPosition: TVector4Single);
var
  R: TRenderer;
begin
  R := TRenderer.Create;
  try
    R.Scene := Scene;
    R.Frustum := @Frustum;

    SV.InitFrustumAndLight(Frustum, MainLightPosition);
    SV.Render(
      nil,
      @R.RenderScene,
      @R.RenderShadowVolumes,
      DrawShadowVolumes);
  finally FreeAndNil(R) end;
end;

initialization
  ShadowsPossibleWanted := ConfigFile.GetValue(
    'video_options/shadows_possible_wanted', DefaultShadowsPossibleWanted);
finalization
  ConfigFile.SetDeleteValue('video_options/shadows_possible_wanted',
    ShadowsPossibleWanted, DefaultShadowsPossibleWanted);
end.
