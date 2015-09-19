unit V3DSceneShadows;

{$I v3dsceneconf.inc}

interface

uses CastleWindow, CastleScene, Castle3D, CastleVectors, CastleSceneManager,
  CastleRenderer;

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
  ShadowVolumes: boolean = true;
  ShadowVolumesRender: boolean = false;

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);

implementation

uses SysUtils, CastleConfig, CastleGLUtils, V3DSceneFillMode;

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);
begin
  Viewport.ShadowVolumes := ShadowVolumes;
  Viewport.ShadowVolumesRender := ShadowVolumesRender;
end;

procedure Render3DShadowsBegin(Scene: TCastleScene);
begin
  {$ifndef OpenGLES} //TODO-es
  { Thanks to using PureGeometryShadowedColor, shadow is visible
    even when rmPureGeometry is used. Note: no need to push current
    glColor value, renderer doesn't preserve state anyway
    and caller is prepared to deal with it. }
  if Scene.Attributes.Mode = rmPureGeometry then
    glColorv(PureGeometryShadowedColor);
  {$endif}
end;

procedure Render3DNoShadowsBegin(Scene: TCastleScene);
begin
  {$ifndef OpenGLES} //TODO-es
  if Scene.Attributes.Mode = rmPureGeometry then
    glColorv(PureGeometryColor);
  {$endif}
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

end.
