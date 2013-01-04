unit V3DSceneShadows;

interface

uses CastleWindow, CastleScene, Castle3D, CastleVectors, CastleSceneManager, GLRenderer;

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
  ShadowsOn: boolean = true;
  DrawShadowVolumes: boolean = false;

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);

implementation

uses SysUtils, CastleConfig, GL, CastleGLUtils, V3DSceneFillMode;

procedure ViewportShadowsProperties(Viewport: TCastleAbstractViewport);
begin
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

end.
