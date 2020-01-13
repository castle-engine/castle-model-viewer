unit V3DSceneShadows;

{$I v3dsceneconf.inc}

interface

uses CastleWindow, CastleScene, CastleTransform, CastleVectors, CastleViewport,
  CastleRenderer;

type
  { Takes care of setting shadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all view3dscene
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsViewport = class(TCastleViewport)
  protected
    procedure Render3D(const Params: TRenderParams); override;
  end;

var
  ShadowVolumes: boolean = true;
  ShadowVolumesRender: boolean = false;

procedure ViewportShadowsProperties(Viewport: TCastleViewport);

implementation

uses SysUtils, CastleConfig, CastleGLUtils, V3DSceneFillMode;

procedure ViewportShadowsProperties(Viewport: TCastleViewport);
begin
  Viewport.ShadowVolumes := ShadowVolumes;
  Viewport.ShadowVolumesRender := ShadowVolumesRender;
end;

procedure TV3DShadowsViewport.Render3D(const Params: TRenderParams);

  procedure Render3DShadowsBegin(Scene: TCastleScene);
  begin
    { Thanks to using SolidShadowColor, shadow is visible
      even when rmSolidColor is used }
    Scene.Attributes.SolidColor := SolidShadowColor;
  end;

  procedure Render3DNoShadowsBegin(Scene: TCastleScene);
  begin
    Scene.Attributes.SolidColor := SolidColor;
  end;

begin
  if Params.InShadow then
  begin
    Render3DShadowsBegin(Items.MainScene);
    inherited;
  end else
  begin
    Render3DNoShadowsBegin(Items.MainScene);
    inherited;
  end;
end;

end.
