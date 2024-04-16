unit V3DSceneShadows;

{$I v3dsceneconf.inc}

interface

uses CastleWindow, CastleScene, CastleTransform, CastleVectors, CastleViewport,
  CastleRenderOptions;

{ Compile also with CGE branches that don't yet have new-cameras work merged.
  Once new-cameras merged -> master, we can remove this. }
{$if not declared(TCastleAutoNavigationViewport)}
  {$define TCastleAutoNavigationViewport:=TCastleViewport}
{$endif}

type
  { Takes care of setting shadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all castle-model-viewer
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsViewport = class(TCastleAutoNavigationViewport)
  protected
    procedure RenderOnePass(const Params: TRenderParams); override;
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

procedure TV3DShadowsViewport.RenderOnePass(const Params: TRenderParams);

  procedure RenderOnePassShadowsBegin(Scene: TCastleScene);
  begin
    { Thanks to using SolidShadowColor, shadow is visible
      even when rmSolidColor is used }
    Scene.RenderOptions.SolidColor := SolidShadowColor;
  end;

  procedure RenderOnePassNoShadowsBegin(Scene: TCastleScene);
  begin
    Scene.RenderOptions.SolidColor := SolidColor;
  end;

begin
  if Params.InShadow then
  begin
    RenderOnePassShadowsBegin(Items.MainScene);
    inherited;
  end else
  begin
    RenderOnePassNoShadowsBegin(Items.MainScene);
    inherited;
  end;
end;

end.
