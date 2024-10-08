{
  Copyright 2006-2024 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Extra support for shadow volumes. }
unit V3DSceneShadows;

{$I v3dsceneconf.inc}

interface

uses CastleWindow, CastleScene, CastleTransform, CastleVectors, CastleViewport,
  CastleRenderOptions, CastleUtils,
  V3DSceneViewports;

type
  { Takes care of setting shadow volume properties, and modifies a little
    shadow volume rendering to work nicely with all castle-model-viewer
    configurations (bump mapping, fill modes etc.) }
  TV3DShadowsViewport = class(TMyViewport)
  protected
    procedure RenderOnePass(const Params: TRenderParams;
      const PassParams: TRenderOnePassParams); override;
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

procedure TV3DShadowsViewport.RenderOnePass(const Params: TRenderParams;
  const PassParams: TRenderOnePassParams);
begin
  { By tweaking SolidShadowColor, shadow is visible
    even when rmSolidColor is used }
  if PassParams.DisableShadowVolumeCastingLights then
    MainScene.RenderOptions.SolidColor := SolidShadowColor
  else
    MainScene.RenderOptions.SolidColor := SolidColor;
  inherited;
end;

end.
