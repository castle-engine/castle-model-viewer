{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "view3dscene".

  "view3dscene" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Manage camera navigation types. }
unit V3DSceneNavigationTypes;

interface

uses SysUtils, CastleUtils, CastleWindow, CastleCameras, CastleVectors,
  CastleGLUtils, CastleSceneManager, Classes, CastleUIControls,
  CastleControls, CastleControlsImages;

{ Call this once on created SceneManager.
  This will take care of using proper SceneManager.Camera. }
procedure InitCameras(SceneManager: TCastleSceneManager);

const
  CameraNames: array [TNavigationType] of string =
  ('Examine', 'Turntable (Work in Progress)', 'Walk', 'Fly', 'None');
  StableNavigationType = [Low(TNavigationType)..High(TNavigationType)]
    -[ntTurntable];

var
  CameraRadios: array [TNavigationType] of TMenuItemRadio;
  CameraButtons: array [TNavigationType] of TCastleButton;

procedure UpdateCameraNavigationTypeUI;

type
  TNavigationTypeButton = class(TCastleButton)
  public
    NavigationType: TNavigationType;
    constructor Create(AOwner: TComponent;
      const ANavigationType: TNavigationType); reintroduce;
    function TooltipExists: boolean; override;
    procedure TooltipRender; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

{ Same as SceneManager.Camera, where SceneManager was given to InitCameras. }
function Camera: TCamera;

implementation

uses CastleParameters, CastleClassUtils, CastleImages, CastleGLImages,
  V3DSceneImages, CastleRectangles;

var
  ImageExamine_TooltipGL: TGLImage;
  ImageWalk_Fly_TooltipGL: TGLImage;
  ImageTooltipArrow: TGLImage;
  { Saved SceneManager from InitCameras. }
  FSceneManager: TCastleSceneManager;

procedure UpdateCameraNavigationTypeUI;
var
  NT: TNavigationType;
begin
  if CameraRadios[FSceneManager.NavigationType] <> nil then
    CameraRadios[FSceneManager.NavigationType].Checked := true;
  for NT := Low(NT) to High(NT) do
    { check <> nil, since for ntNone and not StableNavigationType
      we don't show buttons }
    if CameraButtons[NT] <> nil then
      CameraButtons[NT].Pressed := NT = FSceneManager.NavigationType;
end;

procedure InitCameras(SceneManager: TCastleSceneManager);
begin
  FSceneManager := SceneManager;
  UpdateCameraNavigationTypeUI;
end;

function Camera: TCamera;
begin
  Result := FSceneManager.Camera;
end;

{ TNavigationTypeButton ------------------------------------------------------ }

constructor TNavigationTypeButton.Create(AOwner: TComponent;
  const ANavigationType: TNavigationType);
begin
  inherited Create(AOwner);
  NavigationType := ANavigationType;
end;

function TNavigationTypeButton.TooltipExists: boolean;
begin
  Result := NavigationType in [ntExamine, ntWalk, ntFly];
end;

{ By using image instead of drawing the text we avoid some lacks
  of our text output:
  - it would be unhandy to print both normal and bold fonts
  - it would be unhandy to use non-monospace fonts and still
    make columns (key names) matching
  - Also we can draw a nice circle instead of "*" inside walk_fly list.

  Of course, it also causes some problems. Things are no longer configurable
  at runtime:
  - fonts
  - text contents (e.g. we cannot allow view3dscene keys config,
    although this wasn't planned anyway, as it would make problems
    with KeySensor, StringSensor)
  - we cannot wrap text to window width. We just have to assume
    we'll fit inside.
}

procedure TNavigationTypeButton.TooltipRender;

  procedure DoDraw(GLImage: TGLImage);
  const
    WindowBorderMargin = 8;
    ButtonBottomMargin = 16;
    ImageMargin = 8;
  var
    R: TFloatRectangle;
  begin
    R := FloatRectangle(
      WindowBorderMargin,
      Bottom - ButtonBottomMargin - (GLImage.Height + 2 * ImageMargin),
      GLImage.Width  + 2 * ImageMargin,
      GLImage.Height + 2 * ImageMargin);

    Theme.Draw(R, tiTooltip);
    GLImage.Draw(R.Left + ImageMargin, R.Bottom + ImageMargin);
    { we decrease R.Top to overdraw the tooltip image border }
    ImageTooltipArrow.Draw(Left + (EffectiveWidth - ImageTooltipArrow.Width) / 2, R.Top - 1);
  end;

begin
  if NavigationType = ntExamine then
    DoDraw(ImageExamine_TooltipGL) else
    DoDraw(ImageWalk_Fly_TooltipGL);
end;

procedure TNavigationTypeButton.GLContextOpen;
begin
  inherited;

  { Just use GLContextOpen/Close for ntExamine to initialize global unit
    variables . }
  if NavigationType = ntExamine then
  begin
    if ImageExamine_TooltipGL = nil then
      ImageExamine_TooltipGL := TGLImage.Create(Examine_Tooltip, false, false);
    if ImageWalk_Fly_TooltipGL = nil then
      ImageWalk_Fly_TooltipGL := TGLImage.Create(Walk_Fly_Tooltip, false, false);
    if ImageTooltipArrow = nil then
      ImageTooltipArrow := TGLImage.Create(TooltipArrow, false, false);
  end;
end;

procedure TNavigationTypeButton.GLContextClose;
begin
  if NavigationType = ntExamine then
  begin
    FreeAndNil(ImageExamine_TooltipGL);
    FreeAndNil(ImageWalk_Fly_TooltipGL);
    FreeAndNil(ImageTooltipArrow);
  end;
  inherited;
end;

initialization
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);
end.
