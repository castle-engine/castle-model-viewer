{
  Copyright 2003-2023 Michalis Kamburelis.

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

{$I v3dsceneconf.inc}

interface

uses SysUtils, CastleUtils, CastleWindow, CastleCameras, CastleVectors,
  CastleGLUtils, CastleViewport, Classes, CastleUIControls,
  CastleControls, CastleInternalControlsImages, CastleGLImages;

{ Compile also with CGE branches that don't yet have new-cameras work merged.
  Once new-cameras merged -> master, we can remove this. }
{$if not declared(TCastleAutoNavigationViewport)}
  {$define TCastleAutoNavigationViewport:=TCastleViewport}
{$endif}

{ Call this once on created Viewport.
  This will take care of using proper Viewport.Navigation. }
procedure InitNavigation(const Viewport: TCastleAutoNavigationViewport);

type
  { Navigation types useful in view3dscene, in order suitable for view3dscene
    menu and toolbar.
    Note that "Walk" is after "Fly", which is safer because "Walk" automatically
    activates gravity. }
  TUserNavigationType = (
    untExamine,
    untFly,
    untWalk,
    unt2D,
    untNone
  );

const
  NavigationNames: array [TUserNavigationType] of string =
  ('Examine', 'Fly', 'Walk', '2D', 'None');

var
  CameraRadios: array [TUserNavigationType] of TMenuItemRadio;
  NavigationButtons: array [TUserNavigationType] of TCastleButton;

function NavigationType: TUserNavigationType;

{ Make UI reflect the current state of Viewport.NavigationType. }
procedure UpdateCameraNavigationTypeUI;

type
  TNavigationTypeButton = class(TCastleButton)
  strict private
    ImageTooltipArrow, ImageTooltip: TCastleImagePersistent;
  public
    NavigationType: TUserNavigationType;
    constructor Create(AOwner: TComponent;
      const ANavigationType: TUserNavigationType); reintroduce;
    destructor Destroy; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender(const TooltipPosition: TVector2); override;
  end;

{ Same as Viewport.Navigation, where Viewport was given to InitNavigation. }
function Navigation: TCastleNavigation;

implementation

uses CastleParameters, CastleClassUtils, CastleImages,
  V3DSceneImages, V3DSceneOpaqueImages, CastleRectangles;

var
  { Saved Viewport from InitNavigation. }
  FViewport: TCastleAutoNavigationViewport;

procedure UpdateCameraNavigationTypeUI;
var
  NT: TUserNavigationType;
begin
  if CameraRadios[NavigationType] <> nil then
    CameraRadios[NavigationType].Checked := true;
  for NT := Low(NT) to High(NT) do
    { check <> nil, since for ntNone and not StableNavigationType
      we don't show buttons }
    if NavigationButtons[NT] <> nil then
      NavigationButtons[NT].Pressed := NT = NavigationType;
end;

procedure InitNavigation(const Viewport: TCastleAutoNavigationViewport);
begin
  FViewport := Viewport;
  UpdateCameraNavigationTypeUI;
end;

function NavigationType: TUserNavigationType;
begin
  case FViewport.NavigationType of
    nt2D: Result := unt2D;
    ntExamine, ntTurntable: Result := untExamine;
    ntWalk: Result := untWalk;
    ntFly: Result := untFly;
    ntNone: Result := untNone;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('FViewport.NavigationType?');
    {$endif}
  end;
end;

function Navigation: TCastleNavigation;
begin
  Result := FViewport.Navigation;
end;

{ TNavigationTypeButton ------------------------------------------------------ }

constructor TNavigationTypeButton.Create(AOwner: TComponent;
  const ANavigationType: TUserNavigationType);
begin
  inherited Create(AOwner);
  NavigationType := ANavigationType;

  ImageTooltipArrow := TCastleImagePersistent.Create;
  ImageTooltipArrow.OwnsImage := false;
  ImageTooltipArrow.Image := TooltipArrow;

  ImageTooltip := TCastleImagePersistent.Create;
  ImageTooltip.OwnsImage := false;
  case NavigationType of
    untExamine:
      ImageTooltip.Image := Examine_Tooltip;
    untWalk, untFly:
      ImageTooltip.Image := Walk_Fly_Tooltip;
    else ;
    // unt2D:
      //ImageTooltip.Image := Navigation2D_Tooltip; // TODO
  end;
end;

destructor TNavigationTypeButton.Destroy;
begin
  FreeAndNil(ImageTooltip);
  FreeAndNil(ImageTooltipArrow);
  inherited;
end;

function TNavigationTypeButton.TooltipExists: boolean;
begin
  Result := NavigationType in [untExamine, untWalk, untFly];
end;

{ By using image instead of drawing the text we avoid some lacks
  of our text output:
  - it would be unhandy to print both normal and bold fonts
    (note: this limit was later removed with TCastleLabel.Html)
  - it would be unhandy to use non-monospace fonts and still
    make columns (key names) matching
  - Also we can draw a nice circle instead of "*" inside walk_fly list.

  (Note: this limitation does not exist anymore,
  as you can arrange layout in CGE editor.
  Our tooltips could be reimplemented to allow any UI hierarchy.)

  Of course, it also causes some problems. Things are no longer configurable
  at runtime:
  - fonts
  - text contents (e.g. we cannot allow view3dscene keys config,
    although this wasn't planned anyway, as it would make problems
    with KeySensor, StringSensor)
  - we cannot wrap text to window width. We just have to assume
    we'll fit inside.
}

procedure TNavigationTypeButton.TooltipRender(const TooltipPosition: TVector2);
const
  WindowBorderMargin = 8;
  ButtonBottomMargin = 16;
  ImgMargin = 8;
var
  ButtonR, R: TFloatRectangle;
begin
  ButtonR := RenderRect;

  R := FloatRectangle(
    WindowBorderMargin,
    ButtonR.Bottom - ButtonBottomMargin - (ImageTooltip.Height + 2 * ImgMargin),
    ImageTooltip.Width  + 2 * ImgMargin,
    ImageTooltip.Height + 2 * ImgMargin);

  Theme.Draw(R, tiTooltip);
  ImageTooltip.DrawableImage.Draw(R.Left + ImgMargin, R.Bottom + ImgMargin);
  { we decrease R.Top to overdraw the tooltip image border }
  ImageTooltipArrow.DrawableImage.Draw(ButtonR.Left + (EffectiveWidth - ImageTooltipArrow.Width) / 2, R.Top - 1);
end;

// initialization
//   Theme.ImagesPersistent[tiTooltip].Image := TooltipRounded;
//   Theme.ImagesPersistent[tiTooltip].OwnsImage := false;
//   Theme.ImagesPersistent[tiTooltip].ProtectedSides.AllSides := 9;
end.
