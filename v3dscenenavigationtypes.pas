{
  Copyright 2003-2010 Michalis Kamburelis.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Manage camera navigation types. }
unit V3DSceneNavigationTypes;

interface

uses SysUtils, KambiUtils, GLWindow, Cameras, Boxes3D, VectorMath,
  GL, GLU, KambiGLUtils, KambiSceneManager, Classes, UIControls, KambiTimeUtils,
  GLControls;

{ Call this once on created SceneManager.
  This will take care of using proper SceneManager.Camera. }
procedure InitCameras(SceneManager: TKamSceneManager);

const
  CameraNames: array [TCameraNavigationType] of string =
  ('Examine', 'Walk', 'Fly', 'None');

var
  CameraRadios: array [TCameraNavigationType] of TMenuItemRadio;
  CameraButtons: array [TCameraNavigationType] of TKamGLButton;

procedure UpdateCameraNavigationTypeUI;

{ Interpret and remove from ParStr(1) ... ParStr(ParCount)
  some params specific for this unit.
  Those params are documented in CamerasOptionsHelp.

  Call this @italic(before) InitCameras. }
procedure CamerasParseParameters;

const
  CamerasOptionsHelp =
  '  --navigation Examine|Walk'+nl+
  '                        Set initial navigation style';

var
  { When loading scene, check InitialNavigationType, and if non-empty:
    use it, and reset to empty. }
  InitialNavigationType: string;

var
  { Same as your SceneManager.Camera after InitCameras. }
  Camera: TUniversalCamera;

type
  TNavigationTypeButton = class(TKamGLButton)
  public
    NavigationType: TCameraNavigationType;
    constructor Create(AOwner: TComponent;
      const ANavigationType: TCameraNavigationType); reintroduce;
    function TooltipStyle: TUIControlDrawStyle; override;
    procedure DrawTooltip; override;
    procedure GLContextInit; override;
    procedure GLContextClose; override;
  end;

implementation

uses ParseParametersUnit, KambiClassUtils, Images, GLImages, V3DSceneImages;

var
  ImageExamine_TooltipDL: TGLuint;
  ImageWalk_Fly_TooltipDL: TGLuint;

procedure UpdateCameraNavigationTypeUI;
var
  NT: TCameraNavigationType;
begin
  if CameraRadios[Camera.NavigationType] <> nil then
    CameraRadios[Camera.NavigationType].Checked := true;
  for NT := Low(NT) to High(NT) do
    { check with <> nil, since for ntNone we don't show button now }
    if CameraButtons[NT] <> nil then
      CameraButtons[NT].Pressed := NT = Camera.NavigationType;
end;

procedure InitCameras(SceneManager: TKamSceneManager);
begin
  { init SceneManager.Camera }
  SceneManager.Camera := Camera;
  UpdateCameraNavigationTypeUI;
end;

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    Assert(OptionNum = 0);
    InitialNavigationType := Argument;
  end;

procedure CamerasParseParameters;
const
  Options: array[0..0]of TOption =
  ((Short:#0; Long:'navigation'; Argument: oaRequired));
begin
  ParseParameters(Options, @OptionProc, nil, true);
end;

{ TNavigationTypeButton ------------------------------------------------------ }

constructor TNavigationTypeButton.Create(AOwner: TComponent;
  const ANavigationType: TCameraNavigationType);
begin
  inherited Create(AOwner);
  NavigationType := ANavigationType;
end;

function TNavigationTypeButton.TooltipStyle: TUIControlDrawStyle;
begin
  if NavigationType in [ntExamine, ntWalk, ntFly] then
    Result := ds2D else
    Result := dsNone;
end;

{ By using image instead of drawing the text we avoid some lacks
  of our text output:
  - it would be unhandy to print both normal and bold fonts
  - it would be unhandy to use non-monospace fonts and still
    make columns (key names) matching
  - GIMP makes fonts antialiased, our bitmap fonts are not antialiased.
    Besides making things look better, this allows us to use smaller fonts,
    which is important (we have a lot of text that we have to fit within
    window).
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
{$define IMAGE_TOOLTIPS}
{$ifdef IMAGE_TOOLTIPS}

procedure TNavigationTypeButton.DrawTooltip;

  procedure DoDraw(Image: TImage; ImageDrawDisplayList: TGLuint);
  const
    WindowBorderMargin = 8;
    ButtonBottomMargin = 16;
    ImageMargin = 8;
    ArrowSize = ButtonBottomMargin + 8;
  var
    X1, Y1, X2, Y2, ArrowMiddleX: Integer;
    Arrow: array [0..2] of TVector2Integer;
  begin
    X1 := WindowBorderMargin;
    X2 := X1 + 2 * ImageMargin + Image.Width;
    Y2 := Bottom - ButtonBottomMargin;
    Y1 := Y2 - 2 * ImageMargin - Image.Height;

    DrawGLBorderedRectangle(X1, Y1, X2, Y2,
      Vector4Single(TooltipInsideColor, 255),
      Vector4Single(TooltipBorderColor, 255));

    SetWindowPos(X1 + ImageMargin, Y1 + ImageMargin);
    glCallList(ImageDrawDisplayList);

    ArrowMiddleX := Left + Width div 2;
    Arrow[0][0] := ArrowMiddleX - ArrowSize;
    Arrow[0][1] := Y2;
    Arrow[1][0] := ArrowMiddleX + ArrowSize;
    Arrow[1][1] := Y2;
    Arrow[2][0] := ArrowMiddleX;
    Arrow[2][1] := Y2 + ArrowSize;

    glColorv(TooltipInsideColor);
    glBegin(GL_TRIANGLES);
      glVertexv(Arrow[0]);
      glVertexv(Arrow[1]);
      glVertexv(Arrow[2]);
    glEnd;

    glColorv(TooltipBorderColor);
    glBegin(GL_LINE_STRIP);
      glVertexv(Arrow[0]);
      glVertexv(Arrow[2]);
      glVertexv(Arrow[1]);
    glEnd;
  end;

begin
  if NavigationType = ntExamine then
    DoDraw(Examine_Tooltip, ImageExamine_TooltipDL) else
    DoDraw(Walk_Fly_Tooltip, ImageWalk_Fly_TooltipDL);
end;

{$else}

procedure TNavigationTypeButton.DrawTooltip;

  procedure DoDraw(const Strings: array of string);
  var
    StringList: TStringList;
  begin
    StringList := TStringList.Create;
    try
      AddStrArrayToStrings(Strings, StringList);

      glTranslatef(Left, Bottom - StringList.Count * Font.RowHeight, 0);
      Font.PrintStringsBox(StringList, 0,
        Vector4Single(TooltipInsideColor, 255),
        Vector4Single(TooltipBorderColor, 255),
        Vector4Single(TooltipTextColor,  255), nil, 5, 1, 1);
    finally FreeAndNil(StringList) end;
  end;

const
  ExamineTooltip: array [0..14] of string = (
    'Examine navigation controls:',
    '',
    'Mouse:',
    '',
    'Rotate         Left mouse dragging',
    'Move           Middle mouse dragging (or Left mouse + Shift)',
    'Zoom           Right mouse dragging (or Left mouse + Ctrl)',
    '',
    'Keys:',
    '',
    'Rotate                           Arrows / PageUp / PageDown',
    'Stop rotating                    Space',
    'Move                             Ctrl + Arrows / PageUp / PageDown',
    'Scale                            + / -',
    'Restore default transformation   Home'
  );

  WalkFlyTooltip: array [0..25] of string = (
    'Walk / Fly navigation controls:',
    '',
    'Basic:',
    '',
    'Forward / backward                        Up / Down',
    'Rotate                                    Left / Right',
    'Raise / bow your head                     PageUp / PageDown',
    'Restore head raise to initial position    Home',
    '  (neutralize any effect of PageUp / PageDown)',
    'Fly up / down                             Insert / Delete',
    'Move left / right                         Comma / Period',
    'Jump / crouch                             A / Z',
    '  (only when Gravity works, in Walk mode)',
    '',
    'Turn "Mouse Look" "On" to comfortably look around by moving the mouse.',
    'Then the keys for strafe and rotations swap their meaning:',
    '  * Left / Right keys move left / right',
    '  * Comma / Period rotate',
    '',
    'Additional controls:',
    '',
    'Increase / decrease moving speed               + / -',
    'Increase / decrease avatar height              Ctrl + Insert/Delete',
    'Rotate slower                                  Ctrl + Left / Right',
    'Raise / bow your head slower                   Ctrl + PageUp / PageDown',
    'Pick a point, selecting triangle and object    Right mouse click'
  );

begin
  if NavigationType = ntExamine then
    DoDraw(ExamineTooltip) else
    DoDraw(WalkFlyTooltip);
end;

{$endif}

procedure TNavigationTypeButton.GLContextInit;
begin
  inherited;

  { Just use GLContextInit/Close for ntExamine to initialize global unit
    variables . }
  if NavigationType = ntExamine then
  begin
    if ImageExamine_TooltipDL = 0 then
      ImageExamine_TooltipDL := ImageDrawToDisplayList(Examine_Tooltip);
    if ImageWalk_Fly_TooltipDL = 0 then
      ImageWalk_Fly_TooltipDL := ImageDrawToDisplayList(Walk_Fly_Tooltip);
  end;
end;

procedure TNavigationTypeButton.GLContextClose;
begin
  if NavigationType = ntExamine then
  begin
    glFreeDisplayList(ImageExamine_TooltipDL);
    glFreeDisplayList(ImageWalk_Fly_TooltipDL);
  end;
  inherited;
end;

initialization
  Camera := TUniversalCamera.Create(nil);
finalization
  FreeAndNil(Camera);
end.
