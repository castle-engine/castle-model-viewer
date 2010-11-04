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
  end;

implementation

uses ParseParametersUnit, KambiClassUtils;

{ global stuff --------------------------------------------------------------- }

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

procedure TNavigationTypeButton.DrawTooltip;

  procedure DoDraw(const Strings: array of string);
  const
    InsideCol: TVector4f = (      1, 234/255, 169/255, 0.9);
    BorderCol: TVector4f = (157/255, 133/255, 105/255,   1);
    TextCol  : TVector4f = (0, 0, 0, 1);
  var
    StringList: TStringList;
  begin
    StringList := TStringList.Create;
    try
      AddStrArrayToStrings(Strings, StringList);

      glTranslatef(Left, 0, 0);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      Font.PrintStringsBorderedRectTop(StringList,
        0, InsideCol, BorderCol, TextCol, nil, 5, 1, 1,
        ContainerHeight, Height + 10);
      glDisable(GL_BLEND);
    finally FreeAndNil(StringList) end;
  end;

const
  ExamineToolTip: array [0..14] of string = (
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

  WalkFlyTooltip: array [0..27] of string = (
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
    'Turn "Mouse Look" "On" to comfortably look around by moving the mouse. Then the keys for strafe and rotations swap their meaning:',
    '    * Left / Right keys move left / right',
    '    * Comma / Period rotate',
    '',
    'Additional controls:',
    '',
    'Increase / decrease moving speed               + / -',
    '  (has effect on keys Up / Down, Insert / Delete, Comma / Period)',
    'Increase / decrease avatar height              Ctrl + Insert/Delete',
    '  (preferred camera height above the ground)',
    'Rotate slower                                  Ctrl + Left / Right',
    '  (useful when you want to set up camera very precisely, e.g. to use this camera setting to render a scene image using ray-tracer)',
    'Raise / bow your head slower                   Ctrl + PageUp / PageDown',
    'Pick a point, selecting triangle and object    Right mouse click'
  );

begin
  if NavigationType = ntExamine then
    DoDraw(ExamineToolTip) else
    DoDraw(WalkFlyTooltip);
end;

initialization
  Camera := TUniversalCamera.Create(nil);
finalization
  FreeAndNil(Camera);
end.
