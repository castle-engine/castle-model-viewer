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

{ Keeping all camera types (two for now: Examine and Walk),
  making them switchable at runtime.

  SceneManager parameter for some procedures here
  is used only to set SceneManager.Camera to current Camera
  (this also causes ViewChangedSuddenly now, which is good).
  You can pass @nil if SceneManager is not initialized yet. }
unit V3DSceneAllCameras;

interface

uses SysUtils, KambiUtils, GLWindow, Cameras, Boxes3D, VectorMath,
  GL, GLU, KambiGLUtils, KambiSceneManager, Classes, UIControls, KambiTimeUtils;

{ Call this ONCE on created SceneManager.
  This will take care of always providing proper SceneManager.Camera for you.

  You CAN NOT directly modify Cameras' properties
  (settings like InitialCameraXxx, Rotation, but also general settings
  like OnMatrixchanged). You can do it only indirectly using this unit. }
procedure InitCameras(SceneManager: TKamSceneManager);

{ Call this always when scene changes. Give new BoundingBox and
  InitialCameraXxx and GravityUp settings for this new scene.
  This will call Init() functions for all cameras (that are ready
  for various values of CameraMode).

  You must call InitCameras before using this. }
procedure SceneInitCameras(
  const ModelBox: TBox3D;
  const InitialPosition, InitialDirection, InitialUp, GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);

const
  CameraNames: array[TCameraNavigationType]of string =
   ('Examine', 'Walk');

var
  CameraRadios: array [TCameraNavigationType] of TMenuItemRadio;

{ Change current CameraMode.

  If KeepView, than we keep the current camera view the same.

  Set/ChangeCameraMode call VisibleChange on new camera.
  That's because changing Camera in fact changed
  Camera.Matrix, so we must do the same thing that would be done in
  Camera.VisibleChange.

  @groupBegin }
procedure SetCameraMode(SceneManager: TKamSceneManager; NewCameraMode: TCameraNavigationType;
  KeepView: boolean);
procedure ChangeCameraMode(SceneManager: TKamSceneManager; change: integer;
  KeepView: boolean);
{ @groupEnd }

{ Interpret and remove from ParStr(1) ... ParStr(ParCount)
  some params specific for this unit.
  Those params are documented in CamerasOptionsHelp.

  Call this BEFORE InitCameras. }
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

implementation

uses ParseParametersUnit;

{ global stuff --------------------------------------------------------------- }

procedure SetCameraModeInternal(SceneManager: TKamSceneManager; value: TCameraNavigationType);
{ This is a private procedure in this module.
  Look at SetCameraMode for something that you can publicly use.
  This procedure does not do some things that SetCameraMode does
  because this is used from InitCameras. }
begin
  Camera.NavigationType := Value;
  if CameraRadios[Camera.NavigationType] <> nil then
    CameraRadios[Camera.NavigationType].Checked := true;
end;

procedure InitCameras(SceneManager: TKamSceneManager);
begin
  { init SceneManager.Camera }
  SceneManager.Camera := Camera;
  if CameraRadios[Camera.NavigationType] <> nil then
    CameraRadios[Camera.NavigationType].Checked := true;
end;

procedure SceneInitCameras(
  const ModelBox: TBox3D;
  const InitialPosition, InitialDirection, InitialUp, GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);
begin
  { Init all cameras }
  Camera.Examine.Init(ModelBox, CameraRadius);
  Camera.Walk.Init(
    InitialPosition, InitialDirection, InitialUp, GravityUp,
    CameraPreferredHeight, CameraRadius);
  Camera.CameraRadius := CameraRadius;
end;

procedure SetCameraMode(SceneManager: TKamSceneManager; NewCameraMode: TCameraNavigationType;
  KeepView: boolean);
var
  Position, Direction, Up: TVector3Single;
begin
  KeepView := KeepView and (NewCameraMode <> Camera.NavigationType);
  if KeepView then
    Camera.Current.GetCameraVectors(Position, Direction, Up);

  SetCameraModeInternal(SceneManager, NewCameraMode);

  if KeepView then
    Camera.Current.SetCameraVectors(Position, Direction, Up);

  SceneManager.Camera.VisibleChange;
end;

procedure ChangeCameraMode(SceneManager: TKamSceneManager; Change: integer;
  KeepView: boolean);
var
  NewCameraMode: TCameraNavigationType;
begin
  NewCameraMode := TCameraNavigationType(
    ChangeIntCycle(Ord(Camera.NavigationType), Change,
      Ord(High(TCameraNavigationType))));
  SetCameraMode(SceneManager, NewCameraMode, KeepView);
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

initialization
  Camera := TUniversalCamera.Create(nil);
finalization
  FreeAndNil(Camera);
end.
