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
  GL, GLU, KambiGLUtils, KambiSceneManager, Classes;

type
  TCameraNavigationType = (ntExamine, ntWalk);

  { Camera that allows any kind of navigation (Examine, Walk).
    You can switch between navigation types, while preserving the camera view.

    This simply keeps an TExamineCamera and TWalkCamera instances inside,
    and passes events (key, mouse presses, idle) to the current one.
    Properties (like camera position, direction, up vectors) are simply
    set on both instances simultaneously.

    For some uses you can even directly access the internal camera instances
    inside @link(Examine) and @link(Walk) properties. However, do not
    change them directly @italic(when you can use instead a property of
    this class). For example, it is Ok to directly change input key
    by @noAutoLink(@code(Walk.Input_Forward)) (see TWalkCamera.Input_Forward).
    However, do not directly call @noAutoLink(@code(Walk.SetInitialCameraVectors))
    (see TWalkCamera.SetInitialCameraVectors), instead use a method of this class:
    TUniversalCamera.SetInitialCameraVectors. This way both @link(Examine)
    and @link(Walk) will be kept in synch. }
  TUniversalCamera = class(TCamera)
  private
    FExamine: TExamineCamera;
    FWalk: TWalkCamera;
    FNavigationType: TCameraNavigationType;
  protected
    procedure SetIgnoreAllInputs(const Value: boolean); override;
    procedure SetProjectionMatrix(const Value: TMatrix4Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Examine: TExamineCamera read FExamine;
    property Walk: TWalkCamera read FWalk;
    { Current (determined by NavigationType) internal camera,
      that is either @link(Examine) or @link(Walk). }
    function Current: TCamera;

    function Matrix: TMatrix4Single; override;
    function RotationMatrix: TMatrix4Single; override;
    procedure GetCameraVectors(out APos, ADir, AUp: TVector3Single); override;
    procedure GetCameraVectors(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetPosition: TVector3Single; override;
    procedure SetCameraVectors(const APos, ADir, AUp: TVector3Single); override;
  published
    property NavigationType: TCameraNavigationType read FNavigationType write FNavigationType;
  end;

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
  UCamera: TUniversalCamera;

implementation

uses ParseParametersUnit;

{ TUniversalCamera ----------------------------------------------------------- }

constructor TUniversalCamera.Create(AOwner: TComponent);
begin
  inherited;
  FExamine := TExamineCamera.Create(nil);
  FWalk := TWalkCamera.Create(nil);
end;

destructor TUniversalCamera.Destroy;
begin
  FreeAndNil(FExamine);
  FreeAndNil(FWalk);
  inherited;
end;

function TUniversalCamera.Current: TCamera;
begin
  if FNavigationType = ntExamine then
    Result := FExamine else
    Result := FWalk;
end;

function TUniversalCamera.Matrix: TMatrix4Single;
begin
  Result := Current.Matrix;
end;

function TUniversalCamera.RotationMatrix: TMatrix4Single;
begin
  Result := Current.RotationMatrix;
end;

procedure TUniversalCamera.GetCameraVectors(out APos, ADir, AUp: TVector3Single);
begin
  Current.GetCameraVectors(APos, ADir, AUp);
end;

procedure TUniversalCamera.GetCameraVectors(out APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  Current.GetCameraVectors(APos, ADir, AUp, AGravityUp);
end;

function TUniversalCamera.GetPosition: TVector3Single;
begin
  Result := Current.GetPosition;
end;

procedure TUniversalCamera.SetCameraVectors(const APos, ADir, AUp: TVector3Single);
begin
  FExamine.SetCameraVectors(APos, ADir, AUp);
  FWalk.SetCameraVectors(APos, ADir, AUp);
end;

procedure TUniversalCamera.SetIgnoreAllInputs(const Value: boolean);
begin
  inherited;
  FExamine.IgnoreAllInputs := Value;
  FWalk.IgnoreAllInputs := Value;
end;

procedure TUniversalCamera.SetProjectionMatrix(const Value: TMatrix4Single);
begin
  { This calls RecalculateFrustum on all 3 cameras, while only once
    is needed... But speed should not be a problem here, this is seldom used. }
  inherited;
  FExamine.ProjectionMatrix := Value;
  FWalk.ProjectionMatrix := Value;
end;

{ global stuff --------------------------------------------------------------- }

procedure SetCameraModeInternal(SceneManager: TKamSceneManager; value: TCameraNavigationType);
{ This is a private procedure in this module.
  Look at SetCameraMode for something that you can publicly use.
  This procedure does not do some things that SetCameraMode does
  because this is used from InitCameras. }
begin
  UCamera.NavigationType := Value;
  SceneManager.Camera := UCamera.Current;
  if CameraRadios[UCamera.NavigationType] <> nil then
    CameraRadios[UCamera.NavigationType].Checked := true;
end;

procedure InitCameras(SceneManager: TKamSceneManager);
begin
  { init SceneManager.Camera }
  SceneManager.Camera := UCamera.Current;
  if CameraRadios[UCamera.NavigationType] <> nil then
    CameraRadios[UCamera.NavigationType].Checked := true;
end;

procedure SceneInitCameras(
  const ModelBox: TBox3D;
  const InitialPosition, InitialDirection, InitialUp, GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);
begin
  { Init all cameras }
  UCamera.Examine.Init(ModelBox, CameraRadius);
  UCamera.Walk.Init(
    InitialPosition, InitialDirection, InitialUp, GravityUp,
    CameraPreferredHeight, CameraRadius);
end;

procedure SetCameraMode(SceneManager: TKamSceneManager; NewCameraMode: TCameraNavigationType;
  KeepView: boolean);
var
  Position, Direction, Up: TVector3Single;
begin
  KeepView := KeepView and (NewCameraMode <> UCamera.NavigationType);
  if KeepView then
    UCamera.Current.GetCameraVectors(Position, Direction, Up);

  SetCameraModeInternal(SceneManager, NewCameraMode);

  if KeepView then
    UCamera.Current.SetCameraVectors(Position, Direction, Up);

  SceneManager.Camera.VisibleChange;
end;

procedure ChangeCameraMode(SceneManager: TKamSceneManager; Change: integer;
  KeepView: boolean);
var
  NewCameraMode: TCameraNavigationType;
begin
  NewCameraMode := TCameraNavigationType(
    ChangeIntCycle(Ord(UCamera.NavigationType), Change,
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
  UCamera := TUniversalCamera.Create(nil);

  { Useful and works sensibly with our view3dscene events that pass
    mouse / keys to VRML/X3D scene. This way in Examine mode you can
    activate pointing device sensors.
    Note: This is the default now. }
  UCamera.Examine.ExclusiveEvents := false;
finalization
  FreeAndNil(UCamera);
end.
