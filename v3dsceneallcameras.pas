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

uses SysUtils, KambiUtils, GLWindow, Cameras, Boxes3d, VectorMath,
  GL, GLU, KambiGLUtils, KambiSceneManager, Classes;

type
  TCameraMode = (cmExamine, cmWalk);

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
  const ModelBox: TBox3d;
  const InitialPosition, InitialDirection, InitialUp,
    GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);

const
  CameraNames: array[TCameraMode]of string =
   ('Examine', 'Walk');

var
  CameraRadios: array [TCameraMode] of TMenuItemRadio;

function CameraMode: TCameraMode;

{ Change current CameraMode.

  Set/ChangeCameraMode call VisibleChange on new camera.
  That's because changing Camera in fact changed
  Camera.Matrix, so we must do the same thing that would be done in
  Camera.VisibleChange.

  @groupBegin }
procedure SetCameraMode(SceneManager: TKamSceneManager; Kind: TCameraMode);
procedure ChangeCameraMode(SceneManager: TKamSceneManager; change: integer);
{ @groupEnd }

procedure SetProjectionMatrix(const AProjectionMatrix: TMatrix4Single);

{ The TWalkCamera used when CameraMode = cmWalk. }
function WalkCamera: TWalkCamera;

{ The TExamineCamera used when CameraMode = cmExamine. }
function ExamineCamera: TExamineCamera;

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

implementation

uses ParseParametersUnit;

const
  CameraClasses: array[TCameraMode]of TCameraClass =
  (TExamineCamera, TWalkCamera);

var
  FCameraMode: TCameraMode = cmExamine;
  AllCameras: array [TCameraMode] of TCamera;

procedure SetCameraModeInternal(SceneManager: TKamSceneManager; value: TCameraMode);
{ This is a private procedure in this module.
  Look at SetCameraMode for something that you can publicly use.
  This procedure does not do some things that SetCameraMode does
  because this is used from InitCameras. }
begin
  FCameraMode := value;
  SceneManager.Camera := AllCameras[FCameraMode];
  if CameraRadios[FCameraMode] <> nil then
    CameraRadios[FCameraMode].Checked := true;
end;

procedure InitCameras(SceneManager: TKamSceneManager);
var nk: TCameraMode;
begin
 { create cameras }
 for nk := Low(nk) to High(nk) do
 begin
   AllCameras[nk] := CameraClasses[nk].Create(nil);
 end;

 { Useful and works sensibly with our view3dscene events that pass
   mouse / keys to VRML/X3D scene. This way in Examine mode you can
   activate pointing device sensors.
   Note: This is the default now. }
 TExamineCamera(AllCameras[cmExamine]).ExclusiveEvents := false;

 { init SceneManager.Camera }
 SetCameraModeInternal(SceneManager, FCameraMode);
end;

procedure SceneInitCameras(
  const ModelBox: TBox3d;
  const InitialPosition, InitialDirection, InitialUp,
    GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);
begin
 { Init all cameras }
 TExamineCamera(AllCameras[cmExamine]).Init(ModelBox, CameraRadius);
 TWalkCamera   (AllCameras[cmWalk  ]).Init(
   InitialPosition, InitialDirection, InitialUp, GravityUp,
   CameraPreferredHeight, CameraRadius);
end;

function CameraMode: TCameraMode;
begin
 Result := FCameraMode;
end;

procedure SetProjectionMatrix(const AProjectionMatrix: TMatrix4Single);
var
  nk: TCameraMode;
begin
  { create cameras }
  for nk := Low(nk) to High(nk) do
    AllCameras[nk].ProjectionMatrix := AProjectionMatrix;
end;

procedure SetCameraMode(SceneManager: TKamSceneManager; Kind: TCameraMode);
begin
 SetCameraModeInternal(SceneManager, Kind);
 SceneManager.Camera.VisibleChange;
end;

procedure ChangeCameraMode(SceneManager: TKamSceneManager; Change: integer);
var
  NewCameraMode: TCameraMode;
  { Pos, Dir, Up: TVector3Single; }
begin
  NewCameraMode := TCameraMode( ChangeIntCycle(Ord(CameraMode), Change,
    Ord(High(TCameraMode))));

  (*
  Test TExamineCamera.GetCameraVectors: when switching from Examine
  to Walk, set Walk camera view to match Examine.

  if (CameraMode = cmExamine) and
     (NewCameraMode = cmWalk) then
  begin
    AllCameras[cmExamine].GetCameraVectors(Pos, Dir, Up);

    WalkCamera.Position  := Pos;
    WalkCamera.Direction := Dir;
    WalkCamera.Up        := Up ;
  end;
  *)

  SetCameraMode(SceneManager, NewCameraMode);
end;

function WalkCamera: TWalkCamera;
begin
  Result := TWalkCamera(AllCameras[cmWalk]);
end;

function ExamineCamera: TExamineCamera;
begin
  Result := TExamineCamera(AllCameras[cmExamine]);
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

{ unit init/fini ------------------------------------------------------------ }

procedure Fini;
var nk: TCameraMode;
begin
 for nk := Low(nk) to High(nk) do
   FreeAndNil(AllCameras[nk]);
end;

finalization
 Fini;
end.
