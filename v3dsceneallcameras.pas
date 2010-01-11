{
  Copyright 2003-2009 Michalis Kamburelis.

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

  SceneAnimation parameter for some procedures here
  is used only to call Scene.ViewChangedSuddenly,
  and to set Scene.Camera to current Camera.
  You can pass @nil if SceneAnimation is not initialized yet. }
unit V3DSceneAllCameras;

{$I openglmac.inc}

interface

uses SysUtils, KambiUtils, GLWindow, Cameras, Boxes3d, VectorMath,
  GL, GLU, GLExt, KambiGLUtils, VRMLGLAnimation, Classes;

type
  TCameraMode = (cmExamine, cmWalk);

{ Call this ONCE on created glwin (glwin need not be after Init).
  This will take care of always providing proper glwin.Camera
  for you. MoveAllowed will be used for collision detection
  when CameraMode in [cmWalk].

  You CAN NOT directly modify Cameras' properties
  (settings like InitialCameraXxx, Rotation, but also general settings
  like OnMatrixchanged). You can do it only indirectly using this unit. }
procedure InitCameras(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation;
  MoveAllowed: TMoveAllowedFunc;
  GetCameraHeight: TGetCameraHeight;
  VisibleChange: TNotifyEvent);

{ Call this always when scene changes. Give new BoundingBox and
  InitialCameraXxx and GravityUp settings for this new scene.
  This will call Init() functions for all cameras (that are ready
  for various values of CameraMode).

  You must call InitCameras before using this. }
procedure SceneInitCameras(
  const ModelBox: TBox3d;
  const InitialCameraPos, InitialCameraDir, InitialCameraUp,
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

  Also glwin.EventResize is called, so that you can adjust
  your projection settings (specifically, projection zNear/zFar)
  to different values of CameraMode.

  @groupBegin }
procedure SetCameraMode(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Kind: TCameraMode);
procedure ChangeCameraMode(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; change: integer);
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

implementation

uses ParseParametersUnit;

const
  CameraClasses: array[TCameraMode]of TCameraClass =
  (TExamineCamera, TWalkCamera);

var
  FCameraMode: TCameraMode = cmExamine;
  AllCameras: array [TCameraMode] of TCamera;

procedure SetCameraModeInternal(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; value: TCameraMode);
{ This is a private procedure in this module.
  Look at SetCameraMode for something that you can publicly use.
  This procedure does not do some things that SetCameraMode does
  because this is used from InitCameras. }
begin
  FCameraMode := value;
  Glwin.Camera := AllCameras[FCameraMode];
  if CameraRadios[FCameraMode] <> nil then
    CameraRadios[FCameraMode].Checked := true;

  if SceneAnimation <> nil then
  begin
    SceneAnimation.Camera := Glwin.Camera;
    { Changing camera changes also the view rapidly. }
    SceneAnimation.ViewChangedSuddenly;
  end;
end;

procedure InitCameras(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation;
  MoveAllowed: TMoveAllowedFunc;
  GetCameraHeight: TGetCameraHeight;
  VisibleChange: TNotifyEvent);
var nk: TCameraMode;
begin
 { create cameras }
 for nk := Low(nk) to High(nk) do
 begin
   AllCameras[nk] := CameraClasses[nk].Create(nil);
   AllCameras[nk].OnVisibleChange := VisibleChange;
 end;

 { Useful and works sensibly with our view3dscene events that pass
   mouse / keys to VRML/X3D scene. This way in Examine mode you can
   activate pointing device sensors.
   Note: This is the default now. }
 TExamineCamera(AllCameras[cmExamine]).ExclusiveEvents := false;

 TWalkCamera(AllCameras[cmWalk]).OnMoveAllowed := MoveAllowed;
 TWalkCamera(AllCameras[cmWalk]).OnGetCameraHeight := GetCameraHeight;

 { init glwin.Camera }
 SetCameraModeInternal(glwin, SceneAnimation, FCameraMode);
end;

procedure SceneInitCameras(
  const ModelBox: TBox3d;
  const InitialCameraPos, InitialCameraDir, InitialCameraUp,
    GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);
begin
 { Init all cameras }
 TExamineCamera(AllCameras[cmExamine]).Init(ModelBox, CameraRadius);
 TWalkCamera   (AllCameras[cmWalk  ]).Init(
   InitialCameraPos, InitialCameraDir, InitialCameraUp, GravityUp,
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

procedure SetCameraMode(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Kind: TCameraMode);
begin
 SetCameraModeInternal(glwin, SceneAnimation, Kind);
 Glwin.Camera.VisibleChange;

 { wywolaj EventResize zeby dostosowal zNear naszego projection do
   aktualnego glw.Camera }
 if not Glwin.Closed then
   Glwin.EventResize;
end;

{$I MacChangeEnum.inc}

procedure ChangeCameraMode(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Change: integer);
var
  NewCameraMode: TCameraMode;
  { Pos, Dir, Up: TVector3Single; }
begin
  {$define CHANGE_ENUM_TYPE := TCameraMode}
  {$define CHANGE_ENUM_NAME := CameraMode}
  {$define CHANGE_ENUM_CHANGE := Change}
  NewCameraMode := CHANGE_ENUM;

  (*
  Test TExamineCamera.GetCameraVectors: when switching from Examine
  to Walk, set Walk camera view to match Examine.

  if (CameraMode = cmExamine) and
     (NewCameraMode = cmWalk) then
  begin
    AllCameras[cmExamine].GetCameraVectors(Pos, Dir, Up);

    WalkCamera.CameraPos := Pos;
    WalkCamera.CameraDir := Dir;
    WalkCamera.CameraUp  := Up ;
  end;
  *)

  SetCameraMode(glwin, SceneAnimation, NewCameraMode);
end;

{$I MacArrayPos.inc}
{$define ARRAY_POS_FUNCTION_NAME := StrToCameraMode}
{$define ARRAY_POS_ARRAY_NAME := CameraNames}
{$define ARRAY_POS_INDEX_TYPE := TCameraMode}
IMPLEMENT_ARRAY_POS_CASE_CHECKED

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
   FCameraMode := StrToCameraMode(Argument, true);
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
