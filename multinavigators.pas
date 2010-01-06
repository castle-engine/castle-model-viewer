{
  Copyright 2003-2008 Michalis Kamburelis.

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
}

unit MultiNavigators;

{ modulik ulatwiajacy view3dmodelowi uzywac roznych Navigatorow
  z okienkiem typu TGLUIWindow.

  SceneAnimation is used only to call ResetOcclusionQuery.
  You can pass @nil if SceneAnimation is not initialized yet. }

{$I openglmac.inc}

interface

uses SysUtils, KambiUtils, GLWindow, Navigation, Boxes3d, VectorMath,
  GL, GLU, GLExt, KambiGLUtils, VRMLGLAnimation, Classes;

type
  TNavigatorKind = (nkExamine, nkWalk);

{ Call this ONCE on created glwin (glwin need not be after Init).
  This will take care of always providing proper glwin.Navigator
  for you. MoveAllowed will be used for collision detection
  when NavigatorKind in [nkWalk].

  You CAN NOT directly modify Navigators' properties
  (settings like InitialCameraXxx, Rotation, but also general settings
  like OnMatrixchanged). You can do it only indirectly using this unit. }
procedure InitMultiNavigators(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation;
  MoveAllowed: TMoveAllowedFunc;
  GetCameraHeight: TGetCameraHeight;
  VisibleChange: TNotifyEvent);

{ Call this always when scene changes. Give new BoundingBox and
  InitialCameraXxx and GravityUp settings for this new scene.
  This will call Init() functions for all navigators (that are ready
  for various values of  NavigatorKind).

  You must call InitMultiNavigators before using this. }
procedure SceneInitMultiNavigators(
  const ModelBox: TBox3d;
  const InitialCameraPos, InitialCameraDir, InitialCameraUp,
    GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);

const
  NavigatorNames: array[TNavigatorKind]of string =
   ('Examine', 'Walk');

var
  NavigatorRadios: array [TNavigatorKind] of TMenuItemRadio;

function NavigatorKind: TNavigatorKind;

{ Change current NavigatorKind.

  Set/ChangeNavigatorKind call VisibleChange on new navigator.
  That's because changing Navigator in fact changed
  Navigator.Matrix, so we must do the same thing that would be done in
  Navigator.VisibleChange.

  Also glwin.EventResize is called, so that you can adjust
  your projection settings (specifically, projection zNear/zFar)
  to different values of NavigatorKind.

  @groupBegin }
procedure SetNavigatorKind(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Kind: TNavigatorKind);
procedure ChangeNavigatorKind(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; change: integer);
{ @groupEnd }

procedure SetProjectionMatrix(const AProjectionMatrix: TMatrix4Single);

{ The TWalkNavigator used when NavigationKind = nkWalk.
  Use this e.g. to set it's ProjectionMatrix. }
function WalkNav: TWalkNavigator;

{ Interpret and remove from ParStr(1) ... ParStr(ParCount)
  some params specific for this unit.
  Those params are documented in MultiNavigatorsOptionsHelp.

  Call this BEFORE InitMultiNavigators. }
procedure MultiNavigatorsParseParameters;

const
  MultiNavigatorsOptionsHelp =
  '  --navigation Examine|Walk'+nl+
  '                        Set initial navigation style';

implementation

uses ParseParametersUnit;

const
  NavigatorClasses: array[TNavigatorKind]of TNavigatorClass =
  (TExamineNavigator, TWalkNavigator);

var
  FNavigatorKind: TNavigatorKind = nkExamine;
  Navigators: array[TNavigatorKind]of TNavigator;

procedure SetNavigatorKindInternal(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; value: TNavigatorKind);
{ This is a private procedure in this module.
  Look at SetNavigatorKind for something that you can publicly use.
  This procedure does not do some things that SetNavigatorKind does
  because this is used from InitMultiNavigators. }
begin
  FNavigatorKind := value;
  Glwin.Navigator := Navigators[FNavigatorKind];
  Glwin.UpdateMouseLook;
  if NavigatorRadios[FNavigatorKind] <> nil then
    NavigatorRadios[FNavigatorKind].Checked := true;

  if (SceneAnimation <> nil) and SceneAnimation.Loaded then
  begin
    SceneAnimation.Navigator := Glwin.Navigator;
    { Changing navigator changes also the view rapidly. }
    SceneAnimation.ViewChangedSuddenly;
  end;
end;

procedure InitMultiNavigators(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation;
  MoveAllowed: TMoveAllowedFunc;
  GetCameraHeight: TGetCameraHeight;
  VisibleChange: TNotifyEvent);
var nk: TNavigatorKind;
begin
 { create navigators }
 for nk := Low(nk) to High(nk) do
 begin
   Navigators[nk] := NavigatorClasses[nk].Create(nil);
   Navigators[nk].OnVisibleChange := VisibleChange;
 end;

 { Useful and works sensibly with our view3dscene events that pass
   mouse / keys to VRML/X3D scene. This way in Examine mode you can
   activate pointing device sensors.
   Note: This is the default now. }
 TExamineNavigator(Navigators[nkExamine]).ExclusiveEvents := false;

 TWalkNavigator(Navigators[nkWalk]).OnMoveAllowed := MoveAllowed;
 TWalkNavigator(Navigators[nkWalk]).OnGetCameraHeight := GetCameraHeight;

 { init glwin.Navigator }
 SetNavigatorKindInternal(glwin, SceneAnimation, FNavigatorKind);
end;

procedure SceneInitMultiNavigators(
  const ModelBox: TBox3d;
  const InitialCameraPos, InitialCameraDir, InitialCameraUp,
    GravityUp: TVector3Single;
  const CameraPreferredHeight, CameraRadius: Single);
begin
 { Init all navigators }
 TExamineNavigator(Navigators[nkExamine]).Init(ModelBox, CameraRadius);
 TWalkNavigator  (Navigators[nkWalk  ]).Init(
   InitialCameraPos, InitialCameraDir, InitialCameraUp, GravityUp,
   CameraPreferredHeight, CameraRadius);
end;

function NavigatorKind: TNavigatorKind;
begin
 Result := FNavigatorKind;
end;

procedure SetProjectionMatrix(const AProjectionMatrix: TMatrix4Single);
var
  nk: TNavigatorKind;
begin
  { create navigators }
  for nk := Low(nk) to High(nk) do
    Navigators[nk].ProjectionMatrix := AProjectionMatrix;
end;

procedure SetNavigatorKind(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Kind: TNavigatorKind);
begin
 SetNavigatorKindInternal(glwin, SceneAnimation, Kind);
 Glwin.Navigator.VisibleChange;

 { wywolaj EventResize zeby dostosowal zNear naszego projection do
   aktualnego glw.Navigator }
 if not Glwin.Closed then
   Glwin.EventResize;
end;

{$I MacChangeEnum.inc}

procedure ChangeNavigatorKind(glwin: TGLUIWindow;
  SceneAnimation: TVRMLGLAnimation; Change: integer);
var
  NewNavigatorKind: TNavigatorKind;
  { Pos, Dir, Up: TVector3Single; }
begin
  {$define CHANGE_ENUM_TYPE := TNavigatorKind}
  {$define CHANGE_ENUM_NAME := NavigatorKind}
  {$define CHANGE_ENUM_CHANGE := Change}
  NewNavigatorKind := CHANGE_ENUM;

  (*
  Test TExamineNavigator.GetCameraVectors: when switching from Examine
  to Walk, set Walk navigator view to match Examine.

  if (NavigatorKind = nkExamine) and
     (NewNavigatorKind = nkWalk) then
  begin
    Navigators[nkExamine].GetCameraVectors(Pos, Dir, Up);

    WalkNav.CameraPos := Pos;
    WalkNav.CameraDir := Dir;
    WalkNav.CameraUp  := Up ;
  end;
  *)

  SetNavigatorKind(glwin, SceneAnimation, NewNavigatorKind);
end;

{$I MacArrayPos.inc}
{$define ARRAY_POS_FUNCTION_NAME := StrToNavigatorKind}
{$define ARRAY_POS_ARRAY_NAME := NavigatorNames}
{$define ARRAY_POS_INDEX_TYPE := TNavigatorKind}
IMPLEMENT_ARRAY_POS_CASE_CHECKED

function WalkNav: TWalkNavigator;
begin
 Result := TWalkNavigator(Navigators[nkWalk]);
end;

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
   Assert(OptionNum = 0);
   FNavigatorKind := StrToNavigatorKind(Argument, true);
  end;

procedure MultiNavigatorsParseParameters;
const
  Options: array[0..0]of TOption =
  ((Short:#0; Long:'navigation'; Argument: oaRequired));
begin
 ParseParameters(Options, @OptionProc, nil, true);
end;

{ unit init/fini ------------------------------------------------------------ }

procedure Fini;
var nk: TNavigatorKind;
begin
 for nk := Low(nk) to High(nk) do
   FreeAndNil(Navigators[nk]);
end;

finalization
 Fini;
end.
