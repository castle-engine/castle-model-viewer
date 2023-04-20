{
  Copyright 2010-2023 Michalis Kamburelis.

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

{ }
unit V3DSceneViewports;

{$I v3dsceneconf.inc}

interface

uses CastleViewport, CastleWindow, CastleCameras,
  V3DSceneNavigationTypes;

{ Compile also with CGE branches that don't yet have new-cameras work merged.
  Once new-cameras merged -> master, we can remove this. }
{$if not declared(TCastleAutoNavigationViewport)}
  {$define TCastleAutoNavigationViewport:=TCastleViewport}
{$endif}

type
  TViewportsConfig = (vc1, vc2Horizontal, vc4);
  TViewportClass = class of TCastleAutoNavigationViewport;
const
  ViewportsConfigNames: array [TViewportsConfig] of string =
  ('_1 Viewport',
   '_2 Horizontal Viewports',
   '_4 Viewports'
  );

var
  ViewportsConfig: TViewportsConfig;

  { Main TCastleViewport.
    Set this before calling InitializeViewports. }
  MainViewport: TCastleAutoNavigationViewport;

  { Custom viewports, using the same Items as MainViewport.Items.
    Note that MainViewport is @italic(not listed here). }
  ExtraViewports: array [0..2] of TCastleAutoNavigationViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindow; MainViewport: TCastleAutoNavigationViewport);

{ Copy all Camera and Navigation settings from Source to Target. }
procedure AssignCameraAndNavigation(const Target, Source: TCastleAutoNavigationViewport);

procedure ResizeViewports(Window: TCastleWindow; MainViewport: TCastleViewport);

{ Copy NewNavigationType to all (existing) viewports. }
procedure SetNavigationType(const NewNavigationType: TUserNavigationType);

{ Create ExtraViewports[...].
  Set default values for all internal navigation components in all ExtraViewports[...].
  and on MainViewport too. }
procedure InitializeViewportsAndDefaultNavigation(ViewportClass: TViewportClass);

{ Redraw all viewports (and background underneath).
  This renders viewports for the off-screen rendering. }
procedure ViewportsRender(const Container: TCastleContainer);

procedure ViewportsSetTransparent(const Transparent: Boolean);

implementation

uses CastleVectors, SysUtils, CastleUtils, CastleUIControls, CastleControls,
  CastleGLUtils, CastleColors, CastleLog, CastleRenderContext, CastleKeysMouse;

{ global routines ------------------------------------------------------------ }

var
  Background: TCastleRectangleControl;

procedure AssignCameraAndNavigation(const Target, Source: TCastleAutoNavigationViewport);
var
  NavigationTypeStr: String;
begin
  Target.NavigationType := Source.NavigationType;

  if (Target.Navigation <> nil) and
     (Source.Navigation <> nil) then
    Target.Navigation.Assign(Source.Navigation)
  else
  if (Target.Navigation <> nil) or
     (Source.Navigation <> nil) then
  begin
    WriteStr(NavigationTypeStr, Target.NavigationType);
    WritelnLog('Both Source and Target have the same NavigationType, but only one of them has non-nil Navigation', [
      NavigationTypeStr
    ]);
  end;

  Target.Camera.Assign(Source.Camera);
end;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindow; MainViewport: TCastleAutoNavigationViewport);

  procedure AddViewport(Viewport: TCastleViewport);
  begin
    Viewport.FullSize := false;
    Window.Controls.InsertBackIfNotExists(Viewport);
  end;

var
  I: Integer;
  OldValue: TViewportsConfig;
begin
  if ViewportsConfig <> Value then
  begin
    OldValue := ViewportsConfig;
    ViewportsConfig := Value;
    MainViewport.FullSize := ViewportsConfig = vc1;
    case ViewportsConfig of
      vc1:
        begin
          { make sure glViewport is also restored }
          RenderContext.Viewport := Window.Rect; // TODO: This is most probably useless now?
          for I := 0 to High(ExtraViewports) do
            Window.Controls.Remove(ExtraViewports[I]);
        end;
      vc2Horizontal:
        begin
          AddViewport(ExtraViewports[0]);
          Window.Controls.Remove(ExtraViewports[1]);
          Window.Controls.Remove(ExtraViewports[2]);
          { Configure camera for newly appearing viewports }
          if OldValue = vc1 then
            AssignCameraAndNavigation(ExtraViewports[0], MainViewport);
        end;
      vc4:
        begin
          AddViewport(ExtraViewports[0]);
          AddViewport(ExtraViewports[1]);
          AddViewport(ExtraViewports[2]);
          { Configure camera for newly appearing viewports }
          case OldValue of
            vc1:
              begin
                AssignCameraAndNavigation(ExtraViewports[0], MainViewport);
                AssignCameraAndNavigation(ExtraViewports[1], MainViewport);
                AssignCameraAndNavigation(ExtraViewports[2], MainViewport);
              end;
            vc2Horizontal:
              begin
                AssignCameraAndNavigation(ExtraViewports[1], MainViewport);
                AssignCameraAndNavigation(ExtraViewports[2], ExtraViewports[0]);
              end;
            else raise EInternalError.Create('ViewportsConfig OldValue was supposed to be <> new value?');
          end;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('ViewportsConfig?');
      {$endif}
    end;
    { Do this at the end, to make Background to most in the back.
      Also, always remove first, and the InsertBack -- to again
      make sure Background is in the back (instead of doing InsertBackIfNotExists). }
    Window.Controls.Remove(Background);
    if ViewportsConfig <> vc1 then
      Window.Controls.InsertBack(Background);
    ResizeViewports(Window, MainViewport);
  end;
end;

procedure ResizeViewports(Window: TCastleWindow; MainViewport: TCastleViewport);
var
  W, H: Cardinal;
begin
  case ViewportsConfig of
    vc1: ;
    vc2Horizontal:
      begin
        W := Window.Width div 2;
        H := Window.Height;

        MainViewport.Anchor(hpLeft);
        MainViewport.Anchor(vpBottom);
        MainViewport.Width := W - 1;
        MainViewport.Height := H;

        ExtraViewports[0].Anchor(hpRight);
        ExtraViewports[0].Anchor(vpBottom);
        ExtraViewports[0].Width := W - 1;
        ExtraViewports[0].Height := H;
      end;
    vc4:
      begin
        W := Window.Width div 2;
        H := Window.Height div 2;

        MainViewport.Anchor(hpLeft);
        MainViewport.Anchor(vpTop);
        MainViewport.Width := W - 1;
        MainViewport.Height := H - 1;

        ExtraViewports[0].Anchor(hpRight);
        ExtraViewports[0].Anchor(vpTop);
        ExtraViewports[0].Width := W - 1;
        ExtraViewports[0].Height := H - 1;

        ExtraViewports[1].Anchor(hpLeft);
        ExtraViewports[1].Anchor(vpBottom);
        ExtraViewports[1].Width := W - 1;
        ExtraViewports[1].Height := H - 1;

        ExtraViewports[2].Anchor(hpRight);
        ExtraViewports[2].Anchor(vpBottom);
        ExtraViewports[2].Width := W - 1;
        ExtraViewports[2].Height := H - 1;
      end;
  end;
end;

procedure SetNavigationType(const NewNavigationType: TUserNavigationType);

  procedure CoreSetNavigationType(const Viewport: TCastleAutoNavigationViewport;
    const Value: TUserNavigationType);
  begin
    case Value of
      untExamine: Viewport.NavigationType := ntExamine;
      untWalk: Viewport.NavigationType := ntWalk;
      untFly: Viewport.NavigationType := ntFly;
      unt2D: Viewport.NavigationType := nt2D;
      untNone: Viewport.NavigationType := ntNone;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('CoreSetNavigationType NavigationType?');
      {$endif}
    end;
  end;

var
  I: Integer;
begin
  CoreSetNavigationType(MainViewport, NewNavigationType);
  for I := 0 to High(ExtraViewports) do
    CoreSetNavigationType(ExtraViewports[I], NewNavigationType);
end;

procedure InitializeViewportsAndDefaultNavigation(ViewportClass: TViewportClass);

  procedure InitializeDefaultNavigation(const V: TCastleAutoNavigationViewport);
  begin
    { This configuration of navigations somewhat corresponds to CGE editor
      navigations in castleviewport_design_navigation.inc .

      However with some differences: in view3dscene, left mouse button is free to use,
      so we let it
      - do move by dragging in Walk
      - operate in Examine and 2D easily (in CGE editor, one has to use right mouse button). }

    V.InternalWalkNavigation.MoveSpeed := 10; // faster default
    V.InternalWalkNavigation.MinAngleFromGravityUp := 0; // allow to rotate smoothly from Top (7) view
    V.InternalWalkNavigation.ZoomEnabled := true; // start with zoom enabled, to support mouse wheel
    V.InternalWalkNavigation.Input_Jump.Assign(keySpace, keyE);
    V.InternalWalkNavigation.Input_Crouch.Assign(keyC, keyQ);
    V.InternalWalkNavigation.Input_IncreasePreferredHeight.Assign(keyInsert);
    V.InternalWalkNavigation.Input_DecreasePreferredHeight.Assign(keyDelete);

    { These check collisions only at zoom by default, and it is more confusing
      than useful it seems.
      See https://github.com/castle-engine/view3dscene/issues/46 . }
    V.InternalExamineNavigation.CheckCollisions := false;
    V.Internal2DNavigation.CheckCollisions := false;
end;

var
  I: Integer;
begin
  for I := 0 to High(ExtraViewports) do
  begin
    ExtraViewports[I] := ViewportClass.Create(nil);
    // Move extra cameras to shared Items.
    ExtraViewports[I].Items.Remove(ExtraViewports[I].Camera);
    MainViewport.Items.Add(ExtraViewports[I].Camera);
    ExtraViewports[I].Items := MainViewport.Items;
    { We will explicitly initialize camera and navigation.
      This also prevents the AutoCamera mechanism from overriding
      our camera set by AssignCameraAndNavigation. }
    ExtraViewports[I].AutoCamera := false;
    ExtraViewports[I].AutoNavigation := false;

    InitializeDefaultNavigation(ExtraViewports[I]);
  end;

  InitializeDefaultNavigation(MainViewport);

  Background := TCastleRectangleControl.Create(nil);
  Background.FullSize := true;
  Background.Color := Gray;
end;

procedure ViewportsRender(const Container: TCastleContainer);
var
  I: Integer;
const
  Visible: array [TViewportsConfig] of Integer = (0, 1, 3);
begin
  if ViewportsConfig <> vc1 then
    Container.RenderControl(Background, Container.Rect);
  for I := 0 to Visible[ViewportsConfig] - 1 do
    Container.RenderControl(ExtraViewports[I], Container.Rect);
  Container.RenderControl(MainViewport, Container.Rect);
end;

procedure ViewportsSetTransparent(const Transparent: Boolean);
var
  I: Integer;
begin
  MainViewport.Transparent := Transparent;
  for I := 0 to High(ExtraViewports) do
    ExtraViewports[I].Transparent := Transparent;
end;

procedure DoFinalization;
var
  I: Integer;
begin
  for I := 0 to High(ExtraViewports) do
    FreeAndNil(ExtraViewports[I]);
  FreeAndNil(Background);
end;

finalization
  DoFinalization;
end.
