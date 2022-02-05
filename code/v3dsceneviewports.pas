{
  Copyright 2010-2022 Michalis Kamburelis.

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

type
  TViewportsConfig = (vc1, vc2Horizontal, vc4);
  TViewportClass = class of TCastleViewport;
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
  MainViewport: TCastleViewport;

  { Custom viewports, using the same Items as MainViewport.Items.
    Note that MainViewport is @italic(not listed here). }
  ExtraViewports: array [0..2] of TCastleViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindow; MainViewport: TCastleViewport);

{ Copy all Camera and Navigation settings from Source to Target. }
procedure AssignCameraAndNavigation(const Target, Source: TCastleViewport);

procedure ResizeViewports(Window: TCastleWindow; MainViewport: TCastleViewport);

{ Copy NewNavigationType to all (existing) viewports. }
procedure SetNavigationType(const NewNavigationType: TUserNavigationType);

procedure InitializeViewports(ViewportClass: TViewportClass);

{ Redraw all viewports (and background underneath).
  This renders viewports for the off-screen rendering. }
procedure ViewportsRender(const Container: TCastleContainer);

procedure ViewportsSetTransparent(const Transparent: Boolean);

implementation

uses CastleVectors, SysUtils, CastleUtils, CastleUIControls, CastleControls,
  CastleGLUtils, CastleColors, CastleLog, CastleRenderContext;

{ global routines ------------------------------------------------------------ }

var
  Background: TCastleRectangleControl;

procedure AssignCameraAndNavigation(const Target, Source: TCastleViewport);
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
  Window: TCastleWindow; MainViewport: TCastleViewport);

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

        MainViewport.Left := 0;
        MainViewport.Bottom := 0;
        MainViewport.Width := W - 1;
        MainViewport.Height := H;

        ExtraViewports[0].Left := W + 1;
        ExtraViewports[0].Bottom := 0;
        ExtraViewports[0].Width := W - 1;
        ExtraViewports[0].Height := H;
      end;
    vc4:
      begin
        W := Window.Width div 2;
        H := Window.Height div 2;

        MainViewport.Left := 0;
        MainViewport.Bottom := H + 1;
        MainViewport.Width := W - 1;
        MainViewport.Height := H - 1;

        ExtraViewports[0].Left := W + 1;
        ExtraViewports[0].Bottom := H + 1;
        ExtraViewports[0].Width := W - 1;
        ExtraViewports[0].Height := H - 1;

        ExtraViewports[1].Left := 0;
        ExtraViewports[1].Bottom := 0;
        ExtraViewports[1].Width := W - 1;
        ExtraViewports[1].Height := H - 1;

        ExtraViewports[2].Left := W + 1;
        ExtraViewports[2].Bottom := 0;
        ExtraViewports[2].Width := W - 1;
        ExtraViewports[2].Height := H - 1;
      end;
  end;
end;

procedure SetNavigationType(const NewNavigationType: TUserNavigationType);

  procedure CoreSetNavigationType(const Viewport: TCastleViewport;
    const Value: TUserNavigationType);
  begin
    case Value of
      untExamine: Viewport.NavigationType := ntExamine;
      untWalk: Viewport.NavigationType := ntWalk;
      untFly: Viewport.NavigationType := ntFly;
      unt2D: Viewport.Navigation := TCastle2DNavigation.Create(Viewport);
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

procedure InitializeViewports(ViewportClass: TViewportClass);
var
  I: Integer;
begin
  for I := 0 to High(ExtraViewports) do
  begin
    ExtraViewports[I] := ViewportClass.Create(nil);
    { do not use lights from Scene on other scenes }
    ExtraViewports[I].UseGlobalLights := false;
    ExtraViewports[I].Items := MainViewport.Items;
    { We will explicitly initialize camera and navigation.
      This also prevents the AutoCamera mechanism from overriding
      our camera set by AssignCameraAndNavigation. }
    ExtraViewports[I].AutoCamera := false;
    ExtraViewports[I].AutoNavigation := false;
  end;
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
