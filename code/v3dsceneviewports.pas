{
  Copyright 2010-2018 Michalis Kamburelis.

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

uses CastleSceneManager, CastleWindow, CastleCameras, CastleGLContainer;

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

  { SceneManager, used also by all viewports.
    Set this before calling InitializeViewports. }
  SceneManager: TCastleSceneManager;

  { Custom viewports. Remember that also SceneManager acts as the first viewport,
    @italic(not listed here). }
  Viewports: array [0..2] of TCastleViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindowCustom; SceneManager: TCastleSceneManager);

{ Copy all Camera and Navigation settings from Source to Target. }
procedure AssignCameraAndNavigation(const Target, Source: TCastleAbstractViewport);

procedure ResizeViewports(Window: TCastleWindowCustom; SceneManager: TCastleSceneManager);

{ Copy NavigationType to (existing) viewports cameras. }
procedure ViewportsSetNavigationType(const NavigationType: TNavigationType);

procedure InitializeViewports(ViewportClass: TViewportClass);

{ Redraw everything in viewports (including scene manager).

  If some custom viewports are visible then redraw background and
  all viewports.
  Then does SceneManager.Render, as SceneManager is also another viewport.

  This renders viewports for the off-screen rendering. }
procedure ViewportsRender(const Container: TGLContainer);

implementation

uses CastleVectors, SysUtils, CastleUtils, CastleUIControls, CastleControls,
  CastleGLUtils, CastleColors, CastleLog;

{ global routines ------------------------------------------------------------ }

var
  Background: TCastleRectangleControl;

procedure AssignCameraAndNavigation(const Target, Source: TCastleAbstractViewport);
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
  Window: TCastleWindowCustom; SceneManager: TCastleSceneManager);

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
    SceneManager.FullSize := ViewportsConfig = vc1;
    case ViewportsConfig of
      vc1:
        begin
          { make sure glViewport is also restored }
          RenderContext.Viewport := Window.Rect;
          for I := 0 to High(Viewports) do
            Window.Controls.Remove(Viewports[I]);
        end;
      vc2Horizontal:
        begin
          AddViewport(Viewports[0]);
          Window.Controls.Remove(Viewports[1]);
          Window.Controls.Remove(Viewports[2]);
          { Configure camera for newly appearing viewports }
          if OldValue = vc1 then
            AssignCameraAndNavigation(Viewports[0], SceneManager);
        end;
      vc4:
        begin
          AddViewport(Viewports[0]);
          AddViewport(Viewports[1]);
          AddViewport(Viewports[2]);
          { Configure camera for newly appearing viewports }
          case OldValue of
            vc1:
              begin
                AssignCameraAndNavigation(Viewports[0], SceneManager);
                AssignCameraAndNavigation(Viewports[1], SceneManager);
                AssignCameraAndNavigation(Viewports[2], SceneManager);
              end;
            vc2Horizontal:
              begin
                AssignCameraAndNavigation(Viewports[1], SceneManager);
                AssignCameraAndNavigation(Viewports[2], Viewports[0]);
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
    ResizeViewports(Window, SceneManager);
  end;
end;

procedure ResizeViewports(Window: TCastleWindowCustom; SceneManager: TCastleSceneManager);
var
  W, H: Cardinal;
begin
  case ViewportsConfig of
    vc1: ;
    vc2Horizontal:
      begin
        W := Window.Width div 2;
        H := Window.Height;

        SceneManager.Left := 0;
        SceneManager.Bottom := 0;
        SceneManager.Width := W - 1;
        SceneManager.Height := H;

        Viewports[0].Left := W + 1;
        Viewports[0].Bottom := 0;
        Viewports[0].Width := W - 1;
        Viewports[0].Height := H;
      end;
    vc4:
      begin
        W := Window.Width div 2;
        H := Window.Height div 2;

        SceneManager.Left := 0;
        SceneManager.Bottom := H + 1;
        SceneManager.Width := W - 1;
        SceneManager.Height := H - 1;

        Viewports[0].Left := W + 1;
        Viewports[0].Bottom := H + 1;
        Viewports[0].Width := W - 1;
        Viewports[0].Height := H - 1;

        Viewports[1].Left := 0;
        Viewports[1].Bottom := 0;
        Viewports[1].Width := W - 1;
        Viewports[1].Height := H - 1;

        Viewports[2].Left := W + 1;
        Viewports[2].Bottom := 0;
        Viewports[2].Width := W - 1;
        Viewports[2].Height := H - 1;
      end;
  end;
end;

procedure ViewportsSetNavigationType(const NavigationType: TNavigationType);
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    Viewports[I].NavigationType := NavigationType;
end;

procedure InitializeViewports(ViewportClass: TViewportClass);
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
  begin
    Viewports[I] := ViewportClass.Create(nil);
    { do not use lights from Scene on other scenes }
    Viewports[I].UseGlobalLights := false;
    Viewports[I].SceneManager := SceneManager;
  end;
  Background := TCastleRectangleControl.Create(nil);
  Background.FullSize := true;
  Background.Color := Gray;
end;

procedure ViewportsRender(const Container: TGLContainer);
var
  I: Integer;
const
  Visible: array [TViewportsConfig] of Integer = (0, 1, 3);
begin
  if ViewportsConfig <> vc1 then
    Container.RenderControl(Background, Container.Rect);
  for I := 0 to Visible[ViewportsConfig] - 1 do
    Container.RenderControl(Viewports[I], Container.Rect);
  Container.RenderControl(SceneManager, Container.Rect);
end;

procedure DoFinalization;
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    FreeAndNil(Viewports[I]);
  FreeAndNil(Background);
end;

finalization
  DoFinalization;
end.
