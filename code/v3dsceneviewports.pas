{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Manage all viewports (this application shows possibly multiple viewports,
  if you use "Display -> 2/4 viewports" menu items). }
unit V3DSceneViewports;

{$I v3dsceneconf.inc}

interface

uses CastleViewport, CastleWindow, CastleCameras, CastleScene;

type
  TMyViewport = class(TCastleAutoNavigationViewport)
  public
    { The central scene that is always assigned, rendered
      and contains the loaded model (or empty model, if nothing loaded).

      This is equivalent to CGE Items.MainScene, but Items.MainScene is
      a deprecated CGE thing that will be eventually removed. }
    MainScene: TCastleScene;
  end;

  TViewportsConfig = (vc1, vc2Horizontal, vc4);
  TViewportClass = class of TMyViewport;

const
  ViewportsConfigNames: array [TViewportsConfig] of string =
  ('_1 Viewport',
   '_2 Horizontal Viewports',
   '_4 Viewports'
  );

var
  ViewportsConfig: TViewportsConfig;

  { Main viewport. Set this before calling InitializeViewports. }
  MainViewport: TMyViewport;

  { Custom viewports, using the same Items as MainViewport.Items.
    Note that MainViewport is @italic(not listed here). }
  ExtraViewports: array [0..2] of TMyViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindow; MainViewport: TMyViewport);

{ Copy all Camera and Navigation settings from Source to Target. }
procedure AssignCameraAndNavigation(const Target, Source: TMyViewport);

procedure ResizeViewports(Window: TCastleWindow; MainViewport: TCastleViewport);

{ Create ExtraViewports[...].
  Set default values for all internal navigation components in all ExtraViewports[...].
  and on MainViewport too. }
procedure InitializeViewportsAndDefaultNavigation(ViewportClass: TViewportClass);

procedure InitializeViewportsMainScene(const NewMainScene: TCastleScene);

{ Redraw all viewports (and background underneath).
  This renders viewports for the off-screen rendering. }
procedure ViewportsRender(const Container: TCastleContainer);

procedure ViewportsSetTransparent(const Transparent: Boolean);

implementation

uses TypInfo, SysUtils,
  CastleVectors, CastleUtils, CastleUIControls, CastleControls,
  CastleGLUtils, CastleColors, CastleLog, CastleRenderContext, CastleKeysMouse;

{ global routines ------------------------------------------------------------ }

var
  Background: TCastleRectangleControl;

procedure AssignCameraAndNavigation(const Target, Source: TMyViewport);
var
  NavigationTypeStr: String;
begin
  Target.NavigationType := Source.NavigationType;

  {$warnings off} // consciously using deprecated Navigation, it makes sense for castle-model-viewer
  if (Target.Navigation <> nil) and
     (Source.Navigation <> nil) then
    Target.Navigation.Assign(Source.Navigation)
  else
  if (Target.Navigation <> nil) or
     (Source.Navigation <> nil) then
  begin
  {$warnings on}
    NavigationTypeStr := GetEnumName(TypeInfo(TNavigationType), Ord(Target.NavigationType));
    WritelnLog('Both Source and Target have the same NavigationType, but only one of them has non-nil Navigation', [
      NavigationTypeStr
    ]);
  end;

  Target.Camera.Assign(Source.Camera);
end;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TCastleWindow; MainViewport: TMyViewport);

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
  W, H: Single;
begin
  case ViewportsConfig of
    vc1: ;
    vc2Horizontal:
      begin
        W := Window.Container.UnscaledWidth / 2;
        H := Window.Container.UnscaledHeight;

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
        W := Window.Container.UnscaledWidth / 2;
        H := Window.Container.UnscaledHeight / 2;

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

procedure InitializeViewportsAndDefaultNavigation(ViewportClass: TViewportClass);

  procedure InitializeDefaultNavigation(const V: TMyViewport);
  begin
    { This configuration of navigations somewhat corresponds to CGE editor
      navigations in castleviewport_design_navigation.inc .

      However with some differences: in castle-model-viewer, left mouse button is free to use,
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
      See https://github.com/castle-engine/castle-model-viewer/issues/46 . }
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
    ExtraViewports[I].MainScene := MainViewport.MainScene;

    // Usign deprecated AutoCamera, castle-model-viewer needs this now to have
    // X3D Viewpoint animation affect camera.
    {$warnings off}
    { We will explicitly initialize camera and navigation.
      This also prevents the AutoCamera mechanism from overriding
      our camera set by AssignCameraAndNavigation. }
    ExtraViewports[I].AutoCamera := false;
    {$warnings on}

    ExtraViewports[I].AutoNavigation := false;

    InitializeDefaultNavigation(ExtraViewports[I]);
  end;

  InitializeDefaultNavigation(MainViewport);

  Background := TCastleRectangleControl.Create(nil);
  Background.FullSize := true;
  Background.Color := Gray;
end;

procedure InitializeViewportsMainScene(const NewMainScene: TCastleScene);
var
  I: Integer;
begin
  MainViewport.MainScene := NewMainScene;
  for I := 0 to High(ExtraViewports) do
    ExtraViewports[I].MainScene := NewMainScene;

  // change also MainViewport.Items, this is shared with all ExtraViewports
  MainViewport.Items.Add(NewMainScene);
  // Usign deprecated MainScene, castle-model-viewer needs this now to have X3D
  // Viewpoint animation affect camera, to have X3D Background node affect
  // background and likely more.
  {$warnings off}
  MainViewport.Items.MainScene := NewMainScene;
  {$warnings on}
end;

procedure ViewportsRender(const Container: TCastleContainer);
var
  I: Integer;
const
  Visible: array [TViewportsConfig] of Integer = (0, 1, 3);
begin
  if ViewportsConfig <> vc1 then
    Container.RenderControl(Background, Container.PixelsRect);
  for I := 0 to Visible[ViewportsConfig] - 1 do
    Container.RenderControl(ExtraViewports[I], Container.PixelsRect);
  Container.RenderControl(MainViewport, Container.PixelsRect);
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

initialization
finalization
  DoFinalization;
end.
