{
  Copyright 2010-2010 Michalis Kamburelis.

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

{ }
unit V3DSceneViewports;

interface

uses KambiSceneManager, GLWindow, Cameras;

type
  TViewportsConfig = (vc1, vc2Horizontal, vc4);
  TViewportClass = class of TKamViewport;
const
  ViewportsConfigNames: array [TViewportsConfig] of string =
  ('_1 viewport',
   '_2 horizontal viewports',
   '_4 viewports'
  );

var
  ViewportsConfig: TViewportsConfig;

  { Custom viewports. Also SceneManager is the first viewport. }
  Viewports: array [0..2] of TKamViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TGLUIWindow; SceneManager: TKamSceneManager);

{ Copy all camera settings from Source.Camera to Target.Camera.
  If CreateIfNeeded then Target will be created if @nil (otherwise
  it will be left as @nil). }
procedure AssignCamera(Target, Source: TKamAbstractViewport;
  SceneManager: TKamSceneManager; const CreateIfNeeded: boolean);

procedure ResizeViewports(Window: TGLUIWindow; SceneManager: TKamSceneManager);

{ Copy NavigationType to (existing) viewports cameras. }
procedure ViewportsSetNavigationType(const NavigationType: TCameraNavigationType);

procedure InitializeViewports(ViewportClass: TViewportClass);

implementation

uses VectorMath, SysUtils, KambiUtils, GL, UIControls;

{ TBackground ---------------------------------------------------------------- }

type
  TBackground = class(TUIControl)
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TBackground.DrawStyle: TUIControlDrawStyle;
begin
  { 3D, because we want to be drawn before other 3D objects }
  Result := ds3D;
end;

procedure TBackground.Draw;
begin
  glPushAttrib(GL_COLOR_BUFFER_BIT);
    glClearColor(0.5, 0.5, 0.5, 1); // saved by GL_COLOR_BUFFER_BIT
    glClear(GL_COLOR_BUFFER_BIT);
  glPopAttrib;
end;

{ global routines ------------------------------------------------------------ }

var
  Background: TBackground;

procedure AssignCamera(Target, Source: TKamAbstractViewport;
  SceneManager: TKamSceneManager; const CreateIfNeeded: boolean);
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  if Target.Camera = nil then
  begin
    if CreateIfNeeded then
      Target.Camera := Target.CreateDefaultCamera(Target) else
      Exit;
  end;

  SceneManager.MainScene.CameraFromNavigationInfo(Target.Camera,
    SceneManager.Items.BoundingBox);
  SceneManager.MainScene.CameraFromViewpoint(Target.Camera,
    false, false);

  Source.Camera.GetView(Pos, Dir, Up, GravityUp);
  Target.Camera.SetView(Pos, Dir, Up, GravityUp);

  if (Source.Camera is TUniversalCamera) and
     (Target.Camera is TUniversalCamera) then
    TUniversalCamera(Target.Camera).NavigationType :=
      TUniversalCamera(Source.Camera).NavigationType;
end;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TGLUIWindow; SceneManager: TKamSceneManager);

  procedure AddViewport(Viewport: TKamViewport);
  begin
    Viewport.SceneManager := SceneManager;
    Viewport.FullSize := false;
    Window.Controls.InsertIfNotExists(0, Viewport);
  end;

var
  I: Integer;
  OldValue: TViewportsConfig;
begin
  if ViewportsConfig <> Value then
  begin
    OldValue := ViewportsConfig;
    ViewportsConfig := Value;
    { After a first change of ViewportsConfig,
      use AlwaysApplyProjection = true, otherwise restoring vc1 would
      not restore the projection. AlwaysApplyProjection is simply safer for
      multiple viewports. }
    SceneManager.AlwaysApplyProjection := true;
    SceneManager.FullSize := ViewportsConfig = vc1;
    if ViewportsConfig = vc1 then
      Window.Controls.Remove(Background) else
      Window.Controls.AddIfNotExists(Background);
    case ViewportsConfig of
      vc1:
        begin
          { make sure glViewport is also restored }
          glViewport(0, 0, Window.Width, Window.Height);
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
            AssignCamera(Viewports[0], SceneManager, SceneManager, true);
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
                AssignCamera(Viewports[0], SceneManager, SceneManager, true);
                AssignCamera(Viewports[1], SceneManager, SceneManager, true);
                AssignCamera(Viewports[2], SceneManager, SceneManager, true);
              end;
            vc2Horizontal:
              begin
                AssignCamera(Viewports[1], SceneManager, SceneManager, true);
                AssignCamera(Viewports[2], Viewports[0], SceneManager, true);
              end;
          end;
        end;
      else raise EInternalError.Create('ViewportsConfig?');
    end;
    ResizeViewports(Window, SceneManager);
  end;
end;

procedure ResizeViewports(Window: TGLUIWindow; SceneManager: TKamSceneManager);
var
  W, H: Cardinal;
begin
  case ViewportsConfig of
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

procedure ViewportsSetNavigationType(const NavigationType: TCameraNavigationType);
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    if (Viewports[I].Camera <> nil) and
       (Viewports[I].Camera is TUniversalCamera) then
      TUniversalCamera(Viewports[I].Camera).NavigationType := NavigationType;
end;

procedure InitializeViewports(ViewportClass: TViewportClass);
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    Viewports[I] := ViewportClass.Create(nil);
  Background := TBackground.Create(nil);
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
