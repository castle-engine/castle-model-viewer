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

uses KambiSceneManager, GLWindow;

type
  TViewportsConfig = (vc1, vc2Horizontal, vc4);
const
  ViewportsConfigNames: array [TViewportsConfig] of string =
  ('One viewport',
   'Two horizontal viewports',
   'Four viewports'
  );

var
  ViewportsConfig: TViewportsConfig;

  { Custom viewports. Also SceneManager is the first viewport. }
  Viewports: array [0..2] of TKamViewport;

procedure SetViewportsConfig(const Value: TViewportsConfig;
  Window: TGLUIWindow; SceneManager: TKamSceneManager);
procedure ResizeViewports(Window: TGLUIWindow; SceneManager: TKamSceneManager);

implementation

uses SysUtils, KambiUtils, GL, UIControls;

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
begin
  if ViewportsConfig <> Value then
  begin
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
        end;
      vc4:
        begin
          AddViewport(Viewports[0]);
          AddViewport(Viewports[1]);
          AddViewport(Viewports[2]);
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
        Viewports[0].Bottom := 0;
        Viewports[0].Width := W - 1;
        Viewports[0].Height := H - 1;

        Viewports[1].Left := W + 1;
        Viewports[1].Bottom := H + 1;
        Viewports[1].Width := W - 1;
        Viewports[1].Height := H - 1;

        Viewports[2].Left := 0;
        Viewports[2].Bottom := 0;
        Viewports[2].Width := W - 1;
        Viewports[2].Height := H - 1;
      end;
  end;
end;

procedure DoInitialization;
var
  I: Integer;
begin
  for I := 0 to High(Viewports) do
    Viewports[I] := TKamViewport.Create(nil);
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

initialization
  DoInitialization;
finalization
  DoFinalization;
end.
