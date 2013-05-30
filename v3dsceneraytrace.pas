{
  Copyright 2003-2013 Michalis Kamburelis.

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

{ Simple unit that allows to incrementally display raytracer result
  in TCastleWindowBase window. }
unit V3DSceneRaytrace;

interface

uses GL, GLU, GLExt, CastleWindow, CastleVectors, X3DNodes,
  CastleFilesUtils, CastleStringUtils, CastleSceneCore;

const
  DefaultRaytracerDepth = 3;

{ Ray-tracer. }
procedure RaytraceToWin(Window: TCastleWindowBase;
  BaseLights: TLightInstancesList;
  Scene: TCastleSceneCore;
  const CamPosition, CamDir, CamUp: TVector3Single;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  const SceneBGColor: TVector3Single);

implementation

uses CastleRayTracer, CastleWindowModes, CastleGLUtils, CastleImages, SysUtils,
  CastleUtils, CastleMessages, CastleGLImages, Classes, V3DSceneStatus,
  CastleURIUtils;

const
  DefaultPrimarySamplesCount = 1;
  DefaultNonPrimarySamplesCount = 4;
  DefaultRRoulContinue = 0.5;

type
  { Callbacks data. Window.UserData contains a PCallData pointer
    during the ray-tracing process (so callbacks like DrawDone
    and DrawWorking can access it, without using any dirty global variables).
    And PixelsMadeNotify gets Window in the Data parameter,
    so PixelsMadeNotify callback can also access it. }
  TCallData = record
    { The screen save of the OpenGL rendering,
      that is successively overwritten by ray-traced image.
      This way we can always display this image to show OpenGL rendering
      partially covered by the (ready part) of ray-traced image. }
    Image: TCastleImage;
    { For PixelsMadeNotify. }
    RowsMadeCount: integer;
    { During and after raytracing, user can exit with Escape. }
    Quit: boolean;
    { Ray-tracer statistics. }
    Stats: TStringList;
  end;
  PCallData = ^TCallData;

type
  BreakRaytracing = class(TCodeBreaker);

{ -------------------------------------------------------------------------
  Window calbacks after rendering (when Image is ready). }

procedure DrawDone(Window: TCastleWindowBase);
var
  D: PCallData;
begin
  D := PCallData(Window.UserData);

  { Although usually the Image will cover the whole window (as it was
    created with the size = window size), we have to clear screen first
    in case user resized the window to make it larger. }
  glClear(GL_COLOR_BUFFER_BIT);
  SetWindowPosZero;
  ImageDraw(D^.Image);
  DrawStatus(D^.Stats);
end;

{ -------------------------------------------------------------------------
  Window calbacks while rendering (when Image is only partially ready).
  Remember that Window.Width/Height do not have to be equal to
  Image.Width/Height anymore, user could already resize our window. }

{ Display callback called when the window must be redrawn
  (e.g. because user resized the window, or covered and uncovered
  it by another window, and window manager requested the redraw). }
procedure DrawWorking(Window: TCastleWindowBase);
var
  D: PCallData;
begin
  D := PCallData(Window.UserData);

  glClear(GL_COLOR_BUFFER_BIT);
  SetWindowPosZero;
  ImageDraw(D^.Image);
end;

{ Callback when ray-tracing is in process.
  Given Data must be a TCastleWindowBase. }
procedure PixelsMadeNotify(PixelsMadeCount: Cardinal; Data: Pointer);
const
  RowsShowCount = 10;
var
  Window: TCastleWindowBase;
  D: PCallData;
  RowsMadeCount: Cardinal;
begin
  Window := TCastleWindowBase(Data);
  D := PCallData(Window.UserData);

  { Take this callback into account only when the row was just completed. }
  if PixelsMadeCount mod D^.Image.Width <> 0 then Exit;
  RowsMadeCount := PixelsMadeCount div D^.Image.Width;
  D^.RowsMadeCount := RowsMadeCount;

  Window.Caption := Format('view3dscene - Ray Tracing - %d%%',
    [Round(100 * RowsMadeCount / D^.Image.Height)]);

  { To be fast (and this has to be fast to not slow down the ray-tracing)
    we update the screen ony after each RowsShowCount rows. }
  if (RowsMadeCount mod RowsShowCount) = 0 then
  begin
    Window.PostRedisplay;
    Application.ProcessAllMessages;
    if D^.Quit then raise BreakRaytracing.Create;
  end;
end;

{ menu things ---------------------------------------------------------------- }

procedure MenuClick(Window: TCastleWindowBase; Item: TMenuItem);
var
  D: PCallData;
  SaveURL: string;
  ImgFormat: TImageFormat;
begin
  D := PCallData(Window.UserData);

  case Item.IntData of
    10: begin
          { This may be called only when rendering is done }
          SaveURL := ApplicationName + '_rt.png';
          if Window.FileDialog('Save image', SaveURL, false,
            SaveImage_FileFilters) then
          begin
            { Determine ImgFormat exactly the same like SaveImage() does. }
            if MimeTypeToImageFormat(URIMimeType(SaveURL), false, true, ImgFormat) and
              (ImgFormat = ifRGBE) then
              MessageOK(Window,
                'Note: When saving raytraced image from view3dscene to ' +
                'RGBE file format, you will *not* get image with perfect ' +
                'RGB+Exponent precision. ' +
                'That''s because image is already stored in memory in RGB ' +
                '(8 bits per component) format (this was required to quickly display ' +
                'image in OpenGL) so any precision (beyond 8-bits) is already lost. ' +
                'Use rayhunter if you want to have RGBE image with precise colors.',
                taLeft);

            SaveImage(D^.Image, SaveURL);
          end;
        end;
    20: D^.Quit := true;
  end;
end;

function CreateMainMenuWorking: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Raytracer working');
  M := TMenu.Create('_Raytracer working');
    M.Append(TMenuItem.Create('_Cancel', 20, CharEscape));
    Result.Append(M);
end;

function CreateMainMenuDone: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Raytracer done');
  M := TMenu.Create('_Raytracer done');
    M.Append(TMenuItem.Create('_Save output to file',   10, CtrlS));
    M.Append(TMenuItem.Create('_Return to main screen', 20, CharEscape));
    Result.Append(M);
end;

{ ----------------------------------------------------------------------------- }

procedure RaytraceToWin(Window: TCastleWindowBase;
  BaseLights: TLightInstancesList;
  Scene: TCastleSceneCore;
  const CamPosition, CamDir, CamUp: TVector3Single;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  const SceneBGColor: TVector3Single);
var
  SavedMode: TGLMode;
  CallData: TCallData;
  RaytracerKind: TRaytracerKind;
  RaytraceDepth, PathtraceNonPrimarySamples: Cardinal;
  MainMenuDone, MainMenuWorking: TMenu;
  RayTracer: TRayTracer;
begin
  { get input from user }
  case LoCase(MessageChar(Window,
      'Which ray tracer do you want to use  ?'+nl+
      '[C] : Classic (Whitted-style) ray tracer'+nl+
      '[P] : Path tracer' +nl+
      '[Escape] : Cancel',
      ['c','C', 'p', 'P', CharEscape], 'Press [C] or [P] or [Escape]', taLeft)) of
    'c': RaytracerKind := rtkClassic;
    'p': RaytracerKind := rtkPathTracer;
    CharEscape: Exit;
  end;

  RaytraceDepth := MessageInputCardinal(Window,
    'Ray-tracer depth (maximum for classic, minimum for path tracer):',
    taLeft, DefaultRaytracerDepth);
  if RaytracerKind = rtkPathTracer then
    PathtraceNonPrimarySamples := MessageInputCardinal(Window,
      'How many samples (non-primary) per pixel ?',
      taLeft, DefaultNonPrimarySamplesCount);

  MainMenuDone := nil;
  MainMenuWorking := nil;
  RayTracer := nil;
  CallData.Image := nil;
  SavedMode := nil;
  CallData.Stats := nil;
  try
    MainMenuDone := CreateMainMenuDone;
    MainMenuWorking := CreateMainMenuWorking;

    CallData.Image := Window.SaveAlignedScreen;

    { switch to our mode }
    SavedMode := TGLMode.CreateReset(Window, GL_ENABLE_BIT, false,
      @DrawWorking, @Resize2D, @NoClose);

    Window.UserData := @CallData;
    Window.MainMenu := MainMenuWorking;
    Window.OnMenuClick := @MenuClick;
    CallData.Quit := false;
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_FOG);
    glLoadIdentity;
    Window.EventResize; { init our projection }

    try
      case RaytracerKind of
        rtkClassic:
          begin
            RayTracer := TClassicRayTracer.Create;
            TClassicRayTracer(RayTracer).InitialDepth := RaytraceDepth;
            TClassicRayTracer(RayTracer).FogNode := Scene.FogStack.Top;
            TClassicRayTracer(RayTracer).BaseLights := BaseLights;
          end;
        rtkPathTracer:
          begin
            RayTracer := TPathTracer.Create;
            TPathTracer(RayTracer).MinDepth := RaytraceDepth;
            TPathTracer(RayTracer).RRoulContinue := DefaultRRoulContinue;
            TPathTracer(RayTracer).PrimarySamplesCount := DefaultPrimarySamplesCount;
            TPathTracer(RayTracer).NonPrimarySamplesCount := PathtraceNonPrimarySamples;
            TPathTracer(RayTracer).DirectIllumSamplesCount := 1;
         end;
      end;

      { For ray-tracing, we create and use OctreeVisibleTriangles.

        Although OctreeDynamicCollisions (usually already prepared for Scene,
        when CollisionChecking is active) has quite excellent performance,
        and ray-tracer can work with it.
        But OctreeCollidableTriangles has only *collidable* geometry ---
        while we want only *visible* geometry for ray-tracer.
        When using Collision node, these may be two different things. }
      Scene.Spatial := Scene.Spatial + [ssVisibleTriangles];
      try
        RayTracer.Image := CallData.Image;
        RayTracer.Octree := Scene.OctreeVisibleTriangles;
        RayTracer.CamPosition := CamPosition;
        RayTracer.CamDirection := CamDir;
        RayTracer.CamUp := CamUp;
        RayTracer.PerspectiveView := PerspectiveView;
        RayTracer.PerspectiveViewAngles := PerspectiveViewAngles;
        RayTracer.OrthoViewDimensions := OrthoViewDimensions;
        RayTracer.SceneBGColor := SceneBGColor;
        RayTracer.PixelsMadeNotifier := @PixelsMadeNotify;
        RayTracer.PixelsMadeNotifierData := Window;

        CallData.Stats := TStringList.Create;
        RayTracer.ExecuteStats(CallData.Stats);
      finally Scene.Spatial := Scene.Spatial - [ssVisibleTriangles] end;

      CallData.Stats.Append('');
      CallData.Stats.Append('<font color="#FFFFFF">Press Escape to return to normal 3D view.</font>');

      { Display the rendered image, wait for Escape.
        We call PostRedisplay to make sure to display the whole image
        (otherwise the image may not be fully displayed yet,
        because of RowsShowCount mechanism). }
      Window.PostRedisplay;
      Window.Caption := 'view3dscene - Ray Tracing - done';
      Window.OnDraw := @DrawDone;
      Window.MainMenu := MainMenuDone;
      repeat Application.ProcessMessage(true, true) until CallData.Quit;

    except on BreakRaytracing do ; end;
  finally
    FreeAndNil(SavedMode);
    FreeAndNil(CallData.Image);
    FreeAndNil(CallData.Stats);
    FreeAndNil(MainMenuWorking);
    FreeAndNil(MainMenuDone);
    FreeAndNil(RayTracer);
  end;
end;

end.
