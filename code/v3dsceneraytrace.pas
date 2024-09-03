{
  Copyright 2003-2024 Michalis Kamburelis.

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

{ Simple unit that allows to incrementally display raytracer result
  in TCastleWindow window. }
unit V3DSceneRaytrace;

interface

uses CastleVectors, X3DNodes, CastleColors, CastleProjection,
  CastleFilesUtils, CastleStringUtils, CastleSceneCore, CastleUIControls;

const
  DefaultRaytracerDepth = 3;

{ Ray-tracer. }
procedure RaytraceToWin(
  BaseLights: TLightInstancesList;
  Scene: TCastleSceneCore;
  const CamPosition, CamDir, CamUp: TVector3;
  const Projection: TProjection;
  const SceneBGColor: TCastleColor);

implementation

// TODO: Using deprecated CastleWindowModes; we should migrate to use TCastleView
{$warnings off}
uses SysUtils, Classes, CastleWindow, CastleRayTracer, CastleWindowModes,
  CastleGLUtils, CastleImages, CastleUtils, CastleMessages, CastleGLImages,
  CastleUriUtils, CastleKeysMouse, CastleRenderContext,
  CastleInternalTriangleOctree, CastleShapes,
  V3DSceneStatus, V3DSceneWindow;
{$warnings on}

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
  end;
  PCallData = ^TCallData;

type
  (*Class to be raised (like an exception) and caught, to exit from some
    code blocks.

    When deriving new TCodeBreaker descendant, specify always where it's caught.
    It should be caught and silenced (without causing any errors) in appropriate
    place. This way doing @code(raise BreakXxx.Create) is a way to exit from
    given code part.

    This class does not inherit from @code(Exception) because it does not
    signal some errorneous situation (it must always be caught and silenced),
    also it doesn't need a Message property. *)
  TCodeBreaker = class(TObject);

  BreakRaytracing = class(TCodeBreaker);

{ TRayTracerImage ----------------------------------------------------------- }

type
  TRayTracerImage = class(TCastleUserInterface)
  private
    Image: TCastleImage;
  public
    procedure Render; override;
  end;

procedure TRayTracerImage.Render;
var
  GLImage: TDrawableImage;
begin
  { Although usually the Image will cover the whole window (as it was
    created with the size = window size), we have to clear screen first
    in case user resized the window to make it larger. }
  RenderContext.Clear([cbColor], Black);
  GLImage := TDrawableImage.Create(Image, false, false);
  try
    GLImage.Draw(0, 0);
  finally FreeAndNil(GLImage) end;
end;

{ Callback when ray-tracing is in process. }
procedure PixelsMadeNotify(PixelsMadeCount: Cardinal; Data: Pointer);
const
  RowsShowCount = 10;
var
  D: PCallData;
  RowsMadeCount: Cardinal;
begin
  D := PCallData(Window.UserData);

  { Take this callback into account only when the row was just completed. }
  if PixelsMadeCount mod D^.Image.Width <> 0 then Exit;
  RowsMadeCount := PixelsMadeCount div D^.Image.Width;
  D^.RowsMadeCount := RowsMadeCount;

  Window.Caption := Format('castle-model-viewer - Ray Tracing - %d%%',
    [Round(100 * RowsMadeCount / D^.Image.Height)]);

  { To be fast (and this has to be fast to not slow down the ray-tracing)
    we update the screen ony after each RowsShowCount rows. }
  if (RowsMadeCount mod RowsShowCount) = 0 then
  begin
    Window.Invalidate;
    Application.ProcessAllMessages;
    if D^.Quit then raise BreakRaytracing.Create;
  end;
end;

{ menu things ---------------------------------------------------------------- }

{ Save rendered image. This may be called only when rendering is done. }
procedure EventSave;
var
  D: PCallData;
  SaveUrl: String;
begin
  D := PCallData(Window.UserData);
  SaveUrl := ApplicationName + '_rt.png';
  if Window.FileDialog('Save image', SaveUrl, false, SaveImage_FileFilters) then
    SaveImage(D^.Image, SaveUrl);
end;

procedure EventEscape;
var
  D: PCallData;
begin
  D := PCallData(Window.UserData);
  D^.Quit := true;
end;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);
begin
  case Item.IntData of
    10: EventSave;
    20: EventEscape;
  end;
end;

{$ifdef LCLCarbon}
procedure PressWorking(Container: TCastleContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(CharEscape) then EventEscape;
end;

procedure PressDone(Container: TCastleContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(CtrlS) then EventSave;
  if Event.IsKey(CharEscape) then EventEscape;
end;
{$endif}

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

{ TRayTracerStatus -------------------------------------------------------- }

type
  TRayTracerStatus = class(TStatusText)
  private
    Stats: TStringList;
  protected
    procedure CalculateText; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TRayTracerStatus.Create(AOwner: TComponent);
begin
  inherited;
  Stats := TStringList.Create;
end;

destructor TRayTracerStatus.Destroy;
begin
  FreeAndNil(Stats);
  inherited;
end;

procedure TRayTracerStatus.CalculateText;
begin
  inherited;
  Text.Assign(Stats);
end;

{ ----------------------------------------------------------------------------- }

procedure RaytraceToWin(
  BaseLights: TLightInstancesList;
  Scene: TCastleSceneCore;
  const CamPosition, CamDir, CamUp: TVector3;
  const Projection: TProjection;
  const SceneBGColor: TCastleColor);
var
  SavedMode: TGLMode;
  CallData: TCallData;
  RaytracerKind: TRaytracerKind;
  RaytraceDepth, PathtraceNonPrimarySamples: Cardinal;
  MainMenuDone, MainMenuWorking: TMenu;
  RayTracer: TRayTracer;
  StatusText: TRayTracerStatus;
  ImageControl: TRayTracerImage;
  OctreeVisibleTriangles: TTriangleOctree;
begin
  { get input from user }
  case MessageChoice(Window,
      'Which ray tracer do you want to use?',
      ['Classic (Whitted)', 'Path tracer', 'Cancel (Esc)'],
      ['c', 'p', CharEscape]) of
    'c': RaytracerKind := rtkClassic;
    'p': RaytracerKind := rtkPathTracer;
    CharEscape: Exit;
  end;

  RaytraceDepth := MessageInputCardinal(Window,
    'Ray-tracer depth (maximum for classic, minimum for path tracer):',
    DefaultRaytracerDepth);
  if RaytracerKind = rtkPathTracer then
    PathtraceNonPrimarySamples := MessageInputCardinal(Window,
      'How many samples (non-primary) per pixel ?',
      DefaultNonPrimarySamplesCount);

  MainMenuDone := nil;
  MainMenuWorking := nil;
  RayTracer := nil;
  CallData.Image := nil;
  SavedMode := nil;
  try
    MainMenuDone := CreateMainMenuDone;
    MainMenuWorking := CreateMainMenuWorking;

    CallData.Image := Window.SaveScreen;

    { switch to our mode }
    SavedMode := TGLMode.CreateReset(Window);

    StatusText := TRayTracerStatus.Create(Window);
    Window.Controls.InsertFront(StatusText);

    ImageControl := TRayTracerImage.Create(Window);
    ImageControl.Image := CallData.Image;
    Window.Controls.InsertBack(ImageControl);

    Window.UserData := @CallData;
    { Lazarus Carbon widgetset (used by CastleWindow LCL backend on Mac OS X)
      doesn't handle MainMenu changes nicely (it would fire ray-tracing
      twice on Ctrl+R, since we rebuild the menu inside the menu handler).
      For now, just handle Escape and CtrlS specially on Mac OS X. }
    {$ifdef LCLCarbon}
    {$fatal Compilation for LCL+Carbon is not supported now, since it's not tested.}
    //Window.OnPress := @PressWorking; // this will not compile with latest CGE
    {$else}
    Window.MainMenu := MainMenuWorking;
    Window.OnMenuClick := @MenuClick;
    {$endif}
    CallData.Quit := false;

    try
      case RaytracerKind of
        rtkClassic:
          begin
            RayTracer := TClassicRayTracer.Create;
            TClassicRayTracer(RayTracer).InitialDepth := RaytraceDepth;
            TClassicRayTracer(RayTracer).FogNode := Scene.FogStack.Top;
            TClassicRayTracer(RayTracer).GlobalLights := BaseLights;
            TClassicRayTracer(RayTracer).OwnsGlobalLights := false;
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
        and ray-tracer could work with it too.

        But OctreeCollidableTriangles has only *collidable* geometry ---
        while we want only *visible* geometry for ray-tracer.
        When using Collision node, these may be two different things. }
      OctreeVisibleTriangles := CreateOctreeVisibleTrianglesForScene(Scene);
      try
        RayTracer.Image := CallData.Image;
        RayTracer.Octree := OctreeVisibleTriangles;
        RayTracer.CamPosition := CamPosition;
        RayTracer.CamDirection := CamDir;
        RayTracer.CamUp := CamUp;
        RayTracer.Projection := Projection;
        RayTracer.SceneBGColor := SceneBGColor.XYZ;
        RayTracer.Background := Scene.BackgroundStack.Top;
        RayTracer.PixelsMadeNotifier := @PixelsMadeNotify;

        RayTracer.ExecuteStats(StatusText.Stats);
      finally FreeAndNil(OctreeVisibleTriangles) end;

      StatusText.Stats.Append('');
      StatusText.Stats.Append('<font color="#FFFFFF">Press Escape to return to normal 3D view.</font>');

      { Display the rendered image, wait for Escape.
        We call Invalidate to make sure to display the whole image
        (otherwise the image may not be fully displayed yet,
        because of RowsShowCount mechanism). }
      Window.Invalidate;
      Window.Caption := 'castle-model-viewer - Ray Tracing - done';
      {$ifdef LCLCarbon}
      {$fatal Compilation for LCL+Carbon is not supported now, since it's not tested.}
      //Window.OnPress := @PressDone;
      {$else}
      Window.MainMenu := MainMenuDone;
      {$endif}
      repeat Application.ProcessMessage(true, true) until CallData.Quit;

    except on BreakRaytracing do ; end;
  finally
    FreeAndNil(SavedMode);
    FreeAndNil(StatusText);
    FreeAndNil(CallData.Image);
    FreeAndNil(MainMenuWorking);
    FreeAndNil(MainMenuDone);
    FreeAndNil(RayTracer);
  end;
end;

end.
