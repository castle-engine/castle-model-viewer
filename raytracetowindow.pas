{
  Copyright 2003-2005 Michalis Kamburelis.

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

{ Simple unit that allows to incrementally display raytracer result
  in TGLWindow window. }

unit RaytraceToWindow;

{$I openglmac.inc}

interface

uses GL, GLU, GLExt, GLWindow, VectorMath, VRMLNodes,
  KambiFilesUtils, KambiStringUtils, VRMLScene, VRMLHeadLight;

const
  DEF_RAYTRACE_DEPTH = 3;
  DEF_PRIMARY_SAMPLES_COUNT = 1;
  DEF_NON_PRIMARY_SAMPLES_COUNT = 4;

{ Ray-tracer.

  We take HeadLightExists, HeadLight instead of using Scene.HeadLight.
  Reason: we want to honor current view3dscene headlight setting,
  this allows user to turn headlight on (by menu item) to add headlight
  to both OpenGL and ray-tracer. }
procedure RaytraceToWin(glwin: TGLWindow;
  Scene: TVRMLScene;
  HeadLightExists: boolean; HeadLight: TVRMLHeadLight;
  const CamPosition, CamDir, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single);

implementation

uses VRMLRayTracer, GLWinModes, KambiGLUtils, Images, SysUtils, KambiUtils,
  GLWinMessages, GLImages;

type
  { callbacks data. Callbacki TGLWindow moga osiagnac to Data przez
    PCallData(glwin.Userdata), callback RowMadeNotify dostaje glwin w
    parametrze Data (wiec musi zrobic PCallData(TGLWindow(Data).UserData) ) }
  TCallData = record
    { Image to na poczatku SaveScreen sceny wyrenderowanej OpenGLem
      i raytracer stopniowo go zapisuje. Musimy pamietac w tym obrazku
      poczatkowa scene OpenGLa bo jesli user np. na chwile przelaczy sie do
      innego okienka a potem wroci do nas to chcemy umiec odmalowac sobie
      cale okienko = czesciowy wynik raytracera + czesciowa resztka oryginalnego
      renderingu przy pomocy OpenGLa. To jest wykorzystywane w DrawRaytracing. }
    Image: TImage;
    { RowMadeNotify bedzie uaktualnialo ta zmienna. }
    RowsMadeCount: integer;
    { KeyDown, zarowno w czasie raytracingu jak i juz po jego zakonczeniu,
      bedzie moglo uaktualnic ta zmienna na true }
    UserWantsToQuit: boolean;
  end;
  PCallData = ^TCallData;

type
  BreakRaytracing = class(TCodeBreaker);

function CaptionBegin: string;
begin
 result := ProgramName+ ' - ray tracer';
end;

{ -------------------------------------------------------------------------
  glwindow calbacks AFTER rendering (when Image is ready) }

procedure DrawRaytraced(glwin: TGLWindow);
begin
 {zrob na poczatku clear na wypadek gdyby user zresizowal okienko - wtedy
  dobrze bedzie narysowac czarne tlo tam gdzie nie bedzie obrazka }
 glClear(GL_COLOR_BUFFER_BIT);
 glRasterPos2i(0, 0);
 ImageDraw(PCallData(glwin.UserData)^.Image);
end;

{ -------------------------------------------------------------------------
  calbacks WHILE rendering (when Image is only partially ready).
  Remember to NOT trust glwin.Width/Height here - they don't have to
  be equal to Image.Width/Height here, user could already resize our window) }

procedure DrawRaytracing(glwin: TGLWindow);
var D: PCallData;
begin
 {this DrawRaytracing callback will be called only when user requested
  refreshing our window using window manager (e.g. resized
  our window using window borders or temporary covered our window
  with other window)}

 {switch our drawing to the back buffer. We have to do it because TGLWindow
  will call SwapBuffers after calling OnDraw (and it's good) but we changed
  glDrawBuffer to GL_FRONT for drawing in RowsMadeNotifier.
  We could use more elegant solution : switch glDrawBuffer to GL_FRONT
  only in RowMadeNotify (in Push/PopAttrib clause) BUT we want
  RowMadeNotify to work as fast as possible - so we don't want to switch
  drawing buffers there}

 glPushAttrib(GL_COLOR_BUFFER_BIT);
   { Actually I will always have glwin.DoubleBuffer = true.
     But I'm checking for it, just to be sure that I can compile
     view3dscene with "glwin.DoubleBuffer := false"
     for testing purposes. }
   if glwin.DoubleBuffer then glDrawBuffer(GL_BACK);

   D := PCallData(glwin.UserData);

   {zrob na poczatku clear na wypadek gdyby user zresizowal okienko - wtedy
    dobrze bedzie narysowac czarne tlo tam gdzie nie bedzie obrazka.}
   glClear(GL_COLOR_BUFFER_BIT);
   glRasterPos2i(0, 0);
   ImageDraw(D^.Image);
 glPopAttrib;
end;

procedure PixelsMadeNotify(PixelsMadeCount: Cardinal; Data: Pointer);
{ przekazane tu data to musi byc glwin: TGLWindow, a jego UserData musi
  byc wskaznikiem PCallData }
const ROWS_SHOW_COUNT = 5;
var D: PCallData;
    RowsMadeCount: Cardinal;
begin
 D := PCallData(TGLWindow(Data).UserData);

 { obchodzi nas tylko RowsMadeCount, ignorujemy ten callback gdy PixelsMadeCount
   nie oznacza ze wlasnie skonczylismy caly wiersz }
 if PixelsMadeCount mod D^.Image.Width <> 0 then Exit;
 RowsMadeCount := PixelsMadeCount div D^.Image.Width;
 D^.RowsMadeCount := RowsMadeCount;

 TGLWindow(Data).Caption := CaptionBegin +
   Format(' working %d%%',[Round(100*RowsMadeCount/D^.Image.Height)]);

 { zeby szybkosc rysowania wierszy nie obnizala znaczaco szybkosci renderingu
   nie wyswietlamy kazdego wiersza osobno i nie wywolujemy Application.ProcessMessage
   po kazdym wierszu. Robimy to wszystko co ROWS_SHOW_COUNT wierszy. }
 if (RowsMadeCount mod ROWS_SHOW_COUNT) = 0 then
 begin
  glRasterPos2i(0, RowsMadeCount-ROWS_SHOW_COUNT);
  ImageDrawRows(D^.Image, RowsMadeCount-ROWS_SHOW_COUNT, ROWS_SHOW_COUNT);
  glFlush;

  Application.ProcessAllMessages;
  if D^.UserWantsToQuit then raise BreakRaytracing.Create;
 end;
end;

{ menu things ---------------------------------------------------------------- }

procedure MenuCommand(glwin: TGLWindow; Item: TMenuItem);
var
  D: PCallData;
  SaveAsFilename: string;
  ImgFormat: TImageFormat;
begin
 D := PCallData(glwin.UserData);

 case Item.IntData of
  10: begin
        { This may be called only when rendering is done }
        SaveAsFilename := ProgramName + '_rt.png';
        if Glwin.FileDialog('Save image', SaveAsFilename, false,
          SaveImage_FileFilters) then
        begin
          { Determine ImgFormat exactly the same like SaveImage() does. }
          ImgFormat := FileExtToImageFormatDef(ExtractFileExt(SaveAsFilename), false, true,
            DefaultSaveImageFormat);

          if ImgFormat = ifRGBE then
            MessageOK(glwin,
              'Note: When saving raytraced image from view3dscene to ' +
              'RGBE file format, you will *not* get image with perfect ' +
              'RGB+Exponent precision. ' +
              'That''s because image is already stored in memory in RGB ' +
              '(8 bits per component) format (this was required to quickly display ' +
              'image in OpenGL) so any precision (beyond 8-bits) is already lost. ' +
              'Use rayhunter if you want to have RGBE image with precise colors.',
              taLeft);

          SaveImage(D^.Image, SaveAsFilename);
        end;
      end;
  20: D^.UserWantsToQuit := true;
 end;
end;

function CreateWhileRTMainMenu: TMenu;
var M: TMenu;
begin
 Result := TMenu.Create('RaytraceToWindow - Main menu while RT');
 M := TMenu.Create('_Raytracer working');
   M.Append(TMenuItem.Create('_Cancel', 20, CharEscape));
   Result.Append(M);
end;

function CreateAfterRTMainMenu: TMenu;
var M: TMenu;
begin
 Result := TMenu.Create('RaytraceToWindow - Main menu after RT');
 M := TMenu.Create('_Raytracer done');
   M.Append(TMenuItem.Create('_Save output to file',   10, CtrlS));
   M.Append(TMenuItem.Create('_Return to main screen', 20, CharEscape));
   Result.Append(M);
end;

{ ----------------------------------------------------------------------------- }

procedure RaytraceToWin(glwin: TGLWindow;
  Scene: TVRMLScene;
  HeadLightExists: boolean; HeadLight: TVRMLHeadLight;
  const CamPosition, CamDir, CamUp: TVector3Single;
  const ViewAngleDegX, ViewAngleDegY: Single;
  const SceneBGColor: TVector3Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single);
var SavedMode: TGLMode;
    CallData: TCallData;
    RealScreenWidth: Cardinal;

    RaytracerKind: TRaytracerKind;
    RaytraceDepth, PathtraceNonPrimarySamples: Cardinal;
    PathtraceRRoulContinue: Single;

    AfterRTMainMenu, WhileRTMainMenu: TMenu;
    RayTracer: TRayTracer;
begin
 { get input from user (BEFORE switching to our mode with ModeGLEnter) }
 case LoCase(MessageChar(glwin,
    'Which ray tracer do you want to use  ?'+nl+
    '[C] : Classic (Whitted-style) ray tracer'+nl+
    '[P] : Path tracer' +nl+
    '[Escape] : Cancel',
    ['c','C', 'p', 'P', CharEscape], 'Press [C] or [P] or [Escape]', taLeft)) of
  'c': RaytracerKind := rtkClassic;
  'p': RaytracerKind := rtkPathTracer;
  CharEscape: Exit;
 end;
 RaytraceDepth := MessageInputCardinal(glwin,
   'Ray-tracer depth (maximum for classic, minimum for path tracer):',
   taLeft, DEF_RAYTRACE_DEPTH);
 if RaytracerKind = rtkPathTracer then
 begin
  PathtraceNonPrimarySamples := MessageInputCardinal(glwin,
    'How many samples (non-primary) per pixel ?',
    taLeft, DEF_NON_PRIMARY_SAMPLES_COUNT);
  PathtraceRRoulContinue := 0.5; { TODO: ask user }
 end;

 AfterRTMainMenu := nil;
 WhileRTMainMenu := nil;
 RayTracer := nil;
 try
  AfterRTMainMenu := CreateAfterRTMainMenu;
  WhileRTMainMenu := CreateWhileRTMainMenu;

  { switch to our mode }
  SavedMode := TGLMode.Create(glwin, GL_ENABLE_BIT, false);
  try
   CallData.Image := glwin.SaveAlignedScreen(RealScreenWidth);
   try

    { set initial state }
    TGLWindowState.SetStandardNoCloseState(glwin, @DrawRaytracing, @Resize2D, @CallData, false,
      false, true, K_None, false, nil);
    glwin.MainMenu := WhileRTMainMenu;
    glwin.OnMenuCommand := @MenuCommand;
    CallData.UserWantsToQuit := false;
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_FOG);
    glLoadIdentity;
    glwin.EventResize; { init our projection }

    try
     {mechanizm wyswietlania w RowMadeNotify wymaga single bufora,
      dlatego przestawiamy sie tu na rysowanie zawsze w FRONT buffer}
     glPushAttrib(GL_COLOR_BUFFER_BIT);
     glDrawBuffer(GL_FRONT);
     try
      case RaytracerKind of
        rtkClassic:
          begin
            RayTracer := TClassicRayTracer.Create;
            TClassicRayTracer(RayTracer).InitialDepth := RaytraceDepth;
            TClassicRayTracer(RayTracer).FogNode := FogNode;
            TClassicRayTracer(RayTracer).FogDistanceScaling := FogDistanceScaling;
            TClassicRayTracer(RayTracer).HeadLightExists := HeadLightExists;
            if HeadLightExists then
              TClassicRayTracer(RayTracer).HeadLight := HeadLight.ActiveLight(CamPosition, CamDir);
          end;
        rtkPathTracer:
          begin
            RayTracer := TPathTracer.Create;
            TPathTracer(RayTracer).MinDepth := RaytraceDepth;
            TPathTracer(RayTracer).RRoulContinue := PathtraceRRoulContinue;
            TPathTracer(RayTracer).PrimarySamplesCount := DEF_PRIMARY_SAMPLES_COUNT;
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
        RayTracer.ViewAngleDegX := ViewAngleDegX;
        RayTracer.ViewAngleDegY := ViewAngleDegY;
        RayTracer.SceneBGColor := SceneBGColor;
        RayTracer.PixelsMadeNotifier := @PixelsMadeNotify;
        RayTracer.PixelsMadeNotifierData := glwin;

        RayTracer.Execute;
      finally Scene.Spatial := Scene.Spatial - [ssVisibleTriangles] end;

     finally glPopAttrib end;

     { wyswietlaj w petli wyrenderowany obrazek, czekaj na Escape.
       Potrzebny PostRedisplay bo na skutek ROWS_SHOW_COUNT koncowe
       wiersze moga nie zostac namalowane przez RowDoneNotify. }
     glwin.PostRedisplay;
     glwin.Caption := CaptionBegin+ ' done';
     glwin.OnDraw := @DrawRaytraced;
     glwin.MainMenu := AfterRTMainMenu;
     repeat Application.ProcessMessage(true) until CallData.UserWantsToQuit;

    except on BreakRaytracing do ; end;
   finally FreeAndNil(CallData.Image) end;
  finally SavedMode.Free end;

 finally
  WhileRTMainMenu.Free;
  AfterRTMainMenu.Free;
  FreeAndNil(RayTracer);
 end;
end;

end.
