{
  Copyright 2008-2023 Michalis Kamburelis.

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

{ Saving screenshots / movies. }
unit V3DSceneScreenShot;

interface

uses Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleTimeUtils, CastleParameters;

type
  EInvalidScreenShotURL = class(EInvalidParams);

  { One screenshot, that is one --screenshot or --screenshot-range option
    occurrence.

    Count describes how many actual image screen shots we have to do
    for this TScreenShot instance. For each Index between 0 and Count - 1,
    you can call UseTime and UseURL to actually get time of this
    capture and the image URL where to save this.

    Note that UseURL will do the substitution and actually
    increment the counter for URLs with @@counter(<padding>) (or %d) inside.
    So always call UseURL only once, and in correct order. }
  TScreenShot = class
    URLPattern: string;

    function Count: Cardinal; virtual; abstract;
    function UseTime(const Index: Integer): TFloatTime; virtual; abstract;
    function UseURL(const Index: Integer): string; virtual; abstract;

    { Call BeginCapture, EndCapture around saving all images for this
      TScreenShot instance. They allow for some tasks, e.g.
      this is the place when TRangeScreenShot may convert images to a single
      movie file.

      Success = @true for EndCapture means that we successfully did
      all the images, @false means that exception occurred and this
      is called only to finalize some things. }
    procedure BeginCapture; virtual;
    procedure EndCapture(Success: boolean); virtual;
  end;

  TSingleScreenShot = class(TScreenShot)
    Time: TFloatTime;

    function Count: Cardinal; override;
    function UseTime(const Index: Integer): TFloatTime; override;
    function UseURL(const Index: Integer): string; override;
  end;

  TRangeScreenShot = class(TScreenShot)
  private
    { Are we making single movie file? Will be calculated in BeginCapture. }
    SingleMovieFile: boolean;
    TemporaryImagesPattern: string;
    TemporaryImagesCounter: Cardinal;
  public
    TimeBegin, TimeStep: TFloatTime;
    FramesCount: Cardinal;

    function Count: Cardinal; override;
    function UseTime(const Index: Integer): TFloatTime; override;
    function UseURL(const Index: Integer): string; override;

    { @raises(EInvalidScreenShotURL When invalid URLPattern
        detected, like an image file with no @@counter(<padding>) or %d.) }
    procedure BeginCapture; override;
    procedure EndCapture(Success: boolean); override;
  end;

  TScreenShotList = class(specialize TObjectList<TScreenShot>)
  private
    ScreenShotCounter: Cardinal;
  public
    procedure BeginCapture;
  end;

var
  { List of screenshots to take.
    Created / destroyed in init / fini of this unit. }
  ScreenShotsList: TScreenShotList;

{ Are we currently making a screenshot? Just a shortcut for
  ScreenShotsList.Count <> 0. }
function MakingScreenShot: boolean;

implementation

uses SysUtils, CastleStringUtils, CastleFilesUtils,
  CastleVideos, CastleUriUtils, CastleLog;

function MakingScreenShot: boolean;
begin
  Result := ScreenShotsList.Count <> 0;
end;

function MakeURL(const URLPattern: string;
  var Counter: Cardinal): string;
var
  ReplacementsDone: Cardinal;
begin
  Result := FormatNameCounter(URLPattern, Counter, true, ReplacementsDone);
  if ReplacementsDone > 0 then
    Inc(Counter);
end;

{ TScreenShot ---------------------------------------------------------------- }

procedure TScreenShot.BeginCapture;
begin
end;

procedure TScreenShot.EndCapture(Success: boolean);
begin
end;

{ TSingleScreenShot ---------------------------------------------------------- }

function TSingleScreenShot.Count: Cardinal;
begin
  Result := 1;
end;

function TSingleScreenShot.UseTime(const Index: Integer): TFloatTime;
begin
  Result := Time;
end;

function TSingleScreenShot.UseURL(const Index: Integer): string;
begin
  Result := MakeURL(URLPattern, ScreenShotsList.ScreenShotCounter);
end;

{ TRangeScreenShot ---------------------------------------------------------- }

function TRangeScreenShot.Count: Cardinal;
begin
  Result := FramesCount;
end;

function TRangeScreenShot.UseTime(const Index: Integer): TFloatTime;
begin
  Result := TimeBegin + Index * TimeStep;
end;

function TRangeScreenShot.UseURL(const Index: Integer): string;
begin
  if SingleMovieFile then
    Result := MakeURL(TemporaryImagesPattern, TemporaryImagesCounter) else
    Result := MakeURL(URLPattern, ScreenShotsList.ScreenShotCounter);
end;

procedure TRangeScreenShot.BeginCapture;
var
  ReplacementsDone: Cardinal;
  TemporaryImagesPrefix: string;
begin
  { calculate SingleMovieFile }
  SingleMovieFile := FfmpegVideoMimeType(UriMimeType(URLPattern), true);

  if SingleMovieFile then
  begin
    { initialize TemporaryImagesPrefix, TemporaryImagesPattern,
      TemporaryImagesCounter }

    TemporaryImagesPrefix := GetTempFileNamePrefix;
    TemporaryImagesPattern := TemporaryImagesPrefix + '%d.png';
    TemporaryImagesCounter := 1;
  end;

  if not SingleMovieFile then
  begin
    { Check that we have some @counter(<padding>) (or %d) in our filename.
      Just call FormatNameCounter and ignore result,
      to get and check ReplacementsDone. }
    FormatNameCounter(URLPattern, -1, true, ReplacementsDone);
    if ReplacementsDone = 0 then
      raise EInvalidScreenShotURL.CreateFmt('--screenshot-range invalid filename "%s": not recognized as movie filename (so assuming image filename), and no @counter(<padding>) or %%d pattern found', [URLPattern]);
  end;

  // TODO: show it somehow in non-deprecated way
  // Progress.Init(Count, Format('Screenshot range from time %f (%d frames)',
  //   [TimeBegin, FramesCount]));
end;

procedure TRangeScreenShot.EndCapture(Success: boolean);
var
  Executable, OutputMovieFileName, TempFile: string;
  I: Integer;
begin
  if SingleMovieFile and Success then
  begin
    Executable := FfmpegExecutable(false);

    if Executable = '' then
    begin
      WritelnWarning('Video', Format('You must have "ffmpeg" program from ' +
        '[http://ffmpeg.mplayerhq.hu/] installed and available on $PATH to be able to ' +
        'create movie files". Leaving generated temporary images "%s"',
        [TemporaryImagesPattern]));
    end else
    begin
      OutputMovieFileName := UriToFilenameSafe(MakeURL(URLPattern, ScreenShotsList.ScreenShotCounter));

      FfmpegExecute(Executable,
        [ '-f', 'image2', '-i', TemporaryImagesPattern, '-y', '-qscale', '1', OutputMovieFileName ]);

      WritelnLog('Removing temporary image files "' + TemporaryImagesPattern + '" ...');
      TemporaryImagesCounter := 1;
      for I := 1 to FramesCount do
      begin
        TempFile := UriToFilenameSafe(MakeURL(TemporaryImagesPattern, TemporaryImagesCounter));
        CheckDeleteFile(TempFile, true);
      end;
      WritelnLog('Done removing temporary image files.');
    end;
  end;
end;

{ TScreenShotList ----------------------------------------------------------- }

procedure TScreenShotList.BeginCapture;
begin
  ScreenShotCounter := 1;
end;

{ unit initialization / finalization ---------------------------------------- }

initialization
  ScreenShotsList := TScreenShotList.Create(true);
finalization
  FreeAndNil(ScreenShotsList);
end.
