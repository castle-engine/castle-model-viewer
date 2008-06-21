{
  Copyright 2008 Michalis Kamburelis.

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

{ }
unit V3DSceneScreenShot;

interface

uses KambiUtils, KambiClassUtils, VRMLOpenGLRenderer;

{$define read_interface}

type
  { World time, used for animations.

    "Single" type is *not* enough for this.
    Proof: open rotate.kanim (from kambi_vrml_test_suite).
    Change "on display" time pass to 1000, wait a couple seconds
    (world time will reach a couple of thousands),
    change "on display" time pass back to 1.
    Result: with TAnimationTime as Single, animation becomes jagged.
    Reason: the precision loss of Single time, and the fact that
    Draw is not called all the time (because AutoRedisplay is false,
    and model is in Examine mode and is still (not rotating)),
    so incrementation steps of AnimationTime are very very small.

    Setting AutoRedisplay to true workarounds the problem too, but that's
    1. unacceptable to eat 100% CPU without a reason for utility like
    view3dscene 2. that's asking for trouble, after all even with
    AutoRedisplay = true the precision loss is there, it's just not
    noticeable... using better precision feels much safer. }
  TAnimationTime = TWorldTime;

  { One screenshot, that is one --screenshot or --screenshot-range option
    occurence.

    Count describes how many actual image screen shots we have to do
    for this TScreenShot instance. For each Index between 0 and Count - 1,
    you can call UseTime and UseFileName to actually get time of this
    capture and the image filename where to save this.

    Note that UseFileName will do the substitution and actually
    increment the counter for filenames with %d inside. So always
    call UseFileName only once, and in correct order. }
  TScreenShot = class
    FileNamePattern: string;

    function Count: Cardinal; virtual; abstract;
    function UseTime(const Index: Integer): TAnimationTime; virtual; abstract;
    function UseFileName(const Index: Integer): string; virtual; abstract;

    { Call BeginCapture, EndCapture around saving all images for this
      TScreenShot instance. They allow for some tasks, e.g.
      this is the place when TRangeScreenShot may convert images to a single
      movie file.

      Success = @true for EndCapture means that we successfully did
      all the images, @false means that exception occured and this
      is called only to finalize some things. }
    procedure BeginCapture; virtual;
    procedure EndCapture(Success: boolean); virtual;
  end;

  TSingleScreenShot = class(TScreenShot)
    Time: TAnimationTime;

    function Count: Cardinal; override;
    function UseTime(const Index: Integer): TAnimationTime; override;
    function UseFileName(const Index: Integer): string; override;
  end;

  TRangeScreenShot = class(TScreenShot)
  private
    { Are we making single movie file? Will be calculated in BeginCapture. }
    SingleMovieFile: boolean;
    TemporaryImagesPattern: string;
    TemporaryImagesCounter: Cardinal;
  public
    TimeBegin, TimeStep: TAnimationTime;
    FramesCount: Cardinal;

    function Count: Cardinal; override;
    function UseTime(const Index: Integer): TAnimationTime; override;
    function UseFileName(const Index: Integer): string; override;
    procedure BeginCapture; override;
    procedure EndCapture(Success: boolean); override;
  end;

  TObjectsListItem_1 = TScreenShot;
  {$I objectslist_1.inc}
  TScreenShotsList = class(TObjectsList_1)
  private
    ScreenShotCounter: Cardinal;
  end;

var
  { List of screenshots to take.
    Created / destroyed in init / fini of this unit. }
  ScreenShotsList: TScreenShotsList;

{ Are we currently making a screenshot? Just a shortcut for
  ScreenShotsList.Count <> 0. }
function MakingScreenShot: boolean;

{$undef read_interface}

implementation

uses SysUtils, KambiStringUtils, ProgressUnit, KambiFilesUtils, EnumerateFiles,
  DataErrors;

{$define read_implementation}
{$I objectslist_1.inc}

function MakingScreenShot: boolean;
begin
  Result := ScreenShotsList.Count <> 0;
end;

function MakeFileName(const FileNamePattern: string;
  var Counter: Cardinal): string;
var
  Replaces: array [0..0] of TPercentReplace;
  ReplacementsDone: Cardinal;
begin
  Replaces[0].c := 'd';
  Replaces[0].s := IntToStr(Counter);
  Result := SPercentReplace(FileNamePattern, Replaces, ReplacementsDone,
    { ErrorOnUnknownPercentFormat = } false);
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

function TSingleScreenShot.UseTime(const Index: Integer): TAnimationTime;
begin
  Result := Time;
end;

function TSingleScreenShot.UseFileName(const Index: Integer): string;
begin
  Result := MakeFileName(FileNamePattern, ScreenShotsList.ScreenShotCounter);
end;

{ TRangeScreenShot ---------------------------------------------------------- }

function TRangeScreenShot.Count: Cardinal;
begin
  Result := FramesCount;
end;

function TRangeScreenShot.UseTime(const Index: Integer): TAnimationTime;
begin
  Result := TimeBegin + Index * TimeStep;
end;

function TRangeScreenShot.UseFileName(const Index: Integer): string;
begin
  if SingleMovieFile then
    Result := MakeFileName(TemporaryImagesPattern, TemporaryImagesCounter) else
    Result := MakeFileName(FileNamePattern, ScreenShotsList.ScreenShotCounter);

  Progress.Step;
end;

procedure TRangeScreenShot.BeginCapture;
var
  Replaces: array [0..0] of TPercentReplace;
  ReplacementsDone: Cardinal;
  TemporaryImagesPrefix, Ext: string;
  FileRec: TSearchRec;
  SearchError: Integer;
begin
  { calculate SingleMovieFile }
  Ext := ExtractFileExt(FileNamePattern);
  SingleMovieFile :=
    SameText(Ext, '.avi') or
    SameText(Ext, '.mpg') or
    SameText(Ext, '.dvd') or
    SameText(Ext, '.ogg') or
    SameText(Ext, '.mov') or
    SameText(Ext, '.swf');

  if SingleMovieFile then
  begin
    { initialize TemporaryImagesPrefix, TemporaryImagesPattern,
      TemporaryImagesCounter }

    TemporaryImagesPrefix := GetTempFileName('', ProgramName) + '_' +
      { Although GetTempFileName should add some randomization here,
        there's no guarentee. And we really need randomization --- in case
        something failed (and, since calling external ffmpeg is involved,
        everything can happen), we don't want to collide with leftovers from
        previous call to other TRangeScreenShot.BeginCapture. }
      IntToStr(Random(MaxInt)) + '_';

    { Check is it really Ok. }
    SearchError := FindFirst(TemporaryImagesPrefix + '*', faReallyAnyFile,
      FileRec);
    try
      if SearchError = 0 then
        raise Exception.CreateFmt('Failed to generate unique temporary file prefix "%s": filename "%s" already exists',
          [TemporaryImagesPrefix, FileRec.Name]);
    finally FindClose(FileRec) end;

    TemporaryImagesPattern := TemporaryImagesPrefix + '%d.png';
    TemporaryImagesCounter := 1;
  end;

  if not SingleMovieFile then
  begin
    { Check that we have some %d in our filename.
      Just call SPercentReplace and ignore result,
      to get and check ReplacementsDone. }
    Replaces[0].c := 'd';
    Replaces[0].s := 'dummy';
    SPercentReplace(FileNamePattern, Replaces, ReplacementsDone,
      { ErrorOnUnknownPercentFormat = } false);
    if ReplacementsDone = 0 then
      raise EInvalidParams.CreateFmt('--screenshot-range invalid filename "%s": not recognized as movie filename (so assuming image filename), and no %%d pattern found', [FileNamePattern]);
  end;

  Progress.Init(Count, Format('Screenshot range from time %f (%d frames)',
    [TimeBegin, FramesCount]));
end;

procedure TRangeScreenShot.EndCapture(Success: boolean);
var
  Executable, OutputMovieFileName, TempFile: string;
  I: Integer;
begin
  Progress.Fini;

  if SingleMovieFile and Success then
  begin
    Executable := FileSearch(
      {$ifdef MSWINDOWS} 'ffmpeg.exe' {$endif}
      {$ifdef UNIX} 'ffmpeg' {$endif}
      , GetEnvironmentVariable('PATH'));

    if Executable = '' then
    begin
      DataNonFatalError(Format('You must have "ffmpeg" program from ' +
        '[http://ffmpeg.mplayerhq.hu/] installed and available on $PATH to be able to ' +
        'create movie files". Leaving generated temporary images "%s"',
        [TemporaryImagesPattern]));
    end else
    begin
      OutputMovieFileName := MakeFileName(FileNamePattern, ScreenShotsList.ScreenShotCounter);

      Writeln(Output, 'FFMpeg found, executing...');
      Writeln(Output, Executable + ' -f image2 -i "' +
        TemporaryImagesPattern + '" -y "' + OutputMovieFileName + '"');

      ExecuteProcess(Executable,
        [ '-f', 'image2', '-i', TemporaryImagesPattern, '-y', OutputMovieFileName ]);

      Write(Output, 'Removing temporary image files "', TemporaryImagesPattern, '" ...');
      TemporaryImagesCounter := 1;
      for I := 1 to FramesCount do
      begin
        TempFile := MakeFileName(TemporaryImagesPattern, TemporaryImagesCounter);
        if not DeleteFile(TempFile) then
          DataNonFatalError(Format('Cannot delete temporary file "%s"', [TempFile]));
      end;
      Writeln('done.');
    end;
  end;
end;

{ unit initialization / finalization ---------------------------------------- }

initialization
  ScreenShotsList := TScreenShotsList.Create;
  ScreenShotsList.ScreenShotCounter := 1;
finalization
  FreeWithContentsAndNil(ScreenShotsList);
end.
