{
  Copyright 2003-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Converter of 3D and 2D model formats supported by Castle Game Engine.

  See https://castle-engine.io/view3dscene.php#section_converting
  for usage docs.
  See https://castle-engine.io/creating_data_model_formats.php
  for the supported 3D model formats. For now, we can read all formats
  documented there, and we can output only VRML or X3D.

  Usage:
  Reads a 3D model from the URL (can be just a filename)
  given as a command-line parameter.
  Outputs model to the stdout.
  Use '-' on the command-line to read from the standard input
  (consider using then also --stdin-url=xxx option, to determine the file type
  and base to resolve relative URLs).

  This tool can be used instead of "view3dscene --write ...".
  This way you don't need e.g. OpenGL libraries (that are required
  to run view3dscene) installed on your system.

  For Castle Game Engine developers:
  For your own uses, you can fork and easily extend this tool
  to process the nodes graph.
  For example, add or remove some nodes.
  See https://castle-engine.io/vrml_x3d.php for docs.
}

program tovrmlx3d;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleUtils, CastleClassUtils, X3DNodes, X3DLoad, CastleParameters, CastleDownload,
  CastleFilesUtils, CastleUriUtils, CastleApplicationProperties, CastleLog,
  X3DLoadInternalUtils, X3DFields,
  V3DSceneVersion;

var
  Encoding: TX3DEncoding = xeClassic;
  ForceX3D: Boolean = false;
  Validate: Boolean = false;
  StdInUrl: String = 'stdin.x3dv';

const
  Options: array [0..9] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'force-x3d'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log'; Argument: oaNone),
    (Short:  #0; Long: 'no-x3d-extensions'; Argument: oaNone),
    (Short:  #0; Long: 'enable-downloads'; Argument: oaNone),
    (Short:  #0; Long: 'validate'; Argument: oaNone),
    (Short:  #0; Long: 'stdin-url'; Argument: oaRequired),
    (Short:  #0; Long: 'float-precision'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  FloatPrecision: Integer;
begin
  case OptionNum of
    0:begin
        InfoWrite(
          'tovrmlx3d: converter from various 3D model formats into VRML/X3D.' +NL+
          'Give input 3D model URL (usually just a filename) on the command-line,' +NL+
          'and output model will be written to the standard output.' +NL+
          NL+
          'Available options are:' +NL+
          OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
          OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
          OptionDescription('--encoding classic|xml', 'Choose X3D encoding. Default is "classic".') + NL +
          OptionDescription('--force-x3d', 'Force conversion from VRML to X3D. Note that if you choose XML encoding (by --encoding=xml), this is automatic. Note that this works sensibly only for VRML 2.0 (not for older Inventor/VRML 1.0, we cannot convert them to valid X3D for now).') +NL+
          OptionDescription('--no-x3d-extensions', 'Do not use Castle Game Engine extensions. This will output file valid in all X3D browsers (but maybe with some CGE-specific features missing).') +NL+
          OptionDescription('--enable-downloads', 'Enable (blocking) downloads from the net, e.g. to download a texture or Inlined model referenced by htt(s) protocol).') +NL+
          OptionDescription('--validate', 'Only validate the input, without any output. Moreover, if there will be any warning or error, we will exit with non-zero status (by default, only errors cause non-zero status).') +NL+
          OptionDescription('--stdin-url', 'If input URL is "-", then we read file contents from the standard input. In this case, you can use this option to provide a "pretend" URL for the input. We will use it to resolve relative URLs inside the input (e.g. to glTF binary blobs) and to guess the input file type. Default is "stdin.x3dv" in current directory, so we assume it is X3D (classic encoded), and resolve with respect to the current directory.') +NL+
          OptionDescription('--float-precision DIGITS', 'Number of digits after the decimal point when writing floating-point numbers. Default is to write all possibly relevant digits. Specify any value >= 0 to use this number of digits.') +NL+
          NL+
          ApplicationProperties.Description);
        Halt;
      end;
    1:begin
        Writeln(Version);
        Halt;
      end;
    2:begin
        if SameText(Argument, 'classic') then
          Encoding := xeClassic
        else
        if SameText(Argument, 'xml') then
          Encoding := xeXML
        else
          raise EInvalidParams.CreateFmt('Invalid --encoding argument "%s"', [Argument]);
      end;
    3:ForceX3D := true;
    4:begin
        LogEnableStandardOutput := false;
        InitializeLog;
        Writeln(ErrOutput, ApplicationProperties.ApplicationName, ': Logging to ', LogOutput);
      end;
    5:CastleX3dExtensions := false;
    6:EnableBlockingDownloads := true;
    7:Validate := true;
    8:StdInUrl := Argument;
    9:begin
        if not TryStrToInt(Argument, FloatPrecision) then
          raise EInvalidParams.CreateFmt('Invalid --float-precision argument "%s"', [Argument]);
        if FloatPrecision < 0 then
          FloatOutputFormat := '%g'
        else
          FloatOutputFormat := '%.' + IntToStr(FloatPrecision) + 'f';
      end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

type
  TEventsHandler = class
    WasWarning: Boolean;
    procedure HandleWarning(const Category, Message: String);
  end;

procedure TEventsHandler.HandleWarning(const Category, Message: String);
begin
  ApplicationProperties.WriteWarningOnConsole(Category, Message);
  WasWarning := true;
end;

function LoadNodeStandardInput: TX3DRootNode;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ReadGrowingStream(StdInStream, Stream, true);
    Result := LoadNode(Stream,
      { BaseUrl. When this is only a filename, like default "stdin.x3dv",
        then LoadNode will interpret it as file in current directory already,
        because LoadNode does AbsoluteUri at start. } StdInUrl,
      { MimeType } UriMimeType(StdInUrl));
  finally FreeAndNil(Stream) end;
end;

procedure Run;
var
  Url: String;
  Node: TX3DNode;
  EventsHandler: TEventsHandler;
begin
  ApplicationProperties.ApplicationName := 'tovrmlx3d';
  ApplicationProperties.Version := Version;

  { parse command-line }
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  Url := Parameters[1];

  EventsHandler := TEventsHandler.Create;
  try
    ApplicationProperties.OnWarning.Add(@EventsHandler.HandleWarning);

    if Url = '-' then
      Node := LoadNodeStandardInput
    else
      Node := LoadNode(Url);

    try
      if not Validate then
      begin
        Save3D(Node, StdOutStream,
          { generator (metadata) } 'tovrmlx3d, https://castle-engine.io/view3dscene.php#section_converting',
          { source (metadata) } ExtractURIName(Url),
          Encoding, ForceX3D);
      end;
    finally FreeAndNil(Node) end;

    if Validate and EventsHandler.WasWarning then
      raise Exception.Create('Validation failed (consult the warnings above), exiting with non-zero status');
  finally FreeAndNil(EventsHandler) end;
end;

begin
  try
    Run;
  except
    on E: TObject do
    begin
      { In case of exception, write nice message and exit with non-zero status,
        without dumping any stack trace (because it's normal to
        exit with exception in case of project/environment error, not a bug,
        and the stack trace is mostly useless for end-users in -dRELEASE mode). }
      Writeln(ErrOutput, ExceptMessage(E));
      Halt(1);
    end;
  end;
end.
