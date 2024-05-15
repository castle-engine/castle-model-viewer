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

  See https://castle-engine.io/castle-model-converter
  for usage docs.
  See https://castle-engine.io/creating_data_model_formats.php
  for the supported model formats.

  Usage:
  Provide 2 parameters on the command-line:
  1. input URL (can be just a filename) to read,
  2. output URL (to save to, can be just a filename).

  The output URL (2nd) is optional.
  If not provided, the stdout is assumed, which is equivalent to providing '-'
  as the output URL.
  Also, the output URL is not allowed when the --validate is used.

  Use '-' as input URL on the command-line to read from the standard input
  (consider using then also --stdin-url=xxx option, to determine the file type
  and base to resolve relative URLs).

  Analogously, use '-' as output URL on the command-line to write to the standard output.
  Consider using --stdout-url=xxx option to determine the file type.

  This tool can be used instead of "castle-model-viewer" conversions done from GUI.
}

program castle_model_converter;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleUtils, CastleClassUtils, X3DNodes, X3DLoad, CastleParameters, CastleDownload,
  CastleFilesUtils, CastleUriUtils, CastleApplicationProperties, CastleLog,
  X3DLoadInternalUtils, X3DFields,
  V3DSceneVersion;

var
  Validate: Boolean = false;
  StdInUrl: String = 'stdin.x3dv';
  StdOutUrl: String = 'stdout.x3dv';
  OutputX3dEncoding: String = '';

const
  Options: array [0..9] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'debug-log'; Argument: oaNone),
    (Short:  #0; Long: 'no-x3d-extensions'; Argument: oaNone),
    (Short:  #0; Long: 'enable-downloads'; Argument: oaNone),
    (Short:  #0; Long: 'validate'; Argument: oaNone),
    (Short:  #0; Long: 'stdin-url'; Argument: oaRequired),
    (Short:  #0; Long: 'stdout-url'; Argument: oaRequired),
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
          'castle-model-converter: converter for all 3D and 2D model formats' +NL+
          'supported by Castle Game Engine (like glTF, X3D, Collada, MD3...).' +NL+
          NL+
          'Call with these parameters on the command-line:' +NL+
          NL+
          '1. The input filename. It can be any filename, URL or - (stdin).' +NL+
          '   Required.' +NL+
          NL+
          '2. The output filename. It can be any filename, URL or - (stdout).' +NL+
          '   This parameter is optional. If not provided, the output is stdout,' +NL+
          '   unless --validate is also used (then the output is not written anywhere).' +NL+
          NL+
          'Available options are:' +NL+
          OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
          OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
          OptionDescription('--force-x3d', 'Force conversion from VRML to X3D. Note that if you choose XML encoding (by --encoding=xml), this is automatic. Note that this works sensibly only for VRML 2.0 (not for older Inventor/VRML 1.0, we cannot convert them to valid X3D for now).') +NL+
          OptionDescription('--no-x3d-extensions', 'Do not use Castle Game Engine extensions. This will output file valid in all X3D browsers (but maybe with some CGE-specific features missing).') +NL+
          OptionDescription('--enable-downloads', 'Enable (blocking) downloads from the net, e.g. to download a texture or Inlined model referenced by htt(s) protocol).') +NL+
          OptionDescription('--validate', 'Only validate the input, without any output (the output URL is not allowed in this case). Moreover, if there will be any warning or error, we will exit with non-zero status (by default, only errors cause non-zero status).') +NL+
          OptionDescription('--stdin-url', 'If input URL is "-", then we read file contents from the standard input. In this case, you can use this option to provide a "pretend" URL for the input. We will use it to resolve relative URLs inside the input (e.g. to glTF binary blobs) and to guess the input file type. Default is "stdin.x3dv" in current directory, so we assume it is X3D (classic encoded), and resolve with respect to the current directory.') +NL+
          OptionDescription('--stdout-url', 'If output URL is "-", then we write file contents to the standard output. In this case, you can use this option to provide a "pretend" URL for the output. We will use it to determine the output file type, e.g. "out.x3d" to output X3D XML encoding or "out.x3dv" to output X3D classic encoding.') +NL+
          OptionDescription('--float-precision DIGITS', 'Number of digits after the decimal point when writing floating-point numbers. Default is to write all possibly relevant digits. Specify any value >= 0 to use this number of digits.') +NL+
          OptionDescription('--encoding classic|xml', 'DEPRECATED. Choose X3D encoding. Do not use -- the 2nd parameter should determine the output type, ".x3d" extension says to make X3D XML, ".x3dv" says to make X3D classic. Or use --stdout-url to provide fake URL in case output is to stdout.') + NL +
          NL+
          ApplicationProperties.Description);
        Halt;
      end;
    1:begin
        Writeln(Version);
        Halt;
      end;
    2:OutputX3dEncoding := Argument;
    3:begin
        LogEnableStandardOutput := false;
        InitializeLog;
        Writeln(ErrOutput, ApplicationProperties.ApplicationName, ': Logging to ', LogOutput);
      end;
    4:CastleX3dExtensions := false;
    5:EnableBlockingDownloads := true;
    6:Validate := true;
    7:StdInUrl := Argument;
    8:StdOutUrl := Argument;
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
  InputUrl, OutputUrl, OutputMimeType: String;
  Node: TX3DRootNode;
  EventsHandler: TEventsHandler;
  OutputStream: TStream;
begin
  ApplicationProperties.ApplicationName := 'castle-model-converter';
  ApplicationProperties.Version := Version;

  { parse command-line, calculating InputUrl and OutputUrl }
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(1);
  Parameters.CheckHighAtMost(2);
  InputUrl := Parameters[1];
  if Parameters.High = 2 then
    OutputUrl := Parameters[2]
  else
    OutputUrl := '-';

  { Make basic check that output URL was not specified when --validate is used.
    This over-uses the fact that "not providing output URL" is equivalent to
    providing "-" as output URL. }
  if Validate and (OutputUrl <> '-') then
    raise EInvalidParams.Create('When --validate is used, the output URL is not allowed');

  EventsHandler := TEventsHandler.Create;
  try
    ApplicationProperties.OnWarning.Add(@EventsHandler.HandleWarning);

    // load the model, thus calculating Node
    if InputUrl = '-' then
      Node := LoadNodeStandardInput
    else
      Node := LoadNode(InputUrl);

    // calculate OutputMimeType
    if OutputUrl = '-' then
      OutputMimeType := UriMimeType(StdOutUrl)
    else
      OutputMimeType := UriMimeType(OutputUrl);

    // override OutputMimeType if we need to support deprecated --encoding
    if OutputX3dEncoding <> '' then
    begin
      WritelnWarning('DEPRECATED: --encoding argument is deprecated. Use instead the 2nd parameter (output URL) to determine the output type, from extension. Or use --stdout-url to provide fake URL in case output is to stdout.');
      if SameText(OutputX3dEncoding, 'classic') then
      begin
        if Node.HasForceVersion and (Node.ForceVersion.Major <= 2) then
          OutputMimeType := 'model/vrml' // VRML <= 2.0
        else
          OutputMimeType := 'model/x3d+vrml'; // X3D in classic encoding
      end else
      if SameText(OutputX3dEncoding, 'xml') then
      begin
        OutputMimeType := 'model/x3d+xml'; // X3D in XML encoding
      end else
        raise EInvalidParams.CreateFmt('Invalid --encoding argument "%s"', [OutputX3dEncoding]);
    end;

    try
      if not Validate then
      begin
        if OutputUrl <> '-' then
          OutputStream := UrlSaveStream(OutputUrl)
        else
          OutputStream := StdOutStream;

        SaveNode(Node, OutputStream, OutputMimeType,
          { generator (metadata) } 'castle-model-converter, https://castle-engine.io/castle-model-converter',
          { source (metadata) } ExtractURIName(InputUrl));

        if OutputStream <> StdOutStream then
          FreeAndNil(OutputStream);
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
