{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple converter to VRML/X3D from various other 3D formats.
  See [https://castle-engine.io/view3dscene.php#section_converting].

  Reads a 3D model from the URL (in sample case, just a filename)
  given as command-line parameter ('-' means stdin),
  and outputs model as VRML/X3D to stdout.

  Can be used instead of "view3dscene --write ...".
  This way you don't need e.g. OpenGL libraries (that are required
  to run view3dscene) installed on your system.

  For your own uses, you can easily extend this to process VRML/X3D graph.
  For example, add or remove some nodes. See TX3DNode methods.
}

program tovrmlx3d;

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleUtils, CastleClassUtils, X3DNodes, X3DLoad, CastleParameters, CastleDownload,
  CastleFilesUtils, CastleUriUtils, CastleApplicationProperties, CastleLog,
  X3DLoadInternalUtils,
  V3DSceneVersion;

var
  Encoding: TX3DEncoding = xeClassic;
  ForceX3D: Boolean = false;
  Validate: Boolean = false;

const
  Options: array [0..7] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'force-x3d'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log'; Argument: oaNone),
    (Short:  #0; Long: 'no-x3d-extensions'; Argument: oaNone),
    (Short:  #0; Long: 'enable-downloads'; Argument: oaNone),
    (Short:  #0; Long: 'validate'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
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
