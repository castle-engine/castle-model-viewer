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

uses SysUtils,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleUtils, CastleClassUtils, X3DNodes, X3DLoad, CastleParameters, CastleDownload,
  CastleFilesUtils, CastleURIUtils, CastleApplicationProperties, CastleLog,
  X3DLoadInternalUtils,
  V3DSceneVersion;

var
  Encoding: TX3DEncoding = xeClassic;
  ForceX3D: boolean = false;

const
  Options: array [0..6] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'force-x3d'; Argument: oaNone),
    (Short:  #0; Long: 'debug-log'; Argument: oaNone),
    (Short:  #0; Long: 'no-x3d-extensions'; Argument: oaNone),
    (Short:  #0; Long: 'enable-downloads'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           'tovrmlx3d: converter from various 3D model formats into VRML/X3D.' +NL+
           'Give input 3D model URL (usually just a filename) on the command-line,' +NL+
           'and output model will be written to the standard output.' +NL+
           NL+
           'Available options are:' +NL+
           HelpOptionHelp +NL+
           VersionOptionHelp +NL+
           '  --encoding classic|xml' +NL+
           '                        Choose X3D encoding. Default is "classic".' +NL+
           '  --force-x3d           Force conversion from VRML to X3D.' +NL+
           '                        Note that if you choose XML encoding' +NL+
           '                        (by --encoding=xml), this is automatic.' +NL+
           '                        Note that this works sensibly only for VRML 2.0' +NL+
           '                        (not for older Inventor/VRML 1.0,' +NL+
           '                        we cannot convert them to valid X3D for now).' +NL+
           '  --no-x3d-extensions   Do not use Castle Game Engine extensions to X3D.' +NL+
           '                        Particularly useful when combined with --write,' +NL+
           '                        to have X3D valid in all browsers (but less functional).' +NL+
           NL+
           ApplicationProperties.Description);
         Halt;
       end;
    1: begin
         Writeln(Version);
         Halt;
       end;
    2: if SameText(Argument, 'classic') then
         Encoding := xeClassic else
       if SameText(Argument, 'xml') then
         Encoding := xeXML else
         raise EInvalidParams.CreateFmt('Invalid --encoding argument "%s"', [Argument]);
    3: ForceX3D := true;
    4: begin
         LogEnableStandardOutput := false;
         InitializeLog;
         Writeln(ErrOutput, ApplicationProperties.ApplicationName, ': Logging to ', LogOutput);
       end;
    5: CastleX3dExtensions := false;
    6: EnableBlockingDownloads := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

procedure Run;
var
  URL: string;
  Node: TX3DNode;
begin
  ApplicationProperties.ApplicationName := 'tovrmlx3d';
  ApplicationProperties.Version := Version;

  { parse command-line }
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  URL := Parameters[1];

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Node := LoadNode(URL);
  try
    Save3D(Node, StdOutStream, 'tovrmlx3d, https://castle-engine.io/view3dscene.php#section_converting',
      ExtractURIName(URL), Encoding, ForceX3D);
  finally FreeAndNil(Node) end;
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
