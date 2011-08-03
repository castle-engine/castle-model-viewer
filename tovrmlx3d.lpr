{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple converter to VRML/X3D from various other 3D formats.
  See [http://vrmlengine.sourceforge.net/view3dscene.php#section_converting].

  Reads a 3D model from the filename given as command-line parameter
  ('-' means stdin), and outputs model as VRML/X3D to stdout.

  Can be used instead of view3dscene --write parameter.
  This way you don't need e.g. OpenGL libraries (that are required
  to run view3dscene) installed on your system.

  For your own uses, you can easily extend this to process VRML/X3D graph.
  For example, add or remove some nodes. See TVRMLNode methods.
}

program tovrmlx3d;

uses SysUtils, KambiUtils, KambiClassUtils, VRMLNodes, X3DLoad,
  ParseParametersUnit, V3DSceneVersion, KambiWarnings, KambiFilesUtils;

var
  Encoding: TX3DEncoding = xeClassic;
  ForceX3D: boolean = false;

const
  Options: array [0..3] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short:  #0; Long: 'encoding'; Argument: oaRequired),
    (Short:  #0; Long: 'force-x3d'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           'tovrmlx3d: converter from various 3D model formats into VRML/X3D.' +NL+
           'Give input 3D model filename on the command-line, and output model' +NL+
           'will be written to the standard output.' +NL+
           NL+
           'Available options are:' +NL+
           HelpOptionHelp +NL+
           VersionOptionHelp +NL+
           '  --encoding classic|xml' +NL+
           '                        Choose X3D encoding to use with --write option.' +NL+
           '                        Default is "classic".' +NL+
           '  --force-x3d           Force convertion from VRML to X3D with --write option.' +NL+
           '                        Note that if you choose XML encoding' +NL+
           '                        (by --encoding=xml), this is automatic.' +NL+
           '                        Note that this works sensibly only for VRML 2.0' +NL+
           '                        (not for older Inventor/VRML 1.0).' +NL+
           NL+
           SVrmlEngineProgramHelpSuffix('tovrmlx3d', Version, true));
         ProgramBreak;
       end;
    1: begin
         Writeln(Version);
         ProgramBreak;
       end;
    2: if SameText(Argument, 'classic') then
         Encoding := xeClassic else
       if SameText(Argument, 'xml') then
         Encoding := xeXML else
         raise EInvalidParams.CreateFmt('Invalid --encoding argument "%s"', [Argument]);
    3: ForceX3D := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  FileName: string;
  Node: TVRMLNode;
begin
  { parse command-line }
  ParseParameters(Options, @OptionProc, nil);
  Parameters.CheckHigh(1);
  FileName := Parameters[1];

  OnWarning := @OnWarningWrite;

  Node := LoadVRML(FileName, true);
  try
    SaveVRML(Node, StdOutStream, 'tovrmlx3d, http://vrmlengine.sourceforge.net/view3dscene.php#section_converting',
      ExtractFileName(FileName), Encoding, ForceX3D);
  finally FreeAndNil(Node) end;
end.
