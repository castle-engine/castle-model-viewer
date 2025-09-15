{ Define various useful symbols. }

{$ifdef FPC}
  {$ifndef VER3_0}
    {$ifndef VER3_1}
      { Compiler automatically detects and warns when "case" doesn't cover
        all possibilities. This means we do not have to, and actually
        we should not have to (as it would cause "Warning: Unreachable code" warning),
        add a clause like "else raise EInternalError.Create('xxx');"
        to "case" statements that should always have a matching possibility.

        This is true for FPC >= 3.2 now. }
      {$define COMPILER_CASE_ANALYSIS}
    {$endif}
  {$endif}

  {$modeswitch advancedrecords}
{$else}
  { Delphi reports some things that are known to be harmless in our case. }
  {$warn COMPARING_SIGNED_UNSIGNED off}
  {$warn COMBINING_SIGNED_UNSIGNED off}
{$endif}

