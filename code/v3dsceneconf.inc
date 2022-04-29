{ Define various useful symbols. }

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$if defined(iPHONESIM) or (defined(DARWIN) and (defined(CPUARM) or defined(CPUARM64)))}
    {$define iOS}
  {$endif}
{$endif}

{ Whether we use OpenGL ES or normal OpenGL.

  view3dscene and CGE by default use OpenGL.
  Only on mobile (Android, iOS) we use OpenGL ES automatically,

  But you can use this to test OpenGLES renderer with view3dscene.
  Under Debian, install libegl1-mesa-dev and libgles2-mesa-dev. }
{.$define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef iOS} {$define OpenGLES} {$endif}

{$ifdef OpenGLES}
  {$ifndef FPC}
    {$error We do not support OpenGLES (necessary for Android, iOS) with Delphi yet}
  {$else}
    {$define CastleGL := CastleGLES}
  {$endif}
{$else}
  {$define CastleGL := GL, GLExt}
{$endif}

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
{$endif}
