{ This is a subset of castle_game_engine/src/base/castleconf.inc,
  defines various useful symbols in the same way.

  The need to define some of these things is only for testing:

  - For testing OpenGLES renderer, view3dscene wants to know OpenGLES state.

    This is only to test view3dscene compilation and rendering with OpenGLES,
    which is in turn useful to test CGE OpenGLES renderer
    and as a preparation to port view3dscene to Android.
    In the future, this should be done in less hacky way: view3dscene
    should not directly perform OpenGL / OpenGLES calls (it should only
    use an abstraction over OpenGL / OpenGLES provided by CGE,
    for example all rendering should go by X3D nodes in TCastleScene).
    So this file will eventually disappear.

  - For testing Delphi compilation, view3dscene code can be compiled
    in my FPC "Delphi test mode", in which we use Delphi mode and disable macros
    and such.
    This is the reason behind CASTLE_ENGINE_TEST_DELPHI_MODE defines.

    In normal usage, view3dscene is compiled with FPC,
    without CASTLE_ENGINE_TEST_DELPHI_MODE defined.
}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$if defined(iPHONESIM) or (defined(DARWIN) and (defined(CPUARM) or defined(CPUARM64)))}
    {$define iOS}
  {$endif}
{$endif}

{ Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is generally for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (under Debian, install libegl1-mesa-dev and libgles2-mesa-dev).
  Put "CastleGL" in uses clause to include the appropriate OpenGL API. }
{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef iOS} {$define OpenGLES} {$endif}

{$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}
  {$ifdef OpenGLES}
    {$define CastleGL := CastleGLES20}
  {$else}
    {$define CastleGL := GL, GLU, GLExt}
  {$endif}
{$endif}

// Allows to workaround FPC bug in some units, FPC seems to "lose"
// the fact that FPC_OBJFPC should be defined.
// The cleanest solution seems to just ignore FPC_OBJFPC,
// and define equivalent symbol (CASTLE_OBJFPC) on our own.
{$ifdef FPC}
  {$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}
    {$define CASTLE_OBJFPC}
  {$endif}
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
