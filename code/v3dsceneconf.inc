{ This is a subset of castle_game_engine/src/base/castleconf.inc,
  defines OpenGLES symbol and CastleGL macro in the same way.

  It's only to test view3dscene compilation and rendering with OpenGLES,
  which is in turn useful to test CGE OpenGLES renderer
  and as a preparation to port view3dscene to Android.
  In the future, this should be done in less hacky way: view3dscene
  should not directly perform OpenGL / OpenGLES calls (it should only
  use an abstraction over OpenGL / OpenGLES provided by CGE,
  for example all rendering should go by X3D nodes in TCastleScene).
  So this file will eventually disappear.
}

{ Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is generally for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (under Debian, install libegl1-mesa-dev and libgles2-mesa-dev).
  Put "CastleGL" in uses clause to include the appropriate OpenGL API. }
{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef iOS} {$define OpenGLES} {$endif}
{$ifdef OpenGLES}
  {$define CastleGL := CastleGLES20}
{$else}
  {$define CastleGL := GL, GLU, GLExt}
{$endif}
