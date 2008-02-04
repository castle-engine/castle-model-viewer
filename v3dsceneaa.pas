unit V3DSceneAA;

interface

const
  MaxAntiAliasing = 4;

var
  { Anti-aliasing level.

    0 - none
    1 - 2 samples, dont_care
    2 - 2 samples, nicest (quincunx (5 taps) for NVidia)
    3 - 4 samples, dont_care
    4 and more - 4 samples, nicest (9 taps for NVidia)
  }
  AntiAliasing: Cardinal = 0;

function AntiAliasingGlwMultiSampling: Cardinal;

procedure AntiAliasingGLInit;

procedure AntiAliasingEnable;
procedure AntiAliasingDisable;

implementation

uses KambiGLUtils, GL, GLExt;

function AntiAliasingGlwMultiSampling: Cardinal;
begin
  case AntiAliasing of
    0: Result := 1;
    1..2: Result := 2;
    else Result := 4;
  end;
end;

procedure AntiAliasingGLInit;
begin
  if ( (AntiAliasing = 2) or
       (AntiAliasing >= 4) ) and
     GL_NV_multisample_filter_hint then
    glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
end;

procedure AntiAliasingEnable;
begin
  if (AntiAliasing > 0) and GL_ARB_multisample then
    glEnable(GL_MULTISAMPLE_ARB);
end;

procedure AntiAliasingDisable;
begin
  if (AntiAliasing > 0) and GL_ARB_multisample then
    glDisable(GL_MULTISAMPLE_ARB);
end;

end.
