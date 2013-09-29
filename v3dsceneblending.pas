unit V3DSceneBlending;

{$I castleconf.inc}

interface

uses CastleGL, CastleGLUtils;

const
  BlendingFactors: array [0..10] of record
    Value: TGLenum;
    Name: string;
    ForSource: boolean;
    ForDestination: boolean;
  end =
  ( ( Value: GL_ZERO;                Name: 'Zero'                       ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE;                 Name: 'One'                        ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_DST_COLOR;           Name: 'Dst (Screen) Color'         ; ForSource: true ; ForDestination: false; ),
    ( Value: GL_SRC_COLOR;           Name: 'Src (Shape) Color'          ; ForSource: false; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_DST_COLOR; Name: '1 - Dst (Screen) Color'     ; ForSource: true ; ForDestination: false; ),
    ( Value: GL_ONE_MINUS_SRC_COLOR; Name: '1 - Src (Shape) Color'      ; ForSource: false; ForDestination: true ; ),
    ( Value: GL_SRC_ALPHA;           Name: 'Src (Shape) Alpha'          ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_SRC_ALPHA; Name: '1 - Src (Shape) Alpha'      ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_DST_ALPHA;           Name: 'Dst (Screen) Alpha'         ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_DST_ALPHA; Name: '1 - Dst (Screen) Alpha'     ; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_SRC_ALPHA_SATURATE;  Name: 'Src (Shape) Alpha Saturated'; ForSource: true ; ForDestination: false; )
  );

implementation

end.