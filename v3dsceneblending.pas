unit V3DSceneBlending;

interface

uses OpenGLh;

const
  BlendingFactors: array [0..10] of record
    Value: TGLenum;
    Name: string;
    ForSource: boolean;
    ForDestination: boolean;
  end =
  ( ( Value: GL_ZERO;                Name: 'GL_ZERO';                ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE;                 Name: 'GL_ONE';                 ForSource: true ; ForDestination: true ; ),
    ( Value: GL_DST_COLOR;           Name: 'GL_DST_COLOR';           ForSource: true ; ForDestination: false; ),
    ( Value: GL_SRC_COLOR;           Name: 'GL_SRC_COLOR';           ForSource: false; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_DST_COLOR; Name: 'GL_ONE_MINUS_DST_COLOR'; ForSource: true ; ForDestination: false; ),
    ( Value: GL_ONE_MINUS_SRC_COLOR; Name: 'GL_ONE_MINUS_SRC_COLOR'; ForSource: false; ForDestination: true ; ),
    ( Value: GL_SRC_ALPHA;           Name: 'GL_SRC_ALPHA';           ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_SRC_ALPHA; Name: 'GL_ONE_MINUS_SRC_ALPHA'; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_DST_ALPHA;           Name: 'GL_DST_ALPHA';           ForSource: true ; ForDestination: true ; ),
    ( Value: GL_ONE_MINUS_DST_ALPHA; Name: 'GL_ONE_MINUS_DST_ALPHA'; ForSource: true ; ForDestination: true ; ),
    ( Value: GL_SRC_ALPHA_SATURATE;  Name: 'GL_SRC_ALPHA_SATURATE';  ForSource: true ; ForDestination: false; )
  );

implementation

end.