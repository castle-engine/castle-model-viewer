unit V3DSceneBlending;

interface

uses GL, KambiGLUtils;

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

  { Although for my engine default is GL_ONE
    (see TVRMLFlatSceneGL.DefaultBlendingDestinationFactor),
    and it's not a problem since it's configurable...

    But for view3dscene default is GL_ONE_MINUS_SRC_ALPHA, since this is usually
    expected by VRML authors. And sometimes things are simply almost invisible
    when on white background. }
  V3DDefaultBlendingDestinationFactor = GL_ONE_MINUS_SRC_ALPHA;

implementation

end.