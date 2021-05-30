{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "visualize_depth.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Visualize depth screen effect.' + LineEnding +
'#' + LineEnding +
'# See https://castle-engine.io/x3d_extensions_screen_effects.php' + LineEnding +
'# for docs how to write screen effects for CGE.' + LineEnding +
'' + LineEnding +
'ScreenEffect {' + LineEnding +
'  needsDepth TRUE' + LineEnding +
'  shaders ComposedShader {' + LineEnding +
'    language "GLSL"' + LineEnding +
'    parts ShaderPart {' + LineEnding +
'      type "FRAGMENT"' + LineEnding +
'      url "data:text/plain,' + LineEnding +
'        void main (void)' + LineEnding +
'        {' + LineEnding +
'          float d = pow(screenf_get_original_depth(), 20.0);' + LineEnding +
'          gl_FragColor = vec4(d, d, d, 1.0);' + LineEnding +
'        };' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
