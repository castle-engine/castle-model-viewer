{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "negative.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Negate RGB screen effect.' + LineEnding +
'#' + LineEnding +
'# See https://castle-engine.io/x3d_extensions_screen_effects.php' + LineEnding +
'# for docs how to write screen effects for CGE.' + LineEnding +
'' + LineEnding +
'ScreenEffect {' + LineEnding +
'  shaders ComposedShader {' + LineEnding +
'    language "GLSL"' + LineEnding +
'    parts ShaderPart {' + LineEnding +
'      type "FRAGMENT"' + LineEnding +
'      url "data:text/plain,' + LineEnding +
'        void main (void)' + LineEnding +
'        {' + LineEnding +
'          vec4 color = screenf_get_original_color();' + LineEnding +
'          gl_FragColor = vec4(vec3(1.0) - color.rgb, color.a);' + LineEnding +
'        }' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
