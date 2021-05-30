{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "grayscale.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Grayscale screen effect.' + LineEnding +
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
'          float gray = color.r * 0.212671 + color.g * 0.715160 + color.b * 0.072169;' + LineEnding +
'          gl_FragColor = vec4(gray, gray, gray, color.a);' + LineEnding +
'        }' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
