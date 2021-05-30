{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "power.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Apply trivial power over RGB colors (fancy brighten/darken) screen effect.' + LineEnding +
'#' + LineEnding +
'# See https://castle-engine.io/x3d_extensions_screen_effects.php' + LineEnding +
'# for docs how to write screen effects for CGE.' + LineEnding +
'' + LineEnding +
'ScreenEffect {' + LineEnding +
'  shaders DEF MyShader ComposedShader {' + LineEnding +
'    language "GLSL"' + LineEnding +
'    inputOutput SFFloat exponent 1.0' + LineEnding +
'    parts ShaderPart {' + LineEnding +
'      type "FRAGMENT"' + LineEnding +
'      url "data:text/plain,' + LineEnding +
'        uniform float exponent;' + LineEnding +
'        void main (void)' + LineEnding +
'        {' + LineEnding +
'          vec4 color = screenf_get_original_color();' + LineEnding +
'          color.rgb = pow(color.rgb, vec3(exponent));' + LineEnding +
'          gl_FragColor = color;' + LineEnding +
'        }' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
