{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "edge_detect.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Simple "edge detect" screen effect.' + LineEnding +
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
'          vec4 left   = screen_get_color(ivec2(screen_x() - 1, screen_y()));' + LineEnding +
'          vec4 right  = screen_get_color(ivec2(screen_x() + 1, screen_y()));' + LineEnding +
'          vec4 top    = screen_get_color(ivec2(screen_x(), screen_y() - 1));' + LineEnding +
'          vec4 bottom = screen_get_color(ivec2(screen_x(), screen_y() + 1));' + LineEnding +
'          gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;' + LineEnding +
'        }' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
