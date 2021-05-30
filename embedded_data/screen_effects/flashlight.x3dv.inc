{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "flashlight.x3dv" }
'#X3D V4.0 utf8' + LineEnding +
'PROFILE Interchange' + LineEnding +
'' + LineEnding +
'# Flashlight screen effect.' + LineEnding +
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
'          gl_FragColor = screenf_get_original_color();' + LineEnding +
'          float dist = distance(vec2(screen_position()), vec2(screen_width, screen_height) / 2.0);' + LineEnding +
'' + LineEnding +
'          // calculate radius_out, \"how large would be light radius for wall at this depth\"' + LineEnding +
'          float radius_out = min(float(screen_width), float(screen_height)) / 2.0;' + LineEnding +
'          float depth = screenf_get_original_depth();' + LineEnding +
'          depth = 1.0 - pow(depth, 20.0);' + LineEnding +
'          radius_out = mix(radius_out / 3.0, radius_out, depth);' + LineEnding +
'' + LineEnding +
'          /* Radeon fglrx (crappy OpenGL driver) refuses to correctly do \"* 0.8\" below */' + LineEnding +
'          float radius_in = 4.0 * radius_out / 5.0;' + LineEnding +
'' + LineEnding +
'          float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));' + LineEnding +
'          gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));' + LineEnding +
'        }' + LineEnding +
'        "' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
