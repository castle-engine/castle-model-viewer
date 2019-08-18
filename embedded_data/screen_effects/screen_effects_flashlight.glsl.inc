{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "flashlight.glsl" }
'/* See https://castle-engine.io/x3d_extensions_screen_effects.php' + LineEnding +
'   for docs how to write screen effects for CGE.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'void main (void)' + LineEnding +
'{' + LineEnding +
'  gl_FragColor = screen_get_color(screen_position());' + LineEnding +
'  float dist = distance(vec2(screen_position()), vec2(screen_width, screen_height) / 2.0);' + LineEnding +
'  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;' + LineEnding +
'  ivec2 middle_pos = ivec2(screen_width, screen_height) / 2;' + LineEnding +
'  float middle_depth = (' + LineEnding +
'      screen_get_depth(middle_pos) +' + LineEnding +
'      screen_get_depth(middle_pos / 2) +' + LineEnding +
'      screen_get_depth(3 * middle_pos / 2) +' + LineEnding +
'      screen_get_depth(ivec2(middle_pos.x / 2, 3 * middle_pos.y / 2)) +' + LineEnding +
'      screen_get_depth(ivec2(3 * middle_pos.x / 2, middle_pos.y / 2))' + LineEnding +
'    ) / 5.0;' + LineEnding +
'  middle_depth = 1.0 - pow(middle_depth, 20.0);' + LineEnding +
'  radius_out = mix(radius_out / 3.0, radius_out, middle_depth);' + LineEnding +
'  /* The magnificent Radeon fglrx crap refuses to correctly do "* 0.8" below */' + LineEnding +
'  float radius_in = 4.0 * radius_out / 5.0;' + LineEnding +
'  float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));' + LineEnding +
'  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));' + LineEnding +
'}' + LineEnding +
''
