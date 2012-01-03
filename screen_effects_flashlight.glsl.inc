{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "flashlight.glsl" }
'#extension GL_ARB_texture_rectangle : enable' + LineEnding + 
'uniform sampler2DRect screen;' + LineEnding + 
'uniform sampler2DRect screen_depth;' + LineEnding + 
'uniform int screen_width;' + LineEnding + 
'uniform int screen_height;' + LineEnding + 
'void main (void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);' + LineEnding + 
'  float dist = distance(gl_TexCoord[0].st, vec2(screen_width, screen_height) / 2.0);' + LineEnding + 
'  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;' + LineEnding + 
'  vec2 middle_pos = vec2(float(screen_width), float(screen_height)) / 2.0;' + LineEnding + 
'  float middle_depth = (' + LineEnding + 
'      texture2DRect(screen_depth, middle_pos).r +' + LineEnding + 
'      texture2DRect(screen_depth, middle_pos / 2.0).r +' + LineEnding + 
'      texture2DRect(screen_depth, 3.0 * middle_pos / 2.0).r +' + LineEnding + 
'      texture2DRect(screen_depth, vec2(middle_pos.x / 2.0, 3.0 * middle_pos.y / 2.0)).r +' + LineEnding + 
'      texture2DRect(screen_depth, vec2(3.0 * middle_pos.x / 2.0, middle_pos.y / 2.0)).r' + LineEnding + 
'    ) / 5.0;' + LineEnding + 
'  middle_depth = 1.0 - pow(middle_depth, 20.0);' + LineEnding + 
'  radius_out = mix(radius_out / 3.0, radius_out, middle_depth);' + LineEnding + 
'  /* The magnificent Radeon fglrx crap refuses to correctly do "* 0.8" below */' + LineEnding + 
'  float radius_in = 4.0 * radius_out / 5.0;' + LineEnding + 
'  float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));' + LineEnding + 
'  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));' + LineEnding + 
'}' + LineEnding + 
''
