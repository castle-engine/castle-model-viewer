{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "edge_detect.glsl" }
'#extension GL_ARB_texture_rectangle : enable' + LineEnding + 
'uniform sampler2DRect screen;' + LineEnding + 
'void main (void)' + LineEnding + 
'{' + LineEnding + 
'  vec4 left   = texture2DRect(screen, vec2(gl_TexCoord[0].s - 1.0, gl_TexCoord[0].t));' + LineEnding + 
'  vec4 right  = texture2DRect(screen, vec2(gl_TexCoord[0].s + 1.0, gl_TexCoord[0].t));' + LineEnding + 
'  vec4 top    = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t + 1.0));' + LineEnding + 
'  vec4 bottom = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t - 1.0));' + LineEnding + 
'  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;' + LineEnding + 
'}' + LineEnding + 
''
