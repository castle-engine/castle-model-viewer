#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
void main (void)
{
  vec4 left   = texture2DRect(screen, vec2(gl_TexCoord[0].s - 1.0, gl_TexCoord[0].t));
  vec4 right  = texture2DRect(screen, vec2(gl_TexCoord[0].s + 1.0, gl_TexCoord[0].t));
  vec4 top    = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t + 1.0));
  vec4 bottom = texture2DRect(screen, vec2(gl_TexCoord[0].s, gl_TexCoord[0].t - 1.0));
  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;
}
