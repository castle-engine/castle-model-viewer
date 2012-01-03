#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
uniform sampler2DRect screen_depth;
uniform int screen_width;
uniform int screen_height;
void main (void)
{
  gl_FragColor = texture2DRect(screen, gl_TexCoord[0].st);
  float dist = distance(gl_TexCoord[0].st, vec2(screen_width, screen_height) / 2.0);
  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;
  vec2 middle_pos = vec2(float(screen_width), float(screen_height)) / 2.0;
  float middle_depth = (
      texture2DRect(screen_depth, middle_pos).r +
      texture2DRect(screen_depth, middle_pos / 2.0).r +
      texture2DRect(screen_depth, 3.0 * middle_pos / 2.0).r +
      texture2DRect(screen_depth, vec2(middle_pos.x / 2.0, 3.0 * middle_pos.y / 2.0)).r +
      texture2DRect(screen_depth, vec2(3.0 * middle_pos.x / 2.0, middle_pos.y / 2.0)).r
    ) / 5.0;
  middle_depth = 1.0 - pow(middle_depth, 20.0);
  radius_out = mix(radius_out / 3.0, radius_out, middle_depth);
  /* The magnificent Radeon fglrx crap refuses to correctly do "* 0.8" below */
  float radius_in = 4.0 * radius_out / 5.0;
  float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));
  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));
}
