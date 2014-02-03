int screen_x();
int screen_y();
vec4 screen_get_color(ivec2 position);
float screen_get_depth(ivec2 position);
ivec2 screen_position();

/* Below already declared by ScreenEffectLibrary that is glued at
   the beginning.
uniform int screen_width;
uniform int screen_height;
*/

void main (void)
{
  gl_FragColor = screen_get_color(screen_position());
  float dist = distance(vec2(screen_position()), vec2(screen_width, screen_height) / 2.0);
  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;
  ivec2 middle_pos = ivec2(screen_width, screen_height) / 2;
  float middle_depth = (
      screen_get_depth(middle_pos) +
      screen_get_depth(middle_pos / 2) +
      screen_get_depth(3 * middle_pos / 2) +
      screen_get_depth(ivec2(middle_pos.x / 2, 3 * middle_pos.y / 2)) +
      screen_get_depth(ivec2(3 * middle_pos.x / 2, middle_pos.y / 2))
    ) / 5.0;
  middle_depth = 1.0 - pow(middle_depth, 20.0);
  radius_out = mix(radius_out / 3.0, radius_out, middle_depth);
  /* The magnificent Radeon fglrx crap refuses to correctly do "* 0.8" below */
  float radius_in = 4.0 * radius_out / 5.0;
  float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));
  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));
}
