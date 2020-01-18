/* See https://castle-engine.io/x3d_extensions_screen_effects.php
   for docs how to write screen effects for CGE.
*/

void main (void)
{
  gl_FragColor = screen_get_color(screen_position());
  float dist = distance(vec2(screen_position()), vec2(screen_width, screen_height) / 2.0);

  // calculate radius_out, "how large would be light radius for wall at this depth"
  float radius_out = min(float(screen_width), float(screen_height)) / 2.0;
  float depth = screen_get_depth(screen_position());
  depth = 1.0 - pow(depth, 20.0);
  radius_out = mix(radius_out / 3.0, radius_out, depth);

  /* Radeon fglrx (crappy OpenGL driver) refuses to correctly do "* 0.8" below */
  float radius_in = 4.0 * radius_out / 5.0;

  float p = mix(1.0 / 4.0, 1.0, smoothstep(radius_in, radius_out, dist));
  gl_FragColor.rgb = pow(gl_FragColor.rgb, vec3(p, p, p));
}
