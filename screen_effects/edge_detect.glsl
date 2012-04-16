int screen_x();
int screen_y();
vec4 screen_get_color(ivec2 position);

void main (void)
{
  vec4 left   = screen_get_color(ivec2(screen_x() - 1, screen_y()));
  vec4 right  = screen_get_color(ivec2(screen_x() + 1, screen_y()));
  vec4 top    = screen_get_color(ivec2(screen_x(), screen_y() - 1));
  vec4 bottom = screen_get_color(ivec2(screen_x(), screen_y() + 1));
  gl_FragColor = (abs(left - right) + abs(top - bottom)) / 2.0;
}
