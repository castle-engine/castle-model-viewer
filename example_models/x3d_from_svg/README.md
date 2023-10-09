# Example of SVG -> X3D export result, along with manual fixes for Z and a shader effect to discard geometry outside of a circle

Castle Game Engine does not support SVG, but it can be converted using X_ITE to X3D.

This is an example of such conversion, done in CGE logo.

Afterwards I manually fixed Z of various geometry items, and added a shader effect to
cut things outside of a circle.

See https://castle-engine.io/creating_data_model_formats.php#section_svg .

Process:

1. Original SVG on https://github.com/castle-engine/castle-engine/blob/master/doc/pasdoc/logo/castle_game_engine_icon.svg .

2. Tweaked in Inkscape to avoid some pieces of tower overlapping, in `castle_game_engine_icon_less_overlap.svg` .

3. Converted to X3D using https://create3000.github.io/x_ite/laboratory/x3d-file-converter .

4. Fixed manually Z of various transformations in X3D, result in `cge_logo_fixed_z.x3d` .

4. Added shader effect. Open `cge_logo_final,x3dv`, it inlines `cge_logo_fixed_z.x3d` .

## Screenshots

![2D view](screenshot1.png)
![3D view](screenshot2.png)
