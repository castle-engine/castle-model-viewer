# Castle Model Viewer (formerly view3dscene)

Viewer for all 3D and 2D model formats supported by Castle Game Engine.

Supported formats:

- [glTF](https://castle-engine.io/gltf)
- [X3D](https://castle-engine.io/vrml_x3d.php)
- VRML 2.0 (aka VRML 97) and 1.0
- [Spine JSON](https://castle-engine.io/spine)
- [Sprite sheets (in CGE, Starling and Cocos2d formats)](https://castle-engine.io/sprite_sheets)
- castle-anim-frames
- OpenInventor (IV)
- 3DS
- [MD3](https://castle-engine.io/md3)
- Wavefront OBJ
- Collada
- STL

See also [Castle Game Engine model formats](https://castle-engine.io/creating_data_model_formats.php).

Complete documentation on https://castle-engine.io/castle-model-viewer .

Using Castle Game Engine, see https://castle-engine.io/ .

# castle-model-converter (formerly tovrmlx3d)

`castle-model-converter` is a simple command-line program to convert all supported model formats to X3D, and to pretty-print X3D and VRML. It can also be used to validate models.

Note that in simple cases, `castle-model-viewer` can also be used to convert models. Interactively (just open a model, and then use menu items _"File -> Save As..."_) or from command-line (use `--write` and related options). But `castle-model-converter` has a bit more options and at the same time lighter requirements (it does not depend on any GUI libraries).

The documentation of command-line options to perform convertion is on https://castle-engine.io/castle-model-converter .

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `castle_model_viewer.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

## License

GNU GPL >= 2.
