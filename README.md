# view3dscene

`view3dscene` is a viewer for all 3D and 2D model formats supported by Castle Game Engine.

Supported formats:

- glTF 2.0
- X3D
- VRML 2.0 (aka VRML 97) and 1.0
- Spine JSON
- Sprite sheets (in CGE, Starling and Cocos2d formats)
- castle-anim-frames
- OpenInventor (IV)
- 3DS
- MD3
- Wavefront OBJ
- Collada
- STL

See also [Castle Game Engine model formats](https://castle-engine.io/creating_data_model_formats.php).

Complete documentation on https://castle-engine.io/view3dscene.php .

Using Castle Game Engine, see https://castle-engine.io/ .

# tovrmlx3d

`tovrmlx3d` is a simple command-line program to convert various 3D file formats to X3D, and to pretty-print X3D and VRML. Note that you can also use `view3dscene` for this task (`view3dscene` has menu items to do it interactively, and command-line options to do it in batch mode). But sometimes using "tovrmlx3d" may be useful as it has lighter requirements (not linked with any GUI libraries, which is useful on a server).

The documentation of command-line options to perform convertion is on https://castle-engine.io/view3dscene.php#section_converting .

## License

GNU GPL >= 2.
