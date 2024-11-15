# Handling new model formats in Castle Model Viewer and Castle Game Engine

- Implement new format reader.

    See the engine `X3DLoad` unit and register your new format using `RegisterModelFormat` routine.

- If you want to modify CGE and make this format available to all CGE applications out-of-the-box, the unit with your registration should be used by `X3DLoad`. This is what existing built-in formats do.

    But this is only if you want to modify CGE! If you just define new model format for your own purposes, then use the unit with your registration in your own application, no need to modify the engine.

- Enable castle-model-viewer to associate with this file format:

    1. Add format to ../freedesktop/castle-model-viewer.xml , to define new MIME type.

    2. Add format to ../freedesktop/castle-model-viewer.desktop , to indicate that castle-model-viewer handles this MIME type.

    3. Add to ../freedesktop/install_thumbnailer.sh , so that GNOME nautilus thumbnailers for this MIME types can be installed.

- Extend the documentation on https://castle-engine.io/creating_data_model_formats.php .
