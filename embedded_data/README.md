# Embedded (in exe) data files

To make distributing castle-model-viewer easy, for now we pack all the data inside the castle-model-viewer executable.

So instead of a traditional `data/` subdirectory used in most Castle Game Engine applications ( https://castle-engine.io/data ), we have `embedded_data/` subdirectory, and we process files to Pascal using

- `file_to_pascal_string` (from https://github.com/pasdoc/pasdoc/tree/master/source/tools )

- `image-to-pascal` (from CGE tools)

- `texture-font-to-pascal` (from CGE tools)

Once we have processed things to Pascal include files or units, we "embed" them in the executable just by linking with them in Pascal code.
