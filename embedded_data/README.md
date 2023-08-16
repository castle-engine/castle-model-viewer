To make distributing view3dscene easy, for now we pack all the data inside the executable.
So instead of traditional data/ subdirectory ( https://castle-engine.io/manual_data_directory.php ),
we have embedded_data/ subdirectory, and we process files using
file_to_pascal_string (from
https://github.com/pasdoc/pasdoc/tree/master/source/tools )
or image-to-pascal (from CGE).
