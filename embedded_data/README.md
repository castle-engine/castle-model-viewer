To make distributing view3dscene easy, for now we pack all the data inside the executable.
So instead of traditional data/ subdirectory ( https://castle-engine.io/manual_data_directory.php ),
we have embedded_data/ subdirectory, and we process files using
file_to_pascal_string or file_to_pascal_data from
https://github.com/pasdoc/pasdoc/tree/master/source/tools .

TODO: Move here all data, from ../internal_scenes/ , ../images/ etc.
