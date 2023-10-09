This is a scene for showing off shadow maps.

You can open with view3dscene
(http://castle-engine.sourceforge.net/view3dscene.php) every VRML file here.
To see the final scene open castle_with_trees_final.x3dv.

You can edit with Blender castle_with_trees.blend scene,
then export it to castle_with_trees.wrl (preferably with our exporter from
http://castle-engine.sourceforge.net/blender.php ),
then run "make" (this makes castle_with_trees_processed.x3dv from
castle_with_trees.wrl). This way you can design most important stuff
(including lights positions) in Blender.

------------------------------------------------------------------------------
Credits:

Castle wall and towers from
http://opengameart.org/content/low-poly-castle-pieces
(GNU GPL 2.0, GNU GPL 3.0, CC-BY-SA 3.0)

Tree from
http://opengameart.org/content/old-oak-tree
(GNU GPL 2.0, GNU GPL 3.0, CC-BY-SA 3.0)

oaktree.blend contains the same tree remade in Blender by Michalis.

I preserved the geometry and texture mapping (actually, Blender Wavefront OBJ importer did :) ). It's split into two Blender objects (main part and leaves), so you can simply hide/remove leaves object if you want a leafless version.

Note: if you want to render from Blender, you'll probably want to set Alpha=0 on leaves material to make them honor texture transparent regions for leaves. Currently it is set to =1, to make VRML exporter behave Ok.

oaktree.wrl is the above Blender model exported to VRML 97 by my script (very slightly modified version of Blender's "VRML 97 Export" script) from http://castle-engine.sourceforge.net/blender.php

Texture dryleaves.jpg from
http://opengameart.org/content/ground-textures-dryleavesjpg
(CC0 (Public Domain))

Texture 04pavInform256.png from
http://opengameart.org/content/even-grey-stone-tile-floor-256px
(GNU LGPL 2.0, GNU LGPL 3.0, CC-BY 3.0)

Programming and modeling of the final scene here by Michalis Kamburelis.
