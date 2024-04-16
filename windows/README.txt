This directory contains Windows-specific desktop stuff.

Like a quick way to use our SVG icon as a raster icon for Windows exe.
History how it was done:

- The automatic export by Inkscape to 32x32 size in castle-image-viewer.png was
  a starting point. But at 32x32, you really have to fix details manually.
  Also Windows (at least Win 2000 Prof) cannot handle 8bit alpha well (actually,
  it's not handled at all, it seems). So the castle-image-viewer.png version was reworked
  in GIMP a bit to make it look good.

- Then convert from castle-image-viewer.png -> castle-image-viewer.ico.
  I used GIMP (choosing 8bpp, 1bit alpha, 256 palette --- like said, Windows
  doesn't seem to handle 8bit alpha channel, it's just ignored.)
