This directory contains Windows-specific desktop stuff.

Like a quick way to use our SVG icon as a raster icon for Windows exe.
History how it was done:

- The automatic export by Inkscape to 32x32 size in view3dscene.png was
  a starting point. But at 32x32, you really have to fix details manually.
  Also Windows (at least Win 2000 Prof) cannot handle 8bit alpha well (actually,
  it's not handled at all, it seems). So the view3dscene.png version was reworked
  in GIMP a bit to make it look good.

- Then convert from view3dscene.png -> view3dscene.ico.
  I used GIMP (choosing 8bpp, 1bit alpha, 256 palette --- like said, Windows
  doesn't seem to handle 8bit alpha channel, it's just ignored.)

- After this `make' will make view3dscene.res.

  Note that main icon has MainIcon identifier, not AppIcon.
  Although [http://freepascal.org/docs-html/prog/progse61.html] suggests using AppIcon,
  I saw some programs using MAINICON, and my CastleWindow in WinAPI backend also loads icon
  from MAINICON name. Ultimately, it doesn't seem to matter to Windows (it grabs
  the first ico available in file?).

Version info crafted looking at docs, what others are doing, and what Lazarus produces:
http://www.osronline.com/article.cfm?article=588
http://msdn.microsoft.com/en-us/library/windows/desktop/aa381058%28v=vs.85%29.aspx
http://stackoverflow.com/questions/12821369/vc-2012-how-to-include-version-info-from-version-inc-maintained-separately
http://forum.lazarus.freepascal.org/index.php?topic=8979.0

Manifest contents als done looking at what Lazarus produces. See also:
http://stackoverflow.com/questions/1402137/how-to-use-a-manifest-embedded-as-a-resource-windows-xp-vista-style-controls
http://qt-project.org/forums/viewthread/17440
http://msdn.microsoft.com/en-us/library/wwtazz9d.aspx

view3dscene.res will be automatically included in view3dscene.exe when recompiled.
