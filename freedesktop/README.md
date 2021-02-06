# Installing desktop files for freedesktop

This directory contains files for desktop integration (icons, associations, menu entries) on https://freedesktop.org/ desktops, which practically means: desktops on Linux, FreeBSD and other free software Unix.

# How to install

It is advised to build and install by executing this in the top-level view3dscene directory:

```
make
sudo make install
```

The latter command makes a system-wide installation, copying the view3dscene and tovrmlx3d binaries to `/usr/local/bin` , to make them available on `$PATH`. It also installs some desktop files under `/usr/local/share`. You can undo it by

```
sudo make uninstall
```

If you would prefer to make an installation only for your used, and not use root privileges, you can also just run

```
cd freedesktop/
./install.sh
```

But in the latter case, it is your responsibility to make the binaries actually visible by X session. Note that you will need to modify `$PATH` used by the X session, which usually means you need to create and use the `~/.Xsession` file. (Adjusting path of your shell, e.g. in `~/.bashrc`, is *not* enough.)

# Details how this works

This is for developers -- normal users can stop reading now :)

See http://library.gnome.org/devel/integration-guide/stable/desktop-files.html.en for basic GNOME integration instructions.

Desktop file

- Should be copied to /usr/share/applications or ~/.local/share/applications
  (for GNOME <= 2.10 (<= 2.8 on Fedora) this should be
  ~/.gnome2/vfolders/applications).

- See http://standards.freedesktop.org/menu-spec/latest/apa.html
  for a list of allowed Category values.

- "StartupNotify=true" can be useful for me to add to *.desktop files,
  as GTK 2 backend should automatically support this
  (http://library.gnome.org/devel/gdk/stable/gdk-General.html#gdk-notify-startup-complete).
  For now, view3dscene opens instantly fast, so it's not needed.

- update-desktop-database call is needed to actually associate my program
  with MimeType specified in desktop file. (Found by looking at some Debian
  packges postinst script.)

Icon file

- Should be copied under /usr/share/icons/,
  most standard is /usr/share/icons/hicolor/48x48/apps/.
  Local user has this in ~/.local/share/icons/
  and ~/.local/share/icons/hicolor/48x48/apps/

- "48x48" is just indicated as most standard, and smaller versions will
  be generated from it. For SVG icons, I think (but didn't actually
  read this anywhere) they should go to "scalable" subdir.

- Update: although on my Debian "scalable" subdir is indeed standard,
  it looks like desktop icon remains for some reason stupidly small
  (even though it's recorded in SVG file that resolution is 48x48).
  Yes, this happens to other programs with only scalable icons too,
  like eog. It doesn't happen e.g. to evolution --- because evolution
  just adds 48x48 icon as PNG file, in addition to SVG in scalable.
  Fortunately, we can trivially easy just autogenerate PNG 48x48 version
  to workaround this.

- Do not specify icon extension in *.desktop file.
  Algorithm for searching icons (from spec
  http://standards.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html)
  makes it clear that implementation will try appending various extensions
  on it's own. Besides, as noted above, we have both SVG and PNG versions of
  the icon, so we do not know what extension to specify.

Mime file:

- Should be copied to /usr/share/mime/packages/,
  local user location is ~/.local/share/mime/packages/.

- After copying you should run
    update-mime-database /usr/share/mime
  or equivalent for local user location.

- Note that our mime XML file describes "model/x3d+binary", but it's
  not yet listed in our desktop file --- because we do not handle x3db yet.

Thumbnailer script:

- It works :)

- Beware that loading arbitrary 3D scene may take a lot of time,
  consume a lot of memory and CPU power.

  Although we try to be fast, and some things are specially optimized
  for screenshot (namely, RendererOptimization is always roNone when
  generating a screenshot for a single image), but stil some things
  could be optimized more (like octree construction should be avoided
  for a screenshot).

  But no amount of optimization will make loading fast for any scene.
  And we can't really detect every resource use (CPU, memory, time)
  and abort model loading (parsing, loading textures, rendering to OpenGL)
  --- since this is quite complicated and delegates work to many libraries,
  OpenGL foremost.

  The bottom line is: making a thumbnail of 3D scene may be resource-consuming,
  and there's really nothing we can do about it.
  Nautilus should automatically terminate thumbnailer that
  runs too long, so this is not critical problem --- but you may experience
  slowdowns when using this thumbnailer.

You may want to run

```
killall gnome-panel
killall nautilus
```

to refresh icons and mime types everywhere without relogging to GNOME. (If anyone knows a cleaner to do this, please let me know.) Without these kills, only the menu entry is actomatically updated. I.e. changes to *.desktop files dir are picked up automatically, but icon and mime files not (it seems).
