#!/bin/bash
set -eu

# Install view3dscene GNOME thumbnailer stuff

install_single_thumbnailer ()
{
  THUMBNAILER_MIME="$1"

  # view3dscene command notes:
  # - TODO: spec says that we should make PNG file on the output.
  #   But currently we recognize output file type by extension,
  #   and nautilus calls us with something like "/tmp/.gnome_thumbnail.0WOIDU",
  #   and on unrecognized extension we produce BMP format.
  #   Seems that it's handled Ok, but I should probably switch to PNG anyway.
  # - %s is what everyone uses, but will this handle filenames with spaces?
  echo -n 'Installing for MIME '"$THUMBNAILER_MIME"': '

  THUMBNAILER_MIME=`echo $THUMBNAILER_MIME | sed -e 's|+|@|'`
  THUMBNAILER_MIME=`echo $THUMBNAILER_MIME | sed -e 's|/|@|'`

#  echo -n '(internal name: '"$THUMBNAILER_MIME"'): '

  gconftool --type bool   --set /desktop/gnome/thumbnailers/"$THUMBNAILER_MIME"/enable true
  gconftool --type string --set /desktop/gnome/thumbnailers/"$THUMBNAILER_MIME"/command "view3dscene %i --screenshot 0 %o --geometry %sx%s"

  echo 'done.'
}

install_single_thumbnailer 'model/vrml'
install_single_thumbnailer 'image/x-3ds'
install_single_thumbnailer 'model/x3d+vrml'
install_single_thumbnailer 'model/x3d+xml'
install_single_thumbnailer 'application/x-collada'
install_single_thumbnailer 'application/x-inventor'
install_single_thumbnailer 'application/x-md3'
install_single_thumbnailer 'application/x-wavefront-obj'
install_single_thumbnailer 'application/x-geo'
install_single_thumbnailer 'application/x-kanim'
install_single_thumbnailer 'application/json'

echo '----------'
echo 'Installing view3dscene as GNOME thumbnailer: all OK.'
echo ''
echo 'Remember to call also ./install.sh (before or after ./install_thumbnailer.sh),'
echo 'to make the thumbnailer actually be used for appropriate MIME types.'
echo '----------'
