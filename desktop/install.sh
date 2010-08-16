#!/bin/bash
set -eu

# Install view3dscene menu entries, icons, mime types (to open view3dscene
# automatically when clicking on 3D models).

# For non-user install, change this to
#   SHARE_PREFIX = /usr/local/share
# For Debian package, you probably want to set this to
#   SHARE_PREFIX = $(DESTDIR)/usr/share
SHARE_PREFIX="$HOME"/.local/share

# Install mime types:
mkdir -p "$SHARE_PREFIX"/mime/packages/
cp view3dscene.xml "$SHARE_PREFIX"/mime/packages/
update-mime-database "$SHARE_PREFIX"/mime

# Install icons:
mkdir -p "$SHARE_PREFIX"/icons/hicolor/scalable/apps/
cp view3dscene.svg "$SHARE_PREFIX"/icons/hicolor/scalable/apps/view3dscene.svg

# Install also 48x48 PNG version, this helps nautilus to display icon
# properly on the desktop.
mkdir -p "$SHARE_PREFIX"/icons/hicolor/48x48/apps/
cp view3dscene.png "$SHARE_PREFIX"/icons/hicolor/48x48/apps/view3dscene.png

# Is this needed?
#if which update-icon-caches >/dev/null 2>&1 ; then update-icon-caches "$SHARE_PREFIX"/icons/hicolor/; fi

# Install desktop file:
# (Do this at the end, when mime and icons are already installed;
# I don't know for sure what update-desktop-database actually does,
# it's better to be safe).
mkdir -p "$SHARE_PREFIX"/applications/
cp view3dscene.desktop "$SHARE_PREFIX"/applications/
if which update-desktop-database >/dev/null 2>&1 ; then update-desktop-database -q "$SHARE_PREFIX"/applications/; fi

echo 'Installed view3dscene (menu entries, icons, mime types): all OK.'
