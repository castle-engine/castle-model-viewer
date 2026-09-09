#!/bin/bash
set -eu

# Install castle-model-viewer menu entries, icons, mime types (to open castle-model-viewer
# automatically when clicking on models).

APP_NAME='castle-model-viewer'

# Use share directory from parameter. By default use user-local directory
# (this makes this script useful for normal users, documented on castle-model-viewer website).
SHARE_PREFIX="${1:-${HOME}/.local/share}"

echo -n "Installing $APP_NAME desktop stuff (MIME types, icons, desktop file) to ${SHARE_PREFIX}... "

# Install mime types:
install -d "$SHARE_PREFIX"/mime/packages/
install --mode 644 "$APP_NAME".xml "$SHARE_PREFIX"/mime/packages/
update-mime-database "$SHARE_PREFIX"/mime

# Install icons:
install -d "$SHARE_PREFIX"/icons/hicolor/scalable/apps/
install --mode 644 "$APP_NAME".svg "$SHARE_PREFIX"/icons/hicolor/scalable/apps/

# Install also PNG versions, this helps nautilus to display icon
# properly on the desktop.
install -d "$SHARE_PREFIX"/icons/hicolor/48x48/apps/
install -d "$SHARE_PREFIX"/icons/hicolor/512x512/apps/
install --mode 644 "$APP_NAME"_48.png "$SHARE_PREFIX"/icons/hicolor/48x48/apps/"$APP_NAME".png
install --mode 644 "$APP_NAME"_512.png "$SHARE_PREFIX"/icons/hicolor/512x512/apps/"$APP_NAME".png

# Makes the system aware of new icons, otherwise new icons in ~/.local/share/icons
# are not immediately visible, until relogin.
# See https://portland.freedesktop.org/doc/xdg-icon-resource.html
#
# Note that we don't use "xdg-icon-resource install .." to install as it
# doesn't seem to support SVG file types or --size scalable, according to docs,
# and SVG is our primary icon format.
xdg-icon-resource forceupdate

# Is this needed?
#if which update-icon-caches >/dev/null 2>&1 ; then update-icon-caches "$SHARE_PREFIX"/icons/hicolor/; fi

# Install desktop file:
# (Do this at the end, when mime and icons are already installed;
# I don't know for sure what update-desktop-database actually does,
# it's better to be safe).
install -d "$SHARE_PREFIX"/applications/
install --mode 644 "$APP_NAME".desktop "$SHARE_PREFIX"/applications/
if which update-desktop-database >/dev/null 2>&1 ; then update-desktop-database -q "$SHARE_PREFIX"/applications/; fi

echo "installed OK."
