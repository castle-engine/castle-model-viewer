#!/bin/bash
set -eu

# Install view3dscene menu entries, icons, mime types (to open view3dscene
# automatically when clicking on 3D models).

APP_NAME='view3dscene'

# Use share directory from parameter. By default use user-local directory
# (this makes this script useful for normal users, documented on view3dscene website).
SHARE_PREFIX="${1:-${HOME}/.local/share}"

echo -n "Installing $APP_NAME desktop stuff (MIME types, icons, desktop file) to ${SHARE_PREFIX}... "

# Install mime types:
install -d "$SHARE_PREFIX"/mime/packages/
install --mode 644 "$APP_NAME".xml "$SHARE_PREFIX"/mime/packages/
update-mime-database "$SHARE_PREFIX"/mime

# Install icons:
install -d "$SHARE_PREFIX"/icons/hicolor/scalable/apps/
install --mode 644 "$APP_NAME".svg "$SHARE_PREFIX"/icons/hicolor/scalable/apps/

# Install also 48x48 PNG version, this helps nautilus to display icon
# properly on the desktop.
install -d "$SHARE_PREFIX"/icons/hicolor/48x48/apps/
install --mode 644 "$APP_NAME".png "$SHARE_PREFIX"/icons/hicolor/48x48/apps/

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
