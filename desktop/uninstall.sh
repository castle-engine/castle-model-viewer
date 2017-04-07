#!/bin/bash
set -eu

# Remove files installed by install.sh

remove_files ()
{
  echo 'Removing files:' "$@"
  rm -Rf "$@"
}

APP_NAME='view3dscene'
SHARE_PREFIX="${1:-${HOME}/.local/share}"
remove_files \
  "$SHARE_PREFIX"/mime/packages/"$APP_NAME".xml \
  "$SHARE_PREFIX"/icons/hicolor/scalable/apps/"$APP_NAME".svg \
  "$SHARE_PREFIX"/icons/hicolor/48x48/apps/"$APP_NAME".png \
  "$SHARE_PREFIX"/applications/"$APP_NAME".desktop

echo "Uninstalled OK."
