#!/bin/bash
set -eu

rm -Rf snap/
mkdir snap/
# Old: snapcraft warns about snapcraft.yaml~ otherwise.
# Not useful anymore, as we create clean snap/ directory.
# rm -f snap/*~

VERSION=`castle-engine output version`
sed -e 's|${SNAP_VERSION}|'${VERSION}'|' snap/snapcraft.yaml.template

snapcraft

ls -Flah castle-model-viewer_*.snap

echo 'To install run:'
echo 'sudo snap install --devmode castle-model-viewer_*_amd64.snap'
