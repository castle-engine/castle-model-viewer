#!/bin/bash
set -eu

rm -Rf snap/
mkdir snap/
# Old: snapcraft warns about snapcraft.yaml~ otherwise.
# Not useful anymore, as we create clean snap/ directory.
# rm -f snap/*~

VERSION=`castle-engine output version`
sed -e 's|${SNAP_VERSION}|'${VERSION}'|' snapcraft.yaml.template > snap/snapcraft.yaml

# See https://snapcraft.io/docs/snapcraft-overview
# about --debug .
snapcraft --debug

ls -Flah castle-model-viewer_*.snap
