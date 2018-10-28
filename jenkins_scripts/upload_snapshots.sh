#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Make the binaries (created by build.sh)
# available on http://michalis.ii.uni.wroc.pl/view3dscene-snapshots/ .

export CASTLE_SNAPSHOTS_PATH="/var/www/view3dscene-snapshots/"
rm -f "${CASTLE_SNAPSHOTS_PATH}"view3dscene-*.tar.gz \
      "${CASTLE_SNAPSHOTS_PATH}"view3dscene-*.zip

cp -f view3dscene-*.tar.gz view3dscene-*.zip "${CASTLE_SNAPSHOTS_PATH}"
