#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Build view3dscene for various platforms,
# make the binaries available on http://michalis.ii.uni.wroc.pl/view3dscene-snapshots/ .
#
# This script uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .
#
# This script uses some commands and assumes environment of our Jenkins on
# https://jenkins.castle-engine.io/ .

# remove previous artifacts
rm -f view3dscene-*.tar.gz view3dscene-*.zip view3dscene*.apk

. /usr/local/fpclazarus/bin/setup.sh default

export CASTLE_ENGINE_PATH=/var/lib/jenkins/workspace/castle_game_engine_build/
# add castle-engine tool to $PATH
export PATH="${CASTLE_ENGINE_PATH}"tools/build-tool/:"${PATH}"

# remove artifacts from previous build
rm -f view3dscene-*.tar.gz view3dscene-*.zip

castle-engine package --os=win64 --cpu=x86_64 --verbose
castle-engine package --os=win32 --cpu=i386 --verbose
castle-engine package --os=linux --cpu=x86_64 --verbose
castle-engine package-source --verbose

echo '---- Build OK.'

export CASTLE_SNAPSHOTS_PATH="/var/www/view3dscene-snapshots/"
rm -f "${CASTLE_SNAPSHOTS_PATH}"view3dscene-*.tar.gz \
      "${CASTLE_SNAPSHOTS_PATH}"view3dscene-*.zip

cp -f view3dscene-*.tar.gz view3dscene-*.zip "${CASTLE_SNAPSHOTS_PATH}"
