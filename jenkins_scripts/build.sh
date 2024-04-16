#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# ----------------------------------------------------------------------------
# Build castle-model-viewer for various platforms.
#
# Just like CGE pack_release, you can call this with:
#
# - 2 arguments, OS and CPU, to pack for the given platform.
#
# - no arguments, to pack for "default" platforms,
#   which means "target platforms possible thanks to Docker image
#   https://hub.docker.com/r/kambi/castle-engine-cloud-builds-tools/ ".
#   We also package sources in this case, using "castle-engine package-source".
#
# This script uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .
# ----------------------------------------------------------------------------

# remove previous artifacts
rm -f *.tar.gz *.zip *.apk

package_platform ()
{
  OS="$1"
  CPU="$2"
  shift 2

  # not only "castle-engine clean", to clean also castle-model-converter binaries
  make clean

  # build castle-model-converter first, it will be packaged in castle-model-viewer CastleEngineManifest.xml
  castle-engine compile --os="${OS}" --cpu="${CPU}" --manifest-name=CastleEngineManifest.converter.xml

  # build and package castle-model-viewer

  if [ "${OS}" = 'darwin' ]; then
    castle-engine package --os="${OS}" --cpu="${CPU}" --package-format=mac-app-bundle

    # Add 2nd exe "castle-model-converter" to the bundle.
    # This also means we zip it later manually (TODO: we could add file to zip).
    cp castle-model-converter castle-model-viewer.app/Contents/MacOS/

    VERSION=`castle-engine output version`
    ZIPNAME=castle-model-viewer-"${VERSION}"-darwin-x86_64.zip
    zip -r "${ZIPNAME}" castle-model-viewer.app/
    echo "Packed to ${ZIPNAME}"
  else
    castle-engine package --os="${OS}" --cpu="${CPU}"
  fi
}

if [ -n "${1:-}" ]; then
  package_platform "${1}" "${2}"
else
  package_platform win64 x86_64
  package_platform win32 i386
  package_platform linux x86_64
  castle-engine package-source --verbose
fi
