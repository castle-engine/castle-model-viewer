#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# ----------------------------------------------------------------------------
# Build view3dscene for various platforms.
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
rm -f view3dscene-*.tar.gz view3dscene-*.zip view3dscene*.apk
rm -f tovrmlx3d-*.tar.gz tovrmlx3d-*.zip tovrmlx3d*.apk # just in case, remove

package_platform ()
{
  OS="$1"
  CPU="$2"
  shift 2

  # not only "castle-engine clean", to clean also tovrmlx3d binaries
  make clean

  # build tovrmlx3d first, it will be packaged in view3dscene CastleEngineManifest.xml
  castle-engine compile --os="${OS}" --cpu="${CPU}" --manifest-name=CastleEngineManifest.tovrmlx3d.xml

  # build and package view3dscene
  castle-engine package --os="${OS}" --cpu="${CPU}"
}

if [ -n "${1:-}" ]; then
  package_platform "${1}" "${2}"
else
  package_platform win64 x86_64
  package_platform win32 i386
  package_platform linux x86_64
  castle-engine package-source --verbose
fi
