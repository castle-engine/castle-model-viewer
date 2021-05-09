#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Build view3dscene for various platforms.
# This script uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .

# remove previous artifacts
rm -f view3dscene-*.tar.gz view3dscene-*.zip view3dscene*.apk
rm -f tovrmlx3d-*.tar.gz tovrmlx3d-*.zip tovrmlx3d*.apk # just in case, remove

package_platform ()
{
  EXE_EXTENSION="$1"
  shift 1

  # not only "castle-engine clean", to clean also tovrmlx3d binaries
  make clean

  # build tovrmlx3d first, it will be packaged in view3dscene CastleEngineManifest.xml
  castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.tovrmlx3d.xml

  # build and package view3dscene
  castle-engine package "$@"
}

package_platform '.exe' --os=win64 --cpu=x86_64
package_platform '.exe' --os=win32 --cpu=i386
package_platform ''     --os=linux --cpu=x86_64
castle-engine package-source --verbose
