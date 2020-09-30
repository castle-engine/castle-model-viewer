#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Build view3dscene for various platforms.
# This script uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .

# remove previous artifacts
rm -f view3dscene-*.tar.gz view3dscene-*.zip view3dscene*.apk

package_platform ()
{
  EXE_EXTENSION="$1"
  shift 1

  make clean # not only "castle-engine clean", to clean also tovrmlx3d binaries
  castle-engine simple-compile code/tovrmlx3d.lpr "$@"
  mv -f code/tovrmlx3d"${EXE_EXTENSION}" .
  castle-engine package "$@"
}

package_platform '.exe' --os=win64 --cpu=x86_64
package_platform '.exe' --os=win32 --cpu=i386
package_platform ''     --os=linux --cpu=x86_64
castle-engine package-source --verbose
