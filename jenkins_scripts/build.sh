#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Build view3dscene for various platforms.
# This script uses http://redsymbol.net/articles/unofficial-bash-strict-mode/ .

# remove previous artifacts
rm -f view3dscene-*.tar.gz view3dscene-*.zip view3dscene*.apk

castle-engine package --os=win64 --cpu=x86_64 --verbose
castle-engine package --os=win32 --cpu=i386 --verbose
castle-engine package --os=linux --cpu=x86_64 --verbose
castle-engine package-source --verbose
