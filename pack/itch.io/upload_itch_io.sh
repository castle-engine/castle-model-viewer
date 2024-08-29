#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# -----------------------------------------------------------------------
# Uploads Castle Model Viewer packages
# to https://castle-engine.itch.io/castle-model-viewer .
#
# Uses itch.io command-line butler:
# https://itch.io/docs/butler/installing.html
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html
# -----------------------------------------------------------------------

source "${CASTLE_ENGINE_PATH}/../cge-www/pack/download_github.sh"

ITCH_IO_NAME='castle-engine/castle-model-viewer'
GITHUB_ORG_REPO='castle-engine/castle-model-viewer'

#VERSION=`castle-engine output version`
VERSION=5.2.0
echo "Uploading Castle Model Viewer version ${VERSION} to itch.io"

MANIFESTS_PATH="`pwd`/"

do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"manifest-windows.itch.toml "${GITHUB_ORG_REPO}" "v${VERSION}" "castle-model-viewer-${VERSION}-win64-x86_64.zip"    $ITCH_IO_NAME:windows --userversion "${VERSION}"
do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"manifest-unix.itch.toml    "${GITHUB_ORG_REPO}" "v${VERSION}" "castle-model-viewer-${VERSION}-linux-x86_64.tar.gz" $ITCH_IO_NAME:linux   --userversion "${VERSION}"
do_upload_itch_io "${ITCH_IO_NAME}" "${MANIFESTS_PATH}"manifest-unix.itch.toml    "${GITHUB_ORG_REPO}" "v${VERSION}" "castle-model-viewer-${VERSION}-darwin-x86_64.zip"   $ITCH_IO_NAME:mac     --userversion "${VERSION}"

echo 'Running "butler status ...":'
butler status $ITCH_IO_NAME
