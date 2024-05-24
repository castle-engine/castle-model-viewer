#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# -----------------------------------------------------------------
# Prepare a release and upload for itch.io.
# -----------------------------------------------------------------

ITCH_IO_NAME='castle-engine/castle-model-viewer'

do_upload_one_platform ()
{
  local OS=$1
  local CPU=$2

  local UPLOAD_DIR_PLATFORM=$UPLOAD_DIR/$OS-$CPU

  mkdir -p $UPLOAD_DIR_PLATFORM

  # We create a directory with release -- not packed into zip or tar.gz.
  # butler prefers to upload unpacked directories.
  # See https://itch.io/docs/butler/single-files.html
  castle-engine package --os=$OS --cpu=$CPU \
    --package-format=directory --verbose \
    --output=$UPLOAD_DIR_PLATFORM

  cp $OS-$CPU'.itch.toml' $UPLOAD_DIR_PLATFORM/.itch.toml

  butler push \
    $UPLOAD_DIR_PLATFORM $ITCH_IO_NAME:$OS-$CPU \
    --userversion "${VERSION}"
}

UPLOAD_DIR=/tmp/upload_itch_io_unpacked_$$
rm -Rf $UPLOAD_DIR # make sure it is clean

VERSION=`castle-engine output version`
echo "Uploading version ${VERSION} to itch.io"

do_upload_one_platform linux x86_64
do_upload_one_platform win64 x86_64

echo 'Running "butler status ...":'
butler status $ITCH_IO_NAME
