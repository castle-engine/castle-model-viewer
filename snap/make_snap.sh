#!/bin/bash
set -eu

# snapcraft warns about snapcraft.yaml~ otherwise.
rm -f snap/*~

snapcraft

ls -Flah view3dscene_*.snap

echo 'To install run:'
echo 'sudo snap install --devmode view3dscene_4.1.0_amd64.snap'
