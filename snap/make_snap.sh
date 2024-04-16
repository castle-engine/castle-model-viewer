#!/bin/bash
set -eu

# snapcraft warns about snapcraft.yaml~ otherwise.
rm -f snap/*~

snapcraft

ls -Flah castle-model-viewer_*.snap

echo 'To install run:'
echo 'sudo snap install --devmode castle-model-viewer_4.1.0_amd64.snap'
