#!/bin/bash
set -eu

# -----------------------------------------------------------------------------
# Compile for macOS, create macOS app bundle (.app).
# -----------------------------------------------------------------------------

cd ../
castle-engine package ${CASTLE_ENGINE_TOOL_OPTIONS:-}

# add tovrmlx3d binary
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.tovrmlx3d.xml
cp tovrmlx3d view3dscene.app/Contents/MacOS/tovrmlx3d

# TODO: Add OggVorbis library to bundle, for now loading OggVorbis will not work.
