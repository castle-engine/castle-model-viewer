#!/bin/bash
set -eu

# -----------------------------------------------------------------------------
# Compile for macOS, create macOS app bundle (.app).
# -----------------------------------------------------------------------------

cd ../
castle-engine package ${CASTLE_ENGINE_TOOL_OPTIONS:-}

# add castle-model-converter binary
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.converter.xml
cp castle-model-converter castle-model-viewer.app/Contents/MacOS/castle-model-converter

# TODO: Add OggVorbis library to bundle, for now loading OggVorbis will not work.
