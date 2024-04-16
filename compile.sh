#!/usr/bin/env bash
set -euo pipefail
# See http://redsymbol.net/articles/unofficial-bash-strict-mode/

# ----------------------------------------------------------------------------
# Compile castle-model-viewer and castle-model-converter.
# Compile using our build tool "castle-engine",
# see https://castle-engine.io/build_tool .
# ----------------------------------------------------------------------------

# build castle-model-converter
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.converter.xml

# build castle-model-viewer (after castle-model-converter, so that on Windows we have all DLLs necessary for castle-model-viewer)
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
