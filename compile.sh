#! /bin/sh
set -eu

# Compile view3dscene and tovrmlx3d.
# Compile using our build tool "castle-engine",
# see https://github.com/castle-engine/castle-engine/wiki/Build-Tool.

# build tovrmlx3d
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.tovrmlx3d.xml

# build view3dscene
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
