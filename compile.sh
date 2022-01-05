#! /bin/sh
set -eu

# Compile view3dscene and tovrmlx3d.
# Compile using our build tool "castle-engine",
# see https://castle-engine.io/build_tool.

# build tovrmlx3d
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} --manifest-name=CastleEngineManifest.tovrmlx3d.xml

# build view3dscene (after tovrmlx3d, so that on Windows we have all DLLs necessary for view3dscene)
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
