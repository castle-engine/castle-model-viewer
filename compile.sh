#! /bin/sh
set -eu

# Call FPC with proper command-line options to compile view3dscene
# and related programs here.
#
# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because castle-fpc.cfg file is there and it contains paths relative
# to that directory.

cd ../castle_game_engine/

# Force rebuilding CastleWindow unit with proper backend.
make clean-window

# fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/view3dscene.lpr
# Compile view3dscene binary using castle-engine,
# this is good for Windows to include the icon/versioninfo/manfest in resources.
cd ../view3dscene/
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
cd ../castle_game_engine/

fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/tovrmlx3d.lpr
