#! /bin/sh
set -eu

# Compile view3dscene and tovrmlx3d.

# Force rebuilding CastleWindow unit with proper backend.
cd ../castle_game_engine/
make --quiet clean-window
cd ../view3dscene/

# fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/code/view3dscene.lpr
# Compile view3dscene binary using castle-engine,
# this is good for Windows to include the icon/versioninfo/manfest in resources.
castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}

if which castle-engine  > /dev/null; then
  cd code/
  castle-engine simple-compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} tovrmlx3d.lpr
  cd ../
else
  # We must do cd ../castle_game_engine/ (and call FPC from that directory)
  # because castle-fpc.cfg file is there and it contains paths relative
  # to that directory.
  cd ../castle_game_engine/
  fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/code/tovrmlx3d.lpr
  cd ../view3dscene/
fi

# move tovrmlx3d binaries up
if [ -f code/tovrmlx3d ]; then
  mv -f code/tovrmlx3d .
fi
if [ -f code/tovrmlx3d.exe ]; then
  mv -f code/tovrmlx3d.exe .
fi
