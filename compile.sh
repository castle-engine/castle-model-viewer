#! /bin/sh
set -eu

# Compile view3dscene and tovrmlx3d.

if [ -d ../castle_game_engine ]; then
  # Force rebuilding CastleWindow unit with proper backend.
  cd ../castle_game_engine/
  make --quiet clean-window
  cd ../view3dscene/
fi

if which castle-engine  > /dev/null; then
  # If possible, compile using our build tool "castle-engine",
  # see https://github.com/castle-engine/castle-engine/wiki/Build-Tool.

  # This is good for Windows view3dscene,
  # to include the icon/versioninfo/manfest in resources.
  # It's also a little simpler.
  castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
  cd code/
  castle-engine simple-compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} tovrmlx3d.lpr
  cd ../

  # Move tovrmlx3d binaries up.
  #
  # Note: move "xxx.exe" first, before the Unix binary "xxx".
  # Otherwise, this fails on Windows + Cygwin, if the test program "["
  # is from Cygwin (so it detects that "xxx" exists when in fact it's "xxx.exe"),
  # while "mv" is from MinGW (distributed by FPC too), so it fails to find
  # extension-less "xxx".
  if [ -f code/tovrmlx3d.exe ]; then
    mv -f code/tovrmlx3d.exe .
  fi
  if [ -f code/tovrmlx3d ]; then
    mv -f code/tovrmlx3d .
  fi
else
  # We must do cd ../castle_game_engine/ (and call FPC from that directory)
  # because castle-fpc.cfg file is there and it contains paths relative
  # to that directory.
  cd ../castle_game_engine/
  fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/code/view3dscene.lpr
  fpc -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg ../view3dscene/code/tovrmlx3d.lpr
  cd ../view3dscene/

  # move binaries up
  if [ -f code/view3dscene.exe ]; then
    mv -f code/view3dscene.exe .
  fi
  if [ -f code/view3dscene ]; then
    mv -f code/view3dscene .
  fi
  if [ -f code/tovrmlx3d.exe ]; then
    mv -f code/tovrmlx3d.exe .
  fi
  if [ -f code/tovrmlx3d ]; then
    mv -f code/tovrmlx3d .
  fi
fi
