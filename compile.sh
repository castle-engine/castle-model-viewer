#! /bin/sh
set -eu

# This is automatically generated script that should compile
# all programs in this archive. It simply calls FPC
# with proper command-line options.
#
# We must do cd ../kambi_vrml_game_engine/ (and call FPC from that directory)
# because kambi.cfg file is there and it contains paths relative
# to that directory.

cd ../kambi_vrml_game_engine/

# Force rebuilding GLWindow unit with proper backend.
make clean-glwindow

fpc -dRELEASE "${KAMBI_FPC_OPTIONS:-}" @kambi.cfg ../view3dscene/view3dscene.lpr
#ppcx64 -dRELEASE "${KAMBI_FPC_OPTIONS:-}" @kambi.cfg ../view3dscene/view3dscene.lpr
