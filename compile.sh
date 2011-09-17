#! /bin/sh
set -eu

# Call FPC with proper command-line options to compile view3dscene
# and related programs here.
#
# We must do cd ../castle_game_engine/ (and call FPC from that directory)
# because kambi.cfg file is there and it contains paths relative
# to that directory.

cd ../castle_game_engine/

# Force rebuilding GLWindow unit with proper backend.
make clean-glwindow

fpc -dRELEASE "${KAMBI_FPC_OPTIONS:-}" @kambi.cfg ../view3dscene/view3dscene.lpr
fpc -dRELEASE "${KAMBI_FPC_OPTIONS:-}" @kambi.cfg ../view3dscene/tovrmlx3d.lpr
