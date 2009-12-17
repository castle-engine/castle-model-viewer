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

# This program uses GLWindow unit. GLWindow unit may be compiled
# with various back-ends (e.g. under Unices two most useful back-ends
# are XLIB and GTK). To make sure that compilation of this program
# will produce exactly what you need, below we make sure that
# unit GLWindow will be *always* *rebuild*.
#
# Of course this means that compilation time will suffer a little,
# since GLWindow unit will be possibly rebuild without any real need.
# Comment out line below if you want.
rm -f glwindow/glwindow.o \
      glwindow/glwindow.ppu \
      glwindow/GLWindow.o \
      glwindow/GLWindow.ppu

fpc -dRELEASE "${KAMBI_FPC_OPTIONS:-}" @kambi.cfg ../view3dscene/view3dscene.pasprogram
