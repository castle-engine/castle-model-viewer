#!/bin/bash
set -eu

# Build view3dscene and tovrmlx3d,
# run various tests.
#
# This script uses some commands and assumes environment of our Jenkins on
# https://jenkins.castle-engine.io/ .

. /usr/local/fpclazarus/bin/setup.sh default
./compile.sh
jenkins_scripts/run_tests.sh /tmp/view3dscene_run_tests_output.txt /tmp/view3dscene_run_tests_output_verbose.txt

# remove OpenAL trash from outpt
sed --in-place=.bak \
  -e '/ALSA lib/d' \
  -e '/AL lib/d' \
  -e '/jack server is not running or cannot be started/d' \
  -e '/JackShmReadWritePtr/d' \
  -e '/Cannot connect to server/d' \
  /tmp/view3dscene_run_tests_output.txt

# replace current dir (present in some output messages) with string "DIR",
# to make the result reproducible, regardless of the current directory.
REPOSITORY_DIR="`pwd`"
REPOSITORY_DIR="`dirname \"$REPOSITORY_DIR\"`"
sed --in-place=.bak2 -e "s|${REPOSITORY_DIR}|DIR|g" /tmp/view3dscene_run_tests_output.txt

# compare with last correct output
diff -u /tmp/view3dscene_run_tests_output.txt jenkins_scripts/run_tests_valid_output.txt
rm -f /tmp/view3dscene_run_tests_output.txt
