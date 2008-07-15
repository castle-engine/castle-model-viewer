#!/bin/bash
set -eu

# This script is usually run by run_tests.sh script, see there for some
# comments.

# You can disable heaptrc for shorter output (useful only for debug builds
# that have heaptrc compiled in)
export HEAPTRC=disabled

FILE="$1"

# It's important that temp_file is inside the same directory,
# otherwise re-reading it would not work because relative URLs
# are broken.
TEMP_FILE=`dirname "$FILE"`/view3dscene_test_temporary_file.wrl

echo '---- Reading' "$FILE"
#echo '(temp is ' "$TEMP_FILE" ')'
view3dscene "$FILE" --write-to-vrml > "$TEMP_FILE"

# Check input file and output file headers.
# They indicate VRML version used to write the file.
# They should match, otherwise SuggestVRMLVersion of some nodes
# possibly doesn't work and the resulting file has different VRML version.
#
# Note that this test works only with classic VRML files (XML X3D versions,
# or gzip compressed, have version elsewhere, other 3D formats are converted
# to any version we like, and output is always classic VRML --- so comparison
# has no sense).

FILE_EXTENSION=`stringoper ExtractFileExt "$FILE"`

if [ '(' '(' "$FILE_EXTENSION" = '.wrl' ')' -o \
         '(' "$FILE_EXTENSION" = '..x3dv' ')' ')' -a \
     '(' `basename "$FILE"` != cones_gzipped_but_with_normal_extension.wrl ')' ]; then
  INPUT_HEADER=`head -n 1 "$FILE"`
  OUTPUT_HEADER=`head -n 1 "$TEMP_FILE"`

  # trim both headers, to trim possibly different newlines
  # (maybe they are stripped by ` already?)
  # and whitespace around.
  INPUT_HEADER="`stringoper Trim \"$INPUT_HEADER\"`"
  OUTPUT_HEADER="`stringoper Trim \"$OUTPUT_HEADER\"`"

  if [ "$INPUT_HEADER" != "$OUTPUT_HEADER" ]; then
    echo 'WARNING: input/output headers differ:'
    echo 'Header on input is' "$INPUT_HEADER"
    echo 'Header on output is' "$OUTPUT_HEADER"
  fi
fi

echo '---- Reading again' "$FILE"
view3dscene "$TEMP_FILE" --write-to-vrml > /dev/null

rm -f "$TEMP_FILE"

mk_screnshot ()
{
  echo '---- Rendering and making screenshot' "$@"
  TEMP_SCREENSHOT=/tmp/view3dscene_test_screenshot.png
  view3dscene "$FILE" --screenshot 0 "$TEMP_SCREENSHOT" "$@"
  # display "$TEMP_SCREENSHOT"
  rm -f "$TEMP_SCREENSHOT"
}

# Uncomment these for much longer test (this does --screenshot,
# testing actual rendering of the scene)

# mk_screnshot --renderer-optimization none
# mk_screnshot --renderer-optimization scene-as-a-whole
# mk_screnshot --renderer-optimization separate-shape-states
# mk_screnshot --renderer-optimization separate-shape-states-no-transform
