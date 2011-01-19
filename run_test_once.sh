#!/bin/bash
set -eu

# This script is usually run by run_tests.sh script, see there for some
# comments.

# You can disable heaptrc for shorter output (useful only for debug builds
# that have heaptrc compiled in)
export HEAPTRC=disabled

if [ -f view3dscene ]; then
  VIEW3DSCENE=./view3dscene
else
  # expect view3dscene on $PATH
  VIEW3DSCENE=view3dscene
fi

# If you enable screenshot comparison (lower in this script),
# this will be used to make a reference (considered "correct") screenshot,
# that will be compared with others. It may be just the same view3dscene
# version, or it may be some other stable version.
VIEW3DSCENE_FOR_CORRECT_SCREENSHOT="$VIEW3DSCENE"
#VIEW3DSCENE_FOR_CORRECT_SCREENSHOT=view3dscene-3.8.0-release

FILE="$1"

# It's important that temp_file is inside the same directory,
# otherwise re-reading it would not work because relative URLs
# are broken.
TEMP_FILE=`dirname "$FILE"`/view3dscene_test_temporary_file.wrl

echo '---- Reading' "$FILE"
#echo '(temp is ' "$TEMP_FILE" ')'
"$VIEW3DSCENE" "$FILE" --write-to-vrml > "$TEMP_FILE"

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
"$VIEW3DSCENE" "$TEMP_FILE" --write-to-vrml > /dev/null

rm -f "$TEMP_FILE"

# Screenshot comparison ------------------------------------------------------

SCREENSHOT_CORRECT=`stringoper ChangeFileExt "$FILE" _test_screen.png`
SCREENSHOT_COMPARE=`stringoper ChangeFileExt "$FILE" _test_screen_compare.png`

mk_screnshot ()
{
  echo '---- Rendering and making screenshot' "$VIEW3DSCENE_FOR_CORRECT_SCREENSHOT" "$@"
  "$VIEW3DSCENE_FOR_CORRECT_SCREENSHOT" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_CORRECT" "$@"
}

compare_screenshot ()
{
  echo '---- Comparing screenshot' "$VIEW3DSCENE" "$@"
  "$VIEW3DSCENE" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_COMPARE" "$@"

  # Don't exit of screenshot comparison fail. That's because
  # taking screenshots takes a long time, so just continue checking.
  # The caller will have to check script output to know if something failed.
  set +e
  image_compare "$SCREENSHOT_CORRECT" "$SCREENSHOT_COMPARE"
  set -e
}

# Uncomment these for much longer test (this does --screenshot,
# testing actual rendering of the scene)

# mk_screnshot --renderer-optimization none
# compare_screenshot --renderer-optimization none
# compare_screenshot --renderer-optimization scene-display-list
# compare_screenshot --renderer-optimization shape-display-list

rm -f "$SCREENSHOT_CORRECT"
rm -f "$SCREENSHOT_COMPARE"
