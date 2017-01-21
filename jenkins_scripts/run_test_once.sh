#!/bin/bash
set -eu

# Run various tests of view3dscene / tovrmlx3d on a given 3D model.
# 3D model filename is provided as a parameter for this script.
# This script is usually run by run_tests.sh script, see there for some
# comments.

# Disable heaptrc, to get shorter output and avoid reports of harmless leaks
# occuring when ending program with Halt.
# Useful in case you test debug builds of view3dscene/tovrmlx3d
# (that have heaptrc compiled in).
export HEAPTRC=disabled

if [ -f view3dscene ]; then
  VIEW3DSCENE=./view3dscene
else
  # expect view3dscene on $PATH
  VIEW3DSCENE=view3dscene
fi

if [ -f tovrmlx3d ]; then
  TOVRMLX3D=./tovrmlx3d
else
  # expect tovrmlx3d on $PATH
  TOVRMLX3D=tovrmlx3d
fi

# For some tests (by default commented out, see lower in this script)
# you need a 2nd view3dscene binary. Usually you want to use
# some older, stable view3dscene release as "other" below,
# and as $VIEW3DSCENE use newer view3dscene from SVN or nightly snapshots.
#VIEW3DSCENE_OTHER="$VIEW3DSCENE"
VIEW3DSCENE_OTHER=view3dscene-3.10.0-release

OUTPUT_SHORT="$1"
OUTPUT_VERBOSE="$2"
FILE="$3"
shift 3

TEMP_PARTIAL_OUTPUT=/tmp/view3dscene_run_tests_output_partial.txt

test_log ()
{
  echo "----" "$@" >> "${OUTPUT_VERBOSE}"
}

dump_partial ()
{
  cat "${TEMP_PARTIAL_OUTPUT}" >> "${OUTPUT_SHORT}"
  cat "${TEMP_PARTIAL_OUTPUT}" >> "${OUTPUT_VERBOSE}"
}

# Run $1 with "$3...", stdout redirected to $2.
# stderr is captured to correct log file, and we exit in case of failure.
run_command ()
{
  local COMMAND="$1"
  local COMMAND_OUTPUT="$2"
  shift 2

  # Ignore exit status temporarily, to save output and make dump_partial.
  # Only later do "exit" in case of failure.
  set +e
  "$COMMAND" "$@" >  "$COMMAND_OUTPUT" 2> "${TEMP_PARTIAL_OUTPUT}"
  COMMAND_EXIT_STATUS=$?
  set -e
  dump_partial
  if [ "$COMMAND_EXIT_STATUS" '!=' 0 ]; then
    exit 1
  fi
}

run_tovrmlx3d ()
{
  run_command "$TOVRMLX3D" "$@"
}

run_view3dscene ()
{
  run_command "$VIEW3DSCENE" "$@"
}

run_view3dscene_other ()
{
  run_command "$VIEW3DSCENE_OTHER" "$@"
}

# Reading and saving ---------------------------------------------------------

do_read_save ()
{
  # It's important that temp_file is inside the same directory,
  # otherwise re-reading it would not work because relative URLs
  # are broken.
  local TEMP_FILE="`dirname \"$FILE\"`"/test_temporary.wrl

  test_log 'Reading' "$FILE"
  run_tovrmlx3d "$TEMP_FILE" "$FILE" --encoding=classic

  # Check input file and output file headers.
  # They indicate VRML version used to write the file.
  # They should match, otherwise SuggestVRMLVersion of some nodes
  # possibly doesn't work and the resulting file has different VRML version.
  #
  # Note that this test works only with classic VRML files (XML X3D versions,
  # or gzip compressed, have version elsewhere, other 3D formats are converted
  # to any version we like, and output is always classic VRML --- so comparison
  # has no sense).

  # Like ExtractFileExt from $FILE --- get (last) extension, including the dot
  local FILE_EXTENSION=".${FILE##*.}"

  if [ '(' '(' "$FILE_EXTENSION" = '.wrl' ')' -o \
           '(' "$FILE_EXTENSION" = '.x3dv' ')' ')' -a \
       '(' "`basename \"$FILE\"`" != cones_gzipped_but_with_normal_extension.wrl ')' ]; then
    local INPUT_HEADER="`head -n 1 \"$FILE\"`"
    local OUTPUT_HEADER="`head -n 1 \"$TEMP_FILE\"`"

    # trim both headers, to trim possibly different newlines
    # (or maybe they are stripped by ` already?)
    # and whitespace around.
    # local INPUT_HEADER="`stringoper Trim \"$INPUT_HEADER\"`"
    # local OUTPUT_HEADER="`stringoper Trim \"$OUTPUT_HEADER\"`"
    local INPUT_HEADER="$(echo -e "${INPUT_HEADER}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
    local OUTPUT_HEADER="$(echo -e "${OUTPUT_HEADER}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"

    if [ "$INPUT_HEADER" != "$OUTPUT_HEADER" ]; then
      echo 'WARNING: input/output headers differ:'  > "${TEMP_PARTIAL_OUTPUT}"
      echo 'Header on input is' "$INPUT_HEADER"    >> "${TEMP_PARTIAL_OUTPUT}"
      echo 'Header on output is' "$OUTPUT_HEADER"  >> "${TEMP_PARTIAL_OUTPUT}"
      dump_partial
    fi
  fi

  test_log 'Reading again' "$FILE"
  run_tovrmlx3d /dev/null "$TEMP_FILE" --encoding=classic

  rm -f "$TEMP_FILE"
}

# Test reading and writing back 3D file.
# For 3D models in VRML/X3D classic encoding, also check that
# saving back produces the same header (indicating the same VRML/X3D version).
do_read_save

# Saving to file: regressions ------------------------------------------------

do_compare_classic_save ()
{
  local SAVE_CLASSIC_OLD="${FILE%.*}_test_temporary_classic_save_old.x3dv"
  local SAVE_CLASSIC_NEW="${FILE%.*}_test_temporary_classic_save_new.x3dv"

  test_log 'Comparing classic save with' "$VIEW3DSCENE_OTHER"
  run_view3dscene_other "$SAVE_CLASSIC_OLD" "$FILE" --write-to-vrml
  run_view3dscene       "$SAVE_CLASSIC_NEW" "$FILE" --write-to-vrml

  set +e
  diff -w --ignore-blank-lines --unified=0 "$SAVE_CLASSIC_OLD" "$SAVE_CLASSIC_NEW"
  set -e

  rm -f "$SAVE_CLASSIC_OLD" "$SAVE_CLASSIC_NEW"
}

# Uncomment this to compare classic save with other (e.g. older) view3dscene version.
# Uses --write (--write-to-vrml for older view3dscene versions) to save,
# and standard Unix "diff" to compare.
# do_compare_classic_save

# Saving to file: XML validity -------------------------------------------------

do_save_xml_valid ()
{
  if grep --silent '#VRML V1.0 ascii' < "$FILE"; then
    test_log 'Testing is xml valid aborted (VRML 1.0 -> xml not supported)'
  else
    # Like ChangeFileExt $FILE ..., must preserve directory
    local     SAVE_XML="${FILE%.*}_test_temporary_save_xml_valid.x3d"
    local SAVE_CLASSIC="${FILE%.*}_test_temporary_save_xml_valid.x3dv"

    test_log 'Testing is xml valid (can be read back, by tovrmlx3d and xmllint)'
    run_tovrmlx3d "$SAVE_XML"     "$FILE"     --encoding=xml
    run_tovrmlx3d "$SAVE_CLASSIC" "$SAVE_XML" --encoding=classic

    if [ "`basename \"$SAVE_XML\"`" '=' 'chinchilla_with_prt.wrl_test_temporary_save_xml_valid.x3d' -o \
         "`basename \"$SAVE_XML\"`" '=' 'chinchilla_with_prt_rays1000.wrl_test_temporary_save_xml_valid.x3d' -o \
         "`basename \"$SAVE_XML\"`" '=' 'water_no_shaders.x3dv_test_temporary_save_xml_valid.x3d' ]; then

      # Not running xmllint, as it fails with
      # parser error : internal error: Huge input lookup
      # on this file. See https://bugzilla.redhat.com/show_bug.cgi?id=862969
      echo "Not running xmllint on $SAVE_XML" > /dev/null
    else
      set +e
      # We do not test with official DTD or XSD, they are too buggy ---
      # at least for xmllint.  --postvalid
      xmllint --noout "$SAVE_XML"
      # 2>&1 | grep --invert-match 'Content model of ProtoBody is not determinist'
      set -e
    fi

    rm -f "$SAVE_CLASSIC" "$SAVE_XML"
  fi
}

# Test writing and reading back X3D encoded in XML.
# This tests that XML output produces something valid that can be read back.
# Also, test basic generated XML validity by xmllint.
do_save_xml_valid

# Saving to file: XML/classic preserving ---------------------------------------

do_compare_classic_xml_save ()
{
  local SAVE_1_CLASSIC="${FILE%.*}_test_temporary_classic_xml_1.x3dv"
  local     SAVE_2_XML="${FILE%.*}_test_temporary_classic_xml_2.x3d"
  local SAVE_2_CLASSIC="${FILE%.*}_test_temporary_classic_xml_2.x3dv"

  test_log 'Comparing saving to classic vs saving to xml and then classic'
  run_tovrmlx3d "$SAVE_1_CLASSIC" "$FILE"  --force-x3d --encoding=classic
  run_tovrmlx3d "$SAVE_2_XML"     "$FILE"              --encoding=xml
  run_tovrmlx3d "$SAVE_2_CLASSIC" "$SAVE_2_XML"        --encoding=classic

  set +e
  diff --unified=0 "$SAVE_1_CLASSIC" "$SAVE_2_CLASSIC"
  set -e

  rm -f "$SAVE_1_CLASSIC" "$SAVE_2_CLASSIC" "$SAVE_2_XML"
}

# Uncomment this to compare saving to X3D classic with saving to X3D XML,
# reading back this XML and saving to classic.
# This checks that saving + loading xml preserves everything.
# Uses --write to save, and standard Unix "diff" to compare.
#
# This test is unfortunately far from automatic. You need to spend some
# time to filter our harmless differences, occuring for various reasons:
# - meta "source" is different (we could introduce a command-line option
#   to avoid it, but just for this test? Not worth it for now.)
# - reading XML always sorts attribute names (this is a limitation
#   of FPC DOM unit, outside of our reach; and it's there for a good reason
#   (fast log lookup)). So result has some fields ordered differently.
# - Bacause of IS treatment, other things may also be ordered differently.
#
# Although, at least this automatically tests that generated XML is valid
# (can be read back). If it cleaned _test_classic_xml temp files, then at least
# this was OK. But this is also tested by do_save_xml_valid above.
#
# do_compare_classic_xml_save

# Saving to file: view3dscene and tovrmlx3d equal ----------------------------

# Remove META lines from $1
filter_out_generator_meta ()
{
  local TEMP_FILE='test_temporary_filter_out_generator_meta'

  set +e
  grep --invert-match 'META "generator"' "$1" | \
    grep --invert-match '<meta name="generator"' - | \
      grep --invert-match '# Generated by' - > \
        "$TEMP_FILE"
  set -e

  mv -f "$TEMP_FILE" "$1"
}

do_view3dscene_and_tovrmlx3d_equal ()
{
  test_log "Comparing $VIEW3DSCENE and $TOVRMLX3D output"
  # Like ChangeFileExt $FILE ..., must preserve directory
  local VIEW3DSCENE_OUT="${FILE%.*}_test_temporary_view3dscene_and_tovrmlx3d_equal_1"
  local   TOVRMLX3D_OUT="${FILE%.*}_test_temporary_view3dscene_and_tovrmlx3d_equal_2"

  run_view3dscene "$VIEW3DSCENE_OUT" "$FILE" --write
  run_tovrmlx3d   "$TOVRMLX3D_OUT" "$FILE"
  filter_out_generator_meta "$VIEW3DSCENE_OUT"
  filter_out_generator_meta "$TOVRMLX3D_OUT"
  diff "$VIEW3DSCENE_OUT" "$TOVRMLX3D_OUT"

  run_view3dscene "$VIEW3DSCENE_OUT" "$FILE" --write --write-encoding=xml
  run_tovrmlx3d   "$TOVRMLX3D_OUT"   "$FILE"               --encoding=xml
  filter_out_generator_meta "$VIEW3DSCENE_OUT"
  filter_out_generator_meta "$TOVRMLX3D_OUT"
  diff "$VIEW3DSCENE_OUT" "$TOVRMLX3D_OUT"

  run_view3dscene "$VIEW3DSCENE_OUT" "$FILE" --write --write-force-x3d
  run_tovrmlx3d   "$TOVRMLX3D_OUT"   "$FILE"               --force-x3d
  filter_out_generator_meta "$VIEW3DSCENE_OUT"
  filter_out_generator_meta "$TOVRMLX3D_OUT"
  diff "$VIEW3DSCENE_OUT" "$TOVRMLX3D_OUT"

  rm -f "$VIEW3DSCENE_OUT" "$TOVRMLX3D_OUT"
}

# Test that view3dscene and tovrmlx3d generate the same output
# do_view3dscene_and_tovrmlx3d_equal

# Screenshots comparison -----------------------------------------------------

do_compare_screenshot ()
{
  local SCREENSHOT_OLD="${FILE%.*}_test_temporary_screen_old.png"
  local SCREENSHOT_NEW="${FILE%.*}_test_temporary_screen_new.png"

  local DELETE_SCREENSHOTS='t'

  test_log 'Rendering and making screenshot' "$VIEW3DSCENE_OTHER"
  "$VIEW3DSCENE_OTHER" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_OLD"

  test_log 'Comparing screenshot' "$VIEW3DSCENE"
  "$VIEW3DSCENE" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_NEW"

  # Don't exit on screenshot comparison fail. That's because
  # taking screenshots takes a long time, so just continue checking.
  # The caller will have to check script output to know if something failed.
  # (Also the comparison images will be left, unrecognized
  # by version control system, for cases that failed.)
  if image_compare "$SCREENSHOT_OLD" "$SCREENSHOT_NEW"; then
    true # do nothing
  else
    DELETE_SCREENSHOTS=''
  fi

  if [ -n "$DELETE_SCREENSHOTS" ]; then
    rm -f "$SCREENSHOT_OLD"
    rm -f "$SCREENSHOT_NEW"
  fi
}

# Uncomment this to generate screenshots,
# and compare them with other (e.g. older) view3dscene version.
# Uses --screenshot to capture, and image_compare to compare
# (compile and put on $PATH this:
# ../castle_game_engine/examples/images_videos/image_compare.lpr).
#
# This tests screenshot generation goes Ok,
# and checks for any regressions in the renderer.
#
# The output (_test_temporary_screen*.png files that were not removed)
# should be examined manually afterwards. Note that many differences should
# be ignored (OpenGL is not guaranteed to render the same scene pixel-by-pixel
# the same if some state is different).
#
# do_compare_screenshot
