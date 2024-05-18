#!/bin/bash
set -eu

# -------------------------------------------------------------------------
# Run various tests of castle-model-viewer / castle-model-converter on a given model.
# Model filename is provided as a parameter for this script.
# This script is usually run by run_tests_on_dir.sh script, see there for some
# comments.
#
# Tip: To test manually how it works on a single file, you can use this:
#   $ cd castle-model-viewer/
#   $ rm -f output-short.txt output-verbose.txt
#   $ ./jenkins_scripts/run_test_on_model.sh output-short.txt output-verbose.txt ../demo-models/texturing_advanced/warnings/tex3d_composed_warnings.x3dv
#   ... and consult output-short.txt output-verbose.txt output
#
# This bash script uses settings similar to bash strict mode, for similar reasons,
# see http://redsymbol.net/articles/unofficial-bash-strict-mode/
# ----------------------------------------------------------------------------

# Disable FPC heaptrc, to get shorter output and avoid reports of harmless leaks
# occuring when ending program with Halt.
# Useful in case you test debug builds of castle-model-viewer/castle-model-converter
# (that have heaptrc compiled in).
export HEAPTRC=disabled

if [ -f castle-model-viewer ]; then
  VIEWER=./castle-model-viewer
else
  # expect castle-model-viewer on $PATH
  VIEWER=castle-model-viewer
fi

if [ -f castle-model-converter ]; then
  CONVERTER=./castle-model-converter
else
  # expect castle-model-converter on $PATH
  CONVERTER=castle-model-converter
fi

# For some tests you need a 2nd castle-model-viewer/castle-model-converter binary.
# Usually you want to use some older, stable release as "other" below,
# and as $VIEWER use newer release.
# Undefined by default, all tests using it are commented out by default,
# since this needs special preparations on the user side to make sense.
#
#VIEWER_OTHER=castle-model-viewer-3.10.0-release
#CONVERTER_OTHER=castle-model-converter-3.10.0-release

OUTPUT_SHORT="$1"
OUTPUT_VERBOSE="$2"
FILE="$3"
shift 3

test_log ()
{
  echo "----" "$@" >> "${OUTPUT_VERBOSE}"
}

# Append $@ to both short and verbose log outputs.
# We just pass $@ to echo, which adds by default a newline at end.
# Pass -n to avoid it.
add_to_both_logs ()
{
  echo "$@" >> "${OUTPUT_SHORT}"
  echo "$@" >> "${OUTPUT_VERBOSE}"
}

# Run $1 with "$3...", stdout redirected to $2.
# stderr is captured to correct log file, and we exit in case of failure.
run_command ()
{
  local COMMAND="$1"
  local COMMAND_OUTPUT="$2"
  shift 2

  # Ignore exit status temporarily,
  # to add COMMAND_ERRORS to both logs using add_to_both_logs.
  # Only later do "exit" in case of failure.
  set +e
  COMMAND_ERRORS=$("$COMMAND" "$@" 2>&1 > "$COMMAND_OUTPUT")
  COMMAND_EXIT_STATUS=$?
  set -e

  # Do not print anything if COMMAND_ERRORS is empty,
  # but if it is not empty -> print COMMAND_ERRORS with following newline
  # (since $(...) ate the trailing newlines, but we want to write them to log).
  if [ -n "${COMMAND_ERRORS}" ]; then
    add_to_both_logs "${COMMAND_ERRORS}"
  fi

  if [ "$COMMAND_EXIT_STATUS" '!=' 0 ]; then
    exit 1
  fi
}

run_converter ()
{
  run_command "$CONVERTER" "$@"
}

run_converter_other ()
{
  run_command "$CONVERTER_OTHER" "$@"
}

run_viewer ()
{
  run_command "$VIEWER" "$@"
}

run_viewer_other ()
{
  run_command "$VIEWER_OTHER" "$@"
}

# Reading and saving ---------------------------------------------------------

do_read_save ()
{
  # It's important that temp_file is inside the same directory,
  # otherwise re-reading it would not work because relative URLs
  # are broken.
  local TEMP_FILE="`dirname \"$FILE\"`"/test_temporary.wrl

  test_log 'Reading' "$FILE"
  run_converter "$TEMP_FILE" "$FILE" --stdout-url=out.x3dv

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
      add_to_both_logs "WARNING: input/output headers differ:
Header on input is ${INPUT_HEADER}
Header on output is ${OUTPUT_HEADER}"
    fi
  fi

  test_log 'Reading again' "$FILE"
  run_converter /dev/null "$TEMP_FILE" --stdout-url=out.x3dv

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

  test_log 'Comparing classic save with' "$VIEWER_OTHER"
  run_converter_other "$SAVE_CLASSIC_OLD" "$FILE" --stdout-url=out.x3dv
  run_converter       "$SAVE_CLASSIC_NEW" "$FILE" --stdout-url=out.x3dv

  set +e
  diff -w --ignore-blank-lines --unified=0 "$SAVE_CLASSIC_OLD" "$SAVE_CLASSIC_NEW"
  set -e

  rm -f "$SAVE_CLASSIC_OLD" "$SAVE_CLASSIC_NEW"
}

# Uncomment this to compare classic save with other (e.g. older) castle-model-viewer version.
# Uses standard Unix "diff" to compare.
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

    test_log 'Testing is xml valid (can be read back, by castle-model-converter and xmllint)'
    run_converter "$SAVE_XML"     "$FILE"     --stdout-url=out.x3d
    run_converter "$SAVE_CLASSIC" "$SAVE_XML" --stdout-url=out.x3dv

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
  run_converter "$SAVE_1_CLASSIC" "$FILE"       --stdout-url=out.x3dv
  run_converter "$SAVE_2_XML"     "$FILE"       --stdout-url=out.x3d
  run_converter "$SAVE_2_CLASSIC" "$SAVE_2_XML" --stdout-url=out.x3dv

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

# Screenshots comparison -----------------------------------------------------

do_compare_screenshot ()
{
  local SCREENSHOT_OLD="${FILE%.*}_test_temporary_screen_old.png"
  local SCREENSHOT_NEW="${FILE%.*}_test_temporary_screen_new.png"

  local DELETE_SCREENSHOTS='t'

  test_log 'Rendering and making screenshot' "$VIEWER_OTHER"
  "$VIEWER_OTHER" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_OLD"

  test_log 'Comparing screenshot' "$VIEWER"
  "$VIEWER" "$FILE" --screenshot 0 --geometry 300x200 "$SCREENSHOT_NEW"

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
# and compare them with other (e.g. older) castle-model-viewer version.
# Uses --screenshot to capture, and image_compare to compare
# (compile and put on $PATH this:
# ../castle_game_engine/examples/images_videos/image_compare.dpr).
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
