#!/bin/bash
set -eu

# Run view3dscene / tovrmlx3d tests on models inside the given directories.
# Use with any directory of test models,
# like CGE demo-models, castle-engine/tests/, cge-www/htdocs/ .
#
# Arguments:
# $1 - short output file
# $2 - short output file
# $3 and more - directories with models to test
#
# Running this requires:
# - view3dscene and tovrmlx3d binaries compiled (either in current dir,
#   or on the $PATH, see run_test_once.sh)
#
# Some optional tests require also:
#
# - image_compare binary compiled and available on $PATH,
#   only if you uncomment the "screenshot comparison" test in run_test_once.sh
#   (image_compare comes from ../castle_game_engine/examples/images_videos/image_compare.lpr)
#
# - xmllint for basic XML validation (should be included in any
#   popular Linux distribution; on Debian (and maybe derivatives),
#   xmllint may be found in package libxml2-utils, see "dpkg -S xmllint").
#
# - for some optional comparisons (regression checking), it's useful
#   to have alternative view3dscene / tovrmlx3d binaries installed.
#   For example, if you test unstable view3dscene, you may also install last
#   stable view3dscene release. Just rename the binary (like view3dscene-stable-release),
#   and uncomment appropriate parts of run_test_once.sh.
#
# Every suitable model format is tested by the run_test_once.sh script:
# - It reads a model, writes it back to file, and then reads again.
#   This somewhat tests that there are no problems with parsing
#   and saving models.
# - Various other reading and writing validation is done by comparing
#   outputs going through various (classic and XML) encodings.
# - Check rendering, by making screenshots
#   and comparing (against the same view3dscene
#   or against some previous stable view3dscene version).
#
# Some of the tests are commented out by default in run_test_once.sh,
# as they require more time or have to be interpreted manually (to filter
# out some harmless warnings). Default tests are quick and fully automatic.

# `sort' results are affected by the current locale (see man sort).
# We want them to be predictable, the same on every system,
# to make comparing two outputs (like run_tests_valid_output.txt) reliable.
export LANG=C
export LC_COLLATE=C

test_dir()
{
  set +e

  # Explanation for some omitted files/dirs:
  # - errors subdirs in demo_models contains files that *should* fail when reading.
  # - *test_temporary* files are leftovers from interrupted previous tests.
  # - ios_tests/CastleEngineTest/CastleEngineTest contains VRML with .wrl extension
  #   but gzip compressed, out test script is not prepared for it now.

  # The "find" output is run through sort and then xargs.
  # Otherwise, find prints files in an unpredictable order (from readdir),
  # which makes comparing two outputs from two different systems (like
  # comparing run_tests_valid_output.txt with output on michalis.ii snapshots)
  # impossible. Fortunately, find and sort goes very fast (compared to actual
  # run_test_once.sh time).

  find "$@" \
  '(' -type d -iname 'errors' -prune ')' -or \
  '(' -type f -iwholename '*ios_tests/CastleEngineTest/CastleEngineTest*' ')' -or \
  '(' -type f -name '*test_temporary*' ')' -or \
  '(' -type f '(' -iname '*.wrl' -or \
                  -iname '*.wrz' -or \
                  -iname '*.wrl.gz' -or \
                  -iname '*.x3d' -or \
                  -iname '*.x3dz' -or \
                  -iname '*.x3d.gz' -or \
                  -iname '*.x3dv' -or \
                  -iname '*.x3dvz' -or \
                  -iname '*.x3dv.gz' -or \
                  -iname '*.3ds' -or \
                  -iname '*.dae' -or \
                  -iname '*.gltf' -or \
                  -iname '*.glb' -or \
                  -iname '*.plist' -or \
                  -iname '*.starling-xml' ')' \
              -print0 ')' | \
  sort --zero-terminated | \
  xargs -0 --max-args=1 jenkins_scripts/run_test_once.sh "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"
  set -e
}

OUTPUT_SHORT="$1"
OUTPUT_VERBOSE="$2"
shift 2

rm -f "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"

test_dir "$@"
