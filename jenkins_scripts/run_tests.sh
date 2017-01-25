#!/bin/bash
set -eu

# Run view3dscene / tovrmlx3d tests on 3D models inside the ../ directory.
#
# We assume that ../ directory contains various Castle Game Engine-related
# projects, like demo-models from https://github.com/castle-engine/demo-models .
# You can either clone multiple GIT repositories alongside the view3dscene repository,
# or you can grab the SVN trunk with various CGE projects
# from https://sourceforge.net/projects/castle-engine/ .
#
# This script is designed to work with any 3D models, so feel free to use this
# on your own private 3D models. Just modify the "test_dir" calls at the bottom.
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
#   For example, if you test view3dscene from SVN, you may also install last
#   stable view3dscene release. Just rename the binary (like view3dscene-3.10.0-release),
#   and uncomment appropriate parts of run_test_once.sh.
#
# Every suitable 3D model format is tested by the run_test_once.sh script:
# - It reads a model, writes it back to file, and then reads again.
#   This somewhat tests that there are no problems with parsing
#   and saving 3D files.
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

  find "$1" \
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
                  -iname '*.dae' ')' \
              -print0 ')' | \
  sort --zero-terminated | \
  xargs -0 --max-args=1 jenkins_scripts/run_test_once.sh "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"
  set -e
}

OUTPUT_SHORT="$1"
OUTPUT_VERBOSE="$2"
shift 2

rm -f "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"

# test_dir ../demo_models/
# test_dir ../castle/data/
# test_dir ../castle_game_engine/
# This dir has fallback_prototypes in VRML 97 and X3D, two really important
# files that should be correct.
# test_dir ../www/htdocs/

# Just test all models within ../
test_dir ../
