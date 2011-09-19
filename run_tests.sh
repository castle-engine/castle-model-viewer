#!/bin/bash
set -eu

# Run view3dscene / tovrmlx3d tests on 3D models inside ../demo_models/
# (and some other dirs, see lower).
# You can checkout demo_models from SVN, or download and unpack
# from [http://castle-engine.sourceforge.net/demo_models.php],
# such that it's a sibling of view3dscene.
# This is designed to work with any 3D models, so feel free to use this
# on your own private 3D models (add "test_dir" call lower,
# or just call run_test_once.sh explicitly with your 3D model filename).
#
# Requires at least:
# - view3dscene and tovrmlx3d binaries compiled (either in current dir,
#   or on the $PATH, see run_test_once.sh)
# - stringoper binary compiled and available on $PATH
#   (stringoper comes from ../castle_game_engine/examples/tools/stringoper.lpr)
# Some optional tests require also:
# - image_compare binary compiled and available on $PATH,
#   only if you uncomment the "screenshot comparison" test in run_test_once.sh
#   (image_compare comes from ../castle_game_engine/examples/images_videos/image_compare.lpr)
# - xmllint for basic XML validation (should be included in any
#   popular Linux distribution).
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

test_dir()
{
  set +e

  # Explanation for some omitted files/dirs:
  # - errors subdirs in demo_models contains files that *should* fail when reading.
  # - *test_temporary* files are leftovers from interrupted previous tests.
  # - ssao tests are not used for now, because fglrx is awfully slow
  #   for them (like 1 frame for ~couple of minutes, using full cpu power;
  #   seems like it falls back to software shaders implementation for this case).

  # The find output is run through sort and then xargs.
  # Otherwise, find prints files in an unpredictable order (from readdir),
  # which makes comparing two outputs from two different systems (like
  # comparing run_tests_valid_output.txt with output on michalis.ii snapshots)
  # impossible. Fortunately, find and sort goes very fast (compared to actual
  # run_test_once.sh time).

  find "$1" \
  '(' -type d -iname 'errors' -prune ')' -or \
  '(' -type f -name '*test_temporary*' ')' -or \
  '(' -type f -name 'ssao_barna29_0.x3dv' ')' -or \
  '(' -type f -name 'ssao_stairs_with_test_plane.x3dv' ')' -or \
  '(' -type f -name 'ssao_stairs.x3dv' ')' -or \
  '(' -type f -name 'twoboxes_ssao.x3dv' ')' -or \
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
  xargs -0 --max-args=1 ./run_test_once.sh
  set -e
}

test_dir ../demo_models/
test_dir ../castle/data/
test_dir ../rift/data/

# This dir has fallback_prototypes in VRML 97 and X3D, two really important
# files that should be correct.
test_dir ../www/htdocs/

# Extreme test on all models within repository
# test_dir ../
