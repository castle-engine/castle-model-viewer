#!/bin/bash
set -eu

# Run view3dscene tests on ../demo_models/ (and some other dirs).
# You can checkout demo_models from SVN, or download and unpack
# from [http://vrmlengine.sourceforge.net/demo_models.php],
# such that it's a sibling of view3dscene.
#
# This requires
# - view3dscene binary compiled (either in current dir,
#   or on the $PATH, see run_test_once.sh script setting VIEW3DSCENE variable)
# - stringoper binary compiled and available on $PATH
#   (stringoper comes from ../kambi_vrml_game_engine/examples/base/stringoper.lpr)
# - image_compare binary compiled and available on $PATH,
#   only if you uncomment the "screenshot comparison" test in run_test_once.sh
#   (image_compare comes from ../kambi_vrml_game_engine/examples/images/image_compare.lpr)
#
# Every suitable 3D model format is tested by the run_test_once.sh script:
# - It reads a model, writes it back to VRML file, and then reads again.
#   This somewhat tests that there are no problems with parsing
#   and saving VRML (and other) files.
# - Then we check rendering abilities, by making screenshots
#   and comparing (you can compare against the same view3dscene
#   or against some previous stable view3dscene version).
#   (This test is commented out by default, as it takes a lot of time...)
#
# Doesn't enter 'errors' subdir (for demo_models,
# it contains files that *should* fail when reading).
#
# Also doesn't try to check view3dscene_test_temporary_file.wrl,
# this may be leftover from interrupted previous test (it would cause
# errors that are not interesting), it will be removed by run_test_once.sh
# inside anyway.
#
# Also ssao tests are not used for now, because fglrx is awfully slow
# for them (like 1 frame for ~couple of minutes, using full cpu power;
# seems like it falls back to software shaders implementation for this case).

test_dir()
{
  set +e
  find "$1" \
  '(' -type d -iname 'errors' -prune ')' -or \
  '(' -type f -name 'view3dscene_test_temporary_file.wrl' ')' -or \
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
              -exec ./run_test_once.sh '{}' ';' ')'
  set -e
}

test_dir ../demo_models/
test_dir ../vrml_engine_doc/
test_dir ../castle/data/
test_dir ../rift/data/

# This dir has fallback_prototypes in VRML 97 and X3D, two really important
# files that should be correct.
test_dir ../www/htdocs/

# Extreme test on all models within vrmlengine repository
# test_dir ../
