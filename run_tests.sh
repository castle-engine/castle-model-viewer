#!/bin/bash
set -eu

# Run view3dscene tests on ../kambi_vrml_test_suite/ (and some other dirs).
# You can checkout kambi_vrml_test_suite from SVN, or download and unpack
# from [http://vrmlengine.sourceforge.net/kambi_vrml_test_suite.php],
# such that it's a sibling of view3dscene.
#
# Tests are fully automatic.
#
# Every suitable 3D model format is read,
# and written back to VRML. Then this written result is read back.
# This somewhat makes sure that there are no problems with parsing
# and saving VRML (and other) files.
#
# Doesn't enter 'errors' subdir (for kambi_vrml_test_suite,
# it contains files that *should* fail when reading).

test_dir()
{
  set +e
  find "$1" \
  '(' -type d -iname 'errors' -prune ')' -or \
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

test_dir ../kambi_vrml_test_suite/
test_dir ../vrml_engine_doc/examples/
test_dir ../castle/data/
test_dir ../rift/data/
