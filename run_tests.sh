#!/bin/bash
set -eu

# Run view3dscene tests on ../kambi_vrml_test_suite/
# (you should checkout this from SVN, or download and unpack
# from [http://vrmlengine.sourceforge.net/kambi_vrml_test_suite.php],
# such that it's a sibling of view3dscene).
#
# Tests are fully automatic.
#
# Every suitable 3D model format is read,
# and written back to VRML. Then this written result is read back.
# This somewhat makes sure that there are no problems with parsing
# and saving VRML (and other) files.

# Doesn't enter 'errors' subdir (it contains files that should fail
# when reading).

find ../kambi_vrml_test_suite/ \
     ../vrml_engine_doc/examples/ \
     ../castle/data/ \
     ../rift/data/ \
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
                  -iname '*.dae'-or \
                  -iname '*.kanim' ')' \
              -exec ./run_test_once.sh '{}' ';' ')'
