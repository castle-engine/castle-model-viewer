#!/bin/bash
set -eu

# ----------------------------------------------------------------------------
# Run castle-model-viewer / castle-model-converter tests on models inside the given directories.
# Use with any directory of test models,
# like CGE demo-models, castle-engine/tests/, cge-www/htdocs/ .
#
# Arguments:
# $1 - short output file
# $2 - short output file
# $3 and more - directories with models to test
#
# Running this requires:
# - castle-model-viewer and castle-model-converter binaries compiled (either in current dir,
#   or on the $PATH, see run_test_on_model.sh)
#
# Some optional tests (done by run_test_on_model.sh) require also:
#
# - image_compare binary compiled and available on $PATH,
#   only if you uncomment the "screenshot comparison" test in run_test_on_model.sh
#   (image_compare comes from ../castle_game_engine/examples/images_videos/image_compare.dpr)
#
# - xmllint for basic XML validation (should be included in any
#   popular Linux distribution; on Debian (and maybe derivatives),
#   xmllint may be found in package libxml2-utils, see "dpkg -S xmllint").
#
# - for some optional comparisons (regression checking), it's useful
#   to have alternative castle-model-viewer / castle-model-converter binaries installed.
#   For example, if you test unstable castle-model-viewer, you may also install last
#   stable castle-model-viewer release. Just rename the binary (like castle-model-viewer-stable-release),
#   and uncomment appropriate parts of run_test_on_model.sh.
#
# Every suitable model format is tested by the run_test_on_model.sh script:
# - It reads a model, writes it back to file, and then reads again.
#   This somewhat tests that there are no problems with parsing
#   and saving models.
# - Various other reading and writing validation is done by comparing
#   outputs going through various (classic and XML) encodings.
# - Check rendering, by making screenshots
#   and comparing (against the same castle-model-viewer
#   or against some previous stable castle-model-viewer version).
#
# Some of the tests are commented out by default in run_test_on_model.sh,
# as they require more time or have to be interpreted manually (to filter
# out some harmless warnings). Default tests are quick and fully automatic.
#
# This bash script uses settings similar to bash strict mode, for similar reasons,
# see http://redsymbol.net/articles/unofficial-bash-strict-mode/
# ----------------------------------------------------------------------------

# Change LANG to something hardcoded,
# because `sort' results are affected by the current locale (see man sort).
# And we want them to be predictable, the same on every system,
# to make comparing two outputs (like run_tests_on_dir_final.sh does) reliable.
export LANG=C
export LC_COLLATE=C

OUTPUT_SHORT="$1"
OUTPUT_VERBOSE="$2"
shift 2

rm -f "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"

set +e

# Explanation for some omitted files/dirs:
# - errors subdirs in demo_models contains files that *should* fail when reading.
# - *test_temporary* files are leftovers from interrupted previous tests.
# - ios_tests/CastleEngineTest/CastleEngineTest contains VRML with .wrl extension
#   but gzip compressed, out test script is not prepared for it now.

# The "find" output is run through sort and then xargs.
# Otherwise, find prints files in an unpredictable order (from readdir),
# which would make comparing two outputs from two different systems (like
# run_tests_on_dir_final.sh does) impossible.
# Fortunately, find + sort go very fast (compared to actual
# run_test_on_model.sh time).

find "$@" \
'(' -type d -iname 'errors' -prune ')' -or \
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
xargs -0 --max-args=1 test_scripts/run_test_on_model.sh "${OUTPUT_SHORT}" "${OUTPUT_VERBOSE}"

set -e
