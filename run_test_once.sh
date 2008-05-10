#!/bin/bash
set -eu

# This is usually run by run_tests.sh script.


FILE="$1"

# It's important that temp_file is inside the same directory,
# otherwise re-reading it would not work because relative URLs
# are broken.
TEMP_FILE=`dirname "$FILE"`/view3dscene_test_temporary_file.wrl

echo '---- Reading' "$FILE"
echo '(temp is ' "$TEMP_FILE" ')'
view3dscene "$FILE" --write-to-vrml > "$TEMP_FILE"
echo '---- Reading again' "$FILE"
view3dscene "$TEMP_FILE" --write-to-vrml > /dev/null

rm -f "$TEMP_FILE"
