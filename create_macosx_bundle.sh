#!/bin/bash
set -eu

../scripts/create_macosx_bundle.sh view3dscene view3dscene desktop/view3dscene.icns \
'  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>wrl</string>
      <string>wrz</string>
      <string>wrl.gz</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>model/vrml</string>
    <key>CFBundleTypeName</key>
    <string>VRML document</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Editor</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>3ds</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>image/x-3ds</string>
    <key>CFBundleTypeName</key>
    <string>3DS model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>x3dv</string>
      <string>x3dv.gz</string>
      <string>x3dvz</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>model/x3d+vrml</string>
    <key>CFBundleTypeName</key>
    <string>X3D model (classic VRML encoding)</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Editor</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>x3d</string>
      <string>x3d.gz</string>
      <string>x3dz</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>model/x3d+xml</string>
    <key>CFBundleTypeName</key>
    <string>X3D model (XML encoding)</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Editor</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>dae</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>model/vnd.collada+xml</string>
    <key>CFBundleTypeName</key>
    <string>COLLADA model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>iv</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>object/x-inventor</string>
    <key>CFBundleTypeName</key>
    <string>Inventor model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>md3</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>application/x-md3</string>
    <key>CFBundleTypeName</key>
    <string>MD3 (Quake 3 engine) model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>obj</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>application/x-wavefront-obj</string>
    <key>CFBundleTypeName</key>
    <string>Wavefront OBJ model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>geo</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>application/x-geo</string>
    <key>CFBundleTypeName</key>
    <string>Videoscape GEO model</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
  <dict>
    <key>CFBundleTypeExtensions</key>
    <array>
      <string>kanim</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>application/x-kanim</string>
    <key>CFBundleTypeName</key>
    <string>Castle Game Engine animation</string>
    <key>CFBundleTypeIconFile</key>
    <string>view3dscene</string>
    <key>CFBundleTypeOSTypes</key>
    <array>
      <string>****</string>
    </array>
    <key>CFBundleTypeRole</key>
    <string>Viewer</string>
  </dict>
'

cd view3dscene.app/Contents/MacOS/

# Copy fink lib $1, and adjust it's -id (how the library identifies itself,
# may be important if another lib depends on it -- although tests show it's not really
# important?).
cp_fink_lib ()
{
  cp /sw/lib/"$1" .
  install_name_tool -id @executable_path/"$1" "$1"
}

cp_fink_lib libpng14.14.dylib
cp_fink_lib libvorbisfile.3.dylib
cp_fink_lib libvorbis.0.dylib
cp_fink_lib libogg.0.dylib

install_name_tool -change /sw/lib/libvorbis.0.dylib @executable_path/libvorbis.0.dylib libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbis.0.dylib

if otool -L *.dylib | grep /sw/lib/; then
  echo 'Error: Some references to /sw/lib/ remain inside the bundle, application possibly will not run without fink installed. Check install_name_tool commands in create_macosx_bundle.sh script.'
  exit 1
fi
