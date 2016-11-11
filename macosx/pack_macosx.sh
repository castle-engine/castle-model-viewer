#!/bin/bash
set -eu

# Create Mac OS X bundle, and then dmg (disk image) file to distribute view3dscene.
# Compile the view3dscene (and tovrmlx3d) binaries before calling this.

. ../../scripts/create_macosx_bundle.sh

create_bundle view3dscene ../view3dscene ../desktop/view3dscene.icns \
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
    <string>application/x-inventor</string>
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
    <string>application/x-castle-anim-frames</string>
    <key>CFBundleTypeName</key>
    <string>Castle Animation Frames</string>
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
      <string>castle-anim-frames</string>
    </array>
    <key>CFBundleTypeMIMETypes</key>
    <string>application/x-castle-anim-frames</string>
    <key>CFBundleTypeName</key>
    <string>Castle Animation Frames</string>
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

# add tovrmlx3d binary
cp ../tovrmlx3d view3dscene.app/Contents/MacOS/tovrmlx3d

# add libraries from fink
cd view3dscene.app/Contents/MacOS/

cp_fink_lib libpng14.14.dylib
cp_fink_lib libvorbisfile.3.dylib
cp_fink_lib libvorbis.0.dylib
cp_fink_lib libogg.0.dylib

install_name_tool -change /sw/lib/libvorbis.0.dylib @executable_path/libvorbis.0.dylib libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbisfile.3.dylib
install_name_tool -change /sw/lib/libogg.0.dylib    @executable_path/libogg.0.dylib    libvorbis.0.dylib

check_libs_not_depending_on_fink view3dscene tovrmlx3d

cd ../../../

make -f ../../scripts/macosx_dmg.makefile NAME=view3dscene
