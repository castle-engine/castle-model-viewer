compile:
	./compile.sh

# See desktop/INSTALL for some comments about installing desktop data.

# For non-user install, change this to
#   SHARE_PREFIX = /usr/local/share
# For Debian package, you probably want to set this to
#   SHARE_PREFIX = $(DESTDIR)/usr/share
SHARE_PREFIX = $(HOME)/.local/share

install:
	mkdir -p $(SHARE_PREFIX)/applications/
	cp desktop/view3dscene.desktop $(SHARE_PREFIX)/applications/
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/scalable/apps/
	cp desktop/icon/6_thicker.svg $(SHARE_PREFIX)/icons/hicolor/scalable/apps/view3dscene.svg
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/48x48/apps/
	inkscape $(SHARE_PREFIX)/icons/hicolor/scalable/apps/view3dscene.svg --export-png=$(SHARE_PREFIX)/icons/hicolor/48x48/apps/view3dscene.png
	mkdir -p $(SHARE_PREFIX)/mime/packages/
	cp desktop/view3dscene.xml $(SHARE_PREFIX)/mime/packages/
	update-mime-database $(SHARE_PREFIX)/mime
