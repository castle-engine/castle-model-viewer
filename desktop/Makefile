# See INSTALL file for some comments about installing desktop data.

# For non-user install, change this to
#   SHARE_PREFIX = /usr/local/share
# For Debian package, you probably want to set this to
#   SHARE_PREFIX = $(DESTDIR)/usr/share
SHARE_PREFIX := $(HOME)/.local/share

VIEW3DSCENE_ICON := 6_thicker
ENGINE_ICON := 8_tentacles_moon_polish

ALL_IMAGES := ../../www/htdocs/images/kambi_vrml_game_engine_icon.png \
              icon/$(VIEW3DSCENE_ICON)_48x48.png

.PHONY: all_images
all_images: $(ALL_IMAGES)

# We generate PNG versions from SVG and put them in our SVN repo,
# to not require user to have inkscape just to install our icons.


../../www/htdocs/images/kambi_vrml_game_engine_icon.png: icon/$(ENGINE_ICON).svg
	inkscape --export-width=256 --export-height=256 $< --export-png=$@

icon/$(VIEW3DSCENE_ICON)_48x48.png: icon/$(VIEW3DSCENE_ICON).svg
	inkscape --export-width=48 --export-height=48 $< --export-png=$@

.PHONY: install
install: all_images
	mkdir -p $(SHARE_PREFIX)/applications/
	cp view3dscene.desktop $(SHARE_PREFIX)/applications/
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/scalable/apps/
	cp icon/$(VIEW3DSCENE_ICON).svg $(SHARE_PREFIX)/icons/hicolor/scalable/apps/view3dscene.svg
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/48x48/apps/
# Install also 48x48 PNG version, this helps nautilus to display icon
# properly on the desktop.
	cp icon/$(VIEW3DSCENE_ICON)_48x48.png $(SHARE_PREFIX)/icons/hicolor/48x48/apps/view3dscene.png
	mkdir -p $(SHARE_PREFIX)/mime/packages/
	cp view3dscene.xml $(SHARE_PREFIX)/mime/packages/
	update-mime-database $(SHARE_PREFIX)/mime
