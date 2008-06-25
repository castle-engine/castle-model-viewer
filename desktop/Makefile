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
install: icon/$(VIEW3DSCENE_ICON)_48x48.png
# Install mime types:
	mkdir -p $(SHARE_PREFIX)/mime/packages/
	cp view3dscene.xml $(SHARE_PREFIX)/mime/packages/
	update-mime-database $(SHARE_PREFIX)/mime
# Install icons:
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/scalable/apps/
	cp icon/$(VIEW3DSCENE_ICON).svg $(SHARE_PREFIX)/icons/hicolor/scalable/apps/view3dscene.svg
# Install also 48x48 PNG version, this helps nautilus to display icon
# properly on the desktop.
	mkdir -p $(SHARE_PREFIX)/icons/hicolor/48x48/apps/
	cp icon/$(VIEW3DSCENE_ICON)_48x48.png $(SHARE_PREFIX)/icons/hicolor/48x48/apps/view3dscene.png
# Is this needed?
#	if which update-icon-caches >/dev/null 2>&1 ; then update-icon-caches $(SHARE_PREFIX)/icons/hicolor/; fi
# Install desktop file:
# (Do this at the end, when mime and icons are already installed;
# I don't know for sure what update-desktop-database actually does,
# it's better to be safe).
	mkdir -p $(SHARE_PREFIX)/applications/
	cp view3dscene.desktop $(SHARE_PREFIX)/applications/
	if which update-desktop-database >/dev/null 2>&1 ; then update-desktop-database -q $(SHARE_PREFIX)/applications/; fi

# GNOME thumbnailer stuff ------------------------------------------------------

# This should be called only by recursive make call from install_thumbnailer.
# (This looks better than using a GNU make function).
.PHONY: install_single_thumbnailer
install_single_thumbnailer:
	@echo 'Installing for MIME '$(THUMBNAILER_MIME)
	gconftool --type bool   --set /desktop/gnome/thumbnailers/$(subst +,@,$(subst /,@,$(THUMBNAILER_MIME)))/enable true
	gconftool --type string --set /desktop/gnome/thumbnailers/$(subst +,@,$(subst /,@,$(THUMBNAILER_MIME)))/command "view3dscene-thumbnailer %i %o %s"

.PHONY: install_thumbnailer
install_thumbnailer:
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=model/vrml
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=image/x-3ds
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=model/x3d+vrml
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=model/x3d+xml
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-collada
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-inventor
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-md3
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-wavefront-obj
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-geo
	@$(MAKE) --no-print-directory install_single_thumbnailer THUMBNAILER_MIME=application/x-kanim
	@echo '--------'
	@echo 'All done. Remember to call also "make install" (before or after "make install_thumbnailer"), otherwise MIME types of 3D models will not be known and thumbnailer will never be run.'
	@echo '--------'
