# See INSTALL file for some comments about installing desktop data.

VIEW3DSCENE_ICON := 6_thicker
ENGINE_ICON := 8_tentacles_moon_polish

ALL_IMAGES := ../../www/htdocs/images/kambi_vrml_game_engine_icon.png \
              view3dscene.png view3dscene.svg view3dscene.icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

# We generate PNG versions from SVG and put them in our SVN repo,
# to not require user to have inkscape just to install our icons.
#
# Note that for WWW engine icon, we generate non-transparent PNG file.
# That's because IE on M$, well, you guessed it --- it can't reliably handle
# PNG transparency anyway. So non-transparent PNG is safer.

../../www/htdocs/images/kambi_vrml_game_engine_icon.png: icon/$(ENGINE_ICON).svg
	inkscape --export-width=256 --export-height=256 \
	  --export-background=#FFFFFF \
	  --export-background-opacity=1.0 \
	  $< --export-png=$@

# Copy $(VIEW3DSCENE_ICON) to nice names, in svg and (48x48) png.
view3dscene.svg: icon/$(VIEW3DSCENE_ICON).svg
	cp -f $< $@

view3dscene.png: icon/$(VIEW3DSCENE_ICON).svg
	inkscape --export-width=48 --export-height=48 $< --export-png=$@

view3dscene.icns: icon/$(VIEW3DSCENE_ICON).svg
	../../scripts/svg_to_icns.sh $< $@
