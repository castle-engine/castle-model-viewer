# See INSTALL file for some comments about installing desktop data.

VIEW3DSCENE_ICON := ../../www/logo/6_thicker.svg

ALL_IMAGES := view3dscene.png view3dscene.svg view3dscene.icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

# Copy/convert $(VIEW3DSCENE_ICON) to nice names, in svg and (48x48) png.
view3dscene.svg: $(VIEW3DSCENE_ICON)
	cp -f $< $@

view3dscene.png: $(VIEW3DSCENE_ICON)
	inkscape --export-width=48 --export-height=48 $< --export-png=$@

view3dscene.icns: $(VIEW3DSCENE_ICON)
	../../scripts/svg_to_icns.sh $< $@
