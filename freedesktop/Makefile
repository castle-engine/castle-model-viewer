# See INSTALL file for some comments about installing desktop data.

APP_NAME := view3dscene

VIEW3DSCENE_ICON := ../../www/logo/6_thicker.svg

ALL_IMAGES := $(APP_NAME).png $(APP_NAME).svg $(APP_NAME).icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

# Copy/convert $(VIEW3DSCENE_ICON) to nice names, in svg and (48x48) png.
$(APP_NAME).svg: $(VIEW3DSCENE_ICON)
	cp -f $< $@

$(APP_NAME).png: $(VIEW3DSCENE_ICON)
	inkscape --export-width=48 --export-height=48 $< --export-png=$@

$(APP_NAME).icns: $(VIEW3DSCENE_ICON)
	../../scripts/svg_to_icns.sh $< $@
