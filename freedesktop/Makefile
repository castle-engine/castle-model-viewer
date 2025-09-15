# See INSTALL file for some comments about installing desktop data.

APP_NAME := castle-model-viewer

APP_ICON := ../../www/logo/6_thicker.svg

ALL_IMAGES := $(APP_NAME).png $(APP_NAME).svg $(APP_NAME).icns

.PHONY: all_images
all_images: $(ALL_IMAGES)

# Copy/convert $(APP_ICON) to nice names, in svg and (48x48) png.
$(APP_NAME).svg: $(APP_ICON)
	cp -f $< $@

$(APP_NAME).png: $(APP_ICON)
	inkscape --export-width=48 --export-height=48 $< --export-filename=$@

$(APP_NAME).icns: $(APP_ICON)
	../../scripts/svg_to_icns.sh $< $@
