# See README.md file for some comments about installing desktop data.
# $(APP_NAME).svg is the source file.

APP_NAME := castle-model-viewer

ALL_IMAGES := $(APP_NAME)_48.png \
	$(APP_NAME)_512.png \
	$(APP_NAME)_1024.png

.PHONY: all_images
all_images: $(ALL_IMAGES)

.PHONY: clean
clean:
	rm -f $(ALL_IMAGES)

$(APP_NAME)_48.png: $(APP_NAME).svg
	inkscape --export-width=48 --export-height=48 $< --export-filename=$@
$(APP_NAME)_512.png: $(APP_NAME).svg
	inkscape --export-width=512 --export-height=512 $< --export-filename=$@
$(APP_NAME)_1024.png: $(APP_NAME).svg
	inkscape --export-width=1024 --export-height=1024 $< --export-filename=$@

$(APP_NAME).icns: $(APP_NAME).svg
	TODO/svg_to_icns.sh $< $@
