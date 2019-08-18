# This requires file_to_pascal_string from PasDoc
# ( https://github.com/pasdoc/pasdoc/wiki ) project. Get it and compile from
# https://raw.githubusercontent.com/pasdoc/pasdoc/master/source/tools/file_to_pascal_string.dpr .

ALL_TARGETS := welcome_scene.inc clear_scene.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.inc: %.wrl
	file_to_pascal_string $< $@
