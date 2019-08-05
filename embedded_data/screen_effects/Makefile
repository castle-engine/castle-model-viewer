# This requires file_to_pascal_string from PasDoc
# ( https://github.com/pasdoc/pasdoc/wiki ) project. Get it and compile from
# https://raw.githubusercontent.com/pasdoc/pasdoc/master/source/tools/file_to_pascal_string.dpr .

ALL_TARGETS := screen_effects_flashlight.glsl.inc \
               screen_effects_edge_detect.glsl.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

screen_effects_%.glsl.inc: %.glsl
	file_to_pascal_string $< $@
