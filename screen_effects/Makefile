ALL_TARGETS := ../screen_effects_flashlight.glsl.inc \
               ../screen_effects_edge_detect.glsl.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

../screen_effects_%.glsl.inc: %.glsl
	file_to_pascal_string $< $@
