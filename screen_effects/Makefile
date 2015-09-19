# This uses file_to_pascal_string, a trivial program
# from the pasdoc [http://pasdoc.sf.net/] project, see
# [https://pasdoc.svn.sourceforge.net/svnroot/pasdoc/trunk/source/tools/file_to_pascal_string.dpr]

ALL_TARGETS := ../code/screen_effects_flashlight.glsl.inc \
               ../code/screen_effects_edge_detect.glsl.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

../code/screen_effects_%.glsl.inc: %.glsl
	file_to_pascal_string $< $@
