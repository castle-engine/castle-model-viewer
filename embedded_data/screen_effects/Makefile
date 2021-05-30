# This requires file_to_pascal_string from PasDoc
# ( https://github.com/pasdoc/pasdoc/wiki ) project. Get it and compile from
# https://raw.githubusercontent.com/pasdoc/pasdoc/master/source/tools/file_to_pascal_string.dpr .

ALL_ORIGINAL := $(wildcard *.x3dv)
ALL_TARGETS := $(subst .x3dv,.x3dv.inc,$(ALL_ORIGINAL))

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.x3dv.inc: %.x3dv
	file_to_pascal_string $< $@
