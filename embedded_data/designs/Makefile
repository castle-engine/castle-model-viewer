# This requires file_to_pascal_string from PasDoc
# ( https://github.com/pasdoc/pasdoc/wiki ) project. Get it and compile from
# https://raw.githubusercontent.com/pasdoc/pasdoc/master/source/tools/file_to_pascal_string.dpr .

ALL_TARGETS := help_message.castle-user-interface.inc main.castle-user-interface.inc

default: $(ALL_TARGETS)

clean:
	rm -f $(ALL_TARGETS)

%.castle-user-interface.inc: %.castle-user-interface
	file_to_pascal_string $< $@
