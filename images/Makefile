.PHONY: all clean

ALL_UNITS=../imagewarning_icon.pas \
  ../imageexamine.pas ../imagewalk.pas ../imagefly.pas \
  ../imageopen.pas ../imageexamine_tooltip.pas \
  ../imagewalk_fly_tooltip.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../image%.pas: %.png
	image_to_pas $< > $@
