.PHONY: all clean

ALL_UNITS=../imagewarning_icon.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../imagewarning_icon.pas: warning_icon.png
	image_to_pas warning_icon.png > ../imagewarning_icon.pas
