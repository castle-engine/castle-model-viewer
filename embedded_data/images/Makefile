# Process all *.png images in this directory to Pascal,
# preserving alpha channel.

.PHONY: all clean

ALL_UNITS=../../code/v3dsceneimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

$(ALL_UNITS): *.png
	image-to-pascal --output ../../code V3DSceneImages *.png
