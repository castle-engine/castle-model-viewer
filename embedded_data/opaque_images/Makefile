# Process all *.png images in this directory to Pascal,
# stripping alpha channel.

.PHONY: all clean

ALL_UNITS=../../code/v3dsceneopaqueimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

$(ALL_UNITS): *.png
	image-to-pascal --output ../../code V3DSceneOpaqueImages @alpha=strip *.png
