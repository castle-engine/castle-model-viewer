.PHONY: all clean

ALL_UNITS=../v3dsceneimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../v3dsceneimages.pas: warning_icon.png examine.png walk.png fly.png \
  open.png screenshot.png examine_tooltip.png walk_fly_tooltip.png
	image_to_pas --output .. V3DSceneImages \
	  warning_icon.png examine.png walk.png fly.png open.png screenshot.png \
	  @alpha=strip examine_tooltip.png walk_fly_tooltip.png
