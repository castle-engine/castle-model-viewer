.PHONY: all clean

ALL_UNITS=../code/v3dsceneimages.pas

all: $(ALL_UNITS)

clean:
	rm -f $(ALL_UNITS)

../code/v3dsceneimages.pas: warning_icon.png examine.png walk.png fly.png \
  open.png screenshot.png examine_tooltip.png walk_fly_tooltip.png \
  TooltipArrow.png light_gizmo.png
	image2pascal --output ../code V3DSceneImages \
	  warning_icon.png examine.png walk.png fly.png open.png \
	  screenshot.png TooltipArrow.png light_gizmo.png \
	  @alpha=strip examine_tooltip.png walk_fly_tooltip.png
