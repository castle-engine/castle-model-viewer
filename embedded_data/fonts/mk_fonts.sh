#!/bin/bash
set -eu

TEXTUREFONT2PASCAL='texture-font-to-pascal'

# Add option to have common Unicode characters available (not just ASCII),
# this way we cover many Latin languages out-of-the-box,
# see https://gist.github.com/ivandrofly/0fe20773bd712b303f78 .
# Note: CGE applications can always define fonts that use more glyphs,
# e.g. to support Japanese or Chinese see https://castle-engine.io/manual_text.php .
TEXTUREFONT2PASCAL="${TEXTUREFONT2PASCAL} --sample-text-file=common_unicode_chars.txt"

# Some texture-font-to-pascal options are useful for debugging what's going on.
#TEXTUREFONT2PASCAL="${TEXTUREFONT2PASCAL} --debug-log --debug-font-image"

# Used by Text node in X3DNodes. This font is drawn in 3D,
# always automatically stretched to desired size, so you can freely
# adjust TEXT_NODE_FONT_SIZE to balance the font texture size vs nice font look.
#
# The default value is the maximum value where we still fit all in (at most)
# 2048x2048 texture.
# We deliberately use unit and function names that make the unit/function indendepent of the size,
# so that changing sizes used by default by CGE can be easily done by changing +
# running this shell script.
TEXT_NODE_FONT_SIZE=25
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSans-BoldOblique.ttf      --unit-name CastleTextureFont_Default3D_SansBI  --function-name Font_Default3D_SansBI
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSans-Bold.ttf             --unit-name CastleTextureFont_Default3D_SansB   --function-name Font_Default3D_SansB
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSans-Oblique.ttf          --unit-name CastleTextureFont_Default3D_SansI   --function-name Font_Default3D_SansI
# This is embedded in CGE:
# $TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSans.ttf                  --unit-name CastleTextureFont_Default3D_Sans    --function-name Font_Default3D_Sans
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSansMono-BoldOblique.ttf  --unit-name CastleTextureFont_Default3D_MonoBI  --function-name Font_Default3D_MonoBI
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSansMono-Bold.ttf         --unit-name CastleTextureFont_Default3D_MonoB   --function-name Font_Default3D_MonoB
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSansMono-Oblique.ttf      --unit-name CastleTextureFont_Default3D_MonoI   --function-name Font_Default3D_MonoI
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSansMono.ttf              --unit-name CastleTextureFont_Default3D_Mono    --function-name Font_Default3D_Mono
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSerif-BoldItalic.ttf      --unit-name CastleTextureFont_Default3D_SerifBI --function-name Font_Default3D_SerifBI
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSerif-Bold.ttf            --unit-name CastleTextureFont_Default3D_SerifB  --function-name Font_Default3D_SerifB
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSerif-Italic.ttf          --unit-name CastleTextureFont_Default3D_SerifI  --function-name Font_Default3D_SerifI
$TEXTUREFONT2PASCAL --size "${TEXT_NODE_FONT_SIZE}" ttf/DejaVuSerif.ttf                 --unit-name CastleTextureFont_Default3D_Serif   --function-name Font_Default3D_Serif
