# Fonts to support all 16 font variants possibly used by VRML / X3D models

Authors of VRML / X3D can use `FontStyle` node to request one of possibly 12 fonts:

- 3 font families (sans, serif, monospace)
- italic or not
- bold or not

See https://github.com/castle-engine/demo-models/blob/master/text/text.x3dv for an example X3D file using all of them,

From Pascal you can also request these fonts, using TTextNode and TFontStyleNode.

This yields 12 total fonts.

## Why are these 12 fonts not embedded in Castle Game Engine?

_Castle Game Engine_ contains only a default sans-serif font (not italic, not bold) now. The rest of fonts, 11 of them, are only present in the view3dscene now.

We do not embed these 11 fonts in _Castle Game Engine_, since most CGE users don't need them:

- Our recommended components to display text in CGE are `TCastleLabel` and `TCastleText` and they expose a `CustomFont` property, to which you can assign any font, in particular `TCastleFontFamily` that represents 4 options (italic or not, bold or not).

- Effectively we advise to add to your project all fonts (.ttf, .otf) inside `data` subdirectory, and reference them using `TCastleFontFamily`  or `TCastleFont`.

- See CGE manual: https://castle-engine.io/manual_text.php for more information how to display text and manage fonts.

This means that CGE users don't need to embed all 12 fonts in their executables.

And having these 12 fonts was problematic in CGE, esp. since we made our default font "rich" with 2000+ Unicode characters to support most languages out-of-the-box. This made each default font file (and texture) much larger. This means compilation is significantly longer, and increases executable size.

- Increased exe size:

    view3dscene build ("make"):
    Without this defined: 24 MB.
    With this defined: 66 MB

- Increased build time:

    view3dscene + tovrmlx3d build time:
    Without this defined: 1m8s
    With this defined: 2m18s

The "significantly longer compilation" is a more severe problem than it sounds:

- In case of FPC, if the `castle_base.lpk` would contain all 12 fonts, it would make the bug https://gitlab.com/freepascal.org/fpc/source/-/issues/40164 more critical. Unfortunately, CGE developers using Lazarus have to recompile `castle_base.lpk` from scratch quite often due to that FPC bug, and having 12 fonts would the waiting time worse.

- In case of Delphi, if the Delphi package would contain 12 fonts, the compilation for _Linux 64_ would crash from time to time with "out of memory" exception.

- Increased build time becomes multiplied when CI (Jenkins, GitHub Actions) tests CGE doing "make examples". For safety these builds compile each example with cache disabled, so all 12 fonts would be recompiled for each example from scratch. This would make build time over 5 or 8 hours.

In the end, the extra font variants are really not useful for most all CGE applications, unless you make X3D browser.
