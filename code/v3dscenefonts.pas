{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "view3dscene".

  "view3dscene" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Embed all font variants used by VRML / X3D in view3dscene.
  See embedded_data/fonts/README.md for explanation. }
unit V3DSceneFonts;

interface

procedure FontsInitialize;

implementation

uses X3DNodes,

  { Fonts for Text, FontStyle, AsciiText nodes }
  CastleTextureFont_Default3d_Sans,
  CastleTextureFont_Default3d_SansB,
  CastleTextureFont_Default3d_SansI,
  CastleTextureFont_Default3d_SansBI,

  CastleTextureFont_Default3d_Mono,
  CastleTextureFont_Default3d_MonoB,
  CastleTextureFont_Default3d_MonoI,
  CastleTextureFont_Default3d_MonoBI,

  CastleTextureFont_Default3d_Serif,
  CastleTextureFont_Default3d_SerifB,
  CastleTextureFont_Default3d_SerifI,
  CastleTextureFont_Default3d_SerifBI;

procedure FontsInitialize;
begin
  DefaultFontData[ffTypeWriter, true , true ] := Font_Default3d_MonoBI;
  DefaultFontData[ffTypeWriter, true , false] := Font_Default3d_MonoB;
  DefaultFontData[ffTypeWriter, false, true ] := Font_Default3d_MonoI;
  DefaultFontData[ffTypeWriter, false, false] := Font_Default3d_Mono;

  DefaultFontData[ffSerif, true , true ] := Font_Default3d_SerifBI;
  DefaultFontData[ffSerif, true , false] := Font_Default3d_SerifB;
  DefaultFontData[ffSerif, false, true ] := Font_Default3d_SerifI;
  DefaultFontData[ffSerif, false, false] := Font_Default3d_Serif;

  DefaultFontData[ffSans, true , true ] := Font_Default3d_SansBI;
  DefaultFontData[ffSans, true , false] := Font_Default3d_SansB;
  DefaultFontData[ffSans, false, true ] := Font_Default3d_SansI;
  DefaultFontData[ffSans, false, false] := Font_Default3d_Sans;
end;

end.