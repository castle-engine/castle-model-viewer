{
  Copyright 2003-2013 Michalis Kamburelis.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Status text font and colors. }
unit V3DSceneStatus;

interface

uses CastleGLBitmapFonts, Classes;

var
  { Initialized in Window.Open, freed in Window.Close. }
  StatusFont: TGLBitmapFont;

procedure DrawStatus(const S: TStringList);

function StatusMaxLineChars(const WindowWidth: Cardinal): Cardinal;

implementation

uses CastleVectors, GL, CastleUtils;

const
  { Margin inside status box size. }
  BoxMargin = 5;
  { Margin from status box to window border. }
  WindowMargin = 5;

procedure DrawStatus(const S: TStringList);
const
  InsideColor: TVector4Single = (0, 0, 0, 0.7);
  BorderColor: TVector4Single = (0, 1, 0, 1);
  TextColor  : TVector4Single = (1, 1, 0, 1);
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  StatusFont.PrintStringsBox(S, true, WindowMargin, WindowMargin, 0,
    InsideColor, BorderColor, TextColor, BoxMargin);
  glDisable(GL_BLEND);
end;

function StatusMaxLineChars(const WindowWidth: Cardinal): Cardinal;
begin
  Result := Max(Integer(10), Integer(WindowWidth - BoxMargin * 2 - WindowMargin * 2) div StatusFont.TextWidth('W'));
end;

end.
