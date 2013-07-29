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

uses CastleGLBitmapFonts, Classes, CastleUIControls;

type
  TStatusText = class(TUIControl)
  private
    Font: TGLBitmapFont;
    FMaxLineChars: Cardinal;
  protected
    procedure CalculateText(const Strs: TStringList); virtual;
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;
    property MaxLineChars: Cardinal read FMaxLineChars;
  end;

var
  StatusText: TStatusText;

{ Draw status text directly. Call this only if StatusText is created
  (since we reuse it's Font). }
procedure DrawStatus(const Strs: TStringList);

implementation

uses SysUtils, CastleBitmapFont_BVSansMono_Bold_m15,
  CastleVectors, GL, CastleUtils;

const
  { Margin inside status box size. }
  BoxMargin = 5;
  { Margin from status box to window border. }
  WindowMargin = 5;

{ TStatusText ---------------------------------------------------------------- }

procedure DrawStatus(const Strs: TStringList);
const
  InsideColor: TVector4Single = (0, 0, 0, 0.7);
  BorderColor: TVector4Single = (0, 1, 0, 1);
  TextColor  : TVector4Single = (1, 1, 0, 1);
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  StatusText.Font.PrintStringsBox(Strs, true, WindowMargin, WindowMargin, 0,
    InsideColor, BorderColor, TextColor, BoxMargin);
  glDisable(GL_BLEND);
end;

procedure TStatusText.Draw;
var
  Strs: TStringList;
begin
  if not GetExists then Exit;
  Strs := TStringList.Create;
  try
    CalculateText(Strs);
    DrawStatus(Strs);
  finally FreeAndNil(Strs) end;
end;

function TStatusText.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TStatusText.CalculateText(const Strs: TStringList);
begin
end;

procedure TStatusText.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  FMaxLineChars := Max(Integer(10), Integer(ContainerWidth - BoxMargin * 2 -
    WindowMargin * 2) div Font.TextWidth('W'));
end;

procedure TStatusText.GLContextOpen;
begin
  inherited;
  if Font = nil then
    Font := TGLBitmapFont.Create(BitmapFont_BVSansMono_Bold_m15);
end;

procedure TStatusText.GLContextClose;
begin
  FreeAndNil(Font);
  inherited;
end;

end.
