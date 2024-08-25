{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Status text font and colors. }
unit V3DSceneStatus;

interface

uses CastleFonts, Classes, CastleUIControls, CastleTimeUtils,
  CastleControls;

type
  TStatusText = class(TCastleLabel)
  private
    FlashTime, Time: TFloatTime;
    FlashText: string;
  protected
    procedure CalculateText; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    { Show a message that will fadeout with time. }
    procedure Flash(const AText: string);
  end;

var
  StatusText: TStatusText;

implementation

uses SysUtils, Math,
  CastleTextureFont_Default3d_MonoB,
  CastleVectors, CastleUtils, CastleColors;

{ TStatusText ---------------------------------------------------------------- }

constructor TStatusText.Create(AOwner: TComponent);
begin
  inherited;
  Html := true;
  Anchor(hpLeft, 5);
  Anchor(vpBottom, 5);
  Color := Yellow;
  PaddingHorizontal := 5;
  PaddingVertical := 5;
  FontSize := 15;
  CustomFont := TCastleFont.Create(Self);
  TCastleFont(CustomFont).Load(Font_Default3d_MonoB);
  Frame := true;
end;

procedure TStatusText.Render;
begin
  Text.Clear;
  CalculateText;
  inherited;
end;

const
  FlashDelay = 1.0;

procedure TStatusText.CalculateText;
begin
  if (FlashText <> '') and (FlashTime + FlashDelay > Time) then
  begin
    Text.Append('<font color="#' + ColorToHex(Vector4(1, 1, 0,
      1 - (Time - FlashTime) / FlashDelay)) + '">' + FlashText + '</font>');
    Text.Append('');
  end;
end;

procedure TStatusText.Flash(const AText: string);
begin
  FlashText := AText;
  FlashTime := Time;
end;

procedure TStatusText.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  if (FlashText <> '') and (FlashTime + FlashDelay > Time) then
    VisibleChange([chRender]);
  Time += SecondsPassed;
end;

end.
