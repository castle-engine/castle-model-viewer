{
  Copyright 2003-2010 Michalis Kamburelis.

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

{ Simple management of ColorModulator of VRML scene/animation, to show
  some sample modulators.

  Remember - usually you have to do glwin.PostRedisplay after
  SetColorModulatorType.

  Note that ctNegative looks best when OpenGL lighting is turned off
  (add this to docs somewhere ?).
}

unit ColorModulators;

interface

uses KambiUtils, VectorMath, VRMLNodes, VRMLOpenGLRenderer, VRMLGLAnimation;

type
  TColorModulatorType = (
    ctNone, ctNegative, ctGrayscale, ctGrayscaleNegative,
    ctStrippedToRed,  ctStrippedToGreen,  ctStrippedToBlue,
    ctConvertedToRed, ctConvertedToGreen, ctConvertedToBlue );

  TColorModulatorInfo = record
    Name: string;
  end;

const
  ColorModulatorInfos: array[TColorModulatorType]of TColorModulatorInfo =
  ( (Name:'Off'),
    (Name:'Negative'),
    (Name:'Grayscale'),
    (Name:'Grayscale negative'),
    (Name:'Strip to red channel'),
    (Name:'Strip to green channel'),
    (Name:'Strip to blue channel'),
    (Name:'Convert to red'),
    (Name:'Convert to green'),
    (Name:'Convert to blue')
  );

procedure InitColorModulator(SceneAnimation: TVRMLGLAnimation);

function ColorModulatorType: TColorModulatorType;
procedure SetColorModulatorType(value: TColorModulatorType;
  SceneAnimation: TVRMLGLAnimation);

implementation

uses KambiGLUtils;

type
  TColorModulatorPrivateInfo = record
    SingleFunc: TColorModulatorSingleFunc;
    ByteFunc: TColorModulatorByteFunc;
  end;

const
  ColorModulatorPrivateInfos:
    array[TColorModulatorType]of TColorModulatorPrivateInfo =
  ( (SingleFunc: nil; ByteFunc: nil),
    (SingleFunc: @ColorNegativeSingle;           ByteFunc: @ColorNegativeByte),
    (SingleFunc: @ColorGrayscaleSingle;          ByteFunc: @ColorGrayscaleByte),
    (SingleFunc: @ColorGrayscaleNegativeSingle;  ByteFunc: @ColorGrayscaleNegativeByte),
    (SingleFunc: @ColorRedStripSingle;           ByteFunc: @ColorRedStripByte),
    (SingleFunc: @ColorGreenStripSingle;         ByteFunc: @ColorGreenStripByte),
    (SingleFunc: @ColorBlueStripSingle;          ByteFunc: @ColorBlueStripByte),
    (SingleFunc: @ColorRedConvertSingle;         ByteFunc: @ColorRedConvertByte),
    (SingleFunc: @ColorGreenConvertSingle;       ByteFunc: @ColorGreenConvertByte),
    (SingleFunc: @ColorBlueConvertSingle;        ByteFunc: @ColorBlueConvertByte)
  );

var
  FColorModulatorType: TColorModulatorType = ctNone;

procedure SetColorModulatorType(Value: TColorModulatorType;
  SceneAnimation: TVRMLGLAnimation);
begin
 FColorModulatorType := Value;
 SceneAnimation.Attributes.ColorModulatorSingle := ColorModulatorPrivateInfos[Value].SingleFunc;
 SceneAnimation.Attributes.ColorModulatorByte   := ColorModulatorPrivateInfos[Value].ByteFunc;
end;

function ColorModulatorType: TColorModulatorType;
begin
 Result := FColorModulatorType;
end;

procedure InitColorModulator(SceneAnimation: TVRMLGLAnimation);
begin
 SetColorModulatorType(ColorModulatorType, SceneAnimation);
end;

end.
