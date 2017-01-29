{
  Copyright 2014-2017 Michalis Kamburelis.

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

{ Utility for captions. }
unit V3DSceneCaptions;

interface

const
  DefaultCaptionLimit = 50;

{ Return S with newlines replaced with spaces and trimmed to
  sensible number of characters. This is useful when you
  want to use some user-supplied string (e.g. in VRML/X3D
  SFString field) in your UI (e.g. as menu or window caption). }
function SForCaption(const S: string;
  const Limit: Cardinal = DefaultCaptionLimit): string;

implementation

uses CastleStringUtils;

function SForCaption(const S: string; const Limit: Cardinal): string;
begin
  Result := SCompressWhiteSpace(S);
  if Length(Result) > Limit then
    Result := Copy(Result, 1, Limit - 3) + '...';
end;

end.
