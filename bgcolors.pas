{
  Copyright 2002-2005 Michalis Kamburelis.

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
}

{ Simple management of glClearColor OpenGL property. }

unit BGColors;

{$I openglmac.inc}

interface

uses GL, GLU, GLExt, VectorMath;

var 
  BGColor: TVector3Single;

{ wywoluj zawsze po zmianie BGColor, wywolaj tez na poczatku w OnInit.
  ustawi odpowiednie glClearColor }
procedure BGColorChanged;

implementation

procedure BGColorChanged;
begin
 { alpha ponizszego koloru jest bez znaczenia dla view3dscene. 
   Wartosc 0 jest tak samo dobra jak kazda inna. }
 glClearColor(BGColor[0], BGColor[1], BGColor[2], 0);
end;

end.