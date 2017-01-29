{
  Copyright 2006-2017 Michalis Kamburelis.

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

{ glDrawCornerMarkers utility. }
unit V3DSceneBoxes;

{$I v3dsceneconf.inc}

interface

uses CastleBoxes;

{$ifndef OpenGLES}

{ Draw corner markers (3 lines) at the 8 corners of the box.
  Proportion is the fraction of the box length, the marker extends too. }
procedure glDrawCornerMarkers(const Box: TBox3D; const Proportion: Single = 0.1);

{$endif}

implementation

uses CastleGL;

{$ifndef OpenGLES}

procedure glDrawCornerMarkers(const Box: TBox3D; const Proportion: Single);

  procedure glDrawCorners(const minx, miny, minz, maxx, maxy, maxz: Single);

    procedure glDrawCornerLines(const x, y, z, dx, dy, dz: Single);
    begin
      glVertex3f(x, y, z);
      glVertex3f(x+dx, y, z);
      glVertex3f(x, y, z);
      glVertex3f(x, y+dy, z);
      glVertex3f(x, y, z);
      glVertex3f(x, y, z+dz);
    end;

  var
    Xlength, Ylength, Zlength: Single;
  begin
    Xlength := (maxx - minx) * Proportion;
    Ylength := (maxy - miny) * Proportion;
    Zlength := (maxz - minz) * Proportion;
    glBegin(GL_LINES);
      glDrawCornerLines(minx,miny,minz,Xlength,Ylength,Zlength);
      glDrawCornerLines(minx,miny,maxz,Xlength,Ylength,-Zlength);
      glDrawCornerLines(minx,maxy,minz,Xlength,-Ylength,Zlength);
      glDrawCornerLines(minx,maxy,maxz,Xlength,-Ylength,-Zlength);
      glDrawCornerLines(maxx,miny,minz,-Xlength,Ylength,Zlength);
      glDrawCornerLines(maxx,miny,maxz,-Xlength,Ylength,-Zlength);
      glDrawCornerLines(maxx,maxy,minz,-Xlength,-Ylength,Zlength);
      glDrawCornerLines(maxx,maxy,maxz,-Xlength,-Ylength,-Zlength);
    glEnd;
  end;

begin
  glDrawCorners(Box.Data[0,0], Box.Data[0,1], Box.Data[0,2],
                Box.Data[1,0], Box.Data[1,1], Box.Data[1,2]);
end;

{$endif}

end.
