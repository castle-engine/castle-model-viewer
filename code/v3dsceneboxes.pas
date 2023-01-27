{
  Copyright 2006-2023 Michalis Kamburelis.

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

uses CastleBoxes, CastleVectors, CastleColors;

{ Draw corner markers (3 lines) at the 8 corners of the box.
  Proportion is the fraction of the box length, the marker extends too. }
procedure glDrawCornerMarkers(const Box: TBox3D;
  const Color: TCastleColor; const ModelViewProjection: TMatrix4);

implementation

uses SysUtils,
  CastleInternalGLUtils, CastleGLUtils;

procedure glDrawCornerMarkers(const Box: TBox3D;
  const Color: TCastleColor; const ModelViewProjection: TMatrix4);
const
  Proportion = 0.1;
var
  Vertexes: TVector4List;

  procedure glDrawCorners(const minx, miny, minz, maxx, maxy, maxz: Single);

    procedure glDrawCornerLines(const x, y, z, dx, dy, dz: Single);
    begin
      Vertexes.AddRange([
        Vector4(x     , y     , z     , 1),
        Vector4(x + dx, y     , z     , 1),
        Vector4(x     , y     , z     , 1),
        Vector4(x     , y + dy, z     , 1),
        Vector4(x     , y     , z     , 1),
        Vector4(x     , y     , z + dz, 1)
      ]);
    end;

  var
    Xlength, Ylength, Zlength: Single;
  begin
    Xlength := (maxx - minx) * Proportion;
    Ylength := (maxy - miny) * Proportion;
    Zlength := (maxz - minz) * Proportion;

    glDrawCornerLines(minx, miny, minz,  Xlength,  Ylength,  Zlength);
    glDrawCornerLines(minx, miny, maxz,  Xlength,  Ylength, -Zlength);
    glDrawCornerLines(minx, maxy, minz,  Xlength, -Ylength,  Zlength);
    glDrawCornerLines(minx, maxy, maxz,  Xlength, -Ylength, -Zlength);
    glDrawCornerLines(maxx, miny, minz, -Xlength,  Ylength,  Zlength);
    glDrawCornerLines(maxx, miny, maxz, -Xlength,  Ylength, -Zlength);
    glDrawCornerLines(maxx, maxy, minz, -Xlength, -Ylength,  Zlength);
    glDrawCornerLines(maxx, maxy, maxz, -Xlength, -Ylength, -Zlength);
  end;

var
  Mesh: TCastleRenderUnlitMesh;
begin
  Vertexes := TVector4List.Create;
  try
    glDrawCorners(
      Box.Data[0].X, Box.Data[0].Y, Box.Data[0].Z,
      Box.Data[1].X, Box.Data[1].Y, Box.Data[1].Z
    );

    Mesh := TCastleRenderUnlitMesh.Create(true);
    try
      Mesh.ModelViewProjection := ModelViewProjection;
      Mesh.Color := Color;
      Mesh.SetVertexes(Vertexes, false);
      Mesh.Render(pmLines);
    finally FreeAndNil(Mesh) end;
  finally FreeAndNil(Vertexes) end;
end;

end.
