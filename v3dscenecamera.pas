{
  Copyright 2004-2005,2007 Michalis Kamburelis.

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

{ }
unit V3DSceneCamera;

interface

uses VectorMath, VRMLNodes, GLWindow, KambiUtils;

{$define read_interface}

{ Return S with newlines replaced with spaces and trimmed to
  sensible number of characters. This is useful when you
  want to use some user-supplied string (e.g. in VRML
  SFString field) in your UI (e.g. as menu or window caption). }
function SForCaption(const S: string): string;

type
  TViewpointInfo = record
    Node: TVRMLViewpointNode;
    Transform: TMatrix4Single;
  end;
  PViewpointInfo = ^TViewpointInfo;

  TDynArrayItem_1 = TViewpointInfo;
  PDynArrayItem_1 = PViewpointInfo;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TViewpointsList = class(TDynArray_1)
  public
    MenuJumpToViewpoint: TMenu;

    procedure AddNodeTransform(Node: TVRMLViewpointNode;
      const Transform: TMatrix4Single);

    procedure MakeMenuJumpToViewpoint;

    { Calculate viewpoint given by Index (and return it).
      If Index is outside 0..High, we calculate default VRML camera
      properties (and return nil). }
    function GetViewpoint(Index: Integer;
      out CameraKind: TVRMLCameraKind;
      out CameraPos, CameraDir, CameraUp, GravityUp: TVector3Single):
        TVRMLViewpointNode;
  end;

var
  ViewpointsList: TViewpointsList;

{$undef read_interface}

implementation

uses SysUtils, RaysWindow, KambiStringUtils, VRMLCameraUtils;

{$define read_implementation}
{$I dynarray_1.inc}

function SForCaption(const S: string): string;
begin
  Result := SReplaceChars(S, [#10, #13], ' ');
  if Length(Result) > 50 then
    Result := Copy(Result, 1, 50) + '...';
end;

procedure TViewpointsList.AddNodeTransform(Node: TVRMLViewpointNode;
  const Transform: TMatrix4Single);
var
  Item: TViewpointInfo;
begin
  Item.Node := Node;
  Item.Transform := Transform;
  AppendItem(Item);
end;

procedure TViewpointsList.MakeMenuJumpToViewpoint;
var
  Node: TVRMLViewpointNode;
  I: Integer;
  S: string;
  Viewpoint: TNodeViewpoint;
begin
  MenuJumpToViewpoint.EntriesDeleteAll;
  { We don't add more than 100 menu item entries for viewpoints. }
  for I := 0 to Min(Count, 100) - 1 do
  begin
    if I < 10 then
      S := '_' + IntToStr(I) else
      S := IntToStr(I);
    Node := Items[I].Node;
    S += ': ' + Node.NodeTypeName;
    if Node is TNodeViewpoint then
    begin
      Viewpoint := TNodeViewpoint(Node);
      if Viewpoint.FdDescription.Value <> '' then
        S += ' "' + SQuoteMenuEntryCaption(
          SForCaption(Viewpoint.FdDescription.Value)) + '"';
    end else
    begin
      if Node.NodeName <> '' then
        S += ' "' + SQuoteMenuEntryCaption(Node.NodeName) + '"';
    end;
    MenuJumpToViewpoint.Append(TMenuItem.Create(S, 300 + I));
  end;
  MenuJumpToViewpoint.Append(TMenuSeparator.Create);
  MenuJumpToViewpoint.Append(TMenuItem.Create('Default VRML 1.0 viewpoint', 51));
  MenuJumpToViewpoint.Append(TMenuItem.Create('Default VRML 2.0 viewpoint', 52));
  MenuJumpToViewpoint.Append(TMenuItem.Create('Calculated viewpoint to see the whole scene', 53));
end;

function TViewpointsList.GetViewpoint(
  Index: Integer; out CameraKind: TVRMLCameraKind;
  out CameraPos, CameraDir, CameraUp, GravityUp: TVector3Single):
  TVRMLViewpointNode;
begin
  if Between(Index, 0, High) then
  begin
    Result := ViewpointsList.Items[Index].Node;
    Result.GetCameraVectors(
      ViewpointsList.Items[Index].Transform,
      CameraPos, CameraDir, CameraUp, GravityUp);
    CameraKind := Result.CameraKind;
  end else
  begin
    Result := nil;
    CameraPos := StdVRMLCamPos[1];
    CameraDir := StdVRMLCamDir;
    CameraUp := StdVRMLCamUp;
    CameraKind := ckPerspective;
    GravityUp := StdVRMLGravityUp;
  end;
end;

initialization
  ViewpointsList := TViewpointsList.Create;
finalization
  FreeAndNil(ViewpointsList);
end.