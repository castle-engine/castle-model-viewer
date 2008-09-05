{
  Copyright 2004-2008 Michalis Kamburelis.

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

uses VectorMath, VRMLNodes, GLWindow, KambiUtils, VRMLScene;

{$define read_interface}

{ Return S with newlines replaced with spaces and trimmed to
  sensible number of characters. This is useful when you
  want to use some user-supplied string (e.g. in VRML
  SFString field) in your UI (e.g. as menu or window caption). }
function SForCaption(const S: string): string;

type
  { TODO: this Transform is not used now, remove and simplify }
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
  private
    procedure AddNodeTransform(Node: TVRMLViewpointNode;
      const Transform: TMatrix4Single);
  public
    MenuJumpToViewpoint: TMenu;

    { Recalculate our list of viewpoints, also recalculating
      MenuJumpToViewpoint contents if MenuJumpToViewpoint is initialized.

      Special value Scene = @nil means no scene is loaded, so no
      viewpoint nodes exist. }
    procedure Recalculate(Scene: TVRMLScene);

    { This does part of Recalculate job: only recalculate
      MenuJumpToViewpoint contents if MenuJumpToViewpoint is initialized.
      Call this when MenuJumpToViewpoint reference changed, and you know
      that actual scene viewpoints didn't change. }
    procedure MakeMenuJumpToViewpoint;
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
  Result := SCompressWhiteSpace(S);
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

procedure TViewpointsList.Recalculate(Scene: TVRMLScene);
begin
  Count := 0;

  if Scene <> nil then
    Scene.EnumerateViewpoints(@AddNodeTransform);

  if MenuJumpToViewpoint <> nil then
    MakeMenuJumpToViewpoint;
end;

procedure TViewpointsList.MakeMenuJumpToViewpoint;
var
  Node: TVRMLViewpointNode;
  I: Integer;
  S: string;
  Viewpoint: TNodeX3DViewpointNode;
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
    if Node is TNodeX3DViewpointNode then
    begin
      Viewpoint := TNodeX3DViewpointNode(Node);
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

initialization
  ViewpointsList := TViewpointsList.Create;
finalization
  FreeAndNil(ViewpointsList);
end.
