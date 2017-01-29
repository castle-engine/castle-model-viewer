{
  Copyright 2004-2017 Michalis Kamburelis.

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

{ }
unit V3DSceneViewpoints;

interface

uses CastleVectors, X3DNodes, CastleWindow, CastleUtils, Classes, CastleClassUtils,
  CastleSceneCore, CastleScene, CastleSceneManager, CastleKeysMouse, V3DSceneCaptions;

type
  { Menu item referring to a viewpoint.

    Note that in some moments (when we process some events),
    the @link(Viewpoint) instance may no longer be valid:
    we can get TCastleSceneCore.ViewpointStack.OnBoundChanged
    when other viewpoint nodes are already freed. }
  TMenuItemViewpoint = class(TMenuItemRadio)
  private
    FViewpoint: TAbstractViewpointNode;
  public
    property Viewpoint: TAbstractViewpointNode read FViewpoint write FViewpoint;
  end;

  { A menu that as children has TMenuItemViewpoint (specific viewpoint)
    or other TMenuViewpointGroup (only with ViewpointGroup <> nil).

    It is associated with a ViewpointGroup node. Unless it's a root node
    (the one descending from TMenuViewpoints), then ViewpointGroup = nil. }
  TMenuViewpointGroup = class(TMenu)
  private
    FViewpointGroup: TViewpointGroupNode;
  public
    property ViewpointGroup: TViewpointGroupNode read FViewpointGroup write FViewpointGroup;
    { Find a children TMenuViewpointGroup associated with given AViewpointGroup
      node. }
    function FindGroup(AViewpointGroup: TViewpointGroupNode): TMenuViewpointGroup;
  end;

  TMenuViewpoints = class(TMenuViewpointGroup)
  private
    { When nil, then BoundViewpoint is always nil }
    ViewpointsRadioGroup: TMenuItemRadioGroup;

    { Used only during AddViewpoint callback }
    SceneBoundViewpoint: TAbstractViewpointNode;
    AddViewpointGroups: TX3DNodeList;

    function AddViewpoint(
      Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;
    function GetBoundViewpoint: TMenuItemViewpoint;
    procedure SetBoundViewpoint(const Value: TMenuItemViewpoint);
  public
    { Currently bound viewpoint (@nil if none),
      used to make appropriate menu item "checked".
      This only takes care of updating menu state,
      it does not actually jump to viewpoint,
      for this see JumpToViewpoint.

      Note: we cannot make this of TAbstractViewpointNode,
      because the same viewpoint node may be USEd many times
      within a single scene. }
    property BoundViewpoint: TMenuItemViewpoint
      read GetBoundViewpoint write SetBoundViewpoint;

    { Recalculate menu contents.
      Special value Scene = @nil means no scene is loaded, so no
      viewpoint nodes exist. }
    procedure Recalculate(Scene: TCastleSceneCore);

    function ItemOf(Viewpoint: TAbstractViewpointNode): TMenuItemViewpoint;

    { Jump to specific viewpoint, by order.
      This both updates the menu state (BoundViewpoint)
      and actually moves the camera (JumpToViewpoint). }
    procedure Initial(const SceneManager: TCastleSceneManager);
    procedure Previous(const SceneManager: TCastleSceneManager);
    procedure Next(const SceneManager: TCastleSceneManager);
    procedure Final(const SceneManager: TCastleSceneManager);
  end;

var
  Viewpoints: TMenuViewpoints;

{ Parse --viewpoint command-line option, if exists. }
procedure ViewpointsParseParameters;

{ Set Scene.InitialViewpoint values.
  If EnableCommandLineCustomization then (only for the 1st time it's used)
  applies the --viewpoint command-line option effect. }
procedure SetInitialViewpoint(Scene: TCastleScene;
  const EnableCommandLineCustomization: boolean);

{ Switch camera to given viewpoint. This only switches the 3D camera,
  does not update the "Viewpoints" menu state (for this, see
  TMenuViewpoints.BoundViewpoint). }
procedure JumpToViewpoint(const SceneManager: TCastleSceneManager;
  const Viewpoint: TAbstractViewpointNode);

implementation

uses SysUtils, CastleStringUtils, CastleParameters, V3DSceneStatus;

function TMenuViewpointGroup.FindGroup(
  AViewpointGroup: TViewpointGroupNode): TMenuViewpointGroup;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Entries[I] is TMenuViewpointGroup) and
       (TMenuViewpointGroup(Entries[I]).ViewpointGroup = AViewpointGroup) then
      Exit(TMenuViewpointGroup(Entries[I]));
  Result := nil;
end;

function NodeToCaption(const Node: TX3DNode): string;
begin
  if Node is TAbstractViewpointNode then
    Result := TAbstractViewpointNode(Node).SmartDescription else
  if Node is TViewpointGroupNode then
    Result := TViewpointGroupNode(Node).SmartDescription else
    Result := '';
  Result := SForCaption(Result);
end;

const
  { We don't add more menu item entries for viewpoints. }
  MaxMenuItems = 20;

function TMenuViewpoints.AddViewpoint(
  Node: TX3DNode; StateStack: TX3DGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean): Pointer;

  { Nice menu item caption for TAbstractViewpointNode or TViewpointGroupNode }
  function ViewpointToMenuCaption(const ItemIndex: Integer; Node: TX3DNode): string;
  begin
    if ItemIndex < 10 then
      Result := '_';
    Result += IntToStr(ItemIndex) + ': ';
    Result += SQuoteMenuEntryCaption(NodeToCaption(Node));
  end;

  { Scan ParentInfo information for TViewpointGroupNode.

    If every ViewpointGroup has "displayed" = true, then we
    fill the AddViewpointGroups list with found ViewpointGroup nodes.
    Ordered from top-most (so the order is similar to submenus order,
    and reversed than order on ParentInfo list).

    If some ViewpointGroup on the way has "displayed" = false, then
    we return false and leave AddViewpointGroups in undefined state. }
  function GetParentViewpointGroups(AddViewpointGroups: TX3DNodeList): boolean;
  var
    Info: PTraversingInfo;
  begin
    AddViewpointGroups.Count := 0;
    Result := true;

    Info := ParentInfo;
    while Info <> nil do
    begin
      if Info^.Node is TViewpointGroupNode then
      begin
        if not TViewpointGroupNode(Info^.Node).FdDisplayed.Value then
          Exit(false);
        AddViewpointGroups.Insert(0, Info^.Node);
      end;
      Info := Info^.ParentInfo;
    end;
  end;

  { For all the ViewpointGroup gathered in AddViewpointGroups,
    create (or find existing) appropriate submenu.
    Returns the final menu where you should directly add your viewpoint. }
  function CreateParentViewpointGroups(AddViewpointGroups: TX3DNodeList): TMenuViewpointGroup;
  var
    Group: TViewpointGroupNode;
    I: Integer;
    NewResult: TMenuViewpointGroup;
  begin
    Result := Self;
    for I := 0 to AddViewpointGroups.Count - 1 do
    begin
      Group := AddViewpointGroups[I] as TViewpointGroupNode;
      NewResult := Result.FindGroup(Group);
      if NewResult = nil then
      begin
        NewResult := TMenuViewpointGroup.Create(
          ViewpointToMenuCaption(Result.Count, Group));
        NewResult.ViewpointGroup := Group;
        Result.Append(NewResult);
      end;
      Result := NewResult;
    end;
  end;

var
  ItemIndex: Integer;
  S: string;
  MenuItem: TMenuItemViewpoint;
  Group: TMenuViewpointGroup;
begin
  Result := nil;
  if not GetParentViewpointGroups(AddViewpointGroups) then Exit;

  Group := CreateParentViewpointGroups(AddViewpointGroups);

  ItemIndex := Group.Count;
  if ItemIndex < MaxMenuItems then
  begin
    S := ViewpointToMenuCaption(ItemIndex, Node);

    MenuItem := TMenuItemViewpoint.Create(S, 300, SceneBoundViewpoint = Node, false);
    MenuItem.Viewpoint := Node as TAbstractViewpointNode;

    { If we have found the currently bound node (and made this menu item
      selected), then reset SceneBoundViewpoint to nil. This way the first
      encountered viewpoint node matching Scene.ViewpointStack.Top will be
      selected.

      TODO: This isn't really 100% correct, but Scene simply doesn't tell
      us which instance of viewpoint node is actually bound. }
    if SceneBoundViewpoint = Node then
      SceneBoundViewpoint := nil;

    if ViewpointsRadioGroup = nil then
      ViewpointsRadioGroup := MenuItem.Group else
      MenuItem.Group := ViewpointsRadioGroup;

    Group.Append(MenuItem);
  end;
end;

procedure TMenuViewpoints.Recalculate(Scene: TCastleSceneCore);
var
  M: TMenuItem;
begin
  MenuUpdateBegin;
  Clear;

  ViewpointsRadioGroup := nil;

  if (Scene <> nil) and
     (Scene.RootNode <> nil) then
  begin
    SceneBoundViewpoint := Scene.ViewpointStack.Top;
    AddViewpointGroups := TX3DNodeList.Create(false);

    Scene.RootNode.Traverse(TAbstractViewpointNode, @AddViewpoint);

    SceneBoundViewpoint := nil; //< just for safety
    FreeAndNil(AddViewpointGroups);
  end;

  Append(TMenuSeparator.Create);
  M := TMenuItem.Create('Initial Viewpoint' , 65, K_Home);
  M.Enabled := ViewpointsRadioGroup <> nil;
  Append(M);
  M := TMenuItem.Create('Previous Viewpoint', 66, K_PageUp);
  M.Enabled := ViewpointsRadioGroup <> nil;
  Append(M);
  M := TMenuItem.Create('Next Viewpoint'    , 67, K_PageDown);
  M.Enabled := ViewpointsRadioGroup <> nil;
  Append(M);
  M := TMenuItem.Create('Final Viewpoint'   , 68, K_End);
  M.Enabled := ViewpointsRadioGroup <> nil;
  Append(M);
  Append(TMenuSeparator.Create);
  Append(TMenuItem.Create('Default VRML 1.0 viewpoint', 51));
  Append(TMenuItem.Create('Default VRML 2.0 (and X3D) viewpoint', 52));
  Append(TMenuSeparator.Create);
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Y up, -Z dir)', 53));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Y up, +Z dir)', 54));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Y up, -X dir)', 55));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Y up, +X dir)', 56));
  Append(TMenuSeparator.Create);
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Z up, -X dir)', 57));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Z up, +X dir)', 58));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Z up, -Y dir)', 59));
  Append(TMenuItem.Create('Calculated viewpoint to see the scene (+Z up, +Y dir)', 60));

  MenuUpdateEnd;
end;

function TMenuViewpoints.GetBoundViewpoint: TMenuItemViewpoint;
begin
  if ViewpointsRadioGroup <> nil then
    Result := ViewpointsRadioGroup.Selected as TMenuItemViewpoint else
    Result := nil; //< then no viewpoints exist
end;

procedure TMenuViewpoints.SetBoundViewpoint(const Value: TMenuItemViewpoint);
begin
  if ViewpointsRadioGroup <> nil then
    ViewpointsRadioGroup.Selected := Value else
    { When no viewpoints exist, BoundViewpoint is always nil }
    Assert(Value = nil);
end;

function TMenuViewpoints.ItemOf(Viewpoint: TAbstractViewpointNode): TMenuItemViewpoint;
var
  I: Integer;
begin
  if ViewpointsRadioGroup <> nil then
    for I := 0 to ViewpointsRadioGroup.Count - 1 do
      if TMenuItemViewpoint(ViewpointsRadioGroup.Items[I]).Viewpoint = Viewpoint then
        Exit(TMenuItemViewpoint(ViewpointsRadioGroup.Items[I]));
  Result := nil;
end;

procedure TMenuViewpoints.Initial(const SceneManager: TCastleSceneManager);
begin
  if (ViewpointsRadioGroup <> nil) and
     (ViewpointsRadioGroup.Count <> 0) then
  begin
    BoundViewpoint := ViewpointsRadioGroup.First as TMenuItemViewpoint;
    JumpToViewpoint(SceneManager, BoundViewpoint.Viewpoint);
  end;
end;

procedure TMenuViewpoints.Previous(const SceneManager: TCastleSceneManager);
var
  Item: TMenuItemRadio;
begin
  if (ViewpointsRadioGroup <> nil) and
     ViewpointsRadioGroup.Previous(Item) then
  begin
    BoundViewpoint := Item as TMenuItemViewpoint;
    JumpToViewpoint(SceneManager, BoundViewpoint.Viewpoint);
  end;
end;

procedure TMenuViewpoints.Next(const SceneManager: TCastleSceneManager);
var
  Item: TMenuItemRadio;
begin
  if (ViewpointsRadioGroup <> nil) and
     ViewpointsRadioGroup.Next(Item) then
  begin
    BoundViewpoint := Item as TMenuItemViewpoint;
    JumpToViewpoint(SceneManager, BoundViewpoint.Viewpoint);
  end;
end;

procedure TMenuViewpoints.Final(const SceneManager: TCastleSceneManager);
begin
  if (ViewpointsRadioGroup <> nil) and
     (ViewpointsRadioGroup.Count <> 0) then
  begin
    BoundViewpoint := ViewpointsRadioGroup.Last as TMenuItemViewpoint;
    JumpToViewpoint(SceneManager, BoundViewpoint.Viewpoint);
  end;
end;

{ command-line options ------------------------------------------------------- }

var
  HasCommandLineViewpoint: boolean = false;
  InitialViewpointIndex: Integer;
  InitialViewpointName: string;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  Assert(OptionNum = 0);
  HasCommandLineViewpoint := true;
  InitialViewpointIndex := 0;
  InitialViewpointName := '';

  { calculate InitialViewpoint* }
  try
    InitialViewpointIndex := StrToInt(Argument);
  except on EConvertError do
    InitialViewpointName := Argument;
  end;
end;

procedure ViewpointsParseParameters;
const
  Options: array[0..0]of TOption =
  ((Short:#0; Long:'viewpoint'; Argument: oaRequired));
begin
  Parameters.Parse(Options, @OptionProc, nil, true);
end;

procedure SetInitialViewpoint(Scene: TCastleScene;
  const EnableCommandLineCustomization: boolean);
begin
  if EnableCommandLineCustomization and HasCommandLineViewpoint then
  begin
    HasCommandLineViewpoint := false;
    Scene.InitialViewpointIndex := InitialViewpointIndex;
    Scene.InitialViewpointName := InitialViewpointName;
  end else
  begin
    Scene.InitialViewpointIndex := 0;
    Scene.InitialViewpointName := '';
  end;
end;

procedure JumpToViewpoint(const SceneManager: TCastleSceneManager;
  const Viewpoint: TAbstractViewpointNode);
var
  Pos, Dir, Up, GravityUp: TVector3Single;
  Scene: TCastleScene;
begin
  StatusText.Flash('Switching to viewpoint: ' + NodeToCaption(Viewpoint));
  Scene := SceneManager.MainScene;
  if Viewpoint = Scene.ViewpointStack.Top then
  begin
    { Sending set_bind = true works fine if it's not current viewpoint,
      otherwise nothing happens... So just explicitly go to viewpoint
      position. }
    Viewpoint.GetView(Pos, Dir, Up, GravityUp);
    Scene.CameraTransition(SceneManager.Camera, Pos, Dir, Up, GravityUp);
  end else
    Viewpoint.EventSet_Bind.Send(true);
end;

initialization
  Viewpoints := TMenuViewpoints.Create('Jump to Viewpoint');
end.
