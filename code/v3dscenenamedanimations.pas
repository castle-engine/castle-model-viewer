{
  Copyright 2014-2018 Michalis Kamburelis.

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

{ Managed named animations of current scene, see TCastleScene.Animations. }
unit V3DSceneNamedAnimations;

interface

uses CastleWindow, CastleScene;

var
  MenuNamedAnimations: TMenu;

procedure CreateMenuNamedAnimations;

procedure RefreshNamedAnimations(const Scene: TCastleScene);

implementation

uses SysUtils, Classes,
  CastleLog, CastleSceneCore,
  V3DSceneCaptions;

var
  Loop: boolean = true;
  Forward: boolean = true;

type
  TMenuItemLoop = class(TMenuItemChecked)
    Scene: TCastleScene;
    function DoClick: boolean; override;
  end;

  TMenuItemForward = class(TMenuItemChecked)
    Scene: TCastleScene;
    function DoClick: boolean; override;
  end;

  TMenuItemAnimation = class(TMenuItem)
    Scene: TCastleScene;
    AnimationName: string;
    constructor Create(const AScene: TCastleScene; const AnAnimationName: string);
    function DoClick: boolean; override;
  end;

constructor TMenuItemAnimation.Create(
  const AScene: TCastleScene; const AnAnimationName: string);
var
  MenuCaption: string;
begin
  Scene := AScene;
  AnimationName := AnAnimationName;
  MenuCaption := Format('%s (%f)',
    [ SQuoteMenuEntryCaption(SForCaption(AnimationName)),
      AScene.AnimationDuration(AnimationName) ]);
  inherited Create(MenuCaption, 0 { unused });
end;

function TMenuItemAnimation.DoClick: boolean;
begin
  inherited;
  if not Scene.PlayAnimation(AnimationName, Loop, Forward) then
    WritelnWarning('Named Animations', Format('Animation "%s" no longer exists, it was removed from scene since loading',
      [AnimationName]));
  Result := true;
end;

function TMenuItemLoop.DoClick: boolean;
begin
  inherited;
  // change future animations
  Loop := Checked;
  // also change currently playing animation, if any
  if Scene.CurrentAnimation <> nil then
    Scene.CurrentAnimation.Loop := Loop;
  Result := true;
end;

function TMenuItemForward.DoClick: boolean;
begin
  inherited;
  // change future animations
  Forward := Checked;
  // also change currently playing animation, if any
  if Scene.CurrentAnimation <> nil then
    Scene.CurrentAnimation.FractionIncreasing := Forward;
  Result := true;
end;

procedure CreateMenuNamedAnimations;
begin
  if MenuNamedAnimations = nil then
    MenuNamedAnimations := TMenu.Create('_Named Animations');
end;

procedure RefreshNamedAnimations(const Scene: TCastleScene);

  procedure AppendLoop;
  var
    MenuItem: TMenuItemLoop;
  begin
    MenuItem := TMenuItemLoop.Create('Loop', 0, Loop, true);
    MenuItem.Scene := Scene;
    MenuNamedAnimations.Append(MenuItem);
  end;

  procedure AppendForward;
  var
    MenuItem: TMenuItemForward;
  begin
    MenuItem := TMenuItemForward.Create('Forward', 0, Forward, true);
    MenuItem.Scene := Scene;
    MenuNamedAnimations.Append(MenuItem);
  end;

var
  NamedAnimations: TStrings;
  I: Integer;
begin
  CreateMenuNamedAnimations;

  MenuNamedAnimations.Clear;
  AppendLoop;
  AppendForward;
  MenuNamedAnimations.Append(TMenuSeparator.Create);

  NamedAnimations := Scene.AnimationsList;
  for I := 0 to NamedAnimations.Count - 1 do
    MenuNamedAnimations.Append(
      TMenuItemAnimation.Create(Scene, NamedAnimations[I]));
end;

end.
