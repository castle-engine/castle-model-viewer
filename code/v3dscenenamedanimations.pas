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

const
  LoopingNames: array [TPlayAnimationLooping] of string =
  ( 'Default Looping',
    'Force Looping',
    'Force Not Looping' );

var
  Looping: TPlayAnimationLooping = paDefault;

type
  TMenuItemLooping = class(TMenuItemRadio)
    LoopingValue: TPlayAnimationLooping;
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
  if not Scene.PlayAnimation(AnimationName, Looping) then
    WritelnWarning('Named Animations', Format('Animation "%s" no longer exists, it was removed from scene since loading',
      [AnimationName]));
  Result := true;
end;

function TMenuItemLooping.DoClick: boolean;
begin
  inherited;
  Looping := LoopingValue;
  Result := true;
end;

procedure CreateMenuNamedAnimations;
begin
  if MenuNamedAnimations = nil then
    MenuNamedAnimations := TMenu.Create('_Named Animations');
end;

procedure RefreshNamedAnimations(const Scene: TCastleScene);

  procedure AppendLooping;
  var
    Group: TMenuItemRadioGroup;
    Radio: TMenuItemLooping;
    L: TPlayAnimationLooping;
  begin
    Group := nil;
    for L := Low(L) to High(L) do
    begin
      Radio := TMenuItemLooping.Create(LoopingNames[L], 0, L = Looping, true);
      Radio.LoopingValue := L;
      if Group = nil then
        Group := Radio.Group else
        Radio.Group := Group;
      MenuNamedAnimations.Append(Radio);
    end;
    MenuNamedAnimations.Append(TMenuSeparator.Create);
  end;

var
  NamedAnimations: TStrings;
  I: Integer;
begin
  CreateMenuNamedAnimations;

  MenuNamedAnimations.Clear;
  AppendLooping;

  NamedAnimations := Scene.AnimationsList;
  for I := 0 to NamedAnimations.Count - 1 do
    MenuNamedAnimations.Append(
      TMenuItemAnimation.Create(Scene, NamedAnimations[I]));
end;

end.
