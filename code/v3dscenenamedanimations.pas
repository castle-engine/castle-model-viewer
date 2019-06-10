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

{ UI to play named animations of the current scene, from TCastleScene.Animations. }
unit V3DSceneNamedAnimations;

interface

uses Classes,
  CastleControls, CastleScene, CastleUIControls, CastleWindow;

{ Recreate UI to show animations on the scene.
  Scene may be @nil, to not show any animations. }
procedure RefreshNamedAnimationsUi(const Window: TCastleWindowBase;
  const Scene: TCastleScene; const WindowMarginTop: Single);

function GetNamedAnimationsUiExists: Boolean;
procedure SetNamedAnimationsUiExists(const Value: Boolean);

property NamedAnimationsUiExists: Boolean
  read GetNamedAnimationsUiExists
  write SetNamedAnimationsUiExists;

implementation

uses SysUtils,
  CastleLog, CastleSceneCore, CastleColors,
  V3DSceneCaptions;

{ State preserved across RefreshNamedAnimationsUi ---------------------------- }

var
  Loop: boolean = true;
  Forward: boolean = true;

{ TNamedAnimationsUi and friend classes -------------------------------------- }

type
  TButtonAnimation = class(TCastleButton)
  public
    AnimationName: string;
  end;

  TNamedAnimationsUi = class(TCastleVerticalGroup)
  public
    Scene: TCastleScene;
    procedure ChangeCheckboxLoop(Sender: TObject);
    procedure ChangeCheckboxForward(Sender: TObject);
    procedure ClickAnimation(Sender: TObject);
    procedure ClickResetAnimationState(Sender: TObject);
    procedure ClickStopAnimation(Sender: TObject);
  end;

procedure TNamedAnimationsUi.ClickAnimation(Sender: TObject);
var
  AnimationName: String;
begin
  AnimationName := (Sender as TButtonAnimation).AnimationName;
  if not Scene.PlayAnimation(AnimationName, Loop, Forward) then
    WritelnWarning('Named Animations', Format('Animation "%s" no longer exists, it was removed from scene since loading',
      [AnimationName]));
end;

procedure TNamedAnimationsUi.ChangeCheckboxLoop(Sender: TObject);
begin
  // change future animations
  Loop := (Sender as TCastleCheckbox).Checked;
  // also change currently playing animation, if any
  if Scene.CurrentAnimation <> nil then
    Scene.CurrentAnimation.Loop := Loop;
end;

procedure TNamedAnimationsUi.ChangeCheckboxForward(Sender: TObject);
begin
  inherited;
  // change future animations
  Forward := (Sender as TCastleCheckbox).Checked;
  // also change currently playing animation, if any
  if Scene.CurrentAnimation <> nil then
    Scene.CurrentAnimation.FractionIncreasing := Forward;
end;

procedure TNamedAnimationsUi.ClickResetAnimationState(Sender: TObject);
begin
  Scene.ResetAnimationState;
end;

procedure TNamedAnimationsUi.ClickStopAnimation(Sender: TObject);
begin
  Scene.StopAnimation;
end;

{ RefreshNamedAnimations ----------------------------------------------------- }

var
  NamedAnimationsUi: TNamedAnimationsUi;

procedure RefreshNamedAnimationsUi(const Window: TCastleWindowBase;
  const Scene: TCastleScene; const WindowMarginTop: Single);

  procedure AppendLoop;
  var
    Ui: TCastleCheckbox;
  begin
    Ui := TCastleCheckbox.Create(NamedAnimationsUi);
    Ui.Caption := 'Loop';
    Ui.Checked := Loop;
    Ui.OnChange := @NamedAnimationsUi.ChangeCheckboxLoop;
    Ui.TextColor := White;
    Ui.CheckboxColor := White;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendForward;
  var
    Ui: TCastleCheckbox;
  begin
    Ui := TCastleCheckbox.Create(NamedAnimationsUi);
    Ui.Caption := 'Forward';
    Ui.Checked := Forward;
    Ui.OnChange := @NamedAnimationsUi.ChangeCheckboxForward;
    Ui.TextColor := White;
    Ui.CheckboxColor := White;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendAnimationsInfo(const AnimationsCount: Cardinal);
  var
    Ui: TCastleLabel;
  begin
    Ui := TCastleLabel.Create(NamedAnimationsUi);
    Ui.Caption := Format('%d Animations', [AnimationsCount]);
    Ui.Color := White;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendAnimation(const AnimationName: String);
  var
    Ui: TButtonAnimation;
  begin
    // TODO: scroll area
    Ui := TButtonAnimation.Create(NamedAnimationsUi);
    Ui.Caption := Format('%s (%f)', [
      SForCaption(AnimationName),
      Scene.AnimationDuration(AnimationName)
    ]);
    Ui.AnimationName := AnimationName;
    Ui.OnClick := @NamedAnimationsUi.ClickAnimation;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendSpacer(const Height: Single);
  var
    Ui: TCastleUserInterface;
  begin
    Ui := TCastleUserInterface.Create(NamedAnimationsUi);
    Ui.Height := 10;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendStopAnimation;
  var
    Ui: TCastleButton;
  begin
    Ui := TCastleButton.Create(NamedAnimationsUi);
    Ui.Caption := 'Stop Animation';
    Ui.OnClick := @NamedAnimationsUi.ClickStopAnimation;
    NamedAnimationsUi.InsertFront(Ui);
  end;

  procedure AppendResetAnimationState;
  var
    Ui: TCastleButton;
  begin
    Ui := TCastleButton.Create(NamedAnimationsUi);
    Ui.Caption := 'Reset Animation State';
    Ui.OnClick := @NamedAnimationsUi.ClickResetAnimationState;
    NamedAnimationsUi.InsertFront(Ui);
  end;

var
  NamedAnimations: TStrings;
  I: Integer;
  OldNamedAnimationsExists: Boolean;
begin
  OldNamedAnimationsExists := (NamedAnimationsUi <> nil) and NamedAnimationsUi.Exists;

  { free previous UI owned by NamedAnimationsUi, including previous animations }
  FreeAndNil(NamedAnimationsUi);

  NamedAnimationsUi := TNamedAnimationsUi.Create(Window);
  NamedAnimationsUi.Exists := OldNamedAnimationsExists;
  NamedAnimationsUi.Scene := Scene;
  NamedAnimationsUi.Frame := true;
  NamedAnimationsUi.Padding := 10;
  NamedAnimationsUi.Spacing := 4;
  NamedAnimationsUi.Anchor(hpLeft, 10);
  NamedAnimationsUi.Anchor(vpTop, - WindowMarginTop - 10);
  Window.Controls.InsertFront(NamedAnimationsUi);

  if Scene <> nil then
  begin
    AppendLoop;
    AppendForward;
    AppendSpacer(10);
    NamedAnimations := Scene.AnimationsList;
    AppendAnimationsInfo(NamedAnimations.Count);
    if NamedAnimations.Count <> 0 then
    begin
      for I := 0 to NamedAnimations.Count - 1 do
        AppendAnimation(NamedAnimations[I]);
      AppendSpacer(10);
      AppendStopAnimation;
      AppendResetAnimationState;
    end;
  end;

  { The state with Scene = nil is never visible by user in practice,
    so we don't bother to make it look nice with some label 'open some scene'. }
end;

function GetNamedAnimationsUiExists: Boolean;
begin
  Result := (NamedAnimationsUi <> nil) and NamedAnimationsUi.Exists;
end;

procedure SetNamedAnimationsUiExists(const Value: Boolean);
begin
  NamedAnimationsUi.Exists := Value;
end;

end.
