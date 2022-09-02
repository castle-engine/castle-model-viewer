{
  Copyright 2014-2022 Michalis Kamburelis.

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
procedure RefreshNamedAnimationsUi(const Window: TCastleWindow;
  const Scene: TCastleScene; const WindowMarginTop: Single);

function GetNamedAnimationsUiExists: Boolean;
procedure SetNamedAnimationsUiExists(const Value: Boolean);

property NamedAnimationsUiExists: Boolean
  read GetNamedAnimationsUiExists
  write SetNamedAnimationsUiExists;

implementation

uses SysUtils, Math,
  CastleLog, CastleSceneCore, CastleColors, CastleUtils, X3DNodes,
  V3DSceneCaptions;

const
  Margin = 10;

{ State preserved across RefreshNamedAnimationsUi ---------------------------- }

var
  Loop: boolean = true;
  Forward: boolean = true;
  MultipleAnimations: boolean = false;
  Transition: Single = 0;

{ TButtonAnimation ----------------------------------------------------------- }

type
  TButtonAnimation = class(TCastleButton)
  strict private
    FTimeSensor: TTimeSensorNode;
    procedure SetTimeSensor(const Value: TTimeSensorNode);
    procedure TimeSensorDestruction(const Node: TX3DNode);
  public
    AnimationName: String;
    { Store TTimeSensorNode reference, not only AnimationName,
      as time sensor better identifies the animation.
      Testcase: Bee_10.x3dv that Inlines and IMPORTs animations
      from multiple copies of the same glTF file. }
    property TimeSensor: TTimeSensorNode read FTimeSensor write SetTimeSensor;
    destructor Destroy; override;
  end;

procedure TButtonAnimation.TimeSensorDestruction(const Node: TX3DNode);
begin
  { Free TButtonAnimation when associated TimeSensor is freed.
    Testcase:
    - open demo-models/x3d/data_uri.x3dv
    - open "Animations" panel
    - click on "Anchor to a model embedded using data URI"
  }
  FTimeSensor := nil; // no point doing FTimeSensor.RemoveDestructionNotification
  Destroy;
end;

destructor TButtonAnimation.Destroy;
begin
  TimeSensor := nil;
  inherited;
end;

procedure TButtonAnimation.SetTimeSensor(const Value: TTimeSensorNode);
begin
  if FTimeSensor <> Value then
  begin
    if FTimeSensor <> nil then
      FTimeSensor.RemoveDestructionNotification(@TimeSensorDestruction);
    FTimeSensor := Value;
    if FTimeSensor <> nil then
      FTimeSensor.AddDestructionNotification(@TimeSensorDestruction);
  end;
end;

{ TNamedAnimationsUi and friend classes -------------------------------------- }

type
  TNamedAnimationsUi = class(TCastleVerticalGroup)
  public
    Scene: TCastleScene;
    AnimationsScrollView: TCastleScrollView;
    AnimationsScrollGroup: TCastleVerticalGroup;
    LabelCurrentAnimation: TCastleLabel;
    constructor Create(const AOwner: TComponent; const AScene: TCastleScene); reintroduce;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure ChangeCheckboxLoop(Sender: TObject);
    procedure ChangeCheckboxForward(Sender: TObject);
    procedure ChangeSliderTransition(Sender: TObject);
    procedure ChangeSliderSpeed(Sender: TObject);
    procedure ChangeCheckboxMultipleAnimations(Sender: TObject);
    procedure ClickAnimation(Sender: TObject);
    procedure ClickResetAnimationState(Sender: TObject);
    procedure ClickStopAnimation(Sender: TObject);
  end;

constructor TNamedAnimationsUi.Create(const AOwner: TComponent; const AScene: TCastleScene);

  procedure AppendLoop;
  var
    Ui: TCastleCheckbox;
  begin
    Ui := TCastleCheckbox.Create(Self);
    Ui.Caption := 'Loop';
    Ui.Checked := Loop;
    Ui.OnChange := @ChangeCheckboxLoop;
    Ui.TextColor := White;
    Ui.CheckboxColor := White;
    InsertFront(Ui);
  end;

  procedure AppendForward;
  var
    Ui: TCastleCheckbox;
  begin
    Ui := TCastleCheckbox.Create(Self);
    Ui.Caption := 'Forward';
    Ui.Checked := Forward;
    Ui.OnChange := @ChangeCheckboxForward;
    Ui.TextColor := White;
    Ui.CheckboxColor := White;
    InsertFront(Ui);
  end;

  procedure AppendMultipleAnimations;
  var
    Ui: TCastleCheckbox;
  begin
    Ui := TCastleCheckbox.Create(Self);
    Ui.Caption := 'Enable Multiple Simultaneous Animations';
    Ui.Checked := MultipleAnimations;
    Ui.OnChange := @ChangeCheckboxMultipleAnimations;
    Ui.TextColor := White;
    Ui.CheckboxColor := White;
    InsertFront(Ui);
  end;

  procedure AppendAnimationsInfo(const AnimationsCount: Cardinal);
  var
    Ui: TCastleLabel;
  begin
    Ui := TCastleLabel.Create(Self);
    Ui.Caption := Format('%d Animations', [AnimationsCount]);
    Ui.Color := White;
    InsertFront(Ui);
  end;

  procedure AppendAnimation(const AnimationName: String; const TimeSensor: TTimeSensorNode);
  var
    Ui: TButtonAnimation;
  begin
    Ui := TButtonAnimation.Create(Self);
    Ui.Caption := Format('%s (%f)', [
      SForCaption(AnimationName),
      Scene.AnimationDuration(AnimationName)
    ]);
    Ui.TimeSensor := TimeSensor;
    Ui.AnimationName := AnimationName;
    Ui.OnClick := @ClickAnimation;
    AnimationsScrollGroup.InsertFront(Ui);
  end;

  procedure AppendSpacer(const Height: Single);
  var
    Ui: TCastleUserInterface;
  begin
    Ui := TCastleUserInterface.Create(Self);
    Ui.Height := Height;
    InsertFront(Ui);
  end;

  procedure AppendCurrentAnimation;
  begin
    LabelCurrentAnimation := TCastleLabel.Create(Self);
    LabelCurrentAnimation.Caption := 'Current:';
    LabelCurrentAnimation.Color := White;
    LabelCurrentAnimation.Exists := false; // will be shown in Update if needed
    InsertFront(LabelCurrentAnimation);
  end;

  procedure AppendStopAnimation;
  var
    Ui: TCastleButton;
  begin
    Ui := TCastleButton.Create(Self);
    Ui.Caption := 'Stop Animation';
    Ui.OnClick := @ClickStopAnimation;
    InsertFront(Ui);
  end;

  procedure AppendResetAnimationState;
  var
    Ui: TCastleButton;
  begin
    Ui := TCastleButton.Create(Self);
    Ui.Caption := 'Reset Animation State';
    Ui.OnClick := @ClickResetAnimationState;
    InsertFront(Ui);
  end;

  procedure AppendTransition;
  var
    Lab: TCastleLabel;
    Slider: TCastleFloatSlider;
    LabelAndSlider: TCastleHorizontalGroup;
  begin
    LabelAndSlider := TCastleHorizontalGroup.Create(Self);
    LabelAndSlider.Spacing := Margin;

    Lab := TCastleLabel.Create(Self);
    Lab.Caption := 'Transition:';
    Lab.Color := White;
    LabelAndSlider.InsertFront(Lab);

    Slider := TCastleFloatSlider.Create(Self);
    Slider.Min := 0;
    Slider.Max := 5;
    Slider.Value := Transition;
    Slider.OnChange := @ChangeSliderTransition;
    LabelAndSlider.InsertFront(Slider);

    InsertFront(LabelAndSlider);
  end;

  procedure AppendSpeed;
  var
    Lab: TCastleLabel;
    Slider: TCastleFloatSlider;
    LabelAndSlider: TCastleHorizontalGroup;
  begin
    LabelAndSlider := TCastleHorizontalGroup.Create(Self);
    LabelAndSlider.Spacing := Margin;

    Lab := TCastleLabel.Create(Self);
    Lab.Caption := 'Playback Speed:';
    Lab.Color := White;
    LabelAndSlider.InsertFront(Lab);

    Slider := TCastleFloatSlider.Create(Self);
    Slider.Min := 0;
    Slider.Max := 10;
    { We use Scene.TimePlayingSpeed to preserve playing speed when scene changes.
      (view3dscene doesn't create new Scene each time, it only calls Load on one
      Scene instance). }
    if Scene <> nil then
      Slider.Value := Scene.TimePlayingSpeed
    else
      Slider.Value := 1;
    Slider.OnChange := @ChangeSliderSpeed;
    LabelAndSlider.InsertFront(Slider);

    InsertFront(LabelAndSlider);
  end;

  procedure CreateScrollView;
  begin
    { We place animations in TCastleScrollView,
      in case we have too many animations to fit on screen. }
    AnimationsScrollView := TCastleScrollView.Create(Self);
    AnimationsScrollView.ScrollArea.AutoSizeToChildren := true;
    InsertFront(AnimationsScrollView);

    AnimationsScrollGroup := TCastleVerticalGroup.Create(Self);
    AnimationsScrollGroup.Spacing := Spacing; // copy own Spacing
    AnimationsScrollView.ScrollArea.InsertFront(AnimationsScrollGroup);
  end;

var
  NamedAnimations: TStrings;
  I: Integer;
begin
  inherited Create(AOwner);

  Scene := AScene;
  Frame := true;
  Padding := Margin;
  Spacing := 4;

  if Scene <> nil then
  begin
    AppendLoop;
    AppendForward;
    AppendMultipleAnimations;
    AppendTransition;
    AppendSpeed;
    AppendSpacer(Margin);
    NamedAnimations := Scene.AnimationsList;
    AppendAnimationsInfo(NamedAnimations.Count);
    if NamedAnimations.Count <> 0 then
    begin
      CreateScrollView;
      for I := 0 to NamedAnimations.Count - 1 do
        AppendAnimation(NamedAnimations[I], Scene.AnimationTimeSensor(I));

      AppendSpacer(Margin);
      AppendStopAnimation;
      AppendResetAnimationState;
      { Since it dynamically appears / disappears, it's best to show it last,
        so that it doesn't shift the remaining UI when disappearing. }
      AppendCurrentAnimation;
    end;
  end;

  { The state with Scene = nil is never visible by user in practice,
    so we don't bother to make it look nice with some label 'open some scene'. }
end;

procedure TNamedAnimationsUi.Resize;

  procedure UpdateScrollViewSize;
  const
    { It is easier to set it experimentally than to calculate from code, for now }
    HeightForRestOfUi = 470;
  begin
    if AnimationsScrollView <> nil then
    begin
      Assert(Container <> nil);
      AnimationsScrollView.Width := AnimationsScrollGroup.EffectiveWidth
        + AnimationsScrollView.EffectiveScrollBarWidth;
      AnimationsScrollView.Height := Max(0, Min(AnimationsScrollGroup.EffectiveHeight,
        Container.UnscaledHeight - HeightForRestOfUi));
    end;
  end;

begin
  inherited;
  UpdateScrollViewSize;
end;

procedure TNamedAnimationsUi.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  TimeSensor: TTimeSensorNode;
  C: TCastleUserInterface;
begin
  inherited;

  // LabelCurrentAnimation is nil <=> Scene is nil
  if LabelCurrentAnimation <> nil then
  begin
    Assert(Scene <> nil);
    LabelCurrentAnimation.Exists := Scene.CurrentAnimation <> nil;
    if Scene.CurrentAnimation <> nil then
    begin
      LabelCurrentAnimation.Caption := Format('Current:' + NL + '%s' + NL + '%f / %f', [
        { We write animation name on a separate line,
          this way we know it will fit within AnimationsScrollView width,
          without the need to resize it. }
        Scene.CurrentAnimation.X3DName,
        Scene.CurrentAnimation.ElapsedTimeInCycle,
        Scene.CurrentAnimation.CycleInterval
      ]);
    end;
  end;

  { AnimationsScrollGroup doesn't exist if no animations.
    Testcase:
    - open glTF Drone animation,
    - play animation by clicking button in Animations panel,
    - load glTF DamagedHelmet through recent menu }
  if AnimationsScrollGroup <> nil then
    for C in AnimationsScrollGroup do
      if C is TButtonAnimation then
      begin
        Assert(Scene <> nil); // no TButtonAnimation should exist if Scene = nil
        TimeSensor := TButtonAnimation(C).TimeSensor;
        TButtonAnimation(C).Pressed := TimeSensor.IsActive;
      end;
end;

procedure TNamedAnimationsUi.ClickAnimation(Sender: TObject);
var
  Button: TButtonAnimation;
  AnimationName: String;
  Params: TPlayAnimationParameters;
  TimeSensor: TTimeSensorNode;
begin
  Button := Sender as TButtonAnimation;
  AnimationName := Button.AnimationName;
  if MultipleAnimations then
  begin
    TimeSensor := Button.TimeSensor;
    if TimeSensor.IsActive then
      TimeSensor.Stop
    else
      TimeSensor.Start(Loop, Forward);
  end else
  begin
    Params := TPlayAnimationParameters.Create;
    try
      Params.Name := AnimationName;
      Params.Loop := Loop;
      Params.Forward := Forward;
      Params.TransitionDuration := Transition;
      if not Scene.PlayAnimation(Params) then
        WritelnWarning('Named Animations', Format('Animation "%s" no longer exists, it was removed from scene since loading',
          [AnimationName]));
    finally FreeAndNil(Params) end;
  end;
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

procedure TNamedAnimationsUi.ChangeSliderTransition(Sender: TObject);
begin
  Transition := (Sender as TCastleFloatSlider).Value;
end;

procedure TNamedAnimationsUi.ChangeSliderSpeed(Sender: TObject);
begin
  if Scene <> nil then
    Scene.TimePlayingSpeed := (Sender as TCastleFloatSlider).Value;
end;

procedure TNamedAnimationsUi.ChangeCheckboxMultipleAnimations(Sender: TObject);
begin
  MultipleAnimations := (Sender as TCastleCheckbox).Checked;
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

procedure RefreshNamedAnimationsUi(const Window: TCastleWindow;
  const Scene: TCastleScene; const WindowMarginTop: Single);
var
  OldNamedAnimationsExists: Boolean;
begin
  OldNamedAnimationsExists := (NamedAnimationsUi <> nil) and NamedAnimationsUi.Exists;

  { free previous UI owned by NamedAnimationsUi, including previous animations }
  FreeAndNil(NamedAnimationsUi);

  NamedAnimationsUi := TNamedAnimationsUi.Create(Window, Scene);
  NamedAnimationsUi.Exists := OldNamedAnimationsExists;
  NamedAnimationsUi.Anchor(hpLeft, Margin);
  NamedAnimationsUi.Anchor(vpTop, - WindowMarginTop - Margin);
  Window.Controls.InsertFront(NamedAnimationsUi);
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
