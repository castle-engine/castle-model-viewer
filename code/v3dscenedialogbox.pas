{
  Copyright 2019-2019 Michalis Kamburelis.

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

{ Dialog box using CastleWindow and UI design. }
unit V3DSceneDialogBox;

interface

procedure WindowMessageOK(const S: String);

implementation

uses SysUtils, Classes,
  CastleWindow, CastleUIControls, CastleControls, CastleComponentSerialize,
  CastleKeysMouse, CastleUIState, CastleStringUtils;

type
  TStateDialogBox = class(TUIState)
  strict private
    FButtonClicked: Boolean;
    LabelMessage: TCastleLabel;
    ButtonOK: TCastleButton;
    procedure ClickOK(Sender: TObject);
  protected
    function StateContainer: TUIContainer; override;
  public
    { Assign these fields before starting the state. }
    WantedStateContainer: TUIContainer;
    Message: String;

    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    property ButtonClicked: Boolean read FButtonClicked;
  end;

procedure TStateDialogBox.ClickOK(Sender: TObject);
begin
  FButtonClicked := true;
end;

function TStateDialogBox.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyEnter) or Event.IsKey(keyEscape) or Event.IsMouseButton(mbLeft) then
  begin
    FButtonClicked := true;
    Exit(ExclusiveEvents);
  end;

  if Event.IsKey(CtrlC) then
  begin
    Clipboard.AsText := LabelMessage.Caption;
    Exit(ExclusiveEvents);
  end;
end;

procedure TStateDialogBox.Start;
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited;

  FButtonClicked := false;

  UiOwner := TComponent.Create(FreeAtStop);
  Ui := StringToComponent({$I ../embedded_data/designs/help_message.castle-user-interface.inc}, UiOwner)
    as TCastleUserInterface;

  LabelMessage := UiOwner.FindRequiredComponent('LabelMessage') as TCastleLabel;
  LabelMessage.Caption := Message;
  ButtonOK := UiOwner.FindRequiredComponent('ButtonOK') as TCastleButton;
  ButtonOK.OnClick := @ClickOK;

  InsertFront(Ui);
end;

function TStateDialogBox.StateContainer: TUIContainer;
begin
  Result := WantedStateContainer;
end;

procedure WindowMessageOK(const S: String);
var
  Window: TCastleWindowBase;
  StateDialogBox: TStateDialogBox;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Open;

    { add TStateDialogBox instance to window }
    StateDialogBox := TStateDialogBox.Create(Window);
    StateDialogBox.WantedStateContainer := Window.Container;
    StateDialogBox.Message := S;
    TUIState.Current := StateDialogBox;

    while (not Window.Closed) and
          (not StateDialogBox.ButtonClicked) do
      Application.ProcessAllMessages;

    { call StateDialogBox when Window is still assigned,
      otherwise TStateDialogBox.StateContainer would return wrong value }
    TUIState.Current := nil;
  finally FreeAndNil(Window) end;
end;

end.
