{
  Copyright 2019-2026 Michalis Kamburelis.

  This file is part of "castle-model-viewer".

  "castle-model-viewer" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle-model-viewer" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle-model-viewer"; if not, write to the Free Software
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
  CastleKeysMouse, CastleUIState, CastleStringUtils,
  { Necessary for Delphi to define LineEnding. }
  CastleUtils;

type
  TViewDialogBox = class(TCastleView)
  strict private
    FButtonClicked: Boolean;
    LabelMessage: TCastleLabel;
    ButtonOK, ButtonCopyClipboard: TCastleButton;
    procedure ClickOK(Sender: TObject);
    procedure ClickCopyClipboard(Sender: TObject);
  public
    { Assign these fields before starting the state. }
    Message: String;

    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    property ButtonClicked: Boolean read FButtonClicked;
  end;

procedure TViewDialogBox.ClickOK(Sender: TObject);
begin
  FButtonClicked := true;
end;

procedure TViewDialogBox.ClickCopyClipboard(Sender: TObject);
begin
  Clipboard.AsText := LabelMessage.Caption;
end;

function TViewDialogBox.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyEnter) or Event.IsKey(keyEscape) then
  begin
    ButtonOK.DoClick;
    Exit(true);
  end;

  if Event.IsKey(CtrlC) then
  begin
    ButtonCopyClipboard.DoClick;
    Exit(true);
  end;
end;

procedure TViewDialogBox.Start;
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
  ButtonOK.OnClick := {$ifdef FPC}@{$endif} ClickOK;
  ButtonCopyClipboard := UiOwner.FindRequiredComponent('ButtonCopyClipboard') as TCastleButton;
  ButtonCopyClipboard.OnClick := {$ifdef FPC}@{$endif} ClickCopyClipboard;

  InsertFront(Ui);
end;

procedure WindowMessageOK(const S: String);
var
  Window: TCastleWindow;
  StateDialogBox: TViewDialogBox;
begin
  Window := TCastleWindow.Create(nil);
  try
    Window.Open;

    { add TViewDialogBox instance to window }
    StateDialogBox := TViewDialogBox.Create(Window);
    StateDialogBox.Message := S;
    Window.Container.View := StateDialogBox;

    while (not Window.Closed) and
          (not StateDialogBox.ButtonClicked) do
      Application.ProcessAllMessages;

    { Freeing the window will also free Container, which will stop
      the view TViewDialogBox. So we don't really need to do anything here. }
  finally FreeAndNil(Window) end;
end;

end.
