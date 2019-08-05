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
  CastleKeysMouse;

var
  ButtonClicked: Boolean;

type
  TEventHandler = class
    procedure ClickOK(Sender: TObject);
    procedure PressAnywhere(const Sender: TInputListener;
      const Event: TInputPressRelease; var Handled: Boolean);
  end;

procedure TEventHandler.ClickOK(Sender: TObject);
begin
  ButtonClicked := true;
end;

procedure TEventHandler.PressAnywhere(const Sender: TInputListener;
  const Event: TInputPressRelease; var Handled: Boolean);
begin
  if Event.IsKey(keyEnter) or Event.IsKey(keyEscape) or Event.IsMouseButton(mbLeft) then
  begin
    ButtonClicked := true;
    Handled := true;
  end;
end;

procedure WindowMessageOK(const S: String);
var
  Window: TCastleWindowBase;
  Ui: TCastleUserInterface;
  LabelMessage: TCastleLabel;
  ButtonOK: TCastleButton;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Open;

    Ui := StringToComponent({$I ../embedded_data/designs/help_message.castle-user-interface.inc}, Window)
      as TCastleUserInterface;
    Ui.OnPress := @TEventHandler(nil).PressAnywhere;
    LabelMessage := Window.FindRequiredComponent('LabelMessage') as TCastleLabel;
    LabelMessage.Caption := S;
    ButtonOK := Window.FindRequiredComponent('ButtonOK') as TCastleButton;
    ButtonOK.OnClick := @TEventHandler(nil).ClickOK;
    Window.Controls.InsertFront(Ui);

    ButtonClicked := false;
    while (not Window.Closed) and (not ButtonClicked) do
      Application.ProcessAllMessages;
  finally FreeAndNil(Window) end;
end;

end.
