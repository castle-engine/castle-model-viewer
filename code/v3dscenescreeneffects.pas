{
  Copyright 2010-2018 Michalis Kamburelis.

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

{ Screen effects (for TKamAbstractViewport.ScreenEffects) available
  in view3dscene menu. }
unit V3DSceneScreenEffects;

interface

uses Classes, CastleUtils, CastleUIControls, CastleWindow, CastleGLShaders,
  CastleViewport, CastleScreenEffects, X3DNodes;

type
  { Screen effects predefined in view3dscene.
    The order below matters: that's the order in which they will be applied.
    Some findings:
    - seNegative looks best after at least sePower*
    - seEdgeDetect looks best before sePower* and seFlashlight
    - seVisualizeDepth overrides color, so it's sensible to place it
      at the beginning (otherwise it just cancels any other effect)
  }
  TScreenEffect = (
    seVisualizeDepth,
    seGrayscale,
    seEdgeDetect,
    sePowerBrighten,
    sePowerBrightenMore,
    sePowerDarken,
    sePowerDarkenMore,
    seFlashlight,
    seNegative
  );

  TScreenEffectsControlIndex = 0..3;

  TScreenEffects = class(TCastleUserInterface)
  private
    MenuItems: array [TScreenEffect] of TMenuItemChecked;
    Nodes: array [TScreenEffect, TScreenEffectsControlIndex] of TX3DRootNode;
  public
    Menu: TMenu;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActiveEffectsApply(
      const ScreenEffectsControl: TCastleScreenEffects;
      const ScreenEffectsControlIndex: TScreenEffectsControlIndex);
  end;

var
  ScreenEffects: TScreenEffects;

function LoadX3DClassicFromString(const FileContents: string;
  const BaseUrl: string): TX3DRootNode;

implementation

uses SysUtils,
  CastleGLUtils, CastleLog, CastleRenderOptions, X3DLoad, X3DFields;

function LoadX3DClassicFromString(const FileContents: string;
  const BaseUrl: string): TX3DRootNode;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(FileContents);
  try
    Result := LoadNode(Stream, BaseUrl, 'model/x3d+vrml');
  finally FreeAndNil(Stream) end;
end;

const
  ScreenEffectsInfo: array [TScreenEffect] of record
    Name: String;
    Node: String;
    Exponent: Single; //< Value of 'exponent' uniform, or 0 if this shader doesn't take such uniform.
  end = (
    (Name: 'Visualize Depth';
     Node: {$I ../embedded_data/screen_effects/visualize_depth.x3dv.inc};
     Exponent: 0),
    (Name: 'Grayscale';
     Node: {$I ../embedded_data/screen_effects/grayscale.x3dv.inc};
     Exponent: 0),
    (Name: 'Edge Detect';
     Node: {$I ../embedded_data/screen_effects/edge_detect.x3dv.inc};
     Exponent: 0),
    (Name: 'Power 1 / 2.2 (Brighten)';
     Node: {$I ../embedded_data/screen_effects/power.x3dv.inc};
     Exponent: 1 / 2.2),
    (Name: 'Power 1 / 4.0 (Brighten More)';
     Node: {$I ../embedded_data/screen_effects/power.x3dv.inc};
     Exponent: 1 / 4.0),
    (Name: 'Power 1.5 (Darken)';
     Node: {$I ../embedded_data/screen_effects/power.x3dv.inc};
     Exponent: 1.5),
    (Name: 'Power 2.2 (Darken More)';
     Node: {$I ../embedded_data/screen_effects/power.x3dv.inc};
     Exponent: 2.2),
    (Name: 'Flashlight (Nice Headlight)';
     Node: {$I ../embedded_data/screen_effects/flashlight.x3dv.inc};
     Exponent: 0),
    (Name: 'Negative';
     Node: {$I ../embedded_data/screen_effects/negative.x3dv.inc};
     Exponent: 0)
  );

constructor TScreenEffects.Create(AOwner: TComponent);
var
  SE: TScreenEffect;
  ExponentField: TSFFloat;
  I: TScreenEffectsControlIndex;
begin
  inherited;
  Menu := TMenu.Create('Screen Effects');

  { add built-in screen effects }
  Menu.Append(TMenuItemChecked.Create('Screen Space Ambient Occlusion',
    340, TCastleViewport.DefaultScreenSpaceAmbientOcclusion, true));
  Menu.Append(TMenuItemChecked.Create('Screen Space Reflections',
    342, TCastleViewport.DefaultScreenSpaceReflections, true));
  Menu.Append(TMenuSeparator.Create);

  { add custom screen effects handled in this unit }
  for SE := Low(SE) to High(SE) do
  begin
    MenuItems[SE] := TMenuItemChecked.Create(
      SQuoteMenuEntryCaption(ScreenEffectsInfo[SE].Name), 350, false, true);
    Menu.Append(MenuItems[SE]);

    { We create the same node 4 times, for each possible view3dscene viewport,
      as one X3D node cannot be added to multiple TCastleScene instances. }
    for I := Low(TScreenEffectsControlIndex) to High(TScreenEffectsControlIndex) do
    begin
      Nodes[SE, I] := LoadX3DClassicFromString(ScreenEffectsInfo[SE].Node, '');
      Nodes[SE, I].KeepExistingBegin;
      if ScreenEffectsInfo[SE].Exponent <> 0 then
      begin
        ExponentField := Nodes[SE, I].FindNode('MyShader').Field('exponent', true) as TSFFloat;
        ExponentField.Send(ScreenEffectsInfo[SE].Exponent);
      end;
    end;
  end;
end;

destructor TScreenEffects.Destroy;
var
  SE: TScreenEffect;
  I: TScreenEffectsControlIndex;
begin
  for SE := Low(SE) to High(SE) do
    for I := Low(TScreenEffectsControlIndex) to High(TScreenEffectsControlIndex) do
    begin
      Nodes[SE, I].KeepExistingEnd;
      FreeIfUnusedAndNil(Nodes[SE, I]);
    end;
  inherited;
end;

procedure TScreenEffects.ActiveEffectsApply(
  const ScreenEffectsControl: TCastleScreenEffects;
  const ScreenEffectsControlIndex: TScreenEffectsControlIndex);
var
  SE: TScreenEffect;
begin
  // first remove all effects, to later add them in TScreenEffect order
  for SE := Low(SE) to High(SE) do
    ScreenEffectsControl.RemoveScreenEffect(Nodes[SE, ScreenEffectsControlIndex]);

  // add active effects
  for SE := Low(SE) to High(SE) do
    if MenuItems[SE].Checked then
      ScreenEffectsControl.AddScreenEffect(Nodes[SE, ScreenEffectsControlIndex]);

  { Note that we only add active effects, instead of adding all, and using effect
    "Enabled" field. While it is a bit slower (shader must be recompiled),
    but this way we avoid GLSL compilation warnings on broken systems
    if user doesn't even activate shader effect. }
end;

initialization
  ScreenEffects := TScreenEffects.Create(nil);
finalization
  FreeAndNil(ScreenEffects);
end.
