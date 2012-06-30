{
  Copyright 2006-2012 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ Lights editor in view3dscene. }
unit V3DSceneLightsEditor;

interface

uses CastleWindow, CastleSceneManager;

var
  MenuLightsEditor: TMenuItemChecked;

function LightsEditorIsOpen: boolean;

procedure LightsEditorOpen(ASceneManager: TCastleSceneManager;
  AWindow: TCastleWindowCustom);
procedure LightsEditorClose;

implementation

uses SysUtils, VectorMath, Classes, X3DNodes, OnScreenMenu, Boxes3D,
  CastleMessages, CastleUtils, GL, CastleGLUtils;

{ TCastleOnScreenMenu descendants -------------------------------------------- }

type
  TV3DOnScreenMenu = class(TCastleOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOneLightMenu = class;
  THeadLightMenu = class;

  TLightsMenu = class(TV3DOnScreenMenu)
  public
    AmbientColorSlider: array[0..2] of TMenuFloatSlider;
    OneLightMenu: TOneLightMenu;
    HeadLightMenu: THeadLightMenu;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TOneLightMenu = class(TV3DOnScreenMenu)
  public
    Light: TAbstractLightNode;
    RedColorSlider: TMenuFloatSlider;
    GreenColorSlider: TMenuFloatSlider;
    BlueColorSlider: TMenuFloatSlider;
    IntensitySlider: TMenuFloatSlider;
    AmbientIntensitySlider: TMenuFloatSlider;
    OnArgument: TMenuBooleanArgument;
    ShadowsArgument: TMenuBooleanArgument;
    ShadowsMainArgument: TMenuBooleanArgument;
    PositionSlider: array [0..2] of TMenuFloatSlider;
    constructor Create(AOwner: TComponent; ALight: TAbstractLightNode); reintroduce;
    procedure Click; override;
    procedure AccessoryValueChanged; override;

    function GetLightLocation: TVector3Single;
    procedure SetLightLocation(const Value: TVector3Single);
    property LightLocation: TVector3Single
      read GetLightLocation write SetLightLocation;
  end;

  THeadLightMenu = class(TV3DOnScreenMenu)
  public
    Headlight: TAbstractLightNode;
    AmbientIntensitySlider: TMenuFloatSlider;
    ColorSlider: array[0..2] of TMenuFloatSlider;
    IntensitySlider: TMenuFloatSlider;
    SpotArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent; AHeadlight: TAbstractLightNode); reintroduce;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

{ global utils --------------------------------------------------------------- }

var
  { Local copy of scene manager and window for lights editor.
    Both @nil when we're closed, never @nil when we're open. }
  SceneManager: TCastleSceneManager;
  Window: TCastleWindowCustom;
  LightsMenu: TLightsMenu;

procedure SetCurrentMenu(const NewValue: TCastleOnScreenMenu);
begin
  Window.Controls.MakeSingle(TCastleOnScreenMenu, NewValue, true);
end;

function LightsEditorIsOpen: boolean;
begin
  Result := SceneManager <> nil;
end;

procedure LightsEditorOpen(ASceneManager: TCastleSceneManager;
  AWindow: TCastleWindowCustom);
begin
  if SceneManager = ASceneManager then Exit;
  SceneManager := ASceneManager;
  Window := AWindow;

  FreeAndNil(LightsMenu);
  LightsMenu := TLightsMenu.Create(Application);
  SetCurrentMenu(LightsMenu);
  MenuLightsEditor.Checked := true;
end;

procedure LightsEditorClose;
begin
  if SceneManager = nil then Exit;

  SetCurrentMenu(nil);

  SceneManager := nil;
  Window := nil;
  MenuLightsEditor.Checked := false;
end;

{ TV3DOnScreenMenu ----------------------------------------------------------- }

constructor TV3DOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;
  BackgroundOpacityFocused := 0.3;
  BackgroundOpacityNotFocused := 0.2;
end;

{ TLightsMenu ------------------------------------------------------- }

{ TODO: GlobalAmbientLight is just a hack now, it is modified and directly set
  for OpenGL now.
  Default is equal to OpenGL default.
  Control of global ambient may be changed
  to control NavigationInfo.globalAmbient field
  (InstantReality extension that we may implement too,
  see http://doc.instantreality.org/documentation/nodetype/NavigationInfo/ ). }

var
  GlobalAmbientLight: TVector3Single = (0.2, 0.2, 0.2);

constructor TLightsMenu.Create(AOwner: TComponent);
var
  I: Integer;
  LightNode: TAbstractLightNode;
begin
  inherited;

  AmbientColorSlider[0] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[0]);
  AmbientColorSlider[1] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[1]);
  AmbientColorSlider[2] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[2]);

  for I := 0 to SceneManager.MainScene.GlobalLights.Count - 1 do
  begin
    LightNode := SceneManager.MainScene.GlobalLights.Items[I].Node;
    Items.Add(Format('Edit %d: %s "%s"',
      [I, LightNode.NodeTypeName, LightNode.NodeName]));
  end;
  Items.AddObject('Global Ambient Light Red'  , AmbientColorSlider[0]);
  Items.AddObject('Global Ambient Light Green', AmbientColorSlider[1]);
  Items.AddObject('Global Ambient Light Blue' , AmbientColorSlider[2]);
  Items.Add('Edit Headlight');
  Items.Add('Close Lights Editor');
end;

procedure TLightsMenu.Click;
var
  H: TLightInstance;
begin
  case CurrentItem - SceneManager.MainScene.GlobalLights.Count of
    0, 1, 2: ;
    3: begin
         if SceneManager.HeadlightInstance(H) then
         begin
           FreeAndNil(HeadLightMenu);
           HeadLightMenu := THeadLightMenu.Create(Self, H.Node);
           SetCurrentMenu(HeadLightMenu);
         end else
           MessageOK(Window, 'No headlight in level ' +
             ' (set NavigationInfo.headlight to TRUE to get headlight)', taLeft);
       end;
    4: LightsEditorClose;
    else
       begin
         FreeAndNil(OneLightMenu);
         OneLightMenu := TOneLightMenu.Create(Self,
           SceneManager.MainScene.GlobalLights.Items[CurrentItem].Node);
         SetCurrentMenu(OneLightMenu);
       end;
  end;
end;

procedure TLightsMenu.AccessoryValueChanged;
begin
  case CurrentItem - SceneManager.MainScene.GlobalLights.Count of
    2: GlobalAmbientLight[0] := AmbientColorSlider[0].Value;
    3: GlobalAmbientLight[1] := AmbientColorSlider[1].Value;
    4: GlobalAmbientLight[2] := AmbientColorSlider[2].Value;
    else Exit;
  end;

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(GlobalAmbientLight, 1.0));
end;

{ TOneLightMenu ---------------------------------------------------------- }

constructor TOneLightMenu.Create(AOwner: TComponent; ALight: TAbstractLightNode);
const
  DefaultSize = 10;
var
  I: Integer;
  Box: TBox3D;
  BoxSizes: TVector3Single;
begin
  inherited Create(AOwner);

  Light := ALight;

  { determine sensible lights positions.
    Box doesn't depend on LightLocation, to not change range each time
    --- but this causes troubles, as LightLocation may not fit within range,
    which is uncomfortable (works Ok, but not nice for user). }
  Box := SceneManager.Items.BoundingBox + SceneManager.CameraBox;
  if Box.IsEmpty then
    Box := Box3D(Vector3Single(-DefaultSize, -DefaultSize, -DefaultSize),
                 Vector3Single( DefaultSize,  DefaultSize,  DefaultSize)) else
  begin
    BoxSizes := Box.Sizes;
    Box.Data[0] := Box.Data[0] - BoxSizes;
    Box.Data[1] := Box.Data[1] + BoxSizes;
  end;
  for I := 0 to 2 do
    PositionSlider[I] := TMenuFloatSlider.Create(
      Box.Data[0, I], Box.Data[1, I], LightLocation[I]);

  RedColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[0]);
  GreenColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[1]);
  BlueColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[2]);
  IntensitySlider := TMenuFloatSlider.Create(0, 1, Light.FdIntensity.Value);
  AmbientIntensitySlider := TMenuFloatSlider.Create(
    -1, 1, Light.FdAmbientIntensity.Value);
  OnArgument := TMenuBooleanArgument.Create(Light.FdOn.Value);
  ShadowsArgument := TMenuBooleanArgument.Create(Light.FdKambiShadows.Value);
  ShadowsMainArgument := TMenuBooleanArgument.Create(
    Light.FdKambiShadowsMain.Value);

  Items.AddObject('Position X', PositionSlider[0]);
  Items.AddObject('Position Y', PositionSlider[1]);
  Items.AddObject('Position Z', PositionSlider[2]);
  Items.AddObject('Red', RedColorSlider);
  Items.AddObject('Green', GreenColorSlider);
  Items.AddObject('Blue', BlueColorSlider);
  Items.AddObject('Intensity', IntensitySlider);
  Items.AddObject('Ambient Intensity', AmbientIntensitySlider);
  Items.AddObject('On', OnArgument);
  Items.AddObject('Shadow Volumes (Off In Shadows)', ShadowsArgument);
  Items.AddObject('Shadow Volumes Main (Determines Shadows)', ShadowsMainArgument);
  Items.Add('Point/SpotLight: Attenuation ...');
  Items.Add('DirectionalLight: Direction ...');
  Items.Add('SpotLight: Direction ...');
  Items.Add('SpotLight: Spot Beam Width / Drop Off Rate');
  Items.Add('SpotLight: Spot Cut Off Angle');
  Items.Add('Back to Lights Menu');
end;

function TOneLightMenu.GetLightLocation: TVector3Single;
begin
  if Light is TAbstractPositionalLightNode then
    Result := TAbstractPositionalLightNode(Light).FdLocation.Value else
    Result := ZeroVector3Single;
end;

procedure TOneLightMenu.SetLightLocation(const Value: TVector3Single);
begin
  if Light is TAbstractPositionalLightNode then
    TAbstractPositionalLightNode(Light).FdLocation.Value := Value;
end;

procedure TOneLightMenu.Click;

  function MessageInputQueryVector3SingleC(
    Window: TCastleWindowBase; const Title: string;
    var Value: TVector3Single; TextAlign: TTextAlign;
    const OnC: TVector3Single): boolean;
  var
    s: string;
  begin
    Result := false;
    s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
    if MessageInputQuery(Window, Title, s, TextAlign) then
    begin
      try
        if LowerCase(Trim(S)) = 'c' then
          Value := OnC else
          Value := Vector3SingleFromStr(s);
        Result := true;
      except
        on E: EConvertError do
          MessageOK(Window, 'Invalid vector 3 value : ' + E.Message, taLeft);
      end;
    end;
  end;

  function CameraDirection: TVector3Single;
  var
    Pos, Up: TVector3Single;
  begin
    SceneManager.Camera.GetView(Pos, Result, Up);
  end;

var
  Vector: TVector3Single;
  Value: Single;
begin
  case CurrentItem of
    0..7: ;
    8: begin
         OnArgument.Value := not OnArgument.Value;
         Light.FdOn.Send(OnArgument.Value);
       end;
    9: begin
         ShadowsArgument.Value := not ShadowsArgument.Value;
         Light.FdKambiShadows.Send(ShadowsArgument.Value);
       end;
    10:begin
         ShadowsMainArgument.Value := not ShadowsMainArgument.Value;
         Light.FdKambiShadowsMain.Send(ShadowsMainArgument.Value);
       end;
    11:begin
         if Light is TAbstractPositionalLightNode then
         begin
           Vector := TAbstractPositionalLightNode(Light).FdAttenuation.Value;
           if MessageInputQueryVector3Single(Window, 'Change attenuation',
             Vector, taLeft) then
             TAbstractPositionalLightNode(Light).FdAttenuation.Send(Vector);
         end;
       end;
    12:begin
         if Light is TAbstractDirectionalLightNode then
         begin
           Vector := TAbstractDirectionalLightNode(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleC(Window, 'Change direction' +nl+
             '(Input "c" to use current camera''s direction)',
             Vector, taLeft, CameraDirection) then
             TAbstractDirectionalLightNode(Light).FdDirection.Send(Vector);
         end;
       end;
    13:begin
         if Light is TSpotLightNode_1 then
         begin
           Vector := TSpotLightNode_1(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleC(Window, 'Change direction' +nl+
             '(Input "P" to use current camera''s direction)',
             Vector, taLeft, CameraDirection) then
             TSpotLightNode_1(Light).FdDirection.Send(Vector);
         end else
         if Light is TSpotLightNode then
         begin
           Vector := TSpotLightNode(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleC(Window, 'Change direction' +nl+
             '(Input "P" to use current camera''s direction)',
             Vector, taLeft, CameraDirection) then
             TSpotLightNode(Light).FdDirection.Send(Vector);
         end;
       end;
    14:begin
         if Light is TSpotLightNode_1 then
         begin
           Value := TSpotLightNode_1(Light).FdDropOffRate.Value;
           if MessageInputQuery(Window, 'Change dropOffRate', Value, taLeft) then
             TSpotLightNode_1(Light).FdDropOffRate.Send(Value);
         end else
         if Light is TSpotLightNode then
         begin
           Value := TSpotLightNode(Light).FdBeamWidth.Value;
           if MessageInputQuery(Window, 'Change beamWidth', Value, taLeft) then
             TSpotLightNode(Light).FdBeamWidth.Send(Value);
         end;
       end;
    15:begin
         if Light is TSpotLightNode_1 then
         begin
           Value := TSpotLightNode_1(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TSpotLightNode_1(Light).FdCutOffAngle.Send(Value);
         end else
         if Light is TSpotLightNode then
         begin
           Value := TSpotLightNode(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TSpotLightNode(Light).FdCutOffAngle.Send(Value);
         end;
       end;
    16:SetCurrentMenu(LightsMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TOneLightMenu.AccessoryValueChanged;
var
  Index: Integer;
  V: TVector3Single;
begin
  case CurrentItem of
    0..2:
      if Light is TAbstractPositionalLightNode then
      begin
        Index := CurrentItem;
        V := TAbstractPositionalLightNode(Light).FdLocation.Value;
        V[Index] := PositionSlider[Index].Value;
        TAbstractPositionalLightNode(Light).FdLocation.Send(V);
      end;
    3: begin Light.FdColor.Value[0] := RedColorSlider.Value  ; Light.FdColor.Changed; end;
    4: begin Light.FdColor.Value[1] := GreenColorSlider.Value; Light.FdColor.Changed; end;
    5: begin Light.FdColor.Value[2] := BlueColorSlider.Value ; Light.FdColor.Changed; end;
    6: Light.FdIntensity.Send(IntensitySlider.Value);
    7: Light.FdAmbientIntensity.Send(AmbientIntensitySlider.Value);
    else Exit;
  end;
end;

{ THeadLightMenu --------------------------------------------------------- }

constructor THeadLightMenu.Create(AOwner: TComponent; AHeadlight: TAbstractLightNode);
begin
  inherited Create(AOwner);

  Headlight := AHeadlight;

  AmbientIntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdAmbientIntensity.Value);

  ColorSlider[0] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[0]);
  ColorSlider[1] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[1]);
  ColorSlider[2] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[2]);

  IntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdIntensity.Value);

  Items.AddObject('Ambient Intensity'  , AmbientIntensitySlider);

  Items.AddObject('Red'  , ColorSlider[0]);
  Items.AddObject('Green', ColorSlider[1]);
  Items.AddObject('Blue' , ColorSlider[2]);

  Items.AddObject('Intensity'  , IntensitySlider);

  Items.Add('Attenuation ...');

  Items.Add('Back to Lights Menu');
end;

procedure THeadLightMenu.Click;

  procedure ChangeAttenuation;
  var
    Vector3: TVector3Single;
  begin
    if Headlight is TAbstractPositionalLightNode then
    begin
      Vector3 := TAbstractPositionalLightNode(Headlight).FdAttenuation.Value;
      if MessageInputQueryVector3Single(Window, 'Change headlight Attenuation',
        Vector3, taLeft) then
        TAbstractPositionalLightNode(Headlight).FdAttenuation.Value := Vector3;
    end else
      MessageOk(Window, 'Light is not positional, no attenuation');
  end;

begin
  case CurrentItem of
    0..4: Exit;
    5: ChangeAttenuation;
    6: SetCurrentMenu(LightsMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure THeadLightMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    0:    Headlight.FdAmbientIntensity.Value := AmbientIntensitySlider.Value;
    1..3: Headlight.FdColor.Value[CurrentItem-1] := ColorSlider[CurrentItem-1].Value;
    4:    Headlight.FdIntensity.Value := IntensitySlider.Value;
    else Exit;
  end;
end;

end.
