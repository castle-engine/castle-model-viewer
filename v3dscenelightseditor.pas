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

  TLightMenu = class;
  THeadLightMenu = class;

  TString3 = array [0..2] of string;

  { Three float sliders to control TVector3Single value. }
  TMenuVector3Sliders = class(TComponent)
  strict private
    Floats: array [0..2] of TMenuFloatSlider;
    ItemsIndex: Integer;
  public
    constructor Create(const AOwner: TComponent;
      const Range: TBox3D; const AValue: TVector3Single);
    constructor Create(const AOwner: TComponent;
      const Min, Max: Single; const AValue: TVector3Single);
    procedure AddToMenu(const Items: TStringList;
      const TitleBase, Title0, Title1, Title2: string);
    function Selected(const CurrentItem: Integer): boolean;
    function Value: TVector3Single;
  end;

  TLightsMenu = class(TV3DOnScreenMenu)
  strict private
    AmbientColorSlider: TMenuVector3Sliders;
    { collected lights of the scene }
    Lights: TX3DNodeList;
    { seen headlight, if any }
    Headlight: TAbstractLightNode;
    LightMenu: TLightMenu;
    HeadLightMenu: THeadLightMenu;
    ItemsIndex: Integer;
    procedure AddLight(Node: TX3DNode);
    procedure DestructionNotification(Node: TX3DNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    destructor Destroy; override;
    procedure AccessoryValueChanged; override;
  end;

  TLightMenu = class(TV3DOnScreenMenu)
  strict private
    Light: TAbstractLightNode;
    BackIndex: Integer;
    ColorSlider: TMenuVector3Sliders;
    IntensitySlider: TMenuFloatSlider;
    AmbientIntensitySlider: TMenuFloatSlider;
    OnArgument: TMenuBooleanArgument;
    ShadowsArgument: TMenuBooleanArgument;
    ShadowVolumesArgument: TMenuBooleanArgument;
    ShadowVolumesMainArgument: TMenuBooleanArgument;
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractLightNode); reintroduce;
    procedure AfterCreate;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TPositionalLightMenu = class(TLightMenu)
  strict private
    Light: TAbstractPositionalLightNode;
    PositionSlider, AttenuationSlider: TMenuVector3Sliders;
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractPositionalLightNode); reintroduce;
    procedure AccessoryValueChanged; override;
  end;

  TSpot1LightMenu = class(TPositionalLightMenu)
  strict private
    Light: TSpotLightNode_1;
    ItemsIndex: Integer;
  public
    constructor Create(AOwner: TComponent; ALight: TSpotLightNode_1); reintroduce;
    procedure Click; override;
  end;

  TSpotLightMenu = class(TPositionalLightMenu)
  strict private
    Light: TSpotLightNode;
    ItemsIndex: Integer;
  public
    constructor Create(AOwner: TComponent; ALight: TSpotLightNode); reintroduce;
    procedure Click; override;
  end;

  TDirectionalLightMenu = class(TLightMenu)
  strict private
    Light: TAbstractDirectionalLightNode;
    ItemsIndex: Integer;
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractDirectionalLightNode); reintroduce;
    procedure Click; override;
  end;

  THeadLightMenu = class(TV3DOnScreenMenu)
  strict private
    Headlight: TAbstractLightNode;
    BackIndex: Integer;
    AmbientIntensitySlider: TMenuFloatSlider;
    ColorSlider: TMenuVector3Sliders;
    IntensitySlider: TMenuFloatSlider;
    AttenuationSlider: TMenuVector3Sliders;
  public
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
  LightsMenu := TLightsMenu.Create(nil);
  SetCurrentMenu(LightsMenu);
  MenuLightsEditor.Checked := true;
end;

procedure LightsEditorClose;
begin
  if SceneManager = nil then Exit;

  SetCurrentMenu(nil);
  FreeAndNil(LightsMenu);

  SceneManager := nil;
  Window := nil;
  MenuLightsEditor.Checked := false;
end;

function MessageInputQueryDirection(
  Window: TCastleWindowBase; const Title: string;
  var Value: TVector3Single; TextAlign: TTextAlign): boolean;
var
  Pos, Up: TVector3Single;
  s: string;
begin
  Result := false;
  s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
  if MessageInputQuery(Window, Title, s, TextAlign) then
  begin
    try
      if LowerCase(Trim(S)) = 'c' then
        SceneManager.Camera.GetView(Pos, Value, Up) else
        Value := Vector3SingleFromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 3 value : ' + E.Message, taLeft);
    end;
  end;
end;

const
  { Although X3D allows attenuation[0] (constant) to be < 1, we don't allow it.
    That's because when rendering with fixed-function, it's not honored
    correctly: X3D spec says to do "1 / max(c1 + c2 * dL + c3 * dL^2, 1)"
    (makes sense: never make light brighter by attenuation),
    but fixed-function OpenGL doesn't do it (our shader rendering does it Ok). }
  AttenuationRange: TBox3D = (Data: ((1, 0, 0), (2, 2, 2)));

{ TMenuVector3Sliders -------------------------------------------------------- }

constructor TMenuVector3Sliders.Create(const AOwner: TComponent;
  const Range: TBox3D; const AValue: TVector3Single);
var
  I: Integer;
begin
  inherited Create(AOwner);
  for I := 0 to 2 do
    Floats[I] := TMenuFloatSlider.Create(
      Range.Data[0, I], Range.Data[1, I], AValue[I]);
end;

constructor TMenuVector3Sliders.Create(const AOwner: TComponent;
  const Min, Max: Single; const AValue: TVector3Single);
begin
  Create(AOwner,
    Box3D(Vector3Single(Min, Min, Min), Vector3Single(Max, Max, Max)), AValue);
end;

procedure TMenuVector3Sliders.AddToMenu(const Items: TStringList;
  const TitleBase, Title0, Title1, Title2: string);
var
  I: Integer;
  Title: TString3;
  TitleBaseSpace: string;
begin
  ItemsIndex := Items.Count;
  Title[0] := Title0;
  Title[1] := Title1;
  Title[2] := Title2;
  if TitleBase <> '' then
    TitleBaseSpace := TitleBase + ' ' else
    TitleBaseSpace := '';
  for I := 0 to 2 do
    Items.AddObject(TitleBaseSpace + Title[I], Floats[I]);
end;

function TMenuVector3Sliders.Value: TVector3Single;
var
  I: Integer;
begin
  for I := 0 to 2 do
    Result[I] := Floats[I].Value;
end;

function TMenuVector3Sliders.Selected(const CurrentItem: Integer): boolean;
begin
  Result := (ItemsIndex <= CurrentItem) and (CurrentItem <= ItemsIndex + 2);
end;

{ TV3DOnScreenMenu ----------------------------------------------------------- }

constructor TV3DOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;
  BackgroundOpacityFocused := 0.3;
  BackgroundOpacityNotFocused := 0.2;
end;

{ TLightsMenu ------------------------------------------------------- }

var
  GlobalAmbientLight: TVector3Single = (0.2, 0.2, 0.2);

constructor TLightsMenu.Create(AOwner: TComponent);
var
  I: Integer;
  Light: TAbstractLightNode;
begin
  inherited;

  Lights := TX3DNodeList.Create(false);

  AmbientColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, GlobalAmbientLight);

  SceneManager.MainScene.RootNode.EnumerateNodes(TAbstractLightNode, @AddLight, false);

  for I := 0 to Lights.Count - 1 do
  begin
    Light := Lights[I] as TAbstractLightNode;
    Items.Add(Format('Edit %d: %s', [I, Light.NiceName]));
  end;
  ItemsIndex := Items.Count;
  AmbientColorSlider.AddToMenu(Items, 'Global Ambient Light', 'Red', 'Green', 'Blue');
  Items.Add('Edit Headlight');
  Items.Add('Close Lights Editor');
end;

destructor TLightsMenu.Destroy;
var
  I: Integer;
begin
  if Lights <> nil then
  begin
    for I := 0 to Lights.Count - 1 do
      Lights[I].DestructionNotifications.Remove(@DestructionNotification);
  end;
  if Headlight <> nil then
    Headlight.DestructionNotifications.Remove(@DestructionNotification);

  FreeAndNil(Lights);
  inherited;
end;

procedure TLightsMenu.DestructionNotification(Node: TX3DNode);
var
  I: Integer;
begin
  { Disconnect our destruction notifications from all other lights.
    Do not disconnect from Node (that is now freed), since implementation
    of this node probably iterates now over it's DestructionNotifications list. }

  if Lights <> nil then
  begin
    for I := 0 to Lights.Count - 1 do
      if Node <> Lights[I] then
        Lights[I].DestructionNotifications.Remove(@DestructionNotification);
    Lights.Clear;
  end;

  if (Headlight <> nil) and (Node <> Headlight) then
    Headlight.DestructionNotifications.Remove(@DestructionNotification);
  Headlight := nil;

  { At one point I tried here to return to LightsMenu,
    and do InitializeLightsItems again.
    But this isn't such good idea, because when we release the scene
    to load a new one, we currently have empty SceneManager.MainScene
    or SceneManager.MainScene.RootNode, and we don't see any lights yet.
    So calling InitializeLightsItems again always fills the list with no lights...
    And we don't get any notifications when new scene is loaded (no such
    mechanism in engine now), so we don't show new lights.
    It's easier to just close lights editor (this also just destroys
    our instance), and force user to open it again if wanted. }

  LightsEditorClose;
end;

procedure TLightsMenu.AddLight(Node: TX3DNode);
begin
  if Lights.IndexOf(Node) = -1 then
  begin
    Lights.Add(Node);
    Node.DestructionNotifications.Add(@DestructionNotification);
  end;
end;

procedure TLightsMenu.Click;
var
  H: TLightInstance;
  Node, NewHeadlight: TAbstractLightNode;
begin
  inherited;
  if CurrentItem < ItemsIndex then
  begin
    FreeAndNil(LightMenu);
    Node := Lights[CurrentItem] as TAbstractLightNode;
    if Node is TSpotLightNode_1 then
      LightMenu := TSpot1LightMenu.Create(Self, TSpotLightNode_1(Node)) else
    if Node is TSpotLightNode then
      LightMenu := TSpotLightMenu.Create(Self, TSpotLightNode(Node)) else
    if Node is TAbstractDirectionalLightNode then
      LightMenu := TDirectionalLightMenu.Create(Self, TAbstractDirectionalLightNode(Node)) else
    if Node is TAbstractPositionalLightNode then
      LightMenu := TPositionalLightMenu.Create(Self, TAbstractPositionalLightNode(Node)) else
      { fallback on TLightMenu, although currently we just capture all
        possible descendants with specialized menu types above }
      LightMenu := TLightMenu.Create(Self, Node);
    LightMenu.AfterCreate;
    SetCurrentMenu(LightMenu);
  end else
  if CurrentItem = ItemsIndex + 3 then
  begin
    if SceneManager.HeadlightInstance(H) then
    begin
      FreeAndNil(HeadLightMenu);
      NewHeadlight := H.Node;
      HeadLightMenu := THeadLightMenu.Create(Self, NewHeadlight);
      SetCurrentMenu(HeadLightMenu);

      if Headlight <> NewHeadlight then
      begin
        if Headlight <> nil then
          Headlight.DestructionNotifications.Remove(@DestructionNotification);
        Headlight := NewHeadlight;
        Headlight.DestructionNotifications.Add(@DestructionNotification);
      end;
    end else
      MessageOK(Window, 'No headlight in level.' +NL+ NL+
        'You have to turn on headlight first:' +NL+
        '- by menu item "View -> Headlight" (Ctrl+H),' +NL+
        '- or by editing the VRML/X3D model and setting NavigationInfo.headlight to TRUE.',
        taLeft);
  end else
  if CurrentItem = ItemsIndex + 4 then
    LightsEditorClose;
end;

procedure TLightsMenu.AccessoryValueChanged;
begin
  inherited;
  if AmbientColorSlider.Selected(CurrentItem) then
  begin
    GlobalAmbientLight := AmbientColorSlider.Value;

    { TODO: GlobalAmbientLight just modifies and directly sets OpenGL paramater now.
      Default is equal to OpenGL default.
      This may be changed to control NavigationInfo.globalAmbient field
      (InstantReality extension that we plan to implement too,
      see http://doc.instantreality.org/documentation/nodetype/NavigationInfo/ ). }
    glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(GlobalAmbientLight, 1.0));
  end;
end;

{ TLightMenu ---------------------------------------------------------- }

constructor TLightMenu.Create(AOwner: TComponent; ALight: TAbstractLightNode);
begin
  inherited Create(AOwner);

  Light := ALight;

  ColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, Light.FdColor.Value);
  IntensitySlider := TMenuFloatSlider.Create(0, 1, Light.FdIntensity.Value);
  AmbientIntensitySlider := TMenuFloatSlider.Create(
    -1, 1, Light.FdAmbientIntensity.Value);
  OnArgument := TMenuBooleanArgument.Create(Light.FdOn.Value);
  ShadowsArgument := TMenuBooleanArgument.Create(Light.FdShadows.Value);
  ShadowVolumesArgument := TMenuBooleanArgument.Create(Light.FdKambiShadows.Value);
  ShadowVolumesMainArgument := TMenuBooleanArgument.Create(
    Light.FdKambiShadowsMain.Value);

  ColorSlider.AddToMenu(Items, '', 'Red', 'Green', 'Blue');
  Items.AddObject('Intensity', IntensitySlider);
  Items.AddObject('Ambient Intensity', AmbientIntensitySlider);
  Items.AddObject('On', OnArgument);
  Items.AddObject('Shadows (Easy: By Shadow Maps)', ShadowsArgument);
  Items.AddObject('Shadow Volumes (Off In Shadows)', ShadowVolumesArgument);
  Items.AddObject('Shadow Volumes Main (Determines Shadows)', ShadowVolumesMainArgument);
end;

procedure TLightMenu.AfterCreate;
begin
  BackIndex := Items.Count;
  Items.Add('Back to Lights Menu');
end;

procedure TLightMenu.Click;
begin
  inherited;
  case CurrentItem of
    5: begin
         OnArgument.Value := not OnArgument.Value;
         Light.FdOn.Send(OnArgument.Value);
       end;
    6: begin
         if (Light is TPointLightNode_1) or
            (Light is TPointLightNode) then
         begin
           MessageOK(Window, 'Shadow maps on point lights are not supported yet. Please speak up on "Castle Game Engine" forum and encourage Michalis to implement them, if you want! :) In the meantime, shadow maps work perfectly on other lights (spot and directional).', taLeft);
           Exit;
         end;

         ShadowsArgument.Value := not ShadowsArgument.Value;
         Light.FdShadows.Send(ShadowsArgument.Value);
       end;
    7: begin
         ShadowVolumesArgument.Value := not ShadowVolumesArgument.Value;
         Light.FdKambiShadows.Send(ShadowVolumesArgument.Value);
       end;
    8: begin
         ShadowVolumesMainArgument.Value := not ShadowVolumesMainArgument.Value;
         Light.FdKambiShadowsMain.Send(ShadowVolumesMainArgument.Value);
       end;
    else
    if CurrentItem = BackIndex then
      SetCurrentMenu(LightsMenu);
  end;
end;

procedure TLightMenu.AccessoryValueChanged;
begin
  inherited;
  if ColorSlider.Selected(CurrentItem) then
    Light.FdColor.Send(ColorSlider.Value) else
  case CurrentItem of
    3: Light.FdIntensity.Send(IntensitySlider.Value);
    4: Light.FdAmbientIntensity.Send(AmbientIntensitySlider.Value);
  end;
end;

{ TPositionalLightMenu ------------------------------------------------------- }

constructor TPositionalLightMenu.Create(AOwner: TComponent; ALight: TAbstractPositionalLightNode);
const
  DefaultSize = 10;
var
  Box: TBox3D;
  BoxSizes: TVector3Single;
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  { determine sensible lights positions.
    Box doesn't depend on Light.FdLocation, to not change range each time
    --- but this causes troubles, as Light.FdLocation may not fit within range,
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
  PositionSlider := TMenuVector3Sliders.Create(Self, Box, Light.FdLocation.Value);

  AttenuationSlider := TMenuVector3Sliders.Create(Self,
    AttenuationRange, Light.FdAttenuation.Value);

  PositionSlider.AddToMenu(Items, 'Position', 'X', 'Y', 'Z');
  AttenuationSlider.AddToMenu(Items, 'Attenuation', 'Constant' , 'Linear', 'Quadratic');
end;

procedure TPositionalLightMenu.AccessoryValueChanged;
begin
  inherited;
  if PositionSlider.Selected(CurrentItem) then
    Light.FdLocation.Send(PositionSlider.Value) else
  if AttenuationSlider.Selected(CurrentItem) then
    Light.FdAttenuation.Send(AttenuationSlider.Value);
end;

{ TSpot1LightMenu ------------------------------------------------------- }

constructor TSpot1LightMenu.Create(AOwner: TComponent; ALight: TSpotLightNode_1);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  ItemsIndex := Items.Count;
  Items.Add('Direction ...');
  Items.Add('Spot Cut Off Angle ...');
  Items.Add('Spot Drop Off Rate ...');
end;

procedure TSpot1LightMenu.Click;
var
  Vector: TVector3Single;
  Value: Single;
begin
  inherited;
  if CurrentItem = ItemsIndex then
  begin
    Vector := Light.FdDirection.Value;
    if MessageInputQueryDirection(Window, 'Change direction' +nl+
      '(Input "C" to use current camera''s direction)',
      Vector, taLeft) then
      Light.FdDirection.Send(Vector);
  end else
  if CurrentItem = ItemsIndex + 1 then
  begin
    Value := Light.FdCutOffAngle.Value;
    if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
      Light.FdCutOffAngle.Send(Value);
  end else
  if CurrentItem = ItemsIndex + 2 then
  begin
    Value := Light.FdDropOffRate.Value;
    if MessageInputQuery(Window, 'Change dropOffRate', Value, taLeft) then
      Light.FdDropOffRate.Send(Value);
  end;
end;

{ TSpotLightMenu ------------------------------------------------------- }

constructor TSpotLightMenu.Create(AOwner: TComponent; ALight: TSpotLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  ItemsIndex := Items.Count;
  Items.Add('Direction ...');
  Items.Add('Spot Cut Off Angle ...');
  Items.Add('Spot Beam Width ...');
end;

procedure TSpotLightMenu.Click;
var
  Vector: TVector3Single;
  Value: Single;
begin
  inherited;
  if CurrentItem = ItemsIndex then
  begin
    Vector := Light.FdDirection.Value;
    if MessageInputQueryDirection(Window, 'Change direction' +nl+
      '(Input "C" to use current camera''s direction)',
      Vector, taLeft) then
      Light.FdDirection.Send(Vector);
  end else
  if CurrentItem = ItemsIndex + 1 then
  begin
    Value := Light.FdCutOffAngle.Value;
    if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
      Light.FdCutOffAngle.Send(Value);
  end else
  if CurrentItem = ItemsIndex + 2 then
  begin
    Value := Light.FdBeamWidth.Value;
    if MessageInputQuery(Window, 'Change beamWidth', Value, taLeft) then
      Light.FdBeamWidth.Send(Value);
  end;
end;

{ TDirectionalLightMenu ------------------------------------------------------- }

constructor TDirectionalLightMenu.Create(AOwner: TComponent; ALight: TAbstractDirectionalLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  ItemsIndex := Items.Count;
  Items.Add('Direction ...');
end;

procedure TDirectionalLightMenu.Click;
var
  Vector: TVector3Single;
begin
  inherited;
  if CurrentItem = ItemsIndex then
  begin
    Vector := Light.FdDirection.Value;
    if MessageInputQueryDirection(Window, 'Change direction' +nl+
      '(Input "C" to use current camera''s direction)',
      Vector, taLeft) then
      Light.FdDirection.Send(Vector);
  end;
end;

{ THeadLightMenu --------------------------------------------------------- }

constructor THeadLightMenu.Create(AOwner: TComponent; AHeadlight: TAbstractLightNode);
begin
  inherited Create(AOwner);
  Headlight := AHeadlight;

  AmbientIntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdAmbientIntensity.Value);
  Items.AddObject('Ambient Intensity', AmbientIntensitySlider);

  ColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, Headlight.FdColor.Value);
  ColorSlider.AddToMenu(Items, '', 'Red', 'Green', 'Blue');

  IntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdIntensity.Value);
  Items.AddObject('Intensity', IntensitySlider);

  if Headlight is TAbstractPositionalLightNode then
  begin
    { This was a nice message about attenuation for headlight,
      but we have nowhere to place it now:
      MessageOk(Window, 'HeadLight is not positional, it is not possible to set attenuation.' +NL+ NL+
        'To use this function, you need to use our KambiNavigationInfo.headlightNode extension inside your VRML/X3D scene source, to indicate that you want headlight to be a PointLight or SpotLight. See the documentation of VRML/X3D extensions in "Castle Game Engine" for examples and details.',
        taLeft); }

    AttenuationSlider := TMenuVector3Sliders.Create(Self, AttenuationRange,
      TAbstractPositionalLightNode(Headlight).FdAttenuation.Value);
    AttenuationSlider.AddToMenu(Items, 'Attenuation', 'Constant' , 'Linear', 'Quadratic');
  end;

  BackIndex := Items.Count;
  Items.Add('Back to Lights Menu');
end;

procedure THeadLightMenu.Click;
begin
  inherited;
  if CurrentItem = BackIndex then
    SetCurrentMenu(LightsMenu);
end;

procedure THeadLightMenu.AccessoryValueChanged;
begin
  inherited;
  if ColorSlider.Selected(CurrentItem) then
    Headlight.FdColor.Send(ColorSlider.Value) else
  if (AttenuationSlider <> nil) and
      AttenuationSlider.Selected(CurrentItem) then
    (Headlight as TAbstractPositionalLightNode).FdAttenuation.Send(AttenuationSlider.Value) else
  case CurrentItem of
    0: Headlight.FdAmbientIntensity.Value := AmbientIntensitySlider.Value;
    4: Headlight.FdIntensity.Value := IntensitySlider.Value;
  end;
end;

finalization
  { free if it exists at the end }
  FreeAndNil(LightsMenu);
end.
