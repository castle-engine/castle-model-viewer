{
  Copyright 2006-2023 Michalis Kamburelis.

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

{ Lights editor in view3dscene. }
unit V3DSceneLightsEditor;

{$I v3dsceneconf.inc}

interface

uses Classes, CastleWindow, CastleViewport, CastleScene;

var
  MenuLightsEditor: TMenuItemChecked;

function LightsEditorIsOpen: boolean;

procedure LightsEditorOpen(const AMainViewport: TCastleViewport;
  const AWindow: TCastleWindow; const AWindowMarginTop: Single);
procedure LightsEditorClose;

implementation

uses SysUtils, CastleColors,
  CastleVectors, X3DNodes, CastleOnScreenMenu, CastleBoxes, CastleTransform,
  CastleMessages, CastleUtils, CastleGLUtils, CastleUIControls,
  CastleRectangles, CastleControls, CastleRenderContext,
  V3DSceneImages, V3DSceneInternalScenes;

{ TCastleOnScreenMenu descendants -------------------------------------------- }

type
  TV3DOnScreenMenu = class(TCastleOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddTitle(const S: string);
  end;

  TLightMenu = class;
  THeadLightMenu = class;

  TString3 = array [0..2] of string;

  TMyFloatSlider = class(TCastleFloatSlider)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Three float sliders to control TVector3 value. }
  TMenuVector3Sliders = class(TComponent)
  strict private
    Floats: array [0..2] of TMyFloatSlider;
    FOnChange: TNotifyEvent;
    procedure ChildSliderChanged(Sender: TObject);
    function GetValue: TVector3;
    procedure SetValue(const AValue: TVector3);
  public
    constructor Create(const AOwner: TComponent;
      const Range: TBox3D; const AValue: TVector3); reintroduce; overload;
    constructor Create(const AOwner: TComponent;
      const Min, Max: Single; const AValue: TVector3); reintroduce; overload;
    procedure AddToMenu(const Menu: TCastleOnScreenMenu;
      const TitleBase, Title0, Title1, Title2: string);
    property Value: TVector3 read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    procedure AddLight(Node: TX3DNode);
    procedure DestructionNotification(const Node: TX3DNode);
    procedure ClickEditLight(Sender: TObject);
    procedure ClickEditHeadlight(Sender: TObject);
    procedure ClickClose(Sender: TObject);
    procedure AmbientColorChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Do a partial destruction: remove everything that can have connections
      to existing scene. }
    procedure ClearLights;
  end;

  TLightMenu = class(TV3DOnScreenMenu)
  strict private
    Light: TAbstractLightNode;
    ColorSlider: TMenuVector3Sliders;
    IntensitySlider: TMyFloatSlider;
    AmbientIntensitySlider: TMyFloatSlider;
    OnToggle: TCastleOnScreenMenuItemToggle;
    procedure ColorChanged(Sender: TObject);
    procedure IntensityChanged(Sender: TObject);
    procedure AmbientIntensityChanged(Sender: TObject);
    procedure ClickOn(Sender: TObject);
  strict protected
    procedure ClickBack(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractLightNode); reintroduce;
    procedure AfterCreate;
  end;

  TPunctualLightMenu = class(TLightMenu)
  strict private
    Light: TAbstractPunctualLightNode;
    LocationSlider: TMenuVector3Sliders;
    procedure LocationChanged(Sender: TObject);
    procedure ClickShadowsMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractPunctualLightNode); reintroduce;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    procedure UpdateLightLocation;
  end;

  TPositionalLightMenu = class(TPunctualLightMenu)
  strict private
    Light: TAbstractPositionalLightNode;
    AttenuationSlider: TMenuVector3Sliders;
    procedure AttenuationChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractPositionalLightNode); reintroduce;
  end;

  TSpot1LightMenu = class(TPositionalLightMenu)
  strict private
    Light: TSpotLightNode_1;
    CutOffAngleSlider: TMyFloatSlider;
    DropOffRateSlider: TMyFloatSlider;
    procedure CutOffAngleChanged(Sender: TObject);
    procedure DropOffRateChanged(Sender: TObject);
    procedure ClickDirection(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALight: TSpotLightNode_1); reintroduce;
  end;

  TSpotLightMenu = class(TPositionalLightMenu)
  strict private
    Light: TSpotLightNode;
    CutOffAngleSlider: TMyFloatSlider;
    BeamWidthSlider: TMyFloatSlider;
    procedure CutOffAngleChanged(Sender: TObject);
    procedure BeamWidthChanged(Sender: TObject);
    procedure ClickDirection(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALight: TSpotLightNode); reintroduce;
  end;

  TDirectionalLightMenu = class(TPunctualLightMenu)
  strict private
    Light: TAbstractDirectionalLightNode;
    procedure ClickDirection(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractDirectionalLightNode); reintroduce;
  end;

  THeadLightMenu = class(TV3DOnScreenMenu)
  strict private
    Headlight: TAbstractLightNode;
    AmbientIntensitySlider: TMyFloatSlider;
    ColorSlider: TMenuVector3Sliders;
    IntensitySlider: TMyFloatSlider;
    AttenuationSlider: TMenuVector3Sliders;
    procedure ColorChanged(Sender: TObject);
    procedure AttenuationChanged(Sender: TObject);
    procedure AmbientIntensityChanged(Sender: TObject);
    procedure IntensityChanged(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AHeadlight: TAbstractLightNode); reintroduce;
  end;

  TCastle2ExponentSlider = class(TCastleIntegerSlider)
    function ValueToStr(const AValue: Integer): string; override;
  end;

  TShadowsMenu = class(TV3DOnScreenMenu)
  strict private
    Light: TAbstractPunctualLightNode;
    ShadowsToggle: TCastleOnScreenMenuItemToggle;
    ShadowVolumesToggle: TCastleOnScreenMenuItemToggle;
    ShadowVolumesMainToggle: TCastleOnScreenMenuItemToggle;
    SliderMapSizeExponent: TCastle2ExponentSlider;
    SliderMapBias: TMyFloatSlider;
    SliderMapScale: TMyFloatSlider;
    CurrentProjectionLabel: TCastleLabel;
    procedure ClickShadows(Sender: TObject);
    procedure ClickShadowVolumes(Sender: TObject);
    procedure ClickShadowVolumesMain(Sender: TObject);
    procedure ClickRecalculateProjection(Sender: TObject);
    function RequiredDefaultShadowMap: TGeneratedShadowMapNode;
    procedure MapSizeExponentChanged(Sender: TObject);
    procedure MapBiasChanged(Sender: TObject);
    procedure MapScaleChanged(Sender: TObject);
    function GetMapSizeExponent: Integer;
    procedure SetMapSizeExponent(const Value: Integer);
    function GetMapBias: Single;
    procedure SetMapBias(const Value: Single);
    function GetMapScale: Single;
    procedure SetMapScale(const Value: Single);
    property MapSizeExponent: Integer read GetMapSizeExponent write SetMapSizeExponent;
    property MapBias: Single read GetMapBias write SetMapBias;
    property MapScale: Single read GetMapScale write SetMapScale;
  strict protected
    procedure ClickBack(Sender: TObject); virtual;
    procedure UpdateCurrentProjectionLabel(const ALabel: TCastleLabel); virtual;
  public
    ParentLightMenu: TPunctualLightMenu;
    constructor Create(AOwner: TComponent; ALight: TAbstractPunctualLightNode); reintroduce;
    procedure AfterCreate;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
  end;

  TSpotLightShadowsMenu = class(TShadowsMenu)
  strict private
    Light: TSpotLightNode;
  strict protected
    procedure UpdateCurrentProjectionLabel(const ALabel: TCastleLabel); override;
  public
    constructor Create(AOwner: TComponent; ALight: TSpotLightNode); reintroduce;
  end;

  TDirectionalLightShadowsMenu = class(TShadowsMenu)
  strict private
    Light: TAbstractDirectionalLightNode;
  strict protected
    procedure UpdateCurrentProjectionLabel(const ALabel: TCastleLabel); override;
  public
    constructor Create(AOwner: TComponent; ALight: TAbstractDirectionalLightNode); reintroduce;
  end;

{ global utils --------------------------------------------------------------- }

var
  { Local copy of MainViewport and Window for lights editor.
    Both @nil when we're closed, never @nil when we're open. }
  MainViewport: TCastleViewport;
  Window: TCastleWindow;
  WindowMarginTop: Single;

  LightsMenu: TLightsMenu;
  Gizmo: TInternalScene;

procedure SetCurrentMenu(const NewValue: TCastleOnScreenMenu);
begin
  Window.Controls.MakeSingle(TCastleOnScreenMenu, NewValue, true);
end;

function LightsEditorIsOpen: boolean;
begin
  Result := MainViewport <> nil;
end;

{ Enlarged viewport bounding box, never empty. }
function ViewportLargerBox(const MainViewport: TCastleViewport): TBox3D;
const
  DefaultSize = 10;
var
  BoxSizes: TVector3;
begin
  Result := MainViewport.Items.BoundingBox;
  if Result.IsEmpty then
    Result := Box3D(
      Vector3(-DefaultSize, -DefaultSize, -DefaultSize),
      Vector3( DefaultSize,  DefaultSize,  DefaultSize)) else
  begin
    BoxSizes := Result.Size;
    Result.Data[0] := Result.Data[0] - BoxSizes;
    Result.Data[1] := Result.Data[1] + BoxSizes;
  end;
end;

procedure IniitalizeGizmo;
const
  LightGizmoPivot: TVector2 = (X:58.5; Y: 146-58.5);
var
  RootNode: TX3DRootNode;
  Billboard: TBillboardNode;
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Texture: TPixelTextureNode;
  Material: TUnlitMaterialNode;
  Quad: TQuadSetNode;
  QuadRect: TFloatRectangle;
  QuadCoords: TCoordinateNode;
  QuadTexCoords: TTextureCoordinateNode;
  Size: Single;
begin
  Size := ViewportLargerBox(MainViewport).AverageSize / 50;

  QuadRect.Left   := - LightGizmoPivot[0];
  QuadRect.Bottom := - LightGizmoPivot[1];
  QuadRect.Width  := Light_Gizmo.Width;
  QuadRect.Height := Light_Gizmo.Height;
  QuadRect := QuadRect.ScaleAround0(Size / Light_Gizmo.Width);

  QuadCoords := TCoordinateNode.Create;
  QuadCoords.SetPoint([
    Vector3(QuadRect.Left , QuadRect.Bottom, 0),
    Vector3(QuadRect.Right, QuadRect.Bottom, 0),
    Vector3(QuadRect.Right, QuadRect.Top, 0),
    Vector3(QuadRect.Left , QuadRect.Top, 0)
  ]);

  QuadTexCoords := TTextureCoordinateNode.Create;
  QuadTexCoords.SetPoint([
    Vector2(0, 0),
    Vector2(1, 0),
    Vector2(1, 1),
    Vector2(0, 1)
  ]);

  Quad := TQuadSetNode.Create;
  Quad.Coord := QuadCoords;
  Quad.TexCoord := QuadTexCoords;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := YellowRGB;

  Texture := TPixelTextureNode.Create;
  Texture.FdImage.Value := Light_Gizmo.MakeCopy;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;
  Appearance.Texture := Texture;
  Appearance.ShadowCaster := false;

  Shape := TShapeNode.Create;
  Shape.Geometry := Quad;
  Shape.Appearance := Appearance;

  Billboard := TBillboardNode.Create;
  Billboard.AxisOfRotation := TVector3.Zero;
  Billboard.AddChildren(Shape);

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(Billboard);

  Gizmo := TInternalScene.Create(Window);
  Gizmo.Load(RootNode, true);
  Gizmo.ProcessEvents := true; // for Billboard to work
  Gizmo.Exists := false; // initially not existing
end;

procedure LightsEditorOpen(const AMainViewport: TCastleViewport;
  const AWindow: TCastleWindow; const AWindowMarginTop: Single);
begin
  if MainViewport = AMainViewport then Exit;
  MainViewport := AMainViewport;
  Window := AWindow;
  WindowMarginTop := AWindowMarginTop;

  FreeAndNil(LightsMenu);
  LightsMenu := TLightsMenu.Create(nil);
  SetCurrentMenu(LightsMenu);
  MenuLightsEditor.Checked := true;

  { create Gizmo on demand }
  if Gizmo = nil then
    IniitalizeGizmo;
  MainViewport.Items.Add(Gizmo);
end;

procedure LightsEditorClose;
begin
  if MainViewport = nil then Exit;

  SetCurrentMenu(nil);

  { Just free Gizmo stuff, it will be recreated next time.
    This makes sure we update gizmo Size when loading different scenes. }
  FreeAndNil(Gizmo);

  { We don't immediately free here LightsMenu instance, because this is called
    also by TLightsMenu.ClickClose, and we should not free ourselves
    from our own method. So instead leave LightsMenu instance existing,
    but make sure (for speed) that it's not connected by DestructionNotifications
    to our scene. }
  if LightsMenu <> nil then
    LightsMenu.ClearLights;

  MainViewport := nil;
  Window := nil;
  MenuLightsEditor.Checked := false;
end;

function MessageInputQueryDirection(
  Window: TCastleWindow; const Title: string;
  var Value: TVector3): boolean;
var
  Pos, Up: TVector3;
  s: string;
begin
  Result := false;
  s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
  if MessageInputQuery(Window, Title, s) then
  begin
    try
      if LowerCase(Trim(S)) = 'c' then
        MainViewport.Camera.GetView(Pos, Value, Up) else
        Value := Vector3FromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 3 value : ' + E.Message);
    end;
  end;
end;

const
  AttenuationRange: TBox3D = (Data: ((X:0; Y: 0; Z: 0), (X:2; Y: 2; Z: 2)));

{ TMyFloatSlider ------------------------------------------------------------- }

constructor TMyFloatSlider.Create(AOwner: TComponent);
begin
  inherited;
  FontSize := 10;
end;

{ TMenuVector3Sliders -------------------------------------------------------- }

constructor TMenuVector3Sliders.Create(const AOwner: TComponent;
  const Range: TBox3D; const AValue: TVector3);
var
  I: Integer;
begin
  inherited Create(AOwner);
  for I := 0 to 2 do
  begin
    Floats[I] := TMyFloatSlider.Create(Self);
    Floats[I].Min := Range.Data[0][I];
    Floats[I].Max := Range.Data[1][I];
    Floats[I].Value := AValue[I];
    Floats[I].OnChange := @ChildSliderChanged;
  end;
end;

constructor TMenuVector3Sliders.Create(const AOwner: TComponent;
  const Min, Max: Single; const AValue: TVector3);
begin
  Create(AOwner,
    Box3D(Vector3(Min, Min, Min), Vector3(Max, Max, Max)), AValue);
end;

procedure TMenuVector3Sliders.AddToMenu(const Menu: TCastleOnScreenMenu;
  const TitleBase, Title0, Title1, Title2: string);
var
  I: Integer;
  Title: TString3;
  TitleBaseSpace: string;
begin
  Title[0] := Title0;
  Title[1] := Title1;
  Title[2] := Title2;
  if TitleBase <> '' then
    TitleBaseSpace := TitleBase + ' ' else
    TitleBaseSpace := '';
  for I := 0 to 2 do
    Menu.Add(TitleBaseSpace + Title[I], Floats[I]);
end;

function TMenuVector3Sliders.GetValue: TVector3;
var
  I: Integer;
begin
  for I := 0 to 2 do
    Result.InternalData[I] := Floats[I].Value;
end;

procedure TMenuVector3Sliders.SetValue(const AValue: TVector3);
var
  I: Integer;
begin
  for I := 0 to 2 do
    Floats[I].Value := AValue[I];
end;

procedure TMenuVector3Sliders.ChildSliderChanged(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TV3DOnScreenMenu ----------------------------------------------------------- }

constructor TV3DOnScreenMenu.Create(AOwner: TComponent);
begin
  inherited;
  BackgroundOpacityFocused := 0.3;
  BackgroundOpacityNotFocused := 0.2;

  Anchor(hpLeft, 20);
  Anchor(vpTop, -WindowMarginTop - 20);

  NonFocusableItemColor := Vector4(0.8, 0.8, 1, 1);
end;

procedure TV3DOnScreenMenu.AddTitle(const S: string);
begin
  Add(S);
end;

{ TLightsMenu ------------------------------------------------------- }

constructor TLightsMenu.Create(AOwner: TComponent);
var
  I: Integer;
  Light: TAbstractLightNode;
  LightEditButton: TCastleOnScreenMenuItem;
begin
  inherited;

  Lights := TX3DNodeList.Create(false);

  AmbientColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, RenderContext.GlobalAmbient);
  AmbientColorSlider.OnChange := @AmbientColorChanged;

  MainViewport.Items.MainScene.RootNode.EnumerateNodes(TAbstractLightNode, @AddLight, false);

  AddTitle('Lights Editor:');
  for I := 0 to Lights.Count - 1 do
  begin
    Light := Lights[I] as TAbstractLightNode;
    LightEditButton := TCastleOnScreenMenuItem.Create(Self);
    LightEditButton.Tag := I;
    LightEditButton.OnClick := @ClickEditLight;
    LightEditButton.Caption := Format('Edit %d: %s', [I, Light.NiceName]);
    Add(LightEditButton);
  end;
  AmbientColorSlider.AddToMenu(Self, 'Global Ambient Light', 'Red', 'Green', 'Blue');
  Add('Edit Headlight', @ClickEditHeadlight);
  Add('Close Lights Editor', @ClickClose);
end;

destructor TLightsMenu.Destroy;
begin
  ClearLights;
  inherited;
end;

procedure TLightsMenu.ClearLights;
var
  I: Integer;
begin
  if Lights <> nil then
  begin
    for I := 0 to Lights.Count - 1 do
      Lights[I].RemoveDestructionNotification(@DestructionNotification);
  end;
  FreeAndNil(Lights);

  if Headlight <> nil then
  begin
    Headlight.RemoveDestructionNotification(@DestructionNotification);
    Headlight := nil;
  end;

  FreeAndNil(LightMenu);
  FreeAndNil(HeadLightMenu);
end;

procedure TLightsMenu.DestructionNotification(const Node: TX3DNode);
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
        Lights[I].RemoveDestructionNotification(@DestructionNotification);
    Lights.Clear;
  end;

  if (Headlight <> nil) and (Node <> Headlight) then
    Headlight.RemoveDestructionNotification(@DestructionNotification);
  Headlight := nil;

  { At one point I tried here to return to LightsMenu,
    and do InitializeLightsItems again.
    But this isn't such good idea, because when we release the scene
    to load a new one, we currently have empty MainViewport.MainScene
    or MainViewport.MainScene.RootNode, and we don't see any lights yet.
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
    Node.AddDestructionNotification(@DestructionNotification);
  end;
end;

procedure TLightsMenu.ClickEditLight(Sender: TObject);
var
  LightIndex: Integer;
  Node: TAbstractLightNode;
begin
  FreeAndNil(LightMenu);
  LightIndex := (Sender as TCastleOnScreenMenuItem).Tag;
  Node := Lights[LightIndex] as TAbstractLightNode;
  if Node is TSpotLightNode_1 then
    LightMenu := TSpot1LightMenu.Create(Self, TSpotLightNode_1(Node))
  else
  if Node is TSpotLightNode then
    LightMenu := TSpotLightMenu.Create(Self, TSpotLightNode(Node))
  else
  if Node is TAbstractDirectionalLightNode then
    LightMenu := TDirectionalLightMenu.Create(Self, TAbstractDirectionalLightNode(Node))
  else
  if Node is TAbstractPositionalLightNode then
    LightMenu := TPositionalLightMenu.Create(Self, TAbstractPositionalLightNode(Node))
  else
    { fallback on TLightMenu, e.g. for TEnvironmentLightNode }
    LightMenu := TLightMenu.Create(Self, Node);
  LightMenu.AfterCreate;
  SetCurrentMenu(LightMenu);
end;

procedure TLightsMenu.ClickEditHeadlight(Sender: TObject);
var
  NewHeadlight: TAbstractLightNode;
begin
  FreeAndNil(HeadLightMenu);
  NewHeadlight := MainViewport.Items.HeadlightNode;
  HeadLightMenu := THeadLightMenu.Create(Self, NewHeadlight);
  SetCurrentMenu(HeadLightMenu);

  if Headlight <> NewHeadlight then
  begin
    if Headlight <> nil then
      Headlight.RemoveDestructionNotification(@DestructionNotification);
    Headlight := NewHeadlight;
    Headlight.AddDestructionNotification(@DestructionNotification);
  end;
end;

procedure TLightsMenu.ClickClose(Sender: TObject);
begin
  LightsEditorClose;
end;

procedure TLightsMenu.AmbientColorChanged(Sender: TObject);
begin
  RenderContext.GlobalAmbient := AmbientColorSlider.Value;

  { TODO: We just directly set global paramater now.
    Default is equal to 0.2, 0.2, 0.2 default.
    This could be changed to control NavigationInfo.globalAmbient field
    (InstantReality extension that we plan to implement too,
    see http://doc.instantreality.org/documentation/nodetype/NavigationInfo/ ),
    and then we would keep GlobalAmbient at TCastleScene level. }
end;

{ TLightMenu ---------------------------------------------------------- }

constructor TLightMenu.Create(AOwner: TComponent; ALight: TAbstractLightNode);
begin
  inherited Create(AOwner);

  Light := ALight;

  ColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, Light.Color);
  ColorSlider.OnChange := @ColorChanged;

  IntensitySlider := TMyFloatSlider.Create(Self);
  IntensitySlider.Min := 0;
  IntensitySlider.Max := 10;
  IntensitySlider.Value := Light.Intensity;
  IntensitySlider.OnChange := @IntensityChanged;

  AmbientIntensitySlider := TMyFloatSlider.Create(Self);
  AmbientIntensitySlider.Min := 0;
  AmbientIntensitySlider.Max := 1;
  AmbientIntensitySlider.Value := Light.AmbientIntensity;
  AmbientIntensitySlider.OnChange := @AmbientIntensityChanged;

  OnToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  OnToggle.Caption := 'On';
  OnToggle.Checked := Light.IsOn;
  OnToggle.OnClick := @ClickOn;

  AddTitle('Edit ' + Light.NiceName + ':');
  ColorSlider.AddToMenu(Self, '', 'Red', 'Green', 'Blue');
  Add('Intensity', IntensitySlider);
  Add('Ambient Intensity', AmbientIntensitySlider);
  Add(OnToggle);
end;

procedure TLightMenu.AfterCreate;
begin
  Add('Back to Lights Menu', @ClickBack);
end;

procedure TLightMenu.ColorChanged(Sender: TObject);
begin
  Light.Color := ColorSlider.Value;
end;

procedure TLightMenu.IntensityChanged(Sender: TObject);
begin
  Light.Intensity := IntensitySlider.Value;
end;

procedure TLightMenu.AmbientIntensityChanged(Sender: TObject);
begin
  Light.AmbientIntensity := AmbientIntensitySlider.Value;
end;

procedure TLightMenu.ClickOn(Sender: TObject);
begin
  OnToggle.Checked := not OnToggle.Checked;
  Light.IsOn := OnToggle.Checked;
end;

procedure TLightMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(LightsMenu);
  Gizmo.Exists := false;
end;

{ TPunctualLightMenu ---------------------------------------------------------- }

constructor TPunctualLightMenu.Create(AOwner: TComponent; ALight: TAbstractPunctualLightNode);
var
  Box: TBox3D;
begin
  inherited Create(AOwner, ALight);

  Light := ALight;

  { determine sensible lights positions.
    Box doesn't depend on Light.SceneLocation, to not change range each time
    --- but this causes troubles,
    as Light.SceneLocation may not fit within range,
    which is uncomfortable (works Ok, but not nice for user). }
  Box := ViewportLargerBox(MainViewport);
  LocationSlider := TMenuVector3Sliders.Create(Self, Box, Light.ProjectionWorldLocation);
  LocationSlider.OnChange := @LocationChanged;

  LocationSlider.AddToMenu(Self, 'Location', 'X', 'Y', 'Z');
  Add('Shadows Settings...', @ClickShadowsMenu);

  Gizmo.Exists := true;
  Gizmo.Translation := Light.ProjectionWorldLocation;
end;

procedure TPunctualLightMenu.ClickShadowsMenu(Sender: TObject);
var
  ShadowsMenu: TShadowsMenu;
begin
  if Light is TSpotLightNode then
    ShadowsMenu := TSpotLightShadowsMenu.Create(Self, TSpotLightNode(Light))
  else
  if Light is TAbstractDirectionalLightNode then
    ShadowsMenu := TDirectionalLightShadowsMenu.Create(Self, TAbstractDirectionalLightNode(Light))
  else
    ShadowsMenu := TShadowsMenu.Create(Self, Light);
  ShadowsMenu.ParentLightMenu := Self;
  ShadowsMenu.AfterCreate;
  SetCurrentMenu(ShadowsMenu);
end;

procedure TPunctualLightMenu.LocationChanged(Sender: TObject);
begin
  Light.ProjectionWorldLocation := LocationSlider.Value;
  Gizmo.Translation := LocationSlider.Value;
end;

procedure TPunctualLightMenu.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  UpdateLightLocation;
end;

procedure TPunctualLightMenu.UpdateLightLocation;
var
  V: TVector3;
begin
  { light location may change due to various things
    (it may be animated by X3D events, it may change when we turn on
    shadows:=TRUE for DirectionalLight...).
    So just update it continuously. }
  V := Light.ProjectionWorldLocation;
  if not TVector3.Equals(V, LocationSlider.Value) then
  begin
    LocationSlider.Value := V;
    Gizmo.Translation := V;
  end;
end;

{ TPositionalLightMenu ------------------------------------------------------- }

constructor TPositionalLightMenu.Create(AOwner: TComponent; ALight: TAbstractPositionalLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  AttenuationSlider := TMenuVector3Sliders.Create(Self,
    AttenuationRange, Light.Attenuation);
  AttenuationSlider.OnChange := @AttenuationChanged;

  AttenuationSlider.AddToMenu(Self, 'Attenuation', 'Constant' , 'Linear', 'Quadratic');
end;

procedure TPositionalLightMenu.AttenuationChanged(Sender: TObject);
begin
  Light.Attenuation := AttenuationSlider.Value;
end;

{ TSpot1LightMenu ------------------------------------------------------- }

constructor TSpot1LightMenu.Create(AOwner: TComponent; ALight: TSpotLightNode_1);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  CutOffAngleSlider := TMyFloatSlider.Create(Self);
  CutOffAngleSlider.Min := 0.01;
  CutOffAngleSlider.Max := Pi/2;
  CutOffAngleSlider.Value := Light.FdCutOffAngle.Value;
  CutOffAngleSlider.OnChange := @CutOffAngleChanged;

  DropOffRateSlider := TMyFloatSlider.Create(Self);
  DropOffRateSlider.Min := 0;
  DropOffRateSlider.Max := 1;
  DropOffRateSlider.Value := Light.FdDropOffRate.Value;
  DropOffRateSlider.OnChange := @DropOffRateChanged;

  Add('Direction ...', @ClickDirection);
  Add('Cut Off Angle', CutOffAngleSlider);
  Add('Drop Off Rate', DropOffRateSlider);
end;

procedure TSpot1LightMenu.ClickDirection(Sender: TObject);
var
  Vector: TVector3;
begin
  Vector := Light.ProjectionWorldDirection;
  if MessageInputQueryDirection(Window, 'Change direction' +nl+
    '(Input "C" to use current camera''s direction)',
    Vector) then
    Light.ProjectionWorldDirection := Vector;
end;

procedure TSpot1LightMenu.CutOffAngleChanged(Sender: TObject);
begin
  Light.FdCutOffAngle.Value := CutOffAngleSlider.Value;
end;

procedure TSpot1LightMenu.DropOffRateChanged(Sender: TObject);
begin
  Light.FdDropOffRate.Value := DropOffRateSlider.Value;
end;

{ TSpotLightMenu ------------------------------------------------------- }

constructor TSpotLightMenu.Create(AOwner: TComponent; ALight: TSpotLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;

  CutOffAngleSlider := TMyFloatSlider.Create(Self);
  CutOffAngleSlider.Min := 0.01;
  CutOffAngleSlider.Max := Pi/2;
  CutOffAngleSlider.Value := Light.CutOffAngle;
  CutOffAngleSlider.OnChange := @CutOffAngleChanged;

  BeamWidthSlider := TMyFloatSlider.Create(Self);
  BeamWidthSlider.Min := 0.01;
  BeamWidthSlider.Max := Pi/2;
  BeamWidthSlider.Value := Light.BeamWidth;
  BeamWidthSlider.OnChange := @BeamWidthChanged;

  Add('Direction ...', @ClickDirection);
  Add('Cut Off Angle', CutOffAngleSlider);
  Add('Beam Width', BeamWidthSlider);
end;

procedure TSpotLightMenu.ClickDirection(Sender: TObject);
var
  Vector: TVector3;
begin
  Vector := Light.ProjectionWorldDirection;
  if MessageInputQueryDirection(Window, 'Change direction' +nl+
    '(Input "C" to use current camera''s direction)',
    Vector) then
    Light.ProjectionWorldDirection := Vector;
end;

procedure TSpotLightMenu.CutOffAngleChanged(Sender: TObject);
begin
  Light.CutOffAngle := CutOffAngleSlider.Value;
end;

procedure TSpotLightMenu.BeamWidthChanged(Sender: TObject);
begin
  Light.BeamWidth := BeamWidthSlider.Value;
end;

{ TDirectionalLightMenu ------------------------------------------------------- }

constructor TDirectionalLightMenu.Create(AOwner: TComponent; ALight: TAbstractDirectionalLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;
  Add('Direction ...', @ClickDirection);
end;

procedure TDirectionalLightMenu.ClickDirection(Sender: TObject);
var
  Vector: TVector3;
begin
  Vector := Light.ProjectionWorldDirection;
  if MessageInputQueryDirection(Window, 'Change direction' +nl+
    '(Input "C" to use current camera''s direction)',
    Vector) then
    Light.ProjectionWorldDirection := Vector;
end;

{ THeadLightMenu --------------------------------------------------------- }

constructor THeadLightMenu.Create(AOwner: TComponent; AHeadlight: TAbstractLightNode);
begin
  inherited Create(AOwner);
  Headlight := AHeadlight;

  AddTitle('Headlight:');

  AmbientIntensitySlider := TMyFloatSlider.Create(Self);
  AmbientIntensitySlider.Min := 0;
  AmbientIntensitySlider.Max := 1;
  AmbientIntensitySlider.Value := Headlight.AmbientIntensity;
  AmbientIntensitySlider.OnChange := @AmbientIntensityChanged;
  Add('Ambient Intensity', AmbientIntensitySlider);

  ColorSlider := TMenuVector3Sliders.Create(Self, 0, 1, Headlight.Color);
  ColorSlider.OnChange := @ColorChanged;
  ColorSlider.AddToMenu(Self, '', 'Red', 'Green', 'Blue');

  IntensitySlider := TMyFloatSlider.Create(Self);
  IntensitySlider.Min := 0;
  IntensitySlider.Max := 1;
  IntensitySlider.Value := Headlight.Intensity;
  IntensitySlider.OnChange := @IntensityChanged;
  Add('Intensity', IntensitySlider);

  if Headlight is TAbstractPositionalLightNode then
  begin
    { This was a nice message about attenuation for headlight,
      but we have nowhere to place it now:
      MessageOk(Window, 'HeadLight is not positional, it is not possible to set attenuation.' +NL+ NL+
        'To use this function, you need to use our KambiNavigationInfo.headlightNode extension inside your VRML/X3D scene source, to indicate that you want headlight to be a PointLight or SpotLight. See the documentation of VRML/X3D extensions in "Castle Game Engine" for examples and details.'); }

    AttenuationSlider := TMenuVector3Sliders.Create(Self, AttenuationRange,
      TAbstractPositionalLightNode(Headlight).Attenuation);
    AttenuationSlider.OnChange := @AttenuationChanged;
    AttenuationSlider.AddToMenu(Self, 'Attenuation', 'Constant' , 'Linear', 'Quadratic');
  end;

  Add('Back to Lights Menu', @ClickBack);
end;

procedure THeadLightMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(LightsMenu);
end;

procedure THeadLightMenu.ColorChanged(Sender: TObject);
begin
  Headlight.Color := ColorSlider.Value;
end;

procedure THeadLightMenu.AttenuationChanged(Sender: TObject);
begin
  (Headlight as TAbstractPositionalLightNode).Attenuation := AttenuationSlider.Value;
end;

procedure THeadLightMenu.AmbientIntensityChanged(Sender: TObject);
begin
  Headlight.AmbientIntensity := AmbientIntensitySlider.Value;
end;

procedure THeadLightMenu.IntensityChanged(Sender: TObject);
begin
  Headlight.Intensity := IntensitySlider.Value;
end;

{ TCastle2ExponentSlider ----------------------------------------------------- }

function TCastle2ExponentSlider.ValueToStr(const AValue: Integer): string;
begin
  Result := IntToStr(2 shl (AValue - 1));
end;

{ TShadowsMenu --------------------------------------------------------------- }

constructor TShadowsMenu.Create(AOwner: TComponent; ALight: TAbstractPunctualLightNode);
begin
  inherited Create(AOwner);
  Light := ALight;

  ShadowsToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  ShadowsToggle.Caption := '        Enable';
  ShadowsToggle.Checked := Light.Shadows;
  ShadowsToggle.OnClick := @ClickShadows;

  ShadowVolumesToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  ShadowVolumesToggle.Caption :='        Off In Shadows';
  ShadowVolumesToggle.Checked := Light.ShadowVolumes;
  ShadowVolumesToggle.OnClick := @ClickShadowVolumes;

  ShadowVolumesMainToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  ShadowVolumesMainToggle.Caption := '        Main (Determines Shadows)';
  ShadowVolumesMainToggle.Checked := Light.ShadowVolumesMain;
  ShadowVolumesMainToggle.OnClick := @ClickShadowVolumesMain;

  SliderMapSizeExponent := TCastle2ExponentSlider.Create(Self);
  SliderMapSizeExponent.Min := 4;
  SliderMapSizeExponent.Max := 13;
  SliderMapSizeExponent.Value := MapSizeExponent;
  SliderMapSizeExponent.OnChange := @MapSizeExponentChanged;

  SliderMapBias := TMyFloatSlider.Create(Self);
  SliderMapBias.Min := 0.0;
  SliderMapBias.Max := 10.0;
  SliderMapBias.Value := MapBias;
  SliderMapBias.OnChange := @MapBiasChanged;

  SliderMapScale := TMyFloatSlider.Create(Self);
  SliderMapScale.Min := 0.0;
  SliderMapScale.Max := 10.0;
  SliderMapScale.Value := MapScale;
  SliderMapScale.OnChange := @MapScaleChanged;

  CurrentProjectionLabel := TCastleLabel.Create(Self);
  CurrentProjectionLabel.Color := Vector4(0.7, 0.7, 0.7, 1);

  AddTitle('Edit ' + Light.NiceName + ' -> Shadows Settings:');
  AddTitle('    Shadow Volumes Settings:');
  Add(ShadowVolumesMainToggle);
  Add(ShadowVolumesToggle);
  AddTitle('    Shadow Maps Settings:');
  Add(ShadowsToggle);
  Add('        Map Size', SliderMapSizeExponent);
  Add('        Map Bias (adjust to polygons slope)', SliderMapBias);
  Add('        Map Scale (adjust to polygons slope)', SliderMapScale);
  Add('        Recalculate Projection Parameters', @ClickRecalculateProjection);
  Add(CurrentProjectionLabel);
end;

procedure TShadowsMenu.AfterCreate;
begin
  Add('Back', @ClickBack);

  { We need to call it, to let OnScreenMenu calculate correct size.
    But we cannot call it from constructor,
    as it's virtual and may use Light in descendants, which was unset
    in constructor. }
  UpdateCurrentProjectionLabel(CurrentProjectionLabel);
end;

procedure TShadowsMenu.ClickShadows(Sender: TObject);
begin
  if (Light is TPointLightNode_1) or
     (Light is TPointLightNode) then
  begin
    MessageOK(Window, 'Shadow maps on point lights are not supported yet. Please speak up on "Castle Game Engine" forum and encourage Michalis to implement them, if you want! :) In the meantime, shadow maps work perfectly on other lights (spot and directional).');
    Exit;
  end;

  ShadowsToggle.Checked := not ShadowsToggle.Checked;
  Light.Shadows := ShadowsToggle.Checked;
end;

procedure TShadowsMenu.ClickRecalculateProjection(Sender: TObject);
var
  WasShadows: boolean;
begin
  if (Light is TPointLightNode_1) or
     (Light is TPointLightNode) then
  begin
    MessageOK(Window, 'Shadow maps on point lights are not supported yet. Please speak up on "Castle Game Engine" forum and encourage Michalis to implement them, if you want! :) In the meantime, shadow maps work perfectly on other lights (spot and directional).');
    Exit;
  end;

  WasShadows := Light.Shadows;
  Light.Shadows := false;
  Light.ProjectionNear := 0;
  Light.ProjectionFar := 0;
  if Light is TDirectionalLightNode then
  begin
    TDirectionalLightNode(Light).ProjectionLocationLocal := TVector3.Zero;
    TDirectionalLightNode(Light).ProjectionRectangle := TVector4.Zero;
  end;
  Light.Shadows := WasShadows;
end;

procedure TShadowsMenu.ClickShadowVolumes(Sender: TObject);
begin
  ShadowVolumesToggle.Checked := not ShadowVolumesToggle.Checked;
  Light.ShadowVolumes := ShadowVolumesToggle.Checked;
end;

procedure TShadowsMenu.ClickShadowVolumesMain(Sender: TObject);
begin
  ShadowVolumesMainToggle.Checked := not ShadowVolumesMainToggle.Checked;
  Light.ShadowVolumesMain := ShadowVolumesMainToggle.Checked;
end;

procedure TShadowsMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(ParentLightMenu);
end;

function TShadowsMenu.RequiredDefaultShadowMap: TGeneratedShadowMapNode;
begin
  if Light.DefaultShadowMap = nil then
  begin
    Light.DefaultShadowMap := TGeneratedShadowMapNode.Create('', Light.BaseUrl);
    Light.DefaultShadowMap.Scene := Light.Scene;
  end;
  Result := Light.DefaultShadowMap;
end;

function TShadowsMenu.GetMapSizeExponent: Integer;
var
  Sm: TGeneratedShadowMapNode;
begin
  Sm := Light.DefaultShadowMap;
  if Sm <> nil then
    Result := Sm.Size
  else
    Result := TGeneratedShadowMapNode.DefaultSize;
  Result := Biggest2Exponent(Result);
end;

procedure TShadowsMenu.SetMapSizeExponent(const Value: Integer);
var
  Sm: TGeneratedShadowMapNode;
begin
  if Value <> GetMapSizeExponent then
  begin
    Sm := RequiredDefaultShadowMap;
    Sm.Size := 2 shl (Value - 1);
  end;
end;

function TShadowsMenu.GetMapBias: Single;
var
  Sm: TGeneratedShadowMapNode;
begin
  Sm := Light.DefaultShadowMap;
  if Sm <> nil then
    Result := Sm.Bias
  else
    Result := TGeneratedShadowMapNode.DefaultBias;
end;

procedure TShadowsMenu.SetMapBias(const Value: Single);
var
  Sm: TGeneratedShadowMapNode;
begin
  if Value <> GetMapBias then
  begin
    Sm := RequiredDefaultShadowMap;
    Sm.Bias := Value;
  end;
end;

function TShadowsMenu.GetMapScale: Single;
var
  Sm: TGeneratedShadowMapNode;
begin
  Sm := Light.DefaultShadowMap;
  if Sm <> nil then
    Result := Sm.Scale
  else
    Result := TGeneratedShadowMapNode.DefaultScale;
end;

procedure TShadowsMenu.SetMapScale(const Value: Single);
var
  Sm: TGeneratedShadowMapNode;
begin
  if Value <> GetMapScale then
  begin
    Sm := RequiredDefaultShadowMap;
    Sm.Scale := Value;
  end;
end;

procedure TShadowsMenu.MapSizeExponentChanged(Sender: TObject);
begin
  MapSizeExponent := SliderMapSizeExponent.Value;
end;

procedure TShadowsMenu.MapBiasChanged(Sender: TObject);
begin
  MapBias := SliderMapBias.Value;
end;

procedure TShadowsMenu.MapScaleChanged(Sender: TObject);
begin
  MapScale := SliderMapScale.Value;
end;

procedure TShadowsMenu.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  { Let ParentLightMenu to update Gizmo, in case light moves.
    Note that DirectionalLight.projectionLocation also may change
    when turning on shadows. }
  ParentLightMenu.UpdateLightLocation;
  UpdateCurrentProjectionLabel(CurrentProjectionLabel);
end;

procedure TShadowsMenu.UpdateCurrentProjectionLabel(const ALabel: TCastleLabel);
begin
  ALabel.Text.Clear;
  ALabel.Text.Add('        Currently used projection for shadow maps:');
  ALabel.Text.Add(Format('          Near: %f', [Light.ProjectionNear]));
  ALabel.Text.Add(Format('          Far: %f', [Light.ProjectionFar]));
end;

{ TSpotLightShadowsMenu ------------------------------------------------------- }

constructor TSpotLightShadowsMenu.Create(
  AOwner: TComponent; ALight: TSpotLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;
end;

procedure TSpotLightShadowsMenu.UpdateCurrentProjectionLabel(
  const ALabel: TCastleLabel);
begin
  inherited;
  ALabel.Text.Add(Format('          Angle: %f', [Light.ProjectionAngle]));
end;

{ TDirectionalLightShadowsMenu ------------------------------------------------------- }

constructor TDirectionalLightShadowsMenu.Create(
  AOwner: TComponent; ALight: TAbstractDirectionalLightNode);
begin
  inherited Create(AOwner, ALight);
  Light := ALight;
end;

procedure TDirectionalLightShadowsMenu.UpdateCurrentProjectionLabel(
  const ALabel: TCastleLabel);
var
  ProjectionRectangle: TFloatRectangle;
begin
  inherited;
  ALabel.Text.Add(Format('          Location: %s',
    [Light.ProjectionLocation.ToString]));
  ProjectionRectangle := TFloatRectangle.FromX3DVector(Light.ProjectionRectangle);
  ALabel.Text.Add(Format('          Rectangle: %fx%f %fx%f',
    [ProjectionRectangle.Left,
     ProjectionRectangle.Bottom,
     ProjectionRectangle.Width,
     ProjectionRectangle.Height]));
end;

finalization
  { free if it exists at the end }
  FreeAndNil(LightsMenu);
end.
