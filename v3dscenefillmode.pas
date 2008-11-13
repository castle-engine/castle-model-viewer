unit V3DSceneFillMode;

interface

uses VRMLGLScene, GLWindow, VectorMath;

type
  TFillMode = 0..8;

const
  FillModes: array [TFillMode] of record
    Name: string;
    PureGeometry: boolean;
    WireframeEffect: TVRMLWireframeEffect;
    WireframeColor: TVector3Single;
    BackgroundWireframe: boolean;
  end =
  { group setting with similar PureGeometry together, as changing PureGeometry
    is costly (rebuild of scene display lists), while changing
    WireframeEffect is generally not costly. }
  ( (Name: 'Normal'                               ; PureGeometry: false; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe'                            ; PureGeometry: false; WireframeEffect: weWireframeOnly ; WireframeColor: (0, 0, 0); BackgroundWireframe: true ; ),
    (Name: 'Solid wireframe'                      ; PureGeometry: false; WireframeEffect: weSolidWireframe; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),
    (Name: 'Normal with silhouette'               ; PureGeometry: false; WireframeEffect: weSilhouette    ; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),

    (Name: 'Solid shape'                          ; PureGeometry: true ; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe (single color)'             ; PureGeometry: true ; WireframeEffect: weWireframeOnly ; WireframeColor: (1, 1, 1); BackgroundWireframe: true ; ),
    (Name: 'Solid wireframe (single color)'       ; PureGeometry: true ; WireframeEffect: weSolidWireframe; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Normal with silhouette (single color)'; PureGeometry: true ; WireframeEffect: weSilhouette    ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),

    (Name: 'Silhouette and border edges'          ;
      { PureGeometry, WireframeEffect, WireframeColor don't matter for this }
      PureGeometry: false; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0);
      BackgroundWireframe: false)
  );

var
  FillMode: TFillMode = 0;

  FillModesMenu: array [TFillMode] of TMenuItemRadio;

const
  fmSilhouetteBorderEdges = 8;

procedure MenuAppendFillModes(M: TMenu; BaseIntData: Cardinal);

implementation

procedure MenuAppendFillModes(M: TMenu; BaseIntData: Cardinal);
var
  RadioGroup: TMenuItemRadioGroup;
  FM: TFillMode;
begin
  RadioGroup := nil;

  for FM := Low(FillMode) to High(FillMode) do
  begin
    FillModesMenu[FM] := TMenuItemRadio.Create(
      SQuoteMenuEntryCaption(FillModes[FM].Name),
      BaseIntData + FM, FM = FillMode, true);
    if RadioGroup = nil then
      RadioGroup := FillModesMenu[FM].Group else
      FillModesMenu[FM].Group := RadioGroup;
    M.Append(FillModesMenu[FM]);
  end;
end;

end.