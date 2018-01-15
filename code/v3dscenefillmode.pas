unit V3DSceneFillMode;

{$I v3dsceneconf.inc}

interface

uses CastleScene, CastleWindow, CastleRenderer, CastleVectors;

type
  TFillMode = 0..8;

const
  FillModes: array [TFillMode] of record
    Name: string;
    Mode: TRenderingMode;
    WireframeEffect: TWireframeEffect;
    WireframeColor: TVector3;
    BackgroundWireframe: boolean;
  end =
  ( (Name: 'Normal'                               ; Mode: rmFull        ; WireframeEffect: weNormal        ; WireframeColor: (Data: (0, 0, 0)); BackgroundWireframe: false; ),
    (Name: 'Wireframe'                            ; Mode: rmFull        ; WireframeEffect: weWireframeOnly ; WireframeColor: (Data: (0, 0, 0)); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe'                      ; Mode: rmFull        ; WireframeEffect: weSolidWireframe; WireframeColor: (Data: (1, 1, 1)); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette'               ; Mode: rmFull        ; WireframeEffect: weSilhouette    ; WireframeColor: (Data: (1, 1, 1)); BackgroundWireframe: false; ),

    (Name: 'Solid Shape'                          ; Mode: rmPureGeometry; WireframeEffect: weNormal        ; WireframeColor: (Data: (0, 0, 0)); BackgroundWireframe: false; ),
    (Name: 'Wireframe (Single Color)'             ; Mode: rmPureGeometry; WireframeEffect: weWireframeOnly ; WireframeColor: (Data: (1, 1, 1)); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe (Single Color)'       ; Mode: rmPureGeometry; WireframeEffect: weSolidWireframe; WireframeColor: (Data: (0, 0, 0)); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette (Single Color)'; Mode: rmPureGeometry; WireframeEffect: weSilhouette    ; WireframeColor: (Data: (0, 0, 0)); BackgroundWireframe: false; ),

    (Name: 'Silhouette and Border Edges'          ;
      { Mode, WireframeEffect, WireframeColor don't matter here,
        we will not call normal T3DScene.Render in this case. }
      Mode: rmFull;
      WireframeEffect: weNormal;
      WireframeColor: (Data: (0, 0, 0));
      BackgroundWireframe: false)
  );

  PureGeometryColor: TVector3 = (Data: (1, 1, 1));
  PureGeometryShadowedColor: TVector3 = (Data: (0.5, 0.5, 0.5));

var
  FillMode: TFillMode = 0;
  FillModesMenu: array [TFillMode] of TMenuItemRadio;

const
  fmSilhouetteBorderEdges = 8;

procedure MenuAppendFillModes(M: TMenu; BaseIntData: Cardinal);

implementation

uses
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleGLUtils;

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
