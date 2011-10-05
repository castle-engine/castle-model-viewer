unit V3DSceneFillMode;

interface

uses Scene, CastleWindow, VectorMath;

type
  TFillMode = 0..8;

const
  FillModes: array [TFillMode] of record
    Name: string;
    PureGeometry: boolean;
    WireframeEffect: TWireframeEffect;
    WireframeColor: TVector3Single;
    BackgroundWireframe: boolean;
  end =
  { group setting with similar PureGeometry together, as changing PureGeometry
    is costly (rebuild of scene display lists), while changing
    WireframeEffect is generally not costly. }
  ( (Name: 'Normal'                               ; PureGeometry: false; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe'                            ; PureGeometry: false; WireframeEffect: weWireframeOnly ; WireframeColor: (0, 0, 0); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe'                      ; PureGeometry: false; WireframeEffect: weSolidWireframe; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette'               ; PureGeometry: false; WireframeEffect: weSilhouette    ; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),

    (Name: 'Solid Shape'                          ; PureGeometry: true ; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe (Single Color)'             ; PureGeometry: true ; WireframeEffect: weWireframeOnly ; WireframeColor: (1, 1, 1); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe (Single Color)'       ; PureGeometry: true ; WireframeEffect: weSolidWireframe; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette (Single Color)'; PureGeometry: true ; WireframeEffect: weSilhouette    ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),

    (Name: 'Silhouette and Border Edges'          ;
      { PureGeometry, WireframeEffect, WireframeColor don't matter for this }
      PureGeometry: false; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0);
      BackgroundWireframe: false)
  );

  PureGeometryColor: TVector3Single = (1, 1, 1);
  PureGeometryShadowedColor: TVector3Single = (0.5, 0.5, 0.5);

var
  FillMode: TFillMode = 0;

  FillModesMenu: array [TFillMode] of TMenuItemRadio;

const
  fmSilhouetteBorderEdges = 8;

procedure MenuAppendFillModes(M: TMenu; BaseIntData: Cardinal);

procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector3Single; Scene: T3DScene);
procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector4Single; Scene: T3DScene);

implementation

uses GL;

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

procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector3Single; Scene: T3DScene);
begin
  RenderSilhouetteBorderEdges(Vector4Single(ObserverPos, 1), Scene);
end;

procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector4Single; Scene: T3DScene);
begin
  glPushAttrib(GL_ENABLE_BIT);
    { Draw BorderEdges first, with thicker width. And draw all without depth
      test.

      This way if some edge is both in BorderEdges and ManifoldEdges
      (e.g. it's present 3 times in data), this will be visible here.
      Otherwise RenderSilhouetteEdges would hide this edge, and we would
      not see that it's a both manifold and border edge. }

    glDisable(GL_DEPTH_TEST); { saved by GL_ENABLE_BIT }

    glColor4f(0, 0, 1, 0.3);
    glPushAttrib(GL_LINE_BIT);
      glLineWidth(5); { saved by GL_LINE_BIT }
      Scene.RenderBorderEdges(IdentityMatrix4Single);
    glPopAttrib;

    glColor4f(1, 1, 0, 0.3);
    Scene.RenderSilhouetteEdges(ObserverPos, IdentityMatrix4Single);

  glPopAttrib;
end;

end.