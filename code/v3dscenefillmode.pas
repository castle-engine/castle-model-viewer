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
    WireframeColor: TVector3Single;
    BackgroundWireframe: boolean;
  end =
  ( (Name: 'Normal'                               ; Mode: rmFull        ; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe'                            ; Mode: rmFull        ; WireframeEffect: weWireframeOnly ; WireframeColor: (0, 0, 0); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe'                      ; Mode: rmFull        ; WireframeEffect: weSolidWireframe; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette'               ; Mode: rmFull        ; WireframeEffect: weSilhouette    ; WireframeColor: (1, 1, 1); BackgroundWireframe: false; ),

    (Name: 'Solid Shape'                          ; Mode: rmPureGeometry; WireframeEffect: weNormal        ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Wireframe (Single Color)'             ; Mode: rmPureGeometry; WireframeEffect: weWireframeOnly ; WireframeColor: (1, 1, 1); BackgroundWireframe: true ; ),
    (Name: 'Solid Wireframe (Single Color)'       ; Mode: rmPureGeometry; WireframeEffect: weSolidWireframe; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),
    (Name: 'Normal with Silhouette (Single Color)'; Mode: rmPureGeometry; WireframeEffect: weSilhouette    ; WireframeColor: (0, 0, 0); BackgroundWireframe: false; ),

    (Name: 'Silhouette and Border Edges'          ;
      { Mode, WireframeEffect, WireframeColor don't matter here,
        we will not call normal T3DScene.Render in this case. }
      Mode: rmFull;
      WireframeEffect: weNormal;
      WireframeColor: (0, 0, 0);
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
  const ObserverPos: TVector3Single; Scene: TCastleScene);
procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector4Single; Scene: TCastleScene);

implementation

uses CastleGL, CastleGLUtils;

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
  const ObserverPos: TVector3Single; Scene: TCastleScene);
begin
  RenderSilhouetteBorderEdges(Vector4Single(ObserverPos, 1), Scene);
end;

procedure RenderSilhouetteBorderEdges(
  const ObserverPos: TVector4Single; Scene: TCastleScene);
{$ifndef OpenGLES} //TODO-es
var
  PreviousLineWidth: Single;
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
    PreviousLineWidth := RenderContext.LineWidth;
    RenderContext.LineWidth := 5;
    Scene.RenderBorderEdges(IdentityMatrix4Single);
    RenderContext.LineWidth := PreviousLineWidth;

    glColor4f(1, 1, 0, 0.3);
    Scene.RenderSilhouetteEdges(ObserverPos, IdentityMatrix4Single);

  glPopAttrib;
{$else}
begin
{$endif}
end;

end.