unit V3DSceneWarnings;

interface

uses Classes;

type
  { A simple wrapper around TStringList.

    Adds multiline strings (with NL inside) nicely.

    And keeps Count property separate,
    Count property reflects number of added items (as opposed to
    TStringList.Count, that reflects the number of lines, which is different
    when we added multiline strings). }
  TSceneWarnings = class
  private
    FCount: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Items: TStringList;
    property Count: Cardinal read FCount;

    procedure Add(const S: string);
    procedure Clear;
    procedure Assign(Source: TSceneWarnings);
  end;

implementation

uses SysUtils, KambiUtils;

constructor TSceneWarnings.Create;
begin
  inherited;
  Items := TStringList.Create;
end;

destructor TSceneWarnings.Destroy;
begin
  FreeAndNil(Items);
  inherited;
end;

procedure TSceneWarnings.Add(const S: string);
var
  SWarning: TStringList;
begin
  if Pos(NL, S) <> 0 then
  begin
    { Handle multiline error msgs in S nicely (multiline error msgs may
      be generated e.g. when GLSL shader source code has errors). }
    SWarning := TStringList.Create;
    try
      SWarning.Text := S;
      Items.AddStrings(SWarning);
    finally FreeAndNil(SWarning) end;
  end else
    Items.Append(S);

  Inc(FCount);
end;

procedure TSceneWarnings.Clear;
begin
  Items.Clear;
  FCount := 0;
end;

procedure TSceneWarnings.Assign(Source: TSceneWarnings);
begin
  Items.Assign(Source.Items);
  FCount := Source.Count;
end;

end.
