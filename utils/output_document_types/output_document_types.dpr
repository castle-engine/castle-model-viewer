uses SysUtils, StrUtils,
  CastleUtils, CastleStringUtils, X3DLoad;

function ManifestDocumentTypes: String;
var
  F: TModelFormat;
  Caption, Name, Extension, MimeType: String;
  ParenPos: Integer;
begin
  Result :=
    '    <!-- BEGIN: Generated using castle-model-viewer/utils/output_document_types/ -->' + NL;
  for F in InternalRegisteredModelFormats do
  begin
    Caption := F.FileFilterName;
    // skip "Images", which we can open but castle-model-viewer is not primarily for viewing images
    if Caption = 'Images' then
      Continue;
    // cut off " (*.xxx)" suffix from F.FileFilterName
    ParenPos := Pos('(', Caption);
    if ParenPos > 0 then
      Caption := Trim(Copy(Caption, 1, ParenPos - 1));

    if F.Extensions.Count = 0 then
      raise Exception.CreateFmt('Model format "%s" has no extensions, cannot generate <document_type> for it', [
        Caption
      ]);
    Name := F.Extensions[0];
    Name := PrefixRemove('.', Name, false); // remove loading .
    Name := SDeleteChars(Name, ['-']); // - not allowed in document_type.name

    Result := Result +
      Format('    <document_type caption="%s" name="%s">' + NL, [
        Caption,
        Name
      ]);
    for Extension in F.Extensions do
      Result := Result +
        Format('      <extension>%s</extension>' + NL, [PrefixRemove('.', Extension, false)]);
    for MimeType in F.MimeTypes do
      Result := Result +
        Format('      <mime>%s</mime>' + NL, [MimeType]);
    Result := Result +
      '    </document_type>' + NL;
  end;
  Result := Result +
    '    <!-- END: Generated using castle-model-viewer/utils/output_document_types/ -->' + NL;
end;

begin
  Writeln(ManifestDocumentTypes);
end.