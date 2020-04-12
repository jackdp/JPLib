unit JPL.Win.VersionInfo;

{
  Jacek Pazera
  http://www.pazera-software.com


  Przydatne linki:
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms647003(v=vs.85).aspx - GetFileVersionInfo
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms647464(v=vs.85).aspx - VarQueryValue
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms647463(v=vs.85).aspx - VerLanguageName
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms646997(v=vs.85).aspx - VS_FIXEDFILEINFO structure


  Metoda postępowania z TJPVersionInfo:
  1. Wywołać Create z nazwą pliku. Automatycznie wywoływana jest procedure ReadFile, która próbuje odczytać VersionInfo z podanego pliku.
  2. Jeśli odczyt się powiedzie, Status będzie wynosił VI_OK, a ValidVersionInfo będzie ustawione na True.
     W przeciwnym razie Status będzie równy jednej ze stałych VI_ERROR_*.
  3. W pliku może znajdować się wiele bloków z informacjami tekstowymi. Dostepne są one przez property StringItems.
     Liczba tych bloków = StringItemsCount.
  4. Jeśli plik zawiera FixedFileInfo, FixedFileInfoExists jest ustawiane na True.

  ------------------------------------

  Alternative solutions:
  FPC:
    fileinfo + winpeimagereader: http://wiki.lazarus.freepascal.org/Show_Application_Title,_Version,_and_Company
    http://forum.lazarus.freepascal.org/index.php/topic,13957.msg233094.html#msg233094
  Delphi: http://delphidabbler.com/software/verinfo/ (works with FPC after very small modifications)

 }

{ TODO: Pobrać dodatkowe informacje o FixedFileInfo. (zob. opis struktury VS_FIXEDFILEINFO w MSDN }


interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

uses
  Windows, SysUtils,
  JPL.Strings, JPL.Language, JPL.Conversion;


const
  VI_OK = 0; // All OK!
  VI_ERROR_FILE_NOT_EXISTS = 10; // File FileName not exists
  VI_ERROR_GET_FILE_VERSION_INFO_SIZE = 11; // GetFileVersionInfoSize returned 0
  VI_ERROR_GET_FILE_VERSION_INFO = 12; // GetFileVersionInfo failed
  VI_ERROR_CANNOT_READ_TRANSLATION_ARRAY = 13; // Cannot read translation array '\VarFileInfo\Translation'
  VI_ERROR_NO_TRANSLATION_ARRAY = 14; // Translation array length = 0
  VI_ERROR_INVALID_SIZE_OF_TRANSLATION_ARRAY = 15; // Invalid size of the translation array. Should be multiple of 4 (SizeOf(TVITranslate))

  VI_LANGID_ENGLISH = 1033;

type

  PVITranslate = ^TVITranslate;
  TVITranslate = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;

  TVIFileVersion = record
    MajorVersion: WORD;
    MinorVersion: WORD;
    Revision: WORD;
    Build: WORD;
  end;

  TVIStringInfoItem = record
    TranslateRec: TVITranslate;
    CodePageStr: string;
    LanguageName: string;

    ProductName: string;
    ProductVersion: string;
    FileVersion: string;
    FileDescription: string;
    OriginalFileName: string;
    InternalName: string;
    CompanyName: string;
    LegalCopyright: string;
    LegalTrademarks: string;
    PrivateBuild: string;
    SpecialBuild: string;
    Comments: string;
  end;

  TVIStringItems = {$IFDEF FPC}specialize{$ENDIF} TArray<TVIStringInfoItem>;


  TJPVersionInfo = class
  private
    FEnglishStringItemIndex: integer;
    FFileName: string;
    FFixedFileInfo: TVSFIXEDFILEINFO;
    FFileVersion: TVIFileVersion;
    FValidVersionInfo: Boolean;
    FStatus: integer;
    FFixedFileInfoExists: Boolean;
    FStringItems: TVIStringItems;
    procedure ClearAll;
    function GetStringItemsCount: integer;
    function GetStringItems(Index: integer): TVIStringInfoItem;
    procedure ReadFile;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    property FileName: string read FFileName;

    property ValidVersionInfo: Boolean read FValidVersionInfo;
    property Status: integer read FStatus;

    property FixedFileInfoExists: Boolean read FFixedFileInfoExists;

    // FixedFileInfo returned by VerQueryValue
    property FixedFileInfo: TVSFIXEDFILEINFO read FFixedFileInfo;

    // Parsed FixedFileInfo. Only available when FixedFileInfoExists = True.
    property FileVersion: TVIFileVersion read FFileVersion;

    // Array with string blocks for all available languages
    property StringItems[Index: integer]: TVIStringInfoItem read GetStringItems;
    property StringItemsCount: integer read GetStringItemsCount;

    // Returns Index in StringItems array or -1 if not available.
    property EnglishStringItemIndex: integer read FEnglishStringItemIndex;
  end;



procedure VIClearStringInfoItem(var sii: TVIStringInfoItem);
function VIStringInfoItemToStr(const sii: TVIStringInfoItem; bIncludeTranslationRec: Boolean = False; bIncludeLangInfo: Boolean = False; PadStr: string = ''): string;
function VIFileVersionToStr(const fv: TVIFileVersion): string;


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}


function VIFixedFileInfoToStr(const ffi: TVSFIXEDFILEINFO): string;
begin
  Result :=
    'Signature: ' + IntToHex(ffi.dwSignature, 8) + ENDL +   // Should be 0xFEEF04BD
    'StructVersion: ' + IntToStr(ffi.dwStrucVersion) + ENDL +
    'FileVersionMS: ' + IntToStr(ffi.dwFileVersionMS) + ENDL +
    'FileVersionLS: ' + IntToStr(ffi.dwFileVersionLS) + ENDL +
    'ProductVersionMS: ' + IntToStr(ffi.dwProductVersionMS) + ENDL +
    'ProductVersionLS: ' + IntToStr(ffi.dwProductVersionLS) + ENDL +
    'FileFlagsMask: ' + IntToStr(ffi.dwFileFlagsMask) + ENDL +
    'FileFlags: ' + IntToStr(ffi.dwFileFlags) + ENDL +
    'FileOS: ' + IntToStr(ffi.dwFileOS) + ENDL +
    'FileType: ' + IntToStr(ffi.dwFileType) + ENDL +
    'FileSubtype: ' + IntToStr(ffi.dwFileSubtype) + ENDL +
    'FileDateMS: ' + IntToStr(ffi.dwFileDateMS) + ENDL +
    'FileDateLS: ' + IntToStr(ffi.dwFileDateLS);
end;


constructor TJPVersionInfo.Create(const FileName: string);
begin
  inherited Create;
  SetLength(FStringItems, 0);
  ClearAll;
  FFileName := FileName;
  ReadFile;
end;

destructor TJPVersionInfo.Destroy;

begin
  ClearAll;
  inherited Destroy;
end;

procedure TJPVersionInfo.ClearAll;
var
  i: integer;
begin
  FillChar(FFixedFileInfo, SizeOf(FFixedFileInfo), 0);
  FValidVersionInfo := False;
  FFixedFileInfoExists := False;
  FEnglishStringItemIndex := -1;
//  for i := 0 to High(FStringItems) do VIClearStringInfoItem(FStringItems[i]);
  for i := 0 to Length(FStringItems) - 1 do VIClearStringInfoItem(FStringItems[i]);
  SetLength(FStringItems, 0);
end;

function TJPVersionInfo.GetStringItemsCount: integer;
begin
  Result := Length(FStringItems);
end;

function TJPVersionInfo.GetStringItems(Index: integer): TVIStringInfoItem;
begin
  Result := FStringItems[Index];
end;

procedure TJPVersionInfo.ReadFile;
var
  dwSize: DWORD;
  dwNeeded: DWORD;
  pBlock: PChar;
  PBuffer: Pointer;
  puLen: UINT;
  i, xLangArrayCount: integer;
  PTranslate: PVITranslate;
  sTransRecStr: string;
  PStringValue: Pointer;
  sii: TVIStringInfoItem;
begin

  if not FileExists(FFileName) then
  begin
    FStatus := VI_ERROR_FILE_NOT_EXISTS;
    Exit;
   end;

  dwSize := GetFileVersionInfoSize(PChar(FileName), dwNeeded);
  if dwSize = 0 then
  begin
    FStatus := VI_ERROR_GET_FILE_VERSION_INFO_SIZE;
    Exit;
  end;


  pBlock := StrAlloc(dwSize);
  try

    if not GetFileVersionInfo(PChar(FileName), {dwHandle - Ignored-->} 0, dwSize, pBlock) then
    begin
      FStatus := VI_ERROR_GET_FILE_VERSION_INFO;
      Exit;
    end;


    // lpSubBlock = '\' - The root block. The function retrieves a pointer to the VS_FIXEDFILEINFO structure
    FFixedFileInfoExists := VerQueryValue(pBlock, '\', PBuffer, puLen);
    if FFixedFileInfoExists then
    begin
      FFixedFileInfo := TVSFixedFileInfo(PBuffer^);

      FFileVersion.MajorVersion := HIWORD(FFixedFileInfo.dwFileVersionMS);
      FFileVersion.MinorVersion := LOWORD(FFixedFileInfo.dwFileVersionMS);
      FFileVersion.Revision := HIWORD(FFixedFileInfo.dwFileVersionLS);
      FFileVersion.Build := LOWORD(FFixedFileInfo.dwFileVersionLS);
    end;




    // Sprawdzanie dostępnych języków
    if not VerQueryValue(pBlock, '\VarFileInfo\Translation', PBuffer, puLen) then
    begin
      FStatus := VI_ERROR_CANNOT_READ_TRANSLATION_ARRAY;
      Exit;
    end;

    if puLen = 0 then
    begin
      FStatus := VI_ERROR_NO_TRANSLATION_ARRAY;
      Exit;
    end;

    // puLen powinien być wielokrotnością 4 (SizeOf(TVITranslate)).
    if puLen mod SizeOf(TVITranslate) <> 0 then
    begin
      FStatus := VI_ERROR_INVALID_SIZE_OF_TRANSLATION_ARRAY;
      Exit;
    end;

    xLangArrayCount := puLen div SizeOf(TVITranslate);


    // Odczyt informacji tekstowych dla każdego dostępnego języka
    for i := 1 to xLangArrayCount do
    begin

      GetMem(PTranslate, SizeOf(TVITranslate));

      try

        Move(PBuffer^, PTranslate^, puLen);

        VIClearStringInfoItem(sii);
        sii.TranslateRec.wLanguage := PTranslate^.wLanguage;
        sii.TranslateRec.wCodePage := PTranslate^.wCodePage;

        sii.LanguageName := LanguageIDToStr(sii.TranslateRec.wLanguage);
        sii.CodePageStr := IntToStr(sii.TranslateRec.wCodePage) + ' - ' + GetCodePageDesc(sii.TranslateRec.wCodePage);



        sTransRecStr := Format('%4.4x%4.4x', [PTranslate^.wLanguage, PTranslate^.wCodePage]);


        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\ProductName'), PStringValue, puLen) then
          sii.ProductName := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\ProductVersion'), PStringValue, puLen) then
          sii.ProductVersion := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\FileVersion'), PStringValue, puLen) then
          sii.FileVersion := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\FileDescription'), PStringValue, puLen) then
          sii.FileDescription := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\OriginalFileName'), PStringValue, puLen) then
          sii.OriginalFileName := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\InternalName'), PStringValue, puLen) then
          sii.InternalName := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\CompanyName'), PStringValue, puLen) then
          sii.CompanyName := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\LegalCopyright'), PStringValue, puLen) then
          sii.LegalCopyright := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\LegalTrademarks'), PStringValue, puLen) then
          sii.LegalTrademarks := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\PrivateBuild'), PStringValue, puLen) then
          sii.PrivateBuild := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\SpecialBuild'), PStringValue, puLen) then
          sii.SpecialBuild := PChar(PStringValue);

        if VerQueryValue(pBlock, PChar('\StringFileInfo\' + sTransRecStr + '\Comments'), PStringValue, puLen) then
          sii.Comments := PChar(PStringValue);


        SetLength(FStringItems, Length(FStringItems) + 1);
//        FStringItems[High(FStringItems)] := sii;
        FStringItems[Length(FStringItems) - 1] := sii;
//        if sii.TranslateRec.wLanguage = VI_LANGID_ENGLISH then FEnglishStringItemIndex := High(FStringItems);
        if sii.TranslateRec.wLanguage = VI_LANGID_ENGLISH then FEnglishStringItemIndex := Length(FStringItems) - 1;

      finally
        FreeMem(PTranslate);
      end;

    end; // for i



  finally
    StrDispose(pBlock);
  end;

  FStatus := VI_OK;
  FValidVersionInfo := True;

end;


procedure VIClearStringInfoItem(var sii: TVIStringInfoItem);
begin
  sii.TranslateRec.wCodePage := 0;
  sii.TranslateRec.wLanguage := 0;
  sii.LanguageName := '';
  sii.CodePageStr := '';
  sii.ProductName := '';
  sii.ProductVersion := '';
  sii.FileVersion := '';
  sii.FileDescription := '';
  sii.OriginalFileName := '';
  sii.InternalName := '';
  sii.CompanyName := '';
  sii.LegalCopyright := '';
  sii.LegalTrademarks := '';
  sii.PrivateBuild := '';
  sii.SpecialBuild := '';
  sii.Comments := '';
end;



function VIStringInfoItemToStr(const sii: TVIStringInfoItem; bIncludeTranslationRec: Boolean = False;
  bIncludeLangInfo: Boolean = False; PadStr: string = ''): string;
var
  s: string;
begin
  Result := '';

  if bIncludeTranslationRec then
  begin
    Result := Result +
      PadStr + 'Translation - Language: ' + IntToStr(sii.TranslateRec.wLanguage) + ENDL +
      PadStr + 'Translation - CodePage: ' + IntToStr(sii.TranslateRec.wCodePage) + ENDL;
  end;

  if bIncludeLangInfo then
  begin
    Result := Result +
      PadStr + 'Language name: ' + sii.LanguageName + ENDL +
      PadStr + 'Code page: ' + sii.CodePageStr + ENDL;
  end;

  Result := Result +
    PadStr + 'ProductName: ' + sii.ProductName + ENDL +
    PadStr + 'ProductVersion: ' + sii.ProductVersion + ENDL +
    PadStr + 'FileVersion: ' + sii.FileVersion + ENDL +
    PadStr + 'FileDescription: ' + sii.FileDescription + ENDL +
    PadStr + 'OriginalFileName: ' + sii.OriginalFileName + ENDL +
    PadStr + 'InternalName: ' + sii.InternalName + ENDL +
    PadStr + 'CompanyName: ' + sii.CompanyName + ENDL +
    PadStr + 'LegalCopyright: ' + sii.LegalCopyright + ENDL;

    s := sii.LegalTrademarks;
    if s<> '' then Result := Result + PadStr + 'LegalTrademarks: ' + s + ENDL;

    s := sii.PrivateBuild;
    if s <> '' then Result := Result + PadStr + 'PrivateBuild: ' + s + ENDL;

    s := sii.SpecialBuild;
    if s <> '' then Result := Result + PadStr + 'SpecialBuild: ' + s + ENDL;

    s := sii.Comments;
    if s <> '' then Result := Result + PadStr + 'Comments: ' + s;

    Result := TrimRight(Result);
end;

function VIFileVersionToStr(const fv: TVIFileVersion): string;
begin
  Result :=
    itos(fv.MajorVersion) + '.' +
    itos(fv.MinorVersion) + '.' +
    itos(fv.Revision) + '.' +
    itos(fv.Build);
end;


{$ENDIF} // MSWINDOWS

end.
