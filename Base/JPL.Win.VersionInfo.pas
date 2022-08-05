unit JPL.Win.VersionInfo;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp


  Links

  https://docs.microsoft.com/en-us/windows/win32/api/winver/nf-winver-getfileversioninfoa
  https://docs.microsoft.com/en-us/windows/win32/api/winver/nf-winver-verqueryvaluea
  https://docs.microsoft.com/en-us/windows/win32/api/winver/nf-winver-verlanguagenamea
  https://docs.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo


  How to use

  1. Call Create with the file name. The ReadFile procedure is called automatically and tries to read the VersionInfo from the given file.
  2. If the read is successful, Status will be set to VI_OK and ValidVersionInfo will be set to True.
     Otherwise, the Status will be set to one of the VI_ERROR_* constants.
  3. There can be multiple blocks of text information in a file. They are accessed via the StringItems property.
     Number of these blocks = StringItemsCount.
  4. If the file contains FixedFileInfo, FixedFileInfoExists is set to True.

  ------------------------------------

  Alternative solutions

  FPC
    fileinfo + winpeimagereader: https://wiki.lazarus.freepascal.org/Show_Application_Title,_Version,_and_Company
    https://forum.lazarus.freepascal.org/index.php/topic,13957.msg233094.html#msg233094

  Delphi
    https://delphidabbler.com/software/verinfo (works with FPC after very small modifications)

 }

{ TODO: Pobrać dodatkowe informacje o FixedFileInfo. (zob. opis struktury VS_FIXEDFILEINFO w MSDN }


interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}

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
    procedure Clear;
    function AsString(const Separator: string = '.'): string;
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
    procedure Clear;
    function AsString(bIncludeTranslationRec: Boolean = False; bIncludeLangInfo: Boolean = False; PadStr: string = ''): string;
  end;

  TExeVersionInfo = record
    FileVersion: TVIFileVersion;
    StringInfo: TVIStringInfoItem;
    function ReadFromFile(const FileName: string): Boolean;
    procedure Clear;
  end;

  //TVIStringItems = {$IFDEF FPC}specialize{$ENDIF} TArray<TVIStringInfoItem>;
  TVIStringItems = TArray<TVIStringInfoItem>;


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
    function TryGetEnglishStringInfoItem(out sii: TVIStringInfoItem): Boolean;

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
function VIFixedFileInfoToStr(const ffi: TVSFIXEDFILEINFO): string;


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}


{$region '   helpers   '}
procedure VIClearStringInfoItem(var sii: TVIStringInfoItem);
begin
  sii.Clear;
end;

function VIStringInfoItemToStr(const sii: TVIStringInfoItem; bIncludeTranslationRec: Boolean = False; bIncludeLangInfo: Boolean = False;
  PadStr: string = ''): string;
begin
  Result := sii.AsString(bIncludeTranslationRec, bIncludeLangInfo, PadStr);
end;

function VIFileVersionToStr(const fv: TVIFileVersion): string;
begin
  Result := fv.AsString('.');
end;

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
{$endregion helpers}


{$region '                   TJPVersionInfo                      '}

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

function TJPVersionInfo.TryGetEnglishStringInfoItem(out sii: TVIStringInfoItem): Boolean;
begin
  Result := False;
  if not FValidVersionInfo then Exit;
  if (FEnglishStringItemIndex >= 0) and (FEnglishStringItemIndex < StringItemsCount) then
  begin
    sii := StringItems[FEnglishStringItemIndex];
    Result := True;
  end;
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

{$endregion TJPVersionInfo}


{ TVIStringInfoItem }

procedure TVIStringInfoItem.Clear;
begin
  TranslateRec.wCodePage := 0;
  TranslateRec.wLanguage := 0;
  LanguageName := '';
  CodePageStr := '';
  ProductName := '';
  ProductVersion := '';
  FileVersion := '';
  FileDescription := '';
  OriginalFileName := '';
  InternalName := '';
  CompanyName := '';
  LegalCopyright := '';
  LegalTrademarks := '';
  PrivateBuild := '';
  SpecialBuild := '';
  Comments := '';
end;

function TVIStringInfoItem.AsString(bIncludeTranslationRec: Boolean = False; bIncludeLangInfo: Boolean = False; PadStr: string = ''): string;
var
  s: string;
begin
  Result :=
    PadStr + 'Product name: ' + ProductName + ENDL +
    PadStr + 'Product version: ' + ProductVersion + ENDL +
    PadStr + 'File version: ' + FileVersion + ENDL +
    PadStr + 'File description: ' + FileDescription + ENDL +
    PadStr + 'Original file name: ' + OriginalFileName + ENDL +
    PadStr + 'Internal name: ' + InternalName + ENDL +
    PadStr + 'Company name: ' + CompanyName + ENDL +
    PadStr + 'Legal copyright: ' + LegalCopyright + ENDL;

    s := LegalTrademarks;
    if s<> '' then Result := Result + PadStr + 'Legal trademarks: ' + s + ENDL;

    s := PrivateBuild;
    if s <> '' then Result := Result + PadStr + 'Private build: ' + s + ENDL;

    s := SpecialBuild;
    if s <> '' then Result := Result + PadStr + 'Special build: ' + s + ENDL;

    s := Comments;
    if s <> '' then Result := Result + PadStr + 'Comments: ' + s + ENDL;


  if bIncludeTranslationRec then
  begin
    Result := Result +
      PadStr + 'Translation - Language: ' + IntToStr(TranslateRec.wLanguage) + ENDL +
      PadStr + 'Translation - Code page: ' + IntToStr(TranslateRec.wCodePage) + ENDL;
  end;

  if bIncludeLangInfo then
  begin
    Result := Result +
      PadStr + 'Language name: ' + LanguageName + ENDL +
      PadStr + 'Code page: ' + CodePageStr + ENDL;
  end;

  Result := TrimRight(Result);
end;


{ TVIFileVersion }

procedure TVIFileVersion.Clear;
begin
  MajorVersion := 0;
  MinorVersion := 0;
  Revision := 0;
  Build := 0;
end;

function TVIFileVersion.AsString(const Separator: string): string;
begin
  Result :=
    itos(MajorVersion) + Separator +
    itos(MinorVersion) + Separator +
    itos(Revision) + Separator +
    itos(Build);
end;



{ TExeVersionInfo }

procedure TExeVersionInfo.Clear;
begin
  FileVersion.Clear;
  StringInfo.Clear;
end;

function TExeVersionInfo.ReadFromFile(const FileName: string): Boolean;
var
  vi: TJPVersionInfo;
begin
  Result := False;
  Clear;
  if not FileExists(FileName) then Exit;

  vi := TJPVersionInfo.Create(FileName);
  try
    if not vi.ValidVersionInfo then Exit;

    Self.FileVersion := vi.FileVersion;
    if not vi.TryGetEnglishStringInfoItem(Self.StringInfo) then Exit;

    Result := True;
  finally
    vi.Free;
  end;
end;



{$ENDIF} // MSWINDOWS





end.
