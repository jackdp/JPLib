unit JPL.Win.FileSystem;

interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Windows, SysUtils, Classes, {$IFDEF DCC}{$IFDEF DELPHI2010_OR_ABOVE}IOUtils,{$ENDIF}{$ENDIF}
  JPL.Strings,
  Dialogs,
  Menus,
  ShellAPI, ComObj, ShlObj, ActiveX;


const
  CSIDL_PROFILES = $003e;
  SHGFP_TYPE_CURRENT = 0;

  {$region ' --- CSIDLs --- '}
  ArrCSIDL: array[0..38] of integer = (
    CSIDL_DESKTOP,
    CSIDL_INTERNET,
    CSIDL_PROGRAMS,
    CSIDL_CONTROLS,
    CSIDL_PRINTERS,
    CSIDL_PERSONAL,
    CSIDL_FAVORITES,
    CSIDL_STARTUP,
    CSIDL_RECENT,
    CSIDL_SENDTO,
    CSIDL_BITBUCKET,
    CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY,
    CSIDL_DRIVES,
    CSIDL_NETWORK,
    CSIDL_NETHOOD,
    CSIDL_FONTS,
    CSIDL_TEMPLATES,
    CSIDL_COMMON_STARTMENU,
    CSIDL_COMMON_PROGRAMS,
    CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY,
    CSIDL_APPDATA,
    CSIDL_PRINTHOOD,
    CSIDL_LOCAL_APPDATA,
    CSIDL_ALTSTARTUP,
    CSIDL_COMMON_ALTSTARTUP,
    CSIDL_COMMON_FAVORITES,
    CSIDL_INTERNET_CACHE,
    CSIDL_COOKIES,
    CSIDL_HISTORY,
    CSIDL_PROFILE,
    CSIDL_CONNECTIONS,
    CSIDL_COMMON_MUSIC,
    CSIDL_COMMON_PICTURES,
    CSIDL_COMMON_VIDEO,
    CSIDL_CDBURN_AREA,
    CSIDL_COMPUTERSNEARME,
    CSIDL_PROFILES
  );


  ArrCSIDLNames: array[0..38] of string = (
    'DESKTOP',
    'INTERNET',
    'PROGRAMS',
    'CONTROLS',
    'PRINTERS',
    'PERSONAL',
    'FAVORITES',
    'STARTUP',
    'RECENT',
    'SENDTO',
    'BITBUCKET',
    'STARTMENU',
    'DESKTOPDIRECTORY',
    'DRIVES',
    'NETWORK',
    'NETHOOD',
    'FONTS',
    'TEMPLATES',
    'COMMON_STARTMENU',
    'COMMON_PROGRAMS',
    'COMMON_STARTUP',
    'COMMON_DESKTOPDIRECTORY',
    'APPDATA',
    'PRINTHOOD',
    'LOCAL_APPDATA',
    'ALTSTARTUP',
    'COMMON_ALTSTARTUP',
    'COMMON_FAVORITES',
    'INTERNET_CACHE',
    'COOKIES',
    'HISTORY',
    'PROFILE',
    'CONNECTIONS',
    'COMMON_MUSIC',
    'COMMON_PICTURES',
    'COMMON_VIDEO',
    'CDBURN_AREA',
    'COMPUTERSNEARME',
    'PROFILES'
  );
  {$endregion CSIDLs}

type

  TAppPathRec = record
    Name: string;
    FileName: string;
  end;

  TSpecialFolderRec = record
    CSIDL_Name: string;
    Path: string;
  end;

  TShellLinkRec = record
    Path: string;
    Args: string;
    Description: string;
    IconFile: string;
    IconIndex: integer;
    ShowCmd: integer;
    WorkingDirectory: string;
    Hotkey: WORD;
  end;

  TURLFileRec = record
    Name: string;
    URL: string;
    IconFile: string;
    IconIndex: integer;
  end;

  TURLFile = class(TObject)
  public
    URLFileRec: TURLFileRec;
    constructor Create;
    destructor Destroy; override;
    procedure ClearInfo;
    procedure GetURLFileInfo(URLFile: string);
    procedure SaveURLFile(fName: string);
    function GetInfoStr(Sep: string = '  '): string;
  end;

  TShellLink = class(TObject)
  public
    ShellLinkRec: TShellLinkRec;
    constructor Create;
    destructor Destroy; override;
    procedure ClearShellLinkRec;
    procedure GetLinkInfo(LinkFile: string);
    procedure SaveLinkFile(LinkFile: string);
    function GetInfoStr(Sep: string = '  '): string;
  end;


function DirectoryExists(const Name: string): Boolean;
function CreateEmptyFile(const fName: string): Boolean;
function GetFileVersionString(FileName, VerStr: string): string;
function CopyFile(const SrcFile, DestFile: string): Boolean;
function ShowFilePropertiesDialog(hOwner: HWND; const FileName: string): Boolean;
function GetDriveNumber(const DriveLetter: Char): integer;
function GetCdId(Letter: string): DWORD;
function SetFileAttr(fName: string; R, A, S, H: Boolean): Boolean;
function IsFileReadOnly(fName: string): Boolean;
function FileOnCdrom(fName: string): Boolean;
function GetFileText(fName: string): string;
function GetFileSizeEx(hFile: THandle; lpFileSize: PLargeInteger): BOOL; stdcall; external kernel32 name 'GetFileSizeEx';
function GetDesktopFolder: string;
function GetSpecialFolder(const CSIDL: integer; Default: string = ''): string;

function GetTmpFileName(Prefix: string = ''; Ext: string = ''; NameLen: Byte = 12): string;

{$IFDEF DELPHI2009_OR_BELOW}
function SHGetFolderPathA(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: LPSTR): HResult; stdcall;
function SHGetFolderPathW(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: LPWSTR): HResult; stdcall;
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: LPWSTR): HResult; stdcall;
{$ENDIF}

{$ENDIF} // MSWINDOWS


implementation

{$IFDEF MSWINDOWS}

uses
  JPL.Files,
  JPL.Win.System;


{$IFDEF DELPHI2009_OR_BELOW}
function SHGetFolderPath; external 'shell32.dll' name 'SHGetFolderPathW';
function SHGetFolderPathA; external 'shell32.dll' name 'SHGetFolderPathA';
function SHGetFolderPathW; external 'shell32.dll' name 'SHGetFolderPathW';
{$ENDIF}

function GetTmpFileName(Prefix: string = ''; Ext: string = ''; NameLen: Byte = 12): string;
var
  Dir, ShortName: string;
begin
  Dir := Rbs(TempDir);
  ShortName := GetUniqueFileName(Prefix, NameLen, '');
  ShortName := Copy(ShortName, 1, NameLen);
  Ext := Trim(Ext);
  if Ext <> '' then
  begin
    if Copy(Ext, 1, 1) <> '.' then Ext := '.' + Ext;
    ShortName := ChangeFileExt(ShortName, Ext);
  end;
  Result := Dir + '\' + ShortName;
end;

function GetSpecialFolder(const CSIDL: integer; Default: string = ''): string;
var
  Path: array[0..Max_Path] of Char;
begin
  if ShGetFolderPath(0, CSIDL, 0, SHGFP_TYPE_CURRENT, Path{%H-}) = S_OK then Result := Path
  else Result := Default;
end;


function GetDesktopFolder: string;
begin
  Result := GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, '');
end;

function GetFileText(fName: string): string;
var
  sl: TStringList;
begin
  Result := '';
  if not FileExists(fName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(fName);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function FileOnCdrom(fName: string): Boolean;
var
  dc: Char;
  i: integer;
begin
  fName := ExpandFileName(fName);
  Result := False;
  if fName = '' then Exit;
  dc := fName[1];
  for i := Ord('A') to Ord('Z') do
  begin
    if GetDriveType(PChar(dc + ':\')) = DRIVE_CDROM then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IsFileReadOnly(fName: string): Boolean;
begin
  Result := FileIsReadOnly(fName);
end;

{$WARN SYMBOL_PLATFORM OFF}
function SetFileAttr(fName: string; R, A, S, H: Boolean): Boolean;
var
  Attrs: integer;
begin
  Result := False;
  if not FileExists(fName) then Exit;
  Attrs := 0;
  if R then Attrs := Attrs or faReadOnly;
  if A then Attrs := Attrs or faArchive;
  if S then Attrs := Attrs or faSysFile;
  if H then Attrs := Attrs or faHidden;
  try
    if FileSetAttr(fName, Attrs) <> 0 then Exit;
  except
    Exit;
  end;
  Result := True;
end;
{$WARN SYMBOL_PLATFORM ON}

function GetCdId(Letter: string): DWORD;
var
  Name: string;
  VolName: array[0..99] of Char;
  CdId, x1, x2: DWORD;
begin
  Name := Letter + ':';
  GetVolumeInformation(
    PChar(Name),
    VolName{%H-},
    SizeOf(VolName),
    @CdId,
    x1{%H-},
    x2{%H-},
    nil,
    0
  );
  if UpperCase(VolName) = 'AUDIO CD' then Result := CdId
  else Result := 0;
end;

function GetDriveNumber(const DriveLetter: Char): integer;
var
  dwSerial, dwMaxComLen, dwSysFlags: DWORD;
  s: string;
begin
  Result := 0;
  s := DriveLetter + ':';
  if GetVolumeInformation(
    PChar(s),
    nil,
    0,
    @dwSerial,
    dwMaxComLen{%H-},
    dwSysFlags{%H-},
    nil,
    0
    ) then Result := dwSerial;
end;

function ShowFilePropertiesDialog(hOwner: HWND; const FileName: string): Boolean;
var
  sei: TShellExecuteInfo;
begin
  with sei do
  begin
    cbSize := SizeOf(sei);
    fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_INVOKEIDLIST or SEE_MASK_FLAG_NO_UI;
    wnd := hOwner;
    lpVerb := 'properties';
    lpFile := PChar(FileName);
    lpParameters := nil;
    lpDirectory := nil;
    nShow := 0;
    hInstApp := 0;
    lpIDList := nil;
  end;
  {$IFDEF DCC}Result := ShellExecuteEx(@sei);{$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNICODE}Result := ShellExecuteExW(@sei);{$ELSE}Result := ShellExecuteExA(@sei);{$ENDIF}
  {$ENDIF}
end;

function CopyFile(const SrcFile, DestFile: string): Boolean;
var
  fsSrc, fsDest: TFileStream;
  buffer: array[0..5119] of Byte; // 5 KB
  x: integer;
begin
  Result := True;
  try
    fsSrc := TFileStream.Create(SrcFile, fmOpenRead or fmShareDenyNone);
    fsDest := TFileStream.Create(DestFile, fmCreate);
    try
      repeat
        x := fsSrc.Read(buffer, SizeOf(buffer));
        fsDest.Write(buffer, x);
      until x <> SizeOf(buffer);
    finally
      fsSrc.Free;
      fsDest.Free;
    end;
  except
    Result := False;
  end;
end;

function GetFileVersionString(FileName, VerStr: string): string;
var
  Size, Handle: DWORD;
  Len: UINT;
  Buffer: PChar;
  Value: PChar;
  TransNo: PLongInt;
  SFInfo: string;
begin
  Result := '';

  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size = 0 then
  begin
    Result := '';
    Exit;
  end;
  Buffer := AllocMem(Size);

  try
    GetFileVersionInfo(PChar(FileName), 0, Size, Buffer);

    VerQueryValue(
      Buffer,
      PChar('VarFileInfo\Translation'),
      Pointer(TransNo),
      Len
      );

    SFInfo := Format(
      '%s%.4x%.4x%s%s%',
      ['\StringFileInfo\', LoWord(TransNo^), HiWord(Transno^), '\', VerStr]
      );

    if VerQueryValue(
      Buffer,
      PChar(SFInfo),
      Pointer(Value),
      Len
      ) then

      Result := Value;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer, Size);
  end;

end;


function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function CreateEmptyFile(const fName: string): Boolean;
var
  fs: TFileStream;
begin
  Result := True;
  try
    fs := TFileStream.Create(fName, fmCreate);
    fs.Free;
  except
    Result := False;
  end;
end;


{$region ' ---------------- TShellLink ------------------- '}

procedure TShellLink.ClearShellLinkRec;
begin
  ShellLinkRec.Path := '';
  ShellLinkRec.Args := '';
  ShellLinkRec.Description := '';
  ShellLinkRec.IconFile := '';
  ShellLinkRec.IconIndex := 0;
  ShellLinkRec.ShowCmd := 0;
  ShellLinkRec.WorkingDirectory := '';
  ShellLinkRec.Hotkey := 0;
end;

constructor TShellLink.Create;
begin
  inherited Create;
  ClearShellLinkRec;
end;

destructor TShellLink.Destroy;
begin
  inherited Destroy;
end;

procedure TShellLink.GetLinkInfo(LinkFile: string);
var
  Obj: IUnknown;
  Link: IShellLink;
  PF: IPersistFile;
  fName: WideString;
  x: integer;
  Buffer: array[0..511] of Char;
  {$IFDEF DCC}pwd: _WIN32_FIND_DATAW;{$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNICODE}pwd: _WIN32_FIND_DATAW;{$ELSE}pwd: _WIN32_FIND_DATAA;{$ENDIF}
  {$ENDIF}
  w: WORD;

  procedure CB;
  begin
    FillChar(Buffer, SizeOf(Buffer), 0);
  end;

begin
  ClearShellLinkRec;
  if not FileExists(LinkFile) then Exit;

  fName := WideString(LinkFile);

  CoInitialize(nil);
  Obj := CreateComObject(CLSID_ShellLink);
  Link := Obj as IShellLink;
  PF := Obj as IPersistFile;

  if PF.Load(PWChar(fName), 0) <> S_OK then Exit;


  CB;
  StrPCopy(Buffer{%H-}, fName{%H-});
  if Link.GetPath(Buffer, SizeOf(Buffer), pwd{%H-}, SLGP_RAWPATH) = S_OK then
    ShellLinkRec.Path := UnquoteStr(ExpandEnvironmentString(Buffer, False));

  CB;
  if Link.GetArguments(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.Args := UnquoteStr(Buffer);

  CB;
  if Link.GetDescription(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.Description := UnquoteStr(Buffer);

  CB;
  if Link.GetIconLocation(Buffer, SizeOf(Buffer), x{%H-}) = S_OK then
  begin
    ShellLinkRec.IconFile := UnquoteStr(ExpandEnvironmentString(Buffer, False));
    ShellLinkRec.IconIndex := x;
  end;

  CB;
  if Link.GetShowCmd(x) = S_OK then ShellLinkRec.ShowCmd := x;

  CB;
  if Link.GetWorkingDirectory(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.WorkingDirectory := UnquoteStr(ExpandEnvironmentString(Buffer, False));

  if Link.GetHotkey(w{%H-}) = S_OK then ShellLinkRec.Hotkey := w;
end;

procedure TShellLink.SaveLinkFile(LinkFile: string);
var
  Obj: IUnknown;
  Link: IShellLink;
  PF: IPersistFile;
  fName: WideString;
begin
  fName := WideString(LinkFile);

  CoInitialize(nil);
  Obj := CreateComObject(CLSID_ShellLink);
  Link := Obj as IShellLink;
  PF := Obj as IPersistFile;

  Link.SetPath(PChar(ShellLinkRec.Path));
  Link.SetArguments(PChar(ShellLinkRec.Args));
  Link.SetDescription(PChar(ShellLinkRec.Description));
  Link.SetIconLocation(PChar(ShellLinkRec.IconFile), ShellLinkRec.IconIndex);
  Link.SetShowCmd(ShellLinkRec.ShowCmd);
  Link.SetWorkingDirectory(PChar(ShellLinkRec.WorkingDirectory));
  Link.SetHotkey(ShellLinkRec.Hotkey);

  PF.Save(PWChar(fName), False);
end;

function TShellLink.GetInfoStr(Sep: string): string;
begin
  Result :=
    Sep + 'Path: ' + ShellLinkRec.Path + CRLF +
    Sep + 'Args: ' + ShellLinkRec.Args + CRLF +
    Sep + 'Description: ' + ShellLinkRec.Description + CRLF +
    Sep + 'Icon file: ' + ShellLinkRec.IconFile + CRLF +
    Sep + 'Icon index: ' + IntToStr(ShellLinkRec.IconIndex) + CRLF +
    Sep + 'Show cmd: ' + IntToStr(ShellLinkRec.ShowCmd) + CRLF +
    Sep + 'Working directory: ' + ShellLinkRec.WorkingDirectory;
end;
{$endregion TShellLink}



{$region ' -------------------- TURLFile --------------------- '}

procedure TURLFile.ClearInfo;
begin
  URLFileRec.Name := '';
  URLFileRec.URL := '';
  URLFileRec.IconFile := '';
  URLFileRec.IconIndex := -1;
end;

constructor TURLFile.Create;
begin
  inherited Create;
end;

destructor TURLFile.Destroy;
begin
  inherited Destroy;;
end;

procedure TURLFile.GetURLFileInfo(URLFile: string);
var
  sl: TStringList;
  i, xStartLineIndex, xp: integer;
  s, uName: string;
begin
  ClearInfo;
  if not FileExists(URLFile) then Exit;

  URLFileRec.Name := ExtractFileName(URLFile);
  sl := TStringList.Create;
  try

    sl.LoadFromFile(URLFile);

    xStartLineIndex := -1;
    for i := 0 to sl.Count - 1 do
      if UpperCase(sl[i]) = '[INTERNETSHORTCUT]' then
      begin
        xStartLineIndex := i;
        Break;
      end;

    if xStartLineIndex < 0 then Exit;

    for i := xStartLineIndex + 1 to sl.Count - 1 do
    begin
      s := sl[i];
      xp := Pos('=', s);
      if xp <= 0 then Continue;
      uName := UpperCase(Trim(Copy(s, 1, xp - 1)));
      s := Trim(Copy(s, xp + 1, Length(s)));
      if uName = 'URL' then URLFileRec.URL := s;
      if uName = 'ICONFILE' then URLFileRec.IconFile := s;
      if uName = 'ICONINDEX' then
      try
        URLFileRec.IconIndex := StrToInt(s);
      except
      end;
    end;

  finally
    sl.Free;
  end;

end;

procedure TURLFile.SaveURLFile(fName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('[InternetShortcut]');
    sl.Add('URL=' + URLFileRec.URL);
    if URLFileRec.IconFile <> '' then
    begin
      sl.Add('IconFile=' + URLFileRec.IconFile);
      if URLFileRec.IconIndex >= 0 then sl.Add('IconIndex=' + IntToStr(URLFileRec.IconIndex));
    end;
    sl.SaveToFile(fName);
  finally
    sl.Free;
  end;
end;

function TURLFile.GetInfoStr(Sep: string): string;
begin
  Result :=
    Sep + 'Name: ' + URLFileRec.Name + CRLF +
    Sep + 'URL=' + URLFileRec.URL;

  if URLFileRec.IconFile <> '' then
  begin
    Result := Result + CRLF + Sep + 'IconFile=' + URLFileRec.IconFile;
    if URLFileRec.IconIndex >= 0 then Result := Result + CRLF + Sep + 'IconIndex=' + IntToStr(URLFileRec.IconIndex);
  end;
end;
{$endregion TURLFile}


{$ENDIF} // MSWINDOWS


end.

