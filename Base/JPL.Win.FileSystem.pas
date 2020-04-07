unit JPL.Win.FileSystem;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface


{$IFDEF MSWINDOWS}
uses
  Windows, SysUtils, Classes, {$IFDEF DCC}IOUtils,{$ENDIF}
  //JP_Lists,
  JPL.Strings, JPL.Conversion,
  Dialogs,
  Registry, Menus,
  ShellAPI, ComObj, ShlObj, ActiveX;


const
  CSIDL_PROFILES = $003e;

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

//  {$region ' --- TFileSystem --- '}
//  TFileSystem = class(TObject)
//  public
//    AppPaths: array of TAppPathRec;
//    SpecialFolders: array of TSpecialFolderRec;
//    FileList: TStringList;
//    UserList: TStringList;
//    PathList: TStringList;
//    constructor Create;
//    destructor Destroy; override;
//    //procedure FillFileList(StartDir: string; FileMask: string = '*.*'; SubdirsCount: integer = 20; StatusProc: TFillFileListStatusProc = nil);
//    function GetEnvVar(EnvVarName: string): string;
//    procedure FillPathList;
//    procedure FillAppPaths(FileMustExists: Boolean = True);
//    function GetAppPathsStr: string;
//    function GetAppPathFileName(AppName: string): string;
//    function GetAppPathIndex(AppName: string): integer;
//    function GetSpecialFolder(CSIDL: integer): string;
//    procedure FillSpecialFolders;
//    function GetSpecialFoldersStr: string;
//    function GetFullFileName(ShortName: string): string;
//    function GetFileIcon(fName: string; IconIndex: integer = 0): HICON;
//  end;
//  {$endregion TFileSystem}




//////////////////////////////////////////////////////////////////////////////////////////
function DirectoryExists(const Name: string): Boolean;
function CreateEmptyFile(const fName: string): Boolean;
//function FileSizeInt(const FileName: string): int64;
//function FileSizeInt2(const FileName: string): int64;
//function FileSizeInt3(const FileName: string): int64;
//function FileSizeStr(FileName: string): string;
//function GetFileSizeString(FileSize: Int64): string;
//function GetFileSizeString64(FileSize: Int64): string;
//function DelFile(const FileName: string): Boolean;
function GetFileVersionString(FileName, VerStr: string): string;
function CopyFile(const SrcFile, DestFile: string): Boolean;
function ShowFilePropertiesDialog(hOwner: HWND; const FileName: string): Boolean;
function GetDriveNumber(const DriveLetter: Char): integer;
function GetCdId(Letter: string): DWORD;
function SetFileAttr(fName: string; R, A, S, H: Boolean): Boolean;
function IsFileReadOnly(fName: string): Boolean;
function FileOnCdrom(fName: string): Boolean;
function GetFileText(fName: string): string;
//function GetFilesSize(Dir: string; xSubdirs: integer = 50; StatusProc: TFillFileListStatusProc = nil): int64;
function GetFileSizeEx(hFile: THandle; lpFileSize: PLargeInteger): BOOL; stdcall; external kernel32 name 'GetFileSizeEx';
function GetDesktopFolder: string;
function GetSpecialFolder(const CSIDL: integer; Default: string = ''): string;

function GetTmpFileName(Prefix: string = ''; Ext: string = ''; NameLen: Byte = 12): string;

{$ENDIF} // MSWINDOWS


implementation
{$IFDEF MSWINDOWS}
uses
  JPL.Files,
  JPL.Win.System;



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
  if ShGetFolderPath(0, CSIDL, 0, SHGFP_TYPE_CURRENT, Path) = S_OK then Result := Path
  else Result := Default;
end;


function GetDesktopFolder: string;
//var
//  pidl: PItemIDList;
//  pMalloc: IMalloc;
//  hr: HRESULT;
//  Buffer: array[0..1023] of Char;
begin
  Result := GetSpecialFolder(CSIDL_DESKTOPDIRECTORY, '');
//  Result := '';
//  pMalloc := nil;
//  hr := SHGetMalloc(pMalloc);
//  if hr <> NOERROR then Exit;
//
//  hr := SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, pidl);
//  if hr <> S_OK then Exit;
//
//  FillChar(Buffer, SizeOf(Buffer), 0);
//  if SHGetPathFromIDList(pidl, Buffer) then Result := Buffer;
//
//  pMalloc.Free(pidl);
//  pMalloc._Release;

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

function GetCdId(Letter: string): DWORD;
var
  Name: string;
  VolName: array[0..99] of Char;
  CdId, x1, x2: DWORD;
begin
  Name := Letter + ':';
  GetVolumeInformation(
    PChar(Name),
    VolName,
    SizeOf(VolName),
    @CdId,
    x1,
    x2,
    nil,
    0
  );
  if UpperCase(VolName) = 'AUDIO CD' then
    Result := CdId
  else
    Result := 0;
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
    dwMaxComLen,
    dwSysFlags,
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

//{$hints off}
//function DelFile(const FileName: string): Boolean;
//var
//  w: WORD;
//begin
//  Result := True;
//  try
//
//    if not DeleteFile(FileName) then
//    try
//      w := 0 and not faReadOnly and not faSysFile and not faHidden;
//      FileSetAttr(FileName, w);
//      DeleteFile(FileName);
//    except
//      Result := not FileExists(FileName);
//    end;
//
//    Result := not FileExists(FileName);
//
//  except
//    Result := not FileExists(FileName);
//  end;
//end;
//{$hints on}

{$region ' --------------- File SIZE procs ----------------- '}


//function GetFilesSize(Dir: string; xSubdirs: integer = 50; StatusProc: TFillFileListStatusProc = nil): int64;
//var
//  sl: TStringList;
//  xSize: int64;
//  i: integer;
//begin
//
//  xSize := 0;
//  sl := TStringList.Create;
//  try
//    FillFileList('*.*', Dir, sl, xSubdirs, StatusProc);
//    for i := 0 to sl.Count - 1 do
//    begin
//      xSize := xSize + FileSizeInt(sl[i]);
//      if i mod 4 = 0 then
//        if Assigned(StatusProc) then
//          if not StatusProc then Break;
//    end;
//  finally
//    sl.Free;
//  end;
//  Result := xSize;
//
//end;

// FileSize routines moved to JPL.Files

//function DSiFileSize(const fileName: string): int64;
//var
//  fHandle: DWORD;
//begin
//  fHandle := CreateFile(PChar(fileName), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
//  if fHandle = INVALID_HANDLE_VALUE then Result := -1
//  else
//    try
//      Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
//    finally
//      CloseHandle(fHandle);
//    end;
//end;

//function FileSizeIntEX(const aFilename: String): Int64;
//var
//  info: TWin32FileAttributeData;
//begin
//  result := 0;
//
//  if not GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then EXIT;
//
//  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
//end;

//function FileSizeInt(const FileName: string): int64;
//{var
//  h: THandle;   }
//begin
//  Result := 0;
//  try
//    //Result := FileSizeIntEX(FileName);
//    Result := FileSizeInt3(FileName);
//  except
//    on E: Exception do
//    try
//      Result := DSiFileSize(FileName);
//    except
//    end;
//  end;
//end;

//function FileSizeInt2(const FileName: string): int64;
//var
//  h: THandle;
//begin
//  if FileExists(FileName) then
//  begin
//    h := CreateFile(PChar(FileName),
//      GENERIC_READ,
//      FILE_SHARE_READ,
//      nil,
//      OPEN_EXISTING,
//      FILE_ATTRIBUTE_NORMAL,
//      0);
//    GetFileSizeEx(h, @Result);
//    CloseHandle(h);
//  end
//  else
//    Result := 0;
//end;

//function FileSizeInt3(const FileName: string): int64;
//var
//  fs: TFileStream;
//begin
//  Result := 0;
//  if not FileExists(FileName) then Exit;
//
//  //try
//    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
//    try
//      Result := fs.Size;
//    finally
//      fs.Free;
//    end;
//
//  //except
//  //end;
//end;

{
function FileSizeInt2(const FileName: string): DWORD;
var
  f: file;
  i: DWORD;
  ofm: BYTE;
begin
  Result := 0;
  try
    AssignFile(f, FileName);
    ofm := FileMode;
    try
      FileMode := fmOpenRead;
      Reset(f, 1);
      i := DWORD(FileSize(f));
      CloseFile(f);
      Result := i;
    finally
      FileMode := ofm;
    end;
  except
    //on E: Exception do ShowMessage(E.Message);
  end;
end; }


//function FileSizeStr(FileName: string): string;
//begin
//  Result := GetFileSizeString(FileSizeInt(FileName));
//end;



// MOVED to JPL.Strings

//function GetFileSizeString(FileSize: Int64): string;
//var
//  fs: extended;
//  s: ShortString;
//  bNegative: Boolean;
//begin
//  bNegative := FileSize < 0;
//  if bNegative then FileSize := -FileSize;
//  Result := IntToStr(FileSize);
//  fs := FileSize;
//  if fs < 1024 then
//  begin
//    str(
//    fs: 2: 0, s);
//    Result := string(s) + ' bytes';
//  end
//  else if (fs >= 1024) and (fs < (1024 * 1024)) then
//  begin
//    fs := fs / 1024;
//    str(fs: 2: 2, s);
//    Result := string(s) + ' KB';
//  end
//  else if (fs >= 1024 * 1024) and (fs < (1024 * 1024 * 1024)) then
//  begin
//    fs := (fs / 1024) / 1024;
//    str(fs: 2: 2, s);
//    Result := string(s) + ' MB';
//  end
//  else
//  begin
//    fs := (fs / 1024) / 1024 / 1024;
//    str(fs: 2: 2, s);
//    Result := string(s) + ' GB';
//  end;
//  if bNegative then Result := '-' + Result;
//end;
//
//function GetFileSizeString64(FileSize: Int64): string;
//begin
//  Result := GetFileSizeString(FileSize);
//end;
{$endregion File SIZE procs}

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


{$region ' ------------------------- TFileSystem ----------------------- '}

//constructor TFileSystem.Create;
//begin
//  inherited Create;
//  FileList := TStringList.Create;
//  UserList := TStringList.Create;
//  PathList := TStringList.Create;
//
//  SetLength(AppPaths, 0);
//  SetLength(SpecialFolders, 0);
//  //FillPathList;
//end;

//destructor TFileSystem.Destroy;
//begin
//  FileList.Free;
//  UserList.Free;
//  PathList.Free;
//  inherited Destroy;
//end;

//procedure TFileSystem.FillAppPaths(FileMustExists: Boolean);
//var
//  Reg: TRegistry;
//  slKeys: TStringList;
//  i, xInd: integer;
//  sAppPaths, sFileName: string;
//begin
//  SetLength(AppPaths, 0);
//
//  sAppPaths := 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\';
//
//  Reg := TRegistry.Create;
//  slKeys := TStringList.Create;
//  try
//    with Reg do
//    begin
//
//      RootKey := HKEY_LOCAL_MACHINE;
//
//      if not OpenKey(sAppPaths, False) then Exit;
//      GetKeyNames(slKeys);
//      CloseKey;
//
//      for i := 0 to slKeys.Count - 1 do
//      begin
//        if not OpenKey(sAppPaths + slKeys[i], False) then Continue;
//        sFileName := ReadString('');
//        sFileName := ExpandEnvironmentString(sFileName, False);
//        if (ExtractFileDir(sFileName) = '') and Reg.ValueExists('Path') then sFileName := rbs(ReadString('Path')) + '\' + sFileName;
//        sFileName := UnquoteStr(sFileName);
//        if FileMustExists then
//          if not FileExists(sFileName) then
//          begin
//            CloseKey;
//            Continue;
//          end;
//        SetLength(AppPaths, Length(AppPaths) + 1);
//        xInd := Length(AppPaths) - 1;
//        AppPaths[xInd].Name := slKeys[i];
//        AppPaths[xInd].FileName := sFileName;
//        CloseKey;
//      end;
//
//    end;
//  finally
//    Reg.Free;
//    slKeys.Free;
//  end;
//end;

//procedure TFileSystem.FillFileList(StartDir: string; FileMask: string; SubdirsCount: integer; StatusProc: TFillFileListStatusProc);
//begin
//  FileList.Clear;
//  JP_Lists.FillFileList(FileMask, StartDir, FileList, SubdirsCount, StatusProc);
//end;

//procedure TFileSystem.FillPathList;
//var
//  Path: string;
//  i: integer;
//
//  function fs(s: string): string;
//  begin
//    Result := rbs(s);
//  end;
//
//begin
//  Path := GetEnvVar('PATH');
//  MakeList(Path, PathList, ';');
//  for i := 0 to PathList.Count - 1 do PathList[i] := fs(PathList[i]);
//end;

//function TFileSystem.GetAppPathFileName(AppName: string): string;
//var
//  i: integer;
//  UName: string;
//begin
//  Result := '';
//  UName := AnsiUpperCase(AppName);
//  for i := 0 to Length(AppPaths) - 1 do
//    if AnsiUpperCase(AppPaths[i].Name) = UName then
//    begin
//      Result := AppPaths[i].FileName;
//      Break;
//    end;
//end;

//function TFileSystem.GetAppPathIndex(AppName: string): integer;
//var
//  i: integer;
//  UName: string;
//begin
//  Result := -1;
//  UName := AnsiUpperCase(AppName);
//  for i := 0 to Length(AppPaths) - 1 do
//    if AnsiUpperCase(AppPaths[i].Name) = UName then
//    begin
//      Result := i;
//      Break;
//    end;
//end;

//function TFileSystem.GetAppPathsStr: string;
//var
//  i: integer;
//  s: string;
//begin
//  s := '';
//  for i := 0 to Length(AppPaths) - 1 do
//  begin
//    s :=
//      s + Pad(IntToStr(i + 1), 3, '0') + CRLF +
//      '  Name: ' + AppPaths[i].Name + CRLF +
//      '  File: ' + AppPaths[i].FileName + CRLF;
//  end;
//  Result := s;
//end;

//function TFileSystem.GetEnvVar(EnvVarName: string): string;
//{var
//  Buffer: array[0..1023] of Char;
//  dwX: DWORD;     }
//begin
//  Result := GetEnvironmentString(EnvVarName);
//{  FillChar(Buffer, SizeOf(Buffer), 0);
//  dwX := GetEnvironmentVariable(PChar(EnvVarName), Buffer, SizeOf(Buffer));
//  if dwX <> 0 then Result := Buffer
//  else Result := ''; }
//end;

//function TFileSystem.GetSpecialFolder(CSIDL: integer): string;
//var
//  pidl: PItemIDList;
//  pMalloc: IMalloc;
//  hr: HRESULT;
//  Buffer: array[0..1023] of Char;
//begin
//  Result := '';
//  pMalloc := nil;
//  hr := SHGetMalloc(pMalloc);
//  if hr <> NOERROR then Exit;
//
//  hr := SHGetSpecialFolderLocation(0, CSIDL, pidl);
//  if hr <> S_OK then Exit;
//
//  FillChar(Buffer, SizeOf(Buffer), 0);
//  if SHGetPathFromIDList(pidl, Buffer) then Result := Buffer;
//
//  pMalloc.Free(pidl);
//  pMalloc._Release;
//end;

//procedure TFileSystem.FillSpecialFolders;
//var
//  i, xInd: integer;
//begin
//  SetLength(SpecialFolders, 0);
//  for i := 0 to Length(ArrCSIDL) - 1 do
//  begin
//    SetLength(SpecialFolders, Length(SpecialFolders) + 1);
//    xInd := Length(SpecialFolders) - 1;
//    SpecialFolders[xInd].CSIDL_Name := ArrCSIDLNames[i];
//    SpecialFolders[xInd].Path := GetSpecialFolder(ArrCSIDL[i]);
//  end;
//end;

//function TFileSystem.GetSpecialFoldersStr: string;
//var
//  i: integer;
//  s: string;
//begin
//  s := '';
//  for i := 0 to Length(SpecialFolders) - 1 do
//  begin
//    s :=
//      s + Pad(IntToStr(i + 1), 2, '0') + '. ' +
//      SpecialFolders[i].CSIDL_Name + ': ' +
//      SpecialFolders[i].Path + CRLF;
//  end;
//  Result := s;
//end;

//function TFileSystem.GetFullFileName(ShortName: string): string;
//var
//  s: string;
//  i: integer;
//begin
//  Result := ShortName;
//
//  if FileExists(ExpandFileName(ShortName)) then
//  begin
//    Result := ExpandFileName(ShortName);
//    Exit;
//  end;
//
//  if PathList.Count = 0 then FillPathList;
//  for i := 0 to PathList.Count - 1 do
//  begin
//    s := PathList[i] + '\' + s;
//    if FileExists(s) then
//    begin
//      Result := s;
//      Exit;
//    end;
//  end;
//
//  if Length(AppPaths) = 0 then FillAppPaths;
//  s := GetAppPathFileName(ShortName);
//  if FileExists(s) then
//  begin
//    Result := s;
//    Exit;
//  end;
//
//  if Length(SpecialFolders) = 0 then FillSpecialFolders;
//  for i := 0 to Length(SpecialFolders) - 1 do
//  begin
//    s := SpecialFolders[i].Path + '\' + ShortName;
//    if FileExists(s) then
//    begin
//      Result := s;
//      Exit;
//    end;
//  end;
//
//end;

//function TFileSystem.GetFileIcon(fName: string; IconIndex: integer): HICON;
//var
//  W: WORD;
//  hi, hIconLarge, hIconSmall: HICON;
//  shfi: TSHFILEINFO;
//begin
//
//  if ExtractFileDir(fName) = '' then fName := GetFullFileName(fName);
//
//
//  ExtractIconEx(PChar(fName), IconIndex, hIconLarge, hIconSmall, 1);
//  hi := hIconSmall;
//
//  if hi = 0 then
//  begin
//    FillChar(shfi, SizeOf(shfi), 0);
//
//    shfi.iIcon := IconIndex;
//    SHGetFileInfo(
//      PChar(fName),
//      FILE_ATTRIBUTE_NORMAL,
//      shfi,
//      SizeOf(shfi),
//      SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_SMALLICON
//    );
//
//    hi := shfi.hIcon;
//  end;
//
//
//
//  if hi = 0 then
//  begin
//    if UpperCase(ExtractFileExt(fName)) <> '.EXE' then
//    hi := ExtractAssociatedIcon(hInstance, PChar(fName), W);
//  end;
//
//  Result := hi;
//end;
{$endregion TFileSystem}


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

  fName := LinkFile;

  CoInitialize(nil);
  Obj := CreateComObject(CLSID_ShellLink);
  Link := Obj as IShellLink;
  PF := Obj as IPersistFile;

  if PF.Load(PWChar(fName), 0) <> S_OK then Exit;


  CB;
  StrPCopy(Buffer, fName);
  if Link.GetPath(Buffer, SizeOf(Buffer), pwd, SLGP_RAWPATH) = S_OK then
    ShellLinkRec.Path := UnquoteStr(ExpandEnvironmentString(Buffer, False));

  CB;
  if Link.GetArguments(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.Args := UnquoteStr(Buffer);

  CB;
  if Link.GetDescription(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.Description := UnquoteStr(Buffer);

  CB;
  if Link.GetIconLocation(Buffer, SizeOf(Buffer), x) = S_OK then
  begin
    ShellLinkRec.IconFile := UnquoteStr(ExpandEnvironmentString(Buffer, False));
    ShellLinkRec.IconIndex := x;
  end;

  CB;
  if Link.GetShowCmd(x) = S_OK then ShellLinkRec.ShowCmd := x;

  CB;
  if Link.GetWorkingDirectory(Buffer, SizeOf(Buffer)) = S_OK then
    ShellLinkRec.WorkingDirectory := UnquoteStr(ExpandEnvironmentString(Buffer, False));

  if Link.GetHotkey(w) = S_OK then ShellLinkRec.Hotkey := w;
end;

procedure TShellLink.SaveLinkFile(LinkFile: string);
var
  Obj: IUnknown;
  Link: IShellLink;
  PF: IPersistFile;
  fName: WideString;
begin
  fName := LinkFile;

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

{$ENDIF} //MSWINDOWS


end.

