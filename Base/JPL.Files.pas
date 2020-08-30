unit JPL.Files;

{
  Jacek Pazera
  http://www.pazera-software.com
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5044 off : Symbol "$1" is not portable}
  {$modeswitch ADVANCEDRECORDS}
{$ENDIF}


interface

uses
  SysUtils, Classes, Types,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF LINUX}BaseUnix,{$ENDIF}
  JPL.Strings, JPL.Conversion, JPL.FileSearch;

const
  EMPTY_DATE = -127552;
  {$IFDEF FPC}
  INVALID_FILE_ATTRIBUTES  = DWORD(-1);
  {$ENDIF}
  UNC_PREFIX = '\\?\';

  {$IFDEF DELPHI2010_OR_BELOW}
  faCompressed  = $00000800;
  faEncrypted   = $00004000;
  faVirtual     = $00010000;
  {$ENDIF}


type

  TFileSystemItemType = (fsitFile, fsitDirectory, fsitUnknown);

  TFileNameRec = record
    FullFileName: string;         //  C:\dir\file.ext   |  /etc/dir/file.ext
    ShortFileName: string;        //  file.ext          |  file.ext
    BaseFileName: string;         //  file              |  file
    Directory: string;            //  C:\dir            |  /etc/dir
    Path: string;                 //  C:\dir\           |  /etc/dir/
    ExtensionWithDot: string;     //  .ext              |  .ext
    ExtensionWithoutDot: string;  //  ext               |  ext
    procedure AssignFileName(const FileName: string);
    procedure Clear;
    function AsString(Indent: string = ''): string;
  end;

  {$IFDEF MSWINDOWS}
  TFileAttributesRec = record
    ValidAttributes: Boolean;
    ReadOnly: Boolean;
    Hidden: Boolean;
    SysFile: Boolean;
    VolumeID: Boolean;
    Directory: Boolean;
    Archive: Boolean;
    Normal: Boolean;
    Temporary: Boolean;
    SymLink: Boolean;
    Compressed: Boolean;
    Encrypted: Boolean;
    Virtual: Boolean;
    AnyFile: Boolean;
    function ReadFileAttributes(const FileName: string): Boolean;
    procedure Clear;
    function AsString(Separator: string = ', '): string;
  end;
  {$ENDIF}


  TFileInfoRec = record
    FileNameRec: TFileNameRec;
    Exists: Boolean;

    {$IFDEF LINUX}
    StatOK: Boolean;
    DeviceNo: UInt64; // QWord;
    InodeNo: Cardinal;
    FileMode: Cardinal;
    HardLinks: UInt64; // QWord;
    OwnerUserID: Cardinal;
    OwnerGroupID: Cardinal;
    BlockSize: Int64;
    Blocks: Int64;
    {$ENDIF}

    Size: Int64;
    CreationTime: TDateTime;
    LastWriteTime: TDateTime;
    LastAccessTime: TDateTime;
    ReadOnly: Boolean;

    {$IFDEF MSWINDOWS}
    Attributes: TFileAttributesRec;
    {$ENDIF}

    function ReadFileInfo(const FileName: string): Boolean;
    procedure Clear;
    function AsString: string;
  end;


function FileSizeInt(const FileName: string): int64;
{$IFDEF MSWINDOWS}
function FileGetCreationTime(const FileName: string): TDateTime;
function FileGetTimes(const FileName: string; out CreationTime, LastAccessTime, LastWriteTime: TDateTime): Boolean;
{$ENDIF}

function DelFile(const FileName: string): Boolean;
function GetIncFileName(const fName: string; NumPrefix: string = '_'; xpad: integer = 3): string;
function GetUniqueFileName(Prefix: string = ''; Len: BYTE = 10; Ext: string = ''): string;

function GetFiles(const Directory: string; SearchPattern: string = '*'; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): TStringDynArray;
function GetDirectories(const StartDir: string; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): TStringDynArray;
function SubdirectoryCount(const Directory: string; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): integer;

function DirectoryExistsEx(const Dir: string; AcceptSymLinks: Boolean = True): Boolean;
function AddUncPrefix(const FileOrDirectoryName: string): string;

function GetFileSystemItemType(const FileOrDirectoryName: string): TFileSystemItemType;


implementation


function GetFileSystemItemType(const FileOrDirectoryName: string): TFileSystemItemType;
begin
  if DirectoryExists(FileOrDirectoryName) then Result := fsitDirectory
  else if FileExists(FileOrDirectoryName) then Result := fsitFile
  else Result := fsitUnknown;
end;

function AddUncPrefix(const FileOrDirectoryName: string): string;
begin
  if Copy(FileOrDirectoryName, 1, 2) <> '\\' then Result := UNC_PREFIX + FileOrDirectoryName
  else Result := FileOrDirectoryName;
end;

function DirectoryExistsEx(const Dir: string; AcceptSymLinks: Boolean = True): Boolean;
var
  SR: SysUtils.TSearchRec;

  function IsDirectory: Boolean;
  begin
    Result := ( (SR.Attr and faDirectory) <> 0 ) and (SR.Name <> '.') and (SR.Name <> '..');
    if Result and (not AcceptSymLinks) then Result := ( Result and ( (SR.Attr and faSymLink) = 0) );
  end;

begin
  Result := False;
  {$IFDEF MSWINDOWS}
  if FindFirst(AddUncPrefix(Dir), faAnyFile, SR) = 0 then
  {$ELSE}
  if FindFirst(Dir, faAnyFile, SR) = 0 then
  {$ENDIF}
  try
    Result := IsDirectory;
  finally
    SysUtils.FindClose(SR);
  end;
end;

function GetFiles(const Directory: string; SearchPattern: string = '*'; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): TStringDynArray;
var
  sl: TStringList;
  i: integer;
begin
  SetLength(Result, 0);
  sl := TStringList.Create;
  try
    JPGetFileList(SearchPattern, Directory, sl, RecurseDepth, AcceptSymLinks, AcceptSymLinks, nil, nil, nil);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := sl[i];
    end;
  finally
    sl.Free;
  end;
end;

function GetDirectories(const StartDir: string; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): TStringDynArray;
var
  sl: TStringList;
  i: integer;
begin
  SetLength(Result, 0);
  sl := TStringList.Create;
  try
    JPGetDirectoryList(StartDir, sl, AcceptSymLinks, RecurseDepth, nil);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := sl[i];
    end;
  finally
    sl.Free;
  end;
end;

function SubdirectoryCount(const Directory: string; RecurseDepth: Word = 0; AcceptSymLinks: Boolean = True): integer;
begin
  Result := Length(GetDirectories(Directory, RecurseDepth, AcceptSymLinks));
end;

function GetIncFileName(const fName: string; NumPrefix: string = '_'; xpad: integer = 3): string;
var
  i: integer;
  ShortName, Ext, fOut: string;
begin
  Result := '';

  if not FileExists(fName) then
  begin
    Result := fName;
    Exit;
  end;

  Ext := ExtractFileExt(fName);
  ShortName := ChangeFileExt(fName, '');

  for i := 1 to 100000 do
  begin
    fOut := ShortName + NumPrefix + Pad(IntToStr(i), xpad, '0') + Ext;
    if not FileExists(fOut) then
    begin
      Result := fOut;
      Break;
    end;
  end;
end;

function GetUniqueFileName(Prefix: string = ''; Len: BYTE = 10; Ext: string = ''): string;
var
  x: integer;
  bt: BYTE;
  s: string;
begin
  x := 0;
  s := '';
  Randomize;

  while x < Len do
  begin
    bt := Random(254) + 1;
    if not ( (bt in [48..57]) or (x in [65..90]) or (x in [97..122]) ) then Continue;
    Inc(x);
    s := s + Char(bt);
  end;

  s := Prefix + s;
  if (Ext <> '') and (Ext[1] <> '.') then Ext := '.' + Ext;
  Result := ChangeFileExt(s, Ext);
end;

{$HINTS OFF}
function DelFile(const FileName: string): Boolean;
var
  w: WORD;
begin
  Result := True;
  try

    {$IFDEF MSWINDOWS}
    if not SysUtils.DeleteFile(FileName) then
    try
      {$WARNINGS OFF}
      w := 0 and not faReadOnly and not faSysFile and not faHidden;
      FileSetAttr(FileName, w);
      {$WARNINGS ON}
      SysUtils.DeleteFile(FileName);
    except
      Result := not FileExists(FileName);
    end;
    {$ELSE}
    SysUtils.DeleteFile(FileName);
    {$ENDIF}

    Result := not FileExists(FileName);

  except
    Result := not FileExists(FileName);
  end;
end;
{$HINTS ON}


{$IFDEF MSWINDOWS}
// From DSiWin32.pas by Primož Gabrijelčič: https://github.com/gabr42/OmniThreadLibrary/tree/master/src
function DSiFileSize(const fileName: string): int64;
var
  fHandle: THandle;
begin
  fHandle := CreateFile(
    PChar(fileName), 0,
    FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0
  );
  if fHandle = INVALID_HANDLE_VALUE then Result := -1
  else
  try
    Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
  finally
    CloseHandle(fHandle);
  end;
end;
{$ENDIF}

function _FileSizeInt(const FileName: string): int64;
var
  fs: TFileStream;
begin
  Result := 0;
  if not FileExists(FileName) then Exit;

  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := fs.Size;
  finally
    fs.Free;
  end;
end;

function FileSizeInt(const FileName: string): int64;
{$IFNDEF MSWINDOWS}
var
  fir: TFileInfoRec;
{$ENDIF}
begin
  //Result := 0;
  try
    Result := _FileSizeInt(FileName);
  except
    on E: Exception do
    try
      {$IFDEF MSWINDOWS}
      Result := DSiFileSize(FileName);
      {$ELSE}
      //if not GetFileInfoRec(FileName, fir, False) then Exit(-1);
      if not fir.ReadFileInfo(FileName) then Exit(-1);
      Result := fir.Size;
      {$ENDIF}
    except
      Result := -1;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
// http://forum.lazarus.freepascal.org/index.php/topic,6705.0.html
function FileGetCreationTime(const FileName: string): TDateTime;
var
  SearchRec: TSearchRec;
  SysTime: SYSTEMTIME;
  FileTime: TFILETIME;
begin
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    FileTimeToLocalFileTime(SearchRec.FindData.ftCreationTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    Result := SystemTimeToDateTime(SysTime);
    SysUtils.FindClose(SearchRec);
  end
  else Result := 0;
end;

function FileGetTimes(const FileName: string; out CreationTime, LastAccessTime, LastWriteTime: TDateTime): Boolean;
var
  SearchRec: TSearchRec;
  SysTime: SYSTEMTIME;
  FileTime: TFILETIME;
begin
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    FileTimeToLocalFileTime(SearchRec.FindData.ftCreationTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    CreationTime := SystemTimeToDateTime(SysTime); //TODO: bad dates

    FileTimeToLocalFileTime(SearchRec.FindData.ftLastAccessTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    LastAccessTime := SystemTimeToDateTime(SysTime);

    FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    LastWriteTime := SystemTimeToDateTime(SysTime);

    SysUtils.FindClose(SearchRec);
    Result := True;
  end
  else Result := False;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}




{$region '                   TFileAttributesRec                      '}

{$IFDEF MSWINDOWS}

{$WARN SYMBOL_PLATFORM OFF}
function TFileAttributesRec.ReadFileAttributes(const FileName: string): Boolean;
var
  x: DWORD;
begin
  Clear;
  Result := False;
  x := GetFileAttributes(PChar(FileName));
  if x = INVALID_FILE_ATTRIBUTES then Exit;

  ValidAttributes := True;
  Result := True;

  {$IFDEF FPC}{$WARN 5044 off : Symbol "$1" is not portable}{$ENDIF}
  ReadOnly := x and faReadOnly <> 0;
  Hidden := x and faHidden <> 0;
  SysFile := x and faSysFile <> 0;
  {$WARN SYMBOL_DEPRECATED OFF}
  VolumeID := x and faVolumeId <> 0;
  {$WARN SYMBOL_DEPRECATED ON}
  Directory := x and faDirectory <> 0;
  Archive := x and faArchive <> 0;
  Normal := x and faNormal <> 0;
  Temporary := x and faTemporary <> 0;
  SymLink := x and faSymLink <> 0;
  Compressed := x and faCompressed <> 0;
  Encrypted := x and faEncrypted <> 0;
  Virtual := x and faVirtual <> 0;
  AnyFile := x and faAnyFile <> 0;
  {$IFDEF FPC}{$WARN 5044 on : Symbol "$1" is not portable}{$ENDIF}
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TFileAttributesRec.Clear;
begin
  ValidAttributes := False;
  ReadOnly := False;
  Hidden := False;
  SysFile := False;
  VolumeID := False;
  Directory := False;
  Archive := False;
  Normal := False;
  Temporary := False;
  SymLink := False;
  Compressed := False;
  Encrypted := False;
  Virtual := False;
  AnyFile := False;
end;

function TFileAttributesRec.AsString(Separator: string = ', '): string;
begin
  Result := '';
  if not ValidAttributes then Exit;
  if ReadOnly then Result := Result + 'ReadOnly' + Separator;
  if Hidden then Result := Result + 'Hidden' + Separator;
  if SysFile then Result := Result + 'SysFile' + Separator;
  if VolumeID then Result := Result + 'VolumeID' + Separator;
  if Directory then Result := Result + 'Directory' + Separator;
  if Archive then Result := Result + 'Archive' + Separator;
  if Normal then Result := Result + 'Normal' + Separator;
  if Temporary then Result := Result + 'Temporary' + Separator;
  if SymLink then Result := Result + 'SymLink' + Separator;
  if Compressed then Result := Result + 'Compressed' + Separator;
  if Encrypted then Result := Result + 'Encrypted' + Separator;
  if Virtual then Result := Result + 'Virtual' + Separator;
  if AnyFile then Result := Result + 'AnyFile' + Separator;

  Result := TrimFromEnd(Result, Separator);
end;
{$ENDIF} // MSWINDOWS

{$endregion TFileAttributesRec}



{$region '                   TFileNameRec                      '}
procedure TFileNameRec.AssignFileName(const FileName: string);
begin
  FullFileName := ExpandFileName(FileName);
  Directory := ExtractFileDir(FullFileName);
  Path := ExtractFilePath(FullFileName);
  ShortFileName := ExtractFileName(FileName);
  BaseFileName := ChangeFileExt(ShortFileName, '');
  ExtensionWithoutDot := GetFileExt(ShortFileName, True);
  if ExtensionWithoutDot <> '' then ExtensionWithDot := '.' + ExtensionWithoutDot
  else ExtensionWithoutDot := '';
end;

procedure TFileNameRec.Clear;
begin
  FullFileName := '';
  ShortFileName := '';
  BaseFileName := '';
  Directory := '';
  Path := '';
  ExtensionWithDot := '';
  ExtensionWithoutDot := '';
end;

function TFileNameRec.AsString(Indent: string = ''): string;
begin
  Result :=
    Indent + 'FullFileName: ' + FullFileName + ENDL +
    Indent + 'ShortFileName: ' + ShortFileName + ENDL +
    Indent + 'BaseFileName: ' + BaseFileName + ENDL +
    Indent + 'Directory: ' + Directory + ENDL +
    Indent + 'Path: ' + Path + ENDL +
    Indent + 'ExtensionWithDot: ' + ExtensionWithDot + ENDL +
    Indent + 'ExtensionWithoutDot: ' + ExtensionWithoutDot;
end;
{$endregion TFileNameRec}



{$region '                   TFileInfoRec                    '}

procedure TFileInfoRec.Clear;
begin
  FileNameRec.Clear;
  Exists := False;

  {$IFDEF LINUX}
  StatOK := False;
  DeviceNo := 0;
  InodeNo := 0;
  FileMode := 0;
  HardLinks := 0;
  OwnerUserID := 0;
  OwnerGroupID := 0;
  BlockSize := 0;
  Blocks := 0;
  {$ENDIF}

  Size := 0;
  CreationTime := EMPTY_DATE;
  LastWriteTime := EMPTY_DATE;
  LastAccessTime := EMPTY_DATE;
  ReadOnly := False;

  {$IFDEF MSWINDOWS}
  Attributes.ValidAttributes := False;
  {$ENDIF}
end;

function TFileInfoRec.ReadFileInfo(const FileName: string): Boolean;
var
{$IFDEF LINUX}
  st: BaseUnix.stat;
{$ENDIF}
{$IFDEF MSWINDOWS}
  tc, ta, tw: TDateTime;
{$ENDIF}
  FullName: string;
begin
  Result := False;
  Clear;
  {$IFDEF MSWINDOWS}
  Attributes.ValidAttributes := False;
  {$ENDIF}
  FileNameRec.Clear;

  if not FileExists(FileName) then Exit;
  Exists := True;

  FileNameRec.AssignFileName(FileName);
  FullName := FileNameRec.FullFileName;

  ReadOnly := FileIsReadOnly(FullName);

  {$IFDEF MSWINDOWS}
  Size := FileSizeInt(FullName);
  if FileGetTimes(FullName, tc, ta, tw) then
  begin
    CreationTime := tc;
    LastAccessTime := ta;
    LastWriteTime := tw;
  end;
  Attributes.ReadFileAttributes(FullName);
  {$ENDIF}


  {$IFDEF LINUX}
  if FpStat(FullName, st{%H-}) = 0 then
  begin
    StatOK := True;
    DeviceNo := st.st_dev;
    InodeNo := st.st_ino;
    FileMode := st.st_mode;
    HardLinks := st.st_nlink;
    OwnerUserID := st.st_uid;
    OwnerGroupID := st.st_gid;
    Size := st.st_size;
    BlockSize := st.st_blksize;
    Blocks := st.st_blocks;
    CreationTime := FileDateToDateTime(st.st_ctime);
    LastAccessTime := FileDateToDateTime(st.st_atime);
    LastWriteTime := FileDateToDateTime(st.st_mtime);
  end
  else StatOK := False;
  {$ENDIF}

  Result := True;
end;

function TFileInfoRec.AsString: string;
begin
  Result := 'File exists: ' + BoolToStr(Exists) + ENDL;
  Result := Result + 'FileNameRec' + ENDL + FileNameRec.AsString('  ');
  {$IFDEF MSWINDOWS}
  Result := Result + ENDL + 'File Attributes: ' + Attributes.AsString(', ');
  {$ENDIF}
  Result := Result + ENDL +
    'Size: ' + itos(Size) + ' bytes' + ENDL +
    'CreationTime: ' + DateTimeToStr(CreationTime) + ENDL +
    'LastWriteTime: ' + DateTimeToStr(LastWriteTime) + ENDL +
    'LastAccessTime: ' + DateTimeToStr(LastAccessTime);
  {$IFDEF LINUX}
  Result := Result + ENDL + 'StatOK: ' + BoolToStr(StatOK);
  if StatOK then
    Result := Result + ENDL +
      'DeviceNo: ' + itos(DeviceNo) + ENDL +
      'InodeNo: ' + itos(InodeNo) + ENDL +
      'FileMode: ' + itos(FileMode) + ENDL +
      'HardLinks: ' + itos(HardLinks) + ENDL +
      'OwnerUserID: ' + itos(OwnerUserID) + ENDL +
      'OwnerGroupID: ' + itos(OwnerGroupID) + ENDL +
      'BlockSize: ' + itos(BlockSize) + ENDL +
      'Blocks: ' + itos(Blocks);
  {$ENDIF}
  Result := Result + ENDL + 'ReadOnly: ' + BoolToStr(ReadOnly);
end;

{$endregion TFileInfoRec}

end.
