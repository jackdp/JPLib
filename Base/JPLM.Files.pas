unit JPLM.Files;

{
  Jacek Pazera
  http://www.pazera-software.com
}

{$mode objfpc}{$H+}
{$WARN 5044 off : Symbol "$1" is not portable}
interface

uses
  SysUtils,// Classes,
  MFPC.Classes.Streams,  dateutils,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF LINUX} BaseUnix, {$ENDIF}
  JPL.Strings;

type

  TFileDates = record
    Creation: TDateTime;
    LastWrite: TDateTime;
    LastAccess: TDateTime;
  end;

  TFileAttrs = record
    Attrs: LongInt;
    Hidden: Boolean;
    System: Boolean;
    ReadOnly: Boolean;
    Archive: Boolean;
    Directory: Boolean;
    SymLink: Boolean;
    Compressed: Boolean;
    Encrypted: Boolean;
  end;

  TFileInfoRec = record
    FullFileName: string;
    Directory: string;
    // directory + path delimiter
    Path: string;
    ShortFileName: string;
    BaseFileName: string;
    Extension: string;

    AttrsOK: Boolean;
    FileAttrs: TFileAttrs;

    StatOK: Boolean;
    DeviceNo: QWord;
    InodeNo: Cardinal;
    FileMode: Cardinal;
    HardLinks: QWord;
    OwnerUserID: Cardinal;
    OwnerGroupID: Cardinal;
    Size: Int64;
    BlockSize: Int64;
    Blocks: Int64;
    CreationTime: TDateTime;
    LastWriteTime: TDateTime;
    LastAccessTime: TDateTime;

    //ReadOnly: Boolean;
  end;

procedure ClearFileInfoRec(var fir: TFileInfoRec);
function GetFileInfoRec(const FileName: string; out fir: TFileInfoRec; bOnlyNames: Boolean = False): Boolean;
function FileSizeInt(const FileName: string): int64;
function TryGetFileLastWriteTime(const FileName: string; out LastWriteTime: TDateTime): Boolean;
{$IFDEF MSWINDOWS}
function FileGetCreationTime(const FileName: string): TDateTime;
function FileGetTimes(const FileName: string; out CreationTime, LastAccessTime, LastWriteTime: TDateTime): Boolean;
function FileAttributesToStr(const Attrs: LongInt; NotSetStr: string = '-'; bExtendedInfo: Boolean = False): string;
{$ENDIF}




implementation



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
  fs: TM_FileStream;
begin
  Result := 0;
  if not FileExists(FileName) then Exit;

  fs := TM_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
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
  Result := 0;
  try
    Result := _FileSizeInt(FileName);
  except
    on E: Exception do
    try
      {$IFDEF MSWINDOWS}
      Result := DSiFileSize(FileName);
      {$ELSE}
      if not GetFileInfoRec(FileName, fir, False) then Exit(-1);
      Result := fir.Size;
      {$ENDIF}
    except
      Result := -1;
    end;
  end;
end;

{$IFDEF MSWINDOWS}

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
    Result := UniversalTimeToLocal(Result);
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
    CreationTime := SystemTimeToDateTime(SysTime); //DONE: bad dates
    CreationTime := UniversalTimeToLocal(CreationTime);

    FileTimeToLocalFileTime(SearchRec.FindData.ftLastAccessTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    LastAccessTime := SystemTimeToDateTime(SysTime);
    LastAccessTime := UniversalTimeToLocal(LastAccessTime);

    FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    LastWriteTime := SystemTimeToDateTime(SysTime);
    LastWriteTime := UniversalTimeToLocal(LastWriteTime);

    Result := True;
  end
  else Result := False;
end;

function FileAttributesToStr(const Attrs: LongInt; NotSetStr: string = '-'; bExtendedInfo: Boolean = False): string;
begin
{
faReadOnly   = $00000001;
faHidden     = $00000002 platform;
faSysFile    = $00000004 platform;
faVolumeId   = $00000008 platform deprecated;
faDirectory  = $00000010;
faArchive    = $00000020;
faNormal     = $00000080;
faTemporary  = $00000100 platform;
faSymLink    = $00000400 platform;
faCompressed = $00000800 platform;
faEncrypted  = $00004000 platform;
faVirtual    = $00010000 platform;
faAnyFile    = $000001FF;
}
  Result := '';
  if (Attrs and faHidden) <> 0 then Result := Result + 'H' else Result := Result + NotSetStr;
  if (Attrs and faSysFile) <> 0 then Result := Result + 'S' else Result := Result + NotSetStr;
  if (Attrs and faReadOnly) <> 0 then Result := Result + 'R' else Result := Result + NotSetStr;
  if (Attrs and faArchive) <> 0 then Result := Result + 'A' else Result := Result + NotSetStr;

  if bExtendedInfo then
  begin
    if (Attrs and faSymLink) <> 0 then Result := Result + 'L' else Result := Result + NotSetStr;
    if (Attrs and faCompressed) <> 0 then Result := Result + 'C' else Result := Result + NotSetStr;
    if (Attrs and faEncrypted) <> 0 then Result := Result + 'E' else Result := Result + NotSetStr;
  end;
end;

{$ENDIF}

function TryGetFileLastWriteTime(const FileName: string; out LastWriteTime: TDateTime): Boolean;
var
{$IFDEF MSWINDOWS}
  SearchRec: TSearchRec;
  SysTime: SYSTEMTIME;
  FileTime: TFILETIME;
{$ELSE}
  st: BaseUnix.stat;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime, FileTime);
    FileTimeToSystemTime(FileTime, SysTime);
    LastWriteTime := UniversalTimeToLocal(SystemTimeToDateTime(SysTime));
    Result := True;
  end
  else Result := False;
  {$ELSE}
  if FpStat(FileName, st{%H-}) = 0 then
  begin
    LastWriteTime := UniversalTimeToLocal(FileDateToDateTime(st.st_mtime));
    Result := True;
  end
  else Result := False;
  {$ENDIF}
end;

procedure ClearFileInfoRec(var fir: TFileInfoRec);
begin
  fir.FullFileName := '';
  fir.Directory := '';
  fir.Path := '';
  fir.ShortFileName := '';
  fir.BaseFileName := '';
  fir.Extension := '';

  fir.AttrsOK := False;
  FillChar(fir.FileAttrs, SizeOf(fir.FileAttrs), 0);
  fir.FileAttrs.Attrs := -1;

  fir.StatOK := False;
  fir.DeviceNo := 0;
  fir.InodeNo := 0;
  fir.FileMode := 0;
  fir.HardLinks := 0;
  fir.OwnerUserID := 0;
  fir.OwnerGroupID := 0;
  fir.Size := 0;
  fir.BlockSize := 0;
  fir.Blocks := 0;
  fir.CreationTime := 0;
  fir.LastWriteTime := 0;
  fir.LastAccessTime := 0;
  //fir.ReadOnly := True;
end;

function GetFileInfoRec(const FileName: string; out fir: TFileInfoRec; bOnlyNames: Boolean = False): Boolean;
var
{$IFDEF LINUX}
  st: BaseUnix.stat;
{$ENDIF}
{$IFDEF MSWINDOWS}
  tc, ta, tw: TDateTime;
{$ENDIF}
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  ClearFileInfoRec(fir{%H-});
  fir.FullFileName := ExpandFileName(FileName);
  fir.Directory := ExtractFileDir(fir.FullFileName);
  fir.Path := ExtractFilePath(fir.FullFileName);
  fir.ShortFileName := ExtractFileName(FileName);
  fir.BaseFileName := ChangeFileExt(fir.ShortFileName, '');
  fir.Extension := GetFileExt(FileName, True);
  if bOnlyNames then Exit(True);

  {$IFDEF LINUX}
  fir.FileAttrs.ReadOnly := FileIsReadOnly(fir.FullFileName);
  fir.AttrsOK := True;
  {$ELSE}
  fir.FileAttrs.Attrs := SysUtils.FileGetAttr(FileName);
  if fir.FileAttrs.Attrs < 0 then fir.AttrsOK := False
  else
  begin
    fir.AttrsOK := True;
    fir.FileAttrs.System := (fir.FileAttrs.Attrs and faSysFile) <> 0;
    fir.FileAttrs.Hidden := (fir.FileAttrs.Attrs and faHidden) <> 0;
    fir.FileAttrs.ReadOnly := (fir.FileAttrs.Attrs and faReadOnly) <> 0;
    fir.FileAttrs.Archive := (fir.FileAttrs.Attrs and faArchive) <> 0;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  fir.Size := FileSizeInt(FileName);
  if FileGetTimes(FileName, tc, ta, tw) then
  begin
    fir.CreationTime := tc;
    fir.LastAccessTime := ta;
    fir.LastWriteTime := tw;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  if FpStat(FileName, st{%H-}) = 0 then
  begin
    fir.StatOK := True;
    fir.DeviceNo := st.st_dev;
    fir.InodeNo := st.st_ino;
    fir.FileMode := st.st_mode;
    fir.HardLinks := st.st_nlink;
    fir.OwnerUserID := st.st_uid;
    fir.OwnerGroupID := st.st_gid;
    fir.Size := st.st_size;
    fir.BlockSize := st.st_blksize;
    fir.Blocks := st.st_blocks;
    fir.CreationTime := UniversalTimeToLocal( FileDateToDateTime(st.st_ctime) );
    fir.LastAccessTime := UniversalTimeToLocal( FileDateToDateTime(st.st_atime) );
    fir.LastWriteTime := UniversalTimeToLocal( FileDateToDateTime(st.st_mtime) );
  end
  else fir.StatOK := False;
  {$ENDIF}

  Result := True;
end;




end.
