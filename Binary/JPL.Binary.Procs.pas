unit JPL.Binary.Procs;

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes,
  JPL.Binary.Types, JPL.Binary.Pe, JPL.Binary.Mach.Types
  ;



function GetBitsInfo(fName: string; var BitsInfo: TBitsInfo; NotStandardChar: Char = '.'): Boolean; overload;
function GetBitsInfo(Stream: TStream; var BitsInfo: TBitsInfo; NotStandardChar: Char = '.'): Boolean; overload;
procedure ClearBitsInfo(var BitsInfo: TBitsInfo);

function GetWinExeBinType(fName: string): integer;
function GetBinMagicStr(fName: string): string; overload;
function GetBinMagicStr(Stream: TStream): string; overload;
function GetExecutableBinType(fName: string; var FileTypeStr: string): integer;
function GetBinFileTypeStr(fName: string): string; // returns: MZ, ELF, CAFEBABE or empty string


function SwapBytes(Value: Word): Word; overload;
function SwapBytes(Value: DWORD): DWORD; overload;
function SwapBytes(Value: Integer): Integer; overload;
function SwapBytes(Value: UInt64): UInt64; overload;

function SwapInt(Value: Integer): Integer;
function SwapWord(Value: Word): Word;
function SwapDword(Value: DWORD): DWORD;
function SwapUInt64(Value: UInt64): UInt64;

function FileSizeInt(const FileName: string): int64;


implementation


uses
  JPL.Binary.Elf, JPL.Binary.MachFat;


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
begin
  Result := 0;
  try
    Result := _FileSizeInt(FileName);
  except
    on E: Exception do
    try
      Result := DSiFileSize(FileName);
    except
      { TODO : Mniam, mniam... Eating exceptions is not a good practice! }
    end;
  end;
end;

function SwapWord(Value: Word): Word;
type
  Bytes = packed array[0..1] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[1];
  Bytes(Result)[1]:= Bytes(Value)[0];
end;

function SwapDword(Value: DWORD): DWORD;
type
  Bytes = packed array[0..3] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[3];
  Bytes(Result)[1]:= Bytes(Value)[2];
  Bytes(Result)[2]:= Bytes(Value)[1];
  Bytes(Result)[3]:= Bytes(Value)[0];
end;

function SwapInt(Value: Integer): Integer;
type
  Bytes = packed array[0..3] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[3];
  Bytes(Result)[1]:= Bytes(Value)[2];
  Bytes(Result)[2]:= Bytes(Value)[1];
  Bytes(Result)[3]:= Bytes(Value)[0];
end;

function SwapUInt64(Value: UInt64): UInt64;
begin
  Result := SwapDword(LongWord(Value));
  Result := (Result shl 32) or SwapDword(LongWord(Value shr 32));
end;


function SwapBytes(Value: Word): Word;
begin
  Result := SwapWord(Value);
end;

function SwapBytes(Value: DWORD): DWORD;
begin
  Result := SwapDword(Value);
end;

function SwapBytes(Value: Integer): Integer;
begin
  Result := SwapInt(Value);
end;

function SwapBytes(Value: UInt64): UInt64;
begin
  Result := SwapUInt64(Value);
end;



function GetBinFileTypeStr(fName: string): string;
var
  fs: TFileStream;
  Buf: array[0..3] of Byte;
  s: string;
  x, i: integer;
begin
  Result := '';
  if not FileExists(fName) then Exit;

  try

    fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
    try
      if fs.Size < 4 then Exit;
      x := fs.Read(Buf, 4);
      if x <> 4 then Exit;

      s := '';
      for i := 0 to Length(Buf) - 1 do s := s + Chr(Buf[i]);
      s := Trim(s);
      if Copy(s, 1, 2) = 'MZ' then s := 'MZ'
      else if Copy(s, 2, 4) = 'ELF' then s := 'ELF';
      Result := s;
    finally
      fs.Free;
    end;

  except
  end;
end;

procedure ClearBitsInfo(var BitsInfo: TBitsInfo);
begin
  BitsInfo.B1 := 0;
  BitsInfo.B2 := 0;
  BitsInfo.B3 := 0;
  BitsInfo.B4 := 0;
  BitsInfo.BitsStr := '';
end;

function GetBitsInfo(Stream: TStream; var BitsInfo: TBitsInfo; NotStandardChar: Char = '.'): Boolean; overload;
var
  Arr: array[0..3] of Byte;
  c1, c2, c3, c4: Char;
  OldPos: Int64;

  function FixChar(c: Char): Char;
  begin
    if (Ord(c) < 33) or (Ord(c) > 126) then c := NotStandardChar;
    Result := c;
  end;

begin
  ClearBitsInfo(BitsInfo);
  Result := False;
  if not Assigned(Stream) then Exit;

  OldPos := Stream.Position;

  if Stream.Size < 4 then Exit;
  Stream.Position := 0;
  Stream.ReadBuffer(Arr{%H-}, Length(Arr));
  Stream.Position := OldPos;

  BitsInfo.B1 := Arr[0];
  BitsInfo.B2 := Arr[1];
  BitsInfo.B3 := Arr[2];
  BitsInfo.B4 := Arr[3];
  c1 := FixChar(Chr(BitsInfo.B1));
  c2 := FixChar(Chr(BitsInfo.B2));
  c3 := FixChar(Chr(BitsInfo.B3));
  c4 := FixChar(Chr(BitsInfo.B4));
  BitsInfo.BitsStr := c1 + c2 + c3 + c4;

  Result := True;
end;

function GetBitsInfo(fName: string; var BitsInfo: TBitsInfo; NotStandardChar: Char = '.'): Boolean;
var
  fs: TFileStream;
begin
  ClearBitsInfo(BitsInfo);
  Result := False;
  if not FileExists(fName) then Exit;

  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := GetBitsInfo(fs, BitsInfo, NotStandardChar);
  finally
    fs.Free;
  end;

end;

function GetExecutableBinType(fName: string; var FileTypeStr: string): integer;
var
  BinType: integer;
  BinTypeStr: string;
  PeFile: TPeFile;
  ElfFile: TElfFile;
  MachFatFile: TMachFatFile;
begin
  FileTypeStr := UNKNOWN_EXECUTABLE;
  BinType := BIN_UNKNOWN;
  BinTypeStr := UpperCase(GetBinMagicStr(fName));

  if BinTypeStr = 'MZ' then
  begin
    PeFile := TPeFile.Create;
    try
      PeFile.FileName := fName;
      PeFile.ReadFileInfo;

      case PeFile.Bits of
        16: BinType := BIN_WIN16;
        32: BinType := BIN_WIN32;
        64: BinType := BIN_WIN64;
      end;

      FileTypeStr := PeFile.InfoStr;

    finally
      PeFile.Free;
    end;
  end

  else if BinTypeStr = 'ELF' then
  begin
    ElfFile := TElfFile.Create;
    try
      ElfFile.FileName := fName;
      ElfFile.ReadFileInfo;
      if ElfFile.IsValidElfFile then
      begin
        case ElfFile.Bits of
          16: BinType := BIN_UNIX16;
          32: BinType := BIN_UNIX32;
          64: BinType := BIN_UNIX64;
        end;
        FileTypeStr := ElfFile.InfoStr;
      end;
    finally
      ElfFile.Free;
    end;
  end

  // CAFE BABE / BEBA FECA
  else if BinTypeStr = FAT_MAGIC_STR then
  begin
    BinType := BIN_MACH_FAT;
    FileTypeStr := 'CAFE BABE Little endian'; // 'Mac OS X - Mach-Fat';
  end
  else if BinTypeStr = FAT_CIGAM_STR then
  begin
    BinType := BIN_MACH_FAT;
    FileTypeStr := 'CAFE BABE Big endian';  // 'Mac OS X - Mach-Fat CIGAM';
    MachFatFile := TMachFatFile.Create;
    try
      MachFatFile.FileName := fName;
      MachFatFile.SearchUpxInfo := False;
      MachFatFile.ReadFileInfo;
      if MachFatFile.IsValidMachFatFile then
      begin
        BinType := BIN_MACHO_FAT_MULTIARCH;
        FileTypeStr := 'Mac OS X - Mach-Fat multiarch';
      end;
    finally
      MachFatFile.Free;
    end;
  end

  // Mac OS X
  else if BinTypeStr = MH_MAGIC_STR then
  begin
    BinType := BIN_MACHO_32_LITTLE_ENDIAN;
    FileTypeStr := 'Mac OS X - Mach-O 32 bit (Little endian)';
  end
  else if BinTypeStr = MH_CIGAM_STR then
  begin
    BinType := BIN_MACHO_32_BIG_ENDIAN;
    FileTypeStr := 'Mac OS X - Mach-O 32 bit (Big endian)';
  end
  else if BinTypeStr = MH_MAGIC_64_STR then
  begin
    BinType := BIN_MACHO_64_LITTLE_ENDIAN;
    FileTypeStr := 'Mac OS X - Mach-O 64 bit (Little endian)';
  end
  else if BinTypeStr = MH_CIGAM_64_STR then
  begin
    BinType := BIN_MACHO_64_BIG_ENDIAN;
    FileTypeStr := 'Mac OS X - Mach-O 64 bit (Big endian)';
  end

//  else if BinTypeStr = 'MacOS' then
//  begin
//    BinType := BIN_MACOS;
//    FileTypeStr := 'Mac OS CFFA EDFE';
//  end

  else

  begin
    BinType := GetWinExeBinType(fName);
    //MB(BinType.ToString);
  end;
                             //WinMsg(BinTypeStr, FileTypeStr);
  Result := BinType;
end;

function GetWinExeBinType(fName: string): integer;
var
  bt: Cardinal;
begin
  {$IFDEF DCC}
  if GetBinaryType(PWideChar(fName), bt) then
  {$ELSE}
  if GetBinaryType(PChar(fName), bt{%H-}) then
  {$ENDIF}
  begin
    case bt of
      BIN_WIN32: Result := BIN_WIN32;
      BIN_WIN64: Result := BIN_WIN64;
      //BIN_DOS: Result := BIN_DOS;
      BIN_WIN16: Result := BIN_WIN16;
    else
      Result := BIN_UNKNOWN;
    end;
  end
  else Result := BIN_UNKNOWN;
end;

function GetBinMagicStr(Stream: TStream): string; overload;
var
  Buffer: array[0..3] of Byte;
  Magic: DWORD;
  BufferSize: integer;
  s: string;
  x, i: integer;
  OldPos: Int64;
begin
  Result := '';
  if not Assigned(Stream) then Exit;

  BufferSize := SizeOf(Buffer);
  if Stream.Size < BufferSize then Exit;

  OldPos := Stream.Position;

  Stream.Position := 0;
  x := Stream.Read(Buffer{%H-}, BufferSize);
  Stream.Position := 0;
  Stream.Read(Magic{%H-}, SizeOf(Magic));

  Stream.Position := OldPos;
  if x <> BufferSize then Exit; // BufferSize = SizeOf(Magic)

                                                //MB(IntToHex(Magic, 8));
  if Magic = FAT_MAGIC then s := FAT_MAGIC_STR // IntToHex(FAT_MAGIC, 8)
  else if Magic = FAT_CIGAM then s := FAT_CIGAM_STR // IntToHex(FAT_CIGAM, 8)
  else if Magic = MH_MAGIC then s := MH_MAGIC_STR // IntToHex(MH_MAGIC, 8)
  else if Magic = MH_CIGAM then s := MH_CIGAM_STR // IntToHex(MH_CIGAM, 8)
  else if Magic = MH_MAGIC_64 then s := MH_MAGIC_64_STR // IntToHex(MH_MAGIC_64, 8)
  else if Magic = MH_CIGAM_64 then s := MH_CIGAM_64_STR // IntToHex(MH_CIGAM_64, 8)
  else
  begin
    s := '';
    for i := 0 to Length(Buffer) - 1 do s := s + Chr(Buffer[i]);
    s := Trim(s);
    if Copy(s, 1, 2) = 'MZ' then s := 'MZ'
    else if Copy(s, 2, 4) = 'ELF' then s := 'ELF';
  end;

  Result := s;

end;

function GetBinMagicStr(fName: string): string;
var
  fs: TFileStream;
begin
  Result := '';
  if not FileExists(fName) then Exit;

  try
    fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyWrite);
    try
      Result := GetBinMagicStr(fs);
    finally
      fs.Free;
    end;
  except
  end;
end;




end.
