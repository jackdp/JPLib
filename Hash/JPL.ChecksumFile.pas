unit JPL.ChecksumFile;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

{
  TChecksumFile

  Usage

  A. Write checksum file

  1. Create instance
  2. Add some files with AddFile
  3. Call SaveToFile and specify desired cheksum format.
  4. Free instance

  ----------------------------

  B. Read existsing checksum file

  1. Create instance
  2. Call LoadFromFile
  3. If ValidFile = True, do something with Items
  4. Free instance

}

interface

uses 
  SysUtils, Classes,
  Generics.Collections,
  JPL.Strings, JPL.Conversion, JPL.TStr, JPL.Hash.Common, JPL.Win.Dialogs;


type

  TChecksumFormat = (cfDefault, cfBSD, cfSFV);

  TChecksumItem = record
    HashType: TJPHashType;
    HashID: string;
    FileName: string;
    ExpandedFileName: string;
    FileExists: Boolean;
    Hash: string;
    function AsDebugStr: string;
    procedure ExpandFileName(const RootDir: string);
    procedure CheckFileExists;
  end;

  TChecksumItems = TList<TChecksumItem>;

  TChecksumFile = class
  private
    FValidFile: Boolean;
    FItems: TChecksumItems;
    FModeChar: Char;
    FComment: TStringList;
    FAddComment: Boolean;
    FFileChecksumFormat: TChecksumFormat;
    FRootDirectory: string;
    procedure SetModeChar(const Value: Char);
    procedure SetAddComment(const Value: Boolean);
    function GetCommentStr(ChecksumFormat: TChecksumFormat = cfDefault): string;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFile(const FileName, Hash: string; HashType: TJPHashType);

    function AsDebugStr: string;
    function AsString(ChecksumFormat: TChecksumFormat = cfDefault): string;
    procedure SaveToFile(const FileName: string; ChecksumFormat: TChecksumFormat = cfDefault);
    function LoadFromFile(const FileName: string): Boolean;

    property ValidFile: Boolean read FValidFile;
    property ModeChar: Char read FModeChar write SetModeChar;
    property Comment: TStringList read FComment;
    property AddComment: Boolean read FAddComment write SetAddComment;
    property Count: integer read GetCount;
    property FileChecksumFormat: TChecksumFormat read FFileChecksumFormat;
    property RootDirectory: string read FRootDirectory;
    property Items: TChecksumItems read FItems;
  end;



function HashTypeToHashID(const HashType: TJPHashType): string;
function HashIDToHashType(HashID: string): TJPHashType;
function GetHashTypeByFileExt(Ext: string): TJPHashType;


implementation


{$region '                helpers                '}
function HashTypeToHashID(const HashType: TJPHashType): string;
var
  s: string;
begin
  Result := '';
  s := HashTypeToStr(HashType, '');
  if s = '' then Exit;
  s := ReplaceFirst(s, 'SHA-2', 'SHA', True);
  s := ReplaceFirst(s, 'SHA-3 ', 'SHA3--', True);
  s := ReplaceFirst(s, '-', '');
  s := RemoveSpaces(s);
  Result := s;
end;

function HashIDToHashType(HashID: string): TJPHashType;
begin
  Result := htNone;
  HashID := TStr.TrimAndUp(HashID);
  if HashID = '' then Exit;

  if HashID = 'CRC16' then Result := htCrc16
  else if HashID = 'CRC24' then Result := htCrc24
  else if HashID = 'CRC32' then Result := htCrc32
  else if HashID = 'CRC64' then Result := htCrc64
  else if HashID = 'ADLER32' then Result := htAdler32

  else if HashID = 'MD2' then Result := htMd2
  else if HashID = 'MD5' then Result := htMd5
  else if HashID = 'RIPEMD' then Result := htRipeMD
  else if HashID = 'RIPEMD128' then Result := htRipeMD128
  else if HashID = 'RIPEMD160' then Result := htRipeMD160
  else if HashID = 'RIPEMD256' then Result := htRipeMD256
  else if HashID = 'RIPEMD320' then Result := htRipeMD320

  else if HashID = 'SHA0' then Result := htSha0
  else if HashID = 'SHA1' then Result := htSha1

  else if HashID = 'SHA224' then Result := htSha2_224
  else if HashID = 'SHA256' then Result := htSha2_256
  else if HashID = 'SHA384' then Result := htSha2_384
  else if HashID = 'SHA512' then Result := htSha2_512
  else if HashID = 'SHA512/224' then Result := htSha2_512_224
  else if HashID = 'SHA512/256' then Result := htSha2_512_256

  else if HashID = 'SHA3-224' then Result := htSha3_224
  else if HashID = 'SHA3-256' then Result := htSha3_256
  else if HashID = 'SHA3-384' then Result := htSha3_384
  else if HashID = 'SHA3-512' then Result := htSha3_512

  else if HashID = 'SHAKE128' then Result := htShake128
  else if HashID = 'SHAKE256' then Result := htShake256

  else if HashID = 'SNEFRU128' then Result := htSnefru128
  else if HashID = 'SNEFRU256' then Result := htSnefru256

  else if HashID = 'WHIRLPOOL' then Result := htWhirlpool;
end;

function GetHashTypeByFileExt(Ext: string): TJPHashType;
begin
  Result := htNone;
  Ext := TStr.TrimFromStart(Ext, '.');
  Ext := TStr.TrimAndLow(Ext);
  Ext := RemoveAll(Ext, '-');
  Ext := RemoveAll(Ext, '_');
  if Ext = '' then Exit;

  if Ext = 'sfv' then Result := htCrc32
  else if Ext = 'crc16' then Result := htCrc16
  else if Ext = 'crc24' then Result := htCrc24
  else if Ext = 'crc32' then Result := htCrc32
  else if Ext = 'crc64' then Result := htCrc64
  else if Ext = 'adler32' then Result := htAdler32

  else if Ext = 'md2' then Result := htMd2
  else if Ext = 'md4' then Result := htMd4
  else if Ext = 'md5' then Result := htMd5
  else if (Ext = 'ripemd') or (Ext = 'rmd') then Result := htRipeMD
  else if (Ext = 'ripemd128') or (Ext = 'rmd128') then Result := htRipeMD128
  else if (Ext = 'ripemd160') or (Ext = 'rmd160') then Result := htRipeMD160
  else if (Ext = 'ripemd256') or (Ext = 'rmd256') then Result := htRipeMD256
  else if (Ext = 'ripemd320') or (Ext = 'rmd320') then Result := htRipeMD320

  else if Ext = 'sha0' then Result := htSha0
  else if Ext = 'sha1' then Result := htSha1

  else if Ext = 'sha224' then Result := htSha2_224
  else if Ext = 'sha256' then Result := htSha2_256
  else if Ext = 'sha384' then Result := htSha2_384
  else if Ext = 'sha512' then Result := htSha2_512
  else if Ext = 'sha512224' then Result := htSha2_512_224
  else if Ext = 'sha512256' then Result := htSha2_512_256

  else if Ext = 'sha3224' then Result := htSha3_224
  else if Ext = 'sha3256' then Result := htSha3_256
  else if Ext = 'sha3384' then Result := htSha3_384
  else if Ext = 'sha3512' then Result := htSha3_512

  else if Ext = 'shake128' then Result := htShake128
  else if Ext = 'shake256' then Result := htShake256

  else if Ext = 'snefru128' then Result := htSnefru128
  else if Ext = 'snefru256' then Result := htSnefru256

  else if Ext = 'whirlpool' then Result := htWhirlpool;
end;
{$endregion helpers}


constructor TChecksumFile.Create;
begin
  inherited Create;
  FModeChar := '*';
  FItems := TChecksumItems.Create;
  FComment := TStringList.Create;
  FAddComment := True;
  Clear;
end;

destructor TChecksumFile.Destroy;
begin
  FItems.Clear;
  FItems.Free;
  FComment.Free;
  inherited;
end;

procedure TChecksumFile.Clear;
begin
  FValidFile := True;
  FItems.Clear;
  FComment.Clear;
  FFileChecksumFormat := cfDefault;
  FRootDirectory := ExtractFileDir(ParamStr(0));
end;

procedure TChecksumFile.SetAddComment(const Value: Boolean);
begin
  FAddComment := Value;
end;

procedure TChecksumFile.SetModeChar(const Value: Char);
begin
  FModeChar := Value;
end;

procedure TChecksumFile.AddFile(const FileName, Hash: string; HashType: TJPHashType);
var
  Item: TChecksumItem;
begin
  Item.HashType := HashType;
  Item.HashID := HashTypeToHashID(HashType);
  Item.FileName := FileName;
  Item.ExpandFileName(FRootDirectory);
  Item.CheckFileExists;
  Item.Hash := Hash;
  FItems.Add(Item);
end;

function TChecksumFile.GetCommentStr(ChecksumFormat: TChecksumFormat): string;
var
  CommentChar: Char;
  i: integer;
begin
  Result := '';
  if FComment.Count = 0 then Exit;
  CommentChar := ';';
  for i := 0 to FComment.Count - 1 do
    Result := Result + CommentChar + FComment[i] + ENDL;
  Result := Trim(Result);
end;

function TChecksumFile.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TChecksumFile.LoadFromFile(const FileName: string): Boolean;
var
  sl: TStringList;
  Item: TChecksumItem;
  i, xp, xBracketOpenPos, xBracketClosePos: integer;
  Line, s, Ext: string;
  FirstChar: Char;
  bValidItem: Boolean;
  HashType: TJPHashType;
begin
  Result := False;
  Clear;
  FValidFile := False;
  if not FileExists(FileName) then Exit;

  FRootDirectory := ExtractFileDir(FileName);

  Ext := GetFileExt(FileName, True);
  Ext := LowerCase(Ext);

  sl := TStringList.Create;
  try

    sl.LoadFromFile(FileName);

    for i := 0 to sl.Count - 1 do
    begin
      Line := Trim(sl[i]);
      if Line = '' then Continue;
      FirstChar := Line[1];

      // ---------------- Comment --------------
      if (FirstChar = ';') or (FirstChar = '#') then
      begin
        FComment.Add(Copy(Line, 2, Length(Line)));
        Continue;
      end;

      bValidItem := False;


      // --------------- BSD-style -----------------
      // HashID (File Name) = HASH
      // Eg.: SHA1 (PSGen_32bit_PORTABLE.zip) = 475d8c9d2526967a54e769f7ef4e8f465af38810
      xBracketOpenPos := TStr.FirstCharPos(Line, '(');
      xBracketClosePos := TStr.LastCharPos(Line, ')');
      if (xBracketOpenPos > 0) and (xBracketClosePos > xBracketOpenPos) and (TStr.PosOf('=', Line) > xBracketClosePos) and (TStr.CharCount(Line, ' ') >= 3) then
      begin
        xp := Pos(' ', Line);
        s := Copy(Line, 1, xp - 1);
        s := Trim(s);
        HashType := HashIDToHashType(s);
        if HashType <> htNone then
        begin
          Item.HashType := HashType;
          Item.HashID := s;

          Line := Copy(Line, xp + 1, Length(Line));
          Line := Trim(Line);

          // Pozostało: '(file name) = HASH'
          if not TStr.StartsStr('(', Line) then Continue;
          Line := Copy(Line, 2, Length(Line));
          xp := TStr.LastCharPos(Line, ')');
          Item.FileName := Copy(Line, 1, xp - 1);

          Line := Copy(Line, xp + 1, Length(Line));

          // Pozostało: ' = HASH'
          Line := RemoveSpaces(Line);
          if not TStr.StartsStr('=', Line) then Continue;

          Line := Copy(Line, 2, Length(Line));
          if not TStr.IsValidHexStr(Line) then Continue;
          Item.Hash := Line;

          Item.ExpandFileName(FRootDirectory);
          Item.CheckFileExists;
          bValidItem := True;
          FFileChecksumFormat := cfBSD;
          FItems.Add(Item);
        end;
      end;


      // file name HASH          <-- SFV
      //    OR
      // HASH *file name         <-- eg. md5sum.exe
      if not bValidItem then
      begin

        Line := Trim(sl[i]);

        xp := TStr.LastCharPos(Line, ' ');
        if xp = 0 then Continue;

        if Ext = 'sfv' then
        begin
          s := Trim(Copy(Line, 1, xp - 1));
          Line := Copy(Line, xp + 1, Length(Line));

          TStr.TrimFromStart(Line, '*');
          if TStr.IsValidHexStr(Line) and IsValidHashLen(Line, htCrc32) then
          begin
            Item.HashType := htCrc32;
            Item.HashID := 'CRC32';
            Item.FileName := s;
            Item.Hash := Line;
            Item.ExpandFileName(FRootDirectory);
            Item.CheckFileExists;
            FItems.Add(Item);
            bValidItem := True;
            FFileChecksumFormat := cfSFV;
          end;
        end;

        // .md5, .sha1, .sha224 ...
        if not bValidItem then
        begin
          Line := Trim(sl[i]);
          HashType := GetHashTypeByFileExt(Ext);
          if HashType = htNone then Continue;
          xp := TStr.FirstCharPos(Line, ' ');
          if xp = 0 then Continue;
          s := Trim(Copy(Line, 1, xp - 1));
          Line := Trim(Copy(Line, xp + 1, Length(Line)));
          Line := TStr.TrimFromStart(Line, '*');
          if not TStr.IsValidHexStr(s) then Continue;
          Item.HashType := HashType;
          Item.HashID := HashTypeToHashID(HashType);
          Item.FileName := Line;
          Item.Hash := s;
          Item.ExpandFileName(FRootDirectory);
          Item.CheckFileExists;
          FItems.Add(Item);
          bValidItem := True;
          FFileChecksumFormat := cfDefault;
        end;


      end;


      if not FValidFile then FValidFile := bValidItem;

    end; // for i

    Result := FValidFile;

  finally
    sl.Free;
  end;
end;

function TChecksumFile.AsString(ChecksumFormat: TChecksumFormat = cfDefault): string;
var
  i: integer;
  Item: TChecksumItem;
  sl: TStringList;
  Line: string;
begin
  sl := TStringList.Create;
  try

    for i := 0 to FItems.Count - 1 do
    begin

      Item := FItems[i];

      case ChecksumFormat of
        cfDefault: Line := Item.Hash + ' ' + FModeChar + Item.FileName;
        cfBSD: Line := Item.HashID + ' (' + Item.FileName + ') = ' + Item.Hash;
        cfSFV: Line := Item.FileName + ' ' + Item.Hash;
      end;

      sl.Add(Line);

    end;

    Result := Trim(sl.Text);

  finally
    sl.Free;
  end;
end;

procedure TChecksumFile.SaveToFile(const FileName: string; ChecksumFormat: TChecksumFormat = cfDefault);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if FAddComment and (FComment.Count > 0) then sl.Add(GetCommentStr(ChecksumFormat));
    sl.Add(AsString(ChecksumFormat));
    sl.SaveToFile(FileName, TEncoding.UTF8);
  finally
    sl.Free;
  end;
end;

function TChecksumFile.AsDebugStr: string;
var
  s: string;
  i: integer;
begin
  s := '';

  if FComment.Count > 0 then s := s + 'COMMENT' + ENDL + FComment.Text + ENDL;
  s := s + 'FILES: ' + itos(Count) + ENDL + ENDL;
  for i := 0 to FItems.Count - 1 do
    s := s + 'File ' + itos(i + 1) + ENDL + FItems[i].AsDebugStr + ENDL + '----------------------------------------' + ENDL;

  Result := s;
end;





{ TChecksumItem }

function TChecksumItem.AsDebugStr: string;
begin
  Result :=
    'File name: ' + FileName + ENDL +
    'Expanded file name: ' + ExpandedFileName + ENDL +
    'File exists: ' + BoolToStrYesNo(FileExists) + ENDL +
    'HashType: ' + HashTypeToStr(HashType) + ENDL +
    'HashID: ' + HashID + ENDL +
    'Hash: ' + Hash;
end;

procedure TChecksumItem.CheckFileExists;
begin
  FileExists := SysUtils.FileExists(ExpandedFileName);
end;

procedure TChecksumItem.ExpandFileName(const RootDir: string);
begin
  if PathIsAbsolute(FileName) then ExpandedFileName := FileName
  else ExpandedFileName := TStr.RemoveTrailingPathDelimiter(RootDir) + PathDelim + FileName;
end;

end.