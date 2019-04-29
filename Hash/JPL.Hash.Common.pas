unit JPL.Hash.Common;

interface

uses
  {$IFDEF DCC}Windows,{$ENDIF}
  SysUtils, Classes, JPL.Conversion
  //JP_Math, JP.Hash.CRC32,
  //DCPbase64, DCPmd4, DCPmd5, DCPcrypt2, DCPsha1, DCPtiger, DCPhaval, DCPripemd160, DCPripemd128, DCPsha512, DCPsha256
  ;


const
  ERR_OPEN_FILE = 'Can not open file "%s"';
  ERR_ALLOC_BUFFER = 'Can not allocate hash buffer';
  ERR_INVALID_HASH_TYPE = 'Invalid hash type';
  ERR_ABORTED = 'Aborted';

  {$region ' DIGEST_LEN '}
  DIGEST_LEN_BITS_128 = 16;  // 128 bits / 16 bytes
  DIGEST_LEN_BITS_160 = 20;  // 160 bits / 20 bytes
  DIGEST_LEN_BITS_224 = 28;  // 224 bits / 28 bytes
  DIGEST_LEN_BITS_256 = 32;  // 256 bits / 32 bytes
  DIGEST_LEN_BITS_384 = 48;  // 384 bits / 48 bytes
  DIGEST_LEN_BITS_512 = 64;  // 512 bits / 64 bytes


  DIGEST_LEN_CRC16 = 2;      //   8 bits / 2 bytes
  DIGEST_LEN_CRC24 = 3;      //  12 bits / 3 bytes
  DIGEST_LEN_CRC32 = 4;      //  16 bits / 4 bytes
  DIGEST_LEN_CRC64 = 8;      //  32 bits / 8 bytes
  DIGEST_LEN_ADLER32 = 4;    //  16 bits / 4 bytes

  DIGEST_LEN_MD4 = DIGEST_LEN_BITS_128;
  DIGEST_LEN_MD5 = DIGEST_LEN_BITS_128;
  DIGEST_LEN_RIPEMD = DIGEST_LEN_BITS_128;
  DIGEST_LEN_RIPEMD128 = DIGEST_LEN_BITS_128;
  DIGEST_LEN_RIPEMD160 = DIGEST_LEN_BITS_160;
  DIGEST_LEN_RIPEMD256 = DIGEST_LEN_BITS_256;
  DIGEST_LEN_RIPEMD320 = 40; // 320 bits / 40 bytes

  DIGEST_LEN_SHA0 = DIGEST_LEN_BITS_160;
  DIGEST_LEN_SHA1 = DIGEST_LEN_BITS_160;

  DIGEST_LEN_SHA2_224 = DIGEST_LEN_BITS_224;
  DIGEST_LEN_SHA2_256 = DIGEST_LEN_BITS_256;
  DIGEST_LEN_SHA2_384 = DIGEST_LEN_BITS_384;
  DIGEST_LEN_SHA2_512 = DIGEST_LEN_BITS_512;
  DIGEST_LEN_SHA2_512_224 = DIGEST_LEN_BITS_224;
  DIGEST_LEN_SHA2_512_256 = DIGEST_LEN_BITS_256;

  DIGEST_LEN_SHA3_224 = DIGEST_LEN_BITS_224;
  DIGEST_LEN_SHA3_256 = DIGEST_LEN_BITS_256;
  DIGEST_LEN_SHA3_384 = DIGEST_LEN_BITS_384;
  DIGEST_LEN_SHA3_512 = DIGEST_LEN_BITS_512;

  DIGEST_LEN_SHAKE128 = DIGEST_LEN_BITS_256;
  DIGEST_LEN_SHAKE256 = DIGEST_LEN_BITS_512;

  DIGEST_LEN_WHIRLPOOL = DIGEST_LEN_BITS_512;

  DIGEST_LEN_SNEFRU128 = DIGEST_LEN_BITS_128;
  DIGEST_LEN_SNEFRU256 = DIGEST_LEN_BITS_256;

  DIGEST_LEN_EDONR256 = DIGEST_LEN_BITS_256;
  DIGEST_LEN_EDONR512 = DIGEST_LEN_BITS_512;

  DIGEST_LEN_GOST = DIGEST_LEN_BITS_256;
  DIGEST_LEN_GOST_CRYPTOPRO = DIGEST_LEN_BITS_256;

  DIGEST_LEN_HAS160 = DIGEST_LEN_BITS_160;
  DIGEST_LEN_TIGER = 24;     // 192 bits / 24 bytes
  DIGEST_LEN_BTIH = DIGEST_LEN_BITS_160;
  DIGEST_LEN_ED2K = DIGEST_LEN_BITS_128;
  DIGEST_LEN_AICH = DIGEST_LEN_BITS_128;

  DIGEST_LEN_HAVAL = DIGEST_LEN_BITS_256;

  {$endregion DIGEST_LEN}

  {$region ' HASH_LEN '}
  HASH_LEN_CRC16 = DIGEST_LEN_CRC16 * 2;
  HASH_LEN_CRC24 = DIGEST_LEN_CRC24 * 2;
  HASH_LEN_CRC32 = DIGEST_LEN_CRC32 * 2;
  HASH_LEN_CRC64 = DIGEST_LEN_CRC64 * 2;
  HASH_LEN_ADLER32 = DIGEST_LEN_ADLER32 * 2;

  HASH_LEN_MD4 = DIGEST_LEN_MD4 * 2;
  HASH_LEN_MD5 = DIGEST_LEN_MD5 * 2;
  HASH_LEN_RIPEMD = DIGEST_LEN_RIPEMD * 2;
  HASH_LEN_RIPEMD128 = DIGEST_LEN_RIPEMD128 * 2;
  HASH_LEN_RIPEMD160 = DIGEST_LEN_RIPEMD160 * 2;
  HASH_LEN_RIPEMD256 = DIGEST_LEN_RIPEMD256 * 2;
  HASH_LEN_RIPEMD320 = DIGEST_LEN_RIPEMD320 * 2;

  HASH_LEN_SHA0 = DIGEST_LEN_SHA0 * 2;
  HASH_LEN_SHA1 = DIGEST_LEN_SHA1 * 2;

  HASH_LEN_SHA2_224 = DIGEST_LEN_SHA2_224 * 2;
  HASH_LEN_SHA2_256 = DIGEST_LEN_SHA2_256 * 2;
  HASH_LEN_SHA2_384 = DIGEST_LEN_SHA2_384 * 2;
  HASH_LEN_SHA2_512 = DIGEST_LEN_SHA2_512 * 2;
  HASH_LEN_SHA2_512_224 = DIGEST_LEN_SHA2_512_224 * 2;
  HASH_LEN_SHA2_512_256 = DIGEST_LEN_SHA2_512_256 * 2;

  HASH_LEN_SHA3_224 = DIGEST_LEN_SHA3_224 * 2;
  HASH_LEN_SHA3_256 = DIGEST_LEN_SHA3_256 * 2;
  HASH_LEN_SHA3_384 = DIGEST_LEN_SHA3_384 * 2;
  HASH_LEN_SHA3_512 = DIGEST_LEN_SHA3_512 * 2;

  HASH_LEN_SHAKE128 = DIGEST_LEN_SHAKE128 * 2;
  HASH_LEN_SHAKE256 = DIGEST_LEN_SHAKE256 * 2;

  HASH_LEN_WHIRLPOOL = DIGEST_LEN_WHIRLPOOL * 2;

  HASH_LEN_SNEFRU128 = DIGEST_LEN_SNEFRU128 * 2;
  HASH_LEN_SNEFRU256 = DIGEST_LEN_SNEFRU256 * 2;

  HASH_LEN_EDONR256 = DIGEST_LEN_EDONR256 * 2;
  HASH_LEN_EDONR512 = DIGEST_LEN_EDONR512 * 2;

  HASH_LEN_GOST = DIGEST_LEN_GOST * 2;
  HASH_LEN_GOST_CRYPTOPRO = DIGEST_LEN_GOST_CRYPTOPRO * 2;

  HASH_LEN_HAS160 = DIGEST_LEN_HAS160 * 2;
  HASH_LEN_TIGER = DIGEST_LEN_TIGER * 2;
  HASH_LEN_BTIH = DIGEST_LEN_BTIH * 2;
  HASH_LEN_ED2K = DIGEST_LEN_ED2K * 2;
  HASH_LEN_AICH = DIGEST_LEN_AICH * 2;

  HASH_LEN_HAVAL = DIGEST_LEN_HAVAL * 2;

  {$endregion HASH_LEN}

  MIN_HASH_LEN = HASH_LEN_CRC16;


  HASH_DEF_BUF_SIZE = High(Word);


type

  THashEnumProc = function(PercentComplete: integer; BufNo: integer): Boolean;
  //TReadFileEnumProc = function(PercentComplete: Single; Buffer: array of Byte; ReadBytes: integer; BufNo: integer): Boolean;

  TJPHashType = (
    htNone,
    htCrc16, htCrc24, htCrc32, htCrc64, htAdler32,
    htMd4, htMd5, htRipeMD, htRipeMD128, htRipeMD160, htRipeMD256, htRipeMD320,
    htSha0, htSha1,
    htSha2_224, htSha2_256, htSha2_384, htSha2_512, htSha2_512_224, htSha2_512_256,
    htSha3_224, htSha3_256, htSha3_384, htSha3_512,
    htShake128, htShake256,
    htWhirlpool,
    htSnefru128, htSnefru256,
    htEdonr256, htEdonr512,
    htGost, htGostCryptopro,
    htHas160,
    htTiger,
    //htTTH,
    //htBTIH,
    htED2K,
    //htAICH,
    htHaval
  );

  THashResultRec = record
    StrValueUpper: string;
    StrValueLower: string;
    IntValue: integer;
    Int64Value: Int64;
    StreamSize: Int64;
    ElapsedTimeMs: DWORD;
    SpeedMBperSec: Single;
  end;

  THashCheckRec = record
    ValidChars: Boolean;
    ValidLen: Boolean;
    ExpectedLen: integer;
  end;


var
  //HashBufferSize: integer = (1024 * 800) - 1; // 800 KB
  HashBufferSize: integer = (1024 * 1024); //



function FormatHash(HashStr: string; bUpperCase: Boolean; ByteSeparator: string = ''): string;
function IsValidHashStr(const Hash: string): Boolean;
function IsValidHashLen(const Hash: string; HashType: TJPHashType): Boolean;
function GetHashLen(HashType: TJPHashType; ErrorResult: integer = -1): integer;

// jeœli wszystko OK, CheckHash zwraca True, w przeciwnym razie zwraca False i dodatkowe informacje w HashCheckRec
function CheckHash(Hash: string; HashType: TJPHashType; var HashCheckRec: THashCheckRec): Boolean;
function HashTypeToStr(const HashType: TJPHashType): string;
procedure ClearHashResultRec(var hrr: THashResultRec);

function IsChecksum(const HashType: TJPHashType): Boolean;
function IsMDHash(const HashType: TJPHashType): Boolean;
function IsSha2Hash(const HashType: TJPHashType): Boolean;
function IsSha3Hash(const HashType: TJPHashType): Boolean;

{$IFDEF DCC} function JPFormat(const Msg: string; const Args: array of const): string;  {$ENDIF}
function HashReverseBytes(A: longint): longint; {-rotate byte of longint}
function GetSpeedValue_MB_per_sec(const FileSize: Int64; const ElapsedTimeMs: DWORD; ErrorValue: Double = 0): Double;

//function ProcessStream(Stream: TStream; EnumProc: TReadFileEnumProc; BufferSize: integer = HASH_DEF_BUF_SIZE; StartPos: Int64 = 0; EndPos: integer = -1): Boolean;


implementation



function HashReverseBytes(A: longint): longint; {-rotate byte of longint}
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

{$region '           JPFormat          '}
{$IFDEF DCC}
function JPFormat(const Msg: string; const Args: array of const): string;
var
  i: integer;
  sr: string;
begin

  sr := Msg;

  for i := Low(Args) to High(Args) do
  case TVarRec(Args[i]).VType of

    vtInteger: sr := StringReplace(sr, '%d', IntToStr(Args[i].VInteger), []);
    vtInt64: sr := StringReplace(sr, '%d', IntToStr(Args[i].VInt64^), []);
    vtExtended: sr := StringReplace(sr, '%f', FloatToStr(Args[i].VExtended^), []);
    vtString: sr := StringReplace(sr, '%s', string(Args[i].VString^), []);
    vtAnsiString: sr := StringReplace(sr, '%s', string(AnsiString(Args[i].VAnsiString)), []);
    vtWideString: sr := StringReplace(sr, '%s', WideString(Args[i].VWideString), []);
    vtUnicodeString: sr := StringReplace(sr, '%s', UnicodeString(Args[i].VUnicodeString), []);
    vtPointer: sr := StringReplace(sr, '%p', IntToHex(integer(@Args[i].VPointer), 8), []);
    vtPChar: sr := StringReplace(sr, '%s', string(Args[i].VPChar), []);
    vtPWideChar: sr := StringReplace(sr, '%s', string(Args[i].VPWideChar), []);
    vtChar: sr := StringReplace(sr, '%c', string(Args[i].VChar), []);
    vtWideChar: sr := StringReplace(sr, '%c', string(Args[i].VWideChar), []);
    vtCurrency: sr := StringReplace(sr, '%m', FloatToStr(Args[i].VCurrency^), []);
    vtBoolean: sr := StringReplace(sr, '%b', BoolToStr(Args[i].VBoolean, 'Yes', 'No'), []);
    vtObject: sr := StringReplace(sr, '%o', TObject(Args[i].VObject).ToString, []);
    //vtVariant: sr := StringReplace(sr, '%v', Args[i].VVariant^, []); // too big exe!
    vtVariant: sr := StringReplace(sr, '%v', 'VARIANT TYPES NOT SUPPORTED', []);

  else
    //Writeln('UNKNOWN data type');
  end;

  Result := sr;
end;
{$ENDIF}
{$endregion JPFormat}

function GetSpeedValue_MB_per_sec(const FileSize: Int64; const ElapsedTimeMs: DWORD; ErrorValue: Double = 0): Double;
var
  xMB, xSecs: Double;
begin
  xMB := FileSize / 1024 / 1024;
  xSecs := ElapsedTimeMs / 1000;
  if xSecs > 0 then Result := xMB / xSecs
  else Result := ErrorValue
end;

procedure ClearHashResultRec(var hrr: THashResultRec);
begin
  hrr.StrValueUpper := '';
  hrr.StrValueLower := '';
  hrr.IntValue := 0;
  hrr.Int64Value := 0;
  hrr.StreamSize := 0;
  hrr.ElapsedTimeMs := 0;
  hrr.SpeedMBperSec := 0;
end;

function IsChecksum(const HashType: TJPHashType): Boolean;
begin
  case HashType of
    htCrc16, htCrc24, htCrc32, htCrc64, htAdler32: Result := True;
  else
    Result := False;
  end;
end;

function IsMDHash(const HashType: TJPHashType): Boolean;
begin
  case HashType of
    htMd4, htMd5, htRipeMD, htRipeMD128, htRipeMD160, htRipeMD256, htRipeMD320: Result := True;
  else
    Result := False;
  end;
end;

function IsSha2Hash(const HashType: TJPHashType): Boolean;
begin
  case HashType of
    htSha2_224, htSha2_256, htSha2_384, htSha2_512, htSha2_512_224, htSha2_512_256: Result := True;
  else
    Result := False;
  end;
end;

function IsSha3Hash(const HashType: TJPHashType): Boolean;
begin
  case HashType of
    htSha3_224, htSha3_256, htSha3_384, htSha3_512: Result := True;
  else
    Result := False;
  end;
end;

function HashTypeToStr(const HashType: TJPHashType): string;
begin
  case HashType of
    htCrc16: Result := 'CRC16';
    htCrc24: Result := 'CRC24';
    htCrc32: Result := 'CRC32';
    htCrc64: Result := 'CRC64';
    htAdler32: Result := 'Adler32';

    htMd4: Result := 'MD4';
    htMd5: Result := 'MD5';
    htRipeMD: Result := 'RipeMD';
    htRipeMD128: Result := 'RipeMD-128';
    htRipeMD160: Result := 'RipeMD-160';
    htRipeMD256: Result := 'RipeMD-256';
    htRipeMD320: Result := 'RipeMD-320';

    htSha0: Result := 'SHA-0';
    htSha1: Result := 'SHA-1';
    htSha2_224: Result := 'SHA-2-224';
    htSha2_256: Result := 'SHA-2-256';
    htSha2_384: Result := 'SHA-2-384';
    htSha2_512: Result := 'SHA-2-512';
    htSha2_512_224: Result := 'SHA-2-512/224';
    htSha2_512_256: Result := 'SHA-2-512/256';

    htSha3_224: Result := 'SHA-3-224';
    htSha3_256: Result := 'SHA-3-256';
    htSha3_384: Result := 'SHA-3-384';
    htSha3_512: Result := 'SHA-3-512';

    htShake128: Result := 'SHAKE-128';
    htShake256: Result := 'SHAKE-256';

    htWhirlpool: Result := 'Whirlpool';

    htSnefru128: Result := 'SNEFRU-128';
    htSnefru256: Result := 'SNEFRU-256';

    htEdonr256: Result := 'EDON-R 256';
    htEdonr512: Result := 'EDON-R 512';

    htGost: Result := 'GOST';
    htGostCryptopro: Result := 'GOST-CRYPTOPRO';

    htHas160: Result := 'HAS-160';
    htTiger: Result := 'Tiger';
    //htTTH: Result := 'TTH';
    //htBTIH: Result := 'BTIH';
    htED2K: Result := 'ED2K';
    //htAICH: Result := 'AICH';

    htHaval: Result := 'HAVAL';

  else
    Result := 'unknown';
  end;
end;

function GetHashLen(HashType: TJPHashType; ErrorResult: integer = -1): integer;
begin
  case HashType of
    htCrc16: Result := HASH_LEN_CRC16;
    htCrc24: Result := HASH_LEN_CRC24;
    htCrc32: Result := HASH_LEN_CRC32;
    htCrc64: Result := HASH_LEN_CRC64;
    htAdler32: Result := HASH_LEN_ADLER32;

    htMd4: Result := HASH_LEN_MD4;
    htMd5: Result := HASH_LEN_MD5;
    htRipeMD: Result := HASH_LEN_RIPEMD;
    htRipeMD128: Result := HASH_LEN_RIPEMD128;
    htRipeMD160: Result := HASH_LEN_RIPEMD160;
    htRipeMD256: Result := HASH_LEN_RIPEMD256;
    htRipeMD320: Result := HASH_LEN_RIPEMD320;

    htSha0: Result := HASH_LEN_SHA0;
    htSha1: Result := HASH_LEN_SHA1;

    htSha2_224: Result := HASH_LEN_SHA2_224;
    htSha2_256: Result := HASH_LEN_SHA2_256;
    htSha2_384: Result := HASH_LEN_SHA2_384;
    htSha2_512: Result := HASH_LEN_SHA2_512;
    htSha2_512_224: Result := HASH_LEN_SHA2_512_224;
    htSha2_512_256: Result := HASH_LEN_SHA2_512_256;

    htSha3_224: Result := HASH_LEN_SHA3_224;
    htSha3_256: Result := HASH_LEN_SHA3_256;
    htSha3_384: Result := HASH_LEN_SHA3_384;
    htSha3_512: Result := HASH_LEN_SHA3_512;

    htShake128: Result := HASH_LEN_SHAKE128;
    htShake256: Result := HASH_LEN_SHAKE256;

    htWhirlpool: Result := HASH_LEN_WHIRLPOOL;

    htSnefru128: Result := HASH_LEN_SNEFRU128;
    htSnefru256: Result := HASH_LEN_SNEFRU256;

    htEdonr256: Result := HASH_LEN_EDONR256;
    htEdonr512: Result := HASH_LEN_EDONR512;

    htGost: Result := HASH_LEN_GOST;
    htGostCryptopro: Result := HASH_LEN_GOST_CRYPTOPRO;

    htHas160: Result := HASH_LEN_HAS160;
    htTiger: Result := HASH_LEN_TIGER;
    //htTTH: Result := HASH_LEN_TTH;
    //htBTIH: Result := HASH_LEN_BTIH;
    htED2K: Result := HASH_LEN_ED2K;
    //htAICH: Result := HASH_LEN_AICH;

    htHaval: Result := HASH_LEN_HAVAL;

  else
    Result := ErrorResult;
  end;
end;

function IsValidHashLen(const Hash: string; HashType: TJPHashType): Boolean;
var
  HashLen: integer;
begin
  HashLen := Length(Hash);
  if Odd(HashLen) then Result := False
  else Result := HashLen = GetHashLen(HashType, -1);
end;

function IsValidHashStr(const Hash: string): Boolean;
begin
  Result := IsValidHexStr(Hash, False);
end;

function CheckHash(Hash: string; HashType: TJPHashType; var HashCheckRec: THashCheckRec): Boolean;
begin
  HashCheckRec.ValidChars := IsValidHashStr(Hash);
  HashCheckRec.ValidLen := IsValidHashLen(Hash, HashType);
  HashCheckRec.ExpectedLen := GetHashLen(HashType);
  Result := HashCheckRec.ValidChars and HashCheckRec.ValidLen;
end;

function FormatHash(HashStr: string; bUpperCase: Boolean; ByteSeparator: string = ''): string;
var
  i: integer;
  s: string;
begin
  s := HashStr;

  if bUpperCase then s := UpperCase(s)
  else s := LowerCase(s);

  if ByteSeparator <> '' then
    for i := 1 to Length(s) do
    begin
      if i mod 2 = 0 then
      begin

      end;
    end;

  Result := s;
end;


end.
