unit JPL.Hash.Common;

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface


uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, JPL.Strings, JPL.Conversion
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


  DIGEST_LEN_CRC16 = 2;      //  16 bits / 2 bytes
  DIGEST_LEN_CRC24 = 3;      //  24 bits / 3 bytes
  DIGEST_LEN_CRC32 = 4;      //  32 bits / 4 bytes
  DIGEST_LEN_CRC64 = 8;      //  64 bits / 8 bytes
  DIGEST_LEN_ADLER32 = 4;    //  32 bits / 4 bytes

  DIGEST_LEN_MD2 = DIGEST_LEN_BITS_128;
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

  HASH_LEN_MD2 = DIGEST_LEN_MD2 * 2;
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

  TJPHashType = (
    htNone,
    htCrc16, htCrc24, htCrc32, htCrc64, htAdler32,
    htMd2, htMd4, htMd5, htRipeMD, htRipeMD128, htRipeMD160, htRipeMD256, htRipeMD320,
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

  THashTypeDynArray = array of TJPHashType;

  THashResultRec = record
    ValidHash: Boolean;
    HashType: TJPHashType;
    StrValueUpper: string;
    StrValueLower: string;
    IntValue: integer;
    Int64Value: Int64;
    StreamSize: Int64;
    ElapsedTimeMs: DWORD;
    SpeedMBperSec: Single;
    procedure Clear;
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
function GetHashStrLength(HashType: TJPHashType; ErrorResult: integer = -1): integer;
function GetHashDigestLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;
function GetHashByteLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;
function GetHashBitLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;

// jeśli wszystko OK, CheckHash zwraca True, w przeciwnym razie zwraca False i dodatkowe informacje w HashCheckRec
function CheckHash(Hash: string; HashType: TJPHashType; var HashCheckRec: THashCheckRec): Boolean;
function HashTypeToStr(const HashType: TJPHashType; StrUnknownHash: string = ''): string;
function StrToHashType(s: string; Default: TJPHashType = htNone): TJPHashType;
procedure ClearHashResultRec(var hrr: THashResultRec);

function IsChecksum(const HashType: TJPHashType): Boolean;
function IsMDHash(const HashType: TJPHashType): Boolean;
function IsSha2Hash(const HashType: TJPHashType): Boolean;
function IsSha3Hash(const HashType: TJPHashType): Boolean;

{$IFDEF DCC} function JPFormat(const Msg: string; const Args: array of const): string;  {$ENDIF}
function HashReverseBytes(A: LongInt): LongInt; {-rotate byte of LongInt}
function GetSpeedValue_MB_per_sec(const FileSize: Int64; const ElapsedTimeMs: DWORD; ErrorValue: Double = 0): Double;

function GetPossibleHashes(HashStr: string; bAddZeroIfOddLength: Boolean = True): THashTypeDynArray;



implementation



function GetPossibleHashes(HashStr: string; bAddZeroIfOddLength: Boolean = True): THashTypeDynArray;
var
  Len: integer;

  procedure Add(const ht: TJPHashType);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := ht;
  end;

begin
  SetLength(Result, 0);
  HashStr := RemoveAll(HashStr, ' ');
  HashStr := RemoveAll(HashStr, '-');
  Len := Length(HashStr);
  if Odd(Len) then
    if bAddZeroIfOddLength then HashStr := '0' + HashStr
    else Exit;


  case Len of
    4: Add(htCrc16);
    6: Add(htCrc24);
    8:
      begin
        Add(htCrc32);
        Add(htAdler32);
      end;
    16: Add(htCrc64);
    32:
      begin
        Add(htMd2);
        Add(htMd4);
        Add(htMd5);
        Add(htRipeMD);
        Add(htRipeMD128);
        Add(htSnefru128);
        Add(htED2K);
        //Add(htAICH);
      end;
    40:
      begin
        Add(htRipeMD160);
        Add(htSha0);
        Add(htSha1);
        Add(htHas160);
        //Add(htBTIH);
      end;
    48: Add(htTiger);
    56:
      begin
        Add(htSha2_224);
        Add(htSha2_512_224);
        Add(htSha3_224);
      end;
    64:
      begin
        Add(htRipeMD256);
        Add(htSha2_256);
        Add(htSha2_512_256);
        Add(htSha3_256);
        Add(htShake128);
        Add(htSnefru256);
        Add(htEdonr256);
        Add(htGost);
        Add(htGostCryptopro);
        Add(htHaval);
      end;
    80: Add(htRipeMD320);
    96:
      begin
        Add(htSha2_384);
        Add(htSha3_384);
      end;
    128:
      begin
        Add(htSha2_512);
        Add(htSha3_512);
        Add(htShake256);
        Add(htWhirlpool);
      end;
  end;

end;



function HashReverseBytes(A: LongInt): LongInt; {-rotate byte of LongInt}
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
  hrr.Clear;
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
    htMd2, htMd4, htMd5, htRipeMD, htRipeMD128, htRipeMD160, htRipeMD256, htRipeMD320: Result := True;
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

function StrToHashType(s: string; Default: TJPHashType = htNone): TJPHashType;
begin
  s := RemoveAll(s, ' ');
  s := RemoveAll(s, '-');
  s := RemoveAll(s, '_');
  s := ReplaceAll(s, '\', '');
  s := ReplaceAll(s, '/', '');
  s := LowerCase(s);

  if (s = '') or (s = 'none') then Result := htNone
  else if s = 'crc16' then Result := htCrc16
  else if s = 'crc24' then Result := htCrc24
  else if s = 'crc32' then Result := htCrc32
  else if s = 'crc64' then Result := htCrc64
  else if s = 'adler32' then Result := htAdler32

  else if s = 'md2' then Result := htMd2
  else if s = 'md4' then Result := htMd4
  else if s = 'md5' then Result := htMd5
  else if s = 'ripemd' then Result := htRipeMD
  else if (s = 'ripemd128') or (s = 'rmd128') then Result := htRipeMD128
  else if (s = 'ripemd160') or (s = 'rmd160') then Result := htRipeMD160
  else if (s = 'ripemd256') or (s = 'rmd256') then Result := htRipeMD256
  else if (s = 'ripemd320') or (s = 'rmd320') then Result := htRipeMD320

  else if s = 'sha0' then Result := htSha0
  else if s = 'sha1' then Result := htSha1
  else if (s = 'sha2224') or (s = 'sha224') then Result := htSha2_224
  else if (s = 'sha2256') or (s = 'sha256') then Result := htSha2_256
  else if (s = 'sha2384') or (s = 'sha384') then Result := htSha2_384
  else if (s = 'sha2512') or (s = 'sha512') then Result := htSha2_512
  else if (s = 'sha2512224') then Result := htSha2_512_224
  else if (s = 'sha2512256') then Result := htSha2_512_256

  else if s = 'sha3224' then Result := htSha3_224
  else if s = 'sha3256' then Result := htSha3_256
  else if s = 'sha3384' then Result := htSha3_384
  else if s = 'sha3512' then Result := htSha3_512

  else if s = 'shake128' then Result := htShake128
  else if s = 'shake256' then Result := htShake256
  else if s = 'whirlpool' then Result := htWhirlpool

  else if s = 'snefru128' then Result := htSnefru128
  else if s = 'snefru256' then Result := htSnefru256

  else if s = 'edonr256' then Result := htEdonr256
  else if s = 'edonr512' then Result := htEdonr512

  else if s = 'gost' then Result := htGost
  else if s = 'gostcryptopro' then Result := htGostCryptopro

  else if s = 'has160' then Result := htHas160
  else if s = 'tiger' then Result := htTiger
  else if s = 'ed2k' then Result := htED2K
  else if s = 'haval' then Result := htHaval

  else Result := Default;
end;

function HashTypeToStr(const HashType: TJPHashType; StrUnknownHash: string = ''): string;
begin
  case HashType of
    htCrc16: Result := 'CRC16';
    htCrc24: Result := 'CRC24';
    htCrc32: Result := 'CRC32';
    htCrc64: Result := 'CRC64';
    htAdler32: Result := 'Adler32';

    htMd2: Result := 'MD2';
    htMd4: Result := 'MD4';
    htMd5: Result := 'MD5';
    htRipeMD: Result := 'RipeMD';
    htRipeMD128: Result := 'RipeMD-128';
    htRipeMD160: Result := 'RipeMD-160';
    htRipeMD256: Result := 'RipeMD-256';
    htRipeMD320: Result := 'RipeMD-320';

    htSha0: Result := 'SHA-0';
    htSha1: Result := 'SHA-1';
    htSha2_224: Result := 'SHA-2 224';
    htSha2_256: Result := 'SHA-2 256';
    htSha2_384: Result := 'SHA-2 384';
    htSha2_512: Result := 'SHA-2 512';
    htSha2_512_224: Result := 'SHA-2 512/224';
    htSha2_512_256: Result := 'SHA-2 512/256';

    htSha3_224: Result := 'SHA-3 224';
    htSha3_256: Result := 'SHA-3 256';
    htSha3_384: Result := 'SHA-3 384';
    htSha3_512: Result := 'SHA-3 512';

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
    Result := StrUnknownHash;
  end;
end;

function GetHashStrLength(HashType: TJPHashType; ErrorResult: integer = -1): integer;
begin
  case HashType of
    htCrc16: Result := HASH_LEN_CRC16;
    htCrc24: Result := HASH_LEN_CRC24;
    htCrc32: Result := HASH_LEN_CRC32;
    htCrc64: Result := HASH_LEN_CRC64;
    htAdler32: Result := HASH_LEN_ADLER32;

    htMd2: Result := HASH_LEN_MD2;
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

function GetHashDigestLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;
var
  StrLen: integer;
begin
  Result := ErrorResult;
  StrLen := GetHashStrLength(HashType, ErrorResult);
  if StrLen = ErrorResult then Exit;
  Result := StrLen div 2;
end;

function GetHashByteLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;
begin
  Result := GetHashDigestLength(HashType, ErrorResult);
end;

function GetHashBitLength(const HashType: TJPHashType; ErrorResult: integer = -1): integer;
var
  x: integer;
begin
  Result := ErrorResult;
  x := GetHashDigestLength(HashType, ErrorResult);
  if x = ErrorResult then Exit;
  Result := x * 8;
end;

function IsValidHashLen(const Hash: string; HashType: TJPHashType): Boolean;
var
  HashLen: integer;
begin
  HashLen := Length(Hash);
  if Odd(HashLen) then Result := False
  else Result := HashLen = GetHashStrLength(HashType, -1);
end;

function IsValidHashStr(const Hash: string): Boolean;
begin
  Result := IsValidHexStr(Hash, False);
end;

function CheckHash(Hash: string; HashType: TJPHashType; var HashCheckRec: THashCheckRec): Boolean;
begin
  HashCheckRec.ValidChars := IsValidHashStr(Hash);
  HashCheckRec.ValidLen := IsValidHashLen(Hash, HashType);
  HashCheckRec.ExpectedLen := GetHashStrLength(HashType);
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


{ THashResultRec }

procedure THashResultRec.Clear;
begin
  ValidHash := False;
  HashType := htNone;
  StrValueUpper := '';
  StrValueLower := '';
  IntValue := 0;
  Int64Value := 0;
  StreamSize := 0;
  ElapsedTimeMs := 0;
  SpeedMBperSec := 0;
end;

end.
