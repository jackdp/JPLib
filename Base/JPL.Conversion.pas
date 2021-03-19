unit JPL.Conversion;

{
  Jacek Pazera
  http://www.pazera-software.com

  To jest mój stary moduł z roku 2000 dla Borland Pascala 7.0
  W kolejnych latach rozbudowywany i dostosowywany do nowszych wersji Delphi i FPC.
 }


{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$ENDIF}

interface

uses
  {$IFDEF DCC} Windows, {$ENDIF}
  SysUtils,
  JPL.Strings;



function BoolToStr(b: Boolean; TrueStr: string = 'True'; FalseStr: string = 'False'): string;
function BoolToStrYesNo(const Value: Boolean): string;
function BoolToStrYN(const Value: Boolean): string;
function BoolToStr10(const Value: Boolean): string;
function BoolToStrOnOff(const Value: Boolean): string;
function BoolToStrTF(const Value: Boolean): string;
function StrToBool(s: string): Boolean;
function BoolToInt(const B: Boolean): integer;
function BoolToByte(const B: Boolean): Byte;

function IntToHexEx(const Value: integer; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;
function Int64ToHexEx(const Value: Int64; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;
function UInt64ToHexEx(const Value: UInt64; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;

function HexToInt(Hex: string; ErrorVal: integer = -1): integer;
function HexToInt64(Hex: string; ErrorVal: integer = -1): int64;
function HexToUInt(Hex: string): UInt32;
function HexToUInt64(Hex: string): UInt64;
function HexToBin(Hex: string): string;

function PowerInt(Base, Exponent: integer): integer;
function PowerInt64(Base, Exponent: Int64): Int64;
function PowerUInt(const Base, Exponent: UInt32): UInt32;
function PowerUInt64(const Base, Exponent: UInt32): UInt64;

function BinToInt(Bin: string): integer;
function TryBinToInt(BinStr: string; var xInt: Int64): Boolean;
function BinToInt64(Bin: string): int64;
function BinToUInt(Bin: string): UInt32;
function BinToUInt64(Bin: string): UInt64;
function BinToHex(Bin: string): string;

function IntToBin(Int: int64): string;
function IntToBinEx(Int: int64; Digits: byte): string;

function StrToDec(const s: string): integer;
function StrToDec64(const s: string): Int64;
function StrToUInt(const NumStr: string): UInt32;
function TryStrToUInt(const NumStr: string; out xResult: UInt32): Boolean;
function StrToUInt64(const NumStr: string): UInt64;
function TryStrToUInt64(const NumStr: string; out xResult: UInt64): Boolean;
function StrToWord(const NumStr: string): Word;
function TryStrToWord(const NumStr: string; out xResult: Word): Boolean;

function StrToDWord(const NumStr: string): DWORD;
function TryStrToDWord(const NumStr: string; out xResult: DWORD): Boolean;

function ByteToHex(B: byte): string;

function ftos(xr: Extended; RoundTo: integer = 2; DSep: string = ''): string;
function ftosEx(xr: Extended; Format: string = '0.00'; DecSeparator: string = '.'; ThousandSeparator: string = ''): string;
function stof(s: string; ErrorValue: Real = -1): Real;
function TryStoF(s: string; var x: Single): Boolean; overload;
function TryStoF(s: string; var x: Double): Boolean; overload;

{$IFDEF FPC}

{$IFNDEF HAS_UINTTOSTR}
function UIntToStr(Value: QWord): string; {$ifdef SYSUTILSINLINE}inline;{$ENDIF}
function UIntToStr(Value: Cardinal): string; {$ifdef SYSUTILSINLINE}inline;{$ENDIF}
{$ENDIF}

{$ENDIF}

function itos(const x: integer): string; overload;
function itos(const x: Int64): string; overload;
function itos(const x: Cardinal): string; overload;
function itos(const x: UInt64): string; overload;

function i64tos(const x: int64): string;
function stoi(const s: string): integer;
function stoi64(const s: string): int64;

function IsValidInteger(const s: string): Boolean;
function IsValidInteger64(const s: string): Boolean;
function IsValidFloat(const s: string): Boolean;

function MSecToTimeStr(xmsec: int64; bShowMSec: Boolean = True): string;

function GetIntInRange(const Value, Min, Max: integer): integer; overload;
function GetIntInRange(const Value, Min, Max: Int64): Int64; overload;
function GetIntInRange(const Value, Min, Max: SmallInt): SmallInt; overload;
function GetIntInRange(const Value, Min, Max: ShortInt): ShortInt; overload;
function GetFloatInRange(const Value, Min, Max: Single): Single; overload;
function GetFloatInRange(const Value, Min, Max: Real): Real; overload;
function GetFloatInRange(const Value, Min, Max: Double): Double; overload;
procedure LimitValue(var Value: integer; const Min, Max: integer); overload;
procedure LimitValue(var Value: Int64; const Min, Max: Int64); overload;

function IsValidBinStr(BinStr: string; IgnoreSpaces: Boolean = False): Boolean;
function IsValidHexStr(HexStr: string; IgnoreSpaces: Boolean = False): Boolean;
function IsValidIntStr(IntStr: string; IgnoreSpaces: Boolean = False): Boolean;
function IsValidFloatStr(FloatStr: string; IgnoreSpaces: Boolean = False): Boolean;

function TryStrToByte(s: string; var bt: Byte): Boolean;
function TryHexToInt(Hex: string; var xInt: Int64): Boolean; overload;
function TryHexToInt(Hex: string; var xInt: integer): Boolean; overload;
function TryHexToByte(Hex: string; var xb: Byte): Boolean;

function TryGetMilliseconds(const NumStr: string; out MilliSeconds: Int64): Boolean;



implementation

uses
  JPL.TStr;


function TryGetMilliseconds(const NumStr: string; out MilliSeconds: Int64): Boolean;
var
  s: string;
  dx64: Double;
  ux64: UInt64;
  bSeconds, bMinutes, bHours, bDays: Boolean;
begin
  Result := False;

  s := TStr.TrimAndLow(NumStr);
  s := TStr.TrimBounds(s, '"', '"');
  s := TStr.RemoveSpaces(s);

  bSeconds := False;
  bMinutes := False;
  bHours := False;
  bDays := False;

  if TStr.EndsStr('ms', s) then s := TStr.TrimFromEnd(s, 'ms')
  else if TStr.EndsStr('s', s) then
  begin
    bSeconds := True;
    s := TStr.TrimFromEnd(s, 's');
  end
  else if TStr.EndsStr('m', s) then
  begin
    bMinutes := True;
    s := TStr.TrimFromEnd(s, 'm');
  end
  else if TStr.EndsStr('h', s) then
  begin
    bHours := True;
    s := TStr.TrimFromEnd(s, 'h');
  end
  else if TStr.EndsStr('d', s) then
  begin
    bDays := True;
    s := TStr.TrimFromEnd(s, 'd');
  end
  else bSeconds := True;

  dx64 := 0;
  if (TStr.StartsWithHexPrefix(s)) or (Copy(s, 1, 1) = '%') then
  begin
    if not TryStrToUInt64(s, ux64) then Exit;
    dx64 := ux64;
  end
  else if not TryStoF(s, dx64) then Exit;

  if bSeconds then dx64 := dx64 * 1000
  else if bMinutes then dx64 := dx64 * 1000 * 60
  else if bHours then dx64 := dx64 * 1000 * 60 * 60
  else if bDays then dx64 := dx64 * 1000 * 60 * 60 * 24;

  //if dx64 > TIME_MAX then Exit;

  MilliSeconds := Int64(Round(dx64));
  Result := True;

end;

function TryHexToByte(Hex: string; var xb: Byte): Boolean;
var
  x: Int64;
begin
  Result := False;
  x := 0;
  if not TryHexToInt(Hex, x) then Exit;
  if (x < 0) or (x > 255) then Exit;
  Result := True;
  xb := Byte(x);
end;

function TryHexToInt(Hex: string; var xInt: integer): Boolean; overload;
var
  Code: integer;
  x: integer;
begin
  Result := True;
  if Hex = '' then Exit(False);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, x, Code);
  if Code = 0 then xInt := x
  else Result := False;
end;

function TryHexToInt(Hex: string; var xInt: Int64): Boolean;
var
  Code: integer;
  i64: int64;
begin
  Result := True;
  if Hex = '' then Exit(False);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, i64, Code);
  if Code = 0 then xInt := i64
  else Result := False;
end;

function TryStrToByte(s: string; var bt: Byte): Boolean;
var
  x: integer;
begin
  Result := False;
  if not TryStrToInt(s, x) then Exit;
  if not (x in [0..255]) then Exit;
  bt := Byte(x);
  Result := True;
end;

function GetFloatInRange(const Value, Min, Max: Double): Double;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetFloatInRange(const Value, Min, Max: Real): Real;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetFloatInRange(const Value, Min, Max: Single): Single;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetIntInRange(const Value, Min, Max: integer): integer;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetIntInRange(const Value, Min, Max: Int64): Int64;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetIntInRange(const Value, Min, Max: SmallInt): SmallInt;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

function GetIntInRange(const Value, Min, Max: ShortInt): ShortInt;
begin
  if Value < Min then Result := Min
  else if Value > Max then Result := Max
  else Result := Value;
end;

procedure LimitValue(var Value: integer; const Min, Max: integer);
begin
  if Value < Min then Value := Min
  else if Value > Max then Value := Max;
end;

procedure LimitValue(var Value: Int64; const Min, Max: Int64);
begin
  if Value < Min then Value := Min
  else if Value > Max then Value := Max;
end;


function IsValidFloatStr(FloatStr: string; IgnoreSpaces: Boolean = False): Boolean;
var
  i, xCount, xDots, xCommas: integer;
begin

  Result := False;
  if IgnoreSpaces then FloatStr := StrRemove(FloatStr, ' ');

  xCount := CharCount('-', FloatStr);
  if xCount > 1 then Exit;
  if (xCount = 1) and (FloatStr[1] <> '-') then Exit;

  if Copy(FloatStr, 1, 1) = '+' then Delete(FloatStr, 1, 1);

  FloatStr := UpperCase(FloatStr);
  if CharCount('E', FloatStr) > 1 then Exit;
  xDots := CharCount('.', FloatStr);
  xCommas := CharCount(',', FloatStr);

  if xDots > 1 then Exit;
  if xCommas > 1 then Exit;
  if xDots + xCommas = 2 then Exit;


  Result := True;

  for i := 1 to Length(FloatStr) do
    if not (CharInSet(FloatStr[i], ['0'..'9']) or (FloatStr[i] = '-') or (FloatStr[i] = 'E') or (FloatStr[i] = '.') or (FloatStr[i] = ',')) then
    begin
      Result := False;
      Break;
    end;
end;

function IsValidIntStr(IntStr: string; IgnoreSpaces: Boolean = False): Boolean;
var
  i, xm: integer;
begin
  Result := False;
  if IgnoreSpaces then IntStr := StrRemove(IntStr, ' ');

  xm := CharCount('-', IntStr);
  if xm > 1 then Exit;
  if (xm = 1) and (IntStr[1] <> '-') then Exit;

  if Copy(IntStr, 1, 1) = '+' then Delete(IntStr, 1, 1);

  Result := True;

  for i := 1 to Length(IntStr) do
    if not (
      CharInSet(IntStr[i], ['0'..'9']) or (IntStr[i] = '-')
    ) then
    begin
      Result := False;
      Break;
    end;

end;

function IsValidHexStr(HexStr: string; IgnoreSpaces: Boolean = False): Boolean;
var
  i: integer;
begin
  Result := True;
  if IgnoreSpaces then HexStr := StrRemove(HexStr, ' ');
  HexStr := UpperCase(HexStr);
  for i := 1 to Length(HexStr) do
    if not (
       CharInSet(HexStr[i], ['0'..'9'])  or  CharInSet(HexStr[i], ['A'..'F']) // or (HexStr[i] = '-')
    ) then
    begin
      Result := False;
      Break;
    end;
end;

function IsValidBinStr(BinStr: string; IgnoreSpaces: Boolean = False): Boolean;
var
  i: integer;
begin
  Result := True;
  if IgnoreSpaces then BinStr := StrRemove(BinStr, ' ');
  for i := 1 to Length(BinStr) do
    if not CharInSet(BinStr[i], ['0'..'1']) then
    begin
      Result := False;
      Break;
    end;
end;

{$IFDEF FPC}
{$IFNDEF HAS_UINTTOSTR}
function UIntToStr(Value: QWord): string;
begin
  Result := IntTostr(Value);
end;

function UIntToStr(Value: Cardinal): string;
begin
  System.Str(Value, Result);
end;
{$ENDIF}
{$ENDIF}

function itos(const x: integer): string;
begin
  Result := IntToStr(x);
end;

function itos(const x: Int64): string;
begin
  Result := IntToStr(x);
end;

function itos(const x: Cardinal): string;
begin
  Result := UIntToStr(x);
end;

function itos(const x: UInt64): string;
begin
  Result := UIntToStr(x);
end;

function i64tos(const x: int64): string;
begin
  Result := IntToStr(x);
end;

function stoi(const s: string): integer;
begin
  Result := StrToInt(s);
end;

function stoi64(const s: string): int64;
begin
  Result := StrToInt64(s);
end;

function MSecToTimeStr(xmsec: int64; bShowMSec: Boolean = True): string;
var
  Days, H, M, Sec, MSec: DWORD;
  sr: string;
begin
  Days := xmsec div 86400000;
  xmsec := xmsec - (int64(Days) * 86400000);

  H := xmsec div 3600000;
  xmsec := xmsec - (int64(H) * 3600000);

  M := xmsec div 60000;
  xmsec := xmsec - (int64(M) * 60000);

  Sec := xmsec div 1000;
  xmsec := xmsec - (int64(Sec) * 1000);

  MSec := xmsec;

  sr :=
    Pad(IntToStr(H), 2, '0') + ':' +
    Pad(IntToStr(M), 2, '0') + ':' +
    Pad(IntToStr(Sec), 2, '0');
  if bShowMSec and (MSec > 0) then sr := sr + '.' + Pad(IntToStr(MSec), 3, '0');
  if Days > 0 then sr := IntToStr(Days) + 'd ' + sr;
  Result := sr;
end;


function IsValidInteger(const s: string): Boolean;
begin
  try
    StrToInt(s);
    Result := True;
  except
    on EConvertError do Result := False;
  end;
end;

function IsValidInteger64(const s: string): Boolean;
begin
  try
    StrToInt64(s);
    Result := True;
  except
    on EConvertError do Result := False;
  end;
end;

function IsValidFloat(const s: string): Boolean;
begin
  try
    StrToFloat(s);
    Result := True;
  except
    on EConvertError do Result := False;
  end;
end;

function TryStoF(s: string; var x: Double): Boolean; overload;
begin
  s := StringReplace(s, ',', GetDecimalSeparator{%H-}, [rfReplaceAll]);
  s := StringReplace(s, '.', GetDecimalSeparator{%H-}, [rfReplaceAll]);
  Result := TryStrToFloat(s, x);
end;

function TryStoF(s: string; var x: Single): Boolean; overload;
begin
  s := StringReplace(s, ',', GetDecimalSeparator{%H-}, [rfReplaceAll]);
  s := StringReplace(s, '.', GetDecimalSeparator{%H-}, [rfReplaceAll]);
  Result := TryStrToFloat(s, x);
end;

function stof(s: string; ErrorValue: Real): Real;
begin
  s := StringReplace(s, ',', GetDecimalSeparator, [rfReplaceAll]);
  s := StringReplace(s, '.', GetDecimalSeparator, [rfReplaceAll]);
  try
    Result := StrToFloat(s);
  except
    Result := ErrorValue;
  end;
end;


function ftos(xr: Extended; RoundTo: integer = 2; DSep: string = ''): string;
var
  s, mask: string;
  i: integer;
begin

  if Round(xr) = xr then
    s := IntToStr(Round(xr))
  else
  begin
    mask := StringOfChar('0', RoundTo);
    mask := '0.' + mask;
    s := FormatFloat(mask, xr);
    for i := Length(s) downto 1 do
    begin
      if s[i] <> '0' then Break;
      Delete(s, i, 1);
    end;
  end;
  if s[Length(s)] = GetDecimalSeparator then Delete(s, Length(s), 1);
  if DSep <> '' then s := StringReplace(s, GetDecimalSeparator, DSep, []);
  Result := s;
end;

function ftosEx(xr: Extended; Format: string = '0.00'; DecSeparator: string = '.'; ThousandSeparator: string = ''): string;
var
  xp, i, Len, k: integer;
begin
  Result := FormatFloat(Format, xr);
  Result := StringReplace(Result, GetDecimalSeparator, DecSeparator, []);

  xp := Pos(DecSeparator, Result);
  if xp = 0 then xp := Length(Result);

  k := (xp - 1) div 3;
  Len := xp - 1;
  for i := 1 to k do Insert(ThousandSeparator, Result, Len - (i * 3) + 1);
  if Copy(Result, 1, Length(ThousandSeparator)) = ThousandSeparator then Delete(Result, 1, Length(ThousandSeparator));

end;


function ByteToHex(B: byte): string;
const
  hex: array[0..15] of Char = '0123456789ABCDEF';
begin
  Result := hex[B shr 4] + hex[B and 15];
end;

function BinToHex(Bin: string): string;
var
  int: integer;
begin
  int := BinToInt(Bin);
  Result := IntToHex(int, 0);
end;

function HexToBin(Hex: string): string;
var
  int: integer;
begin
  int := HexToInt(Hex);
  Result := IntToBin(int);
end;

function IntToBin(Int: int64): string;
var
  x, y: int64;
begin
  Result := '';
  x := Int;
  repeat
    y := x mod 2;
    x := x div 2;
    if y > 0 then
      Result := '1' + Result
    else
      Result := '0' + Result;
  until x = 0;
end;

function IntToBinEx(Int: int64; Digits: byte): string;
var
  bin: string;
  i: integer;
begin
  bin := IntToBin(Int);
  i := Digits - length(bin);
  if i > 0 then
    for i := 1 to (Digits - length(bin)) do
      bin := '0' + bin;
  Result := bin;
end;

function BinToInt(Bin: string): integer;
var
  LengthBin, deleted, i, x: integer;
  arr: array of integer; // = nil;
begin
  SetLength(arr, 0);
  Result := 0;
  if pos('1', Bin) > 0 then
  begin
    deleted := 0;
    LengthBin := length(Bin);
    repeat
      x := pos('1', Bin);
      deleted := deleted + x;
      SetLength(arr, length(arr) + 1);
      arr[length(arr) - 1] := LengthBin - deleted;
      Bin := copy(Bin, x + 1, length(Bin));
    until (pos('1', Bin) = 0);

    for i := 0 to length(arr) - 1 do
      Result := Result + PowerInt(2, arr[i]);
  end;
end;

function TryBinToInt(BinStr: string; var xInt: Int64): Boolean;
var
  LengthBin, Deleted, i, x: integer;
  arr: array of Int64;
begin
  Result := True;
  try

    SetLength(arr, 0);

    xInt := 0;
    if Pos('1', BinStr) > 0 then
    begin
      Deleted := 0;
      LengthBin := Length(BinStr);
      repeat
        x := Pos('1', BinStr);
        Deleted := Deleted + x;
        SetLength(arr, Length(arr) + 1);
        arr[Length(arr) - 1] := Int64(LengthBin) - Int64(Deleted);
        BinStr := Copy(BinStr, x + 1, Length(BinStr));
      until (Pos('1', BinStr) = 0);

      for i := 0 to Length(arr) - 1 do
        xInt := xInt + PowerInt64(2, arr[i]);
      if xInt < 0 then Result := False;
    end;

  except
    Result := False;
  end;
end;

function BinToInt64(Bin: string): int64;
var
  LengthBin, deleted, i, x: integer;
  arr: array of integer; // = nil;
begin
  SetLength(arr, 0);
  Result := 0;
  if pos('1', Bin) > 0 then
  begin
    deleted := 0;
    LengthBin := length(Bin);
    repeat
      x := pos('1', Bin);
      deleted := deleted + x;
      SetLength(arr, length(arr) + 1);
      arr[length(arr) - 1] := LengthBin - deleted;
      Bin := copy(Bin, x + 1, length(Bin));
    until (pos('1', Bin) = 0);

    for i := 0 to length(arr) - 1 do
      Result := Result + PowerInt(2, arr[i]);
  end;
end;

function BinToUInt(Bin: string): UInt32;
var
  LengthBin, deleted, i, x: integer;
  arr: array of integer;
begin
  SetLength(arr, 0);
  Result := 0;
  if pos('1', Bin) > 0 then
  begin
    deleted := 0;
    LengthBin := length(Bin);
    repeat
      x := pos('1', Bin);
      deleted := deleted + x;
      SetLength(arr, length(arr) + 1);
      arr[length(arr) - 1] := LengthBin - deleted;
      Bin := copy(Bin, x + 1, length(Bin));
    until (pos('1', Bin) = 0);

    for i := 0 to length(arr) - 1 do
      Result := Result + PowerUInt(2, arr[i]);
  end;
end;

function BinToUInt64(Bin: string): UInt64;
var
  LengthBin, deleted, i, x: integer;
  arr: array of integer;
begin
  SetLength(arr, 0);
  Result := 0;
  if pos('1', Bin) > 0 then
  begin
    deleted := 0;
    LengthBin := length(Bin);
    repeat
      x := pos('1', Bin);
      deleted := deleted + x;
      SetLength(arr, length(arr) + 1);
      arr[length(arr) - 1] := LengthBin - deleted;
      Bin := copy(Bin, x + 1, length(Bin));
    until (pos('1', Bin) = 0);

    for i := 0 to length(arr) - 1 do
      Result := Result + PowerUInt64(2, arr[i]);
  end;
end;

function PowerInt(Base, Exponent: integer): integer;
var
  x, i: integer;
begin
  if Exponent = 0 then Result := 1
  else if Exponent = 1 then Result := Base
  else
  begin
    x := Base;
    for i := 1 to Exponent - 1 do Base := Base * x;
    Result := Base;
  end;
end;

function PowerInt64(Base, Exponent: Int64): Int64;
var
  x, i: integer;
begin
  if Exponent = 0 then Result := 1
  else if Exponent = 1 then Result := Base
  else
  begin
    x := Base;
    for i := 1 to Exponent - 1 do Base := Base * x;
    Result := Base;
  end;
end;

function PowerUInt(const Base, Exponent: UInt32): UInt32;
var
  i: integer;
  x, xb: UInt32;
begin
  if Exponent = 0 then Result := 1
  else if Exponent = 1 then Result := Base
  else
  begin
    x := Base;
    xb := Base;
    for i := 1 to Exponent - 1 do xb := xb * x;
    Result := xb;
  end;
end;

function PowerUInt64(const Base, Exponent: UInt32): UInt64;
var
  i: integer;
  x, xb: UInt64;
begin
  if Exponent = 0 then Result := 1
  else if Exponent = 1 then Result := Base
  else
  begin
    x := Base;
    xb := Base;
    for i := 1 to Exponent - 1 do xb := xb * x;
    Result := xb;
  end;
end;


function IntToHexEx(const Value: integer; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;
var
  s: string;
begin
  s := IntToHex(Value, HexDigits);
  if bLowerCase then s := LowerCase(s);

  if TrimLeadingZeros then
  begin
    while True do
    begin
      if s = '' then Break;
      if s[1] <> '0' then Break;
      Delete(s, 1, 1);
    end;
    if Odd(Length(s)) then s := '0' + s;
    if s = '' then s := '00';
  end
  else if Odd(Length(s)) then s := '0' + s;

  if BytesSeparator <> '' then s := InsertNumSep(s, BytesSeparator, 2);
  s := HexPrefix + s;
  Result := s;
end;

function Int64ToHexEx(const Value: Int64; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;
var
  s: string;
begin
  s := IntToHex(Value, HexDigits);
  if bLowerCase then s := LowerCase(s);

  if TrimLeadingZeros then
  begin
    while True do
    begin
      if s = '' then Break;
      if s[1] <> '0' then Break;
      Delete(s, 1, 1);
    end;
    if Odd(Length(s)) then s := '0' + s;
    if s = '' then s := '00';
  end
  else if Odd(Length(s)) then s := '0' + s;

  if BytesSeparator <> '' then s := InsertNumSep(s, BytesSeparator, 2);
  s := HexPrefix + s;
  Result := s;
end;

function UInt64ToHexEx(const Value: UInt64; HexDigits: integer = 2; HexPrefix: string = '$'; bLowerCase: Boolean = True; BytesSeparator: string = '';
  TrimLeadingZeros: Boolean = False): string;
var
  s: string;
begin
  s := IntToHex(Value, HexDigits);
  if bLowerCase then s := LowerCase(s);

  if TrimLeadingZeros then
  begin
    while True do
    begin
      if s = '' then Break;
      if s[1] <> '0' then Break;
      Delete(s, 1, 1);
    end;
    if Odd(Length(s)) then s := '0' + s;
    if s = '' then s := '00';
  end;

  if BytesSeparator <> '' then s := InsertNumSep(s, BytesSeparator, 2);
  s := HexPrefix + s;
  Result := s;
end;

{$hints off}
function HexToInt(Hex: string; ErrorVal: integer): integer;
var
  code, int: integer;
begin
  Hex := StringReplace(Hex, '0x', '$', [rfIgnoreCase]);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, int, code);
  if code = 0 then Result := int
  else Result := ErrorVal;
end;
{$hints on}

{$hints off}
function HexToInt64(Hex: string; ErrorVal: integer): int64;
var
  code: integer;
  i64: int64;
begin
  Hex := StringReplace(Hex, '0x', '$', [rfIgnoreCase]);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, i64, code);
  if code = 0 then Result := i64
  else Result := ErrorVal;
end;
{$hints on}

function HexToUInt(Hex: string): UInt32;
var
  code: integer;
begin
  Hex := StringReplace(Hex, '0x', '$', [rfIgnoreCase]);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, Result, code);
  if code <> 0 then raise EConvertError.Create('HexToUInt: Cannot convert "' + Hex + '" to UInt32');
end;

function HexToUInt64(Hex: string): UInt64;
var
  code: integer;
begin
  Hex := StringReplace(Hex, '0x', '$', [rfIgnoreCase]);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, Result, code);
  if code <> 0 then raise EConvertError.Create('HexToUInt64: Cannot convert "' + Hex + '" to UInt64');
end;

function StrToDec(const s: string): integer;
var
  s2: string;
begin
  s2 := RemoveSpaces(s);
  if Copy(s2, 1, 1) = '$' then Result := HexToInt(s2)
  else if Copy(s2, 1, 1) = '%' then Result := BinToInt(s2)
  else if UpperCase(Copy(s, 1, 2)) = '0X' then Result := HexToInt(Copy(s, 3, Length(s)))
  else Result := StrToInt(s2);
end;

function StrToDec64(const s: string): Int64;
var
  s2: string;
begin
  s2 := RemoveSpaces(s);
  if Copy(s2, 1, 1) = '$' then Result := HexToInt(s2)
  else if Copy(s2, 1, 1) = '%' then Result := BinToInt64(s2)
  else if UpperCase(Copy(s, 1, 2)) = '0X' then Result := HexToInt64(Copy(s, 3, Length(s)))
  else Result := StrToInt64(s2);
end;

function StrToUInt(const NumStr: string): UInt32;
var
  s2: string;
  code: integer;
begin
  s2 := RemoveSpaces(NumStr);
  if Copy(s2, 1, 1) = '$' then Result := HexToUInt(s2)
  else if Copy(s2, 1, 1) = '%' then Result := BinToUInt(s2)
  else if UpperCase(Copy(s2, 1, 2)) = '0X' then Result := HexToUInt(s2)
  else
  begin
    Val(s2, Result, code);
    if code <> 0 then raise EConvertError.Create('StrToUInt: Cannot convert "' + NumStr + '" to UInt');
  end;
end;

function TryStrToUInt(const NumStr: string; out xResult: UInt32): Boolean;
begin
  try
    xResult := StrToUInt(NumStr);
    Result := True;
  except
    Result := False;
  end;
end;

function StrToUInt64(const NumStr: string): UInt64;
var
  s2: string;
  code: integer;
begin
  s2 := RemoveSpaces(NumStr);
  if Copy(s2, 1, 1) = '$' then Result := HexToUInt64(s2)
  else if Copy(s2, 1, 1) = '%' then Result := BinToUInt64(s2)
  else if UpperCase(Copy(s2, 1, 2)) = '0X' then Result := HexToUInt64(s2)
  else
  begin
    Val(s2, Result, code);
    if code <> 0 then raise EConvertError.Create('StrToUInt64: Cannot convert "' + NumStr + '" to UInt64');
  end;
end;

function TryStrToUInt64(const NumStr: string; out xResult: UInt64): Boolean;
begin
  try
    xResult := StrToUInt64(NumStr);
    Result := True;
  except
    Result := False;
  end;
end;

function StrToWord(const NumStr: string): Word;
var
  s2: string;
  code: integer;
begin
  s2 := RemoveSpaces(NumStr);
  Val(s2, Result, code);
  if code <> 0 then raise EConvertError.Create('StrToWord: Cannot convert "' + NumStr + '" to WORD');
end;

function TryStrToWord(const NumStr: string; out xResult: Word): Boolean;
begin
  try
    xResult := StrToWord(NumStr);
    Result := True;
  except
    Result := False;
  end;
end;

function StrToDWord(const NumStr: string): DWORD;
var
  s2: string;
  code: integer;
begin
  s2 := RemoveSpaces(NumStr);
  Val(s2, Result, code);
  if code <> 0 then raise EConvertError.Create('StrToWord: Cannot convert "' + NumStr + '" to WORD');
end;

function TryStrToDWord(const NumStr: string; out xResult: DWORD): Boolean;
begin
  try
    xResult := StrToDWord(NumStr);
    Result := True;
  except
    Result := False;
  end;
end;

function BoolToStr(b: Boolean; TrueStr: string = 'True'; FalseStr: string = 'False'): string;
begin
  if b then Result := TrueStr
  else Result := FalseStr;
end;

function BoolToStr10(const Value: Boolean): string;
begin
  Result := BoolToStr(Value, '1', '0');
end;

function BoolToStrYesNo(const Value: Boolean): string;
begin
  Result := BoolToStr(Value, 'Yes', 'No');
end;

function BoolToStrYN(const Value: Boolean): string;
begin
  Result := BoolToStrYesNo(Value);
end;

function BoolToStrOnOff(const Value: Boolean): string;
begin
  Result := BoolToStr(Value, 'On', 'Off');
end;

function BoolToStrTF(const Value: Boolean): string;
begin
  Result := BoolToStr(Value, 'True', 'False');
end;


{$hints off}
function StrToBool(s: string): Boolean;
begin
  s := UpperCase(Trim(s));
  Result :=
    (s = '1') or (s = 'TRUE') or (s = 'YES') or //(s = 'TAK') or (s = 'PRAWDA') or
    (s = 'ON') or (s = 'ENABLED'); // or (s = 'T');
end;
{$hints on}

function BoolToInt(const B: Boolean): integer;
begin
  if B then Result := 1 else Result := 0;
end;

function BoolToByte(const B: Boolean): Byte;
begin
  if B then Result := 1 else Result := 0;
end;


end.
