unit JPL.Conversion;

{
  Jacek Pazera
  http://www.pazera-software.com

  To jest mÛj stary modu≥ z roku 2000 dla Borland Pascala 7.0
  W kolejnych latach rozbudowywany i dostosowywany do nowszych wersji Delphi i FPC.
 }


{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC} Windows, {$ENDIF}
  SysUtils,
  //Classes,
  //Graphics,
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

function IsoToWin(const Text: string): string;
function IsoToWin2(const Text: string): string;
function WinToIso(const Text: string): string;
function WinToIso1(s: string): string;
//function ConvertFileIsoToWin(const FileName: string): Boolean;

function HexToInt(Hex: string; ErrorVal: integer = -1): integer;
function HexToInt64(Hex: string; ErrorVal: integer = -1): int64;
function PowerInt(Base, Exponent: integer): integer;
function BinToInt(Bin: string): integer;
function BinToInt64(Bin: string): int64;
function IntToBin(Int: int64): string;
function IntToBinEx(Int: int64; Digits: byte): string;
function HexToBin(Hex: string): string;
function BinToHex(Bin: string): string;
function StrToDec(const s: string): integer;
function StrToDec64(const s: string): int64;

function ByteToHex(B: byte): string;

function ftos(xr: Extended; RoundTo: integer = 2; DSep: string = ''): string;
function ftosEx(xr: Extended; Format: string = '0.00'; DecSeparator: string = '.'; ThousandSeparator: string = ''): string;
function stof(s: string; ErrorValue: Real = -1): Real;
function TryStoF(s: string; var x: Single): Boolean; overload;
function TryStoF(s: string; var x: Double): Boolean; overload;
//function TryStoF(s: string; var x: Extended): Boolean; overload;

function itos(x: integer): string;
function i64tos(x: int64): string;
function stoi(s: string): integer;
function stoi64(s: string): int64;

function IsValidInteger(const s: string): Boolean;
function IsValidInteger64(const s: string): Boolean;
function IsValidFloat(const s: string): Boolean;

//function StringToColorEx(s: string; DefaultColor: TColor = clBlack): TColor;


function MSecToTimeStr(xmsec: int64; bShowMSec: Boolean = True): string;

function GetIntInRange(const Value, Min, Max: integer): integer; overload;
function GetIntInRange(const Value, Min, Max: Int64): Int64; overload;
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



implementation


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
    if not (
      CharInSet(FloatStr[i], ['0'..'9']) or (FloatStr[i] = '-') or (FloatStr[i] = 'E') or (FloatStr[i] = '.') or (FloatStr[i] = ',')
    ) then
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

function itos(x: integer): string;
begin
  Result := IntToStr(x);
end;

function i64tos(x: int64): string;
begin
  Result := IntToStr(x);
end;

function stoi(s: string): integer;
begin
  Result := StrToInt(s);
end;

function stoi64(s: string): int64;
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


//function StringToColorEx(s: string; DefaultColor: TColor = clBlack): TColor;
//var
//  xp: integer;
//  R, G, B: Byte;
//  sc: string;
//begin
//  Result := DefaultColor;
//
//  s := Trim(s);
//  s := StringReplace(s, ' ', '', [rfReplaceAll]);
//  s := LowerCase(s);
//
//  if Copy(s, 1, 4) = 'rgb(' then
//  begin
//    s := StringReplace(s, ')', '', [rfReplaceAll]);
//    s := Copy(s, 5, Length(s));
//    xp := Pos(',', s);
//    if xp <= 0 then Exit;
//    sc := Copy(s, 1, xp - 1);
//    if not IsValidInteger(sc) then Exit;
//    R := StrToInt(sc);
//    s := Copy(s, xp + 1, Length(s));
//    xp := Pos(',', s);
//    if xp <= 0 then Exit;
//    sc := Copy(s, 1, xp - 1);
//    if not IsValidInteger(sc) then Exit;
//    G := StrToInt(sc);
//    s := Copy(s, xp + 1, Length(s));
//    if not IsValidInteger(s) then Exit;
//    B := StrToInt(s);
//    Result := RGB(R, G, B);
//
//    Exit;
//  end;
//
//  if IsValidInteger(s) then
//  begin
//    Result := StringToColor(s);
//    Exit;
//  end;
//
//  if LowerCase(Copy(s, 1, 2)) <> 'cl' then s := 'cl' + s;
//  try
//    Result := StringToColor(s);
//  except
//  end;
//end;



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
{var
  e: extended;
  code: integer;  }
begin
  try
    StrToFloat(s);
    Result := True;
  except
    on EConvertError do
      Result := False;
  end;
 { Val(s, e, code);
  if code = 0 then
    Result := True
  else
    Result := False;   }
end;


//function TryStoF(s: string; var x: Extended): Boolean; overload;
//begin
//  s := StringReplace(s, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
//  s := StringReplace(s, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
//  Result := TryStrToFloat(s, x);
//end;

function TryStoF(s: string; var x: Double): Boolean; overload;
begin
  s := StringReplace(s, ',', FormatSettings.DecimalSeparator{%H-}, [rfReplaceAll]);
  s := StringReplace(s, '.', FormatSettings.DecimalSeparator{%H-}, [rfReplaceAll]);
  Result := TryStrToFloat(s, x);
end;

function TryStoF(s: string; var x: Single): Boolean; overload;
begin
  s := StringReplace(s, ',', FormatSettings.DecimalSeparator{%H-}, [rfReplaceAll]);
  s := StringReplace(s, '.', FormatSettings.DecimalSeparator{%H-}, [rfReplaceAll]);
  Result := TryStrToFloat(s, x);
end;

function stof(s: string; ErrorValue: Real): Real;
begin
  s := StringReplace(s, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  s := StringReplace(s, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
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
  if s[Length(s)] = FormatSettings.DecimalSeparator then Delete(s, Length(s), 1);
  if DSep <> '' then s := StringReplace(s, FormatSettings.DecimalSeparator, DSep, []);
  Result := s;
end;


function ftosEx(xr: Extended; Format: string = '0.00'; DecSeparator: string = '.'; ThousandSeparator: string = ''): string;
var
  xp, i, Len, k: integer;
begin
  Result := FormatFloat(Format, xr);
  Result := StringReplace(Result, FormatSettings.DecimalSeparator, DecSeparator, []);

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

function PowerInt(Base, Exponent: integer): integer;
var
  x, i: integer;
begin
  if Exponent = 0 then
    Result := 1
  else if Exponent = 1 then
    Result := Base
  else
  begin
    x := Base;
    for i := 1 to Exponent - 1 do
      Base := Base * x;
    Result := Base;
  end;
end;

{$hints off}
function HexToInt(Hex: string; ErrorVal: integer): integer;
var
  code, int: integer;
begin
  Hex := StringReplace(Hex, '0x', '$', []);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  val(Hex, int, code);
  if code = 0 then Result := int
  else Result := ErrorVal;
{  else
    raise Exception.Create('Internal Error.' + #10#13 +
      'HexToInt(' + Hex + ')' + #10#13 +
      'position: ' + IntToStr(code));    }
end;
{$hints on}

{$hints off}
function HexToInt64(Hex: string; ErrorVal: integer): int64;
var
  code: integer;
  i64: int64;
begin
  if Hex[1] <> '$' then Hex := '$' + Hex;
  val(Hex, i64, code);
  if code = 0 then Result := i64
  else Result := ErrorVal;
{  else
    raise Exception.Create('Internal Error.' + #10#13 +
      'HexToInt64(' + Hex + ')' + #10#13 +
      'position: ' + IntToStr(code));   }
end;
{$hints on}


function StrToDec(const s: string): integer;
var
  s2: string;
begin
  s2 := Trim(s);
  if Copy(s2, 1, 1) = '$' then
    Result := HexToInt(s2)
  else if Copy(s2, 1, 1) = '%' then
    Result := BinToInt(s2)
  else if UpperCase(Copy(s, 1, 2)) = '0X' then
    Result := HexToInt(Copy(s, 3, Length(s)))
  else
    Result := StrToInt(s2);
end;


function StrToDec64(const s: string): int64;
var
  s2: string;
begin
  s2 := Trim(s);
  if Copy(s2, 1, 1) = '$' then
    Result := HexToInt(s2)
  else if Copy(s2, 1, 1) = '%' then
    Result := BinToInt64(s2)
  else if UpperCase(Copy(s, 1, 2)) = '0X' then
    Result := HexToInt64(Copy(s, 3, Length(s)))
  else
    Result := StrToInt64(s2);
end;

//
//function ConvertFileIsoToWin(const FileName: string): Boolean;
//var
//  sl: TStringList;
//begin
//  if not FileExists(FileName) then
//  begin
//    Result := False;
//    Exit;
//  end;
//
//  Result := True;
//  sl := TStringList.Create;
//  try
//    sl.LoadFromFile(FileName);
//    sl.Text := IsoToWin(sl.Text);
//    sl.SaveToFile(FileName);
//  finally
//    sl.Free;
//  end;
//end;



function IsoToWin(const Text: string): string;
{var
  i: integer;
  znak: Char;  }
begin
  Result := IsoToWin2(Text);
{
  for i := 1 to Length(Text) do
  begin
    case Text[i] of
      '°': znak := '•';
      '¶': znak := 'å';
      '¨': znak := 'è';
      '±': znak := 'π';
      '∂': znak := 'ú';
      'º': znak := 'ü';
    else
      znak := Text[i];
    end;

    Result := Result + znak;
  end;
}
end;


function IsoToWin2(const Text: string): string;
begin
  Result := Text;
  Result := StringReplace(Result, '°', '•', [rfReplaceAll]);
  Result := StringReplace(Result, '¶', 'å', [rfReplaceAll]);
  Result := StringReplace(Result, '¨', 'è', [rfReplaceAll]);
  Result := StringReplace(Result, '±', 'π', [rfReplaceAll]);
  Result := StringReplace(Result, '∂', 'ú', [rfReplaceAll]);
  Result := StringReplace(Result, 'º', 'ü', [rfReplaceAll]);
end;


function WinToIso(const Text: string): string;
var
  i: integer;
  znak: Char;
begin
  Result := '';
  for i := 1 to Length(Text) do
  begin
    case Text[i] of
      '•': znak := '°';
      'å': znak := '¶';
      'è': znak := '¨';
      'π': znak := '±';
      'ú': znak := '∂';
      'ü': znak := 'º';
    else
      znak := Text[i];
    end;

    Result := Result + znak;
  end;
end;

{$hints off}
function WinToIso1(s: string): string;
begin
  s := StringReplace(s, 'π', 'a', [rfReplaceAll]);
  s := StringReplace(s, 'Ê', 'c', [rfReplaceAll]);
  s := StringReplace(s, 'Í', 'e', [rfReplaceAll]);
  s := StringReplace(s, '≥', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'Ò', 'n', [rfReplaceAll]);
  s := StringReplace(s, 'ú', 's', [rfReplaceAll]);
  s := StringReplace(s, 'ø', 'z', [rfReplaceAll]);
  s := StringReplace(s, 'ü', 'z', [rfReplaceAll]);

  s := StringReplace(s, '•', 'A', [rfReplaceAll]);
  s := StringReplace(s, '∆', 'C', [rfReplaceAll]);
  s := StringReplace(s, ' ', 'E', [rfReplaceAll]);
  s := StringReplace(s, '£', 'L', [rfReplaceAll]);
  s := StringReplace(s, '—', 'N', [rfReplaceAll]);
  s := StringReplace(s, 'å', 'S', [rfReplaceAll]);
  s := StringReplace(s, 'Ø', 'Z', [rfReplaceAll]);
  s := StringReplace(s, 'è', 'Z', [rfReplaceAll]);

  Result := s;
end;
{$hints on}





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
    (s = '1') or (s = 'TRUE') or (s = 'YES') or (s = 'TAK') or (s = 'PRAWDA') or
    (s = 'ON') or (s = 'ENABLED') or (s = 'T');
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
