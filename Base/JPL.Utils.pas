unit JPL.Utils;

{
  Jacek Pazera
  http://www.pazera-software.com
}

{$I .\..\jp.inc}
{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface



function IfThenStr(BoolValue: Boolean; const ResultIfTrue: string; ResultIfFalse: string = ''): string;
function IfThenInt(BoolValue: Boolean; const ResultIfTrue: integer; ResultIfFalse: integer = -1): integer;
function IfThenUInt(BoolValue: Boolean; const ResultIfTrue: UInt32; ResultIfFalse: UInt32 = 0): UInt32;
function IfThenInt64(BoolValue: Boolean; const ResultIfTrue: Int64; ResultIfFalse: Int64 = -1): Int64;
function IfThenUInt64(BoolValue: Boolean; const ResultIfTrue: UInt64; ResultIfFalse: UInt64 = 0): UInt64;
function IfThenFloat(BoolValue: Boolean; const ResultIfTrue: Double; ResultIfFalse: Double = -1): Double;

function NumberIn(const Number: integer; IntValArray: array of integer): Boolean; overload;
function NumberIn(const Number: Int64; Int64ValArray: array of integer): Boolean; overload;
function NumberIn(const Number: Cardinal; CardinalValArray: array of Cardinal): Boolean; overload;
function NumberIn(const Number: Double; DoubleValArray: array of Double): Boolean; overload;



implementation



function IfThenStr(BoolValue: Boolean; const ResultIfTrue: string; ResultIfFalse: string = ''): string;
begin
  if BoolValue then Result := ResultIfTrue
  else Result := ResultIfFalse;
end;

function IfThenInt(BoolValue: Boolean; const ResultIfTrue: integer; ResultIfFalse: integer = -1): integer;
begin
  if BoolValue then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function IfThenUInt(BoolValue: Boolean; const ResultIfTrue: UInt32; ResultIfFalse: UInt32 = 0): UInt32;
begin
  if BoolValue then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function IfThenInt64(BoolValue: Boolean; const ResultIfTrue: Int64; ResultIfFalse: Int64 = -1): Int64;
begin
  if BoolValue then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function IfThenUInt64(BoolValue: Boolean; const ResultIfTrue: UInt64; ResultIfFalse: UInt64 = 0): UInt64;
begin
  if BoolValue then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function IfThenFloat(BoolValue: Boolean; const ResultIfTrue: Double; ResultIfFalse: Double = -1): Double;
begin
  if BoolValue then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function NumberIn(const Number: integer; IntValArray: array of integer): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(IntValArray) - 1 do
    if IntValArray[i] = Number then
    begin
      Result := True;
      Break;
    end;
end;

function NumberIn(const Number: Int64; Int64ValArray: array of integer): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(Int64ValArray) - 1 do
    if Int64ValArray[i] = Number then
    begin
      Result := True;
      Break;
    end;
end;

function NumberIn(const Number: Cardinal; CardinalValArray: array of Cardinal): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(CardinalValArray) - 1 do
    if CardinalValArray[i] = Number then
    begin
      Result := True;
      Break;
    end;
end;

function NumberIn(const Number: Double; DoubleValArray: array of Double): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(DoubleValArray) - 1 do
    if DoubleValArray[i] = Number then
    begin
      Result := True;
      Break;
    end;
end;




end.
