unit JPL.Utils;

interface


function IfThenStr(BoolValue: Boolean; const ResultIfTrue: string; ResultIfFalse: string = ''): string;
function IfThenInt(BoolValue: Boolean; const ResultIfTrue: integer; ResultIfFalse: integer = -1): integer;
function IfThenUInt(BoolValue: Boolean; const ResultIfTrue: UInt32; ResultIfFalse: UInt32 = 0): UInt32;
function IfThenInt64(BoolValue: Boolean; const ResultIfTrue: Int64; ResultIfFalse: Int64 = -1): Int64;
function IfThenUInt64(BoolValue: Boolean; const ResultIfTrue: UInt64; ResultIfFalse: UInt64 = 0): UInt64;
function IfThenFloat(BoolValue: Boolean; const ResultIfTrue: Double; ResultIfFalse: Double = -1): Double;

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

end.
