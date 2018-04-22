unit JPL.Math;


interface

uses SysUtils;


function pot(liczba: Extended; wykladnik: Extended): Extended;

function sinSt(x: Extended): Extended;
function cosSt(x: Extended): Extended;
function tgSt(x: Extended): Extended;
function ctgSt(x: Extended): Extended;

function sinGr(x: Extended): Extended;
function cosGr(x: Extended): Extended;
function tgGr(x: Extended): Extended;
function ctgGr(x: Extended): Extended;

function sinRad(x: Extended): Extended;
function cosRad(x: Extended): Extended;
function tgRad(x: Extended): Extended;
function ctgRad(x: Extended): Extended;

function tg(x: Extended): Extended;
function ctg(x: Extended): Extended;

function GetNearestMultiple(xNum, xMultiple: integer): integer;


// Jaki procent wartości x100Percent stanowi Value
function PercentValue(Value, x100Percent: Double): Real;
function PercentValueStr(Value, x100Percent: Double; Digits: integer = 2): string;

// Ile wynosi procent Percent wartości x100Percent
function PercentOf(Percent: Double; x100PercentValue: Double): Double; overload;
function PercentOf(Percent: integer; x100PercentValue: Double): integer; overload;

function InRange(const Value, Min, Max: integer): Boolean;

var
  rad: Extended;



implementation


function PercentOf(Percent: integer; x100PercentValue: Double): integer;
begin
  Result := Round(Percent * x100PercentValue / 100);
end;

function InRange(const Value, Min, Max: integer): Boolean;
begin
  Result := (Value >= Min) and (Value <= Max);
end;

function PercentOf(Percent: Double; x100PercentValue: Double): Double;
begin
  Result := (Percent * x100PercentValue) / 100;
end;

function PercentValue(Value, x100Percent: Double): Real;
begin
  if x100Percent = 0 then Result := 0
  else Result := Value * 100 / x100Percent;
end;

function PercentValueStr(Value, x100Percent: Double; Digits: integer = 2): string;
var
  s: string;
  i: integer;
begin
  s := '0.';
  for i := 1 to Digits do s := s + '0';
  Result := FormatFloat(s, PercentValue(Value, x100Percent));
end;


function GetNearestMultiple(xNum, xMultiple: integer): integer;
begin
  if xMultiple = 0 then Result := 0
  else if (xNum mod xMultiple = 0) then Result := xNum
  else Result := Round(xNum / xMultiple) * xMultiple;
end;


{------------------------------------------------------------}

function pot(liczba: Extended; wykladnik: Extended): Extended;
begin
  pot := exp(wykladnik * ln(liczba));
end;



{------------------------------------------------------------}

function sinSt(x: Extended): Extended;
begin
  rad := x * pi / 180;
  sinSt := sin(rad);
end;

function cosSt(x: Extended): Extended;
begin
  rad := x * pi / 180;
  cosSt := cos(rad);
end;

function tgSt(x: Extended): Extended;
begin
  rad := x * pi / 180;
  tgSt := sin(rad) / cos(rad);
end;

function ctgSt(x: Extended): Extended;
begin
  rad := x * pi / 180;
  ctgSt := cos(rad) / sin(rad);
end;



{------------------------------------------------------------}

function sinGr(x: Extended): Extended;
begin
  rad := x * pi / 200;
  sinGr := sin(rad);
end;

function cosGr(x: Extended): Extended;
begin
  rad := x * pi / 200;
  cosGr := cos(rad);
end;

function tgGr(x: Extended): Extended;
begin
  rad := x * pi / 200;
  tgGr := sin(rad) / cos(rad);
end;

function ctgGr(x: Extended): Extended;
begin
  rad := x * pi / 200;
  ctgGr := cos(rad) / sin(rad);
end;




{------------------------------------------------------------}

function sinRad(x: Extended): Extended;
begin
  sinRad := sin(x);
end;

function cosRad(x: Extended): Extended;
begin
  cosRad := cos(x);
end;

function tgRad(x: Extended): Extended;
begin
  tgRad := sin(x) / cos(x);
end;

function ctgRad(x: Extended): Extended;
begin
  ctgRad := cos(x) / sin(x);
end;




{------------------------------------------------------------}

function tg(x: Extended): Extended;
begin
  tg := sin(x) / cos(x);
end;

function ctg(x: Extended): Extended;
begin
  ctg := cos(x) / sin(x);
end;



end.

