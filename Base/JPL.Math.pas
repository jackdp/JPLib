unit JPL.Math;


interface

uses SysUtils;


function pot(const liczba, wykladnik: Extended): Extended;

function SinDeg(const x: Extended): Extended;
function CosDeg(const x: Extended): Extended;
function TgDeg(const x: Extended): Extended;
function CtgDeg(const x: Extended): Extended;

function SinGrad(const x: Extended): Extended;
function CosGrad(const x: Extended): Extended;
function TgGrad(const x: Extended): Extended;
function CtgGrad(const x: Extended): Extended;

function SinRad(const x: Extended): Extended;
function CosRad(const x: Extended): Extended;
function TgRad(const x: Extended): Extended;
function CtgRad(const x: Extended): Extended;

//function tg(const x: Extended): Extended;
//function ctg(const x: Extended): Extended;

function GetNearestMultiple(xNum, xMultiple: integer): integer;


// Jaki procent wartości x100Percent stanowi Value
function PercentValue(const Value, x100Percent: Double): Double;
function PercentValueStr(const Value, x100Percent: Double; Digits: integer = 2): string;

// Ile wynosi procent Percent wartości x100Percent
function PercentOf(const Percent, x100PercentValue: Double): Double; overload;
function PercentOf(const Percent: integer; const x100PercentValue: Double): integer; overload;

function InRange(const Value, Min, Max: integer): Boolean;



implementation


function PercentOf(const Percent: integer; const x100PercentValue: Double): integer;
begin
  Result := Round(Percent * x100PercentValue / 100);
end;

function InRange(const Value, Min, Max: integer): Boolean;
begin
  Result := (Value >= Min) and (Value <= Max);
end;

function PercentOf(const Percent, x100PercentValue: Double): Double;
begin
  Result := (Percent * x100PercentValue) / 100;
end;

function PercentValue(const Value, x100Percent: Double): Double;
begin
  if x100Percent = 0 then Result := 0
  else Result := Value * 100 / x100Percent;
end;

function PercentValueStr(const Value, x100Percent: Double; Digits: integer = 2): string;
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

function pot(const liczba, wykladnik: Extended): Extended;
begin
  pot := exp(wykladnik * ln(liczba));
end;



{------------------------------------------------------------}

function SinDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  SinDeg := sin(rad);
end;

function CosDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  CosDeg := cos(rad);
end;

function TgDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  TgDeg := sin(rad) / cos(rad);
end;

function CtgDeg(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 180;
  CtgDeg := cos(rad) / sin(rad);
end;



{------------------------------------------------------------}

function SinGrad(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 200;
  SinGrad := sin(rad);
end;

function CosGrad(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 200;
  CosGrad := cos(rad);
end;

function TgGrad(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 200;
  TgGrad := sin(rad) / cos(rad);
end;

function CtgGrad(const x: Extended): Extended;
var
  rad: Extended;
begin
  rad := x * pi / 200;
  CtgGrad := cos(rad) / sin(rad);
end;




{------------------------------------------------------------}

function SinRad(const x: Extended): Extended;
begin
  SinRad := sin(x);
end;

function CosRad(const x: Extended): Extended;
begin
  CosRad := cos(x);
end;

function TgRad(const x: Extended): Extended;
begin
  TgRad := sin(x) / cos(x);
end;

function CtgRad(const x: Extended): Extended;
begin
  CtgRad := cos(x) / sin(x);
end;




{------------------------------------------------------------}

//function tg(const x: Extended): Extended;
//begin
//  tg := sin(x) / cos(x);
//end;
//
//function ctg(const x: Extended): Extended;
//begin
//  ctg := cos(x) / sin(x);
//end;



end.

