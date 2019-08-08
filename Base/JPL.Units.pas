unit JPL.Units;

{
  My old unit from 2001 for Turbo Pascal 7

}

interface


function DegToRad(const Degrees: Double): Double; { Radians := Degrees * PI / 180}
function RadToDeg(const Radians: Double): Double; { Degrees := Radians * 180 / PI }
function GradToRad(const Grads: Double): Double; { Radians := Grads * PI / 200 }
function RadToGrad(const Radians: Double): Double; { Grads := Radians * 200 / PI }
function CycleToRad(const Cycles: Double): Double; { Radians := Cycles * 2PI }
function RadToCycle(const Radians: Double): Double; { Cycles := Radians / 2PI }

function DegToGrad(const Degrees: Double): Double;
function GradToDeg(const Grads: Double): Double;





{jednostki masy}
{g - gram}
{lb. - funt (pound)}
{oz. - uncja (ounce)}
function glb(g: Double): Double;
function goz(g: Double): Double;

function lboz(lb: Double): Double;
function lbg(lb: Double): Double;

function ozlb(oz: Double): Double;
function ozg(oz: Double): Double;


{jednostki astronomiczne}
{m - metry}
{AU - jednostka astronomiczna}
{pc - parsek}
{lyear - rok świetlny}
function mAU(m: Double): Double;
function mpc(m: Double): Double;
function mlyear(m: Double): Double;

function AUm(AU: Double): Double;
function AUpc(AU: Double): Double;
function AUlyear(AU: Double): Double;

function pcm(pc: Double): Double;
function pcAU(pc: Double): Double;
function pclyear(pc: Double): Double;

function lyearm(lyear: Double): Double;
function lyearAU(lyear: Double): Double;
function lyearpc(lyear: Double): Double;



{jedostki mocy}
{W - wat}
{KM - koń mechaniczny}
{HP - (horsepower) koń parowy}
function WKM(W: Double): Double;
function WHP(W: Double): Double;

function KMW(KM: Double): Double;
function KMHP(KM: Double): Double;

function HPW(HP: Double): Double;
function HPKM(HP: Double): Double;


{jednostki prędkości liniowej}
{ms - m/s}
{kmh - km/h}
{kn - węzły}
function mskmh(ms: Double): Double;
function mskn(ms: Double): Double;

function kmhms(kmh: Double): Double;
function kmhkn(kmh: Double): Double;

function knms(kn: Double): Double;
function knkmh(kn: Double): Double;


{jednostki czasu}
{sek - sekunda}
{min - minuta}
{h - godzina}
{ms - milisekunda}
function sekmin(sek: Double): Double;
function sekh(sek: Double): Double;
function sekms(sek: Double): Double;

function minsek(min: Double): Double;
function minh(min: Double): Double;
function minms(min: Double): Double;

function hsek(h: Double): Double;
function hmin(h: Double): Double;
function hms(h: Double): Double;

function mssek(ms: Double): Double;
function msmin(ms: Double): Double;
function msh(ms: Double): Double;


{jednostki ciśnienia, naprężenia}
{Pa - pascal, 1 Pa = 1 N/sqr(m)}
{at - atmosfera techniczna, 1 at = 1 kG/sqr(cm)}
{atm - atmosfera fizyczna}
{mmHg - milimetry słupa rtęci}
{bar = 10E+5 N/sqr(m)}
function Paat(Pa: Double): Double;
function Paatm(Pa: Double): Double;
function PammHg(Pa: Double): Double;
function Pabar(Pa: Double): Double;

function atPa(at: Double): Double;
function atatm(at: Double): Double;
function atmmHg(at: Double): Double;
function atbar(at: Double): Double;

function atmPa(atm: Double): Double;
function atmat(atm: Double): Double;
function atmmmHg(atm: Double): Double;
function atmbar(atm: Double): Double;

function mmHgPa(mmHg: Double): Double;
function mmHgat(mmHg: Double): Double;
function mmHgatm(mmHg: Double): Double;
function mmHgbar(mmHg: Double): Double;

function barPa(bar: Double): Double;
function barat(bar: Double): Double;
function baratm(bar: Double): Double;
function barmmHg(bar: Double): Double;


{jednostki długości}
{A - angstrem}
{m - metr}
{nmil - (n.mil.) mila morska}
{angmil - mila angielska}
{ft - (ft.) stopa}
{inch - (``) cal}
{yd - (yd.) jard}
{pt - (p.t.) punkt typograficzny}
function mA(m: Double): Double;
function mnmil(m: Double): Double;
function mangmil(m: Double): Double;
function mft(m: Double): Double;
function minch(m: Double): Double;
function myd(m: Double): Double;
function mpt(m: Double): Double;

function Am(A: Double): Double;
function Anmil(A: Double): Double;
function Aangmil(A: Double): Double;
function Aft(A: Double): Double;
function Ainch(A: Double): Double;
function Ayd(A: Double): Double;
function Apt(A: Double): Double;

function nmilm(nmil: Double): Double;
function nmilA(nmil: Double): Double;
function nmilangmil(nmil: Double): Double;
function nmilft(nmil: Double): Double;
function nmilinch(nmil: Double): Double;
function nmilyd(nmil: Double): Double;
function nmilpt(nmil: Double): Double;

function angmilm(angmil: Double): Double;
function angmilA(angmil: Double): Double;
function angmilnmil(angmil: Double): Double;
function angmilft(angmil: Double): Double;
function angmilinch(angmil: Double): Double;
function angmilyd(angmil: Double): Double;
function angmilpt(angmil: Double): Double;

function ftm(ft: Double): Double;
function ftA(ft: Double): Double;
function ftnmil(ft: Double): Double;
function ftangmil(ft: Double): Double;
function ftinch(ft: Double): Double;
function ftyd(ft: Double): Double;
function ftpt(ft: Double): Double;

function inchm(inch: Double): Double;
function inchA(inch: Double): Double;
function inchnmil(inch: Double): Double;
function inchangmil(inch: Double): Double;
function inchft(inch: Double): Double;
function inchyd(inch: Double): Double;
function inchpt(inch: Double): Double;

function ydm(yd: Double): Double;
function ydA(yd: Double): Double;
function ydnmil(yd: Double): Double;
function ydangmil(yd: Double): Double;
function ydft(yd: Double): Double;
function ydinch(yd: Double): Double;
function ydpt(yd: Double): Double;

function ptm(pt: Double): Double;
function ptA(pt: Double): Double;
function ptnmil(pt: Double): Double;
function ptangmil(pt: Double): Double;
function ptft(pt: Double): Double;
function ptinch(pt: Double): Double;
function ptyd(pt: Double): Double;



implementation


{$region ' -------------- jednostki kątowe -------------- '}

function DegToRad(const Degrees: Double): Double;
begin
  Result := Degrees * (PI / 180);
end;

function RadToDeg(const Radians: Double): Double; { Degrees := Radians * 180 / PI }
begin
  Result := Radians * (180 / PI);
end;

function GradToRad(const Grads: Double): Double; { Radians := Grads * PI / 200 }
begin
  Result := Grads * (PI / 200);
end;

function RadToGrad(const Radians: Double): Double; { Grads := Radians * 200 / PI}
begin
  Result := Radians * (200 / PI);
end;

function CycleToRad(const Cycles: Double): Double; { Radians := Cycles * 2PI }
begin
  Result := Cycles * (2 * PI);
end;

function RadToCycle(const Radians: Double): Double; { Cycles := Radians / 2PI }
begin
  Result := Radians / (2 * PI);
end;

function DegToGrad(const Degrees: Double): Double;
begin
  Result := RadToGrad(DegToRad(Degrees));
end;

function GradToDeg(const Grads: Double): Double;
begin
  Result := RadToDeg(GradToRad(Grads));
end;

{$endregion jednostki kątowe}


{$region ' --------------- jednostki masy ---------------- '}

function glb(g: Double): Double;
begin
  glb := g * 0.0022046225;
end;

function goz(g: Double): Double;
begin
  goz := g * 0.03527396;
end;

function lboz(lb: Double): Double;
begin
  lboz := lb * 16;
end;

function lbg(lb: Double): Double;
begin
  lbg := lb / 0.0022046225;
end;

function ozlb(oz: Double): Double;
begin
  ozlb := oz / 16;
end;

function ozg(oz: Double): Double;
begin
  ozg := oz / 0.03527396;
end;

{$endregion jednostki masy}


{$region ' ------------- jednostki astronomiczne -------------- '}

function mAU(m: Double): Double;
begin
  mAU := m / 1.495978921E+11;
end;

function mpc(m: Double): Double;
begin
  mpc := m / 3.085667022E+16;
end;

function mlyear(m: Double): Double;
begin
  mlyear := m / 9.4605E+15;
end;

function AUm(AU: Double): Double;
begin
  AUm := AU * 1.495978921E+11;
end;

function AUpc(AU: Double): Double;
begin
  AUpc := AU / 206264.8;
end;

function AUlyear(AU: Double): Double;
begin
  AUlyear := AU / 6.324E+4;
end;

function pcm(pc: Double): Double;
begin
  pcm := pc * 3.085667022E+16;
end;

function pcAU(pc: Double): Double;
begin
  pcAU := pc * 206264.8;
end;

function pclyear(pc: Double): Double;
begin
  pclyear := pc / 0.30659;
end;

function lyearm(lyear: Double): Double;
begin
  lyearm := lyear * 9.4605E+15;
end;

function lyearAU(lyear: Double): Double;
begin
  lyearAU := lyear * 6.324E+4;
end;

function lyearpc(lyear: Double): Double;
begin
  lyearpc := lyear * 0.30659;
end;

{$endregion jednostki astronomiczne}


{$region ' ------------------ jednostki mocy --------------- '}

function WKM(W: Double): Double;
begin
  WKM := W * (1 / 735.5);
end;

function WHP(W: Double): Double;
begin
  WHP := W * (1 / 745.8);
end;

function KMW(KM: Double): Double;
begin
  KMW := KM / (1 / 735.5);
end;

function KMHP(KM: Double): Double;
begin
  KMHP := (KM * 735.5) / 745.8;
end;

function HPW(HP: Double): Double;
begin
  HPW := HP / 745.8;
end;

function HPKM(HP: Double): Double;
begin
  HPKM := (HP * 745.8) / 735.5;
end;

{$endregion jednostki mocy}


{$region ' -------------- jednostki prędkości liniowej -------------- '}

function mskmh(ms: Double): Double;
begin
  mskmh := ms * 3600E-3;
end;

function mskn(ms: Double): Double;
begin
  mskn := (ms / 1852) * 3600;
end;

function kmhms(kmh: Double): Double;
begin
  kmhms := kmh * (10 / 36);
end;

function kmhkn(kmh: Double): Double;
begin
  kmhkn := (kmh / 1852) * 1000;
end;

function knms(kn: Double): Double;
begin
  knms := (kn * 1852) / 3600;
end;

function knkmh(kn: Double): Double;
begin
  knkmh := (kn * 1852) / 1000;
end;

{$endregion jednostki prędkości liniowej}


{$region ' --------------------- jednostki czasu ------------------------- '}

function sekmin(sek: Double): Double;
begin
  sekmin := sek / 60;
end;

function sekh(sek: Double): Double;
begin
  sekh := sek / 3600;
end;

function sekms(sek: Double): Double;
begin
  sekms := sek / 0.001;
end;

function minsek(min: Double): Double;
begin
  minsek := min * 60;
end;

function minh(min: Double): Double;
begin
  minh := min / 60;
end;

function minms(min: Double): Double;
begin
  minms := (min * 60) / 0.001;
end;

function hsek(h: Double): Double;
begin
  hsek := h * 3600;
end;

function hmin(h: Double): Double;
begin
  hmin := h * 60;
end;

function hms(h: Double): Double;
begin
  hms := (h * 3600) / 0.001;
end;

function mssek(ms: Double): Double;
begin
  mssek := ms / 1000;
end;

function msmin(ms: Double): Double;
begin
  msmin := (ms / 1000) / 60;
end;

function msh(ms: Double): Double;
begin
  msh := (ms / 1000) / 3600;
end;

{$endregion jednostki czasu}


{$region ' --------------------- jednostki ciśnienia ------------------------- '}

function Paat(Pa: Double): Double;
begin
  Paat := Pa * (1 / 98066.5);
end;

function Paatm(Pa: Double): Double;
begin
  Paatm := Pa * (1 / 101325);
end;

function PammHg(Pa: Double): Double;
begin
  PammHg := Pa * 0.75E-2;
end;

function Pabar(Pa: Double): Double;
begin
  Pabar := Pa / 100000;
end;

function atPa(at: Double): Double;
begin
  atPa := at * 98066.5;
end;

function atatm(at: Double): Double;
begin
  atatm := (at * 98066.5) / 101325;
end;

function atmmHg(at: Double): Double;
begin
  atmmHg := (at * 98066.5) / (101325 / 760);
end;

function atbar(at: Double): Double;
begin
  atbar := (at * 98066.5) / 100000;
end;

function atmPa(atm: Double): Double;
begin
  atmPa := atm * 101325;
end;

function atmat(atm: Double): Double;
begin
  atmat := (atm * 101325) / 98066.5;
end;

function atmmmHg(atm: Double): Double;
begin
  atmmmHg := atm * 760;
end;

function atmbar(atm: Double): Double;
begin
  atmbar := (atm * 101325) / 100000;
end;

function mmHgPa(mmHg: Double): Double;
begin
  mmHgPa := mmHg * (101325 / 760);
end;

function mmHgat(mmHg: Double): Double;
begin
  mmHgat := (mmHg * (101325 / 760)) / 98066.5;
end;

function mmHgatm(mmHg: Double): Double;
begin
  mmHgatm := mmHg / 760;
end;

function mmHgbar(mmHg: Double): Double;
begin
  mmHgbar := (mmHg * (101325 / 760)) / 100000;
end;

function barPa(bar: Double): Double;
begin
  barPa := bar * 100000;
end;

function barat(bar: Double): Double;
begin
  barat := (bar * 100000) / 98066.5;
end;

function baratm(bar: Double): Double;
begin
  baratm := (bar * 100000) / 101325;
end;

function barmmHg(bar: Double): Double;
begin
  barmmHg := (bar * 100000) / (101325 / 760);
end;

{$endregion jednostki ciśnienia}


{$region ' --------------------- jednostki długości ------------------------- '}

function mA(m: Double): Double;
begin
  mA := m / 10E-10;
end;

function mnmil(m: Double): Double;
begin
  mnmil := m / 1852;
end;

function mangmil(m: Double): Double;
begin
  mangmil := m / 1609.344;
end;

function mft(m: Double): Double;
begin
  mft := m / 0.3048;
end;

function minch(m: Double): Double;
begin
  minch := m / 0.0254;
end;

function myd(m: Double): Double;
begin
  myd := m / 0.9144;
end;

function mpt(m: Double): Double;
begin
  mpt := m * 2660;
end;

function Am(A: Double): Double;
begin
  Am := A * 10E-10;
end;

function Anmil(A: Double): Double;
begin
  Anmil := (A * 10E-10) / 1852;
end;

function Aangmil(A: Double): Double;
begin
  Aangmil := (A * 10E-10) / 1609.344;
end;

function Aft(A: Double): Double;
begin
  Aft := (A * 10E-10) / 0.3048;
end;

function Ainch(A: Double): Double;
begin
  Ainch := (A * 10E-10) / 0.0254;
end;

function Ayd(A: Double): Double;
begin
  Ayd := (A * 10E-10) / 0.9144;
end;

function Apt(A: Double): Double;
begin
  Apt := (A * 10E-10) * 2660;
end;

function nmilm(nmil: Double): Double;
begin
  nmilm := nmil * 1852;
end;

function nmilA(nmil: Double): Double;
begin
  nmilA := (nmil * 1852) / 10E-10;
end;

function nmilangmil(nmil: Double): Double;
begin
  nmilangmil := (nmil * 1852) / 1609.344;
end;

function nmilft(nmil: Double): Double;
begin
  nmilft := (nmil * 1852) / 0.3048;
end;

function nmilinch(nmil: Double): Double;
begin
  nmilinch := (nmil * 1852) / 0.0254;
end;

function nmilyd(nmil: Double): Double;
begin
  nmilyd := (nmil * 1852) / 0.9144;
end;

function nmilpt(nmil: Double): Double;
begin
  nmilpt := (nmil * 1852) * 2660;
end;

function angmilm(angmil: Double): Double;
begin
  angmilm := angmil * 1609.344;
end;

function angmilA(angmil: Double): Double;
begin
  angmilA := (angmil * 1609.344) / 10E-10;
end;

function angmilnmil(angmil: Double): Double;
begin
  angmilnmil := (angmil * 1609.344) / 1852;
end;

function angmilft(angmil: Double): Double;
begin
  angmilft := (angmil * 1609.344) / 0.3048;
end;

function angmilinch(angmil: Double): Double;
begin
  angmilinch := (angmil * 1609.344) / 0.0254;
end;

function angmilyd(angmil: Double): Double;
begin
  angmilyd := (angmil * 1609.344) / 0.9144;
end;

function angmilpt(angmil: Double): Double;
begin
  angmilpt := (angmil * 1609.344) * 2660;
end;

function ftm(ft: Double): Double;
begin
  ftm := ft * 0.3048;
end;

function ftA(ft: Double): Double;
begin
  ftA := (ft * 0.3048) / 10E-10;
end;

function ftnmil(ft: Double): Double;
begin
  ftnmil := (ft * 0.3048) / 1852;
end;

function ftangmil(ft: Double): Double;
begin
  ftangmil := (ft * 0.3048) / 1609.344;
end;

function ftinch(ft: Double): Double;
begin
  ftinch := ft * 12;
end;

function ftyd(ft: Double): Double;
begin
  ftyd := ft / 3;
end;

function ftpt(ft: Double): Double;
begin
  ftpt := (ft * 0.3048) * 2660;
end;

function inchm(inch: Double): Double;
begin
  inchm := inch * 0.0254;
end;

function inchA(inch: Double): Double;
begin
  inchA := (inch * 0.0254) / 10E-10;
end;

function inchnmil(inch: Double): Double;
begin
  inchnmil := (inch * 0.0254) / 1852;
end;

function inchangmil(inch: Double): Double;
begin
  inchangmil := (inch * 0.0254) / 1609.344;
end;

function inchft(inch: Double): Double;
begin
  inchft := inch / 12;
end;

function inchyd(inch: Double): Double;
begin
  inchyd := inch / 36;
end;

function inchpt(inch: Double): Double;
begin
  inchpt := (inch * 0.0254) * 2660;
end;

function ydm(yd: Double): Double;
begin
  ydm := yd * 0.9144;
end;

function ydA(yd: Double): Double;
begin
  ydA := (yd * 0.9144) / 10E-10;
end;

function ydnmil(yd: Double): Double;
begin
  ydnmil := (yd * 0.9144) / 1852;
end;

function ydangmil(yd: Double): Double;
begin
  ydangmil := (yd * 0.9144) / 1609.344;
end;

function ydft(yd: Double): Double;
begin
  ydft := yd * 3;
end;

function ydinch(yd: Double): Double;
begin
  ydinch := yd * 36;
end;

function ydpt(yd: Double): Double;
begin
  ydpt := (yd * 0.9144) * 2660;
end;

function ptm(pt: Double): Double;
begin
  ptm := pt / 2660;
end;

function ptA(pt: Double): Double;
begin
  ptA := (pt / 2660) / 10E-10;
end;

function ptnmil(pt: Double): Double;
begin
  ptnmil := (pt / 2660) / 1852;
end;

function ptangmil(pt: Double): Double;
begin
  ptangmil := (pt / 2660) / 1609.344;
end;

function ptft(pt: Double): Double;
begin
  ptft := (pt / 2660) / 0.3048;
end;

function ptinch(pt: Double): Double;
begin
  ptinch := (pt / 2660) / 0.0254;
end;

function ptyd(pt: Double): Double;
begin
  ptyd := (pt / 2660) / 0.9144;
end;

{$endregion jednostki długości}


end.

