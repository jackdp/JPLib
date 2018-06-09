unit JPL.DateTime;

{
  Jacek Pazera
  http://www.pazera-software.com
}

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface


uses
  {$IFDEF DCC}Windows,{$ENDIF}
  SysUtils,
  //Classes,
  DateUtils,
  JPL.Strings, JPL.Conversion;


const
  MsSec = 1000;
  MsMin = 60000;
  MsHour = 3600000;
  MsDay = 86400000;
  MINUTESPERDAY = 1440;
  MsInBeat = 86.4 * MsSec;
  DTSep = '-';
  TMSep = ':';

  
type

  TEtimeFormat = (etfShort, etfMid, etfLong);
  TTimeFormat = (tfShort, tfMid, tfLong, tfVLong);
  TDateFormat = (dfLongNames, dfShortNames, dfLongDay, dfShortDay, dfShort, dfRev);


  TRegTZI = packed record
    Bias: integer;
    StandardBias: integer;
    DaylightBias: integer;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

  TSwatchBeat = 0..999;



  
function ParseTime(ms: DWORD; TimeFormat: TTimeFormat = tfMid): string;
function GetCurrentTimeStr(TimeFormat: TTimeFormat = tfMid): string;
function ParseDate(dt: TDateTime; DateFormat: TDateFormat = dfLongDay): string;
function FirstDayOfYear(Year: WORD): TDateTime;
function SecToTimeStr(Sec: DWORD): string;
function MinToMs(Min: integer): integer;
function WithinDates(dt, dtStart, dtEnd: TDateTime): Boolean;
function MinToHMStr(Min: integer): string;

// Swatch Internet Time: https://en.wikipedia.org/wiki/Swatch_Internet_Time
function GetETimeStr(tm: TDateTime; ETimeFormat: TETimeFormat = etfMid; DeltaMin: integer = 0): string;

function TimeToMs(tm: TDateTime): int64;
function DayTimeStr(dt: TDateTime; DateSeparator: string = '.'; TimeSeparator: string = ':'; DateTimeSeparator: string = ' '; MSecSeparator: string = '.'; bShowMSec: Boolean = True): string;


function MsToTimeStr(ms: integer; RoundToSeconds: Boolean = False): string;

function GetTimestamp(dt: TDateTime; bShowMSec: Boolean = True; DateSeparator: string = '-'; TimeSeparator: string = ';'; DateTimeSeparator: string = '_';
  MSecSeparator: string = '.'): string;
function JPDateToStr(dt: TDateTime; Separator: string = '-'): string;
function JPTimeToStr(dt: TDateTime; ShowMSec: Boolean = True; TimeSep: string = ':'; MSecSep: string = '.'): string;
function GetDateTime(const Year, Month, Day: Word; Hour: Word = 0; Min: Word = 0; Sec: Word = 0; MSec: Word = 0): TDateTime;

function GetDateTimeStr(dt: TDateTime; Format: string = '$Y.$M.$D-$H;$MIN;$S,$MS'): string;


implementation

function GetDateTimeStr(dt: TDateTime; Format: string = '$Y.$M.$D-$H;$MIN;$S,$MS'): string;
var
  sr: string;
  xYear, xMonth, xDay, xHour, xMinute, xSecond, xMs: Word;
  sYear, sMonth, sDay, sHour, sMinute, sSecond, sMs: string;
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll, rfIgnoreCase];

  DecodeDateTime(dt, xYear, xMonth, xDay, xHour, xMinute, xSecond, xMs);

  sYear := itos(xYear);
  sMonth := Pad(itos(xMonth), 2, '0');
  sDay := Pad(itos(xDay), 2, '0');
  sHour := Pad(itos(xHour), 2, '0');
  sMinute := Pad(itos(xMinute), 2, '0');
  sSecond := Pad(itos(xSecond), 2, '0');
  sMs := Pad(itos(xMs), 3, '0');


  sr := Format;
  sr := StringReplace(sr, '$Y', sYear, rf);
  sr := StringReplace(sr, '$MIN', sMinute, rf);
  sr := StringReplace(sr, '$MS', sMs, rf);
  sr := StringReplace(sr, '$M', sMonth, rf);

  sr := StringReplace(sr, '$D', sDay, rf);
  sr := StringReplace(sr, '$H', sHour, rf);
  sr := StringReplace(sr, '$S', sSecond, rf);


  Result := sr;
end;

function GetDateTime(const Year, Month, Day: Word; Hour: Word = 0; Min: Word = 0; Sec: Word = 0; MSec: Word = 0): TDateTime;
begin
  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec);
end;

function JPTimeToStr(dt: TDateTime; ShowMSec: Boolean = True; TimeSep: string = ':'; MSecSep: string = '.'): string;
var
  Hour, Min, Sec, Msec: WORD;
begin
  DecodeTime(dt, Hour, Min, Sec, MSec);
  Result :=
    Pad(itos(Hour), 2, '0') + TimeSep +
    Pad(itos(Min), 2, '0') + TimeSep +
    Pad(itos(Sec), 2, '0');
  if ShowMSec then Result := Result + MSecSep + Pad(itos(MSec), 3, '0');
end;

function JPDateToStr(dt: TDateTime; Separator: string = '-'): string;
var
  Year, Month, Day: WORD;
begin
  DecodeDate(dt, Year, Month, Day);
  Result :=
    itos(Year) + Separator +
    Pad(itos(Month), 2, '0') + Separator +
    Pad(itos(Day), 2, '0');
end;

function GetTimestamp(dt: TDateTime; bShowMSec: Boolean = True; DateSeparator: string = '-'; TimeSeparator: string = ';'; DateTimeSeparator: string = '_';
  MSecSeparator: string = '.'): string;
begin
  //Result := DayTimeStr(dt, '-', ';', '_', '.', False);
  Result := DayTimeStr(dt, DateSeparator, TimeSeparator, DateTimeSeparator, MSecSeparator, bShowMSec);
end;

function MsToTimeStr(ms: integer; RoundToSeconds: Boolean = False): string;
var
  h, m, s: integer;
begin
  if RoundToSeconds then Result := '00:00:00'
  else Result := '00:00:00.000';
  if ms <= 0 then Exit;

  h := ms div (60 * 60 * 1000);
  ms := ms - (h * 60 * 60 * 1000);
  m := ms div (60 * 1000);
  ms := ms - (m * 60 * 1000);
  s := ms div 1000;
  ms := ms - (s * 1000);
  if RoundToSeconds then
  begin
    if ms >= 500 then Inc(s);
    Result := Pad(itos(h), 2, '0') + ':' + Pad(itos(m), 2, '0') + ':' + Pad(itos(s), 2, '0');
  end
  else Result := Pad(itos(h), 2, '0') + ':' + Pad(itos(m), 2, '0') + ':' + Pad(itos(s), 2, '0') + '.' + Pad(itos(ms), 3, '0');
end;


function DayTimeStr(dt: TDateTime; DateSeparator: string = '.'; TimeSeparator: string = ':'; DateTimeSeparator: string = ' '; MSecSeparator: string = '.'; bShowMSec: Boolean = True): string;
var
  Day, Month, Year, Hour, Min, Sec, Msec: WORD;
begin
  DecodeTime(dt, Hour, Min, Sec, MSec);
  DecodeDate(dt, Year, Month, Day);
  Result :=
    itos(Year) + DateSeparator +
    Pad(itos(Month), 2, '0') + DateSeparator +
    Pad(itos(Day), 2, '0') + 
    DateTimeSeparator +
    Pad(itos(Hour), 2, '0') + TimeSeparator +
    Pad(itos(Min), 2, '0') + TimeSeparator +
    Pad(itos(Sec), 2, '0');
  if bShowMSec then Result := Result + MSecSeparator + Pad(itos(MSec), 3, '0');
end;



function TimeToMs(tm: TDateTime): int64;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(tm, h, m, s, ms);
  Result :=
    Int64(h) * Int64(MsHour) + Int64(m) * Int64(MsMin) + Int64(s) * Int64(MsSec) + Int64(ms);
end;


// Swatch Internet Time: https://en.wikipedia.org/wiki/Swatch_Internet_Time
function GetETimeStr(tm: TDateTime; ETimeFormat: TETimeFormat; DeltaMin: integer): string;
var
  xr: Real;
  s: string;
begin

  try

    xr := TimeToMs(tm);
    //if bIsDST then xr := xr - 60 * MsMin;
    xr := xr + (DeltaMin * MsMin);

    if (xr / MsInBeat) >= 1000 then
      xr := 0
    else
    begin
      xr := xr / MsInBeat;
      if xr < 0 then xr := 1000 + xr;
      if xr >= 1000 then xr := 0;
    end;

    s := FormatFloat('000.000000', xr);
    s := StringReplace(s, ',', '.', []);

    if ETimeFormat = etfShort then s := Copy(s, 1, 3)
    else if ETimeFormat = etfMid then s := Copy(s, 1, 5)
    else s := Copy(s, 1, 6);


    Result := '@' + s;

  except
    Result := '@';
  end;
end;


function MinToHMStr(Min: integer): string;
var
  sh, sm: string;
  xh: integer;
begin
  xh := Min div 60;

  if Min > 0 then
    sh := '+' + Pad(IntToStr(xh), 2, '0')
  else if Min = 0 then
    sh := '00'
  else
    sh := '-' + Pad(IntToStr(Abs(xh)), 2, '0');

  Min := Abs(Min - (xh * 60));
  sm := Pad(IntToStr(Min), 2, '0');
  Result := sh + TMSep + sm;
end;




function WithinDates(dt, dtStart, dtEnd: TDateTime): Boolean;
begin
  Result := (dt >= dtStart) and (dt <= dtEnd);
end;


function FirstDayOfYear(Year: WORD): TDateTime;
begin
  try
    Result := EncodeDateTime(Year, 1, 1, 0, 0, 0, 0);
  except
    Result := 0;
  end;
end;



function ParseDate(dt: TDateTime; DateFormat: TDateFormat): string;
var
  dnum, Day, Month, Year: WORD;
  s: string;
begin
  dt := Now;
  dnum := DayOfWeek(dt);
  DecodeDate(dt, Year, Month, Day);

  if DateFormat = dfLongNames then
    s :=
      FormatSettings.LongDayNames[dnum] + ', ' +
      Pad(IntToStr(Day), 2, '0') + ' ' +
      FormatSettings.LongMonthNames[Month] + ' ' +
      IntToStr(Year)
  else if DateFormat = dfShortNames then
    s :=
      FormatSettings.ShortDayNames[dnum] + ', ' +
      Pad(IntToStr(Day), 2, '0') + ' ' +
      FormatSettings.ShortMonthNames[Month] + ' ' +
      IntToStr(Year)
  else if DateFormat = dfShortDay then
    s :=
      FormatSettings.ShortDayNames[dnum] + ', ' +
      Pad(IntToStr(Day), 2, '0') + '.' +
      Pad(IntToStr(Month), 2, '0') + '.' +
      IntToStr(Year)
  else if DateFormat = dfShort then
    s :=
      Pad(IntToStr(Day), 2, '0') + '.' +
      Pad(IntToStr(Month), 2, '0') + '.' +
      IntToStr(Year)
  else if DateFormat = dfRev then
    s :=
      IntToStr(Year) + '.' +
      Pad(IntToStr(Month), 2, '0') + '.' +
      Pad(IntToStr(Day), 2, '0')
  else
    s :=
      FormatSettings.LongDayNames[dnum] + ', ' +
      Pad(IntToStr(Day), 2, '0') + '.' +
      Pad(IntToStr(Month), 2, '0') + '.' +
      IntToStr(Year);

  Result := s;
end;


function MinToMs(Min: integer): integer;
begin
  Result := Min * 60000;
end;



function SecToTimeStr(Sec: DWORD): string;
begin
  Result := ParseTime(Sec * 1000);
end;


function GetCurrentTimeStr(TimeFormat: TTimeFormat): string;
var
  Hour, Min, Sec, MSec: WORD;
  tm: Double;
  s: string;
begin

  tm := Time;

  DecodeTime(tm, Hour, Min, Sec, MSec);

  if TimeFormat = tfShort then
    s :=
      Pad(IntToStr(Hour), 2, '0') + ':' +
      Pad(IntToStr(Min), 2, '0')
  else if TimeFormat = tfMid then
    s :=
      Pad(IntToStr(Hour), 2, '0') + ':' +
      Pad(IntToStr(Min), 2, '0') + ':' +
      Pad(IntToStr(Sec), 2, '0')
  else
    s :=
      Pad(IntToStr(Hour), 2, '0') + ':' +
      Pad(IntToStr(Min), 2, '0') + ':' +
      Pad(IntToStr(Sec), 2, '0') + '.' +
      IntToStr(MSec div 100);

  Result := s;

end;


{$hints off}
function ParseTime(ms: DWORD; TimeFormat: TTimeFormat): string;
var
  h, m, s, msec, msecv: DWORD;
  sr: string;
begin

  h := ms div MsHour;
  ms := ms - h * MsHour;
  m := ms div MsMin;
  ms := ms - DWORD(m * MsMin);
  s := ms div MsSec;
  ms := ms - s * MsSec;
  msecv := ms;
  msec := ms div 100;


  if TimeFormat = tfShort then
    sr :=
      Pad(IntToStr(h), 2, '0') + ':' +
      Pad(IntToStr(m), 2, '0')
  else if TimeFormat = tfMid then
    sr :=
      Pad(IntToStr(h), 2, '0') + ':' +
      Pad(IntToStr(m), 2, '0') + ':' +
      Pad(IntToStr(s), 2, '0')
  else if TimeFormat = tfLong then
    sr :=
      Pad(IntToStr(h), 2, '0') + ':' +
      Pad(IntToStr(m), 2, '0') + ':' +
      Pad(IntToStr(s), 2, '0') + '.' +
      IntToStr(msec)
  else
    sr :=
      Pad(IntToStr(h), 2, '0') + ':' +
      Pad(IntToStr(m), 2, '0') + ':' +
      Pad(IntToStr(s), 2, '0') + '.' +
      IntToStr(msecv);

  Result := sr;
end;
{$hints on}





end.
