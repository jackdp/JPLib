unit JPL.DateTime;

{
  Jacek Pazera
  https://www.pazera-software.com
}

{$I .\..\jp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface


uses
  {$IFDEF DCC}Windows,{$ENDIF}
  SysUtils, Types,
  DateUtils,
  JPL.Strings, JPL.Conversion;


const
  MsSec = 1000;
  MsMin = 60000;
  MsHour = 3600000;
  MsDay = 86400000;

  MSEC_IN_SEC = 1000;
  MSEC_IN_MIN = 60000;
  MSEC_IN_HOUR = 3600000;
  MSEC_IN_DAY = 86400000;

  MINUTESPERDAY = 1440;
  MsInBeat = 86.4 * MsSec;
  DTSep = '-';
  TMSep = ':';

  
type

  TEtimeFormat = (etfShort, etfMid, etfLong);
  TTimeFormat = (
    tfShort,
    tfMid,
    tfLong,
    tfVLong,
    tfExtraLong
  );
  TDateFormat = (dfLongNames, dfShortNames, dfLongDay, dfShortDay, dfShort, dfRev);


  TRegTZI = packed record
    Bias: integer;
    StandardBias: integer;
    DaylightBias: integer;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

  TSwatchBeat = 0..999;



  
function ParseTime(ms: DWORD; TimeFormat: TTimeFormat = tfMid; Trim0Hour: Boolean = False): string;
function GetCurrentTimeStr(TimeFormat: TTimeFormat = tfMid): string;
function ParseDate(dt: TDateTime; DateFormat: TDateFormat = dfLongDay): string;
function FirstDayOfYear(Year: WORD): TDateTime;
function SecToTimeStr(Sec: DWORD): string;
function MinToMs(Min: integer): integer;
function MsToMinStr(const xms: Int64; MinSecSeparator: string = ':'; MinPostfix: string = ''; SecPostfix: string = ''; MSecPostfix: string = ''): string;
function WithinDates(dt, dtStart, dtEnd: TDateTime): Boolean;
function MinToHMStr(Min: integer): string;

// Swatch Internet Time: https://en.wikipedia.org/wiki/Swatch_Internet_Time
function GetETimeStr(tm: TDateTime; ETimeFormat: TETimeFormat = etfMid; DeltaMin: integer = 0): string;

function TimeToMs(tm: TDateTime): int64;
function DayTimeStr(dt: TDateTime; DateSeparator: string = '.'; TimeSeparator: string = ':'; DateTimeSeparator: string = ' '; MSecSeparator: string = '.'; bShowMSec: Boolean = True): string;


function MsToTimeStr(ms: Int64; RoundToSeconds: Boolean = False): string;
function MsToTimeStrEx(ms: Int64; RoundToSeconds: Boolean = False; DayStr: string = 'd '): string;

function GetTimestamp(dt: TDateTime; bShowMSec: Boolean = True; DateSeparator: string = '-'; TimeSeparator: string = ';'; DateTimeSeparator: string = '_';
  MSecSeparator: string = '.'): string;


// System independent date & time conversion routines
function JPDateToStr(const dt: TDateTime; DateSep: string = '-'): string;
function JPStrToDate(const DateStr: string; Default: TDateTime; DateSep: string = '-'): TDateTime;
function JPTryStrToDate(const DateStr: string; out ADate: TDateTime; DateSep: string = '-'): Boolean;

function JPTimeToStr(const dt: TDateTime; ShowMSec: Boolean = True; TimeSep: string = ':'; MSecSep: string = '.'): string;
function JPStrToTime(const TimeStr: string; Default: TDateTime; TimeSep: string = ':'; MSecSep: string = '.'): TDateTime;
function JPTryStrToTime(const TimeStr: string; out ATime: TDateTime; TimeSep: string = ':'; MSecSep: string = '.'): Boolean;

function JPDateTimeToStr(const dt: TDateTime; UseMilliseconds: Boolean = False; DateSep: string = '-'; TimeSep: string = ':'; MSecSep: string = '.';
  DateToTimeSep: string = ' '): string;
function JPStrToDateTime(const DateTimeStr: string; Default: TDateTime; DateSep: string = '-'; TimeSep: string = ':'; MSecSep: string = '.';
  DateToTimeSep: string = ' '): TDateTime;


function GetDateTime(const Year, Month, Day: Word; Hour: Word = 0; Min: Word = 0; Sec: Word = 0; MSec: Word = 0): TDateTime;
function GetDateTimeStr(dt: TDateTime; Format: string = '$Y.$M.$D-$H;$MIN;$S,$MS'): string;

function SecToMs(const Sec: integer): integer;
function MsToSecStr(const Ms: integer; PaddingS: Byte = 4; PaddingMs: Byte = 3): string;
function SecToTimeStrShort(Sec: DWORD): string;
function MsToDateTime(Ms: DWORD): TDateTime;

function CurrentDate: TDateTime;


implementation


function CurrentDate: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Now, Year, Month, Day);
  Result := EncodeDate(Year, Month, Day);
end;


function SecToTimeStrShort(Sec: DWORD): string;
begin
  Result := ParseTime(Sec * 1000, tfMid, True);
end;

function SecToMs(const Sec: integer): integer;
begin
  Result := Sec * 1000;
end;

function MsToSecStr(const Ms: integer; PaddingS: Byte = 4; PaddingMs: Byte = 3): string;
var
  sec, msec: integer;
begin
  sec := ms div 1000;
  msec := ms mod 1000;
  Result := Pad(IntToStr(sec), PaddingS, '0') + '.' + Pad(IntToStr(msec), PaddingMs, '0');
end;

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




function JPTimeToStr(const dt: TDateTime; ShowMSec: Boolean = True; TimeSep: string = ':'; MSecSep: string = '.'): string;
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

function JPTryStrToTime(const TimeStr: string; out ATime: TDateTime; TimeSep: string = ':'; MSecSep: string = '.'): Boolean;
var
  Arr: TStringDynArray;
  Hour, Min, Sec, MSec: Word;
  xp: integer;
  sSec, sMSec: string;
begin
  Result := False;

  SplitStrToArrayEx(TimeStr, Arr, TimeSep);
  if Length(Arr) < 3 then Exit;

  if not TryStrToWord(Arr[0], Hour) then Exit;
  if not TryStrToWord(Arr[1], Min) then Exit;

  sSec := Arr[2];
  xp := Pos(MSecSep, sSec);
  if xp > 0 then
  begin
    sMSec := Copy(sSec, xp + Length(MSecSep), Length(sSec));
    sSec := Copy(sSec, 1, xp - 1);
    if not TryStrToWord(sSec, Sec) then Exit;
    if not TryStrToWord(sMSec, MSec) then Exit;
  end
  else
  begin
    if not TryStrToWord(sSec, Sec) then Exit;
    MSec := 0;
  end;

  ATime := EncodeTime(Hour, Min, Sec, MSec);
  Result := True;
end;

function JPStrToTime(const TimeStr: string; Default: TDateTime; TimeSep: string = ':'; MSecSep: string = '.'): TDateTime;
begin
  if not JPTryStrToTime(TimeStr, Result, TimeSep, MSecSep) then Result := Default;
end;

function JPDateToStr(const dt: TDateTime; DateSep: string = '-'): string;
var
  Year, Month, Day: WORD;
begin
  DecodeDate(dt, Year, Month, Day);
  Result :=
    itos(Year) + DateSep +
    Pad(itos(Month), 2, '0') + DateSep +
    Pad(itos(Day), 2, '0');
end;

function JPTryStrToDate(const DateStr: string; out ADate: TDateTime; DateSep: string = '-'): Boolean;
var
  Arr: TStringDynArray;
  Year, Month, Day: Word;
begin
  Result := False;
  SplitStrToArrayEx(DateStr, Arr, DateSep);
  if Length(Arr) <> 3 then Exit;
  if not TryStrToWord(Arr[0], Year) then Exit;
  if not TryStrToWord(Arr[1], Month) then Exit;
  if not TryStrToWord(Arr[2], Day) then Exit;
  ADate := EncodeDate(Year, Month, Day);
  Result := True;
end;

function JPStrToDate(const DateStr: string; Default: TDateTime; DateSep: string = '-'): TDateTime;
begin
  if not JPTryStrToDate(DateStr, Result, DateSep) then Result := Default;
end;

function JPDateTimeToStr(const dt: TDateTime; UseMilliseconds: Boolean = False; DateSep: string = '-'; TimeSep: string = ':'; MSecSep: string = '.';
  DateToTimeSep: string = ' '): string;
begin
  Result :=
    JPDateToStr(dt, DateSep) +
    DateToTimeSep +
    JPTimeToStr(dt, UseMilliseconds, TimeSep, MSecSep);
end;

function JPStrToDateTime(const DateTimeStr: string; Default: TDateTime; DateSep: string = '-'; TimeSep: string = ':'; MSecSep: string = '.';
  DateToTimeSep: string = ' '): TDateTime;
var
  xp: integer;
  sDate, sTime: string;
  dtDate, dtTime: TDateTime;
begin
  Result := Default;

  xp := Pos(DateToTimeSep, DateTimeStr);
  if xp = 0 then Exit;
  sDate := Copy(DateTimeStr, 1, xp - 1);
  sTime := Copy(DateTimeStr, xp + Length(DateToTimeSep), Length(DateTimeStr));

  if not JPTryStrToDate(sDate, dtDate, DateSep) then Exit;
  if not JPTryStrToTime(sTime, dtTime, TimeSep, MSecSep) then Exit;

  Result := dtTime;
  ReplaceDate(Result, dtDate);
end;




function GetTimestamp(dt: TDateTime; bShowMSec: Boolean = True; DateSeparator: string = '-'; TimeSeparator: string = ';'; DateTimeSeparator: string = '_';
  MSecSeparator: string = '.'): string;
begin
  //Result := DayTimeStr(dt, '-', ';', '_', '.', False);
  Result := DayTimeStr(dt, DateSeparator, TimeSeparator, DateTimeSeparator, MSecSeparator, bShowMSec);
end;

function MsToTimeStr(ms: Int64; RoundToSeconds: Boolean = False): string;
var
  h, m, s: Int64;
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

function MsToTimeStrEx(ms: Int64; RoundToSeconds: Boolean = False; DayStr: string = 'd '): string;
const
  MS_IN_DAY = 1000 * 60 * 60 * 24;
var
  d, h, m, s: Int64;
begin
  if RoundToSeconds then Result := '00:00:00'
  else Result := '00:00:00.000';
  if ms <= 0 then Exit;
  d := 0;

  if ms >= MS_IN_DAY then
  begin
    d := ms div MS_IN_DAY;
    ms := ms - (d * MS_IN_DAY);
  end;

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

  if d > 0 then Result := itos(d) + DayStr + Result;
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

    if (xr / MsInBeat) >= 1000 then xr := 0
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
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.LongDayNames[dnum] + ', ' + {$ELSE}LongDayNames[dnum] + ', ' + {$ENDIF}
      Pad(IntToStr(Day), 2, '0') + ' ' +
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.LongMonthNames[Month] + ' ' + {$ELSE}LongMonthNames[Month] + ' ' + {$ENDIF}
      IntToStr(Year)
  else if DateFormat = dfShortNames then
    s :=
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.ShortDayNames[dnum] + ', ' + {$ELSE}ShortDayNames[dnum] + ', ' + {$ENDIF}
      Pad(IntToStr(Day), 2, '0') + ' ' +
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.ShortMonthNames[Month] + ' ' + {$ELSE}ShortMonthNames[Month] + ' ' + {$ENDIF}
      IntToStr(Year)
  else if DateFormat = dfShortDay then
    s :=
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.ShortDayNames[dnum] + ', ' + {$ELSE}ShortDayNames[dnum] + ', ' + {$ENDIF}
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
      {$IFDEF HAS_FORMATSETTINGS}FormatSettings.LongDayNames[dnum] + ', ' + {$ELSE}LongDayNames[dnum] + ', ' + {$ENDIF}
      Pad(IntToStr(Day), 2, '0') + '.' +
      Pad(IntToStr(Month), 2, '0') + '.' +
      IntToStr(Year);

  Result := s;
end;


function MinToMs(Min: integer): integer;
begin
  Result := Min * 60000;
end;

function MsToMinStr(const xms: Int64; MinSecSeparator: string = ':'; MinPostfix: string = ''; SecPostfix: string = ''; MSecPostfix: string = ''): string;
var
  ms, xMin, xSec, xMSec: Int64;
begin
  ms := xms;

  xMin := ms div MSEC_IN_MIN;
  if xMin > 0 then ms := ms - xMin * MSEC_IN_MIN;

  xSec := ms div 1000;
  ms := ms - xSec * 1000;

  xMSec := ms;

  Result := itos(xMin) + MinPostfix + MinSecSeparator + Pad(itos(xSec), 2, '0') + SecPostfix + '.' + Pad(itos(xMSec), 3, '0') + MSecPostfix;
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


function MsToDateTime(Ms: DWORD): TDateTime;
var
  h, m, s, msec {, msecv}: DWORD;
begin
  h := ms div MsHour;
  ms := ms {%H-}- h * MsHour;
  m := ms div MsMin;
  ms := ms {%H-}- DWORD(m * MsMin);
  s := ms div MsSec;
  ms := ms {%H-}- s * MsSec;
  //msecv := ms;
  msec := ms div 100;

  Result := EncodeTime(h, m, s, msec);
end;

{$hints off}
function ParseTime(ms: DWORD; TimeFormat: TTimeFormat = tfMid; Trim0Hour: Boolean = False): string;
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
  msec := ms div 100; // 10-te czêœci sekundy


  if TimeFormat = tfShort then
    sr :=
      Pad(IntToStr(h), 2, '0') + ':' +
      Pad(IntToStr(m), 2, '0')

  else if TimeFormat = tfMid then
  begin
    if Trim0Hour and (h = 0) then
      sr :=
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0')
    else
      sr :=
        Pad(IntToStr(h), 2, '0') + ':' +
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0');
  end

  else if TimeFormat = tfLong then
  begin
    if Trim0Hour and (h = 0) then
      sr :=
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        IntToStr(msec)
    else
      sr :=
        Pad(IntToStr(h), 2, '0') + ':' +
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        IntToStr(msec);
  end

  else if TimeFormat = tfExtraLong then
  begin
    if Trim0Hour and (h = 0) then
      sr :=
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        Pad(IntToStr(msecv), 3, '0')
    else
      sr :=
        Pad(IntToStr(h), 2, '0') + ':' +
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        Pad(IntToStr(msecv), 3, '0');
  end

  else

  begin
    if Trim0Hour and (h = 0) then
      sr :=
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        Pad(IntToStr(msecv), 3, '0')
    else
      sr :=
        Pad(IntToStr(h), 2, '0') + ':' +
        Pad(IntToStr(m), 2, '0') + ':' +
        Pad(IntToStr(s), 2, '0') + '.' +
        Pad(IntToStr(msecv), 3, '0');
  end;

  Result := sr;
end;
{$hints on}





end.
