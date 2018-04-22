unit JPL.TimeLogger;

{
  Jacek Pazera
  http://www.pazera-software.com
}

// Acquiring high-resolution time stamps:
//   https://msdn.microsoft.com/en-us/library/windows/desktop/dn553408(v=vs.85).aspx

interface

{$IFDEF FPC}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils;
  //Classes,
  //JP_DateTime;

type

  TTimeLogger = record
  private
    class var xStart: Int64;
    class var xEnd: Int64;

    class var xStart_2: Int64;
    class var xEnd_2: Int64;
  public
    class procedure StartLog; static;
    class procedure EndLog; static;
    class function ElapsedTime: Int64; static;
    class function ElapsedTimeStr: string; static;

    class procedure StartLog_2; static;
    class procedure EndLog_2; static;
    class function ElapsedTime_2: Int64; static;
    class function ElapsedTimeStr_2: string; static;
  end;

  TClassTimeLogger = class
  private
    FStartMs: Int64;
    FEndMs: Int64;
    FStartMs_2: Int64;
    FEndMs_2: Int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartLog;
    procedure EndLog;
    function ElapsedTimeMs: Int64;
    function ElapsedTimeStr: string;

    procedure StartLog_2;
    procedure EndLog_2;
    function ElapsedTimeMs_2: Int64;
    function ElapsedTimeStr_2: string;

    property StartMs: Int64 read FStartMs;
    property EndMs: Int64 read FEndMs;

    property StartMs_2: Int64 read FStartMs_2;
    property EndMs_2: Int64 read FEndMs_2;
  end;


function MsToTimeStr(ms: integer; RoundToSeconds: Boolean = False): string;

implementation

{$hints off}
function GetTimeCounterMS: Int64;
{$IFDEF MSWINDOWS}
var
  Counter, Freq: Int64;
{$ENDIF}
begin
  Result := {$IFDEF FPC}GetTickCount64 {$ELSE} GetTickCount {$ENDIF};
  {$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(Freq);
  if Freq = 0 then Exit;
  if not QueryPerformanceCounter(Counter) then Exit;
  Result := Round(1000 * Counter / Freq);
  {$ENDIF}
end;
{$hints on}

function Pad(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if length(Text) < i then
  begin
    x := length(Text);
    y := i - x;
    for k := 1 to y do s := s + znak;
    Text := s + Text;
  end;
  Result := Text;
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
    Result := Pad(IntToStr(h), 2, '0') + ':' + Pad(IntToStr(m), 2, '0') + ':' + Pad(IntToStr(s), 2, '0');
  end
  else Result := Pad(IntToStr(h), 2, '0') + ':' + Pad(IntToStr(m), 2, '0') + ':' + Pad(IntToStr(s), 2, '0') + '.' + Pad(IntToStr(ms), 3, '0');
end;


{$region ' ----------- TTimeLogger - record ------------- '}
class function TTimeLogger.ElapsedTime: Int64;
begin
  Result := xEnd - xStart;
end;

class function TTimeLogger.ElapsedTimeStr: string;
begin
  Result := MsToTimeStr(ElapsedTime);
end;

class procedure TTimeLogger.EndLog;
begin
  xEnd := GetTimeCounterMS;
end;

class procedure TTimeLogger.StartLog;
begin
  xStart := GetTimeCounterMS;
end;


class function TTimeLogger.ElapsedTime_2: Int64;
begin
  Result := xEnd_2 - xStart_2;
end;

class function TTimeLogger.ElapsedTimeStr_2: string;
begin
  Result := MsToTimeStr(ElapsedTime_2);
end;

class procedure TTimeLogger.EndLog_2;
begin
  xEnd_2 := GetTimeCounterMS;
end;

class procedure TTimeLogger.StartLog_2;
begin
  xStart_2 := GetTimeCounterMS;
end;
{$endregion TTimeLogger - record}


{$region ' ------------ TClassTimeLogger ---------------- '}
constructor TClassTimeLogger.Create;
begin
  inherited;
  FStartMs := GetTimeCounterMS;
  FStartMs_2 := FStartMs;
end;

destructor TClassTimeLogger.Destroy;
begin
  inherited;
end;

function TClassTimeLogger.ElapsedTimeMs: Int64;
begin
  Result := FEndMs - FStartMs;
end;

function TClassTimeLogger.ElapsedTimeMs_2: Int64;
begin
  Result := FEndMs_2 - FStartMs_2;
end;

function TClassTimeLogger.ElapsedTimeStr: string;
begin
  Result := MsToTimeStr(ElapsedTimeMs);
end;

function TClassTimeLogger.ElapsedTimeStr_2: string;
begin
  Result := MsToTimeStr(ElapsedTimeMs_2);
end;

procedure TClassTimeLogger.EndLog;
begin
  FEndMs := GetTimeCounterMS;
end;

procedure TClassTimeLogger.EndLog_2;
begin
  FEndMs_2 := GetTimeCounterMS;
end;

procedure TClassTimeLogger.StartLog;
begin
  FStartMs := GetTimeCounterMS;
end;

procedure TClassTimeLogger.StartLog_2;
begin
  FStartMs_2 := GetTimeCounterMS;
end;

{$endregion TClassTimeLogger}



initialization


end.
