unit JPL.Win.SimpleTimer;



{$IFDEF FPC}{$mode delphi}{$ENDIF}

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  Timers: https://docs.microsoft.com/en-us/windows/win32/winmsg/timers
  SetTimer: https://docs.microsoft.com/pl-pl/windows/win32/api/winuser/nf-winuser-settimer
  KillTimer: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-killtimer
  TimerProc: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nc-winuser-timerproc

  ----------------------------------------------------------

  TJPSimpleTimer - A timer based on WinAPI for Delphi/Lazarus/CodeTyphon. Designed for small console applications. No SysUtils, no Classes!

  Limitations: Only ONE instance at a time! Timer accuracy: approx. 10 ms.

  If you want to change the interval of an already running timer, call Stop, next change Interval property and call Start.

  You can call Start and Stop procedures as many times as you need, but you cannot create two instances at once!

  Time MAX = UINT = 2^32 ms = 4 294 967 296 ms = 4 294 967.296 s = 71 582.78827 min = 1 193.046471 h = 49.71026963 d
}


interface

{$IFDEF MSWINDOWS}

uses
  Windows, Messages;


const
  JPST_NO_COUNT_LIMIT = 0;

type

  TJPTimerProc = procedure(const Counter, ElapsedTotalTime: UINT);

  TJPSimpleTimer = class
  private
    FCounter: UINT;
    FInterval: UINT;
    FRepeatCountLimit: UINT;
    FTimerID: UINT;
    FTimerProc: TJPTimerProc;
    FTimerWindowHandle: HWND;
    FTimerWindowClass: TWndClass;
    FTotalTimeElapsed: UINT;
    FTickCount_Start: UINT;
    procedure SetCounter(AValue: UINT);
    procedure SetInterval(AValue: UINT);
    procedure SetRepeatCountLimit(AValue: UINT);
  protected
    property Counter: UINT read FCounter write SetCounter;
    property TimerWindowHandle: HWND read FTimerWindowHandle;
    property TimerID: UINT read FTimerID;
    property ToatlTimeElapsed: UINT read FTotalTimeElapsed;
    procedure DoTimerProc;
  public
    constructor Create(ATimerProc: TJPTimerProc = nil; const Interval: UINT = 1000; ARepeatCountLimit: UINT = JPST_NO_COUNT_LIMIT);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    // The time interval (in milliseconds) after which the TimerProc procedure will be called.
    property Interval: UINT read FInterval write SetInterval;

    // How many times to call TimerProc. 0 = no limit.
    property RepeatCountLimit: UINT read FRepeatCountLimit write SetRepeatCountLimit;
  end;

{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

var
  GTimer: TJPSimpleTimer = nil;
  GMsg: TMsg;


function WindowProc(hWnd, Msg, wParam, lParam: integer): integer; stdcall;
begin
  if Assigned(GTimer) then
  begin
    case Msg of
      WM_TIMER: GTimer.DoTimerProc;
      WM_DESTROY:
        begin
          KillTimer(GTimer.TimerWindowHandle, GTimer.TimerID);
          PostQuitMessage(0);
        end;
    end;
  end
  else
    if Msg = WM_DESTROY then PostQuitMessage(0);

  Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;



{$region '                         TJPSimpleTimer                         '}

constructor TJPSimpleTimer.Create(ATimerProc: TJPTimerProc = nil; const Interval: UINT = 1000; ARepeatCountLimit: UINT = JPST_NO_COUNT_LIMIT);
begin
  inherited Create;
  FRepeatCountLimit := 0;
  FTimerID := Random(10000);
  GTimer := Self;
  FTimerWindowHandle := 0;
  FInterval := Interval;
  FTimerProc := ATimerProc;
  FRepeatCountLimit := ARepeatCountLimit;
end;

destructor TJPSimpleTimer.Destroy;
begin
  GTimer := nil;
  if FTimerWindowHandle <> 0 then PostQuitMessage(0);
  inherited;
end;

procedure TJPSimpleTimer.Start;
begin
  if FTimerWindowHandle = 0 then
  begin
    FTimerWindowClass.style := 0;
    FTimerWindowClass.lpfnWndProc :=  @WindowProc;
    FTimerWindowClass.cbClsExtra := 0;
    FTimerWindowClass.cbWndExtra := 0;
    FTimerWindowClass.hInstance := HInstance;
    FTimerWindowClass.hIcon := 0;
    FTimerWindowClass.hCursor := 0;
    FTimerWindowClass.hbrBackground:= 0;
    FTimerWindowClass.lpszMenuName := nil;
    FTimerWindowClass.lpszClassName:= 'TJPST';

    RegisterClass(FTimerWindowClass);

    FTimerWindowHandle := CreateWindow(
      FTimerWindowClass.lpszClassName,      // Class name
      '',                                   // Window name (title)
      WS_OVERLAPPED and (not WS_VISIBLE),   // style
      0, 0, 0, 0,                           // Left, Top, Width, Height
      0,                                    // Parent window handle
      0,                                    // HMENU
      HInstance,                            // hInstance
      nil                                   // LParam
    );
  end;

  FCounter := 0;
  FTotalTimeElapsed := 0;
  FTickCount_Start := GetTickCount;
  SetTimer(FTimerWindowHandle, FTimerID, FInterval, nil);

  while GetMessage(GMsg, 0, 0, 0) do DispatchMessage(GMsg);
end;

procedure TJPSimpleTimer.Stop;
begin
  if FTimerWindowHandle <> 0 then
  begin
    KillTimer(FTimerWindowHandle, FTimerID);
    PostQuitMessage(0);
    FTimerWindowHandle := 0;
  end;
end;

procedure TJPSimpleTimer.SetCounter(AValue: UINT);
begin
  if FCounter = AValue then Exit;
  FCounter := AValue;
end;

procedure TJPSimpleTimer.SetInterval(AValue: UINT);
begin
  if FInterval = AValue then Exit;
  FInterval := AValue;
end;

procedure TJPSimpleTimer.SetRepeatCountLimit(AValue: UINT);
begin
  if FRepeatCountLimit = AValue then Exit;
  FRepeatCountLimit := AValue;
end;

procedure TJPSimpleTimer.DoTimerProc;
begin
  Inc(FCounter);
  FTotalTimeElapsed := GetTickCount - FTickCount_Start;
  if (FRepeatCountLimit > JPST_NO_COUNT_LIMIT) and (FCounter >= FRepeatCountLimit) then
  begin
    if Assigned(FTimerProc) then FTimerProc(FCounter, FTotalTimeElapsed);
    Stop;
  end
  else if Assigned(FTimerProc) then FTimerProc(FCounter, FTotalTimeElapsed);
end;

{$endregion TJPSimpleTimer}



{$ENDIF} // MSWINDOWS

end.

