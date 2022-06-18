unit JPL.Win.Mouse;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}

interface

{$IFDEF MSWINDOWS}
uses
  Windows, Messages,
  SysUtils, Classes,
  JPL.Strings, JPL.Conversion;

const
  WH_MOUSE_LL = 14;
  SPI_GETWHEELSCROLLCHARS = $006C;
  SPI_SETWHEELSCROLLCHARS = $006D;


type
  TPixelInfo = record
    WidthMM: Real;
    HeightMM: Real;
  end;

  TVerticalScrollingMode = (vsmLines, vsmPage);


  TJPMouse = class
  private
    FPixelInfo: TPixelInfo;
    FMousePresent: Boolean;
    FWheelPresent: Boolean;
    FWheelScrollLines: DWORD;
    FDoubleClickTime: DWORD;
    FDoubleClickRectWidth: integer;
    FDoubleClickRectHeight: integer;
    FButtonsSwapped: Boolean;
    FMouseAccelThresholdX: integer;
    FMouseAccelThresholdY: integer;
    FMouseAcceleration: Boolean;
    FMouseSpeed: integer;
    FClickLockEnabled: Boolean;
    FClickLockTime: integer;
    FScreenWidth: integer;
    FScreenHeight: integer;
    FWheelScrollChars: DWORD;
    FVerticalScrollingMode: TVerticalScrollingMode;
    FSonarEnabled: Boolean;
    FTrailsEnabled: Boolean;
    FTrailsCount: integer;
    FSnapToDefButton: Boolean;
    FHidePointerWhileTyping: Boolean;
    function GetCursorPos: TPoint;

    procedure UpdateMousePresent;
    procedure UpdateWheelParams;
    procedure UpdateAccelThresholdInfo;
    procedure UpdateMouseSpeed;
    procedure UpdateClickLockEnabled;
    procedure UpdateClickLockTime;
    procedure UpdateCXDoubleClick;
    procedure UpdateCYDoubleClick;
    procedure UpdateDoubleClickTime;
    procedure UpdateButtonsSwapped;
    procedure UpdateScreenWidth;
    procedure UpdateScreenHeight;
    procedure UpdateSonarEnabled;
    procedure UpdateTrailsCount;
    procedure UpdateSnapToDefButton;
    procedure UpdateHidePointerWhileTyping;

    procedure SetDoubleClickTime(const Value: DWORD);
    procedure SetMouseSpeed(const Value: integer);
    procedure SwapButtons(Swap: Boolean);
    procedure SetDoubleClickRectWidth(const Value: integer);
    procedure SetDoubleClickRectHeight(const Value: integer);
    procedure SetClickLockEnabled(const Value: Boolean);
    procedure SetClickLockTime(const Value: integer);
    procedure SetWheelScrollChars(const Value: DWORD);
    procedure SetWheelScrollLines(const Value: DWORD);
    procedure SetVerticalScrollingMode(const Value: TVerticalScrollingMode);
    procedure SetSonarEnabled(const Value: Boolean);
    procedure SetTrailsCount(const Value: integer);
    procedure SetSnapToDefButton(const Value: Boolean);
    procedure SetHidePointerWhileTyping(const Value: Boolean);
    procedure SetMouseAcceleration(const Value: Boolean);
    procedure SetMouseAccelThresholdX(const Value: integer);
    procedure SetMouseAccelThresholdY(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdatePixelInfo;
    procedure UpdateAllInformations;
    function GetReportStr: string;
    procedure SaveReport(const FileName: string);
    procedure BroadcastSettingChange(Msg: DWORD = 0; lPar: LPARAM = 0);
    procedure SetMouseAccelParams(const ENableAcceleration: Boolean; const ThresholdX, ThresholdY: integer);

    property CursorPos: TPoint read GetCursorPos;
    property PixelInfo: TPixelInfo read FPixelInfo;

    property MousePresent: Boolean read FMousePresent;
    property WheelPresent: Boolean read FWheelPresent;

    property WheelScrollLines: DWORD read FWheelScrollLines write SetWheelScrollLines;
    property WheelScrollChars: DWORD read FWheelScrollChars write SetWheelScrollChars;
    property VerticalScrollingMode: TVerticalScrollingMode read FVerticalScrollingMode write SetVerticalScrollingMode;

    property DoubleClickTime: DWORD read FDoubleClickTime write SetDoubleClickTime;
    property DoubleClickRectWidth: integer read FDoubleClickRectWidth write SetDoubleClickRectWidth;
    property DoubleClickRectHeight: integer read FDoubleClickRectHeight write SetDoubleClickRectHeight;

    property ButtonsSwapped: Boolean read FButtonsSwapped write SwapButtons;

    property MouseSpeed: integer read FMouseSpeed write SetMouseSpeed;
    property MouseAccelThresholdX: integer read FMouseAccelThresholdX write SetMouseAccelThresholdX;
    property MouseAccelThresholdY: integer read FMouseAccelThresholdY write SetMouseAccelThresholdY;
    property MouseAcceleration: Boolean read FMouseAcceleration write SetMouseAcceleration;

    property ClickLockEnabled: Boolean read FClickLockEnabled write SetClickLockEnabled;
    property ClickLockTime: integer read FClickLockTime write SetClickLockTime;

    property SonarEnabled: Boolean read FSonarEnabled write SetSonarEnabled;

    property TrailsEnabled: Boolean read FTrailsEnabled; // To enable trails, set TrailsCount > 1
    property TrailsCount: integer read FTrailsCount write SetTrailsCount;

    property SnapToDefButton: Boolean read FSnapToDefButton write SetSnapToDefButton;

    property HidePointerWhileTyping: Boolean read FHidePointerWhileTyping write SetHidePointerWhileTyping;

    property ScreenWidth: integer read FScreenWidth;
    property ScreenHeight: integer read FScreenHeight;
  end;

{$ENDIF} // MSWINDOWS



implementation


{$IFDEF MSWINDOWS}

constructor TJPMouse.Create;
begin
  UpdateAllInformations;
end;

destructor TJPMouse.Destroy;
begin
  inherited;
end;


procedure TJPMouse.UpdateAllInformations;
begin
  UpdateScreenWidth;
  UpdateScreenHeight;
  UpdatePixelInfo;
  UpdateMousePresent;
  UpdateWheelParams;
  UpdateDoubleClickTime;

  UpdateCXDoubleClick;
  UpdateCYDoubleClick;
  UpdateButtonsSwapped;

  UpdateAccelThresholdInfo;
  UpdateMouseSpeed;
  UpdateClickLockEnabled;
  UpdateClickLockTime;

  UpdateSonarEnabled;
  UpdateTrailsCount;
  UpdateSnapToDefButton;
  UpdateHidePointerWhileTyping;
end;






{$region '                                   Report                                    '}
function TitleStr(Title: string): string;
begin
  Result := '--- ' + PadRight(Title + ' ', 60, '-');
end;

function TJPMouse.GetReportStr: string;
const
  M1 = '  ';
begin
  Result :=
    'Mouse present: ' + BoolToStr(MousePresent, 'Yes', 'No') + CRLF +
    'Wheel present: ' + BoolToStr(WheelPresent, 'Yes', 'No') + CRLF;


  if WheelPresent then
  begin
    Result := Result + TitleStr('WHEEL') + CRLF;
    if VerticalScrollingMode = vsmPage then
    begin
      Result := Result + M1 + 'Vertical scrolling mode: Page' + CRLF;
      Result := Result + M1 + 'Wheel scroll lines: ' + itos(WheelScrollLines) + CRLF;
    end
    else
    begin
      Result := Result + M1 + 'Vertical scrolling mode: Lines' + CRLF;
      Result := Result + M1 + 'Wheel scroll lines: ' + itos(WheelScrollLines) + CRLF;
    end;
    Result := Result + M1 + 'Wheel scroll chars: ' + itos(WheelScrollChars) + CRLF;
  end;


  Result := Result + TitleStr('DOUBLE CLICK') + CRLF;
  Result := Result +
    M1 + 'Double-click time: ' + itos(DoubleClickTime) + ' ms (system default: 500 ms)' + CRLF +
    M1 + 'Double click rectangle: ' + itos(DoubleClickRectWidth) + ' x ' + itos(DoubleClickRectHeight) + ' (system default: 4 x 4)' + CRLF;



  Result := Result + TitleStr('BUTTONS') + CRLF;
  Result := Result +
    M1 + 'Buttons are swapped: ' + BoolToStr(ButtonsSwapped, 'Yes', 'No') + CRLF;


  Result := Result + M1 + 'ClickLock enabled: ' + BoolToStr(ClickLockEnabled, 'Yes', 'No') + CRLF;
  Result := Result + M1 + 'ClickLock time: ' + itos(ClickLockTime) + ' ms (system default: 1200 ms)' + CRLF;



  Result := Result + TitleStr('POINTER OPTIONS') + ENDL;
  Result := Result + M1 + 'Mouse speed: ' + itos(MouseSpeed) + ' (range: 1..20 / system default: 10)' + ENDL;
  Result := Result + M1 + 'Mouse acceleration enabled: ' + BoolToStr(MouseAcceleration, 'Yes', 'No') + CRLF;
  if MouseAcceleration then Result := Result +
    M1 + 'Mouse threshold X: ' + itos(MouseAccelThresholdX) + ' (system default: 6)' + CRLF +
    M1 + 'Mouse threshold Y: ' + itos(MouseAccelThresholdY) + ' (system default: 10)' + CRLF;
  Result := Result + M1 + 'Mouse sonar enabled: ' + BoolToStrYN(FSonarEnabled) + ENDL;
  Result := Result + M1 + 'Trails enabled: ' + BoolToStrYN(FTrailsEnabled) + ENDL;
  if FTrailsEnabled then Result := Result + M1 + 'Trails count: ' + itos(FTrailsCount) + ENDL;

  Result := Result + M1 + 'Snap to default button: ' + BoolToStrYN(FSnapToDefButton) + ENDL;

  Result := Result + M1 + 'Hide pointer while typing: ' + BoolToStrYN(FHidePointerWhileTyping) + ENDL;

  Result := Result +
    '';
end;

procedure TJPMouse.SaveReport(const FileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := GetReportStr;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;
{$endregion Report}



procedure TJPMouse.UpdateHidePointerWhileTyping;
var
  b: BOOL;
begin
  FHidePointerWhileTyping := False;
  if SystemParametersInfo(SPI_GETMOUSEVANISH, 0, @b, 0) then FHidePointerWhileTyping := b;
end;


procedure TJPMouse.SetHidePointerWhileTyping(const Value: Boolean);
begin
  if SystemParametersInfo(SPI_SETMOUSEVANISH, 0, Pointer(Value), 0) then BroadcastSettingChange;
  UpdateHidePointerWhileTyping;
end;


{$region '                              SnapToDefButton                        '}
procedure TJPMouse.UpdateSnapToDefButton;
var
  b: BOOL;
begin
  FSnapToDefButton := False;
  if SystemParametersInfo(SPI_GETSNAPTODEFBUTTON, 0, @b, 0) then FSnapToDefButton := b;
end;

procedure TJPMouse.SetSnapToDefButton(const Value: Boolean);
begin
  if SystemParametersInfo(SPI_SETSNAPTODEFBUTTON, integer(Value), nil, 0) then BroadcastSettingChange;
  UpdateSnapToDefButton;
end;
{$endregion SnapToDefButton}


{$region '                              Trails                                     '}
procedure TJPMouse.SetTrailsCount(const Value: integer);
begin
  if SystemParametersInfo(SPI_SETMOUSETRAILS, Value, nil, 0) then BroadcastSettingChange;
  UpdateTrailsCount;
end;

procedure TJPMouse.UpdateTrailsCount;
var
  x: integer;
begin
  FTrailsEnabled := False;
  FTrailsCount := 0;
  if SystemParametersInfo(SPI_GETMOUSETRAILS, 0, @x, 0) then
    if x > 1 then
    begin
      FTrailsEnabled := True;
      FTrailsCount := x;
    end
    else
    begin
      FTrailsEnabled := False;
      FTrailsCount := 0;
    end;
end;
{$endregion Trails}


{$region '                               Sonar                                '}
procedure TJPMouse.UpdateSonarEnabled;
var
  b: BOOL;
begin
  FSonarEnabled := False;
  if SystemParametersInfo(SPI_GETMOUSESONAR, 0, @b, 0) then FSonarEnabled := b;
end;

procedure TJPMouse.SetSonarEnabled(const Value: Boolean);
begin
  if SystemParametersInfo(SPI_SETMOUSESONAR, 0, Pointer(Value), 0) then BroadcastSettingChange;
  UpdateSonarEnabled;
end;
{$endregion Sonar}


{$region '                             ClickLock                                    '}
procedure TJPMouse.SetClickLockTime(const Value: integer);
begin
  if SystemParametersInfo(SPI_SETMOUSECLICKLOCKTIME, 0, Pointer(Value), 0) then BroadcastSettingChange;
  UpdateClickLockTime;
end;

procedure TJPMouse.SetClickLockEnabled(const Value: Boolean);
begin
  if SystemParametersInfo(SPI_SETMOUSECLICKLOCK, 0, Pointer(Value), 0) then BroadcastSettingChange;
  UpdateClickLockEnabled;
end;

procedure TJPMouse.UpdateClickLockEnabled;
var
  b: BOOL;
begin
  SystemParametersInfo(SPI_GETMOUSECLICKLOCK, 0, @b, 0);
  FClickLockEnabled := b;
end;

procedure TJPMouse.UpdateClickLockTime;
var
  x: DWORD;
begin
  SystemParametersInfo(SPI_GETMOUSECLICKLOCKTIME, 0, @x, 0);
  FClickLockTime := x;
end;
{$endregion ClickLock}


{$region '                               Double-Click                                     '}
procedure TJPMouse.SetDoubleClickRectWidth(const Value: integer);
begin
  if SystemParametersInfo(SPI_SETDOUBLECLKWIDTH, Value, nil, 0) then BroadcastSettingChange;
  UpdateCXDoubleClick;
end;

procedure TJPMouse.SetDoubleClickRectHeight(const Value: integer);
begin
  if SystemParametersInfo(SPI_SETDOUBLECLKHEIGHT, Value, nil, 0) then BroadcastSettingChange;
  UpdateCYDoubleClick;
end;

procedure TJPMouse.SetDoubleClickTime(const Value: DWORD);
begin
  Windows.SetDoubleClickTime(Value);
  BroadcastSettingChange;
  UpdateDoubleClickTime;
end;

procedure TJPMouse.UpdateCXDoubleClick;
begin
  FDoubleClickRectWidth := GetSystemMetrics(SM_CXDOUBLECLK);
end;

procedure TJPMouse.UpdateCYDoubleClick;
begin
  FDoubleClickRectHeight := GetSystemMetrics(SM_CYDOUBLECLK);
end;

procedure TJPMouse.UpdateDoubleClickTime;
begin
  FDoubleClickTime := Windows.GetDoubleClickTime;
end;

{$endregion Double-Click}


{$region '                             Mouse Speed                          '}

procedure TJPMouse.SetMouseSpeed(const Value: integer);
begin
  if SystemParametersInfo(SPI_SETMOUSESPEED, 0, Pointer(Value), 0) then
  begin
    UpdateMouseSpeed;
    BroadcastSettingChange;
  end;
end;

procedure TJPMouse.UpdateMouseSpeed;
var
  x: integer;
begin
  if SystemParametersInfo(SPI_GETMOUSESPEED, 0, @x, 0) then FMouseSpeed := x;
end;

procedure TJPMouse.SetMouseAcceleration(const Value: Boolean);
begin
  UpdateAccelThresholdInfo;
  if FMouseAcceleration = Value then Exit;
  SetMouseAccelParams(Value, FMouseAccelThresholdX, FMouseAccelThresholdY);
end;

procedure TJPMouse.SetMouseAccelThresholdX(const Value: integer);
begin
  UpdateAccelThresholdInfo;
  if FMouseAccelThresholdX = Value then Exit;
  SetMouseAccelParams(FMouseAcceleration, Value, FMouseAccelThresholdY);
end;

procedure TJPMouse.SetMouseAccelThresholdY(const Value: integer);
begin
  UpdateAccelThresholdInfo;
  if FMouseAccelThresholdY = Value then Exit;
  SetMouseAccelParams(FMouseAcceleration, FMouseAccelThresholdX, Value);
end;

procedure TJPMouse.SetMouseAccelParams(const ENableAcceleration: Boolean; const ThresholdX, ThresholdY: integer);
var
  Arr: array[0..2] of integer;
begin
  FillChar(Arr{%H-}, SizeOf(Arr), 0);
  Arr[0] := ThresholdX;  // system default: 6
  Arr[1] := ThresholdY;  // system default: 10
  Arr[2] := integer(BOOL(ENableAcceleration));
  if SystemParametersInfo(SPI_SETMOUSE, 0, @Arr, 0) then BroadcastSettingChange;
  UpdateAccelThresholdInfo;
end;


procedure TJPMouse.UpdateAccelThresholdInfo;
var
  Arr: array[0..2] of integer;
begin
  FillChar(Arr{%H-}, SizeOf(Arr), 0);
  if SystemParametersInfo(SPI_GETMOUSE, 0, @Arr, 0) then
  begin
    FMouseAccelThresholdX := Arr[0];
    FMouseAccelThresholdY := Arr[1];
    FMouseAcceleration := BOOL(Arr[2]);
  end;
end;
{$endregion Mouse Speed}


{$region '                          Buttons Swapping                           '}
procedure TJPMouse.SwapButtons(Swap: Boolean);
begin
  SwapMouseButton(Swap);
  BroadcastSettingChange;
  UpdateButtonsSwapped;
end;

procedure TJPMouse.UpdateButtonsSwapped;
begin
  FButtonsSwapped := BOOL(GetSystemMetrics(SM_SWAPBUTTON));
end;
{$endregion Buttons Swapping}


{$region '                              Wheel                              '}
procedure TJPMouse.UpdateWheelParams;
var
  x: Cardinal;
begin
  FWheelPresent := BOOL(GetSystemMetrics(SM_MOUSEWHEELPRESENT));
  if FWheelPresent then
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @x, 0);
    SystemParametersInfo(SPI_GETWHEELSCROLLCHARS, 0, @FWheelScrollChars, 0);

    if x = WHEEL_PAGESCROLL then FVerticalScrollingMode := vsmPage
    else
    begin
      FVerticalScrollingMode := vsmLines;
      FWheelScrollLines := x;
    end;
  end

  else

  begin
    FWheelScrollLines := 0;
    FWheelScrollChars := 0;
  end;
end;

procedure TJPMouse.SetWheelScrollChars(const Value: DWORD);
begin
  if not WheelPresent then Exit;
  SystemParametersInfo(SPI_SETWHEELSCROLLCHARS, Value, nil, SPIF_UPDATEINIFILE);
  BroadcastSettingChange;
  //SystemParametersInfo(SPI_GETWHEELSCROLLCHARS, 0, @FWheelScrollChars, 0);
  UpdateWheelParams;
end;

procedure TJPMouse.SetWheelScrollLines(const Value: DWORD);
begin
  if not WheelPresent then Exit;
  SystemParametersInfo(SPI_SETWHEELSCROLLLINES, Value, nil, SPIF_UPDATEINIFILE);
  BroadcastSettingChange;
  //SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0);
  UpdateWheelParams;
end;

procedure TJPMouse.SetVerticalScrollingMode(const Value: TVerticalScrollingMode);
begin
  FVerticalScrollingMode := Value;
  if Value = vsmPage then SystemParametersInfo(SPI_SETWHEELSCROLLLINES, WHEEL_PAGESCROLL, nil, SPIF_UPDATEINIFILE)
  else SystemParametersInfo(SPI_SETWHEELSCROLLLINES, WheelScrollLines, nil, SPIF_UPDATEINIFILE);
  BroadcastSettingChange;
  UpdateWheelParams;
end;
{$endregion Wheel}

function TJPMouse.GetCursorPos: TPoint;
begin
  Windows.GetCursorPos(Result);
end;

procedure TJPMouse.UpdateMousePresent;
begin
  FMousePresent := BOOL(GetSystemMetrics(SM_MOUSEPRESENT));
end;

procedure TJPMouse.UpdatePixelInfo;
var
  dc: HDC;
  xWidthMM, xHeightMM: integer;
begin
  dc := GetDC(0);
  xWidthMM := GetDeviceCaps(dc, HORZSIZE);  // Width, in millimeters, of the physical screen.
  xHeightMM := GetDeviceCaps(dc, VERTSIZE); // Height, in millimeters, of the physical screen.

  ReleaseDC(0, dc);

  FPixelInfo.WidthMM := xWidthMM / ScreenWidth;
  FPixelInfo.HeightMM := xHeightMM / ScreenHeight;
end;

procedure TJPMouse.UpdateScreenHeight;
begin
  FScreenHeight := GetSystemMetrics(SM_CYSCREEN);
end;

procedure TJPMouse.UpdateScreenWidth;
begin
  FScreenWidth := GetSystemMetrics(SM_CXSCREEN);
end;

procedure TJPMouse.BroadcastSettingChange(Msg: DWORD; lPar: LPARAM);
begin
  SendNotifyMessage(HWND_BROADCAST, WM_SETTINGCHANGE, Msg, lPar);
end;

{$ENDIF} // MSWINDOWS


end.
