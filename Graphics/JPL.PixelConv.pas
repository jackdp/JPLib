unit JPL.PixelConv;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Graphics,
  JPL.Strings, JPL.Conversion;


{$IFDEF MSWINDOWS}
type

  {$region ' INT - IPxelConv '}
  IPixelConv = interface(IInterface)
    function GetDeviceContextHandle: HDC;

    function GetScreenWidthPix: integer;
    function GetScreenHeightPix: integer;
    function GetScreenWidthMm: integer;
    function GetScreenHeightMm: integer;
    function GetScreenWidthLogInch: Single;
    function GetScreenHeightLogInch: Single;

    function GetPixelsPerMillimeterX: Single;
    function GetPixelsPerMillimeterY: Single;
    function GetPixelsPerLogInchX: integer;
    function GetPixelsPerLogInchY: integer;

    function GetMillimetersPerPixelX: Single;
    function GetMillimetersPerPixelY: Single;
    function GetMillimetersPerLogInchX: Single;
    function GetMillimetersPerLogInchY: Single;

    function GetLogInchesPerPixelX: Single;
    function GetLogInchesPerPixelY: Single;
    function GetLogInchesPerMillimeterX: Single;
    function GetLogInchesPerMillimeterY: Single;

    procedure Reinit(Canvas: TCanvas); overload;
    procedure Reinit(DC: HDC); overload;
    function InfoStr(const FloatPrecission: Byte = 4): string;

    function MmToPixelsX(const Millimeters: Single): Single;
    function MmToPixelsY(const Millimeters: Single): Single;
    function MmToLogInchesX(const Millimeters: Single): Single;
    function MmToLogInchesY(const Millimeters: Single): Single;

    function PixelsToMmX(const Pixels: Single): Single;
    function PixelsToMmY(const Pixels: Single): Single;
    function PixelsToLogInchesX(const Pixels: Single): Single;
    function PixelsToLogInchesY(const Pixels: Single): Single;

    function LogInchesToMmX(const LogInches: Single): Single;
    function LogInchesToMmY(const LogInches: Single): Single;
    function LogInchesToPixelsX(const LogInches: Single): Single;
    function LogInchesToPixelsY(const LogInches: Single): Single;

    property DeviceContextHandle: HDC read GetDeviceContextHandle;

    property ScreenWidthPix: integer read GetScreenWidthPix;
    property ScreenHeightPix: integer read GetScreenHeightPix;
    property ScreenWidthMm: integer read GetScreenWidthMm;
    property ScreenHeightMm: integer read GetScreenHeightMm;
    property ScreenWidthLogInch: Single read GetScreenWidthLogInch;
    property ScreenHeightLogInch: Single read GetScreenHeightLogInch;

    property PixelsPerMillimeterX: Single read GetPixelsPerMillimeterX;
    property PixelsPerMillimeterY: Single read GetPixelsPerMillimeterY;
    property PixelsPerLogInchX: integer read GetPixelsPerLogInchX;
    property PixelsPerLogInchY: integer read GetPixelsPerLogInchY;

    property MillimetersPerPixelX: Single read GetMillimetersPerPixelX;
    property MillimetersPerPixelY: Single read GetMillimetersPerPixelY;
    property MillimetersPerLogInchX: Single read GetMillimetersPerLogInchX;
    property MillimetersPerLogInchY: Single read GetMillimetersPerLogInchY;

    property LogInchesPerPixelX: Single read GetLogInchesPerPixelX;
    property LogInchesPerPixelY: Single read GetLogInchesPerPixelY;
    property LogInchesPerMillimeterX: Single read GetLogInchesPerMillimeterX;
    property LogInchesPerMillimeterY: Single read GetLogInchesPerMillimeterY;
  end;
  {$endregion INT - IPxelConv}

  {$region ' INT - TPixelConv '}
  TPixelConv = class(TInterfacedObject, IPixelConv)
  private
    FDeviceContextHandle: HDC;
    FScreenWidthPix: integer;
    FScreenHeightPix: integer;
    FScreenWidthMm: integer;
    FScreenHeightMm: integer;
    FScreenWidthLogInch: Single;
    FScreenHeightLogInch: Single;
    FPixelsPerMillimeterX: Single;
    FPixelsPerMillimeterY: Single;
    FPixelsPerLogInchX: integer;
    FPixelsPerLogInchY: integer;
    FMillimetersPerPixelX: Single;
    FMillimetersPerPixelY: Single;
    FMillimetersPerLogInchX: Single;
    FMillimetersPerLogInchY: Single;
    FLogInchesPerPixelX: Single;
    FLogInchesPerPixelY: Single;
    FLogInchesPerMillimeterX: Single;
    FLogInchesPerMillimeterY: Single;

    function GetDeviceContextHandle: HDC;

    function GetScreenWidthPix: integer;
    function GetScreenHeightPix: integer;
    function GetScreenWidthMm: integer;
    function GetScreenHeightMm: integer;
    function GetScreenWidthLogInch: Single;
    function GetScreenHeightLogInch: Single;

    function GetPixelsPerMillimeterX: Single;
    function GetPixelsPerMillimeterY: Single;
    function GetPixelsPerLogInchX: integer;
    function GetPixelsPerLogInchY: integer;

    function GetMillimetersPerPixelX: Single;
    function GetMillimetersPerPixelY: Single;
    function GetMillimetersPerLogInchX: Single;
    function GetMillimetersPerLogInchY: Single;

    function GetLogInchesPerPixelX: Single;
    function GetLogInchesPerPixelY: Single;
    function GetLogInchesPerMillimeterX: Single;
    function GetLogInchesPerMillimeterY: Single;

  public
    constructor Create(Canvas: TCanvas); overload;
    constructor Create(DC: HDC); overload;

    procedure Reinit(Canvas: TCanvas); overload;
    procedure Reinit(DC: HDC); overload;
    function InfoStr(const FloatPrecission: Byte = 4): string;

    function MmToPixelsX(const Millimeters: Single): Single;
    function MmToPixelsY(const Millimeters: Single): Single;
    function MmToLogInchesX(const Millimeters: Single): Single;
    function MmToLogInchesY(const Millimeters: Single): Single;

    function PixelsToMmX(const Pixels: Single): Single;
    function PixelsToMmY(const Pixels: Single): Single;
    function PixelsToLogInchesX(const Pixels: Single): Single;
    function PixelsToLogInchesY(const Pixels: Single): Single;

    function LogInchesToMmX(const LogInches: Single): Single;
    function LogInchesToMmY(const LogInches: Single): Single;
    function LogInchesToPixelsX(const LogInches: Single): Single;
    function LogInchesToPixelsY(const LogInches: Single): Single;

    property DeviceContextHandle: HDC read GetDeviceContextHandle;

    property ScreenWidthPix: integer read GetScreenWidthPix;
    property ScreenHeightPix: integer read GetScreenHeightPix;
    property ScreenWidthMm: integer read GetScreenWidthMm;
    property ScreenHeightMm: integer read GetScreenHeightMm;
    property ScreenWidthLogInch: Single read GetScreenWidthLogInch;
    property ScreenHeightLogInch: Single read GetScreenHeightLogInch;

    property PixelsPerMillimeterX: Single read GetPixelsPerMillimeterX;
    property PixelsPerMillimeterY: Single read GetPixelsPerMillimeterY;
    property PixelsPerLogInchX: integer read GetPixelsPerLogInchX;
    property PixelsPerLogInchY: integer read GetPixelsPerLogInchY;

    property MillimetersPerPixelX: Single read GetMillimetersPerPixelX;
    property MillimetersPerPixelY: Single read GetMillimetersPerPixelY;
    property MillimetersPerLogInchX: Single read GetMillimetersPerLogInchX;
    property MillimetersPerLogInchY: Single read GetMillimetersPerLogInchY;

    property LogInchesPerPixelX: Single read GetLogInchesPerPixelX;
    property LogInchesPerPixelY: Single read GetLogInchesPerPixelY;
    property LogInchesPerMillimeterX: Single read GetLogInchesPerMillimeterX;
    property LogInchesPerMillimeterY: Single read GetLogInchesPerMillimeterY;
  end;
  {$endregion TPixelConv}

{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}


{$region ' ---------- TPixelConv ----------- '}

constructor TPixelConv.Create(Canvas: TCanvas);
begin
  Create(Canvas.Handle);
end;

constructor TPixelConv.Create(DC: HDC);
begin
  Reinit(DC);
end;

procedure TPixelConv.Reinit(Canvas: TCanvas);
begin
  Reinit(Canvas.Handle);
end;

procedure TPixelConv.Reinit(DC: HDC);
var
  xs: Single;
begin
  FDeviceContextHandle := DC;

  FScreenWidthPix := GetDeviceCaps(DC, HORZRES);    // screen width in pixels
  FScreenHeightPix := GetDeviceCaps(DC, VERTRES);   // screen height in pixels
  FScreenWidthMm := GetDeviceCaps(DC, HORZSIZE);    // screen width in mm
  FScreenHeightMm := GetDeviceCaps(DC, VERTSIZE);   // screen height in mm
  FPixelsPerLogInchX := GetDeviceCaps(DC, LOGPIXELSX); // Number of pixels per logical inch along the screen width
  FPixelsPerLogInchY := GetDeviceCaps(DC, LOGPIXELSY); // Number of pixels per logical inch along the screen height


  // Screen size in inches
  if PixelsPerLogInchX <> 0 then FScreenWidthLogInch := ScreenWidthPix / PixelsPerLogInchX else FScreenWidthLogInch := 0;
  if PixelsPerLogInchY <> 0 then FScreenHeightLogInch := ScreenHeightPix / PixelsPerLogInchY else FScreenHeightLogInch := 0;


  if ScreenWidthMm <> 0 then FPixelsPerMillimeterX := ScreenWidthPix / ScreenWidthMm else FPixelsPerMillimeterX := 0;
  if ScreenHeightMm <> 0 then FPixelsPerMillimeterY := ScreenHeightPix / ScreenHeightMm else FPixelsPerMillimeterY := 0;

  if ScreenWidthPix <> 0 then FMillimetersPerPixelX := ScreenWidthMm / ScreenWidthPix else FMillimetersPerPixelX := 0;
  if ScreenHeightPix <> 0 then FMillimetersPerPixelY := ScreenHeightMm / ScreenHeightPix else FMillimetersPerPixelY := 0;


  FMillimetersPerLogInchX := MillimetersPerPixelX * PixelsPerLogInchX;
  FMillimetersPerLogInchY := MillimetersPerPixelY * PixelsPerLogInchY;


  if PixelsPerLogInchX <> 0 then FLogInchesPerPixelX := 1 / PixelsPerLogInchX else FLogInchesPerPixelX := 0;
  if PixelsPerLogInchY <> 0 then FLogInchesPerPixelY := 1 / PixelsPerLogInchY else FLogInchesPerPixelY := 0;


  xs := MillimetersPerPixelX * PixelsPerLogInchX;
  if xs <> 0 then FLogInchesPerMillimeterX := 1 / xs else FLogInchesPerMillimeterX := 0;

  xs := MillimetersPerPixelY * PixelsPerLogInchY;
  if xs <> 0 then FLogInchesPerMillimeterY := 1 / xs else FLogInchesPerMillimeterY := 0;
end;


function TPixelConv.InfoStr(const FloatPrecission: Byte): string;
begin
  Result :=
    'Screen size in pixels: ' + itos(ScreenWidthPix) + ' x ' + itos(ScreenHeightPix) + ENDL +
    'Screen size in millimeters: ' + itos(ScreenWidthMm) + ' x ' + itos(ScreenHeightMm) + ENDL +
    'Screen size in inches: ' + ftos(ScreenWidthLogInch, FloatPrecission) + ' x ' + ftos(ScreenHeightLogInch, FloatPrecission) + ENDL +
    ENDL +
    'Horizontal parameters:' + ENDL +
    '    1 mm = ' + ftos(PixelsPerMillimeterX, FloatPrecission) + ' pix' + ENDL +
    '    1 mm = ' + ftos(LogInchesPerMillimeterX, FloatPrecission) + ' log. inch' + ENDL +
    '    1 pix = ' + ftos(MillimetersPerPixelX, FloatPrecission) + ' mm' + ENDL +
    '    1 pix = ' + ftos(LogInchesPerPixelX, FloatPrecission) + ' log. inch' + ENDL +
    '    1 log. inch = ' + itos(PixelsPerLogInchX) + ' pix' + ENDL +
    '    1 log. inch = ' + ftos(MillimetersPerLogInchX, FloatPrecission) + ' mm' + ENDL +
    ENDL +
    'Vertical parameters:' + ENDL +
    '    1 mm = ' + ftos(PixelsPerMillimeterY, FloatPrecission) + ' pix' + ENDL +
    '    1 mm = ' + ftos(LogInchesPerMillimeterY, FloatPrecission) + ' log. inch' + ENDL +
    '    1 pix = ' + ftos(MillimetersPerPixelY, FloatPrecission) + ' mm' + ENDL +
    '    1 pix = ' + ftos(LogInchesPerPixelY, FloatPrecission) + ' log. inch' + ENDL +
    '    1 log. inch = ' + itos(PixelsPerLogInchY) + ' pix' + ENDL +
    '    1 log. inch = ' + ftos(MillimetersPerLogInchY, FloatPrecission) + ' mm'
    ;
end;




// Millimeters to...
function TPixelConv.MmToPixelsX(const Millimeters: Single): Single;
begin
  Result := Millimeters * PixelsPerMillimeterX;
end;

function TPixelConv.MmToPixelsY(const Millimeters: Single): Single;
begin
  Result := Millimeters * PixelsPerMillimeterY;
end;

function TPixelConv.MmToLogInchesX(const Millimeters: Single): Single;
begin
  Result := Millimeters * LogInchesPerMillimeterX;
end;

function TPixelConv.MmToLogInchesY(const Millimeters: Single): Single;
begin
  Result := Millimeters * LogInchesPerMillimeterY;
end;



// Pixels to...
function TPixelConv.PixelsToMmX(const Pixels: Single): Single;
begin
  Result := Pixels * MillimetersPerPixelX;
end;

function TPixelConv.PixelsToMmY(const Pixels: Single): Single;
begin
  Result := Pixels * MillimetersPerPixelY;
end;

function TPixelConv.PixelsToLogInchesX(const Pixels: Single): Single;
begin
  Result := Pixels * LogInchesPerPixelX;
end;

function TPixelConv.PixelsToLogInchesY(const Pixels: Single): Single;
begin
  Result := Pixels * LogInchesPerPixelY;
end;



// Logical inches to...
function TPixelConv.LogInchesToMmX(const LogInches: Single): Single;
begin
  Result := LogInches * MillimetersPerLogInchX;
end;

function TPixelConv.LogInchesToMmY(const LogInches: Single): Single;
begin
  Result := LogInches * MillimetersPerLogInchY;
end;

function TPixelConv.LogInchesToPixelsX(const LogInches: Single): Single;
begin
  Result := LogInches * PixelsPerLogInchX;
end;

function TPixelConv.LogInchesToPixelsY(const LogInches: Single): Single;
begin
  Result := LogInches * PixelsPerLogInchY;
end;




function TPixelConv.GetDeviceContextHandle: HDC;
begin
  Result := FDeviceContextHandle;
end;

function TPixelConv.GetScreenWidthPix: integer;
begin
  Result := FScreenWidthPix;
end;

function TPixelConv.GetScreenHeightPix: integer;
begin
  Result := FScreenHeightPix;
end;

function TPixelConv.GetScreenWidthMm: integer;
begin
  Result := FScreenWidthMm;
end;

function TPixelConv.GetScreenHeightMm: integer;
begin
  Result := FScreenHeightMm;
end;

function TPixelConv.GetScreenWidthLogInch: Single;
begin
  Result := FScreenWidthLogInch;
end;

function TPixelConv.GetScreenHeightLogInch: Single;
begin
  Result := FScreenHeightLogInch;
end;

function TPixelConv.GetPixelsPerMillimeterX: Single;
begin
  Result := FPixelsPerMillimeterX;
end;

function TPixelConv.GetPixelsPerMillimeterY: Single;
begin
  Result := FPixelsPerMillimeterY;
end;

function TPixelConv.GetPixelsPerLogInchX: integer;
begin
  Result := FPixelsPerLogInchX;
end;

function TPixelConv.GetPixelsPerLogInchY: integer;
begin
  Result := FPixelsPerLogInchY;
end;

function TPixelConv.GetMillimetersPerPixelX: Single;
begin
  Result := FMillimetersPerPixelX;
end;

function TPixelConv.GetMillimetersPerPixelY: Single;
begin
  Result := FMillimetersPerPixelY;
end;

function TPixelConv.GetMillimetersPerLogInchX: Single;
begin
  Result := FMillimetersPerLogInchX;
end;

function TPixelConv.GetMillimetersPerLogInchY: Single;
begin
  Result := FMillimetersPerLogInchY;
end;

function TPixelConv.GetLogInchesPerPixelX: Single;
begin
  Result := FLogInchesPerPixelX;
end;

function TPixelConv.GetLogInchesPerPixelY: Single;
begin
  Result := FLogInchesPerPixelY;
end;

function TPixelConv.GetLogInchesPerMillimeterX: Single;
begin
  Result := FLogInchesPerMillimeterX;
end;

function TPixelConv.GetLogInchesPerMillimeterY: Single;
begin
  Result := FLogInchesPerMillimeterY;
end;

{$endregion TPixelConv}


{$ENDIF} // MSWINDOWS


end.

