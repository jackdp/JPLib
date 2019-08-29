unit JPL.Colors.ColorClass;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  //{$IFDEF MSWINDOWS} Windows,{$ENDIF}
  //{$IFDEF FPC} LCLType, {$ENDIF} // < TRGBTriple, TRBGQuad, MulDiv ...
  Graphics, SysUtils,
  //Classes, Math,
  JPL.Colors;
  //, JPL.Math, JPL.Strings, JPL.Conversion;


type

  TPercentInt = 0..100;

  TColorClass = class
  private
    const DEFAULT_COLOR = clBlack;
  private
    FColor: TColor;
    FHtmlColorStr: string;
    FHtmlColorPrefix: string;
    FRgbIntStr: string;
    FValid: Boolean;
    FErrorStr: string;
    FRgbPercentIntStr: string;
    FBgrIntStr: string;
    FRed: Byte;
    FGreen: Byte;
    FBlue: Byte;
    FPascalHexStr: string;
    FRgbHex: string;
    FBgrHex: string;
    FPascalIntStr: string;
    FCmykStr: string;
    FHslCssStr: string;
    FHslSysStr: string;
    FCppHex: string;
    FCyan: integer;
    FMagenta: integer;
    FYellow: integer;
    FBlack: integer;
    FInvertedColor: TColor;
    FRedPercentInt: TPercentInt;
    FGreenPercentInt: TPercentInt;
    FBluePercentInt: TPercentInt;
    FHueCss: integer;
    FSatCss: TPercentInt;
    FLumCss: TPercentInt;
    FHue: integer;
    FSat: integer;
    FLum: integer;
    procedure UpdateAll;
    procedure SetColor(const Value: TColor);
    procedure SetHtmlColorStr(const Value: string);
    procedure SetHtmlColorPrefix(const Value: string);
    procedure SetRgbIntStr(const Value: string);
    function GetErrorStr(const sInvalidColorStr: string): string;
    procedure SetRgbPercentIntStr(const Value: string);
    procedure SetBgrIntStr(const Value: string);
    procedure SetRed(const Value: Byte);
    procedure SetGreen(const Value: Byte);
    procedure SetBlue(const Value: Byte);
    procedure SetPascalHexStr(const Value: string);
    procedure SetRgbHex(const Value: string);
    procedure SetBgrHex(const Value: string);
    procedure SetPascalIntStr(const Value: string);
    procedure SetCmykStr(const Value: string);
    procedure SetHslCssStr(const Value: string);
    procedure SetHslSysStr(const Value: string);
    procedure SetCppHex(const Value: string);
    procedure SetCyan(const Value: integer);
    procedure SetMagenta(const Value: integer);
    procedure SetYellow(const Value: integer);
    procedure SetBlack(const Value: integer);
    procedure SetRedPercentInt(const Value: TPercentInt);
    procedure SetGreenPercentInt(const Value: TPercentInt);
    procedure SetBluePercentInt(const Value: TPercentInt);
    procedure SetHueCss(const Value: integer);
    procedure SetSatCss(const Value: TPercentInt);
    procedure SetLumCss(const Value: TPercentInt);
    procedure SetHue(const Value: integer);
    procedure SetSat(const Value: integer);
    procedure SetLum(const Value: integer);
  public
    constructor Create; overload;
    constructor Create(const Color: TColor); overload;
    destructor Destroy; override;

    // ------------------------------------------------------
    property Color: TColor read FColor write SetColor;
    // ------------------------------------------------------

    procedure SetRGB(const Red, Green, Blue: Byte);
    procedure SetBGR(const Blue, Green, Red: Byte);
    procedure SetCMYK(const Cyan, Magenta, Yellow, Black: integer);

    // If the input string cannot be converted to TColor, Valid is set to False
    property Valid: Boolean read FValid;
    property ErrorStr: string read FErrorStr;

    // RGB & BGR
    property RgbHex: string read FRgbHex write SetRgbHex;
    property RgbIntStr: string read FRgbIntStr write SetRgbIntStr;
    property Red: Byte read FRed write SetRed;
    property Green: Byte read FGreen write SetGreen;
    property Blue: Byte read FBlue write SetBlue;
    property RgbPercentIntStr: string read FRgbPercentIntStr write SetRgbPercentIntStr;
    property RedPercentInt: TPercentInt read FRedPercentInt write SetRedPercentInt;
    property GreenPercentInt: TPercentInt read FGreenPercentInt write SetGreenPercentInt;
    property BluePercentInt: TPercentInt read FBluePercentInt write SetBluePercentInt;
    property BgrIntStr: string read FBgrIntStr write SetBgrIntStr;
    property BgrHex: string read FBgrHex write SetBgrHex;

    // CMYK
    property CmykStr: string read FCmykStr write SetCmykStr;
    property Cyan: integer read FCyan write SetCyan;
    property Magenta: integer read FMagenta write SetMagenta;
    property Yellow: integer read FYellow write SetYellow;
    property Black: integer read FBlack write SetBlack;

    // HSL
    property HslSysStr: string read FHslSysStr write SetHslSysStr; // Windows color dialog - MaxHue: 239, MaxSat: 240, MaxLum: 240
    property Hue: integer read FHue write SetHue;
    property Sat: integer read FSat write SetSat;
    property Lum: integer read FLum write SetLum;
    property HslCssStr: string read FHslCssStr write SetHslCssStr; // MaxHue: 360, MaxSat: 100, MaxLum: 100
    property HueCss: integer read FHueCss write SetHueCss;
    property SatCss: TPercentInt read FSatCss write SetSatCss;
    property LumCss: TPercentInt read FLumCss write SetLumCss;

    // Other
    property HtmlColorStr: string read FHtmlColorStr write SetHtmlColorStr;
    property HtmlColorPrefix: string read FHtmlColorPrefix write SetHtmlColorPrefix;
    property PascalHexStr: string read FPascalHexStr write SetPascalHexStr;
    property PascalIntStr: string read FPascalIntStr write SetPascalIntStr;
    property CppHex: string read FCppHex write SetCppHex;


    property InvertedColor: TColor read FInvertedColor;
  end;


implementation


// RGB
// HTML
// RGB%
// BGR
// Pascal HEX
// Pascal INT
// CMYK
// HSL CSS
// HSL SYS
// C++ HEX

{$region '                               TColorClass                                '}

constructor TColorClass.Create;
begin
  Create(DEFAULT_COLOR);
end;

constructor TColorClass.Create(const Color: TColor);
begin
  inherited Create;
  FColor := Color;
  FHtmlColorPrefix := '#';
  UpdateAll;
end;

destructor TColorClass.Destroy;
begin
  inherited;
end;

function TColorClass.GetErrorStr(const sInvalidColorStr: string): string;
begin
  Result := 'Cannot convert "' + sInvalidColorStr + '" to color!';
end;


procedure TColorClass.SetColor(const Value: TColor);
begin
  FColor := Value;
  UpdateAll;
end;


{$region '                 UpdateAll                   '}
procedure TColorClass.UpdateAll;
var
  R, G, B: Byte;
  ck: TCMYKColor;
  pr, pg, pb: Single;
  Hue, Sat, Lum: integer;
begin
  GetRgbChannels(FColor, R, G, B);
  FRed := R;
  FGreen := G;
  FBlue := B;

  ck := ColorToCMYK(FColor);
  FCyan := Round(ck.C * 100);
  FMagenta := Round(ck.M * 100);
  FYellow := Round(ck.Y * 100);
  FBlack := Round(ck.K * 100);



  // RGB & BGR
  FRgbHex := ColorToHtmlColorStr(FColor, '');
  FRgbIntStr := ColorToRgbIntStr(FColor, 3, '0', ',');
  FRgbPercentIntStr := ColorToRgbPercentStr(FColor, 3, '0', ',');
  ColorToRgbPercent(FColor, pr, pg, pb);
  FRedPercentInt := Round(pr);
  FGreenPercentInt := Round(pg);
  FBluePercentInt := Round(pb);
  FBgrIntStr := ColorToBgrIntStr(FColor, 3, '0', ',');
  FBgrHex := ColorToBgrHexStr(FColor);

  // HTML
  FHtmlColorStr := FHtmlColorPrefix + FRgbHex;

  // HSL
  FHslCssStr := ColorToHslCssStr(Color, True, 0, ' ', ',', False); // ColorToHslRangeStr(FColor, 360, 100, 100, True, 0, ' ', ',');
  ColorToHslCss(FColor, Hue, Sat, Lum);
  FHueCss := Hue;
  FSatCss := Sat;
  FLumCss := Lum;
  FHslSysStr := ColorToHslStr(FColor, 239, 240, 240, 3, '0', ',');
  ColorToHslSys(FColor, FHue, FSat, FLum);

  FCmykStr := ColorToCmykStr(FColor, 0); // ColorToCmykStr(FColor, 3, '0', ',');


  FPascalHexStr := ColorToDelphiHex(FColor, '$');
  FPascalIntStr := ColorToDelphiIntStr(FColor);
  FCppHex := ColorToDelphiHex(FColor, '0x');




  FInvertedColor := InvertColor(FColor);

  FErrorStr := '';
  FValid := True;
end;
{$endregion UpdateAll}


procedure TColorClass.SetRGB(const Red, Green, Blue: Byte);
begin
  FColor := RGB(Red, Green, Blue);
  UpdateAll;
end;

procedure TColorClass.SetBGR(const Blue, Green, Red: Byte);
begin
  SetRGB(Red, Green, Blue);
end;

procedure TColorClass.SetRed(const Value: Byte);
begin
  FRed := Value;
  SetRGB(FRed, FGreen, FBlue);
end;

procedure TColorClass.SetRedPercentInt(const Value: TPercentInt);
begin
  FRedPercentInt := Value;
  FColor := RgbPercentToColor(FRedPercentInt, FGreenPercentInt, FBluePercentInt);
  UpdateAll;
end;

procedure TColorClass.SetGreenPercentInt(const Value: TPercentInt);
begin
  FGreenPercentInt := Value;
  FColor := RgbPercentToColor(FRedPercentInt, FGreenPercentInt, FBluePercentInt);
  UpdateAll;
end;

procedure TColorClass.SetBluePercentInt(const Value: TPercentInt);
begin
  FBluePercentInt := Value;
  FColor := RgbPercentToColor(FRedPercentInt, FGreenPercentInt, FBluePercentInt);
  UpdateAll;
end;

procedure TColorClass.SetGreen(const Value: Byte);
begin
  FGreen := Value;
  SetRGB(FRed, FGreen, FBlue);
end;


procedure TColorClass.SetBlue(const Value: Byte);
begin
  FBlue := Value;
  SetRGB(FRed, FGreen, FBlue);
end;


procedure TColorClass.SetCMYK(const Cyan, Magenta, Yellow, Black: integer);
begin
  FColor := CmykToColor(Cyan, Magenta, Yellow, Black);
  UpdateAll;
end;

procedure TColorClass.SetCyan(const Value: integer);
begin
  FCyan := Value;
  SetCMYK(FCyan, FMagenta, FYellow, FBlack);
end;

procedure TColorClass.SetMagenta(const Value: integer);
begin
  FMagenta := Value;
  SetCMYK(FCyan, FMagenta, FYellow, FBlack);
end;

procedure TColorClass.SetYellow(const Value: integer);
begin
  FYellow := Value;
  SetCMYK(FCyan, FMagenta, FYellow, FBlack);
end;

procedure TColorClass.SetBlack(const Value: integer);
begin
  FBlack := Value;
  SetCMYK(FCyan, FMagenta, FYellow, FBlack);
end;




procedure TColorClass.SetHslCssStr(const Value: string);
begin
  FValid := TryHslRangeToColor(Value, FColor, ',', 360, 100, 100);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetHslSysStr(const Value: string);
begin
  FValid := TryHslRangeToColor(Value, FColor, ',', 239, 240, 240);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetHtmlColorStr(const Value: string);
begin
  FValid := TryHtmlStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetHue(const Value: integer);
begin
  FHue := Value;
  FColor := HslSysToColor(FHue, FSat, FLum);
  UpdateAll;
end;

procedure TColorClass.SetSat(const Value: integer);
begin
  FSat := Value;
  FColor := HslSysToColor(FHue, FSat, FLum);
  UpdateAll;
end;

procedure TColorClass.SetLum(const Value: integer);
begin
  FLum := Value;
  FColor := HslSysToColor(FHue, FSat, FLum);
  UpdateAll;
end;



procedure TColorClass.SetHueCss(const Value: integer);
begin
  FHueCss := Value;
  FColor := HslCssToColor(FHueCss, FSatCss, FLumCss);
  UpdateAll;
end;


procedure TColorClass.SetSatCss(const Value: TPercentInt);
begin
  FSatCss := Value;
  FColor := HslCssToColor(FHueCss, FSatCss, FLumCss);
  UpdateAll;
end;


procedure TColorClass.SetLumCss(const Value: TPercentInt);
begin
  FLumCss := Value;
  FColor := HslCssToColor(FHueCss, FSatCss, FLumCss);
  UpdateAll;
end;

procedure TColorClass.SetHtmlColorPrefix(const Value: string);
begin
  FHtmlColorPrefix := Value;
end;


procedure TColorClass.SetPascalHexStr(const Value: string);
begin
  FValid := TryDelphiHexStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetPascalIntStr(const Value: string);
begin
  FValid := TryDelphiIntStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetRgbHex(const Value: string);
begin
  FValid := TryHtmlStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetRgbPercentIntStr(const Value: string);
begin
  FValid := TryRgbPercentStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetRgbIntStr(const Value: string);
begin
  FValid := TryRgbStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetBgrHex(const Value: string);
begin
  FValid := TryBgrHexToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetBgrIntStr(const Value: string);
begin
  FValid := TryBgrStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetCmykStr(const Value: string);
begin
  FValid := TryCmykStrToColor(Value, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;

procedure TColorClass.SetCppHex(const Value: string);
var
  s: string;
begin
  s := Value;
  s := StringReplace(s, '0x', '$', [rfIgnoreCase]);
  FValid := TryDelphiHexStrToColor(s, FColor);
  if not FValid then FErrorStr := GetErrorStr(Value) else UpdateAll;
end;




{$endregion TColorClass}

end.
