unit JPL.Colors;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

{
  I created this unit to have all the routines that convert colors in one place.
  The most of the code is mine, but some routines are copied from other free units.

  Jacek Pazera
  http://www.pazera-software.com
  ============================================================================

  Useful links:

  Color converter: https://www.w3schools.com/colors/colors_converter.asp
  HSL calculator: https://www.w3schools.com/colors/colors_hsl.asp
  CMYK calculator: https://www.w3schools.com/colors/colors_cmyk.asp

  RGB to HSL: http://www.delphigroups.info/2/fd/408721.html

  http://annystudio.com/software/colorpicker/ <-- A small (but useful) color picker created with Lazarus.
  A short explanation of the HSB, HSV, HSL, CMYK color models.
}

interface

uses
  {$IFDEF MSWINDOWS} Windows,{$ENDIF}
  {$IFDEF FPC} LCLType, {$ENDIF} // < TRGBTriple, TRBGQuad, MulDiv ...
  //Dialogs, Done : POTEM WYKOMENTOWAC
  Classes, SysUtils, Graphics, Math, JPL.Math, JPL.Strings, JPL.Conversion;

const
  HSL_MAX_CSS_HUE = 360;
  HSL_MAX_CSS_SAT = 100;
  HSL_MAX_CSS_LUM = 100;

  // Max values used in Windows system color dialog
  HSL_MAX_WIN_HUE = 239;
  HSL_MAX_WIN_SAT = 240;
  HSL_MAX_WIN_LUM = 240;

type
  TRGBAColor = DWORD;   // UInt32

  TColorType = (ctUnknown, ctPascalHex, ctPascalInt, ctHtml, ctRgb, ctRgbPercent, ctBgr, ctCmyk, ctHslCss, ctHslWin, ctCppHex);

  TRGBRec = record
    R, G, B: Byte;
  end;

  TCMYKColor = record
    C: Double;
    M: Double;
    Y: Double;
    K: Double;
  end;

  TRgbChannel = (rcRed, rcGreen, rcBlue);
  TCmykComponent = (ccCyan, ccMagenta, ccYellow, ccBlack);
  THslComponent = (hcHue, hcSat, hcLum);


var
  //Set these variables to your needs:
  //  360, 100, 100 - CSS
  //  239, 240, 240 - Windows system color dialog
  HSLMaxHue: integer = HSL_MAX_CSS_HUE;
  HSLMaxSaturation: integer = HSL_MAX_CSS_SAT;
  HSLMaxLightness: integer = HSL_MAX_CSS_LUM; // lightness (luminance, luminosity)


function RGB(const R, G, B: Byte): TColor;
function RGBA(const R, G, B, A: Byte): TRGBAColor;
function RGB3(const bt: Byte): TColor;
function HasAlpha(const Color: TRGBAColor): Boolean;
procedure GetRgbaChannels(const Color: TRGBAColor; out r, g, b, a: Byte);
procedure GetRgbChannels(const Color: TColor; out r, g, b: Byte); overload;
procedure GetRgbChannels(const Color: TColor; out r, g, b: integer); overload;
function GetRedChannel(const Color: TColor): Byte;
function GetGreenChannel(const Color: TColor): Byte;
function GetBlueChannel(const Color: TColor): Byte;
function RgbaToColor(const Color: TRGBAColor): TColor;
function ColorToRgbIntStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
function ColorToRgbHexStr(const Color: TColor; EmptyClNone: Boolean = True): string;
function TryRgbStrToColor(s: string; out Color: TColor): Boolean;
function RgbSum(const Color: TColor): Word;

function RGBtoRGBTriple(const r, g, b: Byte): TRGBTriple;
function RGBtoRGBQuad(const R, G, B: Byte): TRGBQuad; overload;
function RGBToRGBQuad(const c: TColor): TRGBQuad; overload;


function ColorToRgbPercentStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','; bShowPercent: Boolean = False): string;
procedure ColorToRgbPercent(const Color: TColor; out RedPercent, GreenPercent, BluePercent: Single); overload;
procedure ColorToRgbPercent(const Color: TColor; out RedPercent, GreenPercent, BluePercent: integer); overload;
function RgbPercentToColor(const RedPercent, GreenPercent, BluePercent: Single): TColor;

function TryRgbPercentStrToColor(s: string; out Color: TColor): Boolean;

function ColorToBgrIntStr(Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
function ColorToBgrHexStr(const Color: TColor; EmptyClNone: Boolean = True): string;
function TryBgrStrToColor(s: string; out Color: TColor): Boolean;
function TryBgrHexToColor(s: string; out Color: TColor): Boolean;


function ColorToCmykStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
function TryCmykStrToColor(s: string; var Color: TColor; Separator: string = ','): Boolean;
procedure GetCmykComponents(const AColor: TColor; out Cyan, Magenta, Yellow, Black: integer); overload;
procedure GetCmykComponents(const AColor: TColor; out Cyan, Magenta, Yellow, Black: Double); overload;
function GetCmykCyanValue(const AColor: TColor): integer;
function GetCmykMagentaValue(const AColor: TColor): integer;
function GetCmykYellowValue(const AColor: TColor): integer;
function GetCmykBlackValue(const AColor: TColor): integer;
// The following 4 CMYK functions are from https://github.com/hippy2094/codelib/tree/master/cymk-delphi-lazarus (with my small mods)
function ColorToCMYK(const Color: TColor): TCMYKColor;
function CmykToColor(const CmykColor: TCMYKColor): TColor; overload;
function CmykToColor(const C, M, Y, K: Double): TColor; overload;
function CMYKToString(c: TCMYKColor): String;


function TryHtmlStrToColor(s: string; out Color: TColor): Boolean; overload;
function TryHtmlStrToColor(s: string; out Color: TRGBAColor): Boolean; overload;

function ColorToHtmlColorStr(const Color: TColor; Prefix: string = '#'; EmptyClNone: Boolean = True): string; overload;
function ColorToHtmlColorStr(const Color: TRGBAColor; EmptyClNone: Boolean = True): string; overload;

function ColorToDelphiHex(Color: TColor; Prefix: string = '$'): string;
function ColorToDelphiIntStr(const Color: TColor): string;
function TryDelphiHexStrToColor(s: string; out Color: TColor): Boolean;
function TryDelphiIntStrToColor(s: string; out Color: TColor): Boolean;


procedure SetHslMaxValues(const MaxHue, MaxSat, MaxLum: integer);
procedure SetHslCssMaxValues;
procedure SetHslWinMaxValues;
// HSL related routines from RGBHSLUtils.pas (with my small modfications)
// mbColorLib 2.0.2 - http://mxs.bergsoft.net/index.php?p=2
function HslToColor(H, S, L: double): TColor;
function HSLRangeToRGB(H, S, L: integer): TColor;
procedure ColortoHSLRange(const RGB: TColor; out H1, S1, L1: integer);
function GetHValue(const AColor: TColor): integer;
function GetSValue(const AColor: TColor): integer;
function GetLValue(const AColor: TColor): integer;
procedure Clamp(var Input: integer; Min, Max: integer);
function HSLToRGBTriple(H, S, L: integer): TRGBTriple;
function HSLToRGBQuad(H, S, L: integer): TRGBQuad;
procedure RGBTripleToHSL(RGBTriple: TRGBTriple; out H, S, L: integer);

// my HSL routines
function GetHueCssValue(const AColor: TColor): integer;
function GetSatCssValue(const AColor: TColor): integer;
function GetLumCssValue(const AColor: TColor): integer;
function SetHueCssValue(const AColor: TColor; const NewHueValue: integer): TColor;
function SetSatCssValue(const AColor: TColor; const NewSatValue: integer): TColor;
function SetLumCssValue(const AColor: TColor; const NewLumValue: integer): TColor;
function SetHslCssDelta(const AColor: TColor; const DeltaHue, DeltaSat, DeltaLum: integer): TColor;

function GetHueWinValue(const AColor: TColor): integer;
function GetSatWinValue(const AColor: TColor): integer;
function GetLumWinValue(const AColor: TColor): integer;
function SetHueWinValue(const AColor: TColor; const NewHueValue: integer): TColor;
function SetSatWinValue(const AColor: TColor; const NewSatValue: integer): TColor;
function SetLumWinValue(const AColor: TColor; const NewLumValue: integer): TColor;
function SetHslWinDelta(const AColor: TColor; const DeltaHue, DeltaSat, DeltaLum: integer): TColor;

function TryHslCssStrToColor(HslCssStr: string; out cl: TColor): Boolean;
function TryHslWinStrToColor(HslWinStr: string; out cl: TColor): Boolean;
function ColorToHslStr(const Color: TColor; AMaxHue: integer = 239; AMaxSat: integer = 240; AMaxLum: integer = 240;
  Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
procedure ColorToHslSys(const Color: TColor; out Hue, Sat, Lum: integer);
function HslSysToColor(const Hue, Sat, Lum: integer): TColor;
function ColorToHslRangeStr(const Color: TColor; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100; bShowPercent: Boolean = True;
  Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
procedure ColorToHslCss(const Color: TColor; out H, S, L: integer);
function ColorToHslCssStr(const Color: TColor; UsePercent: Boolean = True; Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
function ColorToHslWinStr(const Color: TColor; UsePercent: Boolean = False; Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','): string;
function HslCssToColor(const Hue, Sat, Lum: Single): TColor;
function HslWinToColor(const Hue, Sat, Lum: Single): TColor;
function TryHslRangeToColor(s: string; var Color: TColor; Separator: string = ','; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100): Boolean;
function HslToHslCssStr(const Hue, Sat, Lum: integer; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100; bShowPercent: Boolean = True;
  Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','): string;

// HSL - color wheel - shifted colors
function HslCssShiftHue(const AColor: TColor; const ShiftValue: integer): TColor;
function ComplementaryColor(const AColor: TColor): TColor;
function TriadicColor1(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
function TriadicColor2(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
procedure GetTriadicColors(const AColor: TColor; out TriadicColor_1, TriadicColor_2: TColor; DistanceFromComplementaryColor: integer = 60);
function SquareColor1(const AColor: TColor): TColor;
function SquareColor2(const AColor: TColor): TColor;
function SquareColor3(const AColor: TColor): TColor;
procedure GetSquareColors(const AColor: TColor; out SqColor1, SqColor2, SqColor3: TColor);
function TetradicColor1(const AColor: TColor; DegDistance: integer = 90): TColor;
function TetradicColor2(const AColor: TColor): TColor; // = ComplementaryColor
function TetradicColor3(const AColor: TColor; DistanceFromComplementaryColor: integer = 90): TColor;
procedure GetTetradicColors(const AColor: TColor; out TetradColor1, TetradColor2, TetradColor3: TColor; DegDistance: integer = 90);


function RandomColor(bRandomize: Boolean = True): TColor;
function RandomRgbChannels(Color: TColor; bRandomRed, bRandomGreen, bRandomBlue: Boolean; bRandomize: Boolean = True): TColor;
function AvgColor(Color1, Color2: TColor): TColor;
function TryGetColor(s: string; out Color: TColor): Boolean;

function ColorToStrEx(const Color: TColor; ColorType: TColorType; Separator: string = ','): string;
{$IFDEF MSWINDOWS}
function PixelColor(const X, Y: integer): TColor;
{$ENDIF}

function InvertColor(const Color: TColor): TColor;
function InvertRgbChannels(const Color: TColor; bInvertRed, bInvertGreen, bInvertBlue: Boolean): TColor;
function InvertRedChannel(const Color: TColor): TColor;
function InvertGreenChannel(const Color: TColor): TColor;
function InvertBlueChannel(const Color: TColor): TColor;

function FadeToGray(Color: TColor): TColor; //<-- from dcrHexEditor - https://launchpad.net/dcr

function GetSimilarColor(cl: TColor; Percent: integer; Lighter: Boolean = True): TColor;
function GetSimilarColor2(Color: TColor; IncPercent: integer): TColor; // ColorSetPercentPale

// 3 x ColorSet.., ColorModify, MediumColor from Cindy - VCL.cyGraphics.pas (with my small mods) - https://sourceforge.net/projects/tcycomponents/
function ColorSetPercentPale(Color: TColor; const IncPercent: integer): TColor;
function ColorSetPercentContrast(Color: TColor; IncPercent: Integer): TColor;
function ColorSetPercentBrightness(Color: TColor; PercentLight: Integer): TColor;
function ColorModify(Color: TColor; incR, incG, incB: Integer): TColor;
function MediumColor(Color1, Color2: TColor): TColor;

function SetColorRedChannel(const Color: TColor; const RedValue: Byte): TColor;
function SetColorGreenChannel(const Color: TColor; const GreenValue: Byte): TColor;
function SetColorBlueChannel(const Color: TColor; const BlueValue: Byte): TColor;

function ModifyColorRedChannel(const Color: TColor; const DeltaRed: integer): TColor;
function ModifyColorGreenChannel(const Color: TColor; const DeltaGreen: integer): TColor;
function ModifyColorBlueChannel(const Color: TColor; const DeltaBlue: integer): TColor;
function ModifyColorRGBChannels(const Color: TColor; const DeltaRed, DeltaGreen, DeltaBlue: integer): TColor;
function RgbDelta(const Color: TColor; const DeltaRed, DeltaGreen, DeltaBlue: integer): TColor; overload;
function RgbDelta(const Color: TColor; const DeltaRGB: integer): TColor; overload;


function HiByte(w: Longint): Byte; overload;

function ColorTypeToStr(const ColorType: TColorType): string;
function StrToColorType(const ColorTypeStr: string): TColorType;

// Similar to AnsiCompareStr
function CompareRgbColors(const Color1, Color2: TColor; RgbChannel: TRgbChannel): integer;
function CompareRgbSumColors(const Color1, Color2: TColor): integer;
function CompareCmykColors(const Color1, Color2: TColor; CmykComponent: TCmykComponent): integer;
function CompareHslColors(const Color1, Color2: TColor; HslComponent: THslComponent): integer;


implementation


{$region ' --------------- Compare colors: RGB / CMYK / HSL -------------------- '}

function CompareRgbColors(const Color1, Color2: TColor; RgbChannel: TRgbChannel): integer;
var
  Value1, Value2: Byte;
begin
  case RgbChannel of

    rcRed:
      begin
        Value1 := GetRedChannel(Color1);
        Value2 := GetRedChannel(Color2);
      end;

    rcGreen:
      begin
        Value1 := GetGreenChannel(Color1);
        Value2 := GetGreenChannel(Color2);
      end;

  else
    // rcBlue
    begin
      Value1 := GetBlueChannel(Color1);
      Value2 := GetBlueChannel(Color2);
    end;

  end;

  if Value1 > Value2 then Result := 1
  else if Value1 < Value2 then Result := -1
  else Result := 0;

end;

function CompareRgbSumColors(const Color1, Color2: TColor): integer;
var
  Value1, Value2: integer;
begin
  Value1 := RgbSum(Color1);
  Value2 := RgbSum(Color2);
  if Value1 > Value2 then Result := 1
  else if Value1 < Value2 then Result := -1
  else Result := 0;
end;

function CompareCmykColors(const Color1, Color2: TColor; CmykComponent: TCmykComponent): integer;
var
  Value1, Value2: Double;
  cc1, cc2: TCMYKColor;
begin
  cc1 := ColorToCMYK(Color1);
  cc2 := ColorToCMYK(Color2);

  case CmykComponent of

    ccCyan:
      begin
        Value1 := cc1.C;
        Value2 := cc2.C;
      end;
    ccMagenta:
      begin
        Value1 := cc1.M;
        Value2 := cc2.M;
      end;
    ccYellow:
      begin
        Value1 := cc1.Y;
        Value2 := cc2.Y;
      end;
  else
    // ccBlack
    begin
      Value1 := cc1.K;
      Value2 := cc2.K;
    end;

  end;

  if Value1 > Value2 then Result := 1
  else if Value1 < Value2 then Result := -1
  else Result := 0;

end;

function CompareHslColors(const Color1, Color2: TColor; HslComponent: THslComponent): integer;
var
  Value1, Value2: integer;
  Hue1, Sat1, Lum1: integer;
  Hue2, Sat2, Lum2: integer;
begin
  ColorToHslSys(Color1, Hue1, Sat1, Lum1);
  ColorToHslSys(Color2, Hue2, Sat2, Lum2);

  case HslComponent of

    hcHue:
      begin
        Value1 := Hue1;
        Value2 := Hue2;
      end;
    hcSat:
      begin
        Value1 := Sat1;
        Value2 := Sat2;
      end;
  else
    // hcLum
    begin
      Value1 := Lum1;
      Value2 := Lum2;
    end;

  end;

  if Value1 > Value2 then Result := 1
  else if Value1 < Value2 then Result := -1
  else Result := 0;

end;
{$endregion Compare colors: RGB / CMYK / HSL}


function HiByte(w: Longint): Byte;
begin
  Result := Byte(((Word(w)) shr 8) and $FF);
end;

function ColorTypeToStr(const ColorType: TColorType): string;
begin
  case ColorType of
    ctPascalHex: Result := 'Pascal Hex';
    ctPascalInt: Result := 'Pascal Int';
    ctHtml: Result := 'HTML';
    ctRgb: Result := 'RGB';
    ctRgbPercent: Result := 'RGB %';
    ctBgr: Result := 'BGR';
    ctCmyk: Result := 'CMYK';
    ctHslCss: Result := 'HSL CSS';
    ctHslWin: Result := 'HSL WIN';
    ctCppHex: Result := 'C++ Hex';
  else
    Result := '';
  end;
end;

function StrToColorType(const ColorTypeStr: string): TColorType;
var
  s: string;
begin
  s := UpperCase(ColorTypeStr);
  s := RemoveAll(s, ' ');
  if s = 'PASCALHEX' then Result := ctPascalHex
  else if s = 'PASCALINT' then Result := ctPascalInt
  else if s = 'HTML' then Result := ctHtml
  else if s = 'RGB' then Result := ctRgb
  else if s = 'RGB%' then Result := ctRgbPercent
  else if s = 'BGR' then Result := ctBgr
  else if s = 'CMYK' then Result := ctCmyk
  else if s = 'HSLCSS' then Result := ctHslCss
  else if s = 'HSLWIN' then Result := ctHslWin
  else if s = 'C++HEX' then Result := ctCppHex
  else Result := ctUnknown;
end;




{$region ' From Cindy - VCL.cyGraphics.pas (with my small mods) '}
function ColorModify(Color: TColor; incR, incG, incB: Integer): TColor;
var
  r, g, b: Integer;
begin
  Color := ColorToRGB(Color);

//  r:= GetRValue(Color);
//  g:= GetGValue(Color);
//  b:= GetBValue(Color);
  GetRgbChannels(Color, r, g, b);

  r := r + incR;
  g := g + incG;
  b := b + incB;

//  if r < 0 then r := 0; if r > 255 then r := 255;
//  if g < 0 then g := 0; if g > 255 then g := 255;
//  if b < 0 then b := 0; if b > 255 then b := 255;
  r := GetIntInRange(r, 0, 255);
  g := GetIntInRange(g, 0, 255);
  b := GetIntInRange(b, 0, 255);

  Result := RGB(r,g,b);
end;

function MediumColor(Color1, Color2: TColor): TColor;
var
  r,g,b: Integer;
  Rec1, Rec2: TRGBRec;
begin
  if Color1 <> Color2 then
  begin
    Color1 := ColorToRGB(Color1);
    Color2 := ColorToRGB(Color2);

    GetRgbChannels(Color1, Rec1.R, Rec1.G, Rec1.B);
    GetRgbChannels(Color2, Rec2.R, Rec2.G, Rec2.B);
    r := (Rec1.R + Rec2.R) div 2;
    g := (Rec1.G + Rec2.G) div 2;
    b := (Rec1.B + Rec2.B) div 2;
    //r := ( GetRValue(Color1) + GetRValue(Color2) ) div 2;
    //g := ( GetGValue(Color1) + GetGValue(Color2) ) div 2;
    //b := ( GetBValue(Color1) + GetBValue(Color2) ) div 2;
//    RESULT := TColor( RGB(r, g, b) );
    RESULT := RGB(r, g, b);
  end
  else
    RESULT := Color1;
end;

function ColorSetPercentPale(Color: TColor; const IncPercent: integer): TColor;
var
  r, g, b: Integer;
begin
  Color := ColorToRGB(Color);
//  r:= GetRValue(Color);
//  g:= GetGValue(Color);
//  b:= GetBValue(Color);
  GetRgbChannels(Color, r, g, b);

  r := r + Round((255 - r) * IncPercent / 100);
  g := g + Round((255 - g) * IncPercent / 100);
  b := b + Round((255 - b) * IncPercent / 100);

//  if r < 0 then r := 0; if r > 255 then r := 255;
//  if g < 0 then g := 0; if g > 255 then g := 255;
//  if b < 0 then b := 0; if b > 255 then b := 255;
  r := GetIntInRange(r, 0, 255);
  g := GetIntInRange(g, 0, 255);
  b := GetIntInRange(b, 0, 255);

  RESULT := RGB(r,g,b);
end;

function ColorSetPercentContrast(Color: TColor; IncPercent: Integer): TColor;
var
  r, g, b, Media: Integer;
begin
  if IncPercent > 100 then IncPercent := 100;
  if IncPercent < -100 then IncPercent := -100;

  Color:= ColorToRGB(Color);

//  r:= GetRValue(Color);
//  g:= GetGValue(Color);
//  b:= GetBValue(Color);
  GetRgbChannels(Color, r, g, b);

  Media := (r+g+b) Div 3;

  r := r + Round(  (r - Media) * (IncPercent / 100)  );
  g := g + Round(  (g - Media) * (IncPercent / 100)  );
  b := b + Round(  (b - Media) * (IncPercent / 100)  );

//  if r < 0 then r := 0; if r > 255 then r := 255;
//  if g < 0 then g := 0; if g > 255 then g := 255;
//  if b < 0 then b := 0; if b > 255 then b := 255;
  r := GetIntInRange(r, 0, 255);
  g := GetIntInRange(g, 0, 255);
  b := GetIntInRange(b, 0, 255);

  RESULT := RGB(r,g,b);
end;

function ColorSetPercentBrightness(Color: TColor; PercentLight: Integer): TColor;
var
  r, g, b, incValue: Integer;
begin
  incValue := MulDiv(255, PercentLight, 100);
  Color:= ColorToRGB(Color);

//  r := GetRValue(Color);
//  g := GetGValue(Color);
//  b := GetBValue(Color);
  GetRgbChannels(Color, r, g, b);

  r := r + incValue;
  g := g + incValue;
  b := b + incValue;

  // 2015-07-17 Distribute light :
  if r > 255 then
  begin
    g := g + (r - 255) div 2;
    b := b + (r - 255) div 2;
    r := 255;
  end;

  if g > 255 then
  begin
    r := r + (g - 255) div 2;
    b := b + (g - 255) div 2;
    g := 255;
  end;

  if b > 255 then
  begin
    r := r + (b - 255) div 2;
    g := g + (b - 255) div 2;
    b := 255;
  end;

  // 2015-07-17 Distribute dark :
  if r < 0 then
  begin
    g := g + r div 2;
    b := b + r div 2;
    r := 0;
  end;

  if g < 0 then
  begin
    r := r + g div 2;
    b := b + g div 2;
    g := 0;
  end;

  if b < 0 then
  begin
    r := r + b div 2;
    g := g + b div 2;
    b := 0;
  end;

//  if r < 0 then r := 0; if r > 255 then r := 255;
//  if g < 0 then g := 0; if g > 255 then g := 255;
//  if b < 0 then b := 0; if b > 255 then b := 255;

  r := GetIntInRange(r, 0, 255);
  g := GetIntInRange(g, 0, 255);
  b := GetIntInRange(b, 0, 255);

  RESULT := RGB(r,g,b);
end;

{$endregion Cindy}


function GetSimilarColor(cl: TColor; Percent: integer; Lighter: Boolean = True): TColor;
var
  r, g, b: integer;
  x: integer;
begin
  cl := ColorToRgb(cl);
  GetRgbChannels(cl, r, g ,b);

  x := r * Percent div 100;
  if Lighter then r := r + x
  else r := r - x;
  r := GetIntInRange(r, 0, 255);

  x := g * Percent div 100;
  if Lighter then g := g + x
  else g := g - x;
  g := GetIntInRange(g, 0, 255);

  x := b * Percent div 100;
  if Lighter then b := b + x
  else b := b - x;
  b := GetIntInRange(b, 0, 255);

  Result := RGB(r, g, b);
end;

function GetSimilarColor2(Color: TColor; IncPercent: integer): TColor;
begin
  Result := ColorSetPercentPale(Color, IncPercent);
end;


function FadeToGray(Color: TColor): TColor;
var
  LBytGray, r, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  GetRgbChannels(Color, r, g, b);
  LBytGray := HiByte(r * 74 {%H-}+ g * 146 {%H-}+ b * 36);
  Result := RGB(LBytGray, LBytGray, LBytGray);
end;

function InvertColor(const Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $00FFFFFF;
end;



function AvgColor(Color1, Color2: TColor): TColor;
var
  r1, r2: TRGBRec;
begin
  if Color1 = Color2 then Exit(Color1);

  Move(Color1, r1{%H-}, 3);
  Move(Color2, r2{%H-}, 3);

  Result := RGB(
    (r1.R + r2.R) div 2,
    (r1.G + r2.G) div 2,
    (r1.B + r2.B) div 2
  );
end;

function RandomColor(bRandomize: Boolean = True): TColor;
var
  R, G, B: Byte;
begin
  if bRandomize then Randomize;
  R := Random(256);
  G := Random(256);
  B := Random(256);
  Result := RGB(R, G, B);
end;

function RandomRgbChannels(Color: TColor; bRandomRed, bRandomGreen, bRandomBlue: Boolean; bRandomize: Boolean = True): TColor;
var
  R, G, B: Byte;
begin
  if bRandomize then Randomize;
  if bRandomRed then R := Random(256) else R := GetRedChannel(Color);
  if bRandomGreen then G := Random(256) else G := GetGreenChannel(Color);
  if bRandomBlue then B := Random(256) else B := GetBlueChannel(Color);
  Result := RGB(R, G, B);
end;

{$IFDEF MSWINDOWS}
function PixelColor(const X, Y: integer): TColor;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetPixel(DC, X, Y);
  ReleaseDC(0, DC);
end;
{$ENDIF}


function ColorToStrEx(const Color: TColor; ColorType: TColorType; Separator: string = ','): string;
begin
  case ColorType of
    ctHtml: Result := ColorToHtmlColorStr(Color, '#');
    ctRgb: Result := ColorToRgbIntStr(Color, 3, '0', Separator);
    ctRgbPercent: Result := ColorToRgbPercentStr(Color, 1, '0', Separator, True);
    ctBgr: Result := ColorToBgrIntStr(Color);
    ctPascalInt: Result := ColorToDelphiIntStr(Color);
    ctPascalHex: Result := ColorToDelphiHex(Color, '$');
    ctCmyk: Result := ColorToCmykStr(Color);
    ctHslCss: Result := ColorToHslCssStr(Color, True, 0, ' ', ',', False);
    ctHslWin: Result := ColorToHslStr(Color, 239, 240, 240, 3, '0', Separator);
    ctCppHex: Result := ColorToDelphiHex(Color, '0x');
  else
    Result := '';
  end;
end;


function TryGetColor(s: string; out Color: TColor): Boolean;
var
  ColorType: TColorType;
  FirstChar: Char;

  function ContainsHexChars(sh: string): Boolean;
  begin
    sh := UpperCase(sh);
    Result :=
      (Pos('A', sh) > 0) or (Pos('B', sh) > 0) or (Pos('C', sh) > 0) or
      (Pos('D', sh) > 0) or (Pos('E', sh) > 0) or (Pos('F', sh) > 0);
  end;
begin
  Result := False;

  s := Trim(s);
  if s = '' then Exit;
  FirstChar := s[1];

  if FirstChar = '$' then ColorType := ctPascalHex
  else if FirstChar = '#' then ColorType := ctHtml
  else
  begin
    //if IsValidHexStr(s, True) then ColorType := ctHtml
    if ContainsHexChars(s) then ColorType := ctPascalHex // ctHtml
    else
    begin
      if (Pos(',', s) > 0) or (Pos(' ', s) > 0) then ColorType := ctRgb
      else if IsValidIntStr(s, True) then ColorType := ctPascalHex // ctDelphiInt
      else ColorType := ctUnknown;
    end;
  end;
              //Msg(Integer(ColorType).toString);
  case ColorType of
    ctPascalHex: Result := TryDelphiHexStrToColor(s, Color);
    ctPascalInt: Result := TryDelphiIntStrToColor(s, Color);
    ctHtml: Result := TryHtmlStrToColor(s, Color);
    ctRgb: Result := TryRgbStrToColor(s, Color);
  else
    Result := False;
  end;


end;


{$region '                                 CMYK colors                                      '}
function ColorToCMYK(const Color: TColor): TCMYKColor;
var
  r, g, b: Byte;
  c, m, y ,k: Double;
begin
  GetRgbChannels(Color, r, g, b);

  c := 1 - (r / 255);
  m := 1 - (g / 255);
  y := 1 - (b / 255);

  k := Min(c, Min(m, y));

  if Color = 0 then
  begin
    Result.C := 0.00;
    Result.M := 0.00;
    Result.Y := 0.00;
    Result.K := 1.00;
  end
  else
  begin
    Result.C := (c - k) / (1 - k);
    Result.M := (m - k) / (1 - k);
    Result.Y := (y - k) / (1 - k);
    Result.K := k;
  end;
end;

function ColorToCmykStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(Color);

  Result :=
    Pad(IntToStr(Round(ck.C * 100)), Padding, PaddingChar) + Separator +
    Pad(IntToStr(Round(ck.M * 100)), Padding, PaddingChar) + Separator +
    Pad(IntToStr(Round(ck.Y * 100)), Padding, PaddingChar) + Separator +
    Pad(IntToStr(Round(ck.K * 100)), Padding, PaddingChar);
end;


function TryCmykStrToColor(s: string; var Color: TColor; Separator: string = ','): Boolean;
var
  ck: TCMYKColor;
  Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>;
  sC, sM, sY, sK: string;
begin
  Result := False;
  s := RemoveAll(s, ' ', True);
  s := RemoveAll(s, '%');
  s := RemoveAll(s, 'cmyk', True);
  s := RemoveAll(s, '(');
  s := RemoveAll(s, ')');
  SplitStrToArray(s, Arr{%H-}, Separator);
  if Length(Arr) <> 4 then Exit;

  sC := Arr[0];
  sM := Arr[1];
  sY := Arr[2];
  sK := Arr[3];

  if not TryStrToFloat(sC, ck.C) then Exit;
  if not TryStrToFloat(sM, ck.M) then Exit;
  if not TryStrToFloat(sY, ck.Y) then Exit;
  if not TryStrToFloat(sK, ck.K) then Exit;

  ck.C := ck.C / 100;
  ck.M := ck.M / 100;
  ck.Y := ck.Y / 100;
  ck.K := ck.K / 100;

  Color := CmykToColor(ck);
  Result := True;
end;


function CmykToColor(const CmykColor: TCMYKColor): TColor;
begin
  Result := CmykToColor(CmykColor.C, CmykColor.M, CmykColor.Y, CmykColor.K);
end;

function CmykToColor(const C, M, Y, K: Double): TColor;
var
  r, g, b: Byte;
begin
  r := Round(255 * (1 - C) * (1 - K));
  g := Round(255 * (1 - M) * (1 - K));
  b := Round(255 * (1 - Y) * (1 - K));
  Result := RGB(r, g, b);
end;

function CMYKToString(c: TCMYKColor): String;
begin
  Result := FloatToStrF(c.C, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(c.M, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(c.Y, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(c.K, ffGeneral, 3, 3);
end;

procedure GetCmykComponents(const AColor: TColor; out Cyan, Magenta, Yellow, Black: integer);
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Cyan := Round(ck.C * 100);
  Magenta := Round(ck.M * 100);
  Yellow := Round(ck.Y * 100);
  Black := Round(ck.K * 100);
end;

procedure GetCmykComponents(const AColor: TColor; out Cyan, Magenta, Yellow, Black: Double);
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Cyan := ck.C * 100;
  Magenta := ck.M * 100;
  Yellow := ck.Y * 100;
  Black := ck.K * 100;
end;

function GetCmykCyanValue(const AColor: TColor): integer;
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Result := Round(ck.C * 100);
end;

function GetCmykMagentaValue(const AColor: TColor): integer;
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Result := Round(ck.M * 100);
end;

function GetCmykYellowValue(const AColor: TColor): integer;
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Result := Round(ck.Y * 100);
end;

function GetCmykBlackValue(const AColor: TColor): integer;
var
  ck: TCMYKColor;
begin
  ck := ColorToCMYK(AColor);
  Result := Round(ck.K * 100);
end;
{$endregion CMYK colors}


{$region '                              BGR colors                             '}

function ColorToBgrIntStr(Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  Result :=
    Pad(IntToStr(b), Padding, PaddingChar) + Separator +
    Pad(IntToStr(g), Padding, PaddingChar) + Separator +
    Pad(IntToStr(r), Padding, PaddingChar);
end;

function ColorToBgrHexStr(const Color: TColor; EmptyClNone: Boolean = True): string;
var
  r, g, b: Byte;
begin
  if EmptyClNone and (Color = clNone) then Exit('');

  GetRgbChannels(Color, r, g, b);
  Result := IntToHex(b, 2) + IntToHex(g, 2) + IntToHex(r, 2);
end;

function TryBgrStrToColor(s: string; out Color: TColor): Boolean;
var
  s2: string;
  r, g, b: Byte;
  xp: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;
  s := StringReplace(s, '   ', ' ', [rfReplaceAll]);
  s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  s := StringReplace(s, ' ', ',', [rfReplaceAll]);

  xp := Pos(',', s);
  if xp > 0 then
  begin
    s2 := Copy(s, 1, xp - 1);
    if not TryStrToByte(s2, b) then Exit;
    s := Copy(s, xp + 1, Length(s));

    xp := Pos(',', s);
    if xp > 0 then
    begin
      s2 := Copy(s, 1, xp - 1);
      if not TryStrToByte(s2, g) then Exit;

      s := Copy(s, xp + 1, Length(s));
      if not TryStrToByte(s, r) then Exit;

      Color := RGB(r,g,b);
      Result := True;
    end;
  end;

end;


function TryBgrHexToColor(s: string; out Color: TColor): Boolean;
var
  sr, sg, sb: string;
  r, g, b: Byte;
  c1, c2, c3: Char;
  xLen: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;

  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '#', '', [rfReplaceAll]);

  xLen := Length(s);
  // short BGR notation (#BGR)
  // short to long conversion
  if xLen = 3 then
  begin
    c1 := s[1];
    c2 := s[2];
    c3 := s[3];
    s := c1 + c1 + c2 + c2 + c3 + c3;
  end;

  xLen := Length(s);
  // long BGR notation (#BBGGRR)
  if xLen = 6 then
  begin
    sb := Copy(s, 1, 2);
    if not TryHexToByte(sb, b) then Exit;

    sg := Copy(s, 3, 2);
    if not TryHexToByte(sg, g) then Exit;

    sr := Copy(s, 5, 2);
    if not TryHexToByte(sr, r) then Exit;

    Color := RGB(r, g, b);
    Result := True;
  end;

end;

{$endregion BGR colors}


{$region '                        RGB colors [%]                                '}

function RgbPercentToColor(const RedPercent, GreenPercent, BluePercent: Single): TColor;
var
  pr, pg, pb: Single;
begin
  pr := GetFloatInRange(RedPercent, 0, 100);
  pg := GetFloatInRange(GreenPercent, 0, 100);
  pb := GetFloatInRange(BluePercent, 0, 100);

  pr := PercentOf(pr, 255);
  pg := PercentOf(pg, 255);
  pb := PercentOf(pb, 255);

  Result := RGB(Round(pr), Round(pg), Round(pb));
end;

procedure ColorToRgbPercent(const Color: TColor; out RedPercent, GreenPercent, BluePercent: Single);
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  RedPercent := PercentValue(r, 255);
  GreenPercent := PercentValue(g, 255);
  BluePercent := PercentValue(b, 255);
end;

procedure ColorToRgbPercent(const Color: TColor; out RedPercent, GreenPercent, BluePercent: integer);
var
  pr, pg, pb: Single;
begin
  ColorToRgbPercent(Color, pr, pg, pb);
  RedPercent := Round(pr);
  GreenPercent := Round(pg);
  BluePercent := Round(pb);
end;

function ColorToRgbPercentStr(const Color: TColor; Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','; bShowPercent: Boolean = False): string;
var
  //r, g, b: Byte;
  pr, pg, pb: Single;
  sp: string;
begin
//  GetRgbChannels(Color, r, g, b);
//
//  pr := PercentValue(r, 255);
//  pg := PercentValue(g, 255);
//  pb := PercentValue(b, 255);
  if bShowPercent then sp := '%' else sp := '';
  ColorToRgbPercent(Color, pr, pg, pb);
  Result :=
    Pad(IntToStr(Round(pr)), Padding, PaddingChar) + sp + Separator +
    Pad(IntToStr(Round(pg)), Padding, PaddingChar) + sp + Separator +
    Pad(IntToStr(Round(pb)), Padding, PaddingChar) + sp;
end;

function TryRgbPercentStrToColor(s: string; out Color: TColor): Boolean;
var
  s2: string;
  r, g, b: Byte;
  xp: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;
  s := StringReplace(s, '   ', ' ', [rfReplaceAll]);
  s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  s := StringReplace(s, ' ', ',', [rfReplaceAll]);

  xp := Pos(',', s);
  if xp > 0 then
  begin
    s2 := Copy(s, 1, xp - 1);
    if not TryStrToByte(s2, r) then Exit;
    s := Copy(s, xp + 1, Length(s));

    xp := Pos(',', s);
    if xp > 0 then
    begin
      s2 := Copy(s, 1, xp - 1);
      if not TryStrToByte(s2, g) then Exit;

      s := Copy(s, xp + 1, Length(s));
      if not TryStrToByte(s, b) then Exit;

      r := PercentOf(r, 255);
      g := PercentOf(g, 255);
      b := PercentOf(b, 255);

      Color := RGB(r,g,b);
      Result := True;
    end;
  end;

end;


{$endregion RGB colors [%]}


{$region '                     RGB colors                           '}

function RGB3(const bt: Byte): TColor;
begin
  Result := RGB(bt, bt, bt);
end;

function RgbSum(const Color: TColor): Word;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  Result := r + g + b;
end;

function RGBtoRGBQuad(const R, G, B: byte): TRGBQuad; overload;
begin
  Result.rgbBlue := B;
  Result.rgbGreen := G;
  Result.rgbRed := R;
  Result.rgbReserved := 0;
end;

function RGBToRGBQuad(const c: TColor): TRGBQuad; overload;
var
  R, G, B: Byte;
begin
  GetRgbChannels(c, R, G, B);
  Result.rgbBlue := B;
  Result.rgbGreen := G;
  Result.rgbRed := R;
  Result.rgbReserved := 0;
end;

function RGBtoRGBTriple(const r, g, b: Byte): TRGBTriple;
begin
  Result.rgbtBlue := b;
  Result.rgbtGreen := g;
  Result.rgbtRed := r;
end;

function RGB(const R, G, B: Byte): TColor;
begin
  Result := (R or (G shl 8) or (B shl 16));
  // Result := Windows.RGB(R, G, B);
end;

procedure GetRgbChannels(const Color: TColor; out r, g, b: Byte);
begin
  {$IFDEF MSWINDOWS}
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  {$ELSE}
  r := Byte(Color);
  g := Byte(Color shr 8);
  b := Byte(Color shr 16);
  {$ENDIF}
end;

procedure GetRgbChannels(const Color: TColor; out r, g, b: integer);
begin
  r := Byte(Color);
  g := Byte(Color shr 8);
  b := Byte(Color shr 16);
end;

function GetRedChannel(const Color: TColor): Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetRValue(Color);
  {$ELSE}
  Result := Byte(Color);
  {$ENDIF}
end;

function GetGreenChannel(const Color: TColor): Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetGValue(Color);
  {$ELSE}
  Result := Byte(Color shr 8);
  {$ENDIF}
end;

function GetBlueChannel(const Color: TColor): Byte;
begin
  {$IFDEF MSWINDOWS}
  Result := GetBValue(Color);
  {$ELSE}
  Result := Byte(Color shr 16);
  {$ENDIF}
end;

function TryRgbStrToColor(s: string; out Color: TColor): Boolean;
var
  s2: string;
  r, g, b: Byte;
  xp: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;
  s := StringReplace(s, '   ', ' ', [rfReplaceAll]);
  s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  s := StringReplace(s, ' ', ',', [rfReplaceAll]);
  s := StringReplace(s, ',,', ',', [rfReplaceAll]);
  s := RemoveAll(s, 'rgb', True);
  s := RemoveAll(s, '(');
  s := RemoveAll(s, ')');

  xp := Pos(',', s);
  if xp > 0 then
  begin
    s2 := Copy(s, 1, xp - 1);
    if not TryStrToByte(s2, r) then Exit;
    s := Copy(s, xp + 1, Length(s));

    xp := Pos(',', s);
    if xp > 0 then
    begin
      s2 := Copy(s, 1, xp - 1);
      if not TryStrToByte(s2, g) then Exit;

      s := Copy(s, xp + 1, Length(s));
      if not TryStrToByte(s, b) then Exit;

      Color := RGB(r,g,b);
      Result := True;
    end;
  end
  else
    if TryStrToByte(s, r) then
    begin
      Color := RGB3(r);
      Result := True;
      Exit;
    end;

end;

function ColorToRgbIntStr(const Color: TColor; Padding: Byte; PaddingChar: Char; Separator: string): string;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  Result :=
    Pad(IntToStr(r), Padding, PaddingChar) + Separator +
    Pad(IntToStr(g), Padding, PaddingChar) + Separator +
    Pad(IntToStr(b), Padding, PaddingChar);
end;

function ColorToRgbHexStr(const Color: TColor; EmptyClNone: Boolean = True): string;
var
  r, g, b: Byte;
begin
  if EmptyClNone and (Color = clNone) then Exit('');

  GetRgbChannels(Color, r, g, b);
  Result := IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2);
end;

function ModifyColorRedChannel(const Color: TColor; const DeltaRed: integer): TColor;
var
  r, g, b: integer;
begin
  if DeltaRed = 0 then Exit(Color);
  GetRgbChannels(Color, r, g, b);
  r := GetIntInRange(r + DeltaRed, 0, 255);
  Result := RGB(r, g, b);
end;

function ModifyColorGreenChannel(const Color: TColor; const DeltaGreen: integer): TColor;
var
  r, g, b: integer;
begin
  if DeltaGreen = 0 then Exit(Color);
  GetRgbChannels(Color, r, g, b);
  g := GetIntInRange(g + DeltaGreen, 0, 255);
  Result := RGB(r, g, b);
end;

function ModifyColorBlueChannel(const Color: TColor; const DeltaBlue: integer): TColor;
var
  r, g, b: integer;
begin
  if DeltaBlue = 0 then Exit(Color);
  GetRgbChannels(Color, r, g, b);
  b := GetIntInRange(b + DeltaBlue, 0, 255);
  Result := RGB(r, g, b);
end;

function ModifyColorRGBChannels(const Color: TColor; const DeltaRed, DeltaGreen, DeltaBlue: integer): TColor;
var
  r, g, b: integer;
begin
  if (DeltaRed = 0) and (DeltaGreen = 0) and (DeltaBlue = 0) then Exit(Color);
  GetRgbChannels(Color, r, g, b);
  if DeltaRed <> 0 then r := GetIntInRange(r + DeltaRed, 0, 255);
  if DeltaGreen <> 0 then g := GetIntInRange(g + DeltaGreen, 0, 255);
  if DeltaBlue <> 0 then b := GetIntInRange(b + DeltaBlue, 0, 255);
  Result := RGB(r, g, b);
end;

function RgbDelta(const Color: TColor; const DeltaRed, DeltaGreen, DeltaBlue: integer): TColor;
begin
  Result := ModifyColorRGBChannels(Color, DeltaRed, DeltaGreen, DeltaBlue);
end;

function RgbDelta(const Color: TColor; const DeltaRGB: integer): TColor;
begin
  Result := ModifyColorRGBChannels(Color, DeltaRGB, DeltaRGB, DeltaRGB);
end;


function SetColorRedChannel(const Color: TColor; const RedValue: Byte): TColor;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  r := GetIntInRange(RedValue, 0, 255);
  Result := RGB(r, g, b);
end;

function SetColorGreenChannel(const Color: TColor; const GreenValue: Byte): TColor;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  g := GetIntInRange(GreenValue, 0, 255);
  Result := RGB(r, g, b);
end;

function SetColorBlueChannel(const Color: TColor; const BlueValue: Byte): TColor;
var
  r, g, b: Byte;
begin
  GetRgbChannels(Color, r, g, b);
  b := GetIntInRange(BlueValue, 0, 255);
  Result := RGB(r, g, b);
end;

function InvertRgbChannels(const Color: TColor; bInvertRed, bInvertGreen, bInvertBlue: Boolean): TColor;
var
  cl: TColor;
begin
  cl := ColorToRGB(Color);
  if bInvertRed then cl := cl xor $000000FF;
  if bInvertGreen then cl := cl xor $0000FF00;
  if bInvertBlue then cl := cl xor $00FF0000;
  Result := cl;
end;

function InvertRedChannel(const Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $000000FF;
end;

function InvertGreenChannel(const Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $0000FF00;
end;

function InvertBlueChannel(const Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $00FF0000;
end;

{$endregion RGB colors}


{$region '                   RGBA colors                         '}
function RGBA(const R, G, B, A: Byte): TRGBAColor;
begin
  Result := (R or (G shl 8) or (B shl 16) or (A shl 24));
end;

function HasAlpha(const Color: TRGBAColor): Boolean;
begin
  Result := Byte(Color shr 24) > 0;
end;

function RgbaToColor(const Color: TRGBAColor): TColor;
var
  a, r, g, b: Byte;
begin
  GetRgbaChannels(Color, r, g, b, a);
  Result := RGB(r, g, b);
end;

procedure GetRgbaChannels(const Color: TRGBAColor; out r, g, b, a: Byte);
begin
  r := Byte(Color);
  g := Byte(Color shr 8);
  b := Byte(Color shr 16);
  a := Byte(Color shr 24);
end;
{$endregion RGBA colors}


{$region '                        Delphi colors                               '}
function ColorToDelphiHex(Color: TColor; Prefix: string = '$'): string;
begin
  Result := Prefix + '00' + ColorToBgrHexStr(Color, False);
end;

function ColorToDelphiIntStr(const Color: TColor): string;
begin
  Result := IntToStr(Color);
end;

function TryDelphiHexStrToColor(s: string; out Color: TColor): Boolean;
var
  x: integer;
begin
  Result := False;
  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '0x', '', [rfIgnoreCase]);
  if Copy(s, 1, 1) <> '$' then s := '$' + s;
  if not TryStrToInt(s, x) then Exit;
  Color := TColor(x);
  Result := True;
end;

function TryDelphiIntStrToColor(s: string; out Color: TColor): Boolean;
var
  x: integer;
begin
  if not TryStrToInt(s, x) then Exit(False);
  Color := TColor(x);
  Result := True;
end;

{$endregion Delphi colors}


{$region '                     HTML HEX colors                            '}
function ColorToHtmlColorStr(const Color: TColor; Prefix: string = '#'; EmptyClNone: Boolean = True): string; overload;
begin
  Result := Prefix + ColorToRgbHexStr(Color, EmptyClNone);
end;

function ColorToHtmlColorStr(const Color: TRGBAColor; EmptyClNone: Boolean = True): string; overload;
var
  r, g, b, a: Byte;
begin
  if EmptyClNone and (Color = UInt32(clNone)) then Exit('');

  GetRgbaChannels(Color, r, g, b, a);
  Result := '#' + IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2);
  if a > 0 then Result := Result + IntToHex(a, 2);
end;

function TryHtmlStrToColor(s: string; out Color: TColor): Boolean; overload;
var
  sr, sg, sb: string;
  r, g, b: Byte;
  c1, c2, c3: Char;
  xLen: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;

  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '#', '', [rfReplaceAll]);

  xLen := Length(s);
  // short RGB notation (#RGB)
  // short to long conversion
  if xLen = 3 then
  begin
    c1 := s[1];
    c2 := s[2];
    c3 := s[3];
    s := c1 + c1 + c2 + c2 + c3 + c3;
  end;

  xLen := Length(s);
  // long RGB notation (#RRGGBB)
  if xLen = 6 then
  begin
    sr := Copy(s, 1, 2);
    if not TryHexToByte(sr, r) then Exit;

    sg := Copy(s, 3, 2);
    if not TryHexToByte(sg, g) then Exit;

    sb := Copy(s, 5, 2);
    if not TryHexToByte(sb, b) then Exit;

    Color := RGB(r, g, b);
    Result := True;
  end;

end;

function TryHtmlStrToColor(s: string; out Color: TRGBAColor): Boolean; overload;
var
  sr, sg, sb, sa: string;
  r, g, b, a: Byte;
  c1, c2, c3, c4: Char;
  xLen: integer;
begin
  r := 0; g := 0; b := 0; a := 0;
  Result := False;

  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '#', '', [rfReplaceAll]);

  xLen := Length(s);
  // short RGB or RGBA notation (#RGB, #RGBA)
  // short to long conversion
  if xLen in [3..4] then
  begin
    c1 := s[1];
    c2 := s[2];
    c3 := s[3];
    if xLen = 4 then c4 := s[4] else c4 := ' ';
    s := c1 + c1 + c2 + c2 + c3 + c3;

    if xLen = 4 then s := s + c4 + c4;

  end;

  xLen := Length(s);
  // long RGB or RGBA notation (#RRGGBB, #RRGGBBAA)
  if (xLen = 6) or (xLen = 8) then
  begin
    sr := Copy(s, 1, 2);
    if not TryHexToByte(sr, r) then Exit;

    sg := Copy(s, 3, 2);
    if not TryHexToByte(sg, g) then Exit;

    sb := Copy(s, 5, 2);
    if not TryHexToByte(sb, b) then Exit;

    if xLen = 6 then
    begin
      Color := RGB(r, g, b);
      Result := True;
    end
    else
    begin
      sa := Copy(s, 7, 2);
      if not TryHexToByte(sa, a) then Exit;
      Color := RGBA(r, g, b, a);
      Result := True;
    end;
  end;

end;

{$endregion HTML colors}


{$region '                       HSL colors                             '}

procedure SetHslMaxValues(const MaxHue, MaxSat, MaxLum: integer);
begin
  HSLMaxHue := MaxHue;
  HSLMaxSaturation := MaxSat;
  HSLMaxLightness := MaxLum;
end;

procedure SetHslCssMaxValues;
begin
  SetHslMaxValues(HSL_MAX_CSS_HUE, HSL_MAX_CSS_SAT, HSL_MAX_CSS_LUM);
end;

procedure SetHslWinMaxValues;
begin
  SetHslMaxValues(HSL_MAX_WIN_HUE, HSL_MAX_WIN_SAT, HSL_MAX_WIN_LUM);
end;

function TryHslCssStrToColor(HslCssStr: string; out cl: TColor): Boolean;
begin
  Result := TryHslRangeToColor(HslCssStr, cl, ',', HSL_MAX_CSS_HUE, HSL_MAX_CSS_SAT, HSL_MAX_CSS_LUM);
end;

function TryHslWinStrToColor(HslWinStr: string; out cl: TColor): Boolean;
begin
  Result := TryHslRangeToColor(HslWinStr, cl, ',', HSL_MAX_WIN_HUE, HSL_MAX_WIN_SAT, HSL_MAX_WIN_LUM);
end;

function TryHslRangeToColor(s: string; var Color: TColor; Separator: string = ','; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100): Boolean;
var
  Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>;
  sH, sS, sL: string;
  xH, xS, xL: integer;
begin
  SetHslMaxValues(AMaxHue, AMaxSat, AMaxLum);

  // Accepted input values:
  //   hsl(210,50%,39%)
  //   210,50%,39%
  // Separator: comma (,)

  Result := False;
  s := RemoveAll(s, ' ');
  s := RemoveAll(s, 'hsl', True);
  s := RemoveAll(s, '(');
  s := RemoveAll(s, ')');
  s := RemoveAll(s, '%');
  s := RemoveAll(s, DEG);
  SplitStrToArray(s, Arr{%H-}, Separator);
  sH := '';
  sS := '';
  sL := '';

  if Length(Arr) <> 3 then Exit;

  sH := Arr[0];
  sS := Arr[1];
  sL := Arr[2];

  if (sH = '') or (sS = '') or (sL = '') then Exit;

  if not TryStrToInt(sH, xH) then Exit;
  if not TryStrToInt(sS, xS) then Exit;
  if not TryStrToInt(sL, xL) then Exit;

  xS := GetIntInRange(xS, 0, HSLMaxSaturation);
  xL := GetIntInRange(xL, 0, HSLMaxLightness);
  Color := HslToColor(xH / HSLMaxHue, xS / HSLMaxSaturation, xL / HSLMaxLightness);

  Result := True;
end;


function HslWinToColor(const Hue, Sat, Lum: Single): TColor;
begin
  SetHslWinMaxValues;
  Result := HslToColor(Hue / HSLMaxHue, Sat / HSLMaxSaturation, Lum / HSLMaxLightness);
end;

function HslCssToColor(const Hue, Sat, Lum: Single): TColor;
begin
  SetHslCssMaxValues;
  Result := HslToColor(Hue / HSLMaxHue, Sat / HSLMaxSaturation, Lum / HSLMaxLightness);
end;


function HslToHslCssStr(const Hue, Sat, Lum: integer; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100; bShowPercent: Boolean = True;
  Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','): string;
var
  sHue, sSat, sLum: string;
  dxp: Double;
  x: integer;
begin
  dxp := PercentValue(Hue, AMaxHue);
  x := Round(PercentOf(dxp, 360));
  sHue := Pad(IntToStr(x), Padding, PaddingChar);

  dxp := PercentValue(Sat, AMaxSat);
  x := Round(dxp);
  sSat := Pad(IntToStr(x), Padding, PaddingChar);
  if bShowPercent then sSat := sSat + '%';

  dxp := PercentValue(Lum, AMaxLum);
  x := Round(dxp);
  sLum := Pad(IntToStr(x), Padding, PaddingChar);
  if bShowPercent then sLum := sLum + '%';

  Result := sHue + Separator + sSat + Separator + sLum;
end;

function ColorToHslRangeStr(const Color: TColor; AMaxHue: integer = 360; AMaxSat: integer = 100; AMaxLum: integer = 100; bShowPercent: Boolean = True;
  Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
var
  H1, S1, L1: integer;
  sH, sS, sL: string;
begin
  SetHslMaxValues(AMaxHue, AMaxSat, AMaxLum);

  ColortoHSLRange(Color, H1, S1, L1);

  sH := Pad(IntToStr(H1), Padding, PaddingChar);
  if bShowDeg then sH := sH + DEG;

  sS := Pad(IntToStr(S1), Padding, PaddingChar);
  if bShowPercent then sS := sS + '%';

  sL := Pad(IntToStr(L1), Padding, PaddingChar);
  if bShowPercent then sL := sL + '%';

  Result := sH + Separator + sS + Separator + sL;
end;

function ColorToHslCssStr(const Color: TColor; UsePercent: Boolean = True; Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','; bShowDeg: Boolean = True): string;
begin
  Result := ColorToHslRangeStr(Color, HSL_MAX_CSS_HUE, HSL_MAX_CSS_SAT, HSL_MAX_CSS_LUM, UsePercent, Padding, PaddingChar, Separator, bShowDeg);
end;

function ColorToHslWinStr(const Color: TColor; UsePercent: Boolean = False; Padding: Byte = 0; PaddingChar: Char = ' '; Separator: string = ','): string;
begin
  Result := ColorToHslRangeStr(Color, HSL_MAX_WIN_HUE, HSL_MAX_WIN_SAT, HSL_MAX_WIN_LUM, UsePercent, Padding, PaddingChar, Separator, False);
end;

procedure ColorToHslCss(const Color: TColor; out H, S, L: integer);
begin
  SetHslCssMaxValues;
  ColortoHSLRange(Color, H, S, L);
end;

function ColorToHslStr(const Color: TColor; AMaxHue: integer = 239; AMaxSat: integer = 240; AMaxLum: integer = 240;
  Padding: Byte = 3; PaddingChar: Char = '0'; Separator: string = ','): string;
var
  h, s, l: integer;
begin
  HSLMaxHue := AMaxHue;
  HSLMaxSaturation := AMaxSat;
  HSLMaxLightness := AMaxLum;
  ColortoHSLRange(Color, h, s, l);

  Result :=
    Pad(IntToStr(h), Padding, PaddingChar) + Separator +
    Pad(IntToStr(s), Padding, PaddingChar) + Separator +
    Pad(IntToStr(l), Padding, PaddingChar);
end;

procedure ColorToHslSys(const Color: TColor; out Hue, Sat, Lum: integer);
begin
  SetHslWinMaxValues;
  ColortoHSLRange(Color, Hue, Sat, Lum);
end;

function HslSysToColor(const Hue, Sat, Lum: integer): TColor;
begin
  SetHslWinMaxValues;
  Result := HSLRangeToRGB(Hue, Sat, Lum);
end;

function HslToColor(H, S, L: double): TColor;
var
  M1, M2: double;

  function HueToColorValue(Hue: double): byte;
  var
    V: double;
  begin
    if Hue < 0 then Hue := Hue + 1
    else if Hue > 1 then Hue := Hue - 1;
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := round(255 * V)
  end;

var
  R, G, B: byte;
begin
  if S = 0 then
  begin
    R := Round(255 * L); //JP mod:  round(HSLMaxLightness * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
  Result := RGB(R, G, B)
end;

function HSLRangeToRGB(H, S, L: integer): TColor;
begin
  if S > HSLMaxSaturation then S := HSLMaxSaturation;
  if S < 0 then S := 0;
  if L > HSLMaxLightness then L := HSLMaxLightness;
  if L < 0 then L := 0;
  Result := HslToColor(H / HSLMaxHue, S / HSLMaxSaturation, L / HSLMaxLightness);
end;

procedure ColortoHSLRange(const RGB: TColor; out H1, S1, L1: integer);
var
  br, bg, bb: Byte;
  R, G, B, D, Cmax, Cmin, H, S, L: double;
begin
  GetRgbChannels(RGB, br, bg, bb);
  R := br / 255;
  G := bg / 255;
  B := bb / 255;

  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    D := Cmax - Cmin;
    //calc L
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    //calc H
    if R = Cmax then H := (G - B) / D
    else if G = Cmax then H := 2 + (B - R) / D
    else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1;
  end;
  H1 := round(H * HSLMaxHue);
  S1 := round(S * HSLMaxSaturation);
  L1 := round(L * HSLMaxLightness);
end;

function GetHValue(const AColor: TColor): integer;
var
  D, H: integer;
begin
  ColortoHSLRange(AColor, H, D, D);
  Result := H;
end;

function GetSValue(const AColor: TColor): integer;
var
  D, S: integer;
begin
  ColortoHSLRange(AColor, D, S, D);
  Result := S;
end;

function GetLValue(const AColor: TColor): integer;
var
  D, L: integer;
begin
  ColortoHSLRange(AColor, D, D, L);
  Result := L;
end;

procedure Clamp(var Input: integer; Min, Max: integer);
begin
  if (Input < Min) then Input := Min;
  if (Input > Max) then Input := Max;
end;

function HSLToRGBTriple(H, S, L: integer): TRGBTriple;
const
  Divisor = 255 * 60;
var
  hTemp, f, LS, p, q, R: integer;
begin
  Clamp(H, 0, HSLMaxHue);
  Clamp(S, 0, HSLMaxSaturation);
  Clamp(L, 0, HSLMaxLightness);
  if (S = 0) then Result := RGBToRGBTriple(L, L, L)
  else
  begin
    hTemp := H mod HSLMaxHue;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    LS := L * S;
    p := L - LS div HSLMaxLightness;
    q := L - (LS * f) div Divisor;
    R := L - (LS * (60 - f)) div Divisor;
    case hTemp of
      0: Result := RGBToRGBTriple(L, R, p);
      1: Result := RGBToRGBTriple(q, L, p);
      2: Result := RGBToRGBTriple(p, L, R);
      3: Result := RGBToRGBTriple(p, q, L);
      4: Result := RGBToRGBTriple(R, p, L);
      5: Result := RGBToRGBTriple(L, p, q);
      else Result := RGBToRGBTriple(0, 0, 0);
    end;
  end;
end;

function HSLToRGBQuad(H, S, L: integer): TRGBQuad;
const
  Divisor = 255 * 60;
var
  hTemp, f, LS, p, q, R: integer;
begin
  Clamp(H, 0, HSLMaxHue);
  Clamp(S, 0, HSLMaxSaturation);
  Clamp(L, 0, HSLMaxLightness);
  if (S = 0) then Result := RGBToRGBQuad(L, L, L)
  else
  begin
    hTemp := H mod HSLMaxHue;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    LS := L * S;
    p := L - LS div HSLMaxLightness;
    q := L - (LS * f) div Divisor;
    R := L - (LS * (60 - f)) div Divisor;
    case hTemp of
      0: Result := RGBToRGBQuad(L, R, p);
      1: Result := RGBToRGBQuad(q, L, p);
      2: Result := RGBToRGBQuad(p, L, R);
      3: Result := RGBToRGBQuad(p, q, L);
      4: Result := RGBToRGBQuad(R, p, L);
      5: Result := RGBToRGBQuad(L, p, q);
      else Result := RGBToRGBQuad(0, 0, 0);
    end;
  end;
end;

procedure RGBTripleToHSL(RGBTriple: TRGBTriple; out H, S, L: integer);

  function RGBMaxValue(RGB: TRGBTriple): byte;
  begin
    Result := RGB.rgbtRed;
    if (Result < RGB.rgbtGreen) then Result := RGB.rgbtGreen;
    if (Result < RGB.rgbtBlue) then Result := RGB.rgbtBlue;
  end;

  function RGBMinValue(RGB: TRGBTriple): byte;
  begin
    Result := RGB.rgbtRed;
    if (Result > RGB.rgbtGreen) then Result := RGB.rgbtGreen;
    if (Result > RGB.rgbtBlue) then Result := RGB.rgbtBlue;
  end;

var
  Delta, Min: byte;
begin
  H := 0;
  L := RGBMaxValue(RGBTriple);
  Min := RGBMinValue(RGBTriple);
  Delta := L - Min;
  if (L = Min) then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    S := MulDiv(Delta, 255, L);
    with RGBTriple do
    begin
      if (rgbtRed = L) then H := MulDiv(60, rgbtGreen - rgbtBlue, Delta)
      else if (rgbtGreen = L) then H := MulDiv(60, rgbtBlue - rgbtRed, Delta) + 120
      else if (rgbtBlue = L) then H := MulDiv(60, rgbtRed - rgbtGreen, Delta) + 240;
      if (H < 0) then H := H + 360;
    end;
  end;
end;


function GetHueCssValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Result, x, x);
end;

function GetSatCssValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, x, Result, x);
end;

function GetLumCssValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, x, x, Result);
end;


function SetHueCssValue(const AColor: TColor; const NewHueValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(NewHueValue, Sat, Lum);
end;

function SetSatCssValue(const AColor: TColor; const NewSatValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(Hue, NewSatValue, Lum);
end;

function SetLumCssValue(const AColor: TColor; const NewLumValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(Hue, Sat, NewLumValue);
end;

function SetHslCssDelta(const AColor: TColor; const DeltaHue, DeltaSat, DeltaLum: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);

  Hue := Hue + DeltaHue;
  LimitValue(Hue, 0, HSL_MAX_CSS_HUE);

  Sat := Sat + DeltaSat;
  LimitValue(Sat, 0, HSL_MAX_CSS_SAT);

  Lum := Lum + DeltaLum;
  LimitValue(Lum, 0, HSL_MAX_CSS_LUM);

  Result := HSLRangeToRGB(Hue, Sat, Lum);
end;




function GetHueWinValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, Result, x, x);
end;

function GetSatWinValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, x, Result, x);
end;

function GetLumWinValue(const AColor: TColor): integer;
var
  x: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, x, x, Result);
end;

function SetHueWinValue(const AColor: TColor; const NewHueValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(NewHueValue, Sat, Lum);
end;

function SetSatWinValue(const AColor: TColor; const NewSatValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(Hue, NewSatValue, Lum);
end;

function SetLumWinValue(const AColor: TColor; const NewLumValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(Hue, Sat, NewLumValue);
end;

function SetHslWinDelta(const AColor: TColor; const DeltaHue, DeltaSat, DeltaLum: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  SetHslWinMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Result := HSLRangeToRGB(Hue + DeltaHue, Sat + DeltaSat, Lum + DeltaLum);
end;




function HslCssShiftHue(const AColor: TColor; const ShiftValue: integer): TColor;
var
  Hue, Sat, Lum: integer;
begin
  if ShiftValue = 0 then Exit(AColor);

  SetHslCssMaxValues;
  ColortoHSLRange(AColor, Hue, Sat, Lum);
  Hue := Hue + ShiftValue;

  if Hue > HSL_MAX_CSS_HUE then
    while Hue > HSL_MAX_CSS_HUE do Hue := Hue - HSL_MAX_CSS_HUE
  else if Hue < 0 then
    while Hue < 0 do Hue := Hue + HSL_MAX_CSS_HUE;

  Result := HSLRangeToRGB(Hue, Sat, Lum);
end;

function ComplementaryColor(const AColor: TColor): TColor;
begin
  Result := HslCssShiftHue(AColor, HSL_MAX_CSS_HUE div 2 { 180° });
end;

function TriadicColor1(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) - DistanceFromComplementaryColor { default: 180° - 60° = 120° });
end;

function TriadicColor2(const AColor: TColor; DistanceFromComplementaryColor: integer = 60): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) + DistanceFromComplementaryColor { default: 180° + 60° = 240° });
end;

procedure GetTriadicColors(const AColor: TColor; out TriadicColor_1, TriadicColor_2: TColor; DistanceFromComplementaryColor: integer = 60);
begin
  TriadicColor_1 := TriadicColor1(AColor, DistanceFromComplementaryColor);
  TriadicColor_2 := TriadicColor2(AColor, DistanceFromComplementaryColor);
end;

function SquareColor1(const AColor: TColor): TColor;
begin
  Result := HslCssShiftHue(AColor, HSL_MAX_CSS_HUE div 4 { 90° });
end;

function SquareColor2(const AColor: TColor): TColor;
begin
  Result := HslCssShiftHue(AColor, HSL_MAX_CSS_HUE div 2 { 180° });
end;

function SquareColor3(const AColor: TColor): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 4) * 3 { 270° });
end;

procedure GetSquareColors(const AColor: TColor; out SqColor1, SqColor2, SqColor3: TColor);
begin
  SqColor1 := SquareColor1(AColor);
  SqColor2 := SquareColor2(AColor);
  SqColor3 := SquareColor3(AColor);
end;

function TetradicColor1(const AColor: TColor; DegDistance: integer = 90): TColor;
begin
  Result := HslCssShiftHue(AColor, DegDistance);
end;

function TetradicColor2(const AColor: TColor): TColor;
begin
  Result := ComplementaryColor(AColor);
end;

function TetradicColor3(const AColor: TColor; DistanceFromComplementaryColor: integer = 90): TColor;
begin
  Result := HslCssShiftHue(AColor, (HSL_MAX_CSS_HUE div 2) + DistanceFromComplementaryColor);
end;

procedure GetTetradicColors(const AColor: TColor; out TetradColor1, TetradColor2, TetradColor3: TColor; DegDistance: integer = 90);
begin
  TetradColor1 := TetradicColor1(AColor, DegDistance);
  TetradColor2 := TetradicColor2(AColor);
  TetradColor3 := TetradicColor3(AColor, DegDistance);
end;



{$endregion HSL colors}


end.

