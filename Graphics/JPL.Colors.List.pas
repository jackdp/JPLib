unit JPL.Colors.List;

{$IFDEF FPC}
  //{$mode objfpc}{$H+}
  //{$MODESWITCH ADVANCEDRECORDS}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS} Windows,{$ENDIF}
  {$IFDEF FPC} LCLType, {$ENDIF}
  Classes, SysUtils, Graphics, Math, JPL.Math, JPL.Strings, JPL.Conversion, JPL.Colors;
  

type

  TColorListItem = record
    No: integer;
    Color: TColor;
    ColorName: string;
    function Red: Byte;
    function Green: Byte;
    function Blue: Byte;
    procedure GetRgbChannels(out r, g, b: Byte);
    procedure Assign(const Item: TColorListItem);
  end;

  TColorListArray = array of TColorListItem;

  TColorCompareDirection = (ccdAscending, ccdDescending);
  TCompareColorListItemsProc = function (const Item1, Item2: TColorListItem; const Direction: TColorCompareDirection): integer;

  TColorListSortMode = (
    clsmRed, clsmGreen, clsmBlue, clsmRgbSum,
    clsmColorName, clsmNumber, clsmColorValue,
    clsmCmykCyan, clsmCmykMagenta, clsmCmykYellow, clsmCmykBlack,
    clsmHslHue, clsmHslSat, clsmHslLum
  );


  TColorList = class
  private
    FArray: TColorListArray;
    FColorToNameSeparator: string;
    FNumberToColorSeparator: string;
    function GetCount: integer;
    function GetItems(Index: integer): TColorListItem;
    procedure SetItems(Index: integer; const Value: TColorListItem);
    procedure SetColorToNameSeparator(const Value: string);
    procedure SetNumberToColorSeparator(const Value: string);
    function GetColors(Index: integer): TColor;
    procedure SetColors(Index: integer; const Value: TColor);
    function GetNames(Index: integer): string;
    procedure SetNames(Index: integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    // Returns the index of the newly added item
    function AddColor(const Color: TColor; const ColorName: string; const No: integer): integer; overload;
    // As above but with auto numbering (No = Index + 1)
    function AddColor(const Color: TColor; const ColorName: string): integer; overload;
    function AddColor(const Color: TColor): integer; overload;
    function AddColor(const Item: TColorListItem; AutoNumbering: Boolean = False): integer; overload;

    procedure AddColorsFromArray(const Arr: TColorListArray; AutoNumbering: Boolean = False);
    procedure SaveColorsToArray(var Arr: TColorListArray);

    // ---------- Sorting ------------
    procedure SortByRedChannel(Ascending: Boolean = True);
    procedure SortByGreenChannel(Ascending: Boolean = True);
    procedure SortByBlueChannel(Ascending: Boolean = True);
    procedure SortByRgbSum(Ascending: Boolean = True);
    procedure SortByColorName(Ascending: Boolean = True);
    procedure SortByNumber(Ascending: Boolean = True);
    procedure SortByColorValue(Ascending: Boolean = True);
    procedure SortByCMYK_Cyan(Ascending: Boolean);
    procedure SortByCMYK_Magenta(Ascending: Boolean);
    procedure SortByCMYK_Yellow(Ascending: Boolean);
    procedure SortByCMYK_Black(Ascending: Boolean);
    procedure SortByHSL_Hue(Ascending: Boolean);
    procedure SortByHSL_Sat(Ascending: Boolean);
    procedure SortByHSL_Lum(Ascending: Boolean);

    // ---------- Color modification -----------
    procedure Modify_Grayscale;
    procedure Modify_MixWithColor(const cl: TColor);
    procedure Modify_Brightness(const Percent: integer);
    procedure Modify_Contrast(const Percent: integer);
    procedure Modify_Pale(const Percent: integer);
    procedure Modify_Invert;
    procedure Modify_InvertRgbChannels(bInvertRed, bInvertGreen, bInvertBlue: Boolean);
    procedure Modify_SetRandomColor;
    procedure Modify_RandomRgbChannels(bRandomRed, bRandomGreen, bRandomBlue: Boolean);
    procedure Modify_SetRedChannel(const NewRedValue: Byte);
    procedure Modify_SetGreenChannel(const NewGreenValue: Byte);
    procedure Modify_SetBlueChannel(const NewBlueValue: Byte);
    procedure Modify_ChangeRedChannel(const DeltaRed: integer);
    procedure Modify_ChangeGreenChannel(const DeltaGreen: integer);
    procedure Modify_ChangeBlueChannel(const DeltaBlue: integer);
    procedure Modify_ChangeRGBChannels(const DeltaRed, DeltaGreen, DeltaBlue: integer);
    procedure Modify_SetHueCssValue(const NewHueValue: integer);
    procedure Modify_SetSatCssValue(const NewSatValue: integer);
    procedure Modify_SetLumCssValue(const NewLumValue: integer);
    procedure Modify_SetHslCssDelta(const DeltaHue, DeltaSat, DeltaLum: integer);
    procedure Modify_SetHueWinValue(const NewHueValue: integer);
    procedure Modify_SetSatWinValue(const NewSatValue: integer);
    procedure Modify_SetLumWinValue(const NewLumValue: integer);
    procedure Modify_SetHslWinDelta(const DeltaHue, DeltaSat, DeltaLum: integer);

    procedure Renumber(FirstItemNo: integer = 1);
    procedure ExchangeItems(Index1, Index2: integer);

    function AsPlainText(ColorType: TColorType = ctRgb; UseColorNames: Boolean = False; UseColorNumbers: Boolean = False; Separator: string = ','): string;
    procedure SaveToPlainTextFile(const FileName: string; ColorType: TColorType = ctRgb; UseColorNames: Boolean = False; UseColorNumbers: Boolean = False);

    function AsGimpPalette(const PaletteName: string): string;
    procedure SaveToGimpPaletteFile(const FileName, PaletteName: string);


    // ------------------------ PROPERTIES ---------------------------
    property ColorArray: TColorListArray read FArray write FArray;
    property Count: integer read GetCount;

    property Items[Index: integer]: TColorListItem read GetItems write SetItems; default;
    property Colors[Index: integer]: TColor read GetColors write SetColors;
    property Names[Index: integer]: string read GetNames write SetNames;

    property ColorToNameSeparator: string read FColorToNameSeparator write SetColorToNameSeparator;
    property NumberToColorSeparator: string read FNumberToColorSeparator write SetNumberToColorSeparator;
  end;


function ColorListSortModeToStr(const SortingMode: TColorListSortMode): string;
function SortModeStrToColorListSortMode(ModeStr: string; Default: TColorListSortMode): TColorListSortMode;


implementation


{$region ' --------- helpers ----------- '}

function ColorListSortModeToStr(const SortingMode: TColorListSortMode): string;
begin
  case SortingMode of
    clsmRed: Result := 'RGB - Red';
    clsmGreen: Result := 'RGB - Green';
    clsmBlue: Result := 'RGB - Blue';
    clsmRgbSum: Result := 'RGB - Sum';
    clsmColorName: Result := 'Color name';
    clsmNumber: Result := 'Color No';
    clsmColorValue: Result := 'Color value';
    clsmCmykCyan: Result := 'CMYK - Cyan';
    clsmCmykMagenta: Result := 'CMYK - Magenta';
    clsmCmykYellow: Result := 'CMYK - Yellow';
    clsmCmykBlack: Result := 'CMYK - Black';
    clsmHslHue: Result := 'HSL - Hue';
    clsmHslSat: Result := 'HSL - Sat';
    clsmHslLum: Result := 'HSL - Lum';
  else
    Result := '';
  end;

end;

function SortModeStrToColorListSortMode(ModeStr: string; Default: TColorListSortMode): TColorListSortMode;
begin
  ModeStr := UpperCase(ModeStr);
  ModeStr := RemoveAll(ModeStr, ' ');
  ModeStr := RemoveAll(ModeStr, '-');
  if ModeStr = 'RGBRED' then Result := clsmRed
  else if ModeStr = 'RGBGREEN' then Result := clsmGreen
  else if ModeStr = 'RGBBLUE' then Result := clsmBlue
  else if ModeStr = 'RGBSUM' then Result := clsmRgbSum
  else if ModeStr = 'COLORNAME' then Result := clsmColorName
  else if ModeStr = 'COLORNO' then Result := clsmNumber
  else if ModeStr = 'COLORVALUE' then Result := clsmColorValue
  else if ModeStr = 'CMYKCYAN' then Result := clsmCmykCyan
  else if ModeStr = 'CMYKMAGENTA' then Result := clsmCmykMagenta
  else if ModeStr = 'CMYKYELLOW' then Result := clsmCmykYellow
  else if ModeStr = 'CMYKBLACK' then Result := clsmCmykBlack
  else if ModeStr = 'HSLHUE' then Result := clsmHslHue
  else if ModeStr = 'HSLSAT' then Result := clsmHslSat
  else if ModeStr = 'HSLLUM' then Result := clsmHslLum
  else Result := Default;
end;

{$endregion helpers}


{$region ' -------------------------------- TColorList -------------------------------- '}

constructor TColorList.Create;
begin
  SetLength(FArray, 0);
  FColorToNameSeparator := TAB;
  FNumberToColorSeparator := ' ';
end;

destructor TColorList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TColorList.Clear;
var
  i: integer;
begin
  for i := 0 to High(FArray) do FArray[i].ColorName := '';
  SetLength(FArray, 0);
end;


  {$region ' ------------ Sorting ---------------- '}

function _CompareColorItems_ByRGBSum(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareRgbSumColors(Item1.Color, Item2.Color);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByRedChannel(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareRgbColors(Item1.Color, Item2.Color, rcRed);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByGreenChannel(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareRgbColors(Item1.Color, Item2.Color, rcGreen);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByBlueChannel(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareRgbColors(Item1.Color, Item2.Color, rcBlue);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByName(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := AnsiCompareStr(Item1.ColorName, Item2.ColorName);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByNumber(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  if Item1.No > Item2.No then Result := 1
  else if Item1.No < Item2.No then Result := -1
  else Result := 0;
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByColorValue(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  if Item1.Color > Item2.Color then Result := 1
  else if Item1.Color < Item2.Color then Result := -1
  else Result := 0;
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByCMYK_Cyan(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareCmykColors(Item1.Color, Item2.Color, ccCyan);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByCMYK_Magenta(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareCmykColors(Item1.Color, Item2.Color, ccMagenta);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByCMYK_Yellow(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareCmykColors(Item1.Color, Item2.Color, ccYellow);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByCMYK_Black(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareCmykColors(Item1.Color, Item2.Color, ccBlack);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByHSL_Hue(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareHslColors(Item1.Color, Item2.Color, hcHue);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByHSL_Sat(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareHslColors(Item1.Color, Item2.Color, hcSat);
  if Direction = ccdDescending then Result := -Result;
end;

function _CompareColorItems_ByHSL_Lum(const Item1, Item2: TColorListItem; Direction: TColorCompareDirection): integer;
begin
  Result := CompareHslColors(Item1.Color, Item2.Color, hcLum);
  if Direction = ccdDescending then Result := -Result;
end;

procedure _ExchangeColorItems(var Arr: TColorListArray; Index1, Index2: integer);
var
  cli1: TColorListItem;
begin
  cli1 := Arr[Index1];
  Arr[Index1].Assign(Arr[Index2]);
  Arr[Index2].Assign(cli1);
end;

procedure _SortColorListArray(var Arr: TColorListArray; LeftIndex, RightIndex: Integer; CompareProc: TCompareColorListItemsProc; Direction: TColorCompareDirection);
var
  I, J, PivotIndex: Integer;
begin
  if Length(Arr) < 2 then Exit; // nothing to sort

  repeat

    I := LeftIndex;
    J := RightIndex;
    PivotIndex := (LeftIndex + RightIndex) shr 1;

    repeat

      while CompareProc(Arr[I], Arr[PivotIndex], Direction) < 0 do Inc(I);
      while CompareProc(Arr[J], Arr[PivotIndex], Direction) > 0 do Dec(J);

      if I <= J then
      begin
        if I <> J then _ExchangeColorItems(Arr, I, J);

        if PivotIndex = I then PivotIndex := J
        else if PivotIndex = J then PivotIndex := I;
        Inc(I);
        Dec(J);
      end;

    until I > J;

    if LeftIndex < J then _SortColorListArray(Arr, LeftIndex, J, CompareProc, Direction);
    LeftIndex := I;

  until I >= RightIndex;
end;


procedure TColorList.SortByRedChannel(Ascending: Boolean = True);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByRedChannel, Direction);
end;

procedure TColorList.SortByGreenChannel(Ascending: Boolean = True);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByGreenChannel, Direction);
end;

procedure TColorList.SortByBlueChannel(Ascending: Boolean = True);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByBlueChannel, Direction);
end;

procedure TColorList.SortByRgbSum(Ascending: Boolean = True);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByRgbSum, Direction);
end;

procedure TColorList.SortByColorName(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByName, Direction);
end;

procedure TColorList.SortByNumber(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByNumber, Direction);
end;

procedure TColorList.SortByColorValue(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByColorValue, Direction);
end;

procedure TColorList.SortByCMYK_Cyan(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByCMYK_Cyan, Direction);
end;

procedure TColorList.SortByCMYK_Magenta(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByCMYK_Magenta, Direction);
end;

procedure TColorList.SortByCMYK_Yellow(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByCMYK_Yellow, Direction);
end;

procedure TColorList.SortByCMYK_Black(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByCMYK_Black, Direction);
end;

procedure TColorList.SortByHSL_Hue(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByHSL_Hue, Direction);
end;

procedure TColorList.SortByHSL_Sat(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByHSL_Sat, Direction);
end;

procedure TColorList.SortByHSL_Lum(Ascending: Boolean);
var
  Direction: TColorCompareDirection;
begin
  if Ascending then Direction := ccdAscending else Direction := ccdDescending;
  _SortColorListArray(FArray, 0, High(FArray), @_CompareColorItems_ByHSL_Lum, Direction);
end;

procedure TColorList.ExchangeItems(Index1, Index2: integer);
begin
  _ExchangeColorItems(FArray, Index1, Index2);
end;

  {$endregion Sorting}


  {$region ' --------------- Adding Colors ---------------- '}
function TColorList.AddColor(const Color: TColor; const ColorName: string; const No: integer): integer;
begin
  SetLength(FArray, Length(FArray) + 1);
  FArray[High(FArray)].No := No;
  FArray[High(FArray)].Color := Color;
  FArray[High(FArray)].ColorName := ColorName;
  Result := High(FArray);
end;

function TColorList.AddColor(const Color: TColor; const ColorName: string): integer;
begin
  Result := AddColor(Color, ColorName, Length(FArray) + 1);
end;

function TColorList.AddColor(const Color: TColor): integer;
begin
  Result := AddColor(Color, '');
end;

function TColorList.AddColor(const Item: TColorListItem; AutoNumbering: Boolean = False): integer;
begin
  SetLength(FArray, Length(FArray) + 1);
  FArray[High(FArray)].Assign(Item);
  if AutoNumbering then FArray[High(FArray)].No := Length(FArray);
  Result := High(FArray);
end;

procedure TColorList.AddColorsFromArray(const Arr: TColorListArray; AutoNumbering: Boolean);
var
  i: integer;
  Item: TColorListItem;
begin
  for i := 0 to High(Arr) do
  begin
    Item := Arr[i];
    AddColor(Item, AutoNumbering);
  end;
end;

  {$endregion Adding Colors}


  {$region ' ----------------- Color modification -------------------- '}
procedure TColorList.Modify_ChangeRedChannel(const DeltaRed: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ModifyColorRedChannel(FArray[i].Color, DeltaRed);
  end;
end;

procedure TColorList.Modify_ChangeGreenChannel(const DeltaGreen: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ModifyColorGreenChannel(FArray[i].Color, DeltaGreen);
  end;
end;

procedure TColorList.Modify_ChangeBlueChannel(const DeltaBlue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ModifyColorBlueChannel(FArray[i].Color, DeltaBlue);
  end;
end;

procedure TColorList.Modify_ChangeRGBChannels(const DeltaRed, DeltaGreen, DeltaBlue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ModifyColorRGBChannels(FArray[i].Color, DeltaRed, DeltaGreen, DeltaBlue);
  end;
end;

procedure TColorList.Modify_Brightness(const Percent: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ColorSetPercentBrightness(FArray[i].Color, Percent);
  end;
end;

procedure TColorList.Modify_Contrast(const Percent: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ColorSetPercentContrast(FArray[i].Color, Percent);
  end;
end;

procedure TColorList.Modify_Pale(const Percent: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := ColorSetPercentPale(FArray[i].Color, Percent);
  end;
end;

procedure TColorList.Modify_RandomRgbChannels(bRandomRed, bRandomGreen, bRandomBlue: Boolean);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := RandomRgbChannels(FArray[i].Color, bRandomRed, bRandomGreen, bRandomBlue, False);
  end;
end;

procedure TColorList.Modify_SetRandomColor;
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := RandomColor;
  end;
end;

procedure TColorList.Modify_SetRedChannel(const NewRedValue: Byte);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetColorRedChannel(FArray[i].Color, NewRedValue);
  end;
end;

procedure TColorList.Modify_SetGreenChannel(const NewGreenValue: Byte);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetColorGreenChannel(FArray[i].Color, NewGreenValue);
  end;
end;

procedure TColorList.Modify_SetBlueChannel(const NewBlueValue: Byte);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetColorBlueChannel(FArray[i].Color, NewBlueValue);
  end;
end;

procedure TColorList.Modify_Grayscale;
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := FadeToGray(FArray[i].Color);
  end;
end;

procedure TColorList.Modify_Invert;
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := InvertColor(FArray[i].Color);
  end;
end;

procedure TColorList.Modify_InvertRgbChannels(bInvertRed, bInvertGreen, bInvertBlue: Boolean);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := InvertRgbChannels(FArray[i].Color, bInvertRed, bInvertGreen, bInvertBlue);
  end;
end;

procedure TColorList.Modify_MixWithColor(const cl: TColor);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := AvgColor(FArray[i].Color, cl);
  end;
end;

procedure TColorList.Modify_SetHueCssValue(const NewHueValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetHueCssValue(FArray[i].Color, NewHueValue);
  end;
end;

procedure TColorList.Modify_SetSatCssValue(const NewSatValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetSatCssValue(FArray[i].Color, NewSatValue);
  end;
end;

procedure TColorList.Modify_SetLumCssValue(const NewLumValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetLumCssValue(FArray[i].Color, NewLumValue);
  end;
end;

procedure TColorList.Modify_SetHslCssDelta(const DeltaHue, DeltaSat, DeltaLum: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetHslCssDelta(FArray[i].Color, DeltaHue, DeltaSat, DeltaLum);
  end;
end;

procedure TColorList.Modify_SetHueWinValue(const NewHueValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetHueWinValue(FArray[i].Color, NewHueValue);
  end;
end;

procedure TColorList.Modify_SetSatWinValue(const NewSatValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetSatWinValue(FArray[i].Color, NewSatValue);
  end;
end;

procedure TColorList.Modify_SetLumWinValue(const NewLumValue: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetLumWinValue(FArray[i].Color, NewLumValue);
  end;
end;

procedure TColorList.Modify_SetHslWinDelta(const DeltaHue, DeltaSat, DeltaLum: integer);
var
  i: integer;
begin
  for i := 0 to High(FArray) do
  begin
    FArray[i].Color := SetHslWinDelta(FArray[i].Color, DeltaHue, DeltaSat, DeltaLum);
  end;
end;

  {$endregion Color modification}



function TColorList.GetCount: integer;
begin
  Result := Length(FArray);
end;

function TColorList.GetItems(Index: integer): TColorListItem;
begin
  Result := FArray[Index];
end;

procedure TColorList.Renumber(FirstItemNo: integer = 1);
var
  i, xNo: integer;
begin
  xNo := FirstItemNo;
  for i := 0 to High(FArray) do
  begin
    FArray[i].No := xNo;
    Inc(xNo);
  end;
end;

function TColorList.AsGimpPalette(const PaletteName: string): string;
var
  s: string;
begin
  s := AsPlainText(ctRgb, False, False, ' ');
  s := 'GIMP Palette' + ENDL + 'Name: ' + PaletteName + ENDL + '#' + ENDL + s;
  Result := s;
end;

function TColorList.AsPlainText(ColorType: TColorType = ctRgb; UseColorNames: Boolean = False; UseColorNumbers: Boolean = False; Separator: string = ','): string;
var
  i: integer;
  cl: TColor;
  s: string;
begin
  Result := '';
  for i := 0 to High(FArray) do
  begin
    cl := FArray[i].Color;
    s := ColorToStrEx(cl, ColorType, Separator);
    if UseColorNames then s := s + FColorToNameSeparator + FArray[i].ColorName;
    if UseColorNumbers then s := itos(FArray[i].No) + FNumberToColorSeparator + s;
    Result := Result + s + ENDL;
  end;
end;

procedure TColorList.SaveToGimpPaletteFile(const FileName, PaletteName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Trim(AsGimpPalette(PaletteName));
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TColorList.SaveToPlainTextFile(const FileName: string; ColorType: TColorType = ctRgb; UseColorNames: Boolean = False; UseColorNumbers: Boolean = False);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Trim(AsPlainText(ColorType, UseColorNames, UseColorNumbers));
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TColorList.SaveColorsToArray(var Arr: TColorListArray);
var
  i: integer;
begin
  SetLength(Arr, 0);
  for i := 0 to High(FArray) do
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := FArray[i];
  end;
end;

function TColorList.GetColors(Index: integer): TColor;
begin
  Result := FArray[Index].Color;
end;

procedure TColorList.SetColors(Index: integer; const Value: TColor);
begin
  FArray[Index].Color := Value;
end;

procedure TColorList.SetItems(Index: integer; const Value: TColorListItem);
begin
  FArray[Index].Assign(Value);
end;

function TColorList.GetNames(Index: integer): string;
begin
  Result := FArray[Index].ColorName;
end;

procedure TColorList.SetNames(Index: integer; const Value: string);
begin
  FArray[Index].ColorName := Value;
end;

procedure TColorList.SetColorToNameSeparator(const Value: string);
begin
  FColorToNameSeparator := Value;
end;

procedure TColorList.SetNumberToColorSeparator(const Value: string);
begin
  FNumberToColorSeparator := Value;
end;

{$endregion TColorList}



{$region ' -------------------- TColorListItem ------------------ '}

function TColorListItem.Red: Byte;
begin
  Result := GetRedChannel(Color);
end;

function TColorListItem.Green: Byte;
begin
  Result := GetGreenChannel(Color);
end;

function TColorListItem.Blue: Byte;
begin
  Result := GetBlueChannel(Color);
end;

procedure TColorListItem.GetRgbChannels(out r, g, b: Byte);
begin
  JPL.Colors.GetRgbChannels(Color, r, g, b);
end;

procedure TColorListItem.Assign(const Item: TColorListItem);
begin
  No := Item.No;
  Color := Item.Color;
  ColorName := Item.ColorName;
end;

{$endregion TColorListItem}



end.
