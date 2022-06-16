unit JPL.RoseDiag.BinDataList;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.


  BIN = numerical (angular) range


  2022.06
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math,
  Generics.Collections,
  JPL.Strings, JPL.TStr, JPL.Conversion, JPL.Math;

type

{$IFDEF DCC}
  Float = Extended;
{$ENDIF}


  // What to do with linear measurements when the measurement value is not between 0-180 degrees?
  //   lmfmNormalize - 180 degrees will be added or subtracted to the measurement value until the value is within 0-180 degrees.
  //   lmfmRemove - The value will be removed from the measurement list.
  TLinearMeasurementFixMode = (lmfmNormalize, lmfmRemove);

  TBinStats = record
    BinCount: Word;
    MeasurementCount: Word; // Total number of valid measurements
    SumOfMeasurements: Double; // Sum of all measurements
    MeasurementsPerBin: Single; // Average number of measurements in the bin
    MaxMeasurementsInBin: Word; // Maximum number of measurements in the bin(s)
    MinMeasurementsInBin: Word; // Minimum number of measurements in the bin(s)
    BinsWithMaxMeasurements: Word; // The number of bins with the maximum number of measurements
    BinsWithMinMeasurements: Word; // The number of bins with the minimum number of measurements
    MaxMeasurementsInBin_Percentage: Single;
    Mean: Float; // Average measurement
    StdDev: Float; // Standard deviation
    procedure Clear;
  end;

  TMeasurementArray = array of Single;

  TBinData = record
    No: integer; // Bin number (the first No = 1)
    BinIndex: integer; // Bin index (the first index = 0)
    Percent: Single; // Number of measurements in the bin relative to the total number of measurements, expressed as a percentage
    Values: TMeasurementArray; // Array with all values belonging to the given bin
    StartValue: Word; // The starting value of the bin (the starting angle in degrees)
    EndValue: Word; // End value of the bin (end angle in degrees)
    Selected: Boolean; // Used when drawing the diagram
    function MeasurementCount: Word; // The number of measurements in a bin. The WORD type should be fine.
    function SumOfMeasurements: Double; // Sum of all measurements in a bin
    function AsString(Separator: string = '  '): string; // for debug purposes
    function Mean: Float;
    function StdDev: Float;
    function ValuesAsStr(Separator: string = '; '): string;
    procedure Clear;
  end;

  TCustomBinDataList = TList<TBinData>;

  TBinDataList = class(TCustomBinDataList)
  private
    FAutoUpdateBins: Boolean;
    FAzimuths: Boolean;
    FLinearMeasurementFixMode: TLinearMeasurementFixMode;
    FClassSize: Byte;
    FMeasurements: TMeasurementArray;
    FStats: TBinStats;
    function GetSelectedBinsCount: integer;
    procedure SetAzimuths(AValue: Boolean);
    procedure SetClassSize(AValue: Byte);
    procedure SetLinearMeasurementFixMode(AValue: TLinearMeasurementFixMode);
  public
    constructor Create(const AClassSize: Byte = 10);
    procedure ClearParams(ClearMeasurements: Boolean = False);
    function AsDebugStr: string;

    // Tworzy listę przedziałów (bins) i przyporządkowuje do nich pomiary (wywołuje ProcessMeasurementArray(FMeasurements)).
    procedure UpdateBins;

    ///////////////// The main procedure //////////////////////
    // It assigns measurements to appropriate bins and calculates some additional parameters (MaxCount, MinCount...)
    procedure ProcessMeasurementArray(const Arr: TMeasurementArray); overload;
    procedure ProcessMeasurementArray(const Arr: array of Single); overload;
    ///////////////////////////////////////////////////////////////////////

    procedure ProcessText(const Text: string); // Process (multi-line) text with meausurements. One measurement per line.

    procedure GetMeasurementArray(var Arr: TMeasurementArray); // Saves all measurements to the given array

    // Generates the given number of random measurements from the given range
    procedure GenerateRandomData(const Count: Word; MinValue: Word = 0; MaxValue: Word = 360; FloatValues: Boolean = False);

    procedure SelectBin(const BinIndex: integer);
    procedure SelectBins(const NumberOfMeasurements: Word); // Select all bins with the given number of measurements
    procedure SelectMaxBins; // Select all bins with the maximum number of measurements
    procedure SelectMinBins; // Select all bins with the minimum number of measurements
    procedure SelectAllBins;
    procedure UnselectAllBins;
    procedure InvertSelectedBins;

    // Calculate various statistical data and save in the Stats record.
    // Automatically called when processing an array with measurements.
    procedure CalculateStats;

    property ClassSize: Byte read FClassSize write SetClassSize; // Must be an integer divisor of 360 (or 90?)
    property SelectedBinsCount: integer read GetSelectedBinsCount; // The number of bins marked as selected
    property Stats: TBinStats read FStats;
    property Azimuths: Boolean read FAzimuths write SetAzimuths; // Measurements in range 0-360

    // See description of the TLinearMeasurementFixMode above
    property LinearMeasurementFixMode: TLinearMeasurementFixMode read FLinearMeasurementFixMode write SetLinearMeasurementFixMode;

    // Not modified list of measurements
    // Przy przetwarzaniu pomiarów, niektóre z nich mogą nie trafić do "binów", gdy Azimuths=False i LinearMeasurementFixMode=lmfmRemove.
    // Tablica Measurements zawiera wszystkie pomiary, także te nieprawidłowe.
    property Measurements: TMeasurementArray read FMeasurements;

    // Decyduje, czy przy zmianie niektórych parametrów (np. ClassSize), lista przedziałów będzie odtwarzana automatycznie.
    property AutoUpdateBins: Boolean read FAutoUpdateBins write FAutoUpdateBins;
  end;


{$region ' helpers '}
function LinearMeasurementFixModeToStrID(const FixMode: TLinearMeasurementFixMode): string;
function StrIDToLinearMeasurementFixMode(StrID: string; Default: TLinearMeasurementFixMode = lmfmNormalize): TLinearMeasurementFixMode;
function MeasurementArrayAsText(const Arr: TMeasurementArray): string;
procedure SaveMeasurementArrayAsTextFile(const Arr: TMeasurementArray; const FileName: string);
procedure CopyMeasurementArray(const Src: array of Single; var Dest: TMeasurementArray);
procedure FixLinearMeasurements(var Arr: TMeasurementArray; const FixMode: TLinearMeasurementFixMode);
procedure GetSymmetricalData(const Arr: TMeasurementArray; var ArrSymm: TMeasurementArray);
function IsValidClassSize(const x: integer): Boolean;
procedure FillMeasurementArrayWithRandomValues(var Arr: TMeasurementArray; Count: Word; MinValue: Word = 0; MaxValue: Word = 360; FloatValues: Boolean = False);
{$endregion helpers}


implementation

uses
  JPL.RoseDiag.Diagram;


{$region ' helpers '}

function LinearMeasurementFixModeToStrID(const FixMode: TLinearMeasurementFixMode): string;
begin
  if FixMode = lmfmNormalize then Result := 'normalize_180' else Result := 'remove';
end;

function StrIDToLinearMeasurementFixMode(StrID: string; Default: TLinearMeasurementFixMode = lmfmNormalize): TLinearMeasurementFixMode;
begin
  StrID := TStr.TrimAndLow(StrID);
  if StrID = 'normalize_180' then Result := lmfmNormalize
  else if StrID = 'remove' then Result := lmfmRemove
  else Result := Default;
end;

function MeasurementArrayAsText(const Arr: TMeasurementArray): string;
var
  s: string;
  i: integer;
begin
  s := '';
  for i := 0 to High(Arr) do
  begin
    s := s + ftos(Arr[i], 2) + ENDL;
  end;
  Result := Trim(s);
end;

procedure SaveMeasurementArrayAsTextFile(const Arr: TMeasurementArray; const FileName: string);
var
  s: string;
begin
  s := MeasurementArrayAsText(Arr);
  SaveStringToFile(FileName, s);
end;

procedure CopyMeasurementArray(const Src: array of Single; var Dest: TMeasurementArray);
var
  i: integer;
begin
  SetLength(Dest, Length(Src));
  for i := 0 to High(Src) do Dest[i] := Src[i];
end;

procedure FixLinearMeasurements(var Arr: TMeasurementArray; const FixMode: TLinearMeasurementFixMode);
var
  i: integer;
  ArrFix: TMeasurementArray;
  xValue: Single;
begin
  SetLength(ArrFix{%H-}, 0);

  for i := 0 to High(Arr) do
  begin
    xValue := Arr[i];

    if xValue < 0 then
    begin
      if FixMode = lmfmRemove then Continue;
      while xValue < 0 do xValue := xValue + 180;
    end

    else if xValue > 180 then
    begin
      if FixMode = lmfmRemove then Continue;
      while xValue > 180 do xValue := xValue - 180;
    end;

    SetLength(ArrFix, Length(ArrFix) + 1);
    ArrFix[High(ArrFix)] := xValue;
  end;


  Arr := ArrFix;
end;

procedure GetSymmetricalData(const Arr: TMeasurementArray; var ArrSymm: TMeasurementArray);
var
  i, dx: integer;
  xVal1, xVal2: Single;
begin
  SetLength(ArrSymm, Length(Arr) * 2);
  dx := 0;

  for i := 0 to High(Arr) do
  begin
    xVal1 := Arr[i];

    if xVal1 >= 180 then xVal2 := xVal1 - 180
    else xVal2 := xVal1 + 180;

    ArrSymm[i + dx] := xVal1;
    ArrSymm[i + dx + 1] := xVal2;
    Inc(dx);
  end;
end;

function IsValidClassSize(const x: integer): Boolean;
begin
  if (x <= 0) or (x > 180) then Exit(False);
  Result := 360 {90} mod x = 0;
end;

procedure FillMeasurementArrayWithRandomValues(var Arr: TMeasurementArray; Count: Word; MinValue: Word = 0; MaxValue: Word = 360; FloatValues: Boolean = False);
var
  i: integer;
begin
  SetLength(Arr, 0);
  if Count <= 0 then Exit;
  if MaxValue < MinValue then Exit;

  SetLength(Arr, Count);
  Randomize;

  if FloatValues then
  begin
    for i := 0 to Count - 1 do
    begin
      Arr[i] := RandomInt(MinValue, MaxValue, 0) + Random;
      if Arr[i] > MaxValue then Arr[i] := Arr[i] - 1;
    end;
  end
  else
    for i := 0 to Count - 1 do Arr[i] := RandomInt(MinValue, MaxValue, 0);
end;

{$endregion helpers}




{$region '                    TBinDataList                     '}

constructor TBinDataList.Create(const AClassSize: Byte);
begin
  inherited Create;
  FClassSize := AClassSize;
  FAzimuths := True;
  LinearMeasurementFixMode := lmfmNormalize;
  FAutoUpdateBins := True;
  ClearParams;
end;

procedure TBinDataList.ClearParams(ClearMeasurements: Boolean);
begin
  Clear;
  FStats.Clear;
  if ClearMeasurements then SetLength(FMeasurements, 0);
end;

procedure TBinDataList.SetClassSize(AValue: Byte);
begin
  if FClassSize = AValue then Exit;
  if not IsValidClassSize(AValue) then AValue := RD_DEFAULT_CLASS_SIZE;
  FClassSize := AValue;
  if FAutoUpdateBins then UpdateBins;
end;

procedure TBinDataList.SetLinearMeasurementFixMode(AValue: TLinearMeasurementFixMode);
begin
  if FLinearMeasurementFixMode = AValue then Exit;
  FLinearMeasurementFixMode := AValue;
  if FAutoUpdateBins then UpdateBins;
end;

procedure TBinDataList.SetAzimuths(AValue: Boolean);
begin
  if FAzimuths = AValue then Exit;
  FAzimuths := AValue;
  if FAutoUpdateBins then UpdateBins;
end;

function TBinDataList.GetSelectedBinsCount: integer;
var
  BinData: TBinData;
begin
  Result := 0;
  for BinData in Self do
    if BinData.Selected then Inc(Result);
end;

  {$region ' ProcessMeasurementArray '}
procedure TBinDataList.ProcessMeasurementArray(const Arr: TMeasurementArray);
var
  i, k: integer;
  BinCount: SmallInt;
  Measurement: Single;
  BinData: TBinData;
  b180, bInRange: Boolean;
  ArrM: TMeasurementArray;
begin
  ClearParams;
  FMeasurements := Arr;

  if ClassSize = 0 then Exit; // must be greather than 0
  b180 := not FAzimuths;


  ArrM := Arr;
  if b180 then FixLinearMeasurements(ArrM, FLinearMeasurementFixMode);

  //SaveMeasurementArrayAsTextFile(ArrM, 'ArrM.txt');



  // ----------- EN -----------
  // BinCount - number of ranges.
  // I am using an angular scale in degrees. Sometimes you can also find rose diagrams graduated in grads.
  // The grad scale, although more accurate, is usually only used in geodesy, so I'll stick to the degrees.


  // ----------- PL -----------
  // BinCount - liczba przedziałów.
  // Stosuję skalę kątową w stopniach. Niekiedy można też spotkać diagramy rozetowe wyskalowane w gradach.
  // Skala gradowa, chociaż dokładniejsza, stosowana jest raczej tylko w geodezji, więc będę się trzymał stopni.

  if b180 then BinCount := 180 div ClassSize
  else BinCount := 360 div ClassSize;


  // ----------- EN -----------
  // BIN - numerical (angular) range
  // In English literature and programs, the term "BIN" is usually used here when drawing rose diagrams.

  // BinData represents the angular range.
  // BinData.MeasurementCount - number of measurements belonging to the given range.
  // BinData.StartValue and BinData.EndValue - start and end of an angular range.
  //   All ranges, except the 1st, are left open and right closed.
  //   The first range is left closed (0 belongs to the 1st range).
  // BinData.Values - array of all measurements belonging to a given range.


  // ----------- PL -----------
  // BIN - przedział liczbowy (kątowy).
  // W literaturze i programach angielskojęzycznych, przy kreśleniu diagramów rozetowych z reguły stosuje się tutaj termin "BIN".

  // BinData reprezentuje przedział kątowy.
  // BinData.MeasurementCount - liczba azymutów należących do danego przedziału.
  // BinData.StartValue i BinData.EndValue - początek i koniec przedziału kątowego.
  //   Wszystkie przedziały, oprócz 1-szego, są lewostronnie otwarte i prawostronnie domknięte.
  //   Przedział pierwszy jest lewostronnie domknięty (0 jest zaliczane do 1-szego przedziału).
  // BinData.Values - tablica wszystkich azymutów należących do danego przedziału.

  for i := 0 to BinCount - 1 do
  begin
    BinData.No := i + 1;
    BinData.Percent := 0;
    BinData.BinIndex := i;
    BinData.Selected := False;
    BinData.StartValue := (i * ClassSize);
    BinData.EndValue := (i + 1) * ClassSize;
    SetLength(BinData.Values, 0);

    for k := 0 to High(ArrM) do
    begin
      Measurement := ArrM[k];

      if Measurement = 0 then bInRange := BinData.BinIndex = 0
      else bInRange := ( (Measurement > BinData.StartValue) and (Measurement <= BinData.EndValue) );

      if bInRange then
      begin
        SetLength(BinData.Values, Length(BinData.Values) + 1);
        BinData.Values[High(BinData.Values)] := Measurement;
      end;
    end;
    Self.Add(BinData);
  end; // for i

  CalculateStats;
end;

procedure TBinDataList.ProcessMeasurementArray(const Arr: array of Single);
var
  AA: TMeasurementArray;
  i: integer;
begin
  SetLength(AA{%H-}, Length(Arr));
  for i := 0 to High(Arr) do
    AA[i] := Arr[i];
  ProcessMeasurementArray(AA);
end;

  {$endregion ProcessMeasurementArray}

  {$region ' ProcessText '}
procedure TBinDataList.ProcessText(const Text: string);
var
  AR: TMeasurementArray;
  sl: TStringList;
  i, x: integer;
  Line, sNum: string;
  znak: Char;
  xd: Double;
begin
  sl := TStringList.Create;
  try

    sl.Text := Text;

    for i := sl.Count - 1 downto 0 do
    begin

      Line := sl[i];
      Line := Trim(Line);

      // Removing empty lines and comments
      {$B-}
      if (Line = '') or (Line[1] = ';') or (Line[1] = '#') or (Copy(Line, 1, 2) = '//') then
      begin
        sl.Delete(i);
        Continue;
      end;

      // Attempt to read a number at the beginning of the line
      // The sign that separates the whole part from the fractional part can be a point or a comma
      sNum := '';
      for x := 1 to Length(Line) do
      begin
        znak := Line[x];
        if (znak <> '.') and (znak <> ',') and (not IsNumber(znak)) then Break
        else sNum := sNum + znak;
      end;

      // If the value read is not a number, the line is removed
      if (sNum = '') or (not TryStoF(sNum, xd{%H-})) then
      begin
        sl.Delete(i);
        Continue;
      end;

      sl[i] := sNum;

    end; // for i


    SetLength(AR{%H-}, sl.Count);
    for i := 0 to sl.Count - 1 do AR[i] := stof(sl[i]);


    ProcessMeasurementArray(AR);


  finally
    sl.Free;
  end;
end;
  {$endregion ProcessText}

procedure TBinDataList.GetMeasurementArray(var Arr: TMeasurementArray);
var
  i, x: integer;
  BinData: TBinData;
begin
  SetLength(Arr, 0);
  for i := 0 to Count - 1 do
  begin
    BinData := Items[i];
    for x := 0 to High(BinData.Values) do
    begin
      SetLength(Arr, Length(Arr) + 1);
      Arr[High(Arr)] := BinData.Values[x];
    end;
  end;
end;

procedure TBinDataList.GenerateRandomData(const Count: Word; MinValue: Word = 0; MaxValue: Word = 360; FloatValues: Boolean = False);
var
  Arr: TMeasurementArray;
begin
  ClearParams;
  FillMeasurementArrayWithRandomValues(Arr{%H-}, Count, MinValue, MaxValue, FloatValues);
  ProcessMeasurementArray(Arr);
end;

function TBinDataList.AsDebugStr: string;
var
  BinData: TBinData;
  sz: string;
begin
  sz := 'Zero degrees belongs to the first bin';
  Result :=
    Self.ClassName + ENDL +
    'ClassSize: ' + itos(ClassSize) + ENDL +
    'Measurements: ' + itos(FStats.MeasurementCount) + ENDL +
    'MaxMeasurementsInBin: ' + itos(FStats.MaxMeasurementsInBin) + ENDL +
    'MinMeasurementsInBin: ' + itos(FStats.MinMeasurementsInBin) + ENDL +
    'Number of bins: ' + itos(Count) + ENDL +
    sz + ENDL;

  for BinData in Self do
    Result := Result + BinData.AsString + ENDL;
end;

procedure TBinDataList.UpdateBins;
begin
  ProcessMeasurementArray(FMeasurements);
end;

procedure TBinDataList.SelectBin(const BinIndex: integer);
var
  BinData: TBinData;
begin
  BinData := Items[BinIndex];
  BinData.Selected := True;
  Items[BinIndex] := BinData;
end;

procedure TBinDataList.SelectBins(const NumberOfMeasurements: Word);
var
  i: integer;
  BinData: TBinData;
begin
  for i := 0 to Count - 1 do
  begin
    BinData := Items[i];
    if BinData.MeasurementCount = NumberOfMeasurements then
    begin
      BinData.Selected := True;
      Items[i] := BinData;
    end;
  end;
end;

procedure TBinDataList.SelectMaxBins;
begin
  SelectBins(FStats.MaxMeasurementsInBin);
end;

procedure TBinDataList.SelectMinBins;
begin
  SelectBins(FStats.MinMeasurementsInBin);
end;

procedure TBinDataList.SelectAllBins;
var
  i: integer;
  BinData: TBinData;
begin
  for i := 0 to Count - 1 do
  begin
    BinData := Items[i];
    BinData.Selected := True;
    Items[i] := BinData;
  end;
end;

procedure TBinDataList.UnselectAllBins;
var
  i: integer;
  BinData: TBinData;
begin
  for i := 0 to Count - 1 do
  begin
    BinData := Items[i];
    BinData.Selected := False;
    Items[i] := BinData;
  end;
end;

procedure TBinDataList.InvertSelectedBins;
var
  i: integer;
  BinData: TBinData;
begin
  for i := 0 to Count - 1 do
  begin
    BinData := Items[i];
    BinData.Selected := not BinData.Selected;
    Items[i] := BinData;
  end;
end;

  {$region ' CalculateStats '}

{$IFNDEF FPC} // Delphi
procedure MeanAndStdDev(const Data: array of Single; var Mean, StdDev: Float);
var
  xM, xSD: Single;
begin
  Math.MeanAndStdDev(Data, xM, xSD);
  Mean := xM;
  StdDev := xSD;
end;
{$ENDIF}

procedure TBinDataList.CalculateStats;
var
  i, xCount: integer;
  BinData: TBinData;
  Arr: TMeasurementArray;
begin
  FStats.Clear;

  FStats.BinCount := Self.Count;
  GetMeasurementArray(Arr{%H-});
  FStats.MeasurementCount := Length(Arr);
  FStats.SumOfMeasurements := Sum(Arr);

  if FStats.BinCount > 0 then FStats.MeasurementsPerBin := FStats.MeasurementCount / FStats.BinCount;


  if FStats.MeasurementCount = 0 then FStats.MinMeasurementsInBin := 0 else FStats.MinMeasurementsInBin := High(FStats.MinMeasurementsInBin);
  FStats.MaxMeasurementsInBin := 0;

  if FStats.MeasurementCount > 0 then
    for i := 0 to Self.Count - 1 do
    begin
      BinData := Items[i];
      xCount := BinData.MeasurementCount;

      // I am looking for the range (bin) with the greatest and lowest number of measurements.
      // The first value will be needed to determine the maximum radius when plotting a rose diagram.
      if xCount > FStats.MaxMeasurementsInBin then FStats.MaxMeasurementsInBin := xCount;
      if xCount < FStats.MinMeasurementsInBin then FStats.MinMeasurementsInBin := xCount;

      BinData.Percent := PercentValue(xCount, FStats.MeasurementCount);

      Items[i] := BinData;
    end;


  FStats.BinsWithMaxMeasurements := 0;
  FStats.BinsWithMinMeasurements := 0;
  if FStats.MeasurementCount > 0 then
    for i := 0 to Count - 1 do
    begin
      xCount := Items[i].MeasurementCount;
      if xCount = FStats.MaxMeasurementsInBin then Inc(FStats.BinsWithMaxMeasurements);
      if xCount = FStats.MinMeasurementsInBin then Inc(FStats.BinsWithMinMeasurements);
    end;


  if FStats.MeasurementCount > 0 then
  begin
    FStats.MaxMeasurementsInBin_Percentage := PercentValue(FStats.MaxMeasurementsInBin, FStats.MeasurementCount);
    MeanAndStdDev(Arr, FStats.Mean, FStats.StdDev);
  end;
end;
  {$endregion CalculateStats}


{$endregion TBinDataList}


{$region '             TBinData              '}
function TBinData.AsString(Separator: string): string;
const
  ValSep = ' / ';
var
  i: integer;
  LeftBracket: string;
begin
  if BinIndex = 0 then LeftBracket := '<' else LeftBracket := '(';
  Result :=
    Separator + '----------' + ENDL +
    Separator + 'Bin No: ' + itos(No) + ENDL +
    Separator + 'Measurements: ' + itos(MeasurementCount) + ENDL +
    Separator + 'Percent: ' + ftos(Percent, 2) + '%' + ENDL +
    Separator + 'Range: ' + LeftBracket + itos(Round(StartValue)) + ',' + itos(Round(EndValue)) + '>' + ENDL +
    Separator + 'Selected: ' + BoolToStrYN(Selected);

  if Length(Values) > 0 then
  begin
    Result := Result + ENDL + Separator + 'Values (' + itos(Length(Values)) + '): ';
    for i := 0 to High(Values) do Result := Result + ftos(Values[i], 2) + ValSep;
    Result := TrimFromEnd(Result, ValSep);
  end;
end;

function TBinData.Mean: Float;
begin
  if Self.MeasurementCount = 0 then Result := 0
  else Result := Math.Mean(Values);
end;

function TBinData.StdDev: Float;
begin
  if Self.MeasurementCount = 0 then Result := 0
  else Result := Math.StdDev(Values);
end;

function TBinData.ValuesAsStr(Separator: string): string;
var
  i: integer;
begin
  Result := '';
  if Length(Values) = 0 then Exit;
  for i := 0 to High(Values) do
    Result := Result + ftos(Values[i]) + Separator;
  Result := TStr.TrimFromEnd(Result, Separator);
end;

function TBinData.MeasurementCount: Word;
begin
  Result := Length(Values);
end;

function TBinData.SumOfMeasurements: Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(Values) do Result := Result + Values[i];
end;

procedure TBinData.Clear;
begin
  No := -1;
  Percent := 0;
  SetLength(Values, 0);
  StartValue := 0;
  EndValue := 0;
  Selected := False;
end;
{$endregion TBinData}

procedure TBinStats.Clear;
begin
  BinCount := 0;
  MeasurementCount := 0;
  SumOfMeasurements := 0;
  MeasurementsPerBin := 0;
  MaxMeasurementsInBin := 0;
  MinMeasurementsInBin := 0;
  BinsWithMaxMeasurements := 0;
  BinsWithMinMeasurements := 0;
  MaxMeasurementsInBin_Percentage := 0;
  Mean := 0;
  StdDev := 0;
end;


end.

