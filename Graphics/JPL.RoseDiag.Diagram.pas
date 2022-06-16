unit JPL.RoseDiag.Diagram;

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

{$IFDEF MSWINDOWS}
uses
  Classes, SysUtils, Graphics,
  GdiPlus, GdiPlusHelpers,
  JPL.RoseDiag.BinDataList,
  JPL.TStr, JPL.Math, JPL.Conversion, JPL.Colors, JPL.PixelConv;
{$ENDIF} // MSWINDOWS

const
  RD_DEFAULT_CLASS_SIZE = 10;

{$IFDEF MSWINDOWS}
  RD_MIN_DIAGRAM_WIDTH_MM = 10;
  RD_MAX_DIAGRAM_WIDTH_MM = 1000;
  RD_MIN_DIAGRAM_MARGIN_MM = 0;
  RD_MAX_DIAGRAM_MARGIN_MM = 100;
  RD_MIN_LINE_WIDTH = 0.1;
  RD_MAX_LINE_WIDTH = 10.0;
  RD_MAX_CIRCLES_COUNT = 90;
  RD_MAX_FONT_SIZE = 40;
  RD_MAX_POS = 500;
  RD_MAX_TEXT_LEN = 500;
  RD_MIN_MARKER_WIDTH = 1;
  RD_MAX_MARKER_WIDTH = 20;

  DEG_SIGN = '°';

  // Integer divisors of 360 (except 360)
  // NOTE: Być może lepiej zastosować dzielniki całkowite 90
  ClassSizeArray: array[0..22] of Byte =
  (
    1, 2, 3, 4, 5, 6, 8, 9,
    10, 12, 15, 18,
    20, 24,
    30, 36, 40, 45, 60, 72, 90,
    120, 180
  );



type

  Int100 = 0..100;

  TRoseDiagramType = (rdtRose, rdtPolygon);
  TMeasurementType = (mt360, mt180); // mt360 - azimuths 0-360 (full circle), mt180 - linear data 0-180 (half cicle or full if CentralSymmetry = True)
  TAxesCaptionType = (actDegrees, actSymbols); // actDegrees - 0,90,180,270; actSymbols - N,E,S,W

  TRoseDiagramFill = class
  private
    FColor: TColor;
    FHatchColor: TColor;
    FHatchStyle: TGPHatchStyle;
    FSolidFill: Boolean;
    FTransparency: Int100;
  public
    procedure SetProperties(const AColor: TColor; const ATransparency: Int100; const AHatchStyle: TGPHatchStyle; const AHatchColor: TColor;
      const ASolidFill: Boolean);

    property Color: TColor read FColor write FColor; // Hatch background color or solid color if SolidFill = True
    property Transparency: Int100 read FTransparency write FTransparency; // Transparency in percent
    property HatchStyle: TGPHatchStyle read FHatchStyle write FHatchStyle; // Ignored if SolidFill = True
    property HatchColor: TColor read FHatchColor write FHatchColor; // Kolor kreskowania (szrafury). Ignored if SolidFill = True
    property SolidFill: Boolean read FSolidFill write FSolidFill; // Solid fill (no hatch)
  end;

  TRoseDiagramLine = class
  private
    FColor: TColor;
    FStyle: TPenStyle;
    FTransparency: Int100;
    FVisible: Boolean;
    FWidth: Single;
  public
    procedure SetProperties(const AVisible: Boolean; const AColor: TColor; const ATransparency: Int100; const AWidth: Single;
      const AStyle: TPenStyle);

    property Visible: Boolean read FVisible write FVisible;
    property Color: TColor read FColor write FColor;
    property Transparency: Int100 read FTransparency write FTransparency;
    property Width: Single read FWidth write FWidth; // Pen width
    property Style: TPenStyle read FStyle write FStyle;
  end;

  TRoseDiagramFont = class
  private
    FColor: TColor;
    FFontName: string;
    FSize: Byte;
    FStyle: TGPFontStyle;
    FTransparency: Int100;
    procedure SetFontName(AValue: string);
  public
    procedure SetProperties(const AFontName: string; const ASize: Byte; const AStyle: TGPFontStyle; const AColor: TColor;
      const ATransparency: Int100);
    procedure Assign(AFont: TRoseDiagramFont);

    property FontName: string read FFontName write SetFontName;
    property Size: Byte read FSize write FSize; // Size in points
    property Color: TColor read FColor write FColor;
    property Style: TGPFontStyle read FStyle write FStyle;
    property Transparency: Int100 read FTransparency write FTransparency;
  end;

  TRoseDiagramText = class
  private
    FFont: TRoseDiagramFont;
    FVisible: Boolean;
    FPosX: Single;
    FPosY: Single;
    FText: string;
    procedure SetFont(AValue: TRoseDiagramFont);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetProperties(const AVisible: Boolean; const AText, AFontName: string; const AFontSizePt: Byte; const AFontStyle: TGPFontStyle;
      const AFontColor: TColor; const APosX, APosY: Single; const AFontTransparency: Int100);

    property Visible: Boolean read FVisible write FVisible;
    property Text: string read FText write FText;
    property Font: TRoseDiagramFont read FFont write SetFont;
    property PosX: Single read FPosX write FPosX;
    property PosY: Single read FPosY write FPosY;
  end;

  TRoseDiagramAxisMarker = class
  private
    FVisible: Boolean;
    FFont: TRoseDiagramFont;
    FMarkerColor: TColor;
    FMarkerTransparency: Int100;
    FMarkerWidth: Byte;
    procedure SetFont(AValue: TRoseDiagramFont);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetProperties(const AVisible: Boolean; const AFontName: string; const AFontSizePt: Byte; const AFontStyle: TGPFontStyle;
      const AFontColor: TColor; const AFontTransparency: Int100; const AMarkerColor: TColor; const AMarkerWidth: Byte;
      const AMarkerTransparency: Int100);

    property Visible: Boolean read FVisible write FVisible;
    property Font: TRoseDiagramFont read FFont write SetFont;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property MarkerWidth: Byte read FMarkerWidth write FMarkerWidth;
    property MarkerTransparency: Int100 read FMarkerTransparency write FMarkerTransparency;
  end;

  TRoseDiagramAxes = class
  private
    FCaptionType: TAxesCaptionType;
    FLine: TRoseDiagramLine;
    FPercentageMarkers: TRoseDiagramAxisMarker;
    FText: TRoseDiagramText;
    procedure SetLine(AValue: TRoseDiagramLine);
    procedure SetPercentageMarkers(AValue: TRoseDiagramAxisMarker);
    procedure SetText(AValue: TRoseDiagramText);
  public
    constructor Create;
    destructor Destroy; override;

    property Line: TRoseDiagramLine read FLine write SetLine;
    property Text: TRoseDiagramText read FText write SetText;
    property PercentageMarkers: TRoseDiagramAxisMarker read FPercentageMarkers write SetPercentageMarkers;
    property CaptionType: TAxesCaptionType read FCaptionType write FCaptionType;
  end;

  TRoseDiagramMetadata = class
  private
    FAuthor: string;
    FDescription: string;
    FSubject: string;
  public
    procedure SetProperties(const ASubject, AAuthor, ADescription: string);

    property Subject: string read FSubject write FSubject;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
  end;


  TRoseDiagram = class
  private
    FAxes: TRoseDiagramAxes;
    FCentralSymmetry: Boolean;
    FDefaultMonoFontName: string;
    FDefaultFontSize: Byte;
    FDefaultFontColor: TColor;
    FBackground: TRoseDiagramFill;
    FCircles: TRoseDiagramLine;
    FCirclesCount: Byte;
    FDescription: TRoseDiagramText;
    FDrawInternalPolygonLines: Boolean;
    FMeasurementType: TMeasurementType;
    FDiagramType: TRoseDiagramType;
    FFrame: TRoseDiagramLine;
    FMarginMM: integer;
    FMetadata: TRoseDiagramMetadata;
    FPieFill: TRoseDiagramFill;
    FPieLine: TRoseDiagramLine;
    FRadii: TRoseDiagramLine;
    FRadiusMM: integer;
    FSelectedBinFill: TRoseDiagramFill;
    FSelectedBinLine: TRoseDiagramLine;
    FBinDataList: TBinDataList;
    FTitle: TRoseDiagramText;
    function GetClassSize: Byte;
    procedure SetAxes(AValue: TRoseDiagramAxes);
    procedure SetCentralSymmetry(AValue: Boolean);
    procedure SetBackground(AValue: TRoseDiagramFill);
    procedure SetCircles(AValue: TRoseDiagramLine);
    procedure SetCirclesCount(AValue: Byte);
    procedure SetClassSize(AValue: Byte);
    procedure SetDescription(AValue: TRoseDiagramText);
    procedure SetFrame(AValue: TRoseDiagramLine);
    procedure SetMarginMM(AValue: integer);
    procedure SetMetadata(AValue: TRoseDiagramMetadata);
    procedure SetPieFill(AValue: TRoseDiagramFill);
    procedure SetPieLine(AValue: TRoseDiagramLine);
    procedure SetRadii(AValue: TRoseDiagramLine);
    procedure SetRadiusMM(AValue: integer);
    procedure SetSelectedBinFill(AValue: TRoseDiagramFill);
    procedure SetSelectedBinLine(AValue: TRoseDiagramLine);
    procedure SetTitle(AValue: TRoseDiagramText);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetToDefautParams;

    //////////// The main proc. //////////////////////////////
    procedure PaintOnCanvas(Canvas: TCanvas);
    //////////////////////////////////////////////////////////

    property ClassSize: Byte read GetClassSize write SetClassSize;
    property BinDataList: TBinDataList read FBinDataList write FBinDataList;
    property CentralSymmetry: Boolean read FCentralSymmetry write SetCentralSymmetry;

    property DiagramType: TRoseDiagramType read FDiagramType write FDiagramType; // Rose or polygon
    property DrawInternalPolygonLines: Boolean read FDrawInternalPolygonLines write FDrawInternalPolygonLines;
    property MeasurementType: TMeasurementType read FMeasurementType write FMeasurementType;

    property Background: TRoseDiagramFill read FBackground write SetBackground;
    property Frame: TRoseDiagramLine read FFrame write SetFrame;
    property Circles: TRoseDiagramLine read FCircles write SetCircles;
    property CirclesCount: Byte read FCirclesCount write SetCirclesCount;
    property Radii: TRoseDiagramLine read FRadii write SetRadii;

    property PieLine: TRoseDiagramLine read FPieLine write SetPieLine;
    property PieFill: TRoseDiagramFill read FPieFill write SetPieFill;

    property Axes: TRoseDiagramAxes read FAxes write SetAxes;

    property SelectedBinLine: TRoseDiagramLine read FSelectedBinLine write SetSelectedBinLine;
    property SelectedBinFill: TRoseDiagramFill read FSelectedBinFill write SetSelectedBinFill;

    property Title: TRoseDiagramText read FTitle write SetTitle;
    property Description: TRoseDiagramText read FDescription write SetDescription;

    property RadiusMM: integer read FRadiusMM write SetRadiusMM; // Radius of the diagram [millimeters]
    property MarginMM: integer read FMarginMM write SetMarginMM; // The space between the border (frame) and the diagram [millimeters]

    property Metadata: TRoseDiagramMetadata read FMetadata write SetMetadata;
  end;



{$region ' helpers '}
function TranspToAlpha(const TransparencyInPercent: Byte): Byte;
procedure CheckGPFontName(var AUserFontName: string; bMonospace: Boolean);
function RoseDiagramTypeToStrID(const DiagramType: TRoseDiagramType): string;
function StrIDToRoseDiagramType(StrID: string; Default: TRoseDiagramType = rdtRose): TRoseDiagramType;
function RoseMeasurementTypeToStrID(const mt: TMeasurementType): string;
function StrIDToRoseMeasurementType(StrID: string; Default: TMeasurementType = mt360): TMeasurementType;
function AxesCaptionTypeToStrID(const CaptionType: TAxesCaptionType): string;
function StrIDToAxesCaptionType(StrID: string; Default: TAxesCaptionType = actDegrees): TAxesCaptionType;
{$endregion helpers}


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

{$region ' helpers '}
function TranspToAlpha(const TransparencyInPercent: Byte): Byte;
begin
  Result := TransparencyToAlpha(TransparencyInPercent);
end;

procedure CheckGPFontName(var AUserFontName: string; bMonospace: Boolean);
begin
  // W GDI+ próba zastosowania nieistniejącej czcionki kończy się błędem (exception).
  // Dlatego należy się upewnić, że do funkcji GDI+ trafi nazwa istniejącej czcionki.
  if bMonospace then
    AUserFontName := string(GetGPFontName([UnicodeString(AUserFontName), 'Fira Mono', 'Roboto Mono', 'Consolas', 'Courier New', 'Tahoma']))
    // Tahoma - not monospaced but "safe" font
  else
    AUserFontName := string(GetGPFontName([UnicodeString(AUserFontName), 'Segoe UI', 'Tahoma']));
end;

function RoseDiagramTypeToStrID(const DiagramType: TRoseDiagramType): string;
begin
  if DiagramType = rdtPolygon then Result := 'polygon' else Result := 'rose';
end;

function StrIDToRoseDiagramType(StrID: string; Default: TRoseDiagramType = rdtRose): TRoseDiagramType;
begin
  StrID := TStr.TrimAndLow(StrID);
  StrID := TSTr.TrimFromStart(StrID, 'rdt');
  if StrID = 'rose' then Result := rdtRose
  else if StrID = 'polygon' then Result := rdtPolygon
  else Result := Default;
end;

function RoseMeasurementTypeToStrID(const mt: TMeasurementType): string;
begin
  if mt = mt360 then Result := 'azimuths' else Result := 'linear';
end;

function StrIDToRoseMeasurementType(StrID: string; Default: TMeasurementType): TMeasurementType;
begin
  StrID := TStr.TrimAndLow(StrID);
  StrID := TStr.TrimFromStart(StrID, 'mt');
  StrID := TStr.ReplaceFirst(StrID, '360', 'azimuths');
  StrID := TStr.ReplaceFirst(StrID, '180', 'linear');
  if StrID = 'azimuths' then Result := mt360
  else if StrID = 'linear' then Result := mt180
  else Result := Default;
end;

function AxesCaptionTypeToStrID(const CaptionType: TAxesCaptionType): string;
begin
  case CaptionType of
    actDegrees: Result := 'degrees';
    actSymbols: Result := 'symbols';
  else
    Result := '';
  end;
end;

function StrIDToAxesCaptionType(StrID: string; Default: TAxesCaptionType = actDegrees): TAxesCaptionType;
begin
  StrID := TStr.TrimAndLow(StrID);
  if StrID = 'degrees' then Result := actDegrees
  else if StrID = 'symbols' then Result := actSymbols
  else Result := Default;
end;

{$endregion helpers}


{$region ' Create & Destroy '}
constructor TRoseDiagram.Create;
begin
  inherited;

  FDiagramType := rdtRose;
  FDrawInternalPolygonLines := True;
  FMeasurementType := mt360;

  FBackground := TRoseDiagramFill.Create;
  FFrame := TRoseDiagramLine.Create;
  FCircles := TRoseDiagramLine.Create;
  FRadii := TRoseDiagramLine.Create;
  FPieLine := TRoseDiagramLine.Create;
  FPieFill := TRoseDiagramFill.Create;
  FAxes := TRoseDiagramAxes.Create;
  FSelectedBinLine := TRoseDiagramLine.Create;
  FSelectedBinFill := TRoseDiagramFill.Create;
  FTitle := TRoseDiagramText.Create;
  FDescription := TRoseDiagramText.Create;

  FBinDataList := TBinDataList.Create(10);
  FMetadata := TRoseDiagramMetadata.Create;

  FDefaultMonoFontName := string(GetGPFontName(['Fira Mono', 'Roboto Mono', 'Consolas', 'Courier New', 'Tahoma']));
  FDefaultFontSize := 9;
  FDefaultFontColor := clBlack;

  ResetToDefautParams;
end;

destructor TRoseDiagram.Destroy;
begin
  FBackground.Free;
  FFrame.Free;
  FCircles.Free;
  FRadii.Free;
  FPieLine.Free;
  FPieFill.Free;
  FAxes.Free;
  FSelectedBinLine.Free;
  FSelectedBinFill.Free;
  FTitle.Free;
  FDescription.Free;

  FBinDataList.ClearParams;
  FBinDataList.Free;

  FMetadata.Free;

  inherited Destroy;
end;
{$endregion Create & Destroy}

procedure TRoseDiagram.ResetToDefautParams;
begin
  RadiusMM := 100;
  MarginMM := 5;

  FBackground.SetProperties(clWhite, 0, HatchStyleDashedVertical, clWhite, True);
  FFrame.SetProperties(False, clBlack, 0, 1, psSolid);
  FCircles.SetProperties(True, clSilver, 0, 1, psDash);
  FCirclesCount := 9;
  FRadii.SetProperties(True, clSilver, 0, 1, psDash);

  FPieLine.SetProperties(True, 16748574, 0, 1.0, psSolid);
  FPieFill.SetProperties(16436871, 40, HatchStyle30Percent, 16748574, False);

  FAxes.Line.SetProperties(True, clBlack, 0, 1, psSolid);
  FAxes.Text.SetProperties(True, '', 'Verdana', 9, [], FDefaultFontColor, 0, 0, 0);
  FAxes.PercentageMarkers.SetProperties(True, 'Verdana', 9, [], clBlack, 0, clBlack, 5, 0);
  FAxes.CaptionType := actDegrees;

  FSelectedBinLine.SetProperties(True, clRed, 0, 1.0, psSolid);
  FSelectedBinFill.SetProperties(RGB(255,160,122), 80, HatchStyle30Percent, clRed, True);

  FTitle.SetProperties(True, '', 'Verdana', 12, [FontStyleBold], clBlack, 3, 3, 0);
  FDescription.SetProperties(True, '', 'Segoe UI', 10, [], clBlack, 3, 10, 0);

  FMetadata.Subject := '';
  FMetadata.Author := '';
  FMetadata.Description := '';
end;

{$region '   PaintOnCanvas   '}
procedure TRoseDiagram.PaintOnCanvas(Canvas: TCanvas);
var
  PixelConv: IPixelConv;
  BinDataList: TBinDataList;
  b180, b360, bSymm: Boolean;
  gr: IGPGraphics;
  Pen: IGPPen;
  Brush: IGPBrush;
  xWidth, xHeight, halfW, halfH, Margin, dx, dxm: Single;
  i: integer;
  RectF: TGPRectF;
  FirstCircleRadius, CircleRadius: Single;
  AngleStart, AngleSweep: Single;

  Points: array of TGPPointF;
  APoint: TGPPointF;

  // variables used in drawing the radii
  //    px - X coordinate, py - Y coordinate
  //    AlphaAngle - angle: 90 - (i * ClassSize)
  px, py, AlphaAngle, radius: Single;

  FontFamily: IGPFontFamily;
  AFont: IGPFont;
  Text: string;
  TextWidth, TextHeight: Single;

  BinData: TBinData;

  RMaxPix, RBinPix: Single;
  fi: Single;

  //Arr: TMeasurementArray;


  function MmToPix(const xMillimeters: Single): Single;
  begin
    Result := PixelConv.MmToPixelsX(xMillimeters);
  end;

begin
  if FRadiusMM <= 2 then Exit;
  BinDataList := FBinDataList;

  b360 := FMeasurementType = mt360;
  b180 := not b360;
  bSymm := FCentralSymmetry and b180;

  PixelConv := TPixelConv.Create(Canvas);
  //TPixelConv.Init(Canvas); // important!

  Margin := MmToPix(FMarginMM);

  if b360 or bSymm then
  begin
    xWidth := 2 * MmToPix(FRadiusMM) + (2 * Margin);
    xHeight := xWidth;
  end
  else
  // 180
  begin
    xWidth := MmToPix(FRadiusMM) + 2 * Margin;
    xHeight := 2 * MmToPix(FRadiusMM) + (2 * Margin);
  end;

  halfW := xWidth / 2;
  halfH := xHeight / 2;


  gr := TGPGraphics.Create(Canvas.Handle);
  gr.SmoothingMode := SmoothingModeHighQuality;
  gr.PixelOffsetMode := PixelOffsetModeHighQuality;


  {$region ' Background '}
  Brush := TGPSolidBrush.Create(GPColor(FBackground.Color, TranspToAlpha(FBackground.Transparency)));
  RectF.Initialize(0, 0, xWidth, xHeight);
  gr.FillRectangle(Brush, RectF);
  {$endregion}


  {$region ' Frame '}
  if FFRame.Visible then
  begin
    Pen := nil;
    Pen := TGPPen.Create(GPColor(FFrame.Color, TranspToAlpha(FFrame.Transparency)), FFRame.Width);
    SetGPPenStyle(Pen, FFrame.Style);

    // A slight correction is needed for the frame to display properly
    dx := 0.5;
    RectF.Initialize(dx, dx, xWidth - (2 * dx), xHeight - (2 * dx));

    gr.DrawRectangle(Pen, RectF);
  end;
  {$endregion}



  // Transformation of the coordinate system
  if b360 or bSymm then gr.TranslateTransform(halfW, halfH) // shifting the origin of the coordinate system to the center
  else gr.TranslateTransform(Margin, halfH);

  gr.ScaleTransform(1, -1); // change of the Y axis direction (down to up)


  {$region ' Circles '}
  if (FCircles.Visible) and (FCirclesCount > 0) then
  begin
    Pen := nil;
    Pen := TGPPen.Create(GPColor(FCircles.Color, TranspToAlpha(FCircles.Transparency)), FCircles.Width);
    SetGPPenStyle(Pen, FCircles.Style);
    Pen.StartCap := LineCapFlat;
    Pen.EndCap := LineCapFlat;

    if b360 or bSymm then FirstCircleRadius := (halfW - Margin) / FCirclesCount
    else FirstCircleRadius := (xWidth - 2 * Margin) / FCirclesCount;

    for i := 1 to FCirclesCount do
    begin
      CircleRadius := i * FirstCircleRadius;
      RectF.X := -CircleRadius;
      RectF.Y := -CircleRadius;
      RectF.Width := 2 * CircleRadius;
      RectF.Height := 2 * CircleRadius;
      if b360 or bSymm then gr.DrawEllipse(Pen, RectF) // full circle
      else {if b180 then} gr.DrawPie(Pen, RectF, -90, 180); // half circle // dla linii biegu
    end;
  end;
  {$endregion}


  {$region ' Radii '}
  if FRadii.Visible then
  begin

    Pen := nil;
    Pen := TGPPen.Create(GPColor(FRadii.Color, TranspToAlpha(FRadii.Transparency)), FRadii.Width);
    SetGPPenStyle(Pen, FRadii.Style);
    Pen.StartCap := LineCapFlat;
    Pen.EndCap := LineCapFlat;

    if b360 or bSymm then radius := halfW - Margin
    else radius := xWidth - 2 * Margin;

    for i := 1 to BinDataList.Count do
    begin
      BinData := BinDataList[i - 1];
      AlphaAngle := 90 - (i * BinDataList.ClassSize);

      px := radius * CosDeg(AlphaAngle);
      py := radius * SinDeg(AlphaAngle);
      gr.DrawLine(Pen, 0, 0, px, py);

      // Kreślenie promieni w ćwiartce II i IV dla symetrycznych diagramów biegu
      if bSymm then
      begin
        px := radius * CosDeg(AlphaAngle + 180);
        py := radius * SinDeg(AlphaAngle + 180);
        gr.DrawLine(Pen, 0, 0, px, py);
      end;
    end;

  end;
  {$endregion}


  {$region ' Pies / Polygon'}
  if (BinDataList.Count > 0) and (BinDataList.Stats.MaxMeasurementsInBin > 0) then
  begin

    gr.ResetTransform;
    if b360 or bSymm then gr.TranslateTransform(halfW, halfH)
    else gr.TranslateTransform(Margin, halfH);

    //gr.ScaleTransform(1, -1);
    gr.RotateTransform(-90); // 0 degrees at the top

    Pen := nil;
    Pen := TGPPen.Create(GPColor(FPieLine.Color, TranspToAlpha(FPieLine.Transparency)), FPieLine.Width);
    SetGPPenStyle(Pen, FPieLine.Style);

    Brush := nil;
    if FPieFill.SolidFill then
      Brush := TGPSolidBrush.Create(GPColor(FPieFill.Color, TranspToAlpha(FPieFill.Transparency)))
    else
      Brush := TGPHatchBrush.Create(
        FPieFill.FHatchStyle,
        GPColor(FPieFill.HatchColor, TranspToAlpha(FPieFill.Transparency)),
        GPColor(FPieFill.Color, TranspToAlpha(FPieFill.Transparency))
      );


    {$region ' Pies - "classic" rose diagram (wedges) '}
    if FDiagramType = rdtRose then
      for i := 0 to BinDataList.Count - 1 do
      begin
        BinData := BinDataList[i];
        if b180 and (not bSymm) and (BinData.StartValue >= 180) then Continue;
        if BinData.MeasurementCount = 0 then Continue;

        if b360 or bSymm then dx := (halfW - Margin) * BinData.MeasurementCount / BinDataList.Stats.MaxMeasurementsInBin
        else dx := (xWidth - 2 * Margin) * BinData.MeasurementCount / BinDataList.Stats.MaxMeasurementsInBin;

        RectF.X := -dx;
        RectF.Y := -dx;
        RectF.Width := 2 * dx;
        RectF.Height := RectF.Width;
        AngleStart := BinData.StartValue;
        AngleSweep := BinDataList.ClassSize;

        gr.DrawPie(Pen, RectF, AngleStart, AngleSweep);
        gr.FillPie(Brush, RectF, AngleStart, AngleSweep);

        if bSymm then
        begin
          dx := (halfW - Margin) * BinData.MeasurementCount / BinDataList.Stats.MaxMeasurementsInBin;

          RectF.X := -dx;
          RectF.Y := -dx;
          RectF.Width := 2 * dx;
          RectF.Height := RectF.Width;
          AngleStart := BinData.StartValue;
          AngleSweep := BinDataList.ClassSize;

          gr.DrawPie(Pen, RectF, AngleStart - 180, AngleSweep);
          gr.FillPie(Brush, RectF, AngleStart - 180, AngleSweep);
        end;

      end;
    {$endregion Pies}



    {$region ' Polygon '}
    if FDiagramType = rdtPolygon then
    begin
      gr.ResetTransform;
      if b360 or bSymm then gr.TranslateTransform(halfW, halfH)
      else gr.TranslateTransform(Margin, halfH);
      gr.ScaleTransform(1, -1);

      SetLength(Points{%H-}, 0);
      if b360 or bSymm then RMaxPix := halfW - Margin
      else RMaxPix := xWidth - 2 * Margin;


      // Tworzenie tablicy punktów wielokąta (polygon)
      // Create an array with a list of polygon points
      for i := 0 to BinDataList.Count - 1 do
      begin
        BinData := BinDataList[i];
        if b180 and (not bSymm) and (BinData.StartValue >= 180) then Continue;

        if BinData.MeasurementCount = 0 then
        begin
          SetLength(Points, Length(Points) + 1);
          Points[High(Points)] := GPPointF(0, 0);
          Continue;
        end;

        // I use the polar coordinate system with the fi angle.
        // Calculation of Cartesian coordinates: px and py.
        RBinPix := RMaxPix * BinData.MeasurementCount / BinDataList.Stats.MaxMeasurementsInBin;
        fi := 90 - BinData.EndValue + (ClassSize / 2);
        px := RBinPix * CosDeg(fi);
        py := RBinPix * SinDeg(fi);

        // If there is no measurement in the first interval (empty), it adds a point (0,0) to the beginning of the Points table.
        // Thanks to this, the polygon will be "hooked" to the origin of the coordinate system.
        if b180 {and (not bSymm)} and (i = 0) and (BinData.MeasurementCount > 0) then
        begin
          SetLength(Points, Length(Points) + 1);
          Points[High(Points)] := GPPointF(0, 0);
        end;

        SetLength(Points, Length(Points) + 1);
        Points[High(Points)] := GPPointF(px, py);
      end;

      gr.FillPolygon(Brush, Points);
      gr.DrawPolygon(Pen, Points);

      if FDrawInternalPolygonLines then
      // The inner lines of the polygon
      // It is important not to plot the lines already drawn by calling gr.DrawPolygon earlier.
      // If this happens, the non-solid lines (dashed, dotted, etc.) will overlap and "form" a solid line.
        for i := 0 to High(Points) do
        begin
          APoint := Points[i];
          if (APoint.X = 0) and (APoint.Y = 0) then Continue;

          if i > 0 then
            if (Points[i - 1].X = 0) and (Points[i - 1].Y = 0) then Continue;

          if i < High(Points) then
            if (Points[i + 1].X = 0) and (Points[i + 1].Y = 0) then Continue;

          gr.DrawLine(Pen, GPPointF(0, 0), APoint);
        end;



      if bSymm then
      begin

        SetLength(Points, 0);
        for i := 0 to BinDataList.Count - 1 do
        begin
          BinData := BinDataList[i];

          if BinData.MeasurementCount = 0 then
          begin
            SetLength(Points, Length(Points) + 1);
            Points[High(Points)] := GPPointF(0, 0);
            Continue;
          end;

          RBinPix := RMaxPix * BinData.MeasurementCount / BinDataList.Stats.MaxMeasurementsInBin;
          fi := 270 - BinData.EndValue + (ClassSize / 2);
          px := RBinPix * CosDeg(fi);
          py := RBinPix * SinDeg(fi);

          if (i = 0) and (BinData.MeasurementCount > 0) then
          begin
            SetLength(Points, Length(Points) + 1);
            Points[High(Points)] := GPPointF(0, 0);
          end;

          SetLength(Points, Length(Points) + 1);
          Points[High(Points)] := GPPointF(px, py);
        end;

        gr.FillPolygon(Brush, Points);
        gr.DrawPolygon(Pen, Points);

        if FDrawInternalPolygonLines then
          for i := 0 to High(Points) do
          begin
            APoint := Points[i];
            if (APoint.X = 0) and (APoint.Y = 0) then Continue;

            if i > 0 then
              if (Points[i - 1].X = 0) and (Points[i - 1].Y = 0) then Continue;

            if i < High(Points) then
              if (Points[i + 1].X = 0) and (Points[i + 1].Y = 0) then Continue;

            gr.DrawLine(Pen, GPPointF(0, 0), APoint);
          end;

      end; // bSymm


    end;
    {$endregion Polygon}

  end;
  {$endregion Pies / Polygon}


  {$region ' Selected bins '}
  if BinDataList.SelectedBinsCount > 0 then
  begin
    gr.ResetTransform;
    if b360 or bSymm then gr.TranslateTransform(halfW, halfH)
    else gr.TranslateTransform(Margin, halfH);
    gr.RotateTransform(-90);

    Pen := nil;
    Pen := TGPPen.Create(GPColor(FSelectedBinLine.Color, TranspToAlpha(FSelectedBinLine.Transparency)), FSelectedBinLine.Width);
    SetGPPenStyle(Pen, FSelectedBinLine.Style);
    if b360 or bSymm then dx := halfW - Margin
    else dx := xWidth - 2 * Margin;

    Brush := nil;
    if FSelectedBinFill.SolidFill then
      Brush := TGPSolidBrush.Create(GPColor(FSelectedBinFill.Color, TranspToAlpha(FSelectedBinFill.Transparency)))
    else
      Brush := TGPHatchBrush.Create(
        FSelectedBinFill.FHatchStyle,
        GPColor(FSelectedBinFill.HatchColor, TranspToAlpha(FSelectedBinFill.Transparency)),
        GPColor(FSelectedBinFill.Color, TranspToAlpha(FSelectedBinFill.Transparency))
      );

    for i := 0 to BinDataList.Count - 1 do
    begin
      BinData := BinDataList[i];
      if b180 and (not bSymm) and (BinData.StartValue >= 180) then Continue;
      if not BinData.Selected then Continue;

      RectF.X := -dx;
      RectF.Y := -dx;
      RectF.Width := 2 * dx;
      RectF.Height := RectF.Width;
      AngleStart := BinData.StartValue;
      AngleSweep := BinDataList.ClassSize;

      gr.DrawPie(Pen, RectF, AngleStart, AngleSweep);
      gr.FillPie(Brush, RectF, AngleStart, AngleSweep);
    end;
  end;
  {$endregion Selected bins}


  {$region ' Axes '}
  gr.ResetTransform;

  {$region ' Axis X and Y - Lines '}
  if FAxes.Line.Visible then
  begin
    Pen := nil;
    Pen := TGPPen.Create(GPColor(FAxes.Line.Color, TranspToAlpha(FAxes.Line.Transparency)), FAxes.Line.Width);
    SetGPPenStyle(Pen, FAxes.Line.Style);
    Pen.StartCap := LineCapFlat;
    Pen.EndCap := LineCapArrowAnchor;

    if b360 or bSymm then
    begin
      gr.DrawLine(Pen, 0, halfH, xWidth, halfH);  // X
      gr.DrawLine(Pen, halfW, xHeight, halfW, 0); // Y
    end
    else
    begin
      gr.DrawLine(Pen, 0, halfH, xWidth, halfH);  // X
      gr.DrawLine(Pen, Margin, xHeight, Margin, 0); // Y
    end;
  end;
  {$endregion Axis X and Y - Lines}


  {$region ' Axis X and Y - Captions '}
  if FAxes.Text.Visible then
  begin
    FontFamily := nil;
    AFont := nil;
    Brush := nil;

    try
      FontFamily := TGPFontFamily.Create(UnicodeString(FAxes.Text.Font.FontName));
    except
      FontFamily := TGPFontFamily.Create(UnicodeString('Tahoma'));
    end;

    AFont := TGPFont.Create(FontFamily, FAxes.Text.Font.Size, FAxes.Text.Font.Style, TGPUnit.UnitPoint);
    Brush := TGPSolidBrush.Create(GPColor(FAxes.Text.Font.Color, TranspToAlpha(FAxes.Text.Font.Transparency)));


    if FAxes.CaptionType = actSymbols then Text := 'N' else Text := '0' + DEG_SIGN;
    //px := halfW;
    if FAxes.Line.Visible then
    begin
      if b360 or bSymm then px := halfW + 1
      else px := Margin + 1;
    end
    else
    begin
      TextWidth := GPTextWidthF(gr, UnicodeString(Text), AFont);
      if b360 or bSymm then px := halfW - (TextWidth / 2)
      else px := Margin - (TextWidth / 2);
    end;
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(px, 1), Brush);


    if FAxes.CaptionType = actSymbols then Text := 'E' else Text := '90' + DEG_SIGN;
    TextWidth := GPTextWidthF(gr, UnicodeString(Text), AFont);
    TextHeight := GPTextHeightF(gr, UnicodeString(Text), AFont);
    if FAxes.Line.Visible then py := halfH - TextHeight
    else py := halfH - (TextHeight / 2);
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(xWidth - TextWidth - 1, py), Brush);


    if FAxes.CaptionType = actSymbols then Text := 'S' else Text := '180' + DEG_SIGN;
    TextHeight := GPTextHeightF(gr, UnicodeString(Text), AFont);
    if FAxes.Line.Visible then
    begin
      if b360 or bSymm then px := halfW + 1
      else px := Margin + 1;
    end
    else
    begin
      TextWidth := GPTextWidthF(gr, UnicodeString(Text), AFont);
      if b360 or bSymm then px := halfW - (TextWidth / 2)
      else px := Margin - (TextWidth / 2);
    end;
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(px, xHeight - TextHeight - 1), Brush);

    if b360 or bSymm then
    begin
      if FAxes.CaptionType = actSymbols then Text := 'W' else Text := '270' + DEG_SIGN;
      TextHeight := GPTextHeightF(gr, UnicodeString(Text), AFont);
      if FAxes.Line.Visible then py := halfH - TextHeight
      else py := halfH - (TextHeight / 2);
      gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(1, py), Brush);
    end;
  end;
  {$endregion Axis X and Y - Captions}


  {$region ' Axis X - Percentage markers (ticks) '}
  if FAxes.PercentageMarkers.Visible then
    if FBinDataList.Stats.MaxMeasurementsInBin_Percentage > 0 then
    begin

      FontFamily := nil;
      AFont := nil;
      Brush := nil;
      Pen := nil;

      try
        FontFamily := TGPFontFamily.Create(UnicodeString(FAxes.PercentageMarkers.Font.FontName));
      except
        FontFamily := TGPFontFamily.Create(UnicodeString('Tahoma'));
      end;

      AFont := TGPFont.Create(FontFamily, FAxes.PercentageMarkers.Font.Size, FAxes.PercentageMarkers.Font.Style, TGPUnit.UnitPoint);
      Brush := TGPSolidBrush.Create(GPColor(FAxes.PercentageMarkers.Font.Color, TranspToAlpha(FAxes.PercentageMarkers.Font.Transparency)));

      Pen := TGPPen.Create(GPColor(FAxes.PercentageMarkers.MarkerColor, TranspToAlpha(FAxes.PercentageMarkers.MarkerTransparency)), 1.5);
      SetGPPenStyle(Pen, psSolid);
      Pen.StartCap := LineCapFlat;
      Pen.EndCap := LineCapFlat;

      py := halfH;
      CircleRadius := MmToPix(RadiusMM);
      dx := CircleRadius / FBinDataList.Stats.MaxMeasurementsInBin_Percentage; // dx = 1 percent in pix
      dxm := FAxes.PercentageMarkers.MarkerWidth / 2; // połowa długości znacznika


      // px - współrz. X-owa znacznika
      // py - współrz. Y-owa znacznika

      if b360 or bSymm then px := HalfW else px := Margin;
      for i := 1 to 100 do
      begin
        if i > FBinDataList.Stats.MaxMeasurementsInBin_Percentage then Break;
        py := py - dx;
        gr.DrawLine(Pen, px - dxm, py, px + dxm, py);
        Text := itos(i) + '%';
        TextHeight := GPTextHeightF(gr, UnicodeString(Text), AFont);
        gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(px + dxm + 1, py - (TextHeight / 2)), Brush);
      end;

    end;
  {$endregion Axis X - Percentage markers (ticks)}

  {$endregion Axes}


  {$region ' Title & description '}

  // Title
  if FTitle.Visible and (FTitle.Text <> '') then
  begin
    gr.ResetTransform;

    FontFamily := nil;
    FontFamily := TGPFontFamily.Create(GetGPFontName([UnicodeString(FTitle.Font.FontName), 'Segoe UI', 'Tahoma']));
    AFont := nil;
    AFont := TGPFont.Create(FontFamily, FTitle.Font.Size, FTitle.Font.Style, TGPUnit.UnitPoint);
    Brush := nil;
    Brush := TGPSolidBrush.Create(GPColor(FTitle.Font.Color, TranspToAlpha(FTitle.Font.Transparency)));
    Text := FTitle.Text;
    px := MmToPix(FTitle.PosX);
    py := MmToPix(FTitle.PosY);
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(px, py), Brush);
  end;

  // Description
  if FDescription.Visible and (FDescription.Text <> '') then
  begin
    gr.ResetTransform;

    FontFamily := nil;
    FontFamily := TGPFontFamily.Create(GetGPFontName([UnicodeString(FDescription.Font.FontName), 'Segoe UI', 'Tahoma']));
    AFont := nil;
    AFont := TGPFont.Create(FontFamily, FDescription.Font.Size, FDescription.Font.Style, TGPUnit.UnitPoint);
    Brush := nil;
    Brush := TGPSolidBrush.Create(GPColor(FDescription.Font.Color, TranspToAlpha(FDescription.Font.Transparency)));
    Text := FDescription.Text;
    px := MmToPix(FDescription.PosX);
    py := MmToPix(FDescription.PosY);
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(px, py), Brush);
  end;

  {$endregion Title & description}


end;
{$endregion PaintOnCanvas}

procedure TRoseDiagram.SetBackground(AValue: TRoseDiagramFill);
begin
  if FBackground = AValue then Exit;
  FBackground := AValue;
end;

procedure TRoseDiagram.SetCircles(AValue: TRoseDiagramLine);
begin
  if FCircles = AValue then Exit;
  FCircles := AValue;
end;

procedure TRoseDiagram.SetCirclesCount(AValue: Byte);
begin
  if FCirclesCount = AValue then Exit;
  FCirclesCount := AValue;
end;

procedure TRoseDiagram.SetClassSize(AValue: Byte);
begin
  if FBinDataList.ClassSize = AValue then Exit;
  FBinDataList.ClassSize := AValue;
end;

procedure TRoseDiagram.SetDescription(AValue: TRoseDiagramText);
begin
  if FDescription = AValue then Exit;
  FDescription := AValue;
end;

procedure TRoseDiagram.SetFrame(AValue: TRoseDiagramLine);
begin
  if FFrame = AValue then Exit;
  FFrame := AValue;
end;

function TRoseDiagram.GetClassSize: Byte;
begin
  Result := FBinDataList.ClassSize;
end;

procedure TRoseDiagram.SetAxes(AValue: TRoseDiagramAxes);
begin
  if FAxes = AValue then Exit;
  FAxes := AValue;
end;

procedure TRoseDiagram.SetCentralSymmetry(AValue: Boolean);
begin
  if FCentralSymmetry = AValue then Exit;
  FCentralSymmetry := AValue;
end;

procedure TRoseDiagram.SetMarginMM(AValue: integer);
begin
  if FMarginMM = AValue then Exit;

  // The sum of the margins must be smaller than the width of the entire diagram.
  // Suma marginesów musi mniejsza od szerokości całego diagramu.
  if (AValue * 2) >= FRadiusMM then AValue := (FRadiusMM div 2) - 1;

  FMarginMM := AValue;
end;

procedure TRoseDiagram.SetMetadata(AValue: TRoseDiagramMetadata);
begin
  if FMetadata = AValue then Exit;
  FMetadata := AValue;
end;

procedure TRoseDiagram.SetPieFill(AValue: TRoseDiagramFill);
begin
  if FPieFill = AValue then Exit;
  FPieFill := AValue;
end;

procedure TRoseDiagram.SetPieLine(AValue: TRoseDiagramLine);
begin
  if FPieLine = AValue then Exit;
  FPieLine := AValue;
end;

procedure TRoseDiagram.SetRadii(AValue: TRoseDiagramLine);
begin
  if FRadii = AValue then Exit;
  FRadii := AValue;
end;

procedure TRoseDiagram.SetRadiusMM(AValue: integer);
begin
  if AValue <= 0 then AValue := 10; // must be greather than 0
  if FRadiusMM = AValue then Exit;
  FRadiusMM := AValue;
end;

procedure TRoseDiagram.SetSelectedBinFill(AValue: TRoseDiagramFill);
begin
  if FSelectedBinFill = AValue then Exit;
  FSelectedBinFill := AValue;
end;

procedure TRoseDiagram.SetSelectedBinLine(AValue: TRoseDiagramLine);
begin
  if FSelectedBinLine = AValue then Exit;
  FSelectedBinLine := AValue;
end;

procedure TRoseDiagram.SetTitle(AValue: TRoseDiagramText);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
end;



{ TRoseDiagramLine }

procedure TRoseDiagramLine.SetProperties
  (const AVisible: Boolean; const AColor: TColor; const ATransparency: Int100; const AWidth: Single; const AStyle: TPenStyle);
begin
  FVisible := AVisible;
  FColor := AColor;
  FTransparency := ATransparency;
  FWidth := AWidth;
  FStyle := AStyle;
end;



{ TRoseDiagramFill }

procedure TRoseDiagramFill.SetProperties
  (const AColor: TColor; const ATransparency: Int100; const AHatchStyle: TGPHatchStyle; const AHatchColor: TColor; const ASolidFill: Boolean);
begin
  FColor := AColor;
  FTransparency := ATransparency;
  FHatchStyle := AHatchStyle;
  FHatchColor := AHatchColor;
  FSolidFill := ASolidFill;
end;



{ TRoseDiagramFont }

procedure TRoseDiagramFont.SetProperties(const AFontName: string; const ASize: Byte; const AStyle: TGPFontStyle; const AColor: TColor;
  const ATransparency: Int100);
begin
  FFontName := AFontName;
  FSize := ASize;
  FStyle := AStyle;
  FColor := AColor;
  FTransparency := ATransparency;
end;

procedure TRoseDiagramFont.Assign(AFont: TRoseDiagramFont);
begin
  SetProperties(AFont.FontName, AFont.Size, AFont.Style, AFont.Color, AFont.Transparency);
end;

procedure TRoseDiagramFont.SetFontName(AValue: string);
begin
  if FFontName = AValue then Exit;
  CheckGPFontName(AValue, True); // check if font exists
  FFontName := AValue;
end;


{ TRoseDiagramText }

constructor TRoseDiagramText.Create;
begin
  inherited Create;
  FFont := TRoseDiagramFont.Create;
end;

destructor TRoseDiagramText.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TRoseDiagramText.SetProperties(const AVisible: Boolean; const AText, AFontName: string; const AFontSizePt: Byte;
  const AFontStyle: TGPFontStyle; const AFontColor: TColor; const APosX, APosY: Single; const AFontTransparency: Int100);
begin
  FVisible := AVisible;
  FText := AText;
  FFont.SetProperties(AFontName, AFontSizePt, AFontStyle, AFontColor, AFontTransparency);
  FPosX := APosX;
  FPosY := APosY;
end;

procedure TRoseDiagramText.SetFont(AValue: TRoseDiagramFont);
begin
  FFont.Assign(AValue);
end;



{ TRoseDiagramAxisMarker }

constructor TRoseDiagramAxisMarker.Create;
begin
  inherited Create;
  FFont := TRoseDiagramFont.Create;
end;

destructor TRoseDiagramAxisMarker.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TRoseDiagramAxisMarker.SetProperties(const AVisible: Boolean; const AFontName: string; const AFontSizePt: Byte;
  const AFontStyle: TGPFontStyle; const AFontColor: TColor; const AFontTransparency: Int100; const AMarkerColor: TColor; const AMarkerWidth: Byte;
  const AMarkerTransparency: Int100);
begin
 FVisible := AVisible;
 FFont.SetProperties(AFontName, AFontSizePt, AFontStyle, AFontColor, AFontTransparency);
 FMarkerColor := AMarkerColor;
 FMarkerWidth := AMarkerWidth;
 FMarkerTransparency := AMarkerTransparency;
end;

procedure TRoseDiagramAxisMarker.SetFont(AValue: TRoseDiagramFont);
begin
  FFont.Assign(AValue);
end;



{ TRoseDiagramAxes }

constructor TRoseDiagramAxes.Create;
begin
  inherited;
  FLine := TRoseDiagramLine.Create;
  FText := TRoseDiagramText.Create;
  FPercentageMarkers := TRoseDiagramAxisMarker.Create;
end;

destructor TRoseDiagramAxes.Destroy;
begin
  FLine.Free;
  FText.Free;
  FPercentageMarkers.Free;
  inherited Destroy;
end;

procedure TRoseDiagramAxes.SetLine(AValue: TRoseDiagramLine);
begin
  if FLine = AValue then Exit;
  FLine := AValue;
end;

procedure TRoseDiagramAxes.SetPercentageMarkers(AValue: TRoseDiagramAxisMarker);
begin
  if FPercentageMarkers = AValue then Exit;
  FPercentageMarkers := AValue;
end;

procedure TRoseDiagramAxes.SetText(AValue: TRoseDiagramText);
begin
  if FText = AValue then Exit;
  FText := AValue;
end;



{ TRoseDiagramMetadata }

procedure TRoseDiagramMetadata.SetProperties(const ASubject, AAuthor, ADescription: string);
begin
  FSubject := ASubject;
  FAuthor := AAuthor;
  FDescription := ADescription;
end;

{$ENDIF}


end.

