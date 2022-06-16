unit JPL.RoseDiag.DataFile;

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
{$ELSE}
  {$IFDEF DELPHIXE2_OR_BELOW}
  Unit for Delphi XE3 or newer!
  {$ENDIF}
{$ENDIF}

interface

{$IFDEF MSWINDOWS}

uses
  Classes, SysUtils, Graphics, Dialogs,
  GdiPlus, GdiPlusHelpers,
  JPL.Strings, JPL.Conversion, JPL.Colors, JPL.SimpleZip,
  JPL.RoseDiag.Diagram, JPL.RoseDiag.BinDataList,
  {$IFDEF FPC}
  fpjson, JPL.JsonHelpers
  {$ELSE}
  JsonDataObjects, //<-- https://github.com/ahausladen/JsonDataObjects
  JPL.JsonDataObjects
  {$ENDIF}
  ;


const
  RD_FORMAT_VERSION = '1.0';
  RDID_MAIN_OBJECT = 'RoseDiagram'; // Object

  RDID_INFO = 'Info'; // Object
  RDID_INFO_FORMAT_VERSION = 'FormatVersion'; // String
  RDID_INFO_APPLICATION = 'Application';

  RDID_METADATA = 'Metadata'; // Object
  RDID_METADATA_AUTHOR = 'Author'; // String
  RDID_METADATA_SUBJECT = 'Subject'; // String
  RDID_METADATA_DESCRIPTION = 'Description'; // String

  RDID_MAIN = 'Main'; // Object
  RDID_MAIN_RADIUS = 'Radius'; // Integer [mm]
  RDID_MAIN_MARGIN = 'Margin'; // Integer [mm]
  RDID_MAIN_CLASS_SIZE = 'ClassSize'; // Integer [degrees]
  RDID_MAIN_DIAGRAM_TYPE = 'DiagramType'; // String: rose, polygon
  RDID_MAIN_DRAW_POLYGON_INNER_LINES = 'DrawPolygonInnerLines'; // Boolean
  RDID_MAIN_MEASUREMENT_TYPE = 'MeasurementType'; // String: azimuths, linear (0-360, 0-180)
  RDID_MAIN_CENTRAL_SYMMETRY = 'CentralSymmetry'; // Boolean. Only for linear mesurements.
  RDID_MAIN_LINEAR_MEASUREMENTS_FIX_MODE = 'LinearMeasurementsFixMode'; // String: normalize_180, remove

  RDID_BACKGROUND = 'Background'; // Object
  RDID_BACKGROUND_COLOR = 'Color'; // String: HTML color
  RDID_BACKGROUND_TRANSPARENCY = 'Transparency'; // Integer: 0..100


  RDID_LINE_OBJ_NAME = 'Line';
  RDID_FONT_OBJ_NAME = 'Font';
  RDID_FILL_OBJ_NAME = 'Fill';
  RDID_TEXT_OBJ_NAME = 'Text';
  RDID_PERCENT_MARKERS_OBJ_NAME = 'PercentMarkers';

  RDID_LINE_VISIBLE = 'Visible'; // Boolean
  RDID_LINE_COLOR = 'Color'; // String: HTML color
  RDID_LINE_WIDTH = 'Width'; // Float
  RDID_LINE_STYLE = 'Style'; // String
  RDID_LINE_TRANSPARENCY = 'Transparency'; // Integer: 0..100

  RDID_FONT_NAME = 'Name';
  RDID_FONT_SIZE = 'Size';
  RDID_FONT_COLOR = 'Color';
  RDID_FONT_STYLE = 'Style';
  RDID_FONT_TRANSPARENCY = 'Transparency';

  RDID_FILL_BACKGROUND_COLOR = 'BackgroundColor'; // String: HTML color
  RDID_FILL_TRANSPARENCY = 'FillTransparency'; // Integer: 0..100
  RDID_FILL_SOLID = 'SolidFill'; // Boolean
  RDID_FILL_HATCH_STYLE = 'HatchStyle'; // String: hatch style ID
  RDID_FILL_HATCH_COLOR = 'HatchColor'; // String: HTML color

  RDID_TEXT_VISIBLE = 'Visible'; // Boolean
  RDID_TEXT_TEXT = 'Text'; // String
  RDID_TEXT_POSITION_X = 'PosX'; // Float
  RDID_TEXT_POSITION_Y = 'PosY'; // Float

  // Percentage markers (tick marks)
  RDID_MARKER_VISIBLE = 'Visible'; // Boolean
  RDID_MARKER_COLOR = 'Color'; // String: HTML color
  RDID_MARKER_WIDTH = 'Width'; // Byte: 3..20
  RDID_MARKER_TRANSPARENCY = 'Transparency'; // Integer: 0..100


  RDID_FRAME = 'Frame'; // Object
  RDID_CIRCLES = 'Circles'; // Object
  RDID_CIRCLES_COUNT = 'Count'; // Integer
  RDID_RADII = 'Radii'; // Object
  RDID_AXES = 'Axes'; // Object
  RDID_AXES_CAPTION_TYPE = 'CaptionType'; // String: degrees, symbols
  RDID_PIES = 'Pies'; // Object
  RDID_SELECTED_BINS = 'SelectedBins'; // Object
  RDID_TITLE = 'Title'; // Object
  RDID_DESCRIPTION = 'Description'; // Object
  RDID_MEASUREMENTS = 'Measurements'; // Array of float values


type

  TRoseDataFileOption = (rdfoAll, rdfoMain, rdfoInfo, rdfoMetadata, rdfoBackground, rdfoFrame, rdfoCircles, rdfoRadii,
    rdfoAxes, rdfoPies, rdfoSelectedBins, rdfoTitle, rdfoDescription, rdfoMeasurements);
  TRoseDataFileOptions = set of TRoseDataFileOption;

  // Record helpers for sets: Delphi XE3 or newer
  TRoseDataFileOptionsHelper = record helper for TRoseDataFileOptions
    function SetAll: TRoseDataFileOptions;
    function RemoveMeasurements: TRoseDataFileOptions;
  end;

  TRoseDataFile = class
  private
    FRoseDiagram: TRoseDiagram;
    FMeasurements: TMeasurementArray;
  public
    constructor Create(RoseDiagram: TRoseDiagram);
    destructor Destroy; override;

    procedure SaveToFile(const FileName: string; ApplicationName: string = ''; ZipCompress: Boolean = False); overload;
    procedure SaveToFile(const FileName: string; const Options: TRoseDataFileOptions; ApplicationName: string = ''; ZipCompress: Boolean = False); overload;
    procedure SaveToString(out JsonStr: string; const Options: TRoseDataFileOptions; ApplicationName: string = '');

    function LoadFromFile(const FileName: string; LoadMeasurements: Boolean): Boolean; overload;
    function LoadFromFile(const FileName: string; const Options: TRoseDataFileOptions): Boolean; overload;
    function LoadFromString(const JsonStr: string; const Options: TRoseDataFileOptions): Boolean;

    // Measurement list read from the file
    property Measurements: TMeasurementArray read FMeasurements;
  end;


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

{$region ' helpers '}
function PenStyleToStr(const PenStyle: TPenStyle): string;
begin
  case PenStyle of
    psSolid: Result := 'Solid';
    psDash: Result := 'Dash';
    psDot: Result := 'Dot';
    psDashDot: Result := 'DashDot';
    psDashDotDot: Result := 'DashDotDot';
  else
    Result := 'Solid';
  end;
end;

function StrToPenStyle(const PenStyleStr: string): TPenStyle;
var
  s: string;
begin
  s := Trim(LowerCase(PenStyleStr));
  if s = 'solid' then Result := psSolid
  else if s = 'dash' then Result := psDash
  else if s = 'dot' then Result := psDot
  else if s = 'dashdot' then Result := psDashDot
  else if s = 'dashdotdot' then Result := psDashDotDot
  else Result := psSolid;
end;
{$endregion helpers}




constructor TRoseDataFile.Create(RoseDiagram: TRoseDiagram);
begin
  inherited Create;
  FRoseDiagram := RoseDiagram;
end;

destructor TRoseDataFile.Destroy;
begin
  inherited Destroy;
end;

{$region '   Save to string / file   '}
procedure TRoseDataFile.SaveToString(out JsonStr: string; const Options: TRoseDataFileOptions; ApplicationName: string = '');
var
  joRoot, joMainObj, joInfo, joMetadata, joMain, joBackground, joFrame, joCircles, joRadii, joAxes, joPies, joSelectedBins,
  joTitle, joDescription: TJSONObject;
  jaM: TJSONArray;
  i: integer;
  ArrM: TMeasurementArray;
  bAll: Boolean;

  procedure WriteLineParams(jo: TJSONObject; const ID: string; Line: TRoseDiagramLine; WriteVisible: Boolean = True);
  var
    joLine: TJSONObject;
  begin
    joLine := TJSONObject.Create;
    if WriteVisible then joLine.WriteBool(RDID_LINE_VISIBLE, Line.Visible);
    joLine.WriteString(RDID_LINE_COLOR, ColorToHtmlColorStr(Line.Color));
    joLine.WriteFloat(RDID_LINE_WIDTH, Line.Width);
    joLine.WriteString(RDID_LINE_STYLE, PenStyleToStr(Line.Style));
    joLine.WriteInteger(RDID_LINE_TRANSPARENCY, Line.Transparency);
    jo.AddObject(ID, joLine, False);
  end;

  procedure WriteFillParams(jo: TJSONObject; const ID: string; Fill: TRoseDiagramFill);
  var
    joFill: TJSONObject;
  begin
    joFill := TJSONObject.Create;
    joFill.WriteString(RDID_FILL_BACKGROUND_COLOR, ColorToHtmlColorStr(Fill.Color));
    joFill.WriteInteger(RDID_FILL_TRANSPARENCY, Fill.Transparency);
    joFill.WriteBool(RDID_FILL_SOLID, Fill.SolidFill);
    joFill.WriteString(RDID_FILL_HATCH_STYLE, string(HatchStyleToStrID(Fill.HatchStyle)));
    joFill.WriteString(RDID_FILL_HATCH_COLOR, ColorToHtmlColorStr(Fill.HatchColor));
    jo.AddObject(ID, joFill, False);
  end;

  procedure WriteFontParams(jo: TJSONObject; const ID: string; Font: TRoseDiagramFont);
  var
    joFont: TJSONObject;
  begin
    joFont := TJSONObject.Create;
    joFont.WriteString(RDID_FONT_NAME, Font.FontName);
    joFont.WriteInteger(RDID_FONT_SIZE, Font.Size);
    joFont.WriteString(RDID_FONT_COLOR, ColorToHtmlColorStr(Font.Color));
    joFont.WriteString(RDID_FONT_STYLE, string(GPFontStyleToStr(Font.Style)));
    joFont.WriteInteger(RDID_FONT_TRANSPARENCY, Font.Transparency);
    jo.AddObject(ID, joFont, False);
  end;

  procedure WriteTextParams(jo: TJSONObject; const ID: string; Text: TRoseDiagramText; WriteText, WritePosition: Boolean);
  var
    joText: TJSONObject;
  begin
    joText := TJSONObject.Create;
    joText.WriteBool(RDID_TEXT_VISIBLE, Text.Visible);
    WriteFontParams(joText, RDID_FONT_OBJ_NAME, Text.Font);
    if WriteText then joText.WriteString(RDID_TEXT_TEXT, Text.Text);
    if WritePosition then
    begin
      joText.WriteFloat(RDID_TEXT_POSITION_X, Text.PosX);
      joText.WriteFloat(RDID_TEXT_POSITION_Y, Text.PosY);
    end;
    jo.AddObject(ID, joText, False);
  end;

  procedure WriteMarkerParams(jo: TJSONObject; const ID: string; Marker: TRoseDiagramAxisMarker);
  var
    joMarker: TJSONObject;
  begin
    joMarker := TJSONObject.Create;
    joMarker.WriteBool(RDID_MARKER_VISIBLE, Marker.Visible);
    WriteFontParams(joMarker, RDID_FONT_OBJ_NAME, Marker.Font);
    joMarker.WriteString(RDID_MARKER_COLOR, ColorToHtmlColorStr(Marker.MarkerColor));
    joMarker.WriteInteger(RDID_MARKER_WIDTH, Marker.MarkerWidth);
    joMarker.WriteInteger(RDID_MARKER_TRANSPARENCY, Marker.MarkerTransparency);
    jo.AddObject(ID, joMarker, False);
  end;

begin
  JsonStr := '';
  joRoot := TJSONObject.Create;
  joMainObj := TJSONObject.Create;
  joInfo := TJSONObject.Create;
  joMetadata := TJSONObject.Create;
  joMain := TJSONObject.Create;
  joBackground := TJSONObject.Create;
  joFrame := TJSONObject.Create;
  joCircles := TJSONObject.Create;
  joRadii := TJSONObject.Create;
  joAxes := TJSONObject.Create;
  joPies := TJSONObject.Create;
  joSelectedBins := TJSONObject.Create;
  joTitle := TJSONObject.Create;
  joDescription := TJSONObject.Create;
  jaM := TJSONArray.Create;
  try

    bAll := rdfoAll in Options;

    // Info
    if bAll or (rdfoInfo in Options) then
    begin
      joInfo.WriteString(RDID_INFO_FORMAT_VERSION, RD_FORMAT_VERSION);
      joInfo.WriteString(RDID_INFO_APPLICATION, ApplicationName);
      joMainObj.AddObject(RDID_INFO, joInfo);
    end;

    // Metadata
    if bAll or (rdfoMetadata in Options) then
    begin
      joMetadata.WriteString(RDID_METADATA_AUTHOR, FRoseDiagram.Metadata.Author);
      joMetadata.WriteString(RDID_METADATA_SUBJECT, FRoseDiagram.Metadata.Subject);
      joMetadata.WriteString(RDID_METADATA_DESCRIPTION, FRoseDiagram.Metadata.Description);
      joMainObj.AddObject(RDID_METADATA, joMetadata);
    end;

    // Main options
    if bAll or (rdfoMain in Options) then
    begin
      joMain.WriteInteger(RDID_MAIN_RADIUS, FRoseDiagram.RadiusMM);
      joMain.WriteInteger(RDID_MAIN_MARGIN, FRoseDiagram.MarginMM);
      joMain.WriteInteger(RDID_MAIN_CLASS_SIZE, FRoseDiagram.ClassSize);
      joMain.WriteString(RDID_MAIN_DIAGRAM_TYPE, RoseDiagramTypeToStrID(FRoseDiagram.DiagramType));
      joMain.WriteBool(RDID_MAIN_DRAW_POLYGON_INNER_LINES, FRoseDiagram.DrawInternalPolygonLines);
      joMain.WriteString(RDID_MAIN_MEASUREMENT_TYPE, RoseMeasurementTypeToStrID(FRoseDiagram.MeasurementType));
      joMain.WriteBool(RDID_MAIN_CENTRAL_SYMMETRY, FRoseDiagram.CentralSymmetry);
      joMain.WriteString(RDID_MAIN_LINEAR_MEASUREMENTS_FIX_MODE, LinearMeasurementFixModeToStrID(FRoseDiagram.BinDataList.LinearMeasurementFixMode));
      joMainObj.AddObject(RDID_MAIN, joMain);
    end;

    // Background
    if bAll or (rdfoBackground in Options) then
    begin
      joBackground.WriteString(RDID_BACKGROUND_COLOR, ColorToHtmlColorStr(FRoseDiagram.Background.Color));
      joBackground.WriteInteger(RDID_BACKGROUND_TRANSPARENCY, FRoseDiagram.Background.Transparency);
      joMainObj.AddObject(RDID_BACKGROUND, joBackground);
    end;

    // Frame
    if bAll or (rdfoFrame in Options) then
    begin
      WriteLineParams(joFrame, RDID_LINE_OBJ_NAME, FRoseDiagram.Frame, True);
      joMainObj.AddObject(RDID_FRAME, joFrame);
    end;

    // Circles
    if bAll or (rdfoCircles in Options) then
    begin
      joCircles.WriteInteger(RDID_CIRCLES_COUNT, FRoseDiagram.CirclesCount);
      WriteLineParams(joCircles, RDID_LINE_OBJ_NAME, FRoseDiagram.Circles, True);
      joMainObj.AddObject(RDID_CIRCLES, joCircles);
    end;

    // Radii
    if bAll or (rdfoRadii in Options) then
    begin
      WriteLineParams(joRadii, RDID_LINE_OBJ_NAME, FRoseDiagram.Radii, True);
      joMainObj.AddObject(RDID_RADII, joRadii);
    end;

    // Axes
    if bAll or (rdfoAxes in Options) then
    begin
      WriteLineParams(joAxes, RDID_LINE_OBJ_NAME, FRoseDiagram.Axes.Line, True);
      joAxes.WriteString(RDID_AXES_CAPTION_TYPE, AxesCaptionTypeToStrID(FRoseDiagram.Axes.CaptionType));
      WriteTextParams(joAxes, RDID_TEXT_OBJ_NAME, FRoseDiagram.Axes.Text, False, False);
      WriteMarkerParams(joAxes, RDID_PERCENT_MARKERS_OBJ_NAME, FRoseDiagram.Axes.PercentageMarkers);
      joMainObj.AddObject(RDID_AXES, joAxes);
    end;

    // Pies
    if bAll or (rdfoPies in Options) then
    begin
      WriteLineParams(joPies, RDID_LINE_OBJ_NAME, FRoseDiagram.PieLine, False);
      WriteFillParams(joPies, RDID_FILL_OBJ_NAME, FRoseDiagram.PieFill);
      joMainObj.AddObject(RDID_PIES, joPies);
    end;

    // Selected bins
    if bAll or (rdfoSelectedBins in Options) then
    begin
      WriteLineParams(joSelectedBins, RDID_LINE_OBJ_NAME, FRoseDiagram.SelectedBinLine, False);
      WriteFillParams(joSelectedBins, RDID_FILL_OBJ_NAME, FRoseDiagram.SelectedBinFill);
      joMainObj.AddObject(RDID_SELECTED_BINS, joSelectedBins);
    end;

    // Title
    if bAll or (rdfoTitle in Options) then
    begin
      WriteTextParams(joTitle, RDID_TEXT_OBJ_NAME, FRoseDiagram.Title, True, True);
      joMainObj.AddObject(RDID_TITLE, joTitle);
    end;

    // Description
    if bAll or (rdfoDescription in Options) then
    begin
      WriteTextParams(joDescription, RDID_TEXT_OBJ_NAME, FRoseDiagram.Description, True, True);
      joMainObj.AddObject(RDID_DESCRIPTION, joDescription);
    end;

    // Measurements
    if bAll or (rdfoMeasurements in Options) then
    begin
      ArrM := FRoseDiagram.BinDataList.Measurements;

      {$IFDEF FPC}
      for i := 0 to High(ArrM) do jaM.Add(ArrM[i]);
      {$ELSE}
      jaM.Count := Length(ArrM);
      for i := 0 to High(ArrM) do jaM.F[i] := ArrM[i];
      {$ENDIF}
      joMainObj.AddArray(RDID_MEASUREMENTS, jaM, True);
    end;


    joRoot.AddObject(RDID_MAIN_OBJECT, joMainObj);
    {$IFDEF FPC}
    JsonStr := joRoot.FormatJSON;
    {$ELSE}
    JsonStr := joRoot.ToJSON(False);
    {$ENDIF}


  finally
    joMainObj.Free;
    joInfo.Free;
    joMetadata.Free;
    joMain.Free;
    joBackground.Free;
    joFrame.Free;
    joCircles.Free;
    joRadii.Free;
    joAxes.Free;
    joPies.Free;
    joRoot.Free;
    joSelectedBins.Free;
    joTitle.Free;
    joDescription.Free;
    jaM.Free;
  end;
end;

procedure TRoseDataFile.SaveToFile(const FileName: string; const Options: TRoseDataFileOptions; ApplicationName: string = ''; ZipCompress: Boolean = False);
var
  JsonStr: string;
begin
  SaveToString(JsonStr, Options, ApplicationName);
  if ZipCompress then SaveStringToZipFile(FileName, JsonStr, BaseFileName(FileName) + '.roz', 'Rose diagram')
  else SaveStringToFile(FileName, JsonStr, TEncoding.UTF8, False);
end;

procedure TRoseDataFile.SaveToFile(const FileName: string; ApplicationName: string = ''; ZipCompress: Boolean = False);
var
  Options: TRoseDataFileOptions;
begin
  Options := {%H-}Options.SetAll;
  SaveToFile(FileName, Options, ApplicationName, ZipCompress);
end;
{$endregion Save to string / file}


{$region '   Load from string / file   '}

function TRoseDataFile.LoadFromString(const JsonStr: string; const Options: TRoseDataFileOptions): Boolean;
var
  joRoot, joMainObj, joInfo, joMetadata, joMain, joBackground, joFrame, joCircles, joRadii, joAxes, joPies, joSelectedBins,
  joTitle, joDescription: TJSONObject;
  jaM: TJSONArray;
  s: string;
  i, x: integer;
  cl: TColor;
  xf: Single;
  FixMode: TLinearMeasurementFixMode;
  bAll: Boolean;


  procedure ReadLineParams(jo: TJSONObject; const ID: string; Line: TRoseDiagramLine; ReadVisible: Boolean = True);
  var
    joLine: TJSONObject;
  begin
    if not jo.TryGetObject(ID, joLine) then Exit;

    // Visible
    if ReadVisible then Line.Visible := joLine.ReadBool(RDID_LINE_VISIBLE, Line.Visible);

    // Color
    s := joLine.ReadString(RDID_LINE_COLOR, ColorToHtmlColorStr(Line.Color));
    if TryHtmlStrToColor(s, cl) then Line.Color := cl;

    // Width
    xf := joLine.ReadFloat(RDID_LINE_WIDTH, Line.Width);
    xf := GetFloatInRange(xf, RD_MIN_LINE_WIDTH, RD_MAX_LINE_WIDTH);
    Line.Width := xf;

    // Style
    s := joLine.ReadString(RDID_LINE_STYLE, PenStyleToStr(Line.Style));
    Line.Style := StrToPenStyle(s);

    // Transparency
    x := joLine.ReadInteger(RDID_LINE_TRANSPARENCY, Line.Transparency);
    x := GetIntInRange(x, 0, 100);
    Line.Transparency := x;
  end;

  procedure ReadFillParams(jo: TJSONObject; const ID: string; Fill: TRoseDiagramFill);
  var
    joFill: TJSONObject;
    hs: TGPHatchStyle;
  begin
    if not jo.TryGetObject(ID, joFill) then Exit;

    // Background color
    s := joFill.ReadString(RDID_FILL_BACKGROUND_COLOR, ColorToHtmlColorStr(Fill.Color));
    if TryHtmlStrToColor(s, cl) then Fill.Color := cl;

    // Fill transparency
    x := joFill.ReadInteger(RDID_FILL_TRANSPARENCY, Fill.Transparency);
    x := GetIntInRange(x, 0, 100);
    Fill.Transparency := x;

    // Solid fill
    Fill.SolidFill := joFill.ReadBool(RDID_FILL_SOLID, Fill.SolidFill);

    // Hatch style
    s := joFill.ReadString(RDID_FILL_HATCH_STYLE, string(HatchStyleToStrID(Fill.HatchStyle)));
    if TryStrIDToHatchStyle(UnicodeString(s), hs{%H-}) then Fill.HatchStyle := hs;

    // Hatch color
    s := joFill.ReadString(RDID_FILL_HATCH_COLOR, ColorToHtmlColorStr(Fill.HatchColor));
    if TryHtmlStrToColor(s, cl) then Fill.HatchColor := cl;
  end;

  procedure ReadFontParams(jo: TJSONObject; const ID: string; Font: TRoseDiagramFont);
  var
    joFont: TJSONObject;
  begin
    if not jo.TryGetObject(ID, joFont) then Exit;

    // Font name
    s := joFont.ReadString(RDID_FONT_NAME, Font.FontName);
    CheckGPFontName(s, False);
    Font.FontName := s;

    // Font size
    x := joFont.ReadInteger(RDID_FONT_SIZE, Font.Size);
    x := GetIntInRange(x, 1, RD_MAX_FONT_SIZE);
    Font.Size := x;

    // Font color
    s := joFont.ReadString(RDID_FONT_COLOR, ColorToHtmlColorStr(Font.Color));
    if TryHtmlStrToColor(s, cl) then Font.Color := cl;

    // Font style
    s := joFont.ReadString(RDID_FONT_STYLE, string(GPFontStyleToStr(Font.Style)));
    Font.Style := StrToGPFontStyle(UnicodeString(s));

    // Font transparency
    x := joFont.ReadInteger(RDID_FONT_TRANSPARENCY, Font.Transparency);
    x := GetIntInRange(x, 0, 100);
    Font.Transparency := x;
  end;

  procedure ReadTextParams(jo: TJSONObject; const ID: string; Text: TRoseDiagramText; ReadText, ReadPosition: Boolean);
  var
    joText: TJSONObject;
  begin
    if not jo.TryGetObject(ID, joText) then Exit;

    // Visible
    Text.Visible := joText.ReadBool(RDID_TEXT_VISIBLE, Text.Visible);

    // Font
    ReadFontParams(joText, RDID_FONT_OBJ_NAME, Text.Font);

    if ReadText then
    begin
      s := joText.ReadString(RDID_TEXT_OBJ_NAME, Text.Text);
      if Length(s) > RD_MAX_TEXT_LEN then s := Copy(s, 1, RD_MAX_TEXT_LEN);
      Text.Text := s;
    end;

    if ReadPosition then
    begin
      // PosX
      xf := joText.ReadFloat(RDID_TEXT_POSITION_X, Text.PosX);
      xf := GetFloatInRange(xf, 0, RD_MAX_POS);
      Text.PosX := xf;

      // PosY
      xf := joText.ReadFloat(RDID_TEXT_POSITION_Y, Text.PosY);
      xf := GetFloatInRange(xf, 0, RD_MAX_POS);
      Text.PosY := xf;
    end;
  end;

  procedure ReadMarkerParams(jo: TJSONObject; const ID: string; Marker: TRoseDiagramAxisMarker);
  var
    joMarker: TJSONObject;
  begin
    if not jo.TryGetObject(ID, joMarker) then Exit;

    // Visible
    Marker.Visible := joMarker.ReadBool(RDID_MARKER_VISIBLE, Marker.Visible);

    // Font
    ReadFontParams(joMarker, RDID_FONT_OBJ_NAME, Marker.Font);

    // Marker color
    s := joMarker.ReadString(RDID_MARKER_COLOR, ColorToHtmlColorStr(Marker.MarkerColor));
    if TryHtmlStrToColor(s, cl) then Marker.MarkerColor := cl;

    // Marker width (length)
    x := joMarker.ReadInteger(RDID_MARKER_WIDTH, Marker.MarkerWidth);
    x := GetIntInRange(x, RD_MIN_MARKER_WIDTH, RD_MAX_MARKER_WIDTH);
    Marker.MarkerWidth := x;

    // Marker transparency
    x := joMarker.ReadInteger(RDID_MARKER_TRANSPARENCY, Marker.MarkerTransparency);
    x := GetIntInRange(x, 0, 100);
    Marker.MarkerTransparency := x;
  end;

  function ImportFromRozeta2DataStr: Boolean;
  var
    sl: TStringList;
    i: integer;
  begin
    FRoseDiagram.BinDataList.ClearParams;
    SetLength(FMeasurements, 0);

    sl := TStringList.Create;
    try
      sl.Text := JsonStr;
      for i := 0 to sl.Count - 1 do
      begin
        s := Trim(sl[i]);
        if s = '' then Continue;
        if Copy(s, 1, 1) = ';' then Continue;
        s := TrimFromCharPosToEnd(s, '/');
        if not TryStoF(s, xf) then Continue;
        SetLength(FMeasurements, Length(FMeasurements) + 1);
        FMeasurements[High(FMeasurements)] := xf;
      end;
    finally
      sl.Free;
    end;

    FRoseDiagram.BinDataList.ProcessMeasurementArray(FMeasurements);
    Result := Length(FMeasurements) > 0;
  end;

begin
  Result := False;
  bAll := rdfoAll in Options;

  try
    joRoot := GetJSONObjectFromStr(JsonStr, False);
  except
    //on E: Exception do
    //  raise Exception.Create('Invalid data file: "' + ExpandFileName(FileName) + '"' + ENDL + ENDL + E.Message);

    Result := ImportFromRozeta2DataStr;
    Exit;
  end;

  if not Assigned(joRoot) then Exit;

  try

    if not joRoot.TryGetObject(RDID_MAIN_OBJECT, joMainObj) then Exit;

    {$region ' Info '}
    if bAll or (rdfoInfo in Options) then
    begin
      if joMainObj.TryGetObject(RDID_INFO, joInfo) then
      begin
        s := joInfo.ReadString(RDID_INFO_FORMAT_VERSION, '');
        if s <> RD_FORMAT_VERSION then
        begin
          Exception.Create('Invalid version number in JSON data: ' + s + ENDL + 'Expected: ' + RD_FORMAT_VERSION);
          Exit;
        end;
      end;
    end;
    {$endregion Info}


    {$region ' Metadata '}
    if bAll or (rdfoMetadata in Options) then
    begin
      if joMainObj.TryGetObject(RDID_METADATA, joMetadata) then
      begin
        FRoseDiagram.Metadata.Subject := joMetadata.ReadString(RDID_METADATA_SUBJECT, FRoseDiagram.Metadata.Subject);
        FRoseDiagram.Metadata.Author := joMetadata.ReadString(RDID_METADATA_AUTHOR, FRoseDiagram.Metadata.Author);
        FRoseDiagram.Metadata.Description := joMetadata.ReadString(RDID_METADATA_DESCRIPTION, FRoseDiagram.Metadata.Description);
      end;
    end;
    {$endregion Metadata}


    {$region ' Main options '}
    if bAll or (rdfoMain in Options) then
    begin
      if joMainObj.TryGetObject(RDID_MAIN, joMain) then
      begin
        // Radius [mm]
        x := joMain.ReadInteger(RDID_MAIN_RADIUS, FRoseDiagram.RadiusMM);
        x := GetIntInRange(x, RD_MIN_DIAGRAM_WIDTH_MM, RD_MAX_DIAGRAM_WIDTH_MM);
        FRoseDiagram.RadiusMM := x;

        // Margin [mm]
        x := joMain.ReadInteger(RDID_MAIN_MARGIN, FRoseDiagram.MarginMM);
        x := GetIntInRange(x, RD_MIN_DIAGRAM_MARGIN_MM, RD_MAX_DIAGRAM_MARGIN_MM);
        FRoseDiagram.MarginMM := x;

        // Class size
        x := joMain.ReadInteger(RDID_MAIN_CLASS_SIZE, FRoseDiagram.ClassSize);
        if not IsValidClassSize(x) then x := RD_DEFAULT_CLASS_SIZE;
        FRoseDiagram.ClassSize := x;

        // Diagram type
        s := joMain.ReadString(RDID_MAIN_DIAGRAM_TYPE, RoseDiagramTypeToStrID(FRoseDiagram.DiagramType));
        FRoseDiagram.DiagramType := StrIDToRoseDiagramType(s);

        // Draw polygon inner lines
        FRoseDiagram.DrawInternalPolygonLines := joMain.ReadBool(RDID_MAIN_DRAW_POLYGON_INNER_LINES, FRoseDiagram.DrawInternalPolygonLines);

        // Measurement type
        s := joMain.ReadString(RDID_MAIN_MEASUREMENT_TYPE, RoseMeasurementTypeToStrID(FRoseDiagram.MeasurementType));
        FRoseDiagram.MeasurementType := StrIDToRoseMeasurementType(s);

        // Central symmetry
        FRoseDiagram.CentralSymmetry := joMain.ReadBool(RDID_MAIN_CENTRAL_SYMMETRY, FRoseDiagram.CentralSymmetry);

        // Linear measurements fix mode
        FixMode := FRoseDiagram.BinDataList.LinearMeasurementFixMode;
        s := joMain.ReadString(RDID_MAIN_LINEAR_MEASUREMENTS_FIX_MODE, LinearMeasurementFixModeToStrID(FixMode));
        FixMode := StrIDToLinearMeasurementFixMode(s, FixMode);
        FRoseDiagram.BinDataList.LinearMeasurementFixMode := FixMode;

      end;
    end;
    {$endregion Size}


    {$region ' Background '}
    if bAll or (rdfoBackground in Options) then
    begin
      if joMainObj.TryGetObject(RDID_BACKGROUND, joBackground) then
      begin
        // Color
        s := joBackground.ReadString(RDID_BACKGROUND_COLOR, ColorToHtmlColorStr(FRoseDiagram.Background.Color));
        if TryHtmlStrToColor(s, cl) then FRoseDiagram.Background.Color := cl;

        // Transparency
        x := joBackground.ReadInteger(RDID_BACKGROUND_TRANSPARENCY, FRoseDiagram.Background.Transparency);
        x := GetIntInRange(x, 0, 100);
        FRoseDiagram.Background.Transparency := x;
      end;
    end;
    {$endregion Background}


    {$region ' Frame '}
    if bAll or (rdfoFrame in Options) then
    begin
      if joMainObj.TryGetObject(RDID_FRAME, joFrame) then ReadLineParams(joFrame, RDID_LINE_OBJ_NAME, FRoseDiagram.Frame, True);
    end;
    {$endregion Frame}


    {$region ' Circles '}
    if bAll or (rdfoCircles in Options) then
    begin
      if joMainObj.TryGetObject(RDID_CIRCLES, joCircles) then
      begin
        ReadLineParams(joCircles, RDID_LINE_OBJ_NAME, FRoseDiagram.Circles, True);

        // Count
        x := joCircles.ReadInteger(RDID_CIRCLES_COUNT, FRoseDiagram.CirclesCount);
        x := GetIntInRange(x, 1, RD_MAX_CIRCLES_COUNT);
        FRoseDiagram.CirclesCount := x;
      end;
    end;
    {$endregion Circles}


    {$region ' Radii '}
    if bAll or (rdfoRadii in Options) then
    begin
      if joMainObj.TryGetObject(RDID_RADII, joRadii) then ReadLineParams(joRadii, RDID_LINE_OBJ_NAME, FRoseDiagram.Radii, True);
    end;
    {$endregion Radii}


    {$region ' Axes '}
    if bAll or (rdfoAxes in Options) then
    begin
      if joMainObj.TryGetObject(RDID_AXES, joAxes) then
      begin
        ReadLineParams(joAxes, RDID_LINE_OBJ_NAME, FRoseDiagram.Axes.Line, True);
        ReadTextParams(joAxes, RDID_TEXT_OBJ_NAME, FRoseDiagram.Axes.Text, False, False);
        ReadMarkerParams(joAxes, RDID_PERCENT_MARKERS_OBJ_NAME, FRoseDiagram.Axes.PercentageMarkers);

        // Caption type
        s := joAxes.ReadString(RDID_AXES_CAPTION_TYPE, AxesCaptionTypeToStrID(FRoseDiagram.Axes.CaptionType));
        FRoseDiagram.Axes.CaptionType := StrIDToAxesCaptionType(s);
      end;
    end;
    {$endregion Axes}


    {$region ' Pies '}
    if bAll or (rdfoPies in Options) then
    begin
      if joMainObj.TryGetObject(RDID_PIES, joPies) then
      begin
        ReadLineParams(joPies, RDID_LINE_OBJ_NAME, FRoseDiagram.PieLine, False);
        ReadFillParams(joPies, RDID_FILL_OBJ_NAME, FRoseDiagram.PieFill);
      end;
    end;
    {$endregion Pies}


    {$region ' Selected bins '}
    if bAll or (rdfoSelectedBins in Options) then
    begin
      if joMainObj.TryGetObject(RDID_SELECTED_BINS, joSelectedBins) then
      begin
        ReadLineParams(joSelectedBins, RDID_LINE_OBJ_NAME, FRoseDiagram.SelectedBinLine, False);
        ReadFillParams(joSelectedBins, RDID_FILL_OBJ_NAME, FRoseDiagram.SelectedBinFill);
      end;
    end;
    {$endregion Selected bins}


    {$region ' Title '}
    if bAll or (rdfoTitle in Options) then
    begin
      if joMainObj.TryGetObject(RDID_TITLE, joTitle) then
        ReadTextParams(joTitle, RDID_TEXT_TEXT, FRoseDiagram.Title, True, True);
    end;
    {$endregion Title}


    {$region ' Description '}
    if bAll or (rdfoDescription in Options) then
    begin
      if joMainObj.TryGetObject(RDID_DESCRIPTION, joDescription) then
        ReadTextParams(joDescription, RDID_TEXT_TEXT, FRoseDiagram.Description, True, True);
    end;
    {$endregion Description}


    {$region ' Measurements '}
    if bAll or (rdfoMeasurements in Options) then
    begin
      FRoseDiagram.BinDataList.ClearParams;
      SetLength(FMeasurements{%H-}, 0);
      if joMainObj.TryGetArray(RDID_MEASUREMENTS, jaM) then
      begin

        for i := 0 to jaM.Count - 1 do
        begin
          {$IFDEF FPC}
          if not jaM[i].IsNumber then Continue;
          xf := jaM[i].AsFloat;
          {$ELSE}
          if not IsNumberDataType(jaM[i].Typ) then Continue;
          xf := jaM[i].FloatValue;
          {$ENDIF}
          SetLength(FMeasurements, Length(FMeasurements) + 1);
          FMeasurements[High(FMeasurements)] := xf;
        end;

        // FMeasurements stores all float values read from the file.
        // ProcessMeasurementArray can change / delete measurements during processing.

        FRoseDiagram.BinDataList.ProcessMeasurementArray(FMeasurements);
      end;
    end;
    {$endregion Measurements}


    Result := True;

  finally
    if Assigned(joRoot) then joRoot.Free;
  end;
end;


function TRoseDataFile.LoadFromFile(const FileName: string; const Options: TRoseDataFileOptions): Boolean;
var
  JsonStr: string;
begin
  Result := False;
  if IsZipFile(FileName) then
  begin
    JsonStr := GetStringFromZipFile(FileName, TEncoding.UTF8, '', 0);
    if JsonStr = '' then Exit;
  end
  else
    if not LoadStringFromFile(FileName, JsonStr{%H-}, TEncoding.UTF8, True) then Exit;

  Result := LoadFromString(JsonStr, Options);
end;


function TRoseDataFile.LoadFromFile(const FileName: string; LoadMeasurements: Boolean): Boolean;
var
  Options: TRoseDataFileOptions;
begin
  Options := {%H-}Options.SetAll;
  if not LoadMeasurements then Options := Options.RemoveMeasurements;
  Result := LoadFromFile(FileName, Options);
end;



{$endregion Load from string / file}



{ TRoseDataFileOptionsHelper }

function TRoseDataFileOptionsHelper.SetAll: TRoseDataFileOptions;
begin
  Result := [
    rdfoMain, rdfoInfo, rdfoMetadata, rdfoBackground, rdfoFrame, rdfoCircles, rdfoRadii,
    rdfoAxes, rdfoPies, rdfoSelectedBins, rdfoTitle, rdfoDescription, rdfoMeasurements
  ];
end;

function TRoseDataFileOptionsHelper.RemoveMeasurements: TRoseDataFileOptions;
begin
  Result := Self - [rdfoMeasurements];
end;


{$ENDIF}


end.

