unit JPL.MemIniFile;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp

  Last mod: 2020.03.14
 }

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$I JppFPC.inc}
{$ENDIF}

interface

uses 
  {$IFDEF DCC}
  Winapi.Windows,
  System.SysUtils, System.Classes, System.IniFiles, System.ZLib, Vcl.Graphics, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Forms,
  {$ELSE}
  SysUtils, Classes, IniFiles, Graphics, StdCtrls, Forms,
  {$ENDIF}
  JPL.Strings, JPL.Conversion, JPL.Math, JPL.Colors;


type

  TJppMemIniFile = class
  private const
    DEFAULT_SECTION = 'MAIN';
    COMPRESSED_VALUE_PREFIX = 'CBUF_';
  private
    FIni: TMemIniFile;
    FUpdateOnExit: Boolean;
    FLeftStringBound: string;
    FRightStringBound: string;
    FCurrentSection: string;
    FFileName: string;
    procedure SetUpdateOnExit(const Value: Boolean);
    procedure SetLeftStringBound(const Value: string);
    procedure SetRightStringBound(const Value: string);
    function GetCurrentSection: string;
    procedure SetCurrentSection(const Value: string);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    {$IFDEF DCC}
    function GetEncoding: TEncoding;
    procedure SetEncoding(const Value: TEncoding);
    {$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_INIFILE_WITH_ENCODING}
    function GetEncoding: TEncoding;
    {$ENDIF}{$ENDIF}
  public

    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; const AEncoding: TEncoding); overload;
    constructor Create(const AFileName: string; const AEncoding: TEncoding; AUpdateOnExit: Boolean); overload;
    destructor Destroy; override;

    procedure UpdateFile;

    procedure DeleteKey(const Section, Ident: string); overload;
    procedure DeleteKey(const Ident: string); overload;

    procedure Clear;
    procedure EraseSection(const Section: string);
    procedure GetStrings(const List: TStrings);
    procedure SetStrings(const List: TStrings);
    function SectionExists(const Section: string): Boolean;
    function ValueExists(const Section, Ident: string): Boolean; overload;
    function ValueExists(const Ident: string): Boolean; overload;
    procedure Rename(const FileName: string; Reload: Boolean);


    // ------------------------------- Read & Write -------------------------------

    procedure ReadSection(const Section: string; Strings: TStrings); // Reads key names from the given section
    procedure ReadSectionKeyNames(const Section: string; Strings: TStrings); // calls ReadSection

    procedure ReadSections(Strings: TStrings); // Reads names of all sections
    procedure ReadSectionNames(Strings: TStrings); // calls ReadSections

    procedure ReadSectionValues(const Section: string; Strings: TStrings); // Reads the values from all keys within the given section

    {$IFDEF DCC}
    procedure ReadSubSections(const Section: string; Strings: TStrings; Recurse: Boolean = False);
    {$ENDIF}


    function ReadBinaryStream(const Section, Ident: string; Value: TStream): integer;
    procedure WriteBinaryStream(const Section, Ident: string; Value: TStream);

    procedure WriteFormPos(const Section: string; Form: TForm); overload;
    procedure WriteFormPos(Form: TForm); overload;
    procedure ReadFormPos(const Section: string; Form: TForm); overload;
    procedure ReadFormPos(Form: TForm); overload;

    procedure WriteString(const Section, Ident, Value: string); overload;
    procedure WriteString(Section: string; Cmp: TComponent; const Value: string); overload;
    procedure WriteString(const Ident, Value: string); overload;
    procedure WriteString(Cmp: TComponent; const Value: string); overload;
    function ReadString(const Section, Ident, Default: string): string; overload;
    function ReadString(const Section: string; Cmp: TComponent; const Default: string): string; overload;
    function ReadString(const Ident, Default: string): string; overload;
    function ReadString(Cmp: TComponent; const Default: string): string; overload;

    procedure WriteBool(const Section, Ident: string; const Value: Boolean); overload;
    procedure WriteBool(const Ident: string; const Value: Boolean); overload;
    function ReadBool(const Section, Ident: string; const Default: Boolean): Boolean; overload;
    function ReadBool(const Ident: string; const Default: Boolean): Boolean; overload;

    procedure WriteInteger(const Section, Ident: string; const Value: integer); overload;
    procedure WriteInteger(const Section: string; Cmp: TComponent; const Value: integer); overload;
    procedure WriteInteger(const Ident: string; const Value: integer); overload;
    procedure WriteInteger(Cmp: TComponent; const Value: integer); overload;
    function ReadInteger(const Section, Ident: string; const Default: integer): integer; overload;
    function ReadInteger(const Section: string; Cmp: TComponent; const Default: integer): integer; overload;
    function ReadInteger(const Ident: string; const Default: integer): integer; overload;
    function ReadInteger(Cmp: TComponent; const Default: integer): integer; overload;

    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; overload;
    function ReadDate(const Ident: string; Default: TDateTime): TDateTime; overload;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); overload;
    procedure WriteDate(const Ident: string; Value: TDateTime); overload;

    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; overload;
    function ReadDateTime(const Ident: string; Default: TDateTime): TDateTime; overload;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); overload;
    procedure WriteDateTime(const Ident: string; Value: TDateTime); overload;

    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; overload;
    function ReadTime(const Ident: string; Default: TDateTime): TDateTime; overload;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); overload;
    procedure WriteTime(const Ident: string; Value: TDateTime); overload;

    function ReadFloat(const Section, Ident: string; Default: Double): Double; overload;
    function ReadFloat(const Ident: string; Default: Double): Double; overload;
    procedure WriteFloat(const Section, Ident: string; Value: Double); overload;
    procedure WriteFloat(const Ident: string; Value: Double); overload;

    procedure WriteColor(const Section: string; const Ident: string; const Color: TColor); overload;
    procedure WriteColor(const Ident: string; const Color: TColor); overload;
    function ReadColor(const Section: string; const Ident: string; const Default: TColor): TColor; overload;
    function ReadColor(const Ident: string; const Default: TColor): TColor; overload;

    procedure WriteHtmlColor(const Section: string; const Ident: string; const AColor: TColor); overload;
    procedure WriteHtmlColor(const Ident: string; const AColor: TColor); overload;
    function ReadHtmlColor(const Section, Ident: string; const Default: TColor): TColor; overload;
    function ReadHtmlColor(const Ident: string; const Default: TColor): TColor; overload;

    function ReadIntegerInRange(const Section, Ident: string; const Default, Min, Max: integer): integer; overload;
    function ReadIntegerInRange(const Ident: string; const Default, Min, Max: integer): integer; overload;

    procedure WriteFontStyles(const Section: string; const Ident: string; const FontStyles: TFontStyles); overload;
    procedure WriteFontStyles(const Ident: string; const FontStyles: TFontStyles); overload;
    function ReadFontStyles(const Section: string; const Ident: string; const Default: TFontStyles): TFontStyles; overload;
    function ReadFontStyles(const Ident: string; const Default: TFontStyles): TFontStyles; overload;

    // Warning! WriteStrings cleans the entire section and then saves the data.
    procedure WriteStrings(const Section: string; Items: TStrings; MaxItemsCount: integer = -1 {$IFDEF DCC}; Compress: Boolean = False{$ENDIF});
    procedure ReadStrings(const Section: string; Items: TStrings {$IFDEF DCC}; ItemsCompressed: Boolean = False{$ENDIF});

    procedure WriteBoundString(const Section, Ident, Value: string); overload;
    procedure WriteBoundString(const Section: string; Cmp: TComponent; const Value: string); overload;
    procedure WriteBoundString(const Ident, Value: string); overload;
    procedure WriteBoundString(Cmp: TComponent; const Value: string); overload;
    function ReadBoundString(const Section, Ident, Default: string): string; overload;
    function ReadBoundString(const Section: string; Cmp: TComponent; const Default: string): string; overload;
    function ReadBoundString(const Ident, Default: string): string; overload;
    function ReadBoundString(Cmp: TComponent; const Default: string): string; overload;

    {$IFDEF DCC}
    procedure WriteInt64(const Section, Ident: string; const Value: Int64); overload;
    procedure WriteInt64(const Ident: string; const Value: Int64); overload;
    function ReadInt64(const Section, Ident: string; const Default: Int64): Int64; overload;
    function ReadInt64(const Ident: string; const Default: Int64): Int64; overload;
    {$ENDIF}

    procedure WriteDotFloat(const Section, Ident: string; const Value: Double); overload;
    procedure WriteDotFloat(const Ident: string; const Value: Double); overload;
    function ReadDotFloat(const Section, Ident: string; const Default: Double): Double; overload;
    function ReadDotFloat(const Ident: string; const Default: Double): Double; overload;

    procedure WriteCheckBox(const Section: string; CheckBox: TCheckBox); overload;
    procedure WriteCheckBox(CheckBox: TCheckBox); overload;
    procedure ReadCheckBox(const Section: string; CheckBox: TCheckBox); overload;
    procedure ReadCheckBox(CheckBox: TCheckBox); overload;

    procedure WriteRadioButton(const Section: string; RadioButton: TRadioButton); overload;
    procedure WriteRadioButton(RadioButton: TRadioButton); overload;
    procedure ReadRadioButton(const Section: string; RadioButton: TRadioButton); overload;
    procedure ReadRadioButton(RadioButton: TRadioButton); overload;


    // ------------------------------ Properties ------------------------------

    property FileName: string read FFileName;
    property UpdateOnExit: Boolean read FUpdateOnExit write SetUpdateOnExit;
    property LeftStringBound: string read FLeftStringBound write SetLeftStringBound;
    property RightStringBound: string read FRightStringBound write SetRightStringBound;

    // Warning! If you want to use Write/Read routines without the Section param, you must set the CurrentSection property first!
    property CurrentSection: string read GetCurrentSection write SetCurrentSection;

    property Ini: TMemIniFile read FIni;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    {$IFDEF DCC}property Encoding: TEncoding read GetEncoding write SetEncoding;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_INIFILE_WITH_ENCODING}
    property Encoding: TEncoding read GetEncoding;
    {$ENDIF}{$ENDIF}
  end;

  
implementation


function FontStylesToStr(const FontStyles: TFontStyles): string;
var
  s: string;
begin
  s := '';
  if fsBold in FontStyles then s := 'Bold';
  if fsItalic in FontStyles then s := s + ',Italic';
  if fsUnderline in FontStyles then s := s + ',Underline';
  if fsStrikeOut in FontStyles then s := s + ',StrikeOut';
  if Copy(s, 1, 1) = ',' then Delete(s, 1, 1);
  Result := s;
end;

function StrToFontStyles(FontStylesStr: string): TFontStyles;
begin
  Result := [];
  FontStylesStr := UpperCase(FontStylesStr);
  if Pos('BOLD', FontStylesStr) > 0 then Result := Result + [fsBold];
  if Pos('ITALIC', FontStylesStr) > 0 then Result := Result + [fsItalic];
  if Pos('UNDERLINE', FontStylesStr) > 0 then Result := Result + [fsUnderline];
  if Pos('STRIKEOUT', FontStylesStr) > 0 then Result := Result + [fsStrikeOut];
end;

function PadLeft(const Text: string; const PadToLen: integer; PaddingChar: Char = ' '): string;
begin
  if Length(Text) < PadToLen then Result := StringOfChar(PaddingChar, PadToLen - Length(Text)) + Text
  else Result := Text;
end;



{$region '                              TJppMemIniFile                                   '}

constructor TJppMemIniFile.Create(const AFileName: string);
begin
  Create(AFileName, nil);
end;

constructor TJppMemIniFile.Create(const AFileName: string; const AEncoding: TEncoding);
begin
  Create(AFileName, AEncoding, False);
end;


constructor TJppMemIniFile.Create(const AFileName: string; const AEncoding: TEncoding; AUpdateOnExit: Boolean);
begin
  {$IFDEF DCC}FIni := TMemIniFile.Create(AFileName, AEncoding);{$ENDIF}
  {$IFDEF FPC}
    {$IFDEF HAS_INIFILE_WITH_ENCODING}
    FIni := TMemIniFile.Create(AFileName, AEncoding);
    {$ELSE}
    FIni := TMemIniFile.Create(AFileName);
    {$ENDIF}
  {$ENDIF}
  FUpdateOnExit := AUpdateOnExit;
  FFileName := AFileName;
  FLeftStringBound := '[';
  FRightStringBound := ']';
end;

destructor TJppMemIniFile.Destroy;
begin
  if FUpdateOnExit then UpdateFile;
  FIni.Free;
  inherited;
end;

procedure TJppMemIniFile.Clear;
begin
  FIni.Clear;
end;



function TJppMemIniFile.GetCurrentSection: string;
begin
  if Trim(FCurrentSection) = '' then FCurrentSection := DEFAULT_SECTION;
  Result := FCurrentSection;
end;



procedure TJppMemIniFile.SetCurrentSection(const Value: string);
begin
  FCurrentSection := Value;
end;

{$IFDEF DCC}
procedure TJppMemIniFile.SetEncoding(const Value: TEncoding);
begin
  FIni.Encoding := Value;
end;

function TJppMemIniFile.GetEncoding: TEncoding;
begin
  Result := FIni.Encoding;
end;
{$ENDIF}

{$IFDEF FPC}{$IFDEF HAS_INIFILE_WITH_ENCODING}
function TJppMemIniFile.GetEncoding: TEncoding;
begin
  Result := FIni.Encoding;
end;
{$ENDIF}{$ENDIF}

procedure TJppMemIniFile.SetLeftStringBound(const Value: string);
begin
  FLeftStringBound := Value;
end;

procedure TJppMemIniFile.SetRightStringBound(const Value: string);
begin
  FRightStringBound := Value;
end;

procedure TJppMemIniFile.SetUpdateOnExit(const Value: Boolean);
begin
  FUpdateOnExit := Value;
end;

procedure TJppMemIniFile.SetCaseSensitive(const Value: Boolean);
begin
  if FIni.CaseSensitive <> Value then FIni.CaseSensitive := Value;
end;

function TJppMemIniFile.GetCaseSensitive: Boolean;
begin
  Result := FIni.CaseSensitive;
end;



procedure TJppMemIniFile.UpdateFile;
begin
  FIni.UpdateFile;
end;

procedure TJppMemIniFile.DeleteKey(const Section, Ident: string);
begin
  FIni.DeleteKey(Section, Ident);
end;

procedure TJppMemIniFile.DeleteKey(const Ident: string);
begin
  DeleteKey(CurrentSection, Ident);
end;

procedure TJppMemIniFile.EraseSection(const Section: string);
begin
  FIni.EraseSection(Section);
end;

procedure TJppMemIniFile.GetStrings(const List: TStrings);
begin
  FIni.GetStrings(List);
end;

procedure TJppMemIniFile.SetStrings(const List: TStrings);
begin
  FIni.SetStrings(List);
end;

function TJppMemIniFile.SectionExists(const Section: string): Boolean;
begin
  Result := FIni.SectionExists(Section);
end;

function TJppMemIniFile.ValueExists(const Section, Ident: string): Boolean;
begin
  Result := FIni.ValueExists(Section, Ident);
end;

function TJppMemIniFile.ValueExists(const Ident: string): Boolean;
begin
  Result := ValueExists(CurrentSection, Ident);
end;

procedure TJppMemIniFile.Rename(const FileName: string; Reload: Boolean);
begin
  FIni.Rename(FileName, Reload);
end;






procedure TJppMemIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  FIni.ReadSection(Section, Strings);
end;

procedure TJppMemIniFile.ReadSectionKeyNames(const Section: string; Strings: TStrings);
begin
  ReadSection(Section, Strings);
end;


procedure TJppMemIniFile.ReadSections(Strings: TStrings);
begin
  FIni.ReadSections(Strings);
end;

procedure TJppMemIniFile.ReadSectionNames(Strings: TStrings);
begin
  ReadSections(Strings);
end;


procedure TJppMemIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin
  FIni.ReadSectionValues(Section, Strings);
end;

{$IFDEF DCC}
procedure TJppMemIniFile.ReadSubSections(const Section: string; Strings: TStrings; Recurse: Boolean);
begin
  FIni.ReadSubSections(Section, Strings, Recurse);
end;
{$ENDIF}


function TJppMemIniFile.ReadBinaryStream(const Section, Ident: string; Value: TStream): integer;
begin
  Result := FIni.ReadBinaryStream(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteBinaryStream(const Section, Ident: string; Value: TStream);
begin
  FIni.WriteBinaryStream(Section, Ident, Value);
end;


  {$region '   Form   '}
procedure TJppMemIniFile.WriteFormPos(const Section: string; Form: TForm);
begin
  WriteBool(Section, Form.Name + '.Maximized', Form.WindowState = wsMaximized);
  if Form.WindowState <> wsMaximized then
  begin
    WriteInteger(Section, Form.Name + '.Left', Form.Left);
    WriteInteger(Section, Form.Name + '.Top', Form.Top);
    WriteInteger(Section, Form.Name + '.Width', Form.Width);
    WriteInteger(Section, Form.Name + '.Height', Form.Height);
  end;
end;

procedure TJppMemIniFile.WriteFormPos(Form: TForm);
begin
  WriteFormPos(CurrentSection, Form);
end;

procedure TJppMemIniFile.ReadFormPos(const Section: string; Form: TForm);
var
  x: integer;
begin
  x := ReadInteger(Section, Form.Name + '.Left', Form.Left);
  x := GetIntInRange(x, 0, Screen.Width - 50);
  Form.Left := x;

  x := ReadInteger(Section, Form.Name + '.Top', Form.Top);
  x := GetIntInRange(x, 0, Screen.Height - 50);
  Form.Top := x;

  x := ReadInteger(Section, Form.Name + '.Width', Form.Width);
  x := GetIntInRange(x, 50, Screen.Width);
  Form.Width := x;

  x := ReadInteger(Section, Form.Name + '.Height', Form.Height);
  x := GetIntInRange(x, 50, Screen.Height);
  Form.Height := x;
end;

procedure TJppMemIniFile.ReadFormPos(Form: TForm);
begin
  ReadFormPos(CurrentSection, Form);
end;
  {$endregion Form}


  {$region '   String   '}
procedure TJppMemIniFile.WriteString(const Section, Ident, Value: string);
begin
  FIni.WriteString(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteString(Section: string; Cmp: TComponent; const Value: string);
begin
  WriteString(Section, Cmp.Name, Value);
end;

procedure TJppMemIniFile.WriteString(const Ident, Value: string);
begin
  WriteString(CurrentSection, Ident, Value);
end;

procedure TJppMemIniFile.WriteString(Cmp: TComponent; const Value: string);
begin
  WriteString(CurrentSection, Cmp, Value);
end;

function TJppMemIniFile.ReadString(const Section, Ident, Default: string): string;
begin
  Result := FIni.ReadString(Section, Ident, Default);
end;

function TJppMemIniFile.ReadString(const Section: string; Cmp: TComponent; const Default: string): string;
begin
  Result := ReadString(Section, Cmp.Name, Default);
end;

function TJppMemIniFile.ReadString(const Ident, Default: string): string;
begin
  Result := ReadString(CurrentSection, Ident, Default);
end;

function TJppMemIniFile.ReadString(Cmp: TComponent; const Default: string): string;
begin
  Result := ReadString(CurrentSection, Cmp.Name, Default);
end;

  {$endregion String}


  {$region '   Bool   '}
procedure TJppMemIniFile.WriteBool(const Section, Ident: string; const Value: Boolean);
begin
  FIni.WriteBool(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteBool(const Ident: string; const Value: Boolean);
begin
  WriteBool(CurrentSection, Ident, Value);
end;

function TJppMemIniFile.ReadBool(const Section, Ident: string; const Default: Boolean): Boolean;
begin
  Result := FIni.ReadBool(Section, Ident, Default);
end;

function TJppMemIniFile.ReadBool(const Ident: string; const Default: Boolean): Boolean;
begin
  Result := ReadBool(CurrentSection, Ident, Default);
end;
  {$endregion Bool}


  {$region '   Integer   '}
procedure TJppMemIniFile.WriteInteger(const Section, Ident: string; const Value: integer);
begin
  FIni.WriteInteger(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteInteger(const Section: string; Cmp: TComponent; const Value: integer);
begin
  WriteInteger(Section, Cmp.Name, Value);
end;

procedure TJppMemIniFile.WriteInteger(const Ident: string; const Value: integer);
begin
  WriteInteger(CurrentSection, Ident, Value);
end;

procedure TJppMemIniFile.WriteInteger(Cmp: TComponent; const Value: integer);
begin
  WriteInteger(CurrentSection, Cmp.Name, Value);
end;

function TJppMemIniFile.ReadInteger(const Section, Ident: string; const Default: integer): integer;
begin
  Result := FIni.ReadInteger(Section, Ident, Default);
end;

function TJppMemIniFile.ReadInteger(const Section: string; Cmp: TComponent; const Default: integer): integer;
begin
  Result := ReadInteger(Section, Cmp.Name, Default);
end;

function TJppMemIniFile.ReadInteger(const Ident: string; const Default: integer): integer;
begin
  Result := ReadInteger(CurrentSection, Ident, Default);
end;

function TJppMemIniFile.ReadInteger(Cmp: TComponent; const Default: integer): integer;
begin
  Result := ReadInteger(CurrentSection, Cmp.Name, Default);
end;
  {$endregion Integer}


  {$region '   Date   '}
function TJppMemIniFile.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;
begin
  Result := FIni.ReadDate(Section, Ident, Default);
end;

function TJppMemIniFile.ReadDate(const Ident: string; Default: TDateTime): TDateTime;
begin
  Result := ReadDate(CurrentSection, Ident, Default);
end;

procedure TJppMemIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  FIni.WriteDate(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteDate(const Ident: string; Value: TDateTime);
begin
  WriteDate(CurrentSection, Ident, Value);
end;
  {$endregion Date}


  {$region '   DateTime   '}
function TJppMemIniFile.ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;
begin
  Result := FIni.ReadDateTime(Section, Ident, Default);
end;

function TJppMemIniFile.ReadDateTime(const Ident: string; Default: TDateTime): TDateTime;
begin
  Result := ReadDateTime(CurrentSection, Ident, Default);
end;

procedure TJppMemIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  FIni.WriteDateTime(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteDateTime(const Ident: string; Value: TDateTime);
begin
  WriteDateTime(CurrentSection, Ident, Value);
end;
  {$endregion DateTime}


  {$region '   Time   '}
function TJppMemIniFile.ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;
begin
  Result := FIni.ReadTime(Section, Ident, Default);
end;

function TJppMemIniFile.ReadTime(const Ident: string; Default: TDateTime): TDateTime;
begin
  Result := ReadTime(CurrentSection, Ident, Default);
end;

procedure TJppMemIniFile.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
  FIni.WriteTime(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteTime(const Ident: string; Value: TDateTime);
begin
  WriteTime(CurrentSection, Ident, Value);
end;
  {$endregion Time}


  {$region '   Float   '}
function TJppMemIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;
begin
  Result := FIni.ReadFloat(Section, Ident, Default);
end;

function TJppMemIniFile.ReadFloat(const Ident: string; Default: Double): Double;
begin
  Result := ReadFloat(CurrentSection, Ident, Default);
end;

procedure TJppMemIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  FIni.WriteFloat(Section, Ident, Value);
end;

procedure TJppMemIniFile.WriteFloat(const Ident: string; Value: Double);
begin
  WriteFloat(CurrentSection, Ident, Value);
end;
  {$endregion Float}


  {$region '   Color   '}
procedure TJppMemIniFile.WriteColor(const Section, Ident: string; const Color: TColor);
begin
  FIni.WriteString(Section, Ident, ColorToString(Color));
end;

procedure TJppMemIniFile.WriteColor(const Ident: string; const Color: TColor);
begin
  WriteColor(CurrentSection, Ident, Color);
end;

function TJppMemIniFile.ReadColor(const Section, Ident: string; const Default: TColor): TColor;
var
  sColor: string;
  xColor: integer;
begin
  sColor := FIni.ReadString(Section, Ident, ColorToString(Default));
  if not IdentToColor(sColor, xColor) then
  try
    xColor := StringToColor(sColor);
  except
    xColor := Default;
  end;

  Result := xColor;
end;

function TJppMemIniFile.ReadColor(const Ident: string; const Default: TColor): TColor;
begin
  Result := ReadColor(CurrentSection, Ident, Default);
end;
  {$endregion Color}


  {$region '   HTML Color   '}
procedure TJppMemIniFile.WriteHtmlColor(const Section, Ident: string; const AColor: TColor);
begin
  FIni.WriteString(Section, Ident, ColorToHtmlColorStr(AColor, '#', True));
end;

procedure TJppMemIniFile.WriteHtmlColor(const Ident: string; const AColor: TColor);
begin
  WriteHtmlColor(CurrentSection, Ident, AColor);
end;

function TJppMemIniFile.ReadHtmlColor(const Section, Ident: string; const Default: TColor): TColor;
var
  s: string;
begin
  s := FIni.ReadString(Section, Ident, '');
  if s = '' then Exit(Default);
  if not TryHtmlStrToColor(s, Result) then Result := Default;
end;

function TJppMemIniFile.ReadHtmlColor(const Ident: string; const Default: TColor): TColor;
begin
  Result := ReadHtmlColor(CurrentSection, Ident, Default);
end;
  {$endregion HTML Color}


  {$region '   ReadIntegerInRange   '}
function TJppMemIniFile.ReadIntegerInRange(const Section, Ident: string; const Default, Min, Max: integer): integer;
var
  x: integer;
begin
  x := FIni.ReadInteger(Section, Ident, Default);
  Result := GetIntInRange(x, Min, Max);
end;

function TJppMemIniFile.ReadIntegerInRange(const Ident: string; const Default, Min, Max: integer): integer;
begin
  Result := ReadIntegerInRange(CurrentSection, Ident, Default, Min, Max);
end;

{$endregion ReadIntegerInRange}


  {$region '   FontStyles   '}
procedure TJppMemIniFile.WriteFontStyles(const Section: string; const Ident: string; const FontStyles: TFontStyles);
begin
  FIni.WriteString(Section, Ident, FontStylesToStr(FontStyles));
end;

procedure TJppMemIniFile.WriteFontStyles(const Ident: string; const FontStyles: TFontStyles);
begin
  WriteFontStyles(CurrentSection, Ident, FontStyles);
end;

function TJppMemIniFile.ReadFontStyles(const Section: string; const Ident: string; const Default: TFontStyles): TFontStyles;
var
  s: string;
begin
  s := FontStylesToStr(Default);
  s := FIni.ReadString(Section, Ident, s);
  Result := StrToFontStyles(s);
end;

function TJppMemIniFile.ReadFontStyles(const Ident: string; const Default: TFontStyles): TFontStyles;
begin
  Result := ReadFontStyles(CurrentSection, Ident, Default);
end;
  {$endregion FontStyles}


  {$region '   Read / Write Strings   '}

procedure TJppMemIniFile.WriteStrings(const Section: string; Items: TStrings; MaxItemsCount: integer = -1 {$IFDEF DCC}; Compress: Boolean = False{$ENDIF});
var
  i: integer;
  {$IFDEF DCC}
  s: string;
  StringStream: TStringStream;
  MemoryStream: TMemoryStream;
  //Buffer: array[0..99] of Byte;
  Buffer: array[0..63] of Byte;
  k, xRead: integer;
  {$ENDIF}
begin
  if FIni.SectionExists(Section) then FIni.EraseSection(Section);
  {$IFDEF DCC}if not Compress then {$ENDIF}
  begin
    for i := 0 to Items.Count - 1 do
    begin
      WriteBoundString(Section, 'Line_' + PadLeft(IntToStr(i + 1), 3, '0'), Items[i]);
      if (i + 1) >= MaxItemsCount then Break;
      //FIni.WriteString(Section, 'Line_' + PadLeft(IntToStr(i + 1), 3, '0'), Items[i]);
    end;
  end

  {$IFDEF DCC}
  else

  // compression
  begin

    s := Items.Text;
    StringStream := TStringStream.Create(s);
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.Size := 0;
      ZCompressStream(StringStream, MemoryStream);

      MemoryStream.Position := 0;
      for i := 0 to MemoryStream.Size div SizeOf(Buffer) do
      begin
        s := '';
        xRead := MemoryStream.Read(Buffer, SizeOf(Buffer));
        for k := 0 to xRead - 1 do s := s + IntToHex(Buffer[k], 2);
        FIni.WriteString(Section, COMPRESSED_VALUE_PREFIX + PadLeft(IntToStr(i + 1), 3, '0'), s);
      end;

    finally
      StringStream.Free;
      MemoryStream.Free;
    end;

  end;
  {$ENDIF}


end;


procedure TJppMemIniFile.ReadStrings(const Section: string; Items: TStrings {$IFDEF DCC}; ItemsCompressed: Boolean{$ENDIF});
var
  sl, slNames: TStringList;
  i, xp: integer;
  s: string;
  {$IFDEF DCC}
  ss: TStringStream;
  ms: TMemoryStream;
  x: integer;
  Hex, sName: string;
  xb: Byte;
  {$ENDIF}
begin
  if FIni.SectionExists(Section) then
  begin


    {$IFDEF DCC}if not ItemsCompressed then {$ENDIF}
    begin
      sl := TStringList.Create;
      try

        FIni.ReadSectionValues(Section, sl);

        // TrimBounds(FIni.ReadString(Section, Ident, Default), FLeftStringBound, FRightStringBound);
        for i := 0 to sl.Count - 1 do
        begin
          xp := Pos('=', sl[i]);
          if xp > 0 then s := Copy(sl[i], xp + 1, Length(sl[i]));
          sl[i] := TrimBounds(s, FLeftStringBound, FRightStringBound);
          //if xp > 0 then sl[i] := Copy(sl[i], xp + 1, Length(sl[i]));
        end;

        Items.Assign(sl);

      finally
        sl.Free;
      end;
    end

    {$IFDEF DCC}

    else


    // decompression
    begin

      s := '';
      slNames := TStringList.Create;
      try
        ReadSectionKeyNames(Section, slNames);
        for i := 0 to slNames.Count - 1 do
        begin
          sName := slNames[i];
          if Copy(sName, 1, Length(COMPRESSED_VALUE_PREFIX)) <> COMPRESSED_VALUE_PREFIX then Continue;
          s := s + FIni.ReadString(Section, sName, '');
        end;
      finally
        slNames.Free;
      end;


      ms := TMemoryStream.Create;
      ss := TStringStream.Create;
      try


        Items.Text := '';
        if s <> '' then
        begin

          for i := 1 to (Length(s) div 2) do
          begin
            x := (i * 2) - 1;
            Hex := '$' + Copy(s, x, 2);
            try
              xb := StrToInt(Hex);
            except
              Items.Text := ''; // niewłaściwe dane!!!
              //ShowMessage('Invalid input data!');
              Exit;
            end;
            ms.Write(xb, 1);
          end;

          ss.Size := 0;
          ms.Position := 0;
          ZDecompressStream(ms, ss);
          Items.Text := ss.DataString;

        end;

      finally
        ss.Free;
        ms.Free;
      end;

    end;
    {$ENDIF}



  end;
end;
  {$endregion Read / Write Strings}


  {$region '   BoundString   '}
procedure TJppMemIniFile.WriteBoundString(const Section, Ident, Value: string);
begin
  FIni.WriteString(Section, Ident, FLeftStringBound + Value + FRightStringBound);
end;

procedure TJppMemIniFile.WriteBoundString(const Section: string; Cmp: TComponent; const Value: string);
begin
  WriteBoundString(Section, Cmp.Name, Value);
end;

procedure TJppMemIniFile.WriteBoundString(const Ident, Value: string);
begin
  WriteBoundString(CurrentSection, Ident, Value);
end;

procedure TJppMemIniFile.WriteBoundString(Cmp: TComponent; const Value: string);
begin
  WriteBoundString(CurrentSection, Cmp.Name, Value);
end;

function TJppMemIniFile.ReadBoundString(const Section, Ident, Default: string): string;
begin
  Result := TrimBounds(FIni.ReadString(Section, Ident, Default), FLeftStringBound, FRightStringBound);
end;

function TJppMemIniFile.ReadBoundString(const Section: string; Cmp: TComponent; const Default: string): string;
begin
  Result := ReadBoundString(Section, Cmp.Name, Default);
end;

function TJppMemIniFile.ReadBoundString(const Ident, Default: string): string;
begin
  Result := ReadBoundString(CurrentSection, Ident, Default);
end;

function TJppMemIniFile.ReadBoundString(Cmp: TComponent; const Default: string): string;
begin
  Result := ReadBoundString(CurrentSection, Cmp.Name, Default);
end;
  {$endregion BoundString}


  {$region '   Int64   '}

{$IFDEF DCC}
procedure TJppMemIniFile.WriteInt64(const Section, Ident: string; const Value: Int64);
begin
  FIni.WriteString(Section, Ident, IntToStr(Value));
end;

procedure TJppMemIniFile.WriteInt64(const Ident: string; const Value: Int64);
begin
  WriteInt64(CurrentSection, Ident, Value);
end;

function TJppMemIniFile.ReadInt64(const Section, Ident: string; const Default: Int64): Int64;
var
  s: string;
  x: Int64;
begin
  s := FIni.ReadString(Section, Ident, 'ERR');
  if TryStrToInt64(s, x) then Result := x else Result := Default;
end;

function TJppMemIniFile.ReadInt64(const Ident: string; const Default: Int64): Int64;
begin
  Result := ReadInt64(CurrentSection, Ident, Default);
end;
{$ENDIF}

  {$endregion Int64}


  {$region '   DotFloat   '}

function TJppMemIniFile.ReadDotFloat(const Section, Ident: string; const Default: Double): Double;
var
  FloatStr: string;
begin
  FloatStr := FIni.ReadString(Section, Ident, '');
  FloatStr := StringReplace(FloatStr, '.', FormatSettings.DecimalSeparator, []);
  Result := Default;
  if FloatStr <> '' then
    if not TryStrToFloat(FloatStr, Result) then Result := Default;
//  try
//    Result := StrToFloat(FloatStr);
//  except
//    on EConvertError do
//      // Ignore EConvertError exceptions
//    else
//      raise;
//  end;
end;

function TJppMemIniFile.ReadDotFloat(const Ident: string; const Default: Double): Double;
begin
  Result := ReadDotFloat(CurrentSection, Ident, Default);
end;

procedure TJppMemIniFile.WriteDotFloat(const Section, Ident: string; const Value: Double);
var
  s: string;
begin
  s := FloatToStr(Value);
  s := StringReplace(s, FormatSettings.DecimalSeparator, '.', []);
  FIni.WriteString(Section, Ident, s);
end;

procedure TJppMemIniFile.WriteDotFloat(const Ident: string; const Value: Double);
begin
  WriteDotFloat(CurrentSection, Ident, Value);
end;
  {$endregion DotFloat}


  {$region '   CheckBox   '}
procedure TJppMemIniFile.WriteCheckBox(const Section: string; CheckBox: TCheckBox);
begin
  WriteBool(Section, CheckBox.Name, CheckBox.Checked);
end;

procedure TJppMemIniFile.WriteCheckBox(CheckBox: TCheckBox);
begin
  WriteCheckBox(CurrentSection, CheckBox);
end;

procedure TJppMemIniFile.ReadCheckBox(const Section: string; CheckBox: TCheckBox);
begin
  CheckBox.Checked := ReadBool(Section, CheckBox.Name, CheckBox.Checked);
end;

procedure TJppMemIniFile.ReadCheckBox(CheckBox: TCheckBox);
begin
  ReadCheckBox(CurrentSection, CheckBox);
end;
  {$endregion CheckBox}


  {$region '   RadioButton   '}
procedure TJppMemIniFile.WriteRadioButton(const Section: string; RadioButton: TRadioButton);
begin
  WriteBool(Section, RadioButton.Name, RadioButton.Checked);
end;

procedure TJppMemIniFile.WriteRadioButton(RadioButton: TRadioButton);
begin
  WriteRadioButton(CurrentSection, RadioButton);
end;

procedure TJppMemIniFile.ReadRadioButton(const Section: string; RadioButton: TRadioButton);
begin
  RadioButton.Checked := ReadBool(Section, RadioButton.Name, RadioButton.Checked);
end;

procedure TJppMemIniFile.ReadRadioButton(RadioButton: TRadioButton);
begin
  ReadRadioButton(CurrentSection, RadioButton);
end;
  {$endregion RadioButton}




{$endregion TJppMemIniFile}


end.
