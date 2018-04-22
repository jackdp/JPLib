unit JPL.Console.ColorParser;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 2018.03.16

  ===================================

  Using TConColorParser:

  1. Create instance: ccp := TConColorParser.Create;
  2. Add some text: ccp.Text := 'Some text';
  3. Add some text to be highlighted: ccp.AddHighlightedText('tex', 15, 4); // 15 - white text, 4 - dark red background (on Windows)
     You can add many highlighted texts.
     Highlighted texts can be nested, eg. you can add 'Windows', 'indow', 'nd' ...
  4. Call Parse method: ccp.Parse;
  5. Display result: ccp.WriteText; or save to array: ccp.SaveResultToArray;
  6. Free ccp: ccp.Free;

  ====================================

  Example:

  // uses LazUTF8, JPL.Console.ColorParser ...
  procedure TestCCP;
  var
    ccp: TConColorParser;
  begin
    ccp := TConColorParser.Create;
    try
      ccp.Text :=
        'Lorem ipsum dolor sit amet, consectetur adipiscing elit, ' +
        'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.' + sLineBreak +
        'Текст - Κείμενο - テキスト- ŻÓŁTE TŁO' + sLineBreak;
      ccp.AddHighlightedText('Lorem', 12, 4); // red
      ccp.AddHighlightedText('dolor', 1, 9); // blue
      ccp.AddHighlightedText('lo', 14, 6); // yellow
      ccp.AddHighlightedText('consectetur', 10, 2); // green
      ccp.AddHighlightedText('nsectet', 5, 13); // magenta
      ccp.AddHighlightedText('ect', 3, 11); // cyan

      ccp.AddHighlightedText('текст', 0, 15);
      ccp.AddHighlightedText('Κείμενο', 0, 12);
      ccp.AddHighlightedText('テキスト', 15, 13);
      ccp.AddHighlightedText('żółte tło', 1, 14);

      ccp.CaseSensitive := True;
      Writeln('Case sensitive');
      ccp.Parse;
      ccp.WriteText;

      ccp.ClearResult;

      Writeln('------------------------------------');
      ccp.CaseSensitive := False;
      Writeln('Not case sensitive');
      ccp.Parse;
      ccp.WriteText;
    finally
      ccp.Free;
    end;
  end;

 }

{$mode objfpc}{$H+}
//{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  //MFPC.LazUtils.LazUTF8,
  SysUtils, StrUtils,
  JPL.Console, JPL.Strings
  ;


type

  TConParTextRec = record
    Text: string;
    TextColor: Byte;
    BgColor: Byte;
  end;

  TConParTextArray = array of TConParTextRec;

  TConColorParser = class
  private
    FArrHiParams: TConParTextArray;
    FText: string;
    FArrResults: TConParTextArray;
    FCaseSensitive: Boolean;
    FDefaultTextColor: Byte;
    FDefaultBackgroundColor: Byte;
    function GetHighlightedTextCount: integer;
    procedure SetText(AValue: string);
    procedure AddResult(const Text: string; TextColor, BgColor: Byte);
    function ResultStr: string; // for debug purposes. Can be deleted.
  public
    constructor Create;
    destructor Destroy; override;

    // Clears all and restores default values.
    procedure Clear;

    // Clears results, but not highligting params and input text.
    procedure ClearResult;

    // The text to be highlighted.
    procedure AddHighlightedText(const Text: string; const TextColor, BgColor: Byte);

    // Main procedure
    procedure Parse;

    // Displays colored text
    procedure WriteText;

    procedure SetDefaultColors(const TextColor, BgColor: Byte); overload;
    procedure SetDefaultColors(const ColorRec: TConsoleColors); overload;
    procedure SaveResultToArray(var Arr: TConParTextArray);

    procedure DisplayHighlightedTextArray; // for debug purposes. TODO: delete or move to private section

    // Text to parse.
    property Text: string read FText write SetText;

    // Returns the number of highlighted texts added by AddHighlightedText proc.
    property HighlightedTextCount: integer read GetHighlightedTextCount;

    // Used when searching for text to highlight.
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;

    // This color will be used when displaying non-highlighted texts.
    property DefaultTextColor: Byte read FDefaultTextColor write FDefaultTextColor;

    //This color will be used when displaying non-highlighted texts.
    property DefaultBackgroundColor: Byte read FDefaultBackgroundColor write FDefaultBackgroundColor;
  end;



implementation


constructor TConColorParser.Create;
begin
  inherited;
  Clear;
end;

destructor TConColorParser.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TConColorParser.Clear;
begin
  FText := '';
  FDefaultTextColor := CON_COLOR_NONE;
  FDefaultBackgroundColor := CON_COLOR_NONE;
  SetLength(FArrHiParams, 0);
  SetLength(FArrResults, 0);
end;

procedure TConColorParser.ClearResult;
begin
  SetLength(FArrResults, 0);
end;

procedure TConColorParser.AddHighlightedText(const Text: string; const TextColor, BgColor: Byte);
var
  xInd: integer;
begin
  if Text = '' then Exit;
  SetLength(FArrHiParams, Length(FArrHiParams) + 1);
  xInd := High(FArrHiParams);
  FArrHiParams[xInd].Text := Text;
  FArrHiParams[xInd].TextColor := TextColor;
  FArrHiParams[xInd].BgColor := BgColor;
end;

procedure TConColorParser.SetText(AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
end;

procedure TConColorParser.AddResult(const Text: string; TextColor, BgColor: Byte);
var
  xInd: integer;
begin
  SetLength(FArrResults, Length(FArrResults) + 1);
  xInd := High(FArrResults);
  FArrResults[xInd].Text := Text;
  FArrResults[xInd].TextColor := TextColor;
  FArrResults[xInd].BgColor := BgColor;
end;

function TConColorParser.GetHighlightedTextCount: integer;
begin
  Result := Length(FArrHiParams);
end;



{$region '                      Parse                           '}

type

  TRange = record
    StartPos: integer;
    EndPos: integer;
    TextColor: Byte;
    BgColor: Byte;
    //function Length: integer;   //      już niepotrzebne
  end;

  TRangesArray = array of TRange;

//function TRange.Length: integer;
//begin
//  Result := Self.EndPos - Self.StartPos + 1;
//end;

procedure TConColorParser.Parse;
type
  TLetter = record
    Znak: Char;
    TextColor: Byte;
    BgColor: Byte;
  end;
  TLetterArray = array of TLetter;
var
  i, x, xStartPos: integer;
  s, us, sub, sr: string;
  ArrRanges: TRangesArray = nil;
  cptr: TConParTextRec;
  Range: TRange;
  ArrL: TLetterArray;
  Letter: TLetter;
  PrevColorT, PrevColorB: Byte;

begin
  s := FText;
  if not FCaseSensitive then us := AnsiUpperCase(s) else us := '';

  // Utworzenie listy podświetlanych zakresów
  for i := 0 to High(FArrHiParams) do
  begin
    cptr := FArrHiParams[i];
    sub := cptr.Text;

    xStartPos := 1;
    while True do
    begin

      if FCaseSensitive then x := PosEx(sub, s, xStartPos)
      else x := PosEx(AnsiUpperCase(sub), us, xStartPos);

      if x = 0 then Break;

      SetLength(ArrRanges, Length(ArrRanges) + 1);
      ArrRanges[High(ArrRanges)].StartPos := x;
      ArrRanges[High(ArrRanges)].EndPos := x + Length(sub) - 1;
      ArrRanges[High(ArrRanges)].TextColor := cptr.TextColor;
      ArrRanges[High(ArrRanges)].BgColor := cptr.BgColor;

      xStartPos := x + Length(sub);
    end;
  end;


  // Zapisanie tekstu wejściowego do tablicy ArrL
  SetLength(ArrL, Length(s));
  for i := 0 to Length(s) - 1 do
  begin
    ArrL[i].Znak := s[i + 1];
    ArrL[i].TextColor := FDefaultTextColor;
    ArrL[i].BgColor := FDefaultBackgroundColor;
  end;

  // Przypisanie kolorów tekstu i tła dla każdego podświetlonego znaku
  for i := 0 to High(ArrRanges) do
  begin
    Range := ArrRanges[i];
    for x := Range.StartPos to Range.EndPos do
    begin
      ArrL[x - 1].TextColor := Range.TextColor;
      ArrL[x - 1].BgColor := Range.BgColor;
    end;
  end;

  // Wyznaczenie łańcuchów podświetlonych i niepodświetlonych, oraz zapisanie ich w tablicy FArrResults (AddResult).
  PrevColorT := FDefaultTextColor;
  PrevColorB := FDefaultBackgroundColor;
  sr := '';
  for i := 0 to High(ArrL) do
  begin

    Letter := ArrL[i];

    if (Letter.TextColor <> PrevColorT) or (Letter.BgColor <> PrevColorB) then
    begin
      AddResult(sr, PrevColorT, PrevColorB);
      sr := Letter.Znak;
      PrevColorT := Letter.TextColor;
      PrevColorB := Letter.BgColor;
    end
    else
      sr += Letter.Znak;

  end;

  if sr <> '' then
  begin
    AddResult(sr, Letter.TextColor, Letter.BgColor);
  end;

end;


{$endregion Parse}


procedure TConColorParser.WriteText;
var
  i: integer;
  s: string;
  TextColor, BgColor: Byte;
begin
  for i := 0 to High(FArrResults) do
  begin

    TConsole.ResetColors;

    s := FArrResults[i].Text;
    if s = ENDL then
    begin
      Writeln;
      Continue;
    end;

    TextColor := FArrResults[i].TextColor;
    BgColor := FArrResults[i].BgColor;

    if TextColor <> CON_COLOR_NONE then TConsole.SetTextColor(TextColor);
    if BgColor <> CON_COLOR_NONE then TConsole.SetBackgroundColor(BgColor);

    Write(s);

  end;

  TConsole.ResetColors;
end;

procedure TConColorParser.SetDefaultColors(const TextColor, BgColor: Byte);
begin
  FDefaultTextColor := TextColor;
  FDefaultBackgroundColor := BgColor;
end;

procedure TConColorParser.SetDefaultColors(const ColorRec: TConsoleColors);
begin
  FDefaultTextColor := ColorRec.Text;
  FDefaultBackgroundColor := ColorRec.Background;
end;

procedure TConColorParser.SaveResultToArray(var Arr: TConParTextArray);
var
  i: integer;
begin
  SetLength(Arr, Length(FArrResults));
  for i := 0 to High(FArrResults) do Arr[i] := FArrResults[i];
end;

procedure TConColorParser.DisplayHighlightedTextArray;
var
  cptr: TConParTextRec;
  i: integer;
begin
  for i := 0 to High(FArrHiParams) do
  begin
    cptr := FArrHiParams[i];
    if cptr.TextColor <> FDefaultTextColor then TConsole.SetTextColor(cptr.TextColor);
    if cptr.BgColor <> FDefaultBackgroundColor then TConsole.SetBackgroundColor(cptr.BgColor);
    Write(cptr.Text);
    TConsole.ResetColors;
    Writeln;
  end;
end;

function TConColorParser.ResultStr: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(FArrResults) do Result += FArrResults[i].Text;
end;

end.

