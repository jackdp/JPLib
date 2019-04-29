unit JPL.Strings;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 2019.04.28

  To jest mÛj stary modu≥ z roku 2000 dla Borland Pascala 7.0
  W kolejnych latach rozbudowywany i dostosowywany do nowszych wersji Delphi i FPC.
 }

{ TODO : ZrobiÊ generalny porzπdek z tym ba≥aganem! }

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  SysUtils;

const
  CR = #13;
  CRCR = #13#13;
  LF = #10;
  LFLF = #10#10;
  CRLF = #13#10;
  TAB = #9;
  ENDL = sLineBreak;



function Rbs(Dir: string): string;
function Qs(const s: string): string;
//function Capitalize(s: string): string;
function FixFileName(fName: string; s: string = '_'; ZamieniajCudzyslowy: Boolean = True): string;
function IsValidShortFileName(const ShortFileName: string): Boolean;
function IsValidLongFileName(const LongFileName: string): Boolean;
function FixFileNameSlashes(const FileName: string): string;
function PadString(Text: string; i: integer; znak: char = ' '): string;
function Pad(Text: string; i: integer; znak: char = ' '): string;
function PadRight(Text: string; i: integer; znak: char = ' '): string;
function UnquoteStr(s: string; bDoubleQuote: Boolean = True): string;
function IntToStrEx(const x: int64; c: Char = ' '): string; overload;
function IntToStrEx(const x: integer; c: Char = ' '): string; overload;
function GetLastCharIndex(s: string; c: Char): integer;
function DelFirstCharInString(s: string; ToDelete: Char): string;
function DelLastCharInString(s: string; ToDelete: Char): string;
function RemoveNum(s: string): string;
function GetLastBIndex(s: string): integer;
//function AddSlashes(Text: string): string;
function RemoveSlashes(Text: string): string;
function RemoveSpaces(s: string): string;
function ReplaceSpecialChars(s: string; sc: CHar = '_'): string;

function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string;
function RemoveNonLetters(s: string): string;
function ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
function ReplaceDecimalSeparator(FloatStr: string; NewSeparator: string = '.'): string;

function RemovePolishChars(s: string): string;
function IsBigLetterPL(c: Char): Boolean;
function IsSmallLetterPL(c: Char): Boolean;
function IsLetterPL(c: Char): Boolean;
function IsBigLetter(c: Char): Boolean;
function IsSmallLetter(c: Char): Boolean;
function IsLetter(c: Char): Boolean;
function IsNumber(c: Char): Boolean;

function DistinctChars(s: string; IgnoreCase: Boolean = True): Boolean;
function MakeDistinctChars(s: string): string;
function CharCount(c: Char; s: string): integer;
function ReverseStr(const s: string): string;
function FillStrToLen(s: string; Len: integer; FillValue: Char = ' '): string;
function AnsiUpCase(zn: Char; Default: Char = #0): Char;
function AnsiLowCase(zn: Char; Default: Char = #0): Char;
function RemoveChars(const SrcStr, CharsToRemove: string; IgnoreCase: Boolean = False): string; overload;
function RemoveChars(const SrcStr: string; Chars: array of Char; IgnoreCase: Boolean = False): string; overload;
function LeaveChars(SrcStr: string; CharsToLeave: string): string;
function CutStrBefore(s, CutBeforeText: string; IgnoreCase: Boolean = False): string;
function CutStrAfter(s, CutAfterText: string; IgnoreCase: Boolean = False; IncludeSearchText: Boolean = True): string;

function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True): string;

function HtmlStringToStr(HTMLStr: string): string;
function GetHref(InStr: string): string;
function GetAnchorText(InStr: string; FixHTMLSpecialChars: Boolean = True): string;
function GetFirstDigitIndex(s: string): integer;
function GetFirstNonDigitIndex(s: string): integer;

function MyDir: string;

function StrRemove(s: string; StringToRemove: string): string;
function GetFileSizeString(const FileSize: int64; BytesStr: string = ' bytes'): string;

function TrimUp(s: string): string;
function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3): string;
function CopyString(const s: string; Copies: integer = 2): string;

//procedure StrToList(LineToParse: string; var List: TStringList; Separator: string = ',');
procedure SplitStrToArray(s: string; var Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>; const EndLineStr: string = sLineBreak);

function TrimBounds(s: string; LeftBound, RightBound: string): string;
function AddBounds(const s: string; LeftBound, RightBound: Char): string; overload;
function AddBounds(const s: string; LeftBound, RightBound: string): string; overload;
function AddBounds(const s: string; StringToBoundSeparator: string = ' '; BoundChar: Char = '-'; BoundLen: Integer = 16): string; overload;

function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
function GetRandomIntStr(Len: integer = 10): string;

function PosCS(const substr, s: string; bCaseSensitive: Boolean = False): integer;


//function TrimFromEnd(InStr, StrToRemove: string): string;
function TrimFromEnd(const s: string; const StringToCut: string): string;
function TrimFromStart(const s: string; const StringToCut: string): string;
function TrimExtDot(const FileExtension: string): string;
function AddFileNameSuffix(const FileName, Suffix: string): string;
function AddFileNamePrefix(const FileName, Prefix: string): string;
procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);



implementation




procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);
begin
  Dir := ExtractFileDir(fName);
  if bIncludePathDelimiter then Dir := IncludeTrailingPathDelimiter(Dir) else Dir := ExcludeTrailingPathDelimiter(Dir);

  BaseFileName := ExtractFileName(fName);
  BaseFileName := ChangeFileExt(BaseFileName, '');

  //Ext := GetFileExt(BaseFileName, bRemoveDotFromExt);
  Ext := GetFileExt(fName, bRemoveDotFromExt);
end;

function AddFileNameSuffix(const FileName, Suffix: string): string;
var
  Dir, ShortName, Ext: string;
begin
  if Suffix = '' then Exit(FileName);
//  Dir := Rbs(ExtractFileDir(FileName));
//  ShortName := ExtractFileName(FileName);
//  Ext := ExtractFileExt(ShortName);
//  ShortName := ChangeFileExt(ShortName, '');
  SplitFileName(FileName, Dir, ShortName, Ext, False, False);
  Result := Dir + PathDelim + ShortName + Suffix + Ext;
end;

function AddFileNamePrefix(const FileName, Prefix: string): string;
var
  Dir, ShortName, Ext: string;
begin
  if Prefix = '' then Exit(FileName);
  SplitFileName(FileName, Dir, ShortName, Ext, False, False);
  Result := Dir + PathDelim + Prefix + ShortName + Ext;
end;

function TrimExtDot(const FileExtension: string): string;
begin
  Result := TrimFromStart(FileExtension, '.');
end;

function TrimFromStart(const s: string; const StringToCut: string): string;
begin
  if Copy(s, 1, Length(StringToCut)) = StringToCut then Result := Copy(s, Length(StringToCut) + 1, Length(s))
  else Result := s;
end;

function TrimFromEnd(const s: string; const StringToCut: string): string;
begin
  if Copy(s, Length(s) - Length(StringToCut) + 1, Length(StringToCut)) = StringToCut then Result := Copy(s, 1, Length(s) - Length(StringToCut))
  else Result := s;
end;

//function TrimFromEnd(InStr, StrToRemove: string): string;
//var
//  xIn, xR: integer;
//begin
//  xIn := Length(InStr);
//  xR := Length(StrToRemove);
//  if Copy(InStr, xIn - xR + 1, xR) = StrToRemove then Result := Copy(InStr, 1, xIn - xR)
//  else Result := InStr;
//end;


function PosCS(const substr, s: string; bCaseSensitive: Boolean = False): integer;
begin
  if bCaseSensitive then Result := Pos(substr, s)
  else Result := Pos(AnsiUpperCase(substr), AnsiUpperCase(s));
end;

function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
var
  i: integer;
  bt: Byte;
begin
  Result := '';

  for i := 1 to Bytes do
  begin
    bt := Random(255);
    Result := Result + IntToHex(bt, 2);// + ByteSeparator;
    if i < Bytes then Result := Result + ByteSeparator;
  end;
  if bLowerCase then Result := LowerCase(Result);
end;

function GetRandomIntStr(Len: integer = 10): string;
const
  Nums = '0123456789';
var
  i, x: integer;
begin
  Result := '';

  for i := 1 to Len do
  begin
    x := Random(10);
    Result := Result + Nums[x + 1];
  end;
end;


function AddBounds(const s: string; LeftBound, RightBound: Char): string;
begin
  Result := LeftBound + s + RightBound;
end;

function AddBounds(const s: string; LeftBound, RightBound: string): string;
begin
  Result := LeftBound + s + RightBound;
end;

function AddBounds(const s: string; StringToBoundSeparator: string = ' '; BoundChar: Char = '-'; BoundLen: Integer = 16): string; overload;
var
  sb: string;
begin
  sb := StringOfChar(BoundChar, BoundLen);
  Result := sb + StringToBoundSeparator + s + StringToBoundSeparator + sb;
end;



function TrimBounds(s: string; LeftBound, RightBound: string): string;
begin
  if Copy(s, 1, Length(LeftBound)) = LeftBound then s := Copy(s, 1 + Length(LeftBound), Length(s));
  if Copy(s, Length(s) - Length(RightBound) + 1, Length(RightBound)) = RightBound then s := Copy(s, 1, Length(s) - Length(RightBound));
             //s := Copy(s, Length(s), 1);
  Result := s;
end;


procedure SplitStrToArray(s: string; var Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>; const EndLineStr: string = sLineBreak);
var
  x: integer;
begin
  SetLength(Arr, 0);
  if s = '' then Exit;

  x := Pos(EndLineStr, s);
  while x > 0 do
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := Copy(s, 1, x - 1);
    s := Copy(s, x + Length(EndLineStr), Length(s));
    x := Pos(EndLineStr, s);
  end;

  if s <> '' then
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := s;
  end;
end;

function CopyString(const s: string; Copies: integer = 2): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Copies do Result := Result + s;
end;

function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3): string;
var
  s: string;
  i, k, Len, LastDigitPos, xp: integer;
begin
  s := NumStr;
  LastDigitPos := Length(s);
  xp := Pos('.', s);
  if xp > 0 then LastDigitPos := xp - 1;

  xp := Pos(FormatSettings.DecimalSeparator, s);
  if xp > 0 then LastDigitPos := xp - 1;
  //Len := Length(s);
  Len := LastDigitPos;
  k := Len div NumBlockSize; // k - liczba wstawien separatora

  for i := 1 to k do Insert(Separator, s, Len - (i * NumBlockSize) + 1);

  if Copy(s, 1, Length(Separator)) = Separator then Delete(s, 1, Length(Separator));
  Result := s;
end;

function TrimUp(s: string): string;
begin
  Result := Trim(UpperCase(s));
end;

function GetFileSizeString(const FileSize: int64; BytesStr: string = ' bytes'): string;
var
  fs: extended;
  s: ShortString;
begin
  Result := IntToStr(FileSize);
  fs := FileSize;
  if fs < 1024 then
  begin
    str(
    fs: 2: 0, s);
    Result := string(s) + BytesStr;
  end
  else if (fs >= 1024) and (fs < (1024 * 1024)) then
  begin
    fs := fs / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' KB';
  end
  else if (fs >= 1024 * 1024) and (fs < (1024 * 1024 * 1024)) then
  begin
    fs := (fs / 1024) / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' MB';
  end
  else
  begin
    fs := (fs / 1024) / 1024 / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' GB';
  end;
end;

function StrRemove(s: string; StringToRemove: string): string;
begin
  Result := StringReplace(s, StringToRemove, '', [rfReplaceAll]);
end;

function MyDir: string;
begin
  //Result := rbs(ExtractFileDir(SysToUTF8(ParamStr(0))));
  Result := rbs(ExtractFileDir(ParamStr(0)));
end;

function GetFirstNonDigitIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    //if not (s[i] in ['0'..'9']) then
    if not CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;


function GetFirstDigitIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    //if s[i] in ['0'..'9'] then
    if CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;

function GetHref(InStr: string): string;
var
  xp: integer;
  sr: string;
begin
  sr := InStr;
  xp := Pos('href="', AnsiLowercase(sr));
  if xp > 0 then
  begin
    sr := Copy(sr, xp + Length('href="'), Length(sr));
    xp := Pos('"', sr);
    if xp > 0 then sr := Copy(sr, 1, xp - 1);
  end;
  Result := sr;
end;


function GetAnchorText(InStr: string; FixHTMLSpecialChars: Boolean = True): string;
var
  xp: integer;
  sr: string;
begin
  sr := InStr;
  xp := Pos('<a ', AnsiLowerCase(sr));
  if xp > 0 then
  begin
    sr := Copy(sr, xp, Length(sr));
    xp := Pos('>', sr);
    if xp > 0 then
    begin
      sr := Copy(sr, xp + 1, Length(sr));
      xp := Pos('</a>', AnsiLowerCase(sr));
      if xp > 0 then sr := Copy(sr, 1, xp - 1);
    end;
  end;
  if FixHTMLSpecialChars then sr := HtmlStringToStr(sr);
  Result := sr;
end;


function HtmlStringToStr(HTMLStr: string): string;
var
  sr: string;
begin
  sr := HTMLStr;
  sr := StringReplace(sr, '&lt;', '<', [rfReplaceAll, rfIgnorecase]);
  sr := StringReplace(sr, '&gt;', '>', [rfReplaceAll, rfIgnorecase]);
  sr := StringReplace(sr, '&amp;', '&', [rfReplaceAll, rfIgnorecase]);
  Result := sr;
end;


//{$hints off}
function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True): string;
begin
  fName := ExtractFileExt(fName);
  if bRemoveFirstDot then
    if Copy(fName, 1, 1) = '.' then Delete(fName, 1, 1); // fName := Copy(fName, 2, Length(fName));
  Result := fName;
end;
//{$hints on}


{$hints off}
// obcina ≥aÒcuch s po wystπpieniu w nim tekstu CutAfterText
function CutStrAfter(s, CutAfterText: string; IgnoreCase: Boolean = False; IncludeSearchText: Boolean = True): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(CutAfterText), AnsiUpperCase(s))
  else xp := Pos(CutAfterText, s);
  if xp > 0 then
  begin
    s := Copy(s, 1, xp - 1);
    if not IncludeSearchText then s := Copy(s, 1, Length(s) - Length(CutAfterText));
  end;
  Result := s;
end;
{$hints on}

{$hints off}
// obcina ≥aÒcuch s przed wystπpieniem w nim tekstu CutBeforeText
function CutStrBefore(s, CutBeforeText: string; IgnoreCase: Boolean = False): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(CutBeforeText), AnsiUpperCase(s))
  else xp := Pos(CutBeforeText, s);
  //if xp > 0 then s := Copy(s, 1, xp - 1); - ???
  if xp > 0 then s := Copy(s, xp, Length(s));
  Result := s;
end;
{$hints on}

{$hints off}
function GetStrBefore(s, GetStrBeforeText: string; IgnoreCase: Boolean = False): string;
var
  xp: integer;
begin
  if IgnoreCase then xp := Pos(AnsiUpperCase(GetStrBeforeText), AnsiUpperCase(s))
  else xp := Pos(GetStrBeforeText, s);
  if xp > 0 then s := Copy(s, 1, xp - 1);
  Result := s;
end;
{$hints on}

function LeaveChars(SrcStr: string; CharsToLeave: string): string;
var
  sr: string;
  c: char;
begin
  sr := '';

  for c in SrcStr do
    if Pos(c, CharsToLeave) > 0 then sr := sr + c;

  Result := (sr);
end;

function RemoveChars(const SrcStr, CharsToRemove: string; IgnoreCase: Boolean): string;
var
  i: integer;
  rf: TReplaceFlags;
begin
  Result := SrcStr;
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  for i := 1 to Length(CharsToRemove) do
    Result := StringReplace(Result, CharsToRemove[i], '', rf);
end;

function RemoveChars(const SrcStr: string; Chars: array of Char; IgnoreCase: Boolean): string;
var
  i: integer;
  rf: TReplaceFlags;
begin
  Result := SrcStr;
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  for i := 0 to High(Chars) do
    Result := StringReplace(Result, Chars[i], '', rf);
end;

function AnsiUpCase(zn: Char; Default: Char): Char;
var
  s: string;
begin
  s := AnsiUpperCase(zn);
  if s = '' then s := Default;
  Result := s[1];
end;


function AnsiLowCase(zn: Char; Default: Char): Char;
var
  s: string;
begin
  s := AnsiLowerCase(zn);
  if s = '' then s := Default;
  Result := s[1];
end;


{$hints off}
function FillStrToLen(s: string; Len: integer; FillValue: Char): string;
var
  i, x: integer;
begin
  x := Length(s);
  for i := x + 1 to Len do
    s := s + FillValue;
  Result := s;
end;
{$hints on}


function ReverseStr(const s: string): string;
var
  i, xLen: integer;
begin
  Result := s;
  xLen := Length(s);
  if xLen <= 1 then Exit;

  for i := 1 to xLen do
    Result[i] := s[xLen - i + 1];
end;

function CharCount(c: Char; s: string): integer;
var
  i, x: integer;
begin
  x := 0;
  for i := 1 to Length(s) do
    if s[i] = c then Inc(x);
  Result := x;
end;

{$hints off}
function MakeDistinctChars(s: string): string;
var
  i: integer;
  sr: string;
begin
  sr := '';
  for i := Length(s) downto 1 do
  begin
    if CharCount(s[i], s) > 1 then
    begin
      Delete(s, i, 1);
      Continue;
    end;
    sr := sr + s[i];
  end;
  Result := ReverseStr(sr);
end;
{$hints on}


{$hints off}
function DistinctChars(s: string; IgnoreCase: Boolean): Boolean;
var
  i, x: integer;
begin
  if IgnoreCase then s := AnsiUpperCase(s);
  Result := False;
  for i := 1 to Length(s) do
    for x := i + 1 to Length(s) do
      if s[x] = s[i] then Exit;
  Result := True;
end;
{$hints on}



function IsNumber(c: Char): Boolean;
begin
  //Result := c in ['0'..'9'];
  Result := CharInSet(c, ['0'..'9']);
end;

function IsLetterPL(c: Char): Boolean;
begin
  Result := IsSmallLetterPL(c) or IsBigLetterPL(c);
end;

function IsSmallLetterPL(c: Char): Boolean;
begin
  //Result := c in ['a'..'z', 'π', 'Ê', 'Í', '≥', 'Û', 'ú', 'ü', 'ø'];
  //97..122 + Polish chars
  Result := CharInSet(c, ['a'..'z', 'π', 'Ê', 'Í', '≥', 'Û', 'ú', 'ü', 'ø']);
end;

function IsBigLetterPL(c: Char): Boolean;
begin
  //Result := c in ['A'..'Z', '•', '∆', ' ', '£', '”', 'å', 'è', 'Ø'];
  // 65..90 + Polish chars
  Result := CharInSet(c, ['A'..'Z', '•', '∆', ' ', '£', '”', 'å', 'è', 'Ø']);
end;



function IsLetter(c: Char): Boolean;
begin
  Result := IsSmallLetter(c) or IsBigLetter(c);
end;

function IsSmallLetter(c: Char): Boolean;
begin
  //Result := c in ['a'..'z'];
  //97..122
  Result := CharInSet(c, ['a'..'z']);
end;

function IsBigLetter(c: Char): Boolean;
begin
  //Result := c in ['A'..'Z'];
  // 65..90
  Result := CharInSet(c, ['A'..'Z']);
end;

function RemoveNonLetters(s: string): string;
var
  i: integer;
  sr: string;
begin
  sr := '';
  for i := 1 to Length(s) do
    if IsLetter(s[i]) then sr := sr + s[i];
  Result := sr;
end;

function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  Result := StringReplace(Text, ToRemove, '', rf);
end;

function ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  Result := StringReplace(SrcStr, OldStr, NewStr, rf);
end;

function ReplaceDecimalSeparator(FloatStr: string; NewSeparator: string = '.'): string;
begin
  Result := StringReplace(FloatStr, FormatSettings.DecimalSeparator, NewSeparator, [rfReplaceAll, rfIgnoreCase]);
end;

{$hints off}
function ReplaceSpecialChars(s: string; sc: CHar): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if Ord(s[i]) < 32 then s[i] := sc;
  Result := s;
end;
{$hints on}


function RemoveSlashes(Text: string): string;
var
  s: string;
begin
  s := Text;
  s := StringReplace(s, '''''', '''', [rfReplaceAll]);
  s := StringReplace(s, '\"', '"', [rfReplaceAll]);
  s := StringReplace(s, '\\', '\', [rfReplaceAll]);
  s := StringReplace(s, '\t', TAB, [rfReplaceAll]);
  s := StringReplace(s, '\r\n', ENDL, [rfReplaceAll]);
  s := StringReplace(s, '\r', ENDL, [rfReplaceAll]);
  s := StringReplace(s, '\n', ENDL, [rfReplaceAll]);

  Result := s;
end;

function RemoveSpaces(s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function GetLastBIndex(s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] = '\' then
    begin
      Result := i;
      Exit;
    end;
end;


function RemoveNum(s: string): string;
var
  i, x: integer;
  c: Char;
  s2: string;
begin
  x := 0;
  s2 := '';
  for i := 1 to Length(s) do
  begin
    c := s[i];
    case c of
      '0'..'9': Continue;
    else
      x := i;
      Break;
    end;
    s2 := s2 + c;
  end;
  if x > 0 then s2 := s2 + Copy(s, x, Length(s));
  s2 := Trim(s2);
  if Copy(s2, 1, 1) = '.' then s2 := Trim(Copy(s2, 2, Length(s2)));
  if Copy(s2, 1, 1) = '-' then s2 := Trim(Copy(s2, 2, Length(s2)));
  if Copy(s2, 1, 1) = '_' then s2 := Trim(Copy(s2, 2, Length(s2)));
  Result := s2;
end;


function GetLastCharIndex(s: string; c: Char): integer;
var
  x, i: integer;
begin
  x := 0;
  //for i := 1 to Length(s) do
  //  if s[i] = c then x := i;
  for i := Length(s) downto 1 do
    if s[i] = c then
    begin
      x := i;
      Break;
    end;
  Result := x;
end;

{$hints off}
function DelFirstCharInString(s: string; ToDelete: Char): string;
begin
  if Copy(s, 1, 1) = ToDelete then
    s := Copy(s, 2, Length(s));
  Result := s;
end;
{$hints on}

{$hints off}
function DelLastCharInString(s: string; ToDelete: Char): string;
begin
  if Copy(s, Length(s), 1) = ToDelete then
    s := Copy(s, 1, Length(s) - 1);
  Result := s;
end;
{$hints on}

{$hints off}
function RemovePolishChars(s: string): string;
var
  i: integer;
begin
  s := StringReplace(s, 'π', 'a', [rfReplaceAll]);
  s := StringReplace(s, 'Ê', 'c', [rfReplaceAll]);
  s := StringReplace(s, 'Í', 'e', [rfReplaceAll]);
  s := StringReplace(s, '≥', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'Ò', 'n', [rfReplaceAll]);
  s := StringReplace(s, 'Û', 'o', [rfReplaceAll]);
  s := StringReplace(s, 'ú', 's', [rfReplaceAll]);
  s := StringReplace(s, 'ü', 'z', [rfReplaceAll]);
  s := StringReplace(s, 'ø', 'z', [rfReplaceAll]);

  s := StringReplace(s, '•', 'A', [rfReplaceAll]);
  s := StringReplace(s, '∆', 'C', [rfReplaceAll]);
  s := StringReplace(s, ' ', 'E', [rfReplaceAll]);
  s := StringReplace(s, '£', 'L', [rfReplaceAll]);
  s := StringReplace(s, '—', 'N', [rfReplaceAll]);
  s := StringReplace(s, '”', 'O', [rfReplaceAll]);
  s := StringReplace(s, 'å', 'S', [rfReplaceAll]);
  s := StringReplace(s, 'è', 'Z', [rfReplaceAll]);
  s := StringReplace(s, 'Ø', 'Z', [rfReplaceAll]);

  s := StringReplace(s, 'Í', 'e', [rfReplaceAll]);
  //s := StringReplace(s, 'A', 'a', [rfReplaceAll]);
  s := StringReplace(s, 'à', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'ù', 'L', [rfReplaceAll]);
  s := StringReplace(s, '‰', 'n', [rfReplaceAll]);
  s := StringReplace(s, '©', 'e', [rfReplaceAll]);
  s := StringReplace(s, 'Í', 'e', [rfReplaceAll]);

  for i := 1 to Length(s) do if not Ord(s[i]) in [65..122] then s[i] := '_';

  Result := s;
end;
{$hints on}


{$hints off}
function UnquoteStr(s: string; bDoubleQuote: Boolean = True): string;
var
  qc: Char;
begin
  if bDoubleQuote then qc := '"'
  else qc := '''';
  if Copy(s, 1, 1) = qc then Delete(s, 1, 1);
  if Copy(s, Length(s), 1) = qc then Delete(s, Length(s), 1);
  Result := s;
end;
{$hints on}


function PadString(Text: string; i: integer; znak: char = ' '): string;
begin
  Result := Pad(Text, i, znak);
end;

{$hints off}
function Pad(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if length(Text) < i then
  begin
    x := length(Text);
    y := i - x;
    for k := 1 to y do
      s := s + znak;
    Text := s + Text;
  end;
  Result := Text;
end;
{$hints on}

{$hints off}
function PadRight(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if Length(Text) < i then
  begin
    x := Length(Text);
    y := i - x;
    for k := 1 to y do
      s := s + znak;
    Text := Text + s;
  end;
  Result := Text;
end;
{$hints on}


{$hints off}
function FixFileName(fName: string; s: string = '_'; ZamieniajCudzyslowy: Boolean = True): string;
begin
  if ZamieniajCudzyslowy then fName := StringReplace(fName, '"', '''', [rfReplaceAll]);
  fName := StringReplace(fName, '?', s, [rfReplaceAll]);
  fName := StringReplace(fName, '*', s, [rfReplaceAll]);
  fName := StringReplace(fName, ':', s, [rfReplaceAll]);
  fName := StringReplace(fName, '/', s, [rfReplaceAll]);
  fName := StringReplace(fName, '\', s, [rfReplaceAll]);
  fName := StringReplace(fName, '<', s, [rfReplaceAll]);
  fName := StringReplace(fName, '>', s, [rfReplaceAll]);
  fName := StringReplace(fName, '|', s, [rfReplaceAll]);

  Result := fName;
end;
{$hints on}

//function IsValidShortFileName(const FileName: string): Boolean;
//var
//  s: string;
//  x: integer;
//begin
//  s := FileName;
//  x := GetLastCharIndex(s, PathDelim);
//  if x > 0 then s := Copy(s, x + 1, Length(s));
//  Result :=
//    (Trim(FileName) <> '') and
//    (Pos('?', s) = 0) and
//    (Pos('*', s) = 0) and
//    (Pos(':', s) = 0) and
//    (Pos('/', s) = 0) and
//    (Pos('\', s) = 0) and
//    (Pos('<', s) = 0) and
//    (Pos('>', s) = 0) and
//    (Pos('|', s) = 0);
//end;

function IsValidShortFileName(const ShortFileName: string): Boolean;
const
  InvalidChars: set of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
var
  i: integer;
begin
  Result := False;
  if (ShortFileName = '') then Exit;

  for i := 1 to Length(ShortFileName) do
    if CharInSet(ShortFileName[i], InvalidChars) then Exit;

  Result := True;
end;

function IsValidLongFileName(const LongFileName: string): Boolean;
const
  InvalidChars: set of AnsiChar = ['*', '?', '"', '<', '>', '|'];
  {$IFDEF MSWINDOWS}DirSeparators : set of Char = ['\','/'];{$ENDIF}
var
  i: integer;
  {$IFDEF MSWINDOWS}c: Char;{$ENDIF}
begin
  Result := False;
  if (LongFileName = '') then Exit;

  for i := 1 to Length(LongFileName) do
    if CharInSet(LongFileName[i], InvalidChars) then Exit;

  {$IFDEF MSWINDOWS}
  i := Pos(':', LongFileName);
  if (i > 0) then
  begin
    if i <> 2 then Exit;
    c := LongFileName[1];
    if (not CharInSet(c, ['A'..'Z'])) and (not CharInSet(c, ['a'..'z'])) then Exit;
    if Length(LongFileName) < 4 then Exit;
    c := LongFileName[3];
    if not (CharInSet(c, DirSeparators)) then Exit;
  end;
  {$ENDIF}
  Result := True;
end;

function FixFileNameSlashes(const FileName: string): string;
begin
  Result := StringReplace(FileName, '\', PathDelim, [rfReplaceAll]);
  Result := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
end;


function Qs(const s: string): string;
begin
  Result := '"' + s + '"';
end;

{$hints off}
function Rbs(Dir: string): string;
begin
  if Copy(Dir, Length(Dir), 1) = '.' then Delete(Dir, Length(Dir), 1);
  Dir := ExcludeTrailingPathDelimiter(Dir);
  //if Copy(Dir, Length(Dir) - 1, 2) = '\.' then Delete(Dir, Length(Dir) - 1, 2);
  Result := Dir;
end;
{$hints on}


function IntToStrEx(const x: int64; c: Char = ' '): string;
var
  s: string;
  i, k, Len: integer;
begin
  s := IntToStr(x);

  k := Length(s) div 3;
  Len := Length(s);
  for i := 1 to k do Insert(c, s, Len - (i * 3) + 1);

  Result := Trim(s);
end;

function IntToStrEx(const x: integer; c: Char): string;
var
  s: string;
  i, k, Len: integer;
begin
  s := IntToStr(x);

  k := Length(s) div 3;
  Len := Length(s);
  for i := 1 to k do
  begin
    Insert(c, s, Len - (i * 3) + 1);
  end;

  Result := Trim(s);
end;


end.


