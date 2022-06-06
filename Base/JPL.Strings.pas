unit JPL.Strings;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  To jest mój stary moduł z roku 2000 dla Borland Pascala 7.0
  W kolejnych latach rozbudowywany i dostosowywany do nowszych wersji Delphi i FPC.
 }


{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}


interface

uses
  SysUtils, Types, JPL.Bytes;

const
  CR = #13;
  CRCR = #13#13;
  LF = #10;
  LFLF = #10#10;
  CRLF = #13#10;
  TAB = #9;
  ENDL = sLineBreak;
  ENDL2 = ENDL + ENDL;
  ENDL3 = ENDL2 + ENDL;
  ENDL4 = ENDL3 + ENDL;
  DEG = '°';

type
  TSpecialChars = record
    const Tab = #9;
    const Euro = '€';
    const Copyright = '©';
    const Reg = '®';
    const Paragraph = '¶';
    const Section = '§';
    const Degree = '°';
    const Sup2 = '²';
    const Sup3 = '³';
    const Integral = '∫';
    const Micro = 'µ';
    const PlusMinus = '±';
    const Times = '×';
    const Divide = '÷';
    const OmegaBig = 'Ω';
    const AlphaSmall = 'α';
    const BetaSmall = 'β';
    const GammaSmall = 'γ';
    const DeltaSmall = 'δ';
    const DeltaBig = 'Δ';
    const PiSmall = 'π';
    const PiBig = 'Π';
    const SigmaBig = 'Σ';
    const Bullet = '•';
    const DashLong = '–';
    const Trademark = '™';
  end;


{$IFDEF DELPHI2009_OR_BELOW}
type
  TArray<T> = array of T;
{$ENDIF}

function Rbs(Dir: string): string; // removes path delimiter from end
function Qs(const s: string): string; // adds " at the beginning and end of the input string
function Capitalize(const s: string): string;
function FixFileName(fName: string; s: string = '_'; ZamieniajCudzyslowy: Boolean = True): string;
function IsValidShortFileName(const ShortFileName: string): Boolean;
function IsValidLongFileName(const LongFileName: string): Boolean;
function FixFileNameSlashes(const FileName: string): string; deprecated {$IFDEF DCC}{$IF CompilerVersion > 19}'Use FixPathDelimiters instead'{$IFEND}{$ENDIF};
function FixPathDelimiters(const FileName: string): string;
function PadString(Text: string; i: integer; znak: Char = ' '): string;
function Pad(Text: string; Len: integer; PaddingChar: Char = ' '): string; overload;
function Pad(const x: integer; Len: integer; PaddingChar: Char = '0'): string; overload;
function Pad(const x: Int64; Len: integer; PaddingChar: Char = '0'): string; overload;
function PadRight(Text: string; Len: integer; PaddingChar: Char = ' '): string; overload;
function PadRight(const x: integer; Len: integer; PaddingChar: Char = ' '): string; overload;
function PadRight(const x: Int64; Len: integer; PaddingChar: Char = ' '): string; overload;
function UnquoteStr(s: string; bDoubleQuote: Boolean = True): string;
function IntToStrEx(const x: int64; c: Char = ' '): string; overload;
function IntToStrEx(const x: integer; c: Char = ' '): string; overload;
function IntToStrEx(const x: UInt64; c: Char = ' '): string; overload;

function GetLastCharIndex(const s: string; c: Char): integer;
function GetLastBIndex(const s: string): integer; deprecated {$IFDEF DCC}{$IF CompilerVersion > 19}'Use GetLastBackslashIndex instead'{$IFEND}{$ENDIF};
function GetLastBackslashIndex(const s: string): integer;

function DelFirstCharInString(s: string; ToDelete: Char): string;
function DelLastCharInString(s: string; ToDelete: Char): string;
function RemoveNum(const s: string): string;

//function AddSlashes(Text: string): string;
function RemoveSlashes(const Text: string): string;
function RemoveSpaces(const s: string): string;
function ReplaceSpecialChars(s: string; sc: Char = '_'): string;

function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string; overload;
function RemoveAll(const Text: string; const StringsToRemove: array of string; IgnoreCase: Boolean = False): string; overload;
function RemoveNonLetters(s: string): string;
function ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
function ReplaceFirst(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
function ReplaceDecimalSeparator(const FloatStr: string; NewSeparator: string = '.'): string;


function RemovePolishChars(s: string): string;
{$IFDEF DCC}
function IsBigLetterPL(c: Char): Boolean;
function IsSmallLetterPL(c: Char): Boolean;
function IsLetterPL(c: Char): Boolean;
{$ENDIF}

function IsBigLetter(const c: Char): Boolean;
function IsSmallLetter(const c: Char): Boolean;
function IsLetter(const c: Char): Boolean;
function IsNumber(const c: Char): Boolean;
function LastCharIsNumber(const s: string): Boolean;

function DistinctChars(s: string; IgnoreCase: Boolean = True): Boolean;
function MakeDistinctChars(s: string): string;
function CharCount(c: Char; s: string): integer;
function ReverseStr(const s: string): string;
function FillStrToLen(s: string; Len: integer; FillValue: Char = ' '): string;
function AnsiUpCase(zn: Char; Default: Char = #0): Char;
function AnsiLowCase(zn: Char; Default: Char = #0): Char;
function RemoveChars(const SrcStr, CharsToRemove: string; IgnoreCase: Boolean = False): string; overload;
function RemoveChars(const SrcStr: string; Chars: array of Char; IgnoreCase: Boolean = False): string; overload;
function LeaveOnlyChars(const SrcStr, CharsToLeave: string): string;
function RemoveNonDecimals(const s: string): string;
function CutStrBefore(s, CutBeforeText: string; IgnoreCase: Boolean = False): string;
function CutStrAfter(s, CutAfterText: string; IgnoreCase: Boolean = False; IncludeSearchText: Boolean = True): string;

function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True): string;

function HtmlStringToStr(HTMLStr: string; IgnoreCase: Boolean = False): string; deprecated {$IFDEF DCC}{$IF CompilerVersion > 19}'Use ReplaceHtmlEntities instead'{$IFEND}{$ENDIF};
function ReplaceHtmlEntities(const AStr: string; IgnoreCase: Boolean = False): string;

function GetHref(const InStr: string): string;
function GetAnchorText(const InStr: string; bReplaceHtmlEntities: Boolean = True): string;
function GetFirstDigitIndex(const s: string): integer;
function GetFirstNonDigitIndex(const s: string): integer;

function MyDir(bExcludeTrailingPathDelim: Boolean = True): string; // Returns the ParamStr(0) directory

function StrRemove(const s, StringToRemove: string; IgnoreCase: Boolean = False): string;
function GetFileSizeString(const FileSize: Int64; BytesStr: string = ' bytes'): string;

function TrimUp(s: string): string;
function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3; MaxInsertions: integer = 255): string;
function CopyString(const s: string; Copies: integer = 2): string;

//procedure StrToList(LineToParse: string; var List: TStringList; Separator: string = ',');
{$IFDEF DELPHI2009_OR_BELOW}
procedure SplitStrToArray(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak);
{$ELSE}
procedure SplitStrToArray(s: string; var Arr: TArray<string>; const EndLineStr: string = sLineBreak);
{$ENDIF}

// The SplitStrToArrayEx procedure uses the initial allocation of the size of the array and incrementing its size by the ArrDeltaSize value
// to avoid increasing the size of the array repeatedly by 1.
procedure SplitStrToArrayEx(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak; ArrDeltaSize: WORD = 100);
function RemoveEmptyStrings(var Arr: TStringDynArray): integer; // Returns the number of removed empty strings
function SplitStr(const InStr: string; out LeftStr, RightStr: string; const Separator: string): Boolean; overload;
function SplitStr(const InStr: string; out LeftInt, RightInt: integer; const Separator: string): Boolean; overload;

function TrimBounds(s: string; LeftBound, RightBound: string): string;
function IsBoundedWith(const s: string; LeftBound, RightBound: string; IgnoreCase: Boolean = False): Boolean;
function AddBounds(const s: string; LeftBound, RightBound: Char): string; overload;
function AddBounds(const s: string; LeftBound, RightBound: string): string; overload;
function AddBounds(const s: string; StringToBoundSeparator: string = ' '; BoundChar: Char = '-'; BoundLen: Integer = 16): string; overload;

function EnsureBounds(const s: string; LeftBound, RightBound: string): string;
function EnsureLeftBound(const s, BoundStr: string): string;
function EnsureRightBound(const s, BoundStr: string): string;

function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
function GetRandomIntStr(Len: integer = 10): string;

// Case sensitive Pos
function PosCS(const substr, s: string; bCaseSensitive: Boolean = False): integer;

function TrimFromCharPosToEnd(const s: string; const AChar: Char): string;
function TrimFromStrPosToEnd(const Src, AStr: string): string;
function TrimFromEnd(const s: string; const StringToCut: string): string;
function TrimFromStart(const s: string; const StringToCut: string): string;
function TrimENDL(const s: string): string; // removes trailing sLineBreak (ENDL)
function TrimExtDot(const FileExtension: string): string;
function AddFileNameSuffix(const FileName, Suffix: string): string;
function AddFileNamePrefix(const FileName, Prefix: string): string;
function TrimFileExt(const FileName: string): string;
function BaseFileName(const FileName: string): string; // returns file name without path and extension
procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);
function PathIsAbsolute(const FileName: string): Boolean;

function GetDecimalSeparator: Char;

function SaveStringToFile(const FileName, Content: string; Encoding: TEncoding; const WriteBOM: Boolean = True):Boolean; overload;
function SaveStringToFile(const FileName, Content: string): Boolean; overload;
function LoadStringFromFile(const FileName: string; var s: string; Encoding: TEncoding): Boolean; overload;
function LoadStringFromFile(const FileName: string; var s: string): Boolean; overload;


implementation


{$IFDEF FPC}
function SaveStringToFile(const FileName, Content: string; Encoding: TEncoding; const WriteBOM: Boolean = True): Boolean; overload;
var
  BOM: TBytes = nil;
  ByteContent: TBytes = nil;
begin
  SetLength(BOM, 0);
  if WriteBOM then BOM := Encoding.GetPreamble;

  SetLength(ByteContent, 0);
  {$IFDEF FPC320_OR_ABOVE}
  if Length(BOM) > 0 then ConcatTBytes(BOM, Encoding.GetAnsiBytes(Content), ByteContent)
  else ByteContent := Encoding.GetAnsiBytes(Content);
  {$ELSE}
  if Length(BOM) > 0 then ConcatTBytes(BOM, Encoding.GetBytes(UnicodeString(Content)), ByteContent)
  else ByteContent := Encoding.GetBytes(UnicodeString(Content));
  {$ENDIF}

  Result := SaveBytesToFile(FileName, ByteContent);
end;
{$ELSE}
function SaveStringToFile(const FileName, Content: string; Encoding: TEncoding; const WriteBOM: Boolean = True): Boolean; overload;
var
  BOM: TBytes {$IFDEF FPC}= nil{$ENDIF};
  ByteContent: TBytes {$IFDEF FPC}= nil{$ENDIF};
begin
  SetLength(BOM, 0);
  if WriteBOM then BOM := Encoding.GetPreamble;

  SetLength(ByteContent, 0);
  if Length(BOM) > 0 then ConcatTBytes(BOM, Encoding.GetBytes(Content), ByteContent)
  else ByteContent := Encoding.GetBytes(Content);

  Result := SaveBytesToFile(FileName, ByteContent);
end;
{$ENDIF}

function SaveStringToFile(const FileName, Content: string): Boolean; overload;
begin
  Result := SaveStringToFile(FileName, Content, TEncoding.Default);
end;

{$IFDEF FPC}
function LoadStringFromFile(const FileName: string; var s: string; Encoding: TEncoding): Boolean; overload;
var
  Bytes: TBytes = nil;
  xBomLen: integer;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  Result := GetFileContentAsBytes(FileName, Bytes);
  if Result then
  begin
    Encoding := nil;
    // http://docwiki.embarcadero.com/Libraries/Sydney/en/System.SysUtils.TEncoding.GetBufferEncoding
    // https://www.freepascal.org/docs-html/rtl/sysutils/tencoding.getbufferencoding.html
    // GetBufferEncoding detects encoding of Bytes, and assigns detected encoding to the Encoding param.
    // So after GetBufferEncoding, the Encoding is NOT = nil
    xBomLen := TEncoding.GetBufferEncoding(Bytes, Encoding, TEncoding.Default);
    {$IFDEF FPC320_OR_ABOVE}
    s := Encoding.GetAnsiString(Bytes, xBomLen, Length(Bytes) - xBomLen);
    {$ELSE}
    s := string(Encoding.GetString(Bytes, xBomLen, Length(Bytes) - xBomLen));
    {$ENDIF}
  end;
end;
{$ELSE}
function LoadStringFromFile(const FileName: string; var s: string; Encoding: TEncoding): Boolean; overload;
var
  Bytes: TBytes;
  xBomLen: integer;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  Result := GetFileContentAsBytes(FileName, Bytes);
  if Result then
  begin
    Encoding := nil;
    // http://docwiki.embarcadero.com/Libraries/Sydney/en/System.SysUtils.TEncoding.GetBufferEncoding
    // https://www.freepascal.org/docs-html/rtl/sysutils/tencoding.getbufferencoding.html
    // GetBufferEncoding detects encoding of Bytes, and assigns detected encoding to the Encoding param.
    // So after GetBufferEncoding, the Encoding is NOT = nil
    xBomLen := TEncoding.GetBufferEncoding(Bytes, Encoding {$IFDEF DELPHIXE_OR_ABOVE}, TEncoding.Default{$ENDIF});
    s := Encoding.GetString(Bytes, xBomLen, Length(Bytes) - xBomLen);
  end;
end;
{$ENDIF}

function LoadStringFromFile(const FileName: string; var s: string): Boolean; overload;
begin
  Result := LoadStringFromFile(FileName, s, TEncoding.Default);
end;

function GetDecimalSeparator: Char;
begin
  {$IFDEF FPC}Result := FormatSettings.DecimalSeparator;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    Result := FormatSettings.DecimalSeparator;
    {$ELSE}
    Result := DecimalSeparator;
    {$ENDIF}
  {$ENDIF}
end;

function PathIsAbsolute(const FileName: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (Length(FileName) >= 3) and CharInSet(FileName[1], ['A'..'Z','a'..'z']) and (FileName[2] = ':') and CharInSet(FileName[3], ['/', '\']);
  {$ELSE}
  // UNIX / Linux
  Result := (Length(FileName) > 0) and (FileName[1] = '/');
  {$ENDIF}
end;

function SplitStr(const InStr: string; out LeftStr, RightStr: string; const Separator: string): Boolean;
var
  xp: integer;
begin
  Result := False;
  xp := Pos(Separator, InStr);
  if xp <= 0 then Exit;
  LeftStr := Copy(InStr, 1, xp - 1);
  RightStr := Copy(InStr, xp + Length(Separator), Length(InStr));
  Result := True;
end;

function SplitStr(const InStr: string; out LeftInt, RightInt: integer; const Separator: string): Boolean; overload;
var
  sLeft, sRight: string;
begin
  Result := False;
  if not SplitStr(InStr, sLeft, sRight, Separator) then Exit;
  if not TryStrToInt(sLeft, LeftInt) then Exit;
  if not TryStrToInt(sRight, RightInt) then Exit;
  Result := True;
end;

function Capitalize(const s: string): string;
var
  i: integer;
  bNeedUp: Boolean;
  AChar: Char;
begin
  if Pos(' ', s) = 0 then Exit(s);
  bNeedUp := True;
  Result := '';
  for i := 1 to Length(s) do
  begin
    AChar := s[i];
    if AChar = ' ' then
    begin
      bNeedUp := True;
      Continue;
    end;
    if bNeedUp then AChar := UpCase(AChar);
    Result := Result + AChar;
    bNeedUp := False;
  end;
end;

procedure SplitFileName(fName: string; out Dir, BaseFileName, Ext: string; bIncludePathDelimiter: Boolean = True; bRemoveDotFromExt: Boolean = False);
begin
  Dir := ExtractFileDir(fName);
  if bIncludePathDelimiter then Dir := IncludeTrailingPathDelimiter(Dir) else Dir := ExcludeTrailingPathDelimiter(Dir);

  BaseFileName := ExtractFileName(fName);
  BaseFileName := ChangeFileExt(BaseFileName, '');

  Ext := GetFileExt(fName, bRemoveDotFromExt);
end;

function AddFileNameSuffix(const FileName, Suffix: string): string;
var
  Dir, ShortName, Ext: string;
begin
  if Suffix = '' then Exit(FileName);
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

function TrimFileExt(const FileName: string): string;
begin
  Result := ChangeFileExt(FileName, '');
end;

function BaseFileName(const FileName: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(FileName, ''));
end;

function TrimENDL(const s: string): string;
begin
  Result := TrimFromEnd(s, ENDL);
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

function TrimFromCharPosToEnd(const s: string; const AChar: Char): string;
var
  xp: integer;
begin
  xp := Pos(AChar, s);
  if xp >= 1 then Result := Copy(s, 1, xp - 1)
  else Result := s;
end;

function TrimFromStrPosToEnd(const Src, AStr: string): string;
var
  xp: integer;
begin
  xp := Pos(AStr, Src);
  if xp >= 1 then Result := Copy(Src, 1, xp - 1)
  else Result := Src;
end;

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
    Result := Result + IntToHex(bt, 2);
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

function EnsureBounds(const s: string; LeftBound, RightBound: string): string;
var
  x: integer;
begin
  Result := s;
  if Copy(Result, 1, Length(LeftBound)) <> LeftBound then Result := LeftBound + Result;
  x := Length(RightBound);
  if Copy(Result, Length(Result) - x + 1, x) <> RightBound then Result := Result + RightBound;
end;

function EnsureLeftBound(const s, BoundStr: string): string;
begin
  Result := s;
  if Copy(Result, 1, Length(BoundStr)) <> BoundStr then Result := BoundStr + Result;
end;

function EnsureRightBound(const s, BoundStr: string): string;
var
  x: integer;
begin
  Result := s;
  x := Length(BoundStr);
  if Copy(Result, Length(Result) - x + 1, x) <> BoundStr then Result := Result + BoundStr;
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

function IsBoundedWith(const s: string; LeftBound, RightBound: string; IgnoreCase: Boolean = False): Boolean;
var
  sLeft, sRight: string;
begin
  sLeft := Copy(s, 1, Length(LeftBound));
  sRight := Copy(s, Length(s) - Length(RightBound) + 1, Length(RightBound));

  if IgnoreCase then
    Result := ( UpperCase(sLeft) = UpperCase(LeftBound) ) and ( UpperCase(sRight) = UpperCase(RightBound) )
  else
    Result := ( sLeft = LeftBound ) and (sRight = RightBound);
end;

function TrimBounds(s: string; LeftBound, RightBound: string): string;
begin
  if Copy(s, 1, Length(LeftBound)) = LeftBound then s := Copy(s, 1 + Length(LeftBound), Length(s));
  if Copy(s, Length(s) - Length(RightBound) + 1, Length(RightBound)) = RightBound then s := Copy(s, 1, Length(s) - Length(RightBound));
  Result := s;
end;

{$IFDEF DELPHI2009_OR_BELOW}
procedure SplitStrToArray(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak);
{$ELSE}
procedure SplitStrToArray(s: string; var Arr: TArray<string>; const EndLineStr: string = sLineBreak);
{$ENDIF}
var
  x: integer;
begin
  SetLength(Arr, 0);
  if s = '' then Exit;

  x := Pos(EndLineStr, s);
  while x > 0 do
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := Copy(s, 1, x - 1);
    s := Copy(s, x + Length(EndLineStr), Length(s));
    x := Pos(EndLineStr, s);
  end;

  if s <> '' then
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := s;
  end;
end;

// The SplitStrToArrayEx procedure uses the initial allocation of the size of the array and incrementing its size by the ArrDeltaSize value
// to avoid increasing the size of the array repeatedly by 1.
procedure SplitStrToArrayEx(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak; ArrDeltaSize: WORD = 100);
var
  x, xCount: integer;
begin
  SetLength(Arr, 0);
  if s = '' then Exit;

  if ArrDeltaSize = 0 then ArrDeltaSize := 1; // must be greater than 0
  xCount := 0;
  x := Pos(EndLineStr, s);

  while x > 0 do
  begin
    Inc(xCount);
    if Length(Arr) = xCount - 1 then SetLength(Arr, Length(Arr) + ArrDeltaSize);
    Arr[xCount - 1] := Copy(s, 1, x - 1);
    s := Copy(s, x + Length(EndLineStr), Length(s));
    x := Pos(EndLineStr, s);
  end;

  SetLength(Arr, xCount);

  if s <> '' then
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[Length(Arr) - 1] := s;
  end;
end;

function RemoveEmptyStrings(var Arr: TStringDynArray): integer;
var
  A: TStringDynArray {$IFDEF FPC}= nil{$ENDIF};
  i, Len, Ind: integer;
begin
  Result := 0;
  Len := Length(Arr);
  if Len = 0 then Exit;

  SetLength(A, Len);
  Ind := 0;
  for i := 0 to Len - 1 do
  begin

    if Arr[i] = '' then Inc(Result)
    else
    begin
      A[Ind] := Arr[i];
      Inc(Ind);
    end;

  end;

  SetLength(A, Len - Result);
  Arr := A;
end;

function CopyString(const s: string; Copies: integer = 2): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Copies do Result := Result + s;
end;

function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3; MaxInsertions: integer = 255): string;
var
  s: string;
  i, k, Len, LastDigitPos, xp: integer;
begin
  s := NumStr;
  LastDigitPos := Length(s);
  xp := Pos('.', s);
  if xp > 0 then LastDigitPos := xp - 1;

  xp := Pos(GetDecimalSeparator, s);
  if xp > 0 then LastDigitPos := xp - 1;

  Len := LastDigitPos;
  k := Len div NumBlockSize; // k - liczba wstawień separatora

  for i := 1 to k do
  begin
    if i > MaxInsertions then Break;
    Insert(Separator, s, Len - (i * NumBlockSize) + 1);
  end;

  s := TrimFromStart(s, Separator);
  if Copy(s, 1, Length(Separator) + 1) = ('-' + Separator) then Delete(s, 2, Length(Separator));  // - 100 --> -100
  Result := s;
end;

function TrimUp(s: string): string;
begin
  Result := Trim(UpperCase(s));
end;

function GetFileSizeString(const FileSize: Int64; BytesStr: string = ' bytes'): string;
const
  _KB = 1024;
  _MB = _KB * 1024;
  _GB = _MB * 1024;
var
  fs: extended;
  s: ShortString;
  bNegative: Boolean;
  xSize: Int64;
begin
  xSize := FileSize;
  bNegative := xSize < 0;
  if bNegative then xSize := -xSize;

  Result := IntToStr(xSize);
  fs := xSize;
  if fs < _KB then
  begin
    str(fs: 2: 0, s);
    Result := string(s) + BytesStr;
  end
  else if (fs >= _KB) and (fs < _MB) then
  begin
    fs := fs / 1024;
    str(fs: 2: 2, s);
    Result := string(s) + ' KB';
  end
  else if (fs >= _MB) and (fs < _GB) then
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
  if bNegative then Result := '-' + Result;
end;

function StrRemove(const s, StringToRemove: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then Include(rf, rfIgnoreCase);
  Result := StringReplace(s, StringToRemove, '', rf);
end;

function MyDir(bExcludeTrailingPathDelim: Boolean = True): string;
begin
  Result := ExtractFileDir(ParamStr(0));
  if bExcludeTrailingPathDelim then Result := ExcludeTrailingPathDelimiter(Result)
  else Result := IncludeTrailingPathDelimiter(Result);
end;

function GetFirstNonDigitIndex(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if not CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;

function GetFirstDigitIndex(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if CharInSet(s[i], ['0'..'9']) then
    begin
      Result := i;
      Break;
    end;
end;

function GetHref(const InStr: string): string;
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

function GetAnchorText(const InStr: string; bReplaceHtmlEntities: Boolean = True): string;
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
  if bReplaceHtmlEntities then sr := ReplaceHtmlEntities(sr);
  Result := sr;
end;


function HtmlStringToStr(HTMLStr: string; IgnoreCase: Boolean = False): string;
begin
  Result := ReplaceHtmlEntities(HTMLStr, IgnoreCase);
end;

function ReplaceHtmlEntities(const AStr: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
  s: string;
begin
  // HTML entities are case sensitive
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];

  s := AStr;

  s := StringReplace(s, '&lt;', '<', rf);
  s := StringReplace(s, '&gt;', '>', rf);

  s := StringReplace(s, '&euro;', '€', rf);
  s := StringReplace(s, '&cent;', '¢', rf);
  s := StringReplace(s, '&pound;', '£', rf);
  s := StringReplace(s, '&yen;', '¥', rf);

  s := StringReplace(s, '&amp;', '&', rf);
  s := StringReplace(s, '&copy;', '©', rf);
  s := StringReplace(s, '&reg;', '®', rf);
  s := StringReplace(s, '&sect;', '§', rf);
  s := StringReplace(s, '&deg;', '°', rf);
  s := StringReplace(s, '&sup2;', '²', rf);
  s := StringReplace(s, '&sup3;', '³', rf);
  s := StringReplace(s, '&Integral;', '∫', rf);
  s := StringReplace(s, '&micro;', 'µ', rf);
  s := StringReplace(s, '&para;', '¶', rf);
  s := StringReplace(s, '&middot;', '·', rf);
  s := StringReplace(s, '&plusmn;', '±', rf);
  s := StringReplace(s, '&times;', '×', rf);
  s := StringReplace(s, '&divide;', '÷', rf);

  s := StringReplace(s, '&Omega;', 'Ω', rf);
  s := StringReplace(s, '&alpha;', 'α', rf);
  s := StringReplace(s, '&beta;', 'β', rf);
  s := StringReplace(s, '&gamma;', 'γ', rf);
  s := StringReplace(s, '&Gamma;', 'Γ', rf);
  s := StringReplace(s, '&delta;', 'δ', rf);
  s := StringReplace(s, '&Delta;', 'Δ', rf);
  s := StringReplace(s, '&pi;', 'π', rf);
  s := StringReplace(s, '&Pi;', 'Π', rf);
  s := StringReplace(s, '&Sigma;', 'Σ', rf);

  s := StringReplace(s, '&bull;', '•', rf);
  s := StringReplace(s, '&ndash;', '–', rf);
  s := StringReplace(s, '&trade;', '™', rf);
  s := StringReplace(s, '&SmallCircle;', '∘', rf); // &#8728; / &#x02218;

  Result := s;
end;

function GetFileExt(fName: string; bRemoveFirstDot: Boolean = True): string;
begin
  fName := ExtractFileExt(fName);
  if bRemoveFirstDot then
    if Copy(fName, 1, 1) = '.' then Delete(fName, 1, 1);
  Result := fName;
end;

{$hints off}
// obcina łańcuch s po wystąpieniu w nim tekstu CutAfterText
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
// obcina łańcuch s przed wystąpieniem w nim tekstu CutBeforeText
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

function LeaveOnlyChars(const SrcStr, CharsToLeave: string): string;
var
  sr: string;
  c: char;
begin
  sr := '';
  for c in SrcStr do
    if Pos(c, CharsToLeave) > 0 then sr := sr + c;
  Result := sr;
end;

function RemoveNonDecimals(const s: string): string;
begin
  Result := LeaveOnlyChars(s, '0123456789');
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

// A po co to?
function AnsiUpCase(zn: Char; Default: Char): Char;
var
  s: string;
begin
  s := AnsiUpperCase(zn);
  if s = '' then s := Default;
  Result := s[1];
end;

// A po co to?
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

function IsNumber(const c: Char): Boolean;
begin
  //Result := c in ['0'..'9'];
  Result := CharInSet(c, ['0'..'9']);
end;


function LastCharIsNumber(const s: string): Boolean;
begin
  Result := False;
  if Length(s) < 1 then Exit;
  Result := IsNumber(s[Length(s)]);
end;

{$IFDEF DCC}
function IsLetterPL(c: Char): Boolean;
begin
  Result := IsSmallLetterPL(c) or IsBigLetterPL(c);
end;

function IsSmallLetterPL(c: Char): Boolean;
begin
  //Result := c in ['a'..'z', 'ą', 'ć', 'ę', 'ł', 'ó', 'ś', 'ź', 'ż'];
  //97..122 + Polish chars
  Result := CharInSet(c, ['a'..'z', 'ą', 'ć', 'ę', 'ł', 'ó', 'ś', 'ź', 'ż']);
end;

function IsBigLetterPL(c: Char): Boolean;
begin
  //Result := c in ['A'..'Z', 'Ą', 'Ć', 'Ę', 'Ł', 'Ó', 'Ś', 'Ź', 'Ż'];
  // 65..90 + Polish chars
  Result := CharInSet(c, ['A'..'Z', 'Ą', 'Ć', 'Ę', 'Ł', 'Ó', 'Ś', 'Ź', 'Ż']);
end;
{$ENDIF}



function IsLetter(const c: Char): Boolean;
begin
  Result := IsSmallLetter(c) or IsBigLetter(c);
end;

function IsSmallLetter(const c: Char): Boolean;
begin
  Result := CharInSet(c, ['a'..'z']);
end;

function IsBigLetter(const c: Char): Boolean;
begin
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

function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  Result := StringReplace(Text, ToRemove, '', rf);
end;

function RemoveAll(const Text: string; const StringsToRemove: array of string; IgnoreCase: Boolean = False): string; overload;
var
  i: integer;
begin
  Result := Text;
  for i := 0 to Length(StringsToRemove) - 1 do
    Result := RemoveAll(Result, StringsToRemove[i], IgnoreCase);
end;

function ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  Result := StringReplace(SrcStr, OldStr, NewStr, rf);
end;

function ReplaceFirst(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
begin
  if IgnoreCase then rf := [rfIgnoreCase] else rf := [];
  Result := StringReplace(SrcStr, OldStr, NewStr, rf);
end;

function ReplaceDecimalSeparator(const FloatStr: string; NewSeparator: string = '.'): string;
begin
  Result := StringReplace(FloatStr, GetDecimalSeparator, NewSeparator, [rfReplaceAll, rfIgnoreCase]);
end;

{$hints off}
function ReplaceSpecialChars(s: string; sc: Char): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if Ord(s[i]) < 32 then s[i] := sc;
  Result := s;
end;
{$hints on}

function RemoveSlashes(const Text: string): string;
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

function RemoveSpaces(const s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function GetLastBIndex(const s: string): integer;
begin
  Result := GetLastCharIndex(s, '\');
end;

function GetLastBackslashIndex(const s: string): integer;
begin
  Result := GetLastCharIndex(s, '\');
end;

function GetLastCharIndex(const s: string; c: Char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] = c then
    begin
      Result := i;
      Break;
    end;
end;

function RemoveNum(const s: string): string;
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
  s := StringReplace(s, 'ą', 'a', [rfReplaceAll]);
  s := StringReplace(s, 'ć', 'c', [rfReplaceAll]);
  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);
  s := StringReplace(s, 'ł', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'ń', 'n', [rfReplaceAll]);
  s := StringReplace(s, 'ó', 'o', [rfReplaceAll]);
  s := StringReplace(s, 'ś', 's', [rfReplaceAll]);
  s := StringReplace(s, 'ź', 'z', [rfReplaceAll]);
  s := StringReplace(s, 'ż', 'z', [rfReplaceAll]);

  s := StringReplace(s, 'Ą', 'A', [rfReplaceAll]);
  s := StringReplace(s, 'Ć', 'C', [rfReplaceAll]);
  s := StringReplace(s, 'Ę', 'E', [rfReplaceAll]);
  s := StringReplace(s, 'Ł', 'L', [rfReplaceAll]);
  s := StringReplace(s, 'Ń', 'N', [rfReplaceAll]);
  s := StringReplace(s, 'Ó', 'O', [rfReplaceAll]);
  s := StringReplace(s, 'Ś', 'S', [rfReplaceAll]);
  s := StringReplace(s, 'Ź', 'Z', [rfReplaceAll]);
  s := StringReplace(s, 'Ż', 'Z', [rfReplaceAll]);

  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);
  //s := StringReplace(s, 'A', 'a', [rfReplaceAll]);
  s := StringReplace(s, '', 'l', [rfReplaceAll]);
  s := StringReplace(s, 'ť', 'L', [rfReplaceAll]);
  s := StringReplace(s, 'ä', 'n', [rfReplaceAll]);
  s := StringReplace(s, '©', 'e', [rfReplaceAll]);
  s := StringReplace(s, 'ę', 'e', [rfReplaceAll]);

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

function PadString(Text: string; i: integer; znak: Char = ' '): string;
begin
  Result := Pad(Text, i, znak);
end;

{$hints off}
function Pad(Text: string; Len: integer; PaddingChar: Char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if Length(Text) < Len then
  begin
    x := Length(Text);
    y := Len - x;
    for k := 1 to y do
      s := s + PaddingChar;
    Text := s + Text;
  end;
  Result := Text;
end;
{$hints on}

function Pad(const x: integer; Len: integer; PaddingChar: Char = '0'): string;
begin
  Result := Pad(IntToStr(x), Len, PaddingChar);
end;

function Pad(const x: Int64; Len: integer; PaddingChar: Char = '0'): string;
begin
  Result := Pad(IntToStr(x), Len, PaddingChar);
end;

{$hints off}
function PadRight(Text: string; Len: integer; PaddingChar: Char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if Length(Text) < Len then
  begin
    x := Length(Text);
    y := Len - x;
    for k := 1 to y do
      s := s + PaddingChar;
    Text := Text + s;
  end;
  Result := Text;
end;
{$hints on}

function PadRight(const x: integer; Len: integer; PaddingChar: Char = ' '): string;
begin
  Result := PadRight(IntToStr(x), Len, PaddingChar);
end;

function PadRight(const x: Int64; Len: integer; PaddingChar: Char = ' '): string;
begin
  Result := PadRight(IntToStr(x), Len, PaddingChar);
end;


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
  {$IFDEF MSWINDOWS}DirSeparators: set of AnsiChar = ['\','/'];{$ENDIF}
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
  Result := FixPathDelimiters(FileName);
end;

function FixPathDelimiters(const FileName: string): string;
begin
  Result := StringReplace(FileName, '\', PathDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
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

function IntToStrEx(const x: integer; c: Char = ' '): string;
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

function IntToStrEx(const x: UInt64; c: Char = ' '): string;
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


end.


