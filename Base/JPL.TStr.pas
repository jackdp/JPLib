unit JPL.TStr;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}


interface

uses 
  Windows, SysUtils, Classes, Types,
  JPL.Strings, JPL.Conversion;


type

  TStr = record
  private
    class function GetDecimalSeparator: Char; static;
    class procedure SetDecimalSeparator(const Value: Char); static;
    class function GetThousandSeparator: Char; static;
    class procedure SetThousandSeparator(const Value: Char); static;
    class function GetDateSeparator: Char; static;
    class procedure SetDateSeparator(const Value: Char); static;
    class function GetTimeSeparator: Char; static;
    class procedure SetTimeSeparator(const Value: Char); static;
  public
    class function Empty(const s: string): Boolean; static;

    class function Pad(Text: string; Len: integer; PaddingChar: Char = ' '): string; overload; static;
    class function Pad(const x: integer; Len: integer; PaddingChar: Char = '0'): string; overload; static;
    class function PadRight(Text: string; Len: integer; PaddingChar: Char = ' '): string; overload; static;
    class function PadRight(const x: integer; Len: integer; PaddingChar: Char = '0'): string; overload; static;

    class function RemoveTrailingPathDelimiter(Dir: string): string; static;

    class function RemoveSpaces(const s: string): string; static;
    class function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string; static;
    class function RemoveChars(const SrcStr, CharsToRemove: string; IgnoreCase: Boolean = False): string; overload; static;
    class function RemoveChars(const SrcStr: string; Chars: array of Char; IgnoreCase: Boolean = False): string; overload; static;
    class function ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string; static;
    class function ReplaceFirst(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string; static;

    class function TrimAndUp(const s: string): string; static;
    class function TrimAndLow(const s: string): string; static;
    class function InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3; MaxInsertions: integer = 255): string; overload; static;
    class function InsertNumSep(const x: integer; Separator: string = ' '; NumBlockSize: integer = 3; MaxInsertions: integer = 255): string; overload; static;

    {$IFDEF DELPHI2009_OR_BELOW}
    class procedure SplitStrToArray(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak); static;
    {$ELSE}
    class procedure SplitStrToArray(s: string; var Arr: TArray<string>; const EndLineStr: string = sLineBreak); static;
    {$ENDIF}
    class function SplitStr(const InStr: string; out LeftStr, RightStr: string; const Separator: string): Boolean; static;

    class function AddBounds(const s: string; LeftBound: string = '['; RightBound: string = ']'): string; static;
    class function TrimBounds(s: string; LeftBound: string = '['; RightBound: string = ']'): string; static;
    class function IsBoundedWith(const s: string; LeftBound: string = '['; RightBound: string = ']'; IgnoreCase: Boolean = False): Boolean; static;

    class function GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string; static;
    class function GetRandomIntStr(Len: integer = 10): string; static;

    class function IsValidHexStr(HexStr: string; IgnoreSpaces: Boolean = False): Boolean; static;
    class function IsValidIntStr(IntStr: string; IgnoreSpaces: Boolean = False): Boolean; static;

    class function Parse(const x: integer): string; overload; static;
    class function Parse(const x: Int64): string; overload; static;
    class function Parse(const x: Cardinal): string; overload; static;
    class function Parse(const x: UInt64): string; overload; static;

    class function PosCS(const SubStr, s: string; bCaseSensitive: Boolean = False): integer; static;
    class function PosOf(const SubStr, s: string; IgnoreCase: Boolean = False): integer; static;
    class function SubString(const s: string; const StartPos: integer): string; overload; static;
    class function SubString(const s: string; const StartPos, Len: integer): string; overload; static;

    class function Contains(const s, SubStr: string; IgnoreCase: Boolean = False): Boolean; static;
    class function StartsStr(const SubStr, s: string): Boolean; static;
    class function EndsStr(const SubStr, s: string): Boolean; static;
    class function StartsText(const SubStr, s: string): Boolean; static;
    class function EndsText(const SubStr, s: string): Boolean; static;
    class function StartsWith(const SubStr, s: string; IgnoreCase: Boolean = False): Boolean; static;
    class function EndsWith(const SubStr, s: string; IgnoreCase: Boolean = False): Boolean; static;

    class function TrimFromEnd(const s: string; const StringToCut: string): string; static;
    class function TrimFromStart(const s: string; const StringToCut: string): string; static;

    class function CharCount(const s: string; const AChar: Char): integer; static;
    class function FirstCharPos(const s: string; const AChar: Char): integer; static;
    class function LastCharPos(const s: string; const AChar: Char): integer; static;

    class property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    class property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    class property DateSeparator: Char read GetDateSeparator write SetDateSeparator;
    class property TimeSeparator: Char read GetTimeSeparator write SetTimeSeparator;
  end;
   
  

implementation



class function TStr.Empty(const s: string): Boolean;
begin
  Result := s = '';
end;

class function TStr.Pad(Text: string; Len: integer; PaddingChar: Char = ' '): string;
begin
  Result := JPL.Strings.Pad(Text, Len, PaddingChar);
end;

class function TStr.Pad(const x: integer; Len: integer; PaddingChar: Char = '0'): string;
begin
  Result := JPL.Strings.Pad(x, Len, PaddingChar);
end;

class function TStr.PadRight(Text: string; Len: integer; PaddingChar: Char = ' '): string;
begin
  Result := JPL.Strings.PadRight(Text, Len, PaddingChar);
end;

class function TStr.PadRight(const x: integer; Len: integer; PaddingChar: Char = '0'): string;
begin
  Result := JPL.Strings.PadRight(x, Len, PaddingChar);
end;

class function TStr.RemoveTrailingPathDelimiter(Dir: string): string;
begin
  Result := JPL.Strings.Rbs(Dir);
end;

class function TStr.RemoveSpaces(const s: string): string;
begin
  Result := JPL.Strings.RemoveSpaces(s);
end;

class function TStr.RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string;
begin
  Result := JPL.Strings.RemoveAll(Text, ToRemove, IgnoreCase);
end;

class function TStr.RemoveChars(const SrcStr, CharsToRemove: string; IgnoreCase: Boolean = False): string;
begin
  Result := JPL.Strings.RemoveChars(SrcStr, CharsToRemove, IgnoreCase);
end;

class function TStr.RemoveChars(const SrcStr: string; Chars: array of Char; IgnoreCase: Boolean): string;
begin
  Result := JPL.Strings.RemoveChars(SrcStr, Chars, IgnoreCase);
end;

class function TStr.ReplaceAll(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
begin
  Result := JPL.Strings.ReplaceAll(SrcStr, OldStr, NewStr, IgnoreCase);
end;

class function TStr.ReplaceFirst(const SrcStr, OldStr, NewStr: string; IgnoreCase: Boolean = False): string;
begin
  Result := JPL.Strings.ReplaceFirst(SrcStr, OldStr, NewStr, IgnoreCase);
end;

class function TStr.TrimAndUp(const s: string): string;
begin
  Result := Trim(AnsiUpperCase(s));
end;

class function TStr.TrimAndLow(const s: string): string;
begin
  Result := Trim(AnsiLowerCase(s));
end;

class function TStr.InsertNumSep(NumStr: string; Separator: string = ' '; NumBlockSize: integer = 3; MaxInsertions: integer = 255): string;
begin
  Result := JPL.Strings.InsertNumSep(NumStr, Separator, NumBlockSize, MaxInsertions);
end;

class function TStr.InsertNumSep(const x: integer; Separator: string; NumBlockSize, MaxInsertions: integer): string;
begin
  Result := JPL.Strings.InsertNumSep(IntToStr(x), Separator, NumBlockSize, MaxInsertions);
end;

{$IFDEF DELPHI2009_OR_BELOW}
class procedure TStr.SplitStrToArray(s: string; var Arr: TStringDynArray; const EndLineStr: string = sLineBreak);
{$ELSE}
class procedure TStr.SplitStrToArray(s: string; var Arr: TArray<string>; const EndLineStr: string = sLineBreak);
{$ENDIF}
begin
  JPL.Strings.SplitStrToArray(s, Arr, EndLineStr);
end;

class function TStr.SplitStr(const InStr: string; out LeftStr, RightStr: string; const Separator: string): Boolean;
begin
  Result := JPL.Strings.SplitStr(InStr, LeftStr, RightStr, Separator);
end;

class function TStr.AddBounds(const s: string; LeftBound, RightBound: string): string;
begin
  Result := JPL.Strings.AddBounds(s, LeftBound, RightBound);
end;

class function TStr.TrimBounds(s: string; LeftBound: string = '['; RightBound: string = ']'): string;
begin
  Result := JPL.Strings.TrimBounds(s, LeftBound, RightBound);
end;

class function TStr.IsBoundedWith(const s: string; LeftBound: string = '['; RightBound: string = ']'; IgnoreCase: Boolean = False): Boolean;
begin
  Result := JPL.Strings.IsBoundedWith(s, LeftBound, RightBound, IgnoreCase);
end;

class function TStr.GetRandomHexStr(Bytes: integer = 4; ByteSeparator: string = ''; bLowerCase: Boolean = False): string;
begin
  Result := JPL.Strings.GetRandomHexStr(Bytes, ByteSeparator, bLowerCase);
end;

class function TStr.GetRandomIntStr(Len: integer = 10): string;
begin
  Result := JPL.Strings.GetRandomIntStr(Len);
end;

class function TStr.IsValidHexStr(HexStr: string; IgnoreSpaces: Boolean): Boolean;
begin
  Result := JPL.Conversion.IsValidHexStr(HexStr, IgnoreSpaces);
end;

class function TStr.IsValidIntStr(IntStr: string; IgnoreSpaces: Boolean): Boolean;
begin
  Result := JPL.Conversion.IsValidIntStr(IntStr, IgnoreSpaces);
end;

class function TStr.Parse(const x: integer): string;
begin
  Result := JPL.Conversion.itos(x);
end;

class function TStr.Parse(const x: Int64): string;
begin
  Result := JPL.Conversion.itos(x);
end;

class function TStr.Parse(const x: Cardinal): string;
begin
  Result := JPL.Conversion.itos(x);
end;

class function TStr.Parse(const x: UInt64): string;
begin
  Result := JPL.Conversion.itos(x);
end;

class function TStr.PosCS(const SubStr, s: string; bCaseSensitive: Boolean = False): integer;
begin
  Result := JPL.Strings.PosCS(SubStr, s, bCaseSensitive);
end;

class function TStr.PosOf(const SubStr, s: string; IgnoreCase: Boolean = False): integer;
begin
  if IgnoreCase then Result := JPL.Strings.PosCS(SubStr, s, False)
  else Result := Pos(SubStr, s);
end;

class function TStr.SubString(const s: string; const StartPos: integer): string;
begin
  Result := Copy(s, StartPos, Length(s));
end;

class function TStr.SubString(const s: string; const StartPos, Len: integer): string;
begin
  Result := Copy(s, StartPos, Len);
end;

class function TStr.Contains(const s, SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := TStr.PosCS(SubStr, s, not IgnoreCase) > 0;
end;

class function TStr.StartsStr(const SubStr, s: string): Boolean;
begin
  Result := Copy(s, 1, Length(SubStr)) = SubStr;
end;

class function TStr.EndsStr(const SubStr, s: string): Boolean;
begin
  Result := Copy(s, Length(s) - Length(SubStr) + 1, Length(SubStr)) = SubStr;
end;

class function TStr.StartsText(const SubStr, s: string): Boolean;
begin
  Result := Copy(AnsiUpperCase(s), 1, Length(SubStr)) = AnsiUpperCase(SubStr);
end;

class function TStr.EndsText(const SubStr, s: string): Boolean;
begin
  Result := Copy(AnsiUpperCase(s), Length(s) - Length(SubStr) + 1, Length(SubStr)) = AnsiUpperCase(SubStr);
end;

class function TStr.StartsWith(const SubStr, s: string; IgnoreCase: Boolean = False): Boolean;
begin
  if IgnoreCase then Result := TStr.StartsText(SubStr, s)
  else Result := TStr.StartsStr(SubStr, s);
end;

class function TStr.EndsWith(const SubStr, s: string; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then Result := TStr.EndsText(SubStr, s)
  else Result := TStr.EndsStr(SubStr, s);
end;

class function TStr.TrimFromEnd(const s, StringToCut: string): string;
begin
  Result := JPL.Strings.TrimFromEnd(s, StringToCut);
end;

class function TStr.TrimFromStart(const s, StringToCut: string): string;
begin
  Result := JPL.Strings.TrimFromStart(s, StringToCut);
end;

class function TStr.CharCount(const s: string; const AChar: Char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = AChar then Inc(Result);
end;

class function TStr.FirstCharPos(const s: string; const AChar: Char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = AChar then
    begin
      Result := i;
      Break;
    end;
end;

class function TStr.LastCharPos(const s: string; const AChar: Char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] = AChar then
    begin
      Result := i;
      Break;
    end;
end;

class function TStr.GetDecimalSeparator: Char;
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

class procedure TStr.SetDecimalSeparator(const Value: Char);
begin
  {$IFDEF FPC}FormatSettings.DecimalSeparator = Value;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    FormatSettings.DecimalSeparator := Value;
    {$ELSE}
    DecimalSeparator := Value;
    {$ENDIF}
  {$ENDIF}
end;

class function TStr.GetThousandSeparator: Char;
begin
  {$IFDEF FPC}Result := FormatSettings.ThousandSeparator;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    Result := FormatSettings.ThousandSeparator;
    {$ELSE}
    Result := ThousandSeparator;
    {$ENDIF}
  {$ENDIF}
end;

class procedure TStr.SetThousandSeparator(const Value: Char);
begin
  {$IFDEF FPC}FormatSettings.ThousandSeparator = Value;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    FormatSettings.ThousandSeparator := Value;
    {$ELSE}
    ThousandSeparator := Value;
    {$ENDIF}
  {$ENDIF}
end;

class function TStr.GetDateSeparator: Char;
begin
  {$IFDEF FPC}Result := FormatSettings.DateSeparator;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    Result := FormatSettings.DateSeparator;
    {$ELSE}
    Result := DateSeparator;
    {$ENDIF}
  {$ENDIF}
end;

class procedure TStr.SetDateSeparator(const Value: Char);
begin
  {$IFDEF FPC}FormatSettings.DateSeparator = Value;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    FormatSettings.DateSeparator := Value;
    {$ELSE}
    DateSeparator := Value;
    {$ENDIF}
  {$ENDIF}
end;

class function TStr.GetTimeSeparator: Char;
begin
  {$IFDEF FPC}Result := FormatSettings.TimeSeparator;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    Result := FormatSettings.TimeSeparator;
    {$ELSE}
    Result := TimeSeparator;
    {$ENDIF}
  {$ENDIF}
end;

class procedure TStr.SetTimeSeparator(const Value: Char);
begin
  {$IFDEF FPC}FormatSettings.TimeSeparator = Value;{$ENDIF}
  {$IFDEF DCC}
    {$IFDEF DELPHIXE_OR_ABOVE}
    FormatSettings.TimeSeparator := Value;
    {$ELSE}
    TimeSeparator := Value;
    {$ENDIF}
  {$ENDIF}
end;




end.