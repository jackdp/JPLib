unit JPL.Strings.Ext;

{
  Jacek Pazera
  http://www.pazera-software.com
 }

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes, StrUtils;


procedure SplitStrToList(LineToParse: string; var List: TStringList; DataSeparator: string = ',');
function SaveStringToFile(const FileName, FileContent: string; Encoding: TEncoding): Boolean;
function GetLineStartingWith(List: TStrings; const TextToFind: string; IgnoreCase: Boolean = True; StartIndex: integer = 0): string;
procedure ReverseStrings(List: TStrings);


implementation


function GetLineStartingWith(List: TStrings; const TextToFind: string; IgnoreCase: Boolean = True; StartIndex: integer = 0): string;
var
  i: integer;
  b: Boolean;
begin
  Result := '';
  for i := StartIndex to List.Count - 1 do
  begin
    if IgnoreCase then b := AnsiStartsText(TextToFind, List[i])
    else b := AnsiStartsStr(TextToFind, List[i]);
    if b then
    begin
      Result := List[i];
      Break;
    end;
  end;
end;

function SaveStringToFile(const FileName, FileContent: string; Encoding: TEncoding): Boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FileContent;
    try
      {$IFDEF DCC}sl.SaveToFile(FileName, Encoding);{$ENDIF}
      {$IFDEF FPC}
        {$IFDEF HAS_SAVE_WITH_ENCODING}
        sl.SaveToFile(FileName, Encoding);
        {$ELSE}
        sl.SaveToFile(FileName);
        {$ENDIF}
      {$ENDIF}
      Result := True;
    except
      Result := False;
    end;
  finally
    sl.Free;
  end;
end;

procedure SplitStrToList(LineToParse: string; var List: TStringList; DataSeparator: string = ',');
var
  xp: integer;
  s: string;
begin
  if not Assigned(List) then Exit;

  xp := Pos(DataSeparator, LineToParse);
  while xp > 0 do
  begin
    s := Trim(Copy(LineToParse, 1, xp - 1));
    List.Add(s);
    Delete(LineToParse, 1, xp + Length(DataSeparator) - 1);
    LineToParse := Trim(LineToParse);
    xp := Pos(DataSeparator, LineToParse);
  end;

  if LineToParse <> '' then
  begin
    LineToParse := Trim(LineToParse);
    if LineToParse <> '' then List.Add(LineToParse);
  end;

end;

procedure ReverseStrings(List: TStrings);
var
  i, xCount, xInd: integer;
  stemp: string;
begin
  xCount := List.Count;
  if xCount <= 1 then Exit;

  for i := 0 to (xCount div 2) - 1 do
  begin
    xInd := xCount - 1 - i;
    stemp := List[i];
    List[i] := List[xInd];
    List[xInd] := stemp;
  end;
end;


end.
