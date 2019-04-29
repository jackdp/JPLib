unit JPL.Strings.Ext;

{
  Jacek Pazera
  http://www.pazera-software.com
 }


{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  SysUtils, Classes;


procedure SplitStrToList(LineToParse: string; var List: TStringList; DataSeparator: string = ',');
function SaveStringToFile(const FileName, FileContent: string; Encoding: TEncoding): Boolean;


implementation


function SaveStringToFile(const FileName, FileContent: string; Encoding: TEncoding): Boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FileContent;
    try
      sl.SaveToFile(FileName, Encoding);
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


end.
