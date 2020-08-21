unit JPL.TStringsHelper;

{$I .\..\jp.inc}

interface

uses 
  SysUtils, Classes;
  

type

  { TStringsHelper }

  TStringsHelper = class helper for TStrings
    procedure TrimAllLines;
    procedure RemoveEmptyLines;
    procedure TrimAndRemoveEmptyLines;

    // Warning! Use only for lists with a relatively small number of lines (up to several thousand)
    // With larger lists it is very slow!
    procedure RemoveDuplicatesWithoutSorting(IgnoreCase: Boolean = False);

    function LastLine: string;
  end;


implementation




procedure TStringsHelper.RemoveEmptyLines;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
      if Strings[i] = '' then Delete(i);
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.TrimAllLines;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := 0 to Count - 1 do
      Strings[i] := Trim(Strings[i]);
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.TrimAndRemoveEmptyLines;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
    begin
      Strings[i] := Trim(Strings[i]);
      if Strings[i] = '' then Delete(i);
    end;
  finally
    EndUpdate;
  end;
end;


procedure TStringsHelper.RemoveDuplicatesWithoutSorting(IgnoreCase: Boolean);
var
  TempList: TStringList;
  AB: array of Boolean;
  i: integer;
  Line: string;

  function _IsLineDuplicated(const IndStart: integer): Boolean;
  var
    x: integer;
  begin
    Result := False;
    for x := IndStart downto 0 do
      if TempList[x] = Line then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if Count = 0 then Exit;

  // Wyszukiwanie indeksów linii do usunięcia.
  // Tablica AB jest zsynchronizowana z listą.
  // Jeśli linia jest zduplikowana, to w tablicy AB o indeksie równym indeksowi linii ze StringLista
  // zapisywana jest wartość True, w przeciwnym razie - False.
  TempList := TStringList.Create;
  try

    TempList.Assign(Self);
    if IgnoreCase then TempList.Text := UpperCase(TempList.Text);
    SetLength(AB, TempList.Count);

    for i := TempList.Count - 1 downto 0 do
    begin
      Line := TempList[i];
      AB[i] := _IsLineDuplicated(i - 1);
    end;

  finally
    TempList.Free;
  end;


  // Usuwanie duplikatów
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
      if AB[i] then Delete(i);
  finally
    EndUpdate;
  end;
end;

function TStringsHelper.LastLine: string;
begin
  Result := '';
  if Count = 0 then Exit;
  Result := Strings[Count - 1];
end;


end.
