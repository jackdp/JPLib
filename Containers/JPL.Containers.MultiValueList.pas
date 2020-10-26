unit JPL.Containers.MultiValueList;


{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  JPL.TStr;



type

  TMultiValue = record
    No: integer;
    Name: string;
    Description: string;
    StrValue: string;
    IntValue: Int64;
    FloatValue: Double;
    BoolValue: Boolean;
    P: Pointer;
    Tag: integer;
    procedure Clear;
  end;

  TMultiValueList = class(TList<TMultiValue>)
  private
    FSortCaseSensitive: Boolean;
    FSortAscending: Boolean;
    function CompareNums(const Left, Right: TMultiValue): integer;
    function CompareNames(const Left, Right: TMultiValue): integer;
    function CompareDescriptions(const Left, Right: TMultiValue): integer;
    function CompareStrValues(const Left, Right: TMultiValue): integer;
    function CompareIntValues(const Left, Right: TMultiValue): integer;
    function CompareFloatValues(const Left, Right: TMultiValue): integer;
    function CompareBoolValues(const Left, Right: TMultiValue): integer;
    function CompareTags(const Left, Right: TMultiValue): integer;

  public
    function AddNoNameValue(const No: integer; const AName, StrValue: string): integer;
    function AddNameValue(const AName, StrValue: string; AutoNum: Boolean = True): integer;
    function NameIndex(const AName: string; IgnoreCase: Boolean = False; SearchFromEnd: Boolean = False): integer;
    function NameExists(const AName: string; IgnoreCase: Boolean = False): Boolean;
    function GetStrValue(const AName: string; IgnoreCase: Boolean = False; ResultIfNotExists: string = ''): string;
    procedure Renumber(const FirstItemNo: integer = 1);
    function RemoveItemWithName(const AName: string; IgnoreCase: Boolean = False; RemoveAll: Boolean = False): Boolean;
    function RemoveItemsWithName(const AName: string; IgnoreCase: Boolean = False): Boolean;

    procedure SortByNo(const Ascending: Boolean = True);
    procedure SortByName(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
    procedure SortByDescription(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
    procedure SortByStrValue(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
    procedure SortByIntValue(const Ascending: Boolean = True);
    procedure SortByFloatValue(const Ascending: Boolean = True);
    procedure SortByTag(const Ascending: Boolean = True);
    procedure SortByBoolValue(const Ascending: Boolean = True);
  end;




implementation





{$region '                             TMultiValueList                              '}

function TMultiValueList.AddNoNameValue(const No: integer; const AName, StrValue: string): integer;
var
  mv: TMultiValue;
begin
  mv.Clear;
  mv.No := No;
  mv.Name := AName;
  mv.StrValue := StrValue;
  Result := Self.Add(mv);
end;

function TMultiValueList.AddNameValue(const AName, StrValue: string; AutoNum: Boolean = True): integer;
begin
  if AutoNum then Result := AddNoNameValue(Count + 1, AName, StrValue)
  else Result := AddNoNameValue(0, AName, StrValue);
end;

function TMultiValueList.NameExists(const AName: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := NameIndex(AName, IgnoreCase) >= 0;
end;

function TMultiValueList.NameIndex(const AName: string; IgnoreCase: Boolean = False; SearchFromEnd: Boolean = False): integer;
var
  i: integer;
  b: Boolean;
begin
  Result := -1;

  if SearchFromEnd then

    for i := Count - 1 downto 0 do
    begin
      if IgnoreCase then b := SameText(Items[i].Name, AName)
      else b := SameStr(Items[i].Name, AName);
      if b then
      begin
        Result := i;
        Break;
      end;
    end

  else

    for i := 0 to Count - 1 do
    begin
      if IgnoreCase then b := SameText(Items[i].Name, AName)
      else b := SameStr(Items[i].Name, AName);
      if b then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TMultiValueList.RemoveItemWithName(const AName: string; IgnoreCase: Boolean = False; RemoveAll: Boolean = False): Boolean;
var
  xInd: integer;
  b: Boolean;
begin
  Result := False;

  if not RemoveAll then
  begin
    xInd := NameIndex(AName, IgnoreCase);
    if xInd >= 0 then
    begin
      Self.Delete(xInd);
      Result := True;
    end;
  end

  else

    for xInd := Count - 1 downto 0 do
    begin
      if IgnoreCase then b := SameText(Items[xInd].Name, AName)
      else b := SameStr(Items[xInd].Name, AName);
      if b then
      begin
        Self.Delete(xInd);
        Result := True;
      end;
    end;
end;

function TMultiValueList.RemoveItemsWithName(const AName: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := RemoveItemWithName(AName, IgnoreCase, True);
end;

procedure TMultiValueList.Renumber(const FirstItemNo: integer = 1);
var
  i: integer;
  mv: TMultiValue;
begin
  for i := 0 to Count - 1 do
  begin
    mv := Items[i];
    mv.No := FirstItemNo + i;
  end;
end;

function TMultiValueList.GetStrValue(const AName: string; IgnoreCase: Boolean; ResultIfNotExists: string): string;
var
  x: integer;
begin
  Result := ResultIfNotExists;
  x := NameIndex(AName, IgnoreCase);
  if x < 0 then Exit;
  Result := Items[x].StrValue;
end;


  {$region '                 Sorting                   '}

function TMultiValueList.CompareNums(const Left, Right: TMultiValue): integer;
begin
  Result := Left.No - Right.No;
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareNames(const Left, Right: TMultiValue): integer;
begin
  if FSortCaseSensitive then Result := CompareStr(Left.Name, Right.Name)
  else Result := CompareText(Left.Name, Right.Name);
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareDescriptions(const Left, Right: TMultiValue): integer;
begin
  if FSortCaseSensitive then Result := CompareStr(Left.Description, Right.Description)
  else Result := CompareText(Left.Description, Right.Description);
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareStrValues(const Left, Right: TMultiValue): integer;
begin
  if FSortCaseSensitive then Result := CompareStr(Left.StrValue, Right.StrValue)
  else Result := CompareText(Left.StrValue, Right.StrValue);
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareIntValues(const Left, Right: TMultiValue): integer;
begin
  Result := Left.IntValue - Right.IntValue;
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareFloatValues(const Left, Right: TMultiValue): integer;
var
  x: Extended;
begin
  x := Left.FloatValue - Right.FloatValue;
  if x < 0 then Result := -1
  else if x > 0 then Result := 1
  else Result := 0;
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareBoolValues(const Left, Right: TMultiValue): integer;
begin
  if Left.BoolValue = Right.BoolValue then Result := 0
  else if Left.BoolValue = False then Result := -1
  else Result := 1;
  if not FSortAscending then Result := -Result;
end;

function TMultiValueList.CompareTags(const Left, Right: TMultiValue): integer;
begin
  Result := Left.Tag - Right.Tag;
  if not FSortAscending then Result := -Result;
end;



procedure TMultiValueList.SortByNo(const Ascending: Boolean = True);
begin
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareNums));
end;

procedure TMultiValueList.SortByName(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
begin
  FSortCaseSensitive := CaseSensitive;
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareNames));
end;

procedure TMultiValueList.SortByDescription(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
begin
  FSortCaseSensitive := CaseSensitive;
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareDescriptions));
end;

procedure TMultiValueList.SortByStrValue(const Ascending: Boolean = True; const CaseSensitive: Boolean = False);
begin
  FSortCaseSensitive := CaseSensitive;
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareStrValues));
end;

procedure TMultiValueList.SortByIntValue(const Ascending: Boolean = True);
begin
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareIntValues));
end;

procedure TMultiValueList.SortByFloatValue(const Ascending: Boolean = True);
begin
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareFloatValues));
end;

procedure TMultiValueList.SortByBoolValue(const Ascending: Boolean = True);
begin
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareBoolValues));
end;

procedure TMultiValueList.SortByTag(const Ascending: Boolean = True);
begin
  FSortAscending := Ascending;
  Sort(TComparer<TMultiValue>.Construct(CompareTags));
end;

  {$endregion Sorting}

{$endregion TMultiValueList}




{$region '        TMultiValue           '}
procedure TMultiValue.Clear;
begin
  No := 0;
  Name := '';
  Description := '';
  StrValue := '';
  IntValue := 0;
  FloatValue := 0;
  BoolValue := False;
  P := nil;
  Tag := 0;
end;
{$endregion TMultiValue}



end.
