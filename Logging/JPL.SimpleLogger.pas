unit JPL.SimpleLogger;

{:
  TJPSimpleLogger.
  This is a very simple logging class designed primarily for a small console applications.
  To reduce dependencies it's based on array.
  Logger works fine with relatively small number of log items (TLogItem) - a few hundreds is not a problem.

}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}


{ TODO : LogEvent }


interface


uses
  SysUtils, JPL.Conversion;

const


  LOG_CATEGORY_ID_INFO = 0;
  LOG_CATEGORY_ID_HINTS = 1;
  LOG_CATEGORY_ID_WARNINGS = 2;
  LOG_CATEGORY_ID_ERRORS = 10;

  LOG_PRIORITY_BASE = 0;
  LOG_PRIORITY_NORMAL: ShortInt = LOG_PRIORITY_BASE;

  LOG_PRIORITY_CRITICAL: ShortInt = LOG_PRIORITY_BASE + 10;
  LOG_PRIORITY_HIGHEST: ShortInt = LOG_PRIORITY_BASE + 3;
  LOG_PRIORITY_HIGH: ShortInt = LOG_PRIORITY_BASE + 2;
  LOG_PRIORITY_HIGHER: ShortInt = LOG_PRIORITY_BASE + 1;

  LOG_PRIORITY_LOWER: ShortInt = LOG_PRIORITY_BASE - 1;
  LOG_PRIORITY_LOW: ShortInt = LOG_PRIORITY_BASE - 2;
  LOG_PRIORITY_LOWEST: ShortInt = LOG_PRIORITY_BASE - 3;

type

  TLogTag = record
    IntValue: integer;
    StrValue: string;
    FloatValue: Double;
    BoolValue: Boolean;
  end;


  TLogItem = record
    No: integer; //:< Item number and unique identifier (not index): 1, 2, 3 ....
    Priority: ShortInt; //:< Item priority (ShortInt - signed byte)
    CategoryID: Byte; //:< Category identifier: LOG_CATEGORY_ID_ERRORS, LOG_CATEGORY_ID_HINTS... or user defined
    DateTime: TDateTime; //:< Logging time (filled automatically with value returned by Now)
    Text: string; //:< Text to log
    Context: string; //:< eg. procedure name
    Active: Boolean; //:< Items can be activated/deactivated
    Deleted: Boolean; //:< Items marked as deleted are ignored while processing the list.
    Tag: TLogTag;
    // some helpers
    function ToString: string;
    procedure Enable;
    procedure Disable;
    procedure Delete;
    procedure Undelete;
    function IsError: Boolean;
    function IsInfo: Boolean;
    function IsHint: Boolean;
    function IsWarning: Boolean;
  end;

  TLogList = array of TLogItem;

  TJPSimpleLogger = class
  private
    FList: TLogList;
    ENDL: string;
    procedure ClearLogItem(var li: TLogItem);
    function GetActiveItemsCount: integer;
    function GetInactiveItemsCount: integer;
    function GetCount: integer;

    function GetItems(Index: integer): TLogItem;
    procedure SetItems(Index: integer; const Value: TLogItem);
    procedure CopyItem(const Src: TLogItem; var Dest: TLogItem; bKeepItemNo: Boolean = False);
    function GetErrorCount: integer;
    function GetWarningCount: integer;
    function GetHintCount: integer;
    function IsValidIndex(const Index: integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearLog; //:< Clears log array (sets array size to 0).

    //: Delete does not really delete item in array. It only sets Deleted field to True.
    function Delete(const Index: integer): Boolean;
    function Undelete(const Index: integer): Boolean; //:< Sets Deleted field of the given item to False.
    function EraseItem(const Index: integer): Boolean; //:< Sets Deleted field to True and fills other fields with default values.

    //: Returns index of the new added item.
    function Log(const Text: string; Context: string = ''; CategoryID: Byte = LOG_CATEGORY_ID_INFO; Priority: ShortInt = LOG_PRIORITY_BASE; Active: Boolean = True): integer;
    function LogError(const Text: string; Context: string = ''; Priority: ShortInt = LOG_PRIORITY_BASE; Active: Boolean = True): integer;
    function LogWarning(const Text: string; Context: string = ''; Priority: ShortInt = LOG_PRIORITY_BASE; Active: Boolean = True): integer;
    function LogHint(const Text: string; Context: string = ''; Priority: ShortInt = LOG_PRIORITY_BASE; Active: Boolean = True): integer;

    function Pack(bRenumberItems: Boolean = True): integer; //:< Removes all items marked as Deleted. Returns the number of removed items.
    procedure RenumberItems;
    function CategoryCount(CategoryID: Byte; OnlyActive: Boolean = False): integer; //:< Returns the number of items in specified category.

    function AsDebugString: string;

    procedure SaveToFile(const FileName: string; Append: Boolean = False;
      bInfo: Boolean = True; bErrors: Boolean = True; bWarnings: Boolean = True; bHints: Boolean = True);


    property List: TLogList read FList; //:< Log list - array of TLogItem.

    property Count: integer read GetCount; //:< Returns the number of all not deleted items (active and inactive)
    property ActiveItemsCount: integer read GetActiveItemsCount; //:< Returns the number of all not deleted active items from all categories.
    property InactiveItemsCount: integer read GetInactiveItemsCount; //:< Returns number of all not deleted and inactive items from all categories.
    property ErrorCount: integer read GetErrorCount; //:< Returns the number of all not deleted errors (active and inactive)
    property WarningCount: integer read GetWarningCount; //:< Returns the number of all not deleted warnings (active and inactive)
    property HintCount: integer read GetHintCount; //:< Returns the number of all not deleted hints (active and inactive)

    //: Log items are accessible by Items default property
    property Items[Index: integer]: TLogItem read GetItems write SetItems; default;

  end;


function LogPriorityToStr(const Priority: ShortInt): string;
function LogCategoryToStr(const CategoryID: Byte): string;



implementation


function LogCategoryToStr(const CategoryID: Byte): string;
begin
  case CategoryID of
    LOG_CATEGORY_ID_INFO: Result := 'Info';
    LOG_CATEGORY_ID_HINTS: Result := 'Hints';
    LOG_CATEGORY_ID_WARNINGS: Result := 'Warnings';
    LOG_CATEGORY_ID_ERRORS: Result := 'Errors';
  else
    Result := itos(CategoryID);
  end;
end;


function LogPriorityToStr(const Priority: ShortInt): string;
begin
//  case Priority of
//    LOG_PRIORITY_NORMAL: Result := 'Normal';
//  end;
  if Priority = LOG_PRIORITY_NORMAL then Result := 'Normal'
  else if Priority = LOG_PRIORITY_CRITICAL then Result := 'Critical'
  else if Priority = LOG_PRIORITY_HIGHEST then Result := 'Highest'
  else if Priority = LOG_PRIORITY_HIGH then Result := 'High'
  else if Priority = LOG_PRIORITY_HIGHER then Result := 'Higher'
  else if Priority = LOG_PRIORITY_LOWER then Result := 'Lower'
  else if Priority = LOG_PRIORITY_LOW then Result := 'Low'
  else if Priority = LOG_PRIORITY_LOWEST then Result := 'Lowest'
  else Result := IntToStr(Priority);
end;




{$region '              Create, Destroy, Clear                  '}
constructor TJPSimpleLogger.Create;
begin
  inherited Create;
  ENDL := sLineBreak;
end;

destructor TJPSimpleLogger.Destroy;
begin
  ClearLog;
  inherited;
end;

procedure TJPSimpleLogger.ClearLog;
var
  i: integer;
begin
  if Length(FList) > 0 then
  begin
    for i := 0 to High(FList) do ClearLogItem(FList[i]);
    SetLength(FList, 0);
  end;
end;
{$endregion Create, Destroy, Clear}

function TJPSimpleLogger.Delete(const Index: integer): Boolean;
begin
  if not IsValidIndex(Index) then Exit(False);
  FList[Index].Delete;
  Result := True;
end;

function TJPSimpleLogger.Undelete(const Index: integer): Boolean;
begin
  if not IsValidIndex(Index) then Exit(False);
  FList[Index].Undelete;
  Result := True;
end;

function TJPSimpleLogger.EraseItem(const Index: integer): Boolean;
var
  x: integer;
begin
  if not IsValidIndex(Index) then Exit(False);
  x := FList[Index].No;
  ClearLogItem(FList[Index]);
  FList[Index].Deleted := True;
  FList[Index].No := x;
  Result := True;
end;

function TJPSimpleLogger.GetActiveItemsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(FList) do
    if (FList[i].Active) and (not FList[i].Deleted) then Inc(Result);
end;

function TJPSimpleLogger.GetInactiveItemsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(FList) do
    if (not FList[i].Active) and (not FList[i].Deleted) then Inc(Result);
end;

function TJPSimpleLogger.CategoryCount(CategoryID: Byte; OnlyActive: Boolean = False): integer;
var
  i: integer;
begin
  if Length(FList) = 0 then Exit(0);
  Result := 0;

  for i := 0 to High(FList) do
  begin
    if FList[i].Deleted then Continue;
    if FList[i].CategoryID = CategoryID then
      if not OnlyActive then Inc(Result)
      else
        if FList[i].Active then Inc(Result);
  end;
end;

function TJPSimpleLogger.GetCount: integer;
var
  i: integer;
begin
  Result := 0;
  if Length(FList) = 0 then Exit;
  for i := 0 to High(FList) do
    if not FList[i].Deleted then Inc(Result);
end;

function TJPSimpleLogger.GetErrorCount: integer;
begin
  Result := CategoryCount(LOG_CATEGORY_ID_ERRORS, False);
end;

function TJPSimpleLogger.GetHintCount: integer;
begin
  Result := CategoryCount(LOG_CATEGORY_ID_HINTS, False);
end;

function TJPSimpleLogger.GetWarningCount: integer;
begin
  Result := CategoryCount(LOG_CATEGORY_ID_WARNINGS, False);
end;

function TJPSimpleLogger.GetItems(Index: integer): TLogItem;
begin
  Result := FList[Index];
end;

function TJPSimpleLogger.IsValidIndex(const Index: integer): Boolean;
begin
  if Length(FList) = 0 then Result := False
  else Result := Index <= Length(FList) - 1;
end;

function TJPSimpleLogger.Log(const Text: string; Context: string = ''; CategoryID: Byte = LOG_CATEGORY_ID_INFO; Priority: ShortInt = LOG_PRIORITY_BASE; Active: Boolean = True): integer;
var
  x: integer;
begin
  SetLength(FList, Length(FList) + 1);
  x := High(FList);
  FList[x].No := x + 1;
  FList[x].Priority := Priority;
  FList[x].CategoryID := CategoryID;
  FList[x].DateTime := Now;
  FList[x].Text := Text;
  FList[x].Context := Context;
  FList[x].Active := Active;
  Result := x;
end;

function TJPSimpleLogger.LogError(const Text: string; Context: string; Priority: ShortInt; Active: Boolean): integer;
begin
  Result := Log(Text, Context, LOG_CATEGORY_ID_ERRORS, Priority, Active);
end;

function TJPSimpleLogger.LogHint(const Text: string; Context: string; Priority: ShortInt; Active: Boolean): integer;
begin
  Result := Log(Text, Context, LOG_CATEGORY_ID_HINTS, Priority, Active);
end;

function TJPSimpleLogger.LogWarning(const Text: string; Context: string; Priority: ShortInt; Active: Boolean): integer;
begin
  Result := Log(Text, Context, LOG_CATEGORY_ID_WARNINGS, Priority, Active);
end;

function TJPSimpleLogger.Pack(bRenumberItems: Boolean): integer;
var
  NewList: TLogList;
  i, x: integer;
begin
  Result := 0;
  if Length(FList) = 0 then Exit;
  SetLength(NewList, 0);
  for i := 0 to High(FList) do
  begin
    if FList[i].Deleted then
    begin
      Inc(Result);
      Continue;
    end;
    SetLength(NewList, Length(NewList) + 1);
    x := High(NewList);
    CopyItem(FList[i], NewList[x], True);
    ClearLogItem(FList[i]);
  end;
  SetLength(FList, 0);
  FList := NewList;
  if bRenumberItems then RenumberItems;
end;

procedure TJPSimpleLogger.RenumberItems;
var
  i: integer;
begin
  for i := 0 to High(FList) do FList[i].No := i + 1;
end;

{$hints off}
procedure TJPSimpleLogger.SaveToFile(const FileName: string; Append, bInfo, bErrors, bWarnings, bHints: Boolean);
var
  i, No: integer;
  LogStr: string;
  Bytes, Preamble: TBytes;
  hf: THandle;
  Enc: TEncoding;
  Item: TLogItem;
begin
  if not Append then if FileExists(FileName) then DeleteFile(FileName);
  if not FileExists(FileName) then hf := SysUtils.FileCreate(FileName)
  else hf := SysUtils.FileOpen(FileName, fmOpenReadWrite);
  try

    if Append then FileSeek(hf, 0, 2);
    Enc := TEncoding.UTF8;
    Preamble := Enc.GetPreamble;
    SysUtils.FileWrite(hf, Preamble[0], Length(Preamble));
    No := 0;

    LogStr := ENDL + '-------------------- LOG GROUP --------------------' + ENDL;
    {$IFDEF MSWINDOWS}
    Bytes := Enc.GetBytes(UnicodeString(LogStr));
    {$ELSE}
    Bytes := Enc.GetBytes(UnicodeString(LogStr));
    {$ENDIF}
    SysUtils.FileWrite(hf, Bytes[0], Length(Bytes));

    for i := 0 to High(FList) do
    begin

      Item := FList[i];
      if Item.Deleted then Continue;
      if (Item.CategoryID = LOG_CATEGORY_ID_INFO) and (not bInfo) then Continue;
      if (Item.CategoryID = LOG_CATEGORY_ID_ERRORS) and (not bErrors) then Continue;
      if (Item.CategoryID = LOG_CATEGORY_ID_WARNINGS) and (not bWarnings) then Continue;
      if (Item.CategoryID = LOG_CATEGORY_ID_HINTS) and (not bHints) then Continue;

      LogStr := '';
      Inc(No);
      LogStr := LogStr + 'Log item no: ' + itos(No) + ENDL;
      LogStr := LogStr + 'Date & Time: ' + DateTimeToStr(Item.DateTime) + ENDL;
      LogStr := LogStr + 'Category: ' + LogCategoryToStr(Item.CategoryID) + ENDL;
      if Item.Context <> '' then LogStr := LogStr + 'Context: ' + Item.Context + ENDL;
      LogStr := LogStr + FList[i].Text + ENDL + ENDL;

      {$IFDEF MSWINDOWS}
      Bytes := Enc.GetBytes(UnicodeString(LogStr));
      {$ELSE}
      Bytes := Enc.GetBytes(UnicodeString(LogStr));
      {$ENDIF}
      SysUtils.FileWrite(hf, Bytes[0], Length(Bytes));

    end;

  finally
    SysUtils.FileClose(hf);
  end;

end;
{$hints on}

procedure TJPSimpleLogger.SetItems(Index: integer; const Value: TLogItem);
begin
  CopyItem(Value, FList[Index]);
end;


function TJPSimpleLogger.AsDebugString: string;
var
  i: integer;
  s: string;
begin
  if Length(FList) = 0 then Exit('Log is empty.');
  s := 'Log List:' + ENDL + ENDL;
  for i := 0 to High(FList) do
  begin
    s := s + '  Item index: ' + itos(i) + ENDL;
    s := s + FList[i].toString + ENDL + ENDL;
  end;
  s := s + '-------------------------------------------' + ENDL;
  s := s + 'All items: ' + itos(Count) + ENDL;
  s := s + 'Active items: ' + itos(ActiveItemsCount) + ENDL;
  s := s + 'Inactive items: ' + itos(InactiveItemsCount) + ENDL;
  s := s + 'Error items: ' + itos(ErrorCount) + ENDL;
  s := s + 'Warnings: ' + itos(WarningCount) + ENDL;
  s := s + 'Hints: ' + itos(HintCount) + ENDL;
  Result := s;
end;

procedure TJPSimpleLogger.ClearLogItem(var li: TLogItem);
begin
  li.No := -1;
  li.Priority := LOG_PRIORITY_NORMAL;
  li.DateTime := 0;
  li.CategoryID := LOG_CATEGORY_ID_INFO;
  li.Text := '';
  li.Context := '';
  li.Deleted := False;
  li.Active := False;
  li.Tag.IntValue := 0;
  li.Tag.StrValue := '';
  li.Tag.FloatValue := 0;
  li.Tag.BoolValue := False;
end;

procedure TJPSimpleLogger.CopyItem(const Src: TLogItem; var Dest: TLogItem; bKeepItemNo: Boolean = False);
begin
  if not bKeepItemNo then Dest.No := Src.No;
  Dest.Priority := Src.Priority;
  Dest.DateTime := Src.DateTime;
  Dest.CategoryID := Src.CategoryID;
  Dest.Text := Src.Text;
  Dest.Context := Src.Context;
  Dest.Deleted := Src.Deleted;
  Dest.Active := Src.Active;
  Dest.Tag.IntValue := Src.Tag.IntValue;
  Dest.Tag.StrValue := Src.Tag.StrValue;
  Dest.Tag.FloatValue := Src.Tag.FloatValue;
  Dest.Tag.BoolValue := Src.Tag.BoolValue;
end;



{$region '                           TLogItem record                               '}
procedure TLogItem.Delete;
begin
  Self.Deleted := True;
end;

procedure TLogItem.Undelete;
begin
  Self.Deleted := False;
end;

function TLogItem.IsError: Boolean;
begin
  Result := Self.CategoryID = LOG_CATEGORY_ID_ERRORS;
end;

function TLogItem.IsInfo: Boolean;
begin
  Result := Self.CategoryID = LOG_CATEGORY_ID_INFO;
end;

function TLogItem.IsHint: Boolean;
begin
  Result := Self.CategoryID = LOG_CATEGORY_ID_HINTS;
end;

function TLogItem.IsWarning: Boolean;
begin
  Result := Self.CategoryID = LOG_CATEGORY_ID_WARNINGS;
end;

procedure TLogItem.Disable;
begin
  Self.Active := False;
end;

procedure TLogItem.Enable;
begin
  Self.Active := True;
end;

function TLogItem.ToString: string;
var
  Indent, Indent2, s, ENDL: string;
begin
  ENDL := sLineBreak;
  Indent := '  ';
  Indent2 := '    ';
  s := '';
  s := s + Indent + 'No: ' + itos(Self.No) + ENDL;
  s := s + Indent + 'Priority: ' + LogPriorityToStr(Self.Priority) + ENDL;
  s := s + Indent + 'CategoryID: ' + itos(Self.CategoryID) + ' (' + LogCategoryToStr(Self.CategoryID) + ')' + ENDL;
  s := s + Indent + 'Time: ' + DateTimeToStr(Self.DateTime) + ENDL;
  s := s + Indent + 'Active: ' + BoolToStrYN(Self.Active) + ENDL;
  s := s + Indent + 'Deleted: ' + BoolToStrYN(Self.Deleted) + ENDL;
  s := s + Indent + 'Context: ' + Self.Context + ENDL;
  s := s + Indent + 'Text: ' + Self.Text + ENDL;
  s := s + Indent + 'Tag' + ENDL;
  s := s + Indent2 + 'Tag.IntValue: ' + itos(Self.Tag.IntValue) + ENDL;
  s := s + Indent2 + 'Tag.StrValue: ' + Self.Tag.StrValue + ENDL;
  s := s + Indent2 + 'Tag.FloatValue: ' + ftos(Self.Tag.FloatValue, 6) + ENDL;
  s := s + Indent2 + 'Tag.BoolValue: ' + BoolToStrYN(Self.Tag.BoolValue);
  Result := s;
end;



{$endregion TLogItem record}

end.
