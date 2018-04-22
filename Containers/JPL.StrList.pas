unit JPL.StrList;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 2018.02.11


  TJPStrList - a class for managing the list of strings based on doubly linked list.
  Designed mainly for small console applications to store file names.
  No Classes, only SysUtils!

  Search for strings much faster than TStringList, but slower than THashedStringList.
  Works well with lists about the number of items up to several hundred thousand.
  Relatively good with lists with a number of items up to several million.

  Sorting based on Quick Sort. Speed similar to THashedStringList (also based on Quick Sort).

  ==============================================

  FPC 3.1.1 Rev. 37820 (trunk) + Lazarus 1.9.0 Rev. 56850 (trunk)
  PC: i7-2600K 3.40 GHz, 8 GB RAM, Windows 10 64-bit

  The number of all files on my disks: 9 621 867
  Characters: 907 327 355  [865 MB ASCII] [1 731 MB UTF-16]
  The average length of the file name: 94.30 characters
  Times measured with QueryPerformanceFrequency + QueryPerformanceCounter.

      Win 64-bit executable


                                    Memory usage
        Items      String length   (Task Manager)
      ---------------------------------------------
          1 000     94 (const.)        1.6 MB
          5 000                        2.5 MB
         10 000                        3.6 MB
         50 000                         12 MB
        100 000                         23 MB
        250 000                         55 MB
        500 000                        109 MB
      1 000 000                        215 MB
      2 000 000                        429 MB
      3 000 000                        643 MB
      4 000 000                        857 MB
      5 000 000                      1 070 MB
      9 500 000                      2 033 MB




      Operation              Items       String length       Time
      --------------------------------------------------------------------------------
      Reverse              1 000 000      94 (const.)      00:00.007
                           2 000 000                       00:00.017
                           3 000 000                       00:00.024
                           4 000 000                       00:00.033
                           5 000 000                       00:00.041
                           9 500 000                       00:00.077
      --------------------------------------------------------------------------------
      Sorting                 25 000      94 (const.)      00:00.211
      case sensitive          50 000                       00:00.468
                             100 000                       00:00.955
                             200 000                       00:02.073
                             300 000                       00:03.184
                             400 000                       00:04.542
                             500 000                       00:05.636
                           1 000 000                       00:12.481
                           2 000 000                       00:25.662
      --------------------------------------------------------------------------------
      Sorting                 25 000      94 (const.)      00:00.330
      not case sensitive      50 000                       00:00.772
                             100 000                       00:01.515
                             200 000                       00:03.222
                             300 000                       00:04.973
                             400 000                       00:06.831
                             500 000                       00:08.547
                           1 000 000                       00:18.026
                           2 000 000                       00:38.080
       --------------------------------------------------------------------------------





  ==============================================
  Adding items.

    1. Add - adds one string at the end of the list.
    2. Insert - inserts string at the given position (index).

  ==============================================
  Accessing items.

    1. Using Items or Strings property:

      var s: string;

      s := JPStrList.Items[10];
        OR
      s := JPStrList.Strings[10];
        OR
      s := JPStrList[10];

    2. GetItemByStr(const s: string... - returns pointer to TSLItem containing the given string (or nil).
    3. GetItemByIndex(const Index: integer): PSLItem - returns pointer to TSLItem with specified index (or nil).
    4. SaveToArray(var Arr: TStringArray) - copies all stored strings to a dynamic array of strings.

  ==============================================
  Iteration of the list.

    1. FOR loop (only strings):

         for i := 0 to JPStrList.Count - 1 do Writeln(JPStrList[i]);

    2. WHILE loop with pointers:

         var PItem: PSLItem;

         PItem := JPStrList.First;
         while PItem <> nil do
         begin
           Writeln(PItem^.Str);
           PItem := PItem^.Next;
         end;

  ==============================================
  Deleting items.

    1. function Delete(const s: string; OnlyFirst: Boolean = True; CaseSensitive: Boolean = True): integer; overload;
    When removing a large number of elements, use BeginUpdate + EndUpdate.
    When BeginUpdate is on, the indexes are not updated each time one element is deleted, only the variable
    FNeedRebuildIndex is set to True. After calling EndUpdate, the indexes of all elements are updated.

      JPStrList.BeginUpdate; // Indexes will not be updated when removing items.
      try
        JPStrList.Delete('some string', False);
        JPStrList.Delete('delete only the first occurence', True);
        JPStrList.Delete('another string');
      finally
        JPStrList.EndUpdate; // RebuildIndexes, RebuildHelperArray
      end;

    2. function Delete(const Index: integer): Boolean; overload;
    It's best to remove items from the end, just like in TStringList.

      for i := JPStrList.Count - 1 downto (JPStrList.Count div 2) do
        JPStrList.Delete(i);

    3. ClearAll - removes all items.

    4. Extract / ExtractStr - Extracts one item / item string and removes extracted item from the list.

  ==============================================
  Searching for strings.

    1. IndexOfStr - Returns the index of the given string in the list - case sensitive.

         if JPStrList.IndexOfStr('some string') >= 0 then Writeln('EXISTS')
         else Writeln('NOT EXISTS');

    2. IndexOfText - Returns the index of the given string in the list - NOT case sensitive.

    LastIndexOfStr, LastIndexOfText


  ==============================================
  Helper items.

  These items are used to speed up the non-sequential access to list items by index.
  By default, every 250th (prop. HelperItemsDelta) element added to the list is also added to the ArrHelperItems array.
  When searching the list for items by index (GetItemByIndex), the element with the index maximally close to the searched one is retrieved
  from this array (func. GetHelperItem). GetItemByIndex starts the search from the returned element, not from the beginning (or end) of the list.
  This solution significantly speeds up searching for elements by index, especially in lists with a large number of elements.

  ==============================================
  Sorting.

  Sorting based on the Quick Sort algorithm.
  The list can be sorted in ascending or descending order, taking into account the size of the characters or not.

  ==============================================


  Usuwanie duplikatów.
  Jeśli lista jest posortowana z CaseSensitive = True, wówczas RemoveDuplicates(True) jest bardzo szybkie.
  Podobnie, gdy posortowano listę z CaseSensitive = False, RemoveDuplicates(False) jest bardzo szybkie.
  W każdym innym przypadku RemoveDuplicates jest bardzo wolne.
}

{ TODO: BOM - LoadFromFile, SaveToFile }
{ TODO: dodać pełną obsługę Name / Value. Na razie dodałem tylko proc. GetNameValue. }


{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  //LazUTF8,
  {$IFDEF DCC} Windows, {$ENDIF}
  SysUtils,
  JPL.Strings,
  JPL.StrHash;


const
  SL_HELPER_ITEMS_DELTA_MIN = 4;
  SL_HELPER_ITEMS_DELTA_DEFAULT = 250;

type

{$IFDEF DCC}

  TStringArray = Array of string;

  {$ifdef CPUX64}
  PtrUInt = UInt64; //QWord;
  {$endif}

  {$ifdef CPUX86}
  PtrUInt = DWord;
  {$endif}

{$ENDIF DCC}

  TSLCompareStrProc = function (const s1, s2: string): integer;

  // Two pair of hashes to minimize the risk of collision.
  TSLHashes = packed record
    Hash: Cardinal;    // first hash
    UHash: Cardinal;   // first hash (upper case)
    Hash2: Cardinal;   // secondary hash
    UHash2: Cardinal;  // secondary hash (upper case)
  end;

  // List item
  PSLItem = ^TSLItem;
  TSLItem = record
    Hashes: TSLHashes;   // Hashes of the "Str" string
    Str: string;         // String
    Index: integer;      // Index on the list
    Next: PSLItem;       // Pointer to the next item in the list
    Prev: PSLItem;       // Pointer to the previous item in the list
  end;

  TSLSortDirection = (sdAscending, sdDescending);

  EStrListException = Exception;


  TJPStrList = class
  private
    FEndOfLineMarker: string;
    FFirst: PSLItem;
    FLast: PSLItem;
    FCount: integer;
    FNeedRebuildIndex: Boolean;
    FUpdating: Boolean; // used by BeginUpdate and EndUpdate
    FForLastItem: PSLItem; // The last iterated element. Used by GetItems and SetItems to speed up sequential access to list items.
    FSorted: Boolean;
    FSortCaseSensitive: Boolean;
    ArrHelperItems: array of PSLItem; // Array with helper items. Used to accelerate non-sequential access to list items by index.
    FHelperItemsDelta: LongWord;
    FUseHelperItems: Boolean;

    procedure ClearItem(var PItem: PSLItem);

    // Returns the "Str" field of the item with specified index. Raises EStrListException exception if Index is invalid.
    function GetItems(Index: integer): string;
    function GetStrings(Index: integer): string;
    function GetText: string;
    procedure SetEndOfLineMarker(AValue: string);
    // Sets "Str" field ot the item with specified index. Raises EStrListException exception if Index is invalid.
    procedure SetItems(Index: integer; AValue: string);

    procedure GetStrHashes(const s: string; var Hashes: TSLHashes);
    function GetItemFullSize(const PItem: PSLItem): integer;
    procedure SetHelperItemsDelta(AValue: LongWord);

    function IsValidIndex(const Index: integer): Boolean;
    procedure CopyItem(const Src: PSLItem; var Dest: PSLItem);

    // Removes PItem element from the list and returns the next element (or nil)
    function PerformDeleteItem(PItem: PSLItem): PSLItem;

    function RemoveDuplicatesFromSortedList(StartItem: PSLItem; CaseSensitive: Boolean = True): integer;

    procedure AddHelperItem(PItem: PSLItem);
    function GetHelperItem(const Index: integer): PSLitem;
    procedure SetStrings(Index: integer; AValue: string);
    procedure SetText(AValue: string);



  public

    procedure DisplayHelperItems; {TODO: move DisplayHelperItems to "private" section}

    constructor Create;
    destructor Destroy; override;

    // Removes all items from the list and resets all parameters to the default values.
    procedure ClearAll;
    procedure Clear; // Clears items.


    // Adds one string to the list. Returns the pointer to the newly added item.
    function Add(const s: string): PSLItem; overload;

    // Adds COPY of the PItem at the end of the list. Returns the pointer to the newly added item.
    // The caller is responsible for freeing the PItem passed as parameter!
    function Add(PItem: PSLItem; Rehash: Boolean = False): PSLItem; overload;

    procedure AddStrings(const Arr: TStringArray); overload;  // Adds strings from the Arr array to the list.
    procedure AddStrings(SList: TJPStrList); overload;
    procedure Assign(SList: TJPStrList);


    // Inserts one element at the specified position (index). Returns the pointer to the newly added item.
    function Insert(const Index: integer; const s: string): PSLItem;

    // Inserts a new item before PItem. Returns the pointer to the newly added item.
    function InsertBefore(PItem: PSLItem; const s: string): PSLItem;

    // Inserts a new item after PItem. Returns the pointer to the newly added item.
    function InsertAfter(PItem: PSLItem; const s: string): PSLItem;


    // Removes one or more items from the list starting the search from the given index. Returns the number of deleted items.
    function Delete(const s: string; OnlyFirst: Boolean = True; CaseSensitive: Boolean = True; StartIndex: integer = 0): integer; overload;

    // Removes one or more items from the list starting the search from the given item. Returns the number of deleted items.
    function Delete(const s: string; StartItem: PSLItem; OnlyFirst: Boolean = True; CaseSensitive: Boolean = True): integer; overload;

    function Delete(const Index: integer): Boolean; overload; // Removes one item from the list.
    function Delete(PItem: PSLItem): PSLItem; overload; // Removes one item from the list and returns the next item.


    // Returns one element at the specified position (index) and removes this element from the list.
    // The caller is responsible for freeing the returned item by Dispose(ReturnedItem)!
    function Extract(const Index: integer): PSLItem;

    // Returns the "Str" field of the item at the specified position (index) and removes this element from the list.
    function ExtractStr(const Index: integer): string;


    procedure ExchangeItems(Item1, Item2: PSLItem); // Replaces Item1 with Item2


    procedure Display(ShowNumbers: Boolean = False); overload; // Lists all strings stored in the list. Only if IsConsole = True
    procedure Display(const StartIndex, EndIndex: integer; ShowNumbers: Boolean = False); overload;
    procedure Debug_Display(bShowItems: Boolean = True); // Similar but with more info.

    function SizeOfAllItems: Int64;
    function CharacterCount: Int64; // Returns number of all characters from all strings stored in the list.
    procedure RebuildIndexes; // Updates all indexes
    procedure UpdateIndexes(const StartItem: PSLItem); // Updates indexes from the StartItem element to the end of the list

    function GetItemByStr(const s: string; CaseSensitive: Boolean = True; ForwardSearch: Boolean = True): PSLItem;
    function GetItemByIndex(const Index: integer): PSLItem;

    procedure GetNameValue(const Index: integer; out AName, AValue: string);

    // Returns the index of the first element containing the given string.
    function IndexOf(const s: string; const CaseSensitive: Boolean): integer;
    function IndexOfStr(const s: string): integer;  // Shortcut for IndexOf(s, True)
    function IndexOfText(const s: string): integer; // Shortcut for IndexOf(s, False)

    // Returns the index of the last element containing the given string.
    function LastIndexOf(const s: string; const CaseSensitive: Boolean): integer;
    function LastIndexOfStr(const s: string): integer;  // Shortcut for LastIndexOf(s, True)
    function LastIndexOfText(const s: string): integer; // Shortcut for LastIndexOf(s, False)

    // Returns the number of occurrences of the given string in the list.
    function CountOf(const s: string; const CaseSensitive: Boolean): integer;
    function CountOfStr(const s: string): integer;  // Shortcut for CountOf(s, True)
    function CountOfText(const s: string): integer; // Shortcut for CountOf(s, False)


    procedure Sort(CaseSensitive: Boolean = False; SortDirection: TSLSortDirection = sdAscending); overload;
    procedure Sort(CompareProc: TSLCompareStrProc); overload;

    procedure Reverse; // Reverses the order of items in the list. 0->Count-1 ; 1->Count-2 ...

    {
      If list is sorted with CaseSensitive = True then RemoveDuplicates(True) is very fast
      else if list is sorted with CaseSensitive = False then RemoveDuplicates(False) is very fast
      else RemoveDuplicates is SLOW
    }
    function RemoveDuplicates(CaseSensitive: Boolean = True): integer; overload;
    function RemoveDuplicates(const StartIndex: integer; CaseSensitive: Boolean = True): integer; overload;
    function RemoveDuplicates(StartItem: PSLItem; CaseSensitive: Boolean = True): integer; overload;


    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure LoadFromArray(const Arr: TStringArray);
    procedure SaveToArray(var Arr: TStringArray);    // Copies all strings to Arr array.


    procedure BeginUpdate;
    procedure EndUpdate;

    // Recreates the array with helper items used to accelerate non-sequential access to list items.
    procedure RebuildHelperArray;

    // --------------------- Properties ------------------------
    property First: PSLItem read FFirst; // The first element (head) on the list. Returns nil if list is empty.
    property Last: PSLItem read FLast; // The last element (tail) on the list. Returns nil if list is empty.
    property Count: integer read FCount; // Number of all elements in the list.
    property Sorted: Boolean read FSorted;

    // Used by GetText. All strings returned by GetText will be separated by EndOfLineMarker.
    // Default sLineBreak but can be any string.
    property EndOfLineMarker: string read FEndOfLineMarker write SetEndOfLineMarker;
    property Text: string read GetText write SetText;

    property HelperItemsDelta: LongWord read FHelperItemsDelta write SetHelperItemsDelta;
    property Items[Index: integer]: string read GetItems write SetItems; default;

    // For compatibility with TStringList. This internally calls Items[Index]
    property Strings[Index: integer]: string read GetStrings write SetStrings;
  end;


function PtrToInt(P: Pointer): PtrUInt; inline;
function PtrToStr(P: Pointer): string; inline;


implementation



{$region '                  Create / Destroy / Clear                      '}
constructor TJPStrList.Create;
begin
  inherited Create;
  FCount := 0;
  FFirst := nil;
  ClearAll;
end;

destructor TJPStrList.Destroy;
begin
  ClearAll;
  inherited Destroy;
end;

procedure TJPStrList.ClearAll;
begin
  Clear;

  FHelperItemsDelta := SL_HELPER_ITEMS_DELTA_DEFAULT;
  FUseHelperItems := True;
  FEndOfLineMarker := sLineBreak;
end;

procedure TJPStrList.Clear;
var
  PItem, PNext: PSLItem;
begin
  if FCount > 0 then
  begin

    PItem := First;

    while PItem <> nil do
    begin
      //Writeln('Removing item ', PtrToStr(PItem), ': ', PItem^.Str);
      PNext := PItem^.Next;
      ClearItem(PItem);
      Dispose(PItem);
      PItem := PNext;
    end;

  end;

  FCount := 0;
  FFirst := nil;
  FLast := nil;
  FNeedRebuildIndex := False;
  FUpdating := False;
  FForLastItem := nil;
  FSorted := False;
  FSortCaseSensitive := True;
  SetLength(ArrHelperItems, 0);

end;

{$endregion Create / Destroy / Clear}


{$region '                  Sorting                                       '}


function CompareStr_Ascending_CaseSensitive(const s1, s2: string): integer;
begin
  Result := AnsiCompareStr(s1, s2);
end;

function CompareStr_Ascending_NotCaseSensitive(const s1, s2: string): integer;
begin
  Result := AnsiCompareText(s1, s2);
end;

function CompareStr_Descending_CaseSensitive(const s1, s2: string): integer;
begin
  Result := -AnsiCompareStr(s1, s2);
end;

function CompareStr_Descending_NotCaseSensitive(const s1, s2: string): integer;
begin
  Result := -AnsiCompareText(s1, s2);
end;

procedure _ExchangeStringArrayItems(var Arr: array of string; const Index1, Index2: Integer);
var
  sTemp: string;
begin
  sTemp := Arr[Index1];
  Arr[Index1] := Arr[Index2];
  Arr[Index2] := sTemp;
end;

procedure _SortStringArray(var Arr: array of string; LeftIndex, RightIndex: Integer; CompareProc: TSLCompareStrProc);
var
  I, J, PivotIndex: Integer;
begin
  repeat

    I := LeftIndex;
    J := RightIndex;
    PivotIndex := (LeftIndex + RightIndex) shr 1;

    repeat

      while CompareProc(Arr[I], Arr[PivotIndex]) < 0 do Inc(I);
      while CompareProc(Arr[J], Arr[PivotIndex]) > 0 do Dec(J);

      if I <= J then
      begin
        if I <> J then _ExchangeStringArrayItems(Arr, I, J);

        if PivotIndex = I then PivotIndex := J
        else if PivotIndex = J then PivotIndex := I;
        Inc(I);
        Dec(J);
      end;

    until I > J;

    if LeftIndex < J then _SortStringArray(Arr, LeftIndex, J, CompareProc);
    LeftIndex := I;

  until I >= RightIndex;
end;

procedure SortStringArray(var Arr: array of string; LeftIndex, RightIndex: Integer; CompareProc: TSLCompareStrProc);
begin
  if Length(Arr) < 2 then Exit; // nothing to sort!
  _SortStringArray(Arr, LeftIndex, RightIndex, CompareProc);
end;


procedure TJPStrList.Sort(CaseSensitive: Boolean = False; SortDirection: TSLSortDirection = sdAscending);
var
  Arr: TStringArray;
begin
  if FCount < 2 then Exit;

  SetLength(Arr, 0);
  SaveToArray(Arr);
  ClearAll;

  if SortDirection = sdAscending then
  begin
    if CaseSensitive then SortStringArray(Arr, 0, High(Arr), @CompareStr_Ascending_CaseSensitive)
    else SortStringArray(Arr, 0, High(Arr), @CompareStr_Ascending_NotCaseSensitive)
  end
  else
  begin
    if CaseSensitive then SortStringArray(Arr, 0, High(Arr), @CompareStr_Descending_CaseSensitive)
    else SortStringArray(Arr, 0, High(Arr), @CompareStr_Descending_NotCaseSensitive)
  end;

  //if SortDirection = sdAscending then SortStringArray_Ascending(Arr, 0, High(Arr), CaseSensitive)
  //else SortStringArray_Descending(Arr, 0, High(Arr), CaseSensitive);

  LoadFromArray(Arr);
  FSorted := True;
  FSortCaseSensitive := CaseSensitive;
end;

procedure TJPStrList.Sort(CompareProc: TSLCompareStrProc);
var
  Arr: TStringArray;
begin
  if FCount < 2 then Exit;

  SetLength(Arr, 0);
  SaveToArray(Arr);
  ClearAll;

  SortStringArray(Arr, 0, High(Arr), CompareProc);

  LoadFromArray(Arr);
  FSorted := True;
  //FSortCaseSensitive := CaseSensitive;
end;

{$endregion Sorting}


{$region '                  Adding items & Assign                         '}
function TJPStrList.Add(const s: string): PSLItem;
var
  PItem: PSLItem;
begin
  New(PItem);
  Inc(FCount);

  ClearItem(PItem);

  PItem^.Str := s;
  GetStrHashes(s, PItem^.Hashes);
  PItem^.Index := FCount - 1;

  if Assigned(FLast) then
  begin
    PItem^.Prev := FLast;
    FLast^.Next := PItem;
  end;

  if not Assigned(FFirst) then FFirst := PItem;

  FLast := PItem;
  Result := PItem;
  FSorted := False;

  if PItem^.Index > 0 then
    if LongWord(PItem^.Index) mod FHelperItemsDelta = 0 then AddHelperItem(PItem);
end;

function TJPStrList.Add(PItem: PSLItem; Rehash: Boolean = False): PSLItem;
var
  NewItem: PSLItem;
begin
  if not Assigned(PItem) then Exit(nil);

  New(NewItem);
  CopyItem(PItem, NewItem);
  if Rehash then GetStrHashes(NewItem^.Str, NewItem^.Hashes);
  NewItem^.Next := nil;
  Inc(FCount);

  NewItem^.Index := FCount - 1;

  if Assigned(FLast) then
  begin
    NewItem^.Prev := FLast;
    FLast^.Next := NewItem;
  end;

  if not Assigned(FFirst) then FFirst := NewItem;

  FLast := NewItem;
  Result := NewItem;
  FSorted := False;

  if NewItem^.Index > 0 then
    if LongWord(NewItem^.Index) mod FHelperItemsDelta = 0 then AddHelperItem(NewItem);
end;

procedure TJPStrList.AddStrings(const Arr: TStringArray);
var
  i: integer;
begin
  for i := Low(Arr) to High(Arr) do Add(Arr[i]);
  FSorted := False;
end;

procedure TJPStrList.AddStrings(SList: TJPStrList);
var
  PItem: PSLItem;
begin
  PItem := SList.First;
  while PItem <> nil do
  begin
    Add(PItem^.Str);
    PItem := PItem^.Next;
  end;
  FSorted := False;
end;

procedure TJPStrList.Assign(SList: TJPStrList);
begin
  ClearAll;
  AddStrings(SList);
end;

{$endregion Adding items & Assign}


{$region '                  Load & Save  (file / array)                   '}
procedure TJPStrList.LoadFromFile(const FileName: string);
var
  f: TextFile;
  s: string;
begin
  ClearAll;
  AssignFile(f, FileName);
  Reset(f);
  while not Eof(f) do
  begin
    Readln(f, s);
    Add(s);
  end;
  CloseFile(f);
end;

procedure TJPStrList.SaveToFile(const FileName: string);
var
  PItem: PSLItem;
  f: TextFile;
begin
  AssignFile(f, FileName);
  Rewrite(f);

  PItem := First;
  while PItem <> nil do
  begin
    Writeln(f, PItem^.Str);
    PItem := PItem^.Next;
  end;

  CloseFile(f);
end;

procedure TJPStrList.LoadFromArray(const Arr: TStringArray);
var
  i: integer;
begin
  ClearAll;
  for i := Low(Arr) to High(Arr) do Add(Arr[i]);
end;

procedure TJPStrList.SaveToArray(var Arr: TStringArray);
var
  PItem: PSLItem;
  x: integer;
begin
  x := -1;
  SetLength(Arr, FCount);
  PItem := First;
  while PItem <> nil do
  begin
    Inc(x);
    Arr[x] := PItem^.Str;
    PItem := PItem^.Next;
  end;
end;
{$endregion Load & Save}

{$region '                  Insert & Extract                              '}
function TJPStrList.Insert(const Index: integer; const s: string): PSLItem;
var
  PItem: PSLItem;
begin
  Result := nil;
  if not IsValidIndex(Index) then raise EStrListException.Create('Exception! ' + ClassName + '.Insert: Index out of range');

  PItem := GetItemByIndex(Index);
  if not Assigned(PItem) then Exit;

  Result := InsertBefore(PItem, s);

  FSorted := False;
end;

// Inserts a new item before PItem
function TJPStrList.InsertBefore(PItem: PSLItem; const s: string): PSLItem;
var
  NewItem, Prev: PSLItem;
begin
  Result := nil;

  if not Assigned(PItem) then Exit;

  Prev := PItem^.Prev;

  New(NewItem);
  Inc(FCount);
  NewItem^.Next := PItem;
  PItem^.Prev := NewItem;
  NewItem^.Prev := Prev;
  if Assigned(Prev) then Prev^.Next := NewItem;
  NewItem^.Index := PItem^.Index;
  NewItem^.Str := s;
  GetStrHashes(s, NewItem^.Hashes);
  UpdateIndexes(NewItem);

  if NewItem^.Prev = nil then FFirst := NewItem;
  if PItem^.Next = nil then FLast := PItem;
  FForLastItem := nil;

  Result := NewItem;

  FSorted := False;

  if NewItem^.Index > 0 then
    if LongWord(NewItem^.Index) mod FHelperItemsDelta = 0 then AddHelperItem(NewItem);
end;

function TJPStrList.InsertAfter(PItem: PSLItem; const s: string): PSLItem;
var
  NewItem, Next: PSLItem;
begin
  Result := nil;

  if not Assigned(PItem) then Exit;

  Next := PItem^.Next;

  New(NewItem);
  Inc(FCount);

  NewItem^.Prev := PItem;
  PItem^.Next := NewItem;
  NewItem^.Next := Next;
  if Assigned(Next) then Next^.Prev := NewItem;

  NewItem^.Index := PItem^.Index + 1;
  NewItem^.Str := s;
  GetStrHashes(s, NewItem^.Hashes);

  UpdateIndexes(NewItem);

  if NewItem^.Next = nil then FLast := NewItem;

  FForLastItem := nil;

  Result := NewItem;

  FSorted := False;

  if NewItem^.Index > 0 then
    if LongWord(NewItem^.Index) mod FHelperItemsDelta = 0 then AddHelperItem(NewItem);
end;

function TJPStrList.Extract(const Index: integer): PSLItem;
var
  PItem, PNewItem, PNext: PSLItem;
begin
  Result := nil;
  if not IsValidIndex(Index) then raise EStrListException.Create('Exception! ' + ClassName + '.Extract: Index out of range');
  PItem := GetItemByIndex(Index);
  if PItem = nil then Exit;

  New(PNewItem);
  CopyItem(PItem, PNewItem);
  PNext := PerformDeleteItem(PItem);
  if Assigned(PNext) and (Assigned(PNext^.Prev)) then UpdateIndexes(PNext^.Prev)
  else RebuildIndexes;

  Result := PNewItem; // The caller is responsible for freeing the returned item!
end;

function TJPStrList.ExtractStr(const Index: integer): string;
var
  PItem: PSLItem;
begin
  Result := '';
  PItem := Extract(Index);
  if PItem = nil then Exit;
  Result := PItem^.Str;
  Dispose(PItem);
end;
{$endregion Insert & Extract}

{$region '                  Reverse & Exchange                            '}
procedure TJPStrList.Reverse;
var
  P1, P2, PNext1, PPrev2: PSLItem;
  x: integer;
  bSorted: Boolean;
begin
  if FCount < 2 then Exit;

  x := 0;
  bSorted := FSorted;
  P1 := FFirst;
  P2 := FLast;

  while x < (FCount div 2) do
  begin

    PNext1 := P1^.Next;
    PPrev2 := P2^.Prev;

    ExchangeItems(P1, P2);
    Inc(x);

    P1 := PNext1;
    P2 := PPrev2;

  end;

  FSorted := bSorted;
  if FUseHelperItems then RebuildHelperArray;
end;

procedure TJPStrList.ExchangeItems(Item1, Item2: PSLItem);
var
  Prev1, Next1, Prev2, Next2: PSLItem;
  Index1, Index2: integer;
begin
  if (Item1 = nil) or (Item2 = nil) then Exit;
  if Item1 = Item2 then Exit;

  Prev1 := Item1^.Prev;
  Next1 := Item1^.Next;
  Index1 := Item1^.Index;

  Prev2 := Item2^.Prev;
  Next2 := Item2^.Next;
  Index2 := Item2^.Index;


  // Elementy sąsiadujące
  if Item1^.Next = Item2 then
  begin

    Item1^.Next := Next2;
    Item1^.Prev := Item2;
    Item1^.Index := Index2;
    if Assigned(Prev1) then Prev1^.Next := Item2;

    Item2^.Next := Item1;
    Item2^.Prev := Prev1;
    Item2^.Index := Index1;
    if Assigned(Next2) then Next2^.Prev := Item1;

  end

  else

  // Elementy rozdzielone
  begin


    if Assigned(Prev1) then Prev1^.Next := Item2; // I
    if Assigned(Next1) then Next1^.Prev := Item2; // III

    // II
    Item2^.Prev := Prev1;
    Item2^.Next := Next1;

    Item2^.Index := Index1;



    if Assigned(Prev2) then Prev2^.Next := Item1; // IV
    if Assigned(Next2) then Next2^.Prev := Item1; // VI

    // V
    Item1^.Prev := Prev2;
    Item1^.Next := Next2;

    Item1^.Index := Index2;

  end;


  if Item1^.Index = 0 then FFirst := Item1
  else if Item2^.Index = 0 then FFirst := Item2;

  if Item1^.Index = Count - 1 then FLast := Item1
  else if Item2^.Index = Count - 1 then FLast := Item2;

  FSorted := False;
end;
{$endregion Reverse & Exchange}

{$region '                  Get & Set: Items, Strings, Text               '}
function TJPStrList.GetItems(Index: integer): string;
var
  PItem: PSLItem;
begin
  Result := '';
  PItem := nil;

  if not IsValidIndex(Index) then raise EStrListException.Create('Exception! ' + ClassName + '.GetItems: Index out of range');

  // przyspieszanie iteracji sekwencyjnej for i := 0 to/downto...
  if Assigned(FForLastItem) then
  begin

    // for Items[0] to ....
    if Assigned(FForLastItem^.Next) then
    begin
      PItem := FForLastItem^.Next;
      if PItem^.Index <> Index then PItem := nil;
    end;

    // for Items[Count - 1] downto ...
    if (not Assigned(PItem)) and (Assigned(FForLastItem^.Prev)) then
    begin
      PItem := FForLastItem^.Prev;
      if PItem^.Index <> Index then PItem := nil;
    end;

  end;


  if not Assigned(PItem) then PItem := GetItemByIndex(Index);
  if PItem = nil then
  begin
    FForLastItem := nil;
    Exit;
  end;

  Result := PItem^.Str;
  FForLastItem := PItem;
end;


procedure TJPStrList.SetItems(Index: integer; AValue: string);
var
  PItem: PSLItem;
begin
  if not IsValidIndex(Index) then raise EStrListException.Create('Exception! ' + ClassName + '.SetItems: Index out of range');
  PItem := GetItemByIndex(Index);
  if not Assigned(PItem) then Exit;
  PItem^.Str := AValue;
  GetStrHashes(AValue, PItem^.Hashes);
  FSorted := False;
end;

function TJPStrList.GetStrings(Index: integer): string;
begin
  Result := GetItems(Index);
end;

procedure TJPStrList.SetStrings(Index: integer; AValue: string);
begin
  SetItems(Index, AValue);
end;

function TJPStrList.GetText: string;
var
  PItem: PSLItem;
begin
  Result := '';
  PItem := FFirst;
  while PItem <> nil do
  begin
    if PItem <> FLast then Result := Result + PItem^.Str + FEndOfLineMarker
    else Result := Result + PItem^.Str;
    PItem := PItem^.Next;
  end;
  //Result := TrimFromEnd(Result, FEndOfLineMarker);
end;

procedure TJPStrList.SetEndOfLineMarker(AValue: string);
begin
  if FEndOfLineMarker = AValue then Exit;
  FEndOfLineMarker := AValue;
end;

procedure TJPStrList.SetText(AValue: string);
var
  P, Start, LB: PChar;
  s: string;
  LineBreakLen: Integer;
  LineBreak: string;
begin
  Clear;
  LineBreak := sLineBreak;

  P := Pointer(AValue);
  if P <> nil then
    if CompareStr(LineBreak, sLineBreak) = 0 then
    begin

      while P^ <> #0 do
      begin
        Start := P;
        while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
        //while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(s, Start, P - Start);
        Add(s);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;

    end

    else

    begin

      LineBreakLen := Length(LineBreak);
      while P^ <> #0 do
      begin
        Start := P;
        LB := AnsiStrPos(P, PChar(LineBreak));
        while (P^ <> #0) and (P <> LB) do Inc(P);
        SetString(s, Start, P - Start);
        Add(s);
        if P = LB then Inc(P, LineBreakLen);
      end;

    end

end;

{$endregion Get & Set: Items, Strings, Text}

{$region '                  Helper items                                  '}
procedure TJPStrList.AddHelperItem(PItem: PSLItem);
begin
  SetLength(ArrHelperItems, Length(ArrHelperItems) + 1);
  ArrHelperItems[High(ArrHelperItems)] := PItem;
end;

function TJPStrList.GetHelperItem(const Index: integer): PSLitem;
var
  x: integer;
begin
  Result := nil;
  if (Length(ArrHelperItems) = 0) or (Int64(FCount) < FHelperItemsDelta) then Exit;

  x := (Index div integer(FHelperItemsDelta)) - 1;
  if x < 0 then x := 0
  else if x > High(ArrHelperItems) then Exit(FLast);// x := High(ArrHelperItems);

  {
    Sprawdzam, czy nie lepiej będzie pobrać element następny po x-owym i zmusić GetItemByIndex do wyszukiwania wstecz.
    Np. jeżeli szukamy elementu pomocniczego dla indeksu 29, przy założeniu, że delta = 10,
    pole Index elementu x-owego z tablicy ArrHelperItems będzie wynosiło 20 (ArrHelperItems[x]^.Index = 20).
    Gdy zwrócimy ten element, funkcja GetItemByIndex musiałaby sprawdzić aż 10 elementów, aby znaleźć
    element o indeksie 29 (sprawdzane byłyby elementy: 20, 21, 22, 23, 24, 25, 26, 27, 28 i dopiero 29).
    W takiej sytuacji korzystniej jest zwrócić z tablicy ArrHelperItems element następny (x+1), którego
    indeks wynosi 30 (ArrHelperItems[x+1]^.Index = 30). Wówczas GetItemByIndex rozpocznie przeszukiwanie wstecz
    i dokona tylko 2-óch sprawdzeń: 30 i 29.
    Zwracanie elementu następnego przyspiesza iterację niesekwencyjną do ok 50%
  }
  if (x < High(ArrHelperItems)) and ( (Index mod ArrHelperItems[x]^.Index) > (integer(FHelperItemsDelta) div 2) ) then Inc(x);

  Result := ArrHelperItems[x];
  //Writeln('Index (param): ', Index, '  x: ', x, '         Helper item^.Index: ', ArrHelperItems[x]^.Index);
end;

procedure TJPStrList.RebuildHelperArray;
var
  PItem: PSLItem;
begin
  SetLength(ArrHelperItems, 0);
  PItem := First;
  while PItem <> nil do
  begin
    if PItem^.Index > 0 then
      if (LongWord(PItem^.Index) mod FHelperItemsDelta) = 0 then AddHelperItem(PItem);
    PItem := PItem^.Next;
  end;
end;

procedure TJPStrList.DisplayHelperItems;
var
  i: integer;
  PItem: PSLItem;
begin
  if not IsConsole then Exit;
  Writeln('Helper items: ', IntToStrEx(Length(ArrHelperItems)));
  for i := 0 to High(ArrHelperItems) do
  begin
    PItem := ArrHelperItems[i];
    Writeln(IntToStrEx(i + 0), '. Index: ', IntToStrEx(PItem^.Index));
  end;
end;
{$endregion Helper items}

{$region '                  GetItemBy... Str / Index                      '}
function TJPStrList.GetItemByIndex(const Index: integer): PSLItem;
var
  PItem: PSLItem;
  bForward: Boolean;
begin
  Result := nil;
  if FCount = 0 then Exit;

  if Assigned(FLast) and (FLast^.Index = Index) then Exit(FLast);
  if Assigned(FFirst) and (FFirst^.Index = Index) then Exit(FFirst);

  bForward := True;
  PItem := nil;

  // Próba pobrania elementu pomocniczego, aby przyspieszyć wyszukiwanie
  if FUseHelperItems then
  begin
    PItem := GetHelperItem(Index);
    if Assigned(PItem) then
      if PItem^.Index >= Index then bForward := False;
  end;

  // Jeżeli Index znajduje się w dugiej połówce listy, szukamy od końca.
  // W przeciwnym przypadku - od początku listy.
  if not Assigned(PItem) then
  begin
    if Index > (FCount div 2) then
    begin
      PItem := FLast;
      bForward := False;
    end
    else
      PItem := FFirst;
  end;

  while PItem <> nil do
  begin
    if PItem^.Index = Index then
    begin
      Result := PItem;
      Break;
    end;
    if bForward then PItem := PItem^.Next
    else PItem := PItem^.Prev;
  end;

end;



function TJPStrList.GetItemByStr(const s: string; CaseSensitive: Boolean = True; ForwardSearch: Boolean = True): PSLItem;
var
  PItem: PSLItem;
  Hashes: TSLHashes;
  b: Boolean;
begin
  Result := nil;
  if FCount = 0 then Exit;

  GetStrHashes(s, Hashes{%H-});

  if ForwardSearch then PItem := First
  else PItem := FLast;

  while PItem <> nil do
  begin

    if not CaseSensitive then b := (PItem^.Hashes.UHash = Hashes.UHash) and (PItem^.Hashes.UHash2 = Hashes.UHash2)
    else b := (PItem^.Hashes.Hash = Hashes.Hash) and (PItem^.Hashes.Hash2 = Hashes.Hash2);

    if b then
    begin
      Result := PItem;
      Break;
    end;

    if ForwardSearch then PItem := PItem^.Next
    else PItem := PItem^.Prev;

  end;
end;

{$endregion GetItemBy... Str / Index}

{$region '                  IndexOf, LastIndexOf, CountOf                 '}
function TJPStrList.IndexOf(const s: string; const CaseSensitive: Boolean): integer;
var
  PItem: PSLItem;
begin
  Result := -1;
  PItem := GetItemByStr(s, CaseSensitive, True);
  if Assigned(PItem) then Result := PItem^.Index;
end;

function TJPStrList.IndexOfStr(const s: string): integer;
begin
  Result := IndexOf(s, True);
end;

function TJPStrList.IndexOfText(const s: string): integer;
begin
  Result := IndexOf(s, False);
end;

function TJPStrList.LastIndexOf(const s: string; const CaseSensitive: Boolean): integer;
var
  PItem: PSLItem;
begin
  Result := -1;
  PItem := GetItemByStr(s, CaseSensitive, False);
  if Assigned(PItem) then Result := PItem^.Index;
end;

function TJPStrList.LastIndexOfStr(const s: string): integer;
begin
  Result := LastIndexOf(s, True);
end;

function TJPStrList.LastIndexOfText(const s: string): integer;
begin
  Result := LastIndexOf(s, False);
end;

function TJPStrList.CountOf(const s: string; const CaseSensitive: Boolean): integer;
var
  PItem: PSLItem;
  Hashes: TSLHashes;
  b: Boolean;
begin
  Result := 0;
  GetStrHashes(s, Hashes{%H-});

  PItem := FFirst;
  while PItem <> nil do
  begin
    if CaseSensitive then b := (Hashes.Hash = PItem^.Hashes.Hash) and (Hashes.Hash2 = PItem^.Hashes.Hash2)
    else b := (Hashes.UHash = PItem^.Hashes.UHash) and (Hashes.UHash2 = PItem^.Hashes.UHash2);
    if b then Inc(Result);
    PItem := PItem^.Next;
  end;
end;

function TJPStrList.CountOfStr(const s: string): integer;
begin
  Result := CountOf(s, True);
end;

function TJPStrList.CountOfText(const s: string): integer;
begin
  Result := CountOf(s, False);
end;

{$endregion IndexOf, LastIndexOf, CountOf}

{$region '                  Delete                                        '}

function TJPStrList.PerformDeleteItem(PItem: PSLItem): PSLItem;
var
  Prev, Next: PSLItem;
begin
  Result := nil;
  if not Assigned(PItem) then Exit;

  Prev := PItem^.Prev;
  Next := PItem^.Next;
  if PItem = FFirst then FFirst := Next;
  if PItem = FLast then FLast := Prev;
  if Assigned(Prev) then Prev^.Next := Next;
  if Assigned(Next) then Next^.Prev := Prev;
  ClearItem(PItem);
  Dispose(PItem);
  Result := Next;
  if Assigned(Result) then
  begin
    if Result^.Next = nil then FLast := Result;
    if Result^.Prev = nil then FFirst := Result;
  end;
  if FCount > 0 then Dec(FCount);
end;

function TJPStrList.Delete(const s: string; StartItem: PSLItem; OnlyFirst: Boolean = True; CaseSensitive: Boolean = True): integer;
var
  PItem: PSLItem;
  Hashes: TSLHashes;
  bDel: Boolean;
begin
  Result := 0;
  if FCount = 0 then Exit;
  GetStrHashes(s, Hashes{%H-});

  PItem := StartItem;
  while PItem <> nil do
  begin

    if not CaseSensitive then bDel := (PItem^.Hashes.UHash = Hashes.UHash) and (PItem^.Hashes.UHash2 = Hashes.UHash2)
    else bDel := (PItem^.Hashes.Hash = Hashes.Hash) and (PItem^.Hashes.Hash2 = Hashes.Hash2);

    if bDel then
    begin
      PItem := PerformDeleteItem(PItem);
      if FUpdating then FNeedRebuildIndex := True;
      if Assigned(PItem) then
      begin
        PItem^.Index := PItem^.Index - 1;
        if not FUpdating then UpdateIndexes(PItem);
      end;
      Inc(Result);
      if OnlyFirst then Break;
    end;

    if not bDel then PItem := PItem^.Next;
  end;

end;

function TJPStrList.Delete(const s: string; OnlyFirst: Boolean = True; CaseSensitive: Boolean = True; StartIndex: integer = 0): integer;
var
  PItem: PSLItem;
  Hashes: TSLHashes;
  bDel: Boolean;
begin
  Result := 0;
  if FCount = 0 then Exit;
  GetStrHashes(s, Hashes{%H-});

  PItem := First;
  while PItem <> nil do
  begin
    if PItem^.Index < StartIndex then
    begin
      PItem := PItem^.Next;
      Continue;
    end;

    if not CaseSensitive then bDel := (PItem^.Hashes.UHash = Hashes.UHash) and (PItem^.Hashes.UHash2 = Hashes.UHash2)
    else bDel := (PItem^.Hashes.Hash = Hashes.Hash) and (PItem^.Hashes.Hash2 = Hashes.Hash2);

    if bDel then
    begin
      PItem := PerformDeleteItem(PItem);
      if FUpdating then FNeedRebuildIndex := True;
      if Assigned(PItem) then
      begin
        PItem^.Index := PItem^.Index - 1;
        if not FUpdating then UpdateIndexes(PItem);
      end;
      //Dec(FCount); // FCount updated in PerformDelete
      Inc(Result);
      if OnlyFirst then Break;
    end;

    if not bDel then PItem := PItem^.Next;
  end;

end;

function TJPStrList.Delete(const Index: integer): Boolean;
var
  PItem: PSLItem;
begin
  Result := False;
  PItem := GetItemByIndex(Index);
  if not Assigned(PItem) then Exit;
  PItem := PerformDeleteItem(PItem);
  //Dec(FCount); // FCount updated in PerformDelete
  Result := True;
  if Assigned(PItem) then
  begin
    PItem^.Index := PItem^.Index - 1;
    if PItem <> FLast then UpdateIndexes(PItem);
  end;
end;

function TJPStrList.Delete(PItem: PSLItem): PSLItem;
begin
  Result := PerformDeleteItem(PItem);
end;

{$endregion Delete}

{$region '                  RemoveDuplicates                              '}
function TJPStrList.RemoveDuplicates(CaseSensitive: Boolean = True): integer;
begin
  //Writeln('FSorted: ', FSorted, '     FSortCaseSensitive: ', FSortCaseSensitive, '       CaseSensitive: ', CaseSensitive);
  if FSorted and (FSortCaseSensitive = CaseSensitive) then Result := RemoveDuplicatesFromSortedList(FFirst, CaseSensitive)
  else Result := RemoveDuplicates(FFirst, CaseSensitive);
end;

function TJPStrList.RemoveDuplicates(const StartIndex: integer; CaseSensitive: Boolean = True): integer;
var
  PItem: PSLItem;
begin
  Result := 0;
  PItem := GetItemByIndex(StartIndex);
  if not Assigned(PItem) then Exit;
  if FSorted and (FSortCaseSensitive = CaseSensitive) then Result := RemoveDuplicatesFromSortedList(PItem, CaseSensitive)
  else Result := RemoveDuplicates(PItem, CaseSensitive);
end;

function TJPStrList.RemoveDuplicates(StartItem: PSLItem; CaseSensitive: Boolean = True): integer;
var
  PItem: PSLItem;
  x: integer;
begin
  Result := 0;
  if FCount < 2 then Exit;

  PItem := StartItem;
  if not Assigned(PItem) then Exit;

  while PItem <> nil do
  begin

    x := Delete(PItem^.Str, PItem^.Next, False, CaseSensitive);
    Inc(Result, x);
    //Dec(FCount, x); // FCount is updated in PerformDeleteItem
    PItem := PItem^.Next;
    //if FCount < 0 then raise EStrListException.Create('FCount = ' + FCount.ToString);
  end;

  FNeedRebuildIndex := False;
  RebuildIndexes;
  if (Result > 0) and FUseHelperItems then RebuildHelperArray;
end;

function TJPStrList.RemoveDuplicatesFromSortedList(StartItem: PSLItem; CaseSensitive: Boolean): integer;
var
  PItem, Prev: PSLItem;
  //x: integer;
  bDup: Boolean;
begin

  Result := 0;
  if FCount < 2 then Exit;
  PItem := StartItem;
  if not Assigned(PItem) then Exit;

  Prev := nil;
  //bDup := False;


  while PItem <> nil do
  begin

    if Assigned(Prev) then
    begin

      if not CaseSensitive then bDup := (Prev^.Hashes.UHash = PItem^.Hashes.UHash) and (Prev^.Hashes.UHash2 = PItem^.Hashes.UHash2)
      else bDup := (Prev^.Hashes.Hash = PItem^.Hashes.Hash) and (Prev^.Hashes.Hash2 = PItem^.Hashes.Hash2);

      if bDup then
      begin
        Inc(Result);
        PItem := Delete(PItem);
        Continue;
      end;

    end;

    Prev := PItem;

    //x := Delete(PItem^.Str, PItem^.Next, False, CaseSensitive);
    //Inc(Result, x);
    PItem := PItem^.Next;

  end;

  FNeedRebuildIndex := False;
  RebuildIndexes;
  if (Result > 0) and FUseHelperItems then RebuildHelperArray;
end;



{$endregion RemoveDuplicates}


{$region '                  Rebuild / Update indexes                      '}
procedure TJPStrList.RebuildIndexes;
var
  PItem: PSLItem;
  x: integer;
begin
  x := 0;
  PItem := First;
  while PItem <> nil do
  begin
    PItem^.Index := x;
    Inc(x);
    PItem := PItem^.Next;
  end;
  FNeedRebuildIndex := False;
end;

procedure TJPStrList.UpdateIndexes(const StartItem: PSLItem);
var
  PItem: PSLItem;
  x: integer;
begin
  if not Assigned(StartItem) then Exit;

  PItem := StartItem^.Next;
  if not Assigned(PItem) then Exit;
  x := StartItem^.Index;

  while PItem <> nil do
  begin
    Inc(x);
    PItem^.Index := x;
    PItem := PItem^.Next;
  end;
end;
{$endregion Rebuild / Update indexes}

{$region '                  Display                                       '}
procedure TJPStrList.Display(ShowNumbers: Boolean = False);
var
  PItem: PSLItem;
begin
  if not IsConsole then Exit;
  //Writeln('Count: ', FCount);
  PItem := First;
  while PItem <> nil do
  begin
    if ShowNumbers then Writeln(IntToStrEx(PItem^.Index + 1), '. ', PItem^.Str)
    else Writeln(PItem^.Str);
    PItem := PItem^.Next;
  end;
end;

procedure TJPStrList.Display(const StartIndex, EndIndex: integer; ShowNumbers: Boolean);
var
  PItem: PSLItem;
begin
  if not IsConsole then Exit;
  if StartIndex > EndIndex then Exit;
  PItem := GetItemByIndex(StartIndex);
  if not Assigned(PItem) then Exit;

  while PItem <> nil do
  begin
    if ShowNumbers then Writeln(IntToStrEx(PItem^.Index + 1), '. ', PItem^.Str)
    else Writeln(PItem^.Str);
    if PItem^.Index = EndIndex then Break;
    PItem := PItem^.Next;
  end;
end;

procedure TJPStrList.Debug_Display(bShowItems: Boolean = True);
var
  PItem: PSLItem;
  xSizeAll, xChars, xMemChars, x: Int64;
  s: string;
begin
  if not IsConsole then Exit;
  Writeln('Size of one item: ', SizeOf(TSLItem));
  Writeln('SizeOf(Char): ', SizeOf(Char));
  xSizeAll := SizeOfAllItems;
  Writeln('Size of all items: ', IntToStrEx(xSizeAll), ' [', GetFileSizeString(xSizeAll), ']');

  xChars := CharacterCount;
  xMemChars := xChars * SizeOf(Char);
  Writeln('Characters: ', IntToStrEx(xChars), ' [', GetFileSizeString(xMemChars), ']');


  Writeln('Count: ', IntToStrEx(FCount));

  s := PtrToStr(FFirst);
  if Assigned(FFirst) then s := s + '  Index: ' + IntToStrEx(FFirst^.Index);
  Writeln('First: ', s);

  s := PtrToStr(FLast);
  if Assigned(FLast) then s := s + '  Index: ' + IntToStrEx(FLast^.Index);
  Writeln('Last: ', s);

  Writeln('Helper items enabled: ', FUseHelperItems);
  if FUseHelperItems then
  begin
    Writeln('Helper items delta: ', IntToStrEx(FHelperItemsDelta));
    Writeln('Helper items: ', IntToStrEx(Length(ArrHelperItems)));
    x := Int64(Length(ArrHelperItems)) * SizeOf(Pointer);
    if x > 0 then Writeln('Helper items size: ', IntToStrEx(x), '   [', GetFileSizeString(x), ']');
  end;

  if bShowItems then
  begin

    Writeln('I T E M S:');
    PItem := First;
    while PItem <> nil do
    begin
      Writeln(
        PtrToStr(PItem) +
        ' [Prev: ', PtrToStr(PItem^.Prev), ']' +
        ' [Next: ', PtrToStr(PItem^.Next), ']' +
        ' [Ind: ', IntToStrEx(PItem^.Index) + ']' +
        ' Str: ', PItem^.Str
      );
      PItem := PItem^.Next;
    end;

  end;
end;
{$endregion Display}



procedure TJPStrList.GetNameValue(const Index: integer; out AName, AValue: string);
var
  PItem: PSLItem;
  s, sep: string;
  xp: integer;
begin
  AName := '';
  AValue := '';
  PItem := GetItemByIndex(Index);
  if not Assigned(PItem) then Exit;

  sep := '=';

  s := PItem^.Str;
  xp := Pos(sep, s);
  if xp > 0 then
  begin
    AName := Copy(s, 1, xp - 1);
    AValue := Copy(s, xp + Length(sep), Length(s));
  end
  else AValue := s;
end;


procedure TJPStrList.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TJPStrList.EndUpdate;
begin
  FUpdating := False;
  if FNeedRebuildIndex then RebuildIndexes;
  if FUseHelperItems then RebuildHelperArray;
end;

function TJPStrList.IsValidIndex(const Index: integer): Boolean;
begin
  if FCount = 0 then Exit(False);
  //Result := Index in [0..FCount - 1]; // ?? dlaczego to nie działa?
  Result := (Index >= 0) and (Index < FCount);
end;

procedure TJPStrList.CopyItem(const Src: PSLItem; var Dest: PSLItem);
begin
  ClearItem(Dest);
  {%H-}Dest^.Hashes := Src^.Hashes;
  Dest^.Index := Src^.Index;
  Dest^.Prev := Src^.Prev;
  Dest^.Next := Src^.Next;
  Dest^.Str := Src^.Str;
end;

procedure TJPStrList.ClearItem(var PItem: PSLItem);
begin
  if not Assigned(PItem) then Exit;
  PItem^.Str := '';
  PItem^.Next := nil;
  PItem^.Prev := nil;
  PItem^.Index := -1;
  FillChar(PItem^.Hashes, SizeOf(PItem^.Hashes), 0);
end;

procedure TJPStrList.GetStrHashes(const s: string; var Hashes: TSLHashes);
var
  xLen: integer;
begin
  xLen := Length(s) * SizeOf(Char);
  Hashes.Hash := FPHash(PChar(s), xLen);
  Hashes.UHash := FPHash(PChar(AnsiUpperCase(s)), xLen);
  Hashes.Hash2 := xxHash32(0, PChar(s), xLen);
  Hashes.UHash2 := xxHash32(0, PChar(AnsiUpperCase(s)), xLen);
end;

function TJPStrList.GetItemFullSize(const PItem: PSLItem): integer;
var
  x: integer;
begin
  x := SizeOf(PItem^) + SizeOf(Pointer);
  x := x + Length(PItem^.Str) * SizeOf(Char);
  Result := x;
end;

procedure TJPStrList.SetHelperItemsDelta(AValue: LongWord);
begin
  if FHelperItemsDelta = AValue then Exit;
  if AValue = 0 then
  begin
    SetLength(ArrHelperItems, 0);
    FUseHelperItems := False;
    Exit;
  end;
  FUseHelperItems := True;
  if AValue < SL_HELPER_ITEMS_DELTA_MIN then AValue := SL_HELPER_ITEMS_DELTA_MIN;
  FHelperItemsDelta := AValue;
  RebuildHelperArray;
end;

function TJPStrList.SizeOfAllItems: Int64;
var
  PItem: PSLItem;
begin
  Result := 0;
  PItem := First;
  while PItem <> nil do
  begin
    Result := Result + GetItemFullSize(PItem);
    PItem := PItem^.Next;
  end;
end;

function TJPStrList.CharacterCount: Int64;
var
  PItem: PSLItem;
begin
  Result := 0;
  PItem := First;
  while PItem <> nil do
  begin
    Result := Result + Length(PItem^.Str);
    PItem := PItem^.Next;
  end;
end;


//////////////////////////////// helpers ///////////////////////////////////
function PtrToInt(P: Pointer): PtrUInt; inline;
begin
  if not Assigned(P) then Result := 0
  else Result := {%H-}PtrUInt(P);
end;

function PtrToStr(P: Pointer): string;
begin
  Result := '$' + IntToHex(PtrToInt(P), 8);
end;



end.
