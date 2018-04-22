unit JPLM.FileSearcher;

{
  Jacek Pazera
  http://www.pazera-software.com
}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  MFPC.LazUtils.LazUTF8,
  SysUtils,

  fgl,
  JPL.StrList,
  JPL.Strings, JPLM.FileSearch, JPLM.Files;


type

  TFSFileInfoMode = (fimOnlyFileNames, fimFull);

const
  FS_DEAFULT_RECURSE_DEPTH = 250;
  FS_DEFAULT_FILE_INFO_MODE: TFSFileInfoMode = fimOnlyFileNames;

type

  TFSSearchRec = record
    InputItemIndex: integer;
    ResultIndex: integer;
  end;

  TFSFileDates = record
    LastWrite: TDateTime;
    LastAccess: TDateTime;
    {$IFDEF MSWINDOWS} Creation: TDateTime; {$ENDIF}
  end;

  TFSResultItem = record
    FileName: string;
    FileSize: Int64;
    Dates: TFSFileDates;
    ExtInfoOK: Boolean;
  end;

  TFSResults = specialize TFPGMap<integer,TFSResultItem>;
  TFSFilesToSerach = specialize TFPGList<string>;

  TFSItem = record
    Directory: string;
    RecurseDepth: WORD;
    FilesToSearch: TFSFilesToSerach;
    Results: TFSResults;
    function ResultCount: integer;
  end;

  TFSItems = specialize TFPGMap<integer,TFSItem>;
  //TFSOutput = specialize TFPGMap<integer,TFSOutputItem>;

  TFSStats = record
    MinFileSize: Int64;
    MaxFileSize: Int64;
    SizeSum: Int64; // 2^64 = 16 777 216 TB, Int64: -8 388 608(7) TB .. +8 388 608(7) TB
    MinFileNameLen: WORD;
    MaxFileNameLen: WORD;
    FileCount: integer;
  end;


  {
    Metoda postępowania z TJPFileSearcher:
    1. Ustawić FileInfoMode (domyślnie fimOnlyFileNames - tylko nazwy plików).
    2. Dodać pliki do wyszukania za pomocą AddInput.
    3. Wywołać Search.
    4: Pobrać listę znalezionych plików: GetFileList lub GetFirstFile + GetNextFile, lub Items[Index].
       OutputCount zwraca liczbę znalezionych plików.
  }


  TJPFileSearcher = class
  private
    FFileCountLimit: DWORD;
    FFileInfoMode: TFSFileInfoMode;
    FItems: TFSItems;
    FRecurseDepth: WORD;
    FSearchRec: TFSSearchRec;
    FStats: TFSStats;
    function GetInputCount: integer;
    function GetItems(Index: integer): TFSItem;
    function GetOutputCount: integer;
    procedure SetFileCountLimit(AValue: DWORD);

    procedure SetFileInfoMode(AValue: TFSFileInfoMode);
    procedure SetRecurseDepth(AValue: WORD);
  public

    constructor Create;
    destructor Destroy; override;

    // Starts the search. First, provide the input for the search using Addinput.
    procedure Search;

    // Ustawianie wartości domyślnych zmiennych i czyszczenie list.
    procedure ClearAll;

    // Tutaj podaje się dane wejściowe dla wyszukiwania:
    // Dir - Directory to search.
    // FileMasks - An array with file masks.
    // Returns the index of the newly added item in the FItems dictionary (map).
    function AddInput(const Dir: string; FileMasks: array of string; RecurseDepth: integer = FS_DEAFULT_RECURSE_DEPTH): integer;

    // An attempt to retrieve an FSInputItem element with the given index.
    function TryGetInputItem(const Index: integer; out FSInputItem: TFSItem): Boolean;

    // Saves the names of all found files to FileList.
    procedure GetFileList(FileList: TJPStrList);

    // GetFirstFile returns the first file found, a GetNextFile - kolejne.
    // Similar to FindFirst and FindNext
    function GetFirstFile(var FileName: string): Boolean;
    function GetNextFile(var FileName: string): Boolean;

    function InputAsStr: string; // for debug purposes

    // Fills the Stats record.
    procedure UpdateStats;

    // The number of all input elements given in the AddInput function.
    property InputCount: integer read GetInputCount;

    // The number of files found.
    property OutputCount: integer read GetOutputCount;

    // fimOnlyFileNames - only the file names are collected during the search.
    // fimFull - fimOnlyFileNames + additional information (size, dates).
    property FileInfoMode: TFSFileInfoMode read FFileInfoMode write SetFileInfoMode;

    // Default recurse depth if not specified in the AddInput function.
    property RecurseDepth: WORD read FRecurseDepth write SetRecurseDepth;

    // Iteration of all elements:  (or GetFirstFile + GetNextFile, but only for file names)
    // for i := 0 to FileSearcher.OutputCount - 1 do Writeln(FileSearcher[i].FileName); ...
    property Items[Index: integer]: TFSItem read GetItems {write SetItems}; default;

    // To read the fields of this record, first call the procedure UpdateStats.
    property Stats: TFSStats read FStats;

    // The search stops when FileCountLimit files are found. 0 = no limit (default)
    property FileCountLimit: DWORD read FFileCountLimit write SetFileCountLimit;
  end;


{$region '      helpers        '}
function FSOuputItemToStr(FSOutputItem: TFSResultItem; ShowOnlyNames: Boolean = False; Indent: string = '  '): string;
function FSInputItemToStr(FSInputItem: TFSItem; ShowResults: Boolean = True; Indent: string = '  '): string;
{$endregion}

implementation

function TFSItem.ResultCount: integer;
begin
  Result := Self.Results.Count;
end;





{$region '                  Create, Clear, Destroy                 '}
constructor TJPFileSearcher.Create;
begin
  FItems := TFSItems.Create;
  ClearAll;
end;

destructor TJPFileSearcher.Destroy;
begin
  ClearAll;
  FItems.Free;
  inherited Destroy;
end;


procedure TJPFileSearcher.ClearAll;
var
  i: integer;
begin
  FRecurseDepth := FS_DEAFULT_RECURSE_DEPTH;
  FFileInfoMode := FS_DEFAULT_FILE_INFO_MODE;
  FFileCountLimit := 0;

  for i := 0 to FItems.Count - 1 do
  begin
    if Assigned(FItems[i].FilesToSearch) then FItems[i].FilesToSearch.Free;
    if Assigned(FItems[i].Results) then FItems[i].Results.Free;
  end;
  FItems.Clear;
end;

{$endregion Create, Clear, Destroy}

{$region '                          Search                            '}
procedure TJPFileSearcher.Search;
var
  i, x, y, xFiles: integer;
  ii: TFSItem;
  sl: TJPStrList;
  Dir, fMask: string;
  oi: TFSResultItem;
  fir: TFileInfoRec;

  function CanContinue: Boolean;
  begin
    Result := not ( (FFileCountLimit > 0) and (xFiles >= integer(FFileCountLimit)) );
  end;

begin
  sl := TJPStrList.Create;
  try

    xFiles := 0;

    for i := 0 to FItems.Count - 1 do
    begin

      ii := FItems.Data[i];
      Dir := ii.Directory;
      Dir := Trim(Dir);
      if Dir = '' then Continue;
      if not DirectoryExists(Dir) then Continue;

      for x := 0 to ii.FilesToSearch.Count - 1 do
      begin
        if not CanContinue then Break;

        fMask := ii.FilesToSearch[x];
        fMask := Trim(fMask);
        if fMask = '' then Continue;
        sl.Clear;

        JPGetFileList(fMask, Dir, sl, ii.RecurseDepth, True, True, nil, nil, nil);
        for y := 0 to sl.Count - 1 do
        begin
          Inc(xFiles);

          oi.FileName := sl[y];
          oi.FileSize := 0;
          {$IFDEF MSWINDOWS} oi.Dates.Creation := 0; {$ENDIF}
          oi.Dates.LastAccess := 0;
          oi.Dates.LastWrite := 0;
          oi.ExtInfoOK := False;
          if FFileInfoMode = fimFull then
          begin
            //oi.FileSize := FileSizeInt(sl[y]);
            GetFileInfoRec(sl[y], fir, False);
            oi.FileSize := fir.Size;
            oi.Dates.LastWrite := fir.LastWriteTime;
            oi.Dates.LastAccess := fir.LastAccessTime;
            {$IFDEF MSWINDOWS} oi.Dates.Creation := fir.CreationTime; {$ENDIF}
            oi.ExtInfoOK := True;
          end;
          ii.Results.Add(ii.Results.Count, oi);

          if not CanContinue then Break;
        end;

      end;

    end;

  finally
    sl.Free;
  end;
end;
{$endregion Search}

function TJPFileSearcher.AddInput(const Dir: string; FileMasks: array of string; RecurseDepth: integer): integer;
var
  ii: TFSItem;
  i: integer;
begin
  ii.Directory := Dir;
  ii.RecurseDepth := RecurseDepth;

  ii.FilesToSearch := TFSFilesToSerach.Create;
  for i := 0 to High(FileMasks) do ii.FilesToSearch.Add(FileMasks[i]);

  ii.Results := TFSResults.Create;

  FItems.AddOrSetData(FItems.Count, ii);
  Result := FItems.Count - 1;
end;

function TJPFileSearcher.TryGetInputItem(const Index: integer; out FSInputItem: TFSItem): Boolean;
begin
  if FItems.Count = 0 then Exit(False);
  Result := Index in [0..FItems.Count - 1];
  FSInputItem := FItems.Data[Index];
end;

procedure TJPFileSearcher.GetFileList(FileList: TJPStrList);
var
  i, x: integer;
  ii: TFSItem;
begin
  FileList.Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    ii := FItems.Data[i];
    for x := 0 to ii.Results.Count - 1 do
      FileList.Add(ii.Results[x].FileName);
  end;
end;

function TJPFileSearcher.GetFirstFile(var FileName: string): Boolean;
var
  i: integer;
  ii: TFSItem;
begin
  FSearchRec.InputItemIndex := 0;
  FSearchRec.ResultIndex := 0;
  Result := False;
  if FItems.Count = 0 then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    ii := FItems.Data[i];
    if ii.Results.Count > 0 then
    begin
      FileName := ii.Results[0].FileName;
      FSearchRec.InputItemIndex := i;
      Exit(True);
    end;
  end;
end;

function TJPFileSearcher.GetNextFile(var FileName: string): Boolean;
var
  ii: TFSItem;
  xIInd, xRInd, i: integer;
begin
  Result := False;
  if FItems.Count = 0 then Exit;
  if FSearchRec.InputItemIndex > FItems.Count - 1 then Exit;

  xIInd := FSearchRec.InputItemIndex;
  xRInd := FSearchRec.ResultIndex + 1;

  ii := FItems.Data[xIInd];

  if xRInd > ii.Results.Count - 1 then
  begin

    for i := xIInd + 1 to FItems.Count - 1 do
    begin
      ii := FItems.Data[i];
      if ii.Results.Count > 0 then
      begin
        FileName := ii.Results[0].FileName;
        FSearchRec.InputItemIndex := i;
        FSearchRec.ResultIndex := 0;
        Exit(True);
      end;
    end;

  end

  else
  begin
    FileName := ii.Results[xRInd].FileName;
    FSearchRec.InputItemIndex := xIInd;
    FSearchRec.ResultIndex := xRInd;
    Result := True;
  end;


end;

function TJPFileSearcher.InputAsStr: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FItems.Count - 1 do
  begin
    Result += '  ### InputItem ' + IntToStrEx(i + 1) + ' / ' + IntToStrEx(FItems.Count) + ' ###' + ENDL;
    Result += FSInputItemToStr(FItems[i]) + ENDL;
    Result += '------------------------------------' + ENDL;
  end;
end;



function TJPFileSearcher.GetInputCount: integer;
begin
  Result := FItems.Count;
end;

function TJPFileSearcher.GetItems(Index: integer): TFSItem;
begin
  Result := FItems.Data[Index];
end;

function TJPFileSearcher.GetOutputCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
    Result += FItems.Data[i].Results.Count;
end;

procedure TJPFileSearcher.SetFileCountLimit(AValue: DWORD);
begin
  if FFileCountLimit = AValue then Exit;
  FFileCountLimit := AValue;
end;

procedure TJPFileSearcher.UpdateStats;
var
  i, x, xLen: integer;
  xSize: Int64;
  ri: TFSResultItem;
begin
  FStats.MinFileSize := 0;
  FStats.MaxFileSize := 0;
  FStats.SizeSum := 0;
  FStats.MinFileNameLen := 0;
  FStats.MaxFileNameLen := 0;
  FStats.FileCount := GetOutputCount;

  if FStats.FileCount = 0 then Exit;

  for i := 0 to FItems.Count - 1 do
    for x := 0 to FItems.Data[i].ResultCount - 1 do
    begin

      ri := FItems.Data[i].Results.Data[x];
      xLen := Length(ri.FileName);
      if xLen < FStats.MinFileNameLen then FStats.MinFileNameLen := xLen;
      if xLen > FStats.MaxFileNameLen then FStats.MaxFileNameLen := xLen;

      if not ri.ExtInfoOK then Continue;
      xSize := ri.FileSize;
      if xSize < FStats.MinFileSize then FStats.MinFileSize := xSize;
      if xSize > FStats.MaxFileSize then FStats.MaxFileSize := xSize;
    end;

end;

procedure TJPFileSearcher.SetFileInfoMode(AValue: TFSFileInfoMode);
begin
  if FFileInfoMode = AValue then Exit;
  FFileInfoMode := AValue;
end;



procedure TJPFileSearcher.SetRecurseDepth(AValue: WORD);
begin
  if FRecurseDepth = AValue then Exit;
  FRecurseDepth := AValue;
end;



{$region '            helpers           '}

function FSOuputItemToStr(FSOutputItem: TFSResultItem; ShowOnlyNames: Boolean; Indent: string): string;
var
  s: string;
begin
  Result := Indent + FSOutputItem.FileName;
  if ShowOnlyNames then Exit;
  s := IntToStrEx(FSOutputItem.FileSize) + ' bytes';
  if FSOutputItem.FileSize > 1024 then s += ' (' + GetFileSizeString(FSOutputItem.FileSize) + ')';
  Result +=
    ENDL + Indent + 'FileSize: ' + s + ENDL +
    Indent + 'LastWrite: ' + DateTimeToStr(FSOutputItem.Dates.LastWrite) + ENDL +
    Indent + 'LastAccess: ' + DateTimeToStr(FSOutputItem.Dates.LastAccess);
  {$IFDEF MSWINDOWS} Result += ENDL + Indent + 'Creation: ' + DateTimeToStr(FSOutputItem.Dates.Creation); {$ENDIF}
  Result += ENDL;
end;

function FSInputItemToStr(FSInputItem: TFSItem; ShowResults: Boolean; Indent: string): string;
var
  Indent2: string;
  i: integer;
  FSOutput: TFSResultItem;
begin
  Indent2 := Indent + '  ';

  if not Assigned(FSInputItem.FilesToSearch) then Exit('');

  Result :=
    Indent + 'Directory: ' + FSInputItem.Directory + ENDL +
    Indent + 'RecurseDepth: ' + FSInputItem.RecurseDepth.ToString;

  if Assigned(FSInputItem.FilesToSearch) then
  begin
    Result += ENDL + Indent + 'Number of files to search: ' + FSInputItem.FilesToSearch.Count.ToString;

    for i := 0 to FSInputItem.FilesToSearch.Count - 1 do
      Result += ENDL + Indent2 + FSInputItem.FilesToSearch[i];
  end;

  if not ShowResults then Exit;

  if FSInputItem.Results.Count > 0 then Result += ENDL + 'Results: ' + IntToStrEx(FSInputItem.Results.Count);
  for i := 0 to FSInputItem.Results.Count - 1 do
  begin
    FSOutput := FSInputItem.Results.Data[i];
    Result += ENDL + FSOuputItemToStr(FSOutput, False, Indent2);
  end;

end;

{$endregion helpers}


end.

