unit JPL.FileSearcher;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 26.04.2019

  A class that allows you to search for files in many specified locations (local HDD, UNC paths) and for different file masks.
  Long file names (over 255 characters) are not a problem.

  How to play with TJPFileSearcher:
  1: Set FileInfoMode to desired value. Default = fimOnlyFileNames - returns only file names (without checking the file sizes and dates).
  2. Add the starting directory and the file masks to be searched using the AddInput method. You can call AddInput many times, with different input data.
         Eg.: AddInput('C:\Windows', ['*.ini', '*.exe']);
              AddInput('\\nas\shared_music\', ['*.mp3', '*.flac', '*.ape', '*.mpc', '*.ogg']);  // UNC paths are OK!
              AddInput('\\192.168.0.184\web\public_html\', ['.htaccess']);
  3. Call Search method.
  4. You can get a list of found files using GetFileList(TStrings) OR GetFirstFile + GetNextFile OR Items[Index].Results
     OutputCount - the number of found files


  Metoda postępowania z TJPFileSearcher:
  1. Ustawić FileInfoMode (domyślnie fimOnlyFileNames - tylko nazwy plików).
  2. Dodać pliki do wyszukania za pomocą AddInput.
  3. Wywołać Search.
  4: Pobrać listę znalezionych plików: GetFileList lub GetFirstFile + GetNextFile, lub Items[Index].Results.
     OutputCount zwraca liczbę znalezionych plików.

}

// TODO: Migracja fgl -> generics.collections

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch ADVANCEDRECORDS}
{$ELSE}
Sorry Delphi! This is for FPC only!
{$ENDIF}

interface

uses
  {$IFDEF FPC}LazUTF8,{$ENDIF} // czy to jeszcze jest potrzebne?
  Classes, SysUtils,

  {$IFDEF FPC}fgl,{$ENDIF}
  JPL.Strings, JPL.FileSearch, JPL.Files;


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

  // Input item
  TFSItem = record
    Directory: string;
    RecurseDepth: WORD;
    FilesToSearch: TFSFilesToSerach;
    Results: TFSResults;
    Tag: integer;
    TagStr: string;
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



  TJPFileSearcher = class
  private
    FAcceptDirectorySymLinks: Boolean;
    FAcceptFileSymLinks: Boolean;
    FFileCountLimit: DWORD;
    FFileInfoMode: TFSFileInfoMode;
    FItems: TFSItems;
    FOnFoundDirectory: TJPEnumDirsProcObj;
    FOnFoundFile: TJPEnumFilesProcObj;
    FRecurseDepth: WORD;
    FSearchRec: TFSSearchRec;
    FStats: TFSStats;
    function GetInputCount: integer;
    function GetItems(Index: integer): TFSItem;
    function GetOutputCount: integer;
    procedure SetAcceptDirectorySymLinks(AValue: Boolean);
    procedure SetAcceptFileSymLinks(AValue: Boolean);
    procedure SetFileCountLimit(AValue: DWORD);

    procedure SetFileInfoMode(AValue: TFSFileInfoMode);
    procedure SetOnFoundDirectory(AValue: TJPEnumDirsProcObj);
    procedure SetOnFoundFile(AValue: TJPEnumFilesProcObj);
    procedure SetRecurseDepth(AValue: WORD);
  public

    constructor Create;
    destructor Destroy; override;

    // Starts the search. First, provide the input for the search using AddInput.
    procedure Search;

    // Clears the result list and restores default values
    procedure ClearAll;

    // The input data for the search is given here:
    // Dir - Directory to search.
    // FileMasks - An array with file masks.
    // Returns the index of the newly added item in the FItems dictionary (map).
    function AddInput(const Dir: string; FileMasks: array of string; RecurseDepth: integer = FS_DEAFULT_RECURSE_DEPTH): integer;
    function AddInputWithTag(const Dir: string; FileMasks: array of string; RecurseDepth: integer = FS_DEAFULT_RECURSE_DEPTH; Tag: integer = -1;
      TagStr: string = ''): integer;

    // An attempt to retrieve an FSInputItem element with the given index.
    function TryGetInputItem(const Index: integer; out FSInputItem: TFSItem): Boolean;

    // Saves the names of all found files to FileList.
    procedure GetFileList(FileList: TStrings);

    // GetFirstFile returns the first file found, a GetNextFile - kolejne.
    // Similar to FindFirst and FindNext
    function GetFirstFile(var FileName: string): Boolean;
    function GetNextFile(var FileName: string): Boolean;

    function InputAsStr: string; // for debug purposes // TODO: Remove or comment out InputAsStr

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
    // TFSStats - this record contains several statistical information about found files: number of found files, total size of all files, max file size...
    property Stats: TFSStats read FStats;

    // The search stops when FileCountLimit files are found. 0 = no limit (default)
    property FileCountLimit: DWORD read FFileCountLimit write SetFileCountLimit;

    // When searching, include symbolic links/junctions to directories.
    // Przeszukuj także linki symboliczne do katalogów.
    property AcceptDirectorySymLinks: Boolean read FAcceptDirectorySymLinks write SetAcceptDirectorySymLinks;

    // Przeszukuj także linki symboliczne do plików.
    property AcceptFileSymLinks: Boolean read FAcceptFileSymLinks write SetAcceptFileSymLinks;

    // This callback function is called after finding each directory. To continue searching, this function must return True.
    // To stop seraching - return False.
    property OnFoundDirectory: TJPEnumDirsProcObj read FOnFoundDirectory write SetOnFoundDirectory;

    // This callback function is called after finding each file. To continue searching, this function must return True.
    // To stop seraching - return False.
    property OnFoundFile: TJPEnumFilesProcObj read FOnFoundFile write SetOnFoundFile;

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
  FOnFoundDirectory := nil;
  FOnFoundFile := nil;
  FAcceptDirectorySymLinks := True;
  FAcceptFileSymLinks := True;
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


procedure TJPFileSearcher.SetOnFoundDirectory(AValue: TJPEnumDirsProcObj);
begin
  if FOnFoundDirectory = AValue then Exit;
  FOnFoundDirectory := AValue;
end;

procedure TJPFileSearcher.SetOnFoundFile(AValue: TJPEnumFilesProcObj);
begin
  if FOnFoundFile = AValue then Exit;
  FOnFoundFile := AValue;
end;


function EnumDirsProc(CurrentDirNo: integer; CurrentDir: string): Boolean;
begin
  Result := True;
end;

function EnumFilesProc(Dirs, CurrentFileNo, CurrentDirNo: integer; CurrentFile: string): Boolean;
begin
  Result := True;
end;

{$region '                          Search                            '}
procedure TJPFileSearcher.Search;
var
  i, x, y, xFiles: integer;
  ii: TFSItem;
  sl: TStringList;
  Dir, fMask: string;
  oi: TFSResultItem;
  fir: TFileInfoRec;

  function CanContinue: Boolean;
  begin
    Result := not ( (FFileCountLimit > 0) and (xFiles >= FFileCountLimit) );
  end;

begin
  sl := TStringList.Create;
  try

    xFiles := 0;

    for i := 0 to FItems.Count - 1 do
    begin

      ii := FItems.Data[i];
      if ii.FilesToSearch.Count = 0 then Continue;
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

        JPGetFileListObj(fMask, Dir, sl, ii.RecurseDepth, FAcceptDirectorySymLinks, FAcceptFileSymLinks, FOnFoundDirectory, FOnFoundFile, nil);
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
            //GetFileInfoRec(sl[y], fir);
            fir.ReadFileInfo(sl[y]);
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
begin
  Result := AddInputWithTag(Dir, FileMasks, RecurseDepth, -1, '');
end;

function TJPFileSearcher.AddInputWithTag(const Dir: string; FileMasks: array of string; RecurseDepth: integer; Tag: integer = -1; TagStr: string = ''): integer;
var
  ii: TFSItem;
  i: integer;
begin
  ii.Directory := Dir;
  ii.RecurseDepth := RecurseDepth;
  ii.Tag := Tag;
  ii.TagStr := '';

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

procedure TJPFileSearcher.GetFileList(FileList: TStrings);
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

procedure TJPFileSearcher.SetAcceptDirectorySymLinks(AValue: Boolean);
begin
  if FAcceptDirectorySymLinks = AValue then Exit;
  FAcceptDirectorySymLinks := AValue;
end;

procedure TJPFileSearcher.SetAcceptFileSymLinks(AValue: Boolean);
begin
  if FAcceptFileSymLinks = AValue then Exit;
  FAcceptFileSymLinks := AValue;
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

