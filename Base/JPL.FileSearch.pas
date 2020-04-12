unit JPL.FileSearch;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 2019.04.26

  Non-recursive search of files and directories.
  The solution is not much slower than using recursion, but less memory-consuming.


  Nierekursywne przetwarzanie plików i katalogów.
  Rozwi¹zanie niewiele wolniejsze ni¿ z zastosowaniem rekurencji, za to mniej pamiêcio¿erne.

  Modu³ z 2002 roku. W kolejnych latach rozbudowywany.

  ------------------------------------------------------

  2018.01.24
  [+] JPFileSearchCaseSensitive
  [+] Sprawdzanie masek MatchesMask [FPC]
  ----
  2019.04.26
  [+] JPGetFileListObj - copy of JPGetFileList with "of object" callback procs (for TJPFileSearcher).

  ------------------------------------------------------

  Uwaga!
  FindFiles
  FPC 3.1.1 ma problemy z maskami w FindFirst/FindNext. Czasami akceptuje pliki zupe³nie nie pasuj¹ce do maski!
  Na Delphi XE7 wszystko dzia³a prawid³owo.
  W FPC wprowadzi³em dodatkowe sprawdzenie przez MatchesMask.



}



{$I .\..\jp.inc}
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  {$IFDEF FPC} LazUTF8, Masks, {$ENDIF}
  SysUtils, Classes
  ;


const

  DEFAULT_RECURSE_DEPTH = 150;

  // UNC_PREFIX - large file names support (above MAX_PATH 260-chars limit)
  // https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396#maxpath
  {$IFDEF MSWINDOWS} UNC_PREFIX = '\\?\'; {$ELSE} UNC_PREFIX = ''; {$ENDIF}
  UNC_PREFIX_LENGTH = Length(UNC_PREFIX);


{$IFDEF FPC}
var
  // This variable determines whether the character case will be taken into account when searching for files.
  // In initialization section of the unit, it is set to False (for Windows), and True for other platforms.
  JPFileSearchCaseSensitive: Boolean; // { TODO: Dodaæ CaseSensitive do funkcji JPGetFileList, JPGetDirectoryList, JPGetFileList2 }
{$ENDIF}


type

  TJPSimpleEnumProc = function(CurrentFileNo: integer): Boolean;
  TJPEnumDirsProc = function(CurrentDirNo: integer; CurrentDir: string): Boolean;
  TJPEnumFilesProc = function(Dirs, CurrentFileNo, CurrentDirNo: integer; CurrentFile: string): Boolean;

  TJPSimpleEnumProcObj = function(CurrentFileNo: integer): Boolean of object;
  TJPEnumDirsProcObj = function(CurrentDirNo: integer; CurrentDir: string): Boolean of object;
  TJPEnumFilesProcObj = function(Dirs, CurrentFileNo, CurrentDirNo: integer; CurrentFile: string): Boolean of object;

  TJPEnumFilesProc2 = function(CurrDirNo, CurrFileNo: integer; CurrDir, CurrFile: string): Boolean;
  TJPAcceptFileProc = function(FileName: string): Boolean;


procedure AddMaskedFileToList(FileMask: string; FileList: TStringList);

//procedure StrToList(LineToParse: string; var List: THashedStringList; Separator: string = ','); overload;
procedure FillFileList(LineToParse: string; const FileSeparator: string; FileList: TStringList);

procedure FindFiles(FileMask, StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True);
procedure FindDirs(StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True; SimpleEnumProc: TJPSimpleEnumProc = nil);
function IsFile(SR: TSearchRec; AcceptSymLinks: Boolean = True): Boolean;
function IsDirectory(SR: TSearchRec; AcceptSymLinks: Boolean = True): Boolean;



//////////////////////////////////////////// MAIN  PROCEDURES //////////////////////////////////////////////

{
  JPGetDirectoryList
  Searches for subdirectories in the "StartDir" directory up to the "RecurseDepth" level in the directory structure.

  StartDir - Must be a directory name (not a mask).
  DirList - A list in which all found directories will be saved.
  AcceptSymLinks - Decides whether to include symbolic links to directories during the search.
}
procedure JPGetDirectoryList(
  StartDir: string; var DirList: TStringList; AcceptSymLinks: Boolean = True; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  EnumDirsProc: TJPEnumDirsProc = nil
);

{
  JPGetFileList
  Searches for files in the "StartDir" directory and it's subdirectories up to the "RecurseDepth" level in the directory structure.

  FileMask - Short file name (without path) or file mask.
  StartDir - Must be a directory name (not a mask).
  FileList - A list in which all found files will be saved.
  AcceptDirSymLinks - Decides whether to include symbolic links to directories during the search.
  AcceptFileSymLinks - Decides whether to include symbolic links to files during the search.
}
{ TODO: Test SymLinks on Linux }
procedure JPGetFileList(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumDirsProc: TJPEnumDirsProc = nil;
  EnumFilesProc: TJPEnumFilesProc = nil;
  SimpleEnumDirsProc: TJPSimpleEnumProc = nil
);

{ "of object" variant }
procedure JPGetFileListObj(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumDirsProc: TJPEnumDirsProcObj = nil;
  EnumFilesProc: TJPEnumFilesProcObj = nil;
  SimpleEnumDirsProc: TJPSimpleEnumProcObj = nil
);

procedure JPGetFileList2(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumFilesProc2: TJPEnumFilesProc2 = nil;
  AcceptFileProc: TJPAcceptFileProc = nil
);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////


function RemoveUncPrefix(const FileName: string): string;
function AddUncPrefix(const FileName: string): string;




implementation

{$region ' ------------------------------ mniej nieistotne ---------------------------- '}

procedure AddMaskedFileToList(FileMask: string; FileList: TStringList);
var
  SR: TSearchRec;
  Dir: string;
begin
  Dir := ExtractFilePath(FileMask);

  {$IFDEF FPC}
  FileMask := ExtractFileName(FileMask);

  if FindFirst(Dir + '*', faAnyFile, SR) = 0 then
  try

    if MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then
      if IsFile(SR) then FileList.Add(Dir + SR.Name);

    while FindNext(SR) = 0 do
    begin
      if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Continue;
      if IsFile(SR) then FileList.Add(Dir + SR.Name);
    end;

  finally
    FindClose(SR);
  end;

  {$ELSE}
  if FindFirst(FileMask, faAnyFile, SR) = 0 then
  try
    if IsFile(SR) then FileList.Add(Dir + SR.Name);
    while FindNext(SR) = 0 do
      if IsFile(SR) then FileList.Add(Dir + SR.Name);
  finally
    FindClose(SR);
  end;
  {$ENDIF}
end;

//procedure AddMaskedFileToList(FileMask: string; FileList: TStringList);
//var
//  SR: TSearchRec;
//  Dir: string;
//begin
//  Dir := ExtractFilePath(FileMask);
//  if FindFirst(FileMask, faAnyFile, SR) = 0 then
//  try
//    {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Exit;{$ENDIF}
//    if IsFile(SR) then FileList.Add(Dir + SR.Name);
//    while FindNext(SR) = 0 do
//    begin
//      {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Continue;{$ENDIF}
//      if IsFile(SR) then FileList.Add(Dir + SR.Name);
//    end;
//  finally
//    FindClose(SR);
//  end;
//end;

procedure FillFileList(LineToParse: string; const FileSeparator: string; FileList: TStringList);
var
  x: integer;
  s: string;
begin
  if LineToParse = '' then Exit;

  if Pos(FileSeparator, LineToParse) > 0 then
  begin

    repeat
      x := Pos(FileSeparator, LineToParse);
      s := Copy(LineToParse, 1, x - 1);

      if (Pos('*', s) > 0) or (Pos('?', s) > 0) then AddMaskedFileToList(s, FileList)
      else FileList.Add(s);

      LineToParse := Copy(LineToParse, x + Length(FileSeparator), Length(LineToParse));
    until Pos(FileSeparator, LineToParse) = 0;

    if (Pos('*', LineToParse) > 0) or (Pos('?', LineToParse) > 0) then AddMaskedFileToList(LineToParse, FileList)
    else FileList.Add(LineToParse);

  end

  else if (Pos('*', LineToParse) > 0) or (Pos('?', LineToParse) > 0) then AddMaskedFileToList(LineToParse, FileList)

  else FileList.Add(LineToParse);

end;


//procedure StrToList(LineToParse: string; var List: THashedStringList; Separator: string = ',');
//var
//  xp: integer;
//  s: string;
//begin
//  //if not Assigned(List) then Exit;
//
//  xp := Pos(Separator, LineToParse);
//  while xp > 0 do
//  begin
//    s := Trim(Copy(LineToParse, 1, xp - 1));
//    List.Add(s);
//    Delete(LineToParse, 1, xp + Length(Separator) - 1);
//    LineToParse := Trim(LineToParse);
//    xp := Pos(Separator, LineToParse);
//  end;
//
//  if LineToParse <> '' then
//  begin
//    LineToParse := Trim(LineToParse);
//    if LineToParse <> '' then List.Add(LineToParse);
//  end;
//
//end;
{$endregion}



{$region ' --------------------------------------------- helpers --------------------------------------------------- '}
function AddUncPrefix(const FileName: string): string;
begin
  if Copy(FileName, 1, 2) <> '\\' then Result := UNC_PREFIX + FileName
  else Result := FileName;
end;

function RemoveUncPrefix(const FileName: string): string;
begin
  Result := StringReplace(FileName, UNC_PREFIX, '', []);
end;
{$warnings off} {$hints off}
function IsFile(SR: TSearchRec; AcceptSymLinks: Boolean = True): Boolean;
begin
  Result := ( (SR.Attr and faDirectory) = 0 ) and (SR.Name <> '.') and (SR.Name <> '..');
  if Result and (not AcceptSymLinks) then Result := ( Result and ( (SR.Attr and faSymLink) = 0) );
end;

function IsDirectory(SR: TSearchRec; AcceptSymLinks: Boolean = True): Boolean;
begin
  Result := ( (SR.Attr and faDirectory) <> 0 ) and (SR.Name <> '.') and (SR.Name <> '..');
  if Result and (not AcceptSymLinks) then Result := ( Result and ( (SR.Attr and faSymLink) = 0) );
end;
{$warnings on} {$hints on}


procedure FindFiles(FileMask, StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True);
var
  SR: TSearchRec;
begin
  {$IFDEF FPC}

  // FPC 3.1.1: FindFirst czasami "przepuszcza" pliki nie pasuj¹ce do maski, dlatego
  // w FindFirst ustawiam maskê na '*', a filtrowanie wykonujê przy pomocy MatchesMask.
  if FindFirst(StartDir + PathDelim + '*', faAnyFile, SR) = 0 then
  try

    if MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then
      if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);

    while FindNext(SR) = 0 do
    begin
      if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Continue;
      if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);
    end;

  finally
    FindClose(SR);
  end;

  {$ELSE}

  if FindFirst(StartDir + PathDelim + FileMask, faAnyFile, SR) = 0 then
  try
    if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);
    while FindNext(SR) = 0 do
      if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);
  finally
    FindClose(SR);
  end;

  {$ENDIF}
end;

//procedure FindFiles(FileMask, StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True);
//var
//  SR: TSearchRec;
//begin
//  if FindFirst(StartDir + PathDelim + FileMask, faAnyFile, SR) = 0 then
//  try
//    {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Exit;{$ENDIF}
//    if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);
//    while FindNext(SR) = 0 do
//    begin
//      {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Continue;{$ENDIF}
//      if IsFile(SR, AcceptSymLinks) then List.Add(StartDir + PathDelim + SR.Name);
//    end;
//  finally
//    FindClose(SR);
//  end;
//end;

procedure FindDirs(StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True; SimpleEnumProc: TJPSimpleEnumProc = nil);
var
  SR: TSearchRec;
  xFileNo: integer;
begin
  xFileNo := 0;
  if FindFirst(StartDir, faAnyFile, SR) = 0 then
  try
    Inc(xFileNo);

    if Assigned(SimpleEnumProc) then if not SimpleEnumProc(xFileNo) then Exit;

    if IsDirectory(SR, AcceptSymLinks) then List.Add(Copy(StartDir, 1, Length(StartDir) - 2) + PathDelim + SR.Name);

    while FindNext(SR) = 0 do
    begin
      if IsDirectory(SR, AcceptSymLinks) then List.Add(Copy(StartDir, 1, Length(StartDir) - 2) + PathDelim + SR.Name);
      Inc(xFileNo);
      if Assigned(SimpleEnumProc) then if not SimpleEnumProc(xFileNo) then Exit;
    end;

  finally
    FindClose(SR);
  end;

end;
{$endregion}


{$region ' ---------------------------------------------- JPGetDirectoryList ------------------------------------------------- '}
procedure JPGetDirectoryList(
  StartDir: string; var DirList: TStringList; AcceptSymLinks: Boolean = True; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH; EnumDirsProc: TJPEnumDirsProc = nil);
var
  SubdirsList, TempList: TStringList;
  Last: TStringList;
  i, x, k: integer;


//--------------------S T A R T--------------------------------
begin

  StartDir := AddUncPrefix(StartDir);
  SubdirsList := TStringList.Create;
  TempList := TStringList.Create;
  Last := TStringList.Create;

  try
    if StartDir[Length(StartDir)] = PathDelim then StartDir := Copy(StartDir, 1, length(StartDir) - 1);


    StartDir := StartDir + PathDelim + '*';
    SubdirsList.Clear;
    FindDirs(StartDir, SubdirsList, AcceptSymLinks);
    //SubdirsList.Sort;

    for i := 0 to SubdirsList.Count - 1 do
    begin
      DirList.Add(RemoveUncPrefix(SubdirsList[i]));
      Last.Add(SubdirsList[i]);
      if Assigned(EnumDirsProc) then if not EnumDirsProc(DirList.Count, DirList[DirList.Count - 1]) then Exit; // DirList.LastItem) then Exit;
    end;


    for i := 1 to RecurseDepth - 1 do
    begin

      for x := 0 to Last.Count - 1 do
      begin
        StartDir := Last[x] + PathDelim + '*';

        SubdirsList.Clear;
        FindDirs(StartDir, SubdirsList, AcceptSymLinks);
        //SubdirsList.Sort;

        for k := 0 to SubdirsList.Count - 1 do
        begin
          DirList.Add(RemoveUncPrefix(SubdirsList[k]));
          TempList.Add(SubdirsList[k]);

          if Assigned(EnumDirsProc) then if not EnumDirsProc(DirList.Count, DirList[DirList.Count - 1]) then Exit; // DirList.LastItem) then Exit;

        end;

      end; //for x

      Last.Clear;
      for k := 0 to TempList.Count - 1 do Last.Add(TempList[k]);
      TempList.Clear;
    end;


  finally
    DirList.Sort;

    SubdirsList.Clear;
    TempList.Clear;
    Last.Clear;

    SubdirsList.Free;
    TempList.Free;
  end;
end;
{$endregion}



{$region ' ---------------------------------------------- JPGetFileList ------------------------------------------------- '}
{
  Procedura JPGetFileList najpierw tworzy listê katalogów w MainDirectoryList, a póŸniej listê plików dla ka¿dego znalezionego katalogu.
}
procedure JPGetFileList(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumDirsProc: TJPEnumDirsProc = nil; EnumFilesProc: TJPEnumFilesProc = nil; SimpleEnumDirsProc: TJPSimpleEnumProc = nil);
var
  SubdirsList, TempFileList, TempList: TStringList;
  MainDirectoryList: TStringList;
  Last: TStringList;
  i, x, k: integer;
begin

  SubdirsList := TStringList.Create;
  TempFileList := TStringList.Create;
  TempList := TStringList.Create;
  MainDirectoryList := TStringList.Create;
  Last := TStringList.Create;
  try

    StartDir := AddUncPrefix(StartDir);

    // ---------------------------------- creating directory list -----------------------------------------
    if StartDir[Length(StartDir)] = PathDelim then StartDir := Copy(StartDir, 1, Length(StartDir) - 1);
    MainDirectoryList.Add(StartDir);

    if RecurseDepth > 0 then
    begin
      StartDir := StartDir + PathDelim + '*';
      SubdirsList.Clear;
      if Assigned(SimpleEnumDirsProc) then FindDirs(StartDir, SubdirsList, AcceptDirSymLinks, SimpleEnumDirsProc)
      else FindDirs(StartDir, SubdirsList, AcceptDirSymLinks);
    end;


    for i := 0 to SubdirsList.Count - 1 do
    begin
      MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[i]));
      if Assigned(EnumDirsProc) then if not EnumDirsProc(MainDirectoryList.Count, MainDirectoryList[MainDirectoryList.Count - 1]) then Exit; // MainDirectoryList.LastItem) then Exit;
      Last.Add(SubdirsList[i]);
    end;

    for i := 1 to RecurseDepth - 1 do
    begin

      for x := 0 to Last.Count - 1 do
      begin

        StartDir := Last[x] + PathDelim + '*';
        SubdirsList.Clear;

        //FindDirs(StartDir, SubdirsList, AcceptDirSymLinks);
        if Assigned(SimpleEnumDirsProc) then FindDirs(StartDir, SubdirsList, AcceptDirSymLinks, SimpleEnumDirsProc)
        else FindDirs(StartDir, SubdirsList, AcceptDirSymLinks);

        for k := 0 to SubdirsList.Count - 1 do
        begin
          MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[k]));
          if Assigned(EnumDirsProc) then if not EnumDirsProc(MainDirectoryList.Count, MainDirectoryList[MainDirectoryList.Count - 1]) then Exit; // MainDirectoryList.LastItem) then Exit;
          TempList.Add(SubdirsList[k]);
        end;

      end; //for x

      Last.Clear;
      for k := 0 to TempList.Count - 1 do Last.Add(TempList[k]);
      TempList.Clear;

    end; // for i
    // ------------------------------------- directory list created ---------------------------------------

    MainDirectoryList.Sort;

    // ------------------------------------------ searching files ---------------------------------------------
    if MainDirectoryList.Count > 0 then
    begin
      TempFileList.Clear;

      for i := 0 to MainDirectoryList.Count - 1 do
      begin
        TempFileList.Clear;
        FindFiles(FileMask, MainDirectoryList[i], TempFileList, AcceptFileSymLinks);
        for k := 0 to TempFileList.Count - 1 do
        begin
          FileList.Add(RemoveUncPrefix(TempFileList[k]));
          if Assigned(EnumFilesProc) then if not EnumFilesProc(MainDirectoryList.Count, FileList.Count, i + 1, FileList[FileList.Count - 1]) then Exit; // FileList.LastItem) then Exit;
        end;
      end;

    end;
    // ------------------------------------------------------------------------------------------------------------


  finally
    //FileList.Sort;
    MainDirectoryList.Free;
    SubdirsList.Free;
    TempList.Free;
    TempFileList.Free;
    Last.Free;
  end;
end;
{$endregion JPGetFileList}


{$region ' ---------------------------------------------- JPGetFileListObj ------------------------------------------------- '}
procedure FindDirsObj(StartDir: string; var List: TStringList; AcceptSymLinks: Boolean = True; SimpleEnumProc: TJPSimpleEnumProcObj = nil);
var
  SR: TSearchRec;
  xFileNo: integer;
begin
  xFileNo := 0;
  if FindFirst(StartDir, faAnyFile, SR) = 0 then
  try
    Inc(xFileNo);

    if Assigned(SimpleEnumProc) then if not SimpleEnumProc(xFileNo) then Exit;

    if IsDirectory(SR, AcceptSymLinks) then List.Add(Copy(StartDir, 1, Length(StartDir) - 2) + PathDelim + SR.Name);

    while FindNext(SR) = 0 do
    begin
      if IsDirectory(SR, AcceptSymLinks) then List.Add(Copy(StartDir, 1, Length(StartDir) - 2) + PathDelim + SR.Name);
      Inc(xFileNo);
      if Assigned(SimpleEnumProc) then if not SimpleEnumProc(xFileNo) then Exit;
    end;

  finally
    FindClose(SR);
  end;

end;

procedure JPGetFileListObj(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumDirsProc: TJPEnumDirsProcObj = nil; EnumFilesProc: TJPEnumFilesProcObj = nil; SimpleEnumDirsProc: TJPSimpleEnumProcObj = nil);
var
  SubdirsList, TempFileList, TempList: TStringList;
  MainDirectoryList: TStringList;
  Last: TStringList;
  i, x, k: integer;
begin

  SubdirsList := TStringList.Create;
  TempFileList := TStringList.Create;
  TempList := TStringList.Create;
  MainDirectoryList := TStringList.Create;
  Last := TStringList.Create;
  try

    StartDir := AddUncPrefix(StartDir);

    // ---------------------------------- creating directory list -----------------------------------------
    if StartDir[Length(StartDir)] = PathDelim then StartDir := Copy(StartDir, 1, Length(StartDir) - 1);
    MainDirectoryList.Add(StartDir);

    if RecurseDepth > 0 then
    begin
      StartDir := StartDir + PathDelim + '*';
      SubdirsList.Clear;
      if Assigned(SimpleEnumDirsProc) then FindDirsObj(StartDir, SubdirsList, AcceptDirSymLinks, SimpleEnumDirsProc)
      else FindDirsObj(StartDir, SubdirsList, AcceptDirSymLinks);
    end;


    for i := 0 to SubdirsList.Count - 1 do
    begin
      MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[i]));
      if Assigned(EnumDirsProc) then if not EnumDirsProc(MainDirectoryList.Count, MainDirectoryList[MainDirectoryList.Count - 1]) then Exit; // MainDirectoryList.LastItem) then Exit;
      Last.Add(SubdirsList[i]);
    end;

    for i := 1 to RecurseDepth - 1 do
    begin

      for x := 0 to Last.Count - 1 do
      begin

        StartDir := Last[x] + PathDelim + '*';
        SubdirsList.Clear;

        if Assigned(SimpleEnumDirsProc) then FindDirsObj(StartDir, SubdirsList, AcceptDirSymLinks, SimpleEnumDirsProc)
        else FindDirsObj(StartDir, SubdirsList, AcceptDirSymLinks);

        for k := 0 to SubdirsList.Count - 1 do
        begin
          MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[k]));
          if Assigned(EnumDirsProc) then if not EnumDirsProc(MainDirectoryList.Count, MainDirectoryList[MainDirectoryList.Count - 1]) then Exit; // MainDirectoryList.LastItem) then Exit;
          TempList.Add(SubdirsList[k]);
        end;

      end; //for x

      Last.Clear;
      for k := 0 to TempList.Count - 1 do Last.Add(TempList[k]);
      TempList.Clear;

    end; // for i
    // ------------------------------------- directory list created ---------------------------------------

    MainDirectoryList.Sort;

    // ------------------------------------------ searching files ---------------------------------------------
    if MainDirectoryList.Count > 0 then
    begin
      TempFileList.Clear;

      for i := 0 to MainDirectoryList.Count - 1 do
      begin
        TempFileList.Clear;
        FindFiles(FileMask, MainDirectoryList[i], TempFileList, AcceptFileSymLinks);
        for k := 0 to TempFileList.Count - 1 do
        begin
          FileList.Add(RemoveUncPrefix(TempFileList[k]));
          if Assigned(EnumFilesProc) then if not EnumFilesProc(MainDirectoryList.Count, FileList.Count, i + 1, FileList[FileList.Count - 1]) then Exit; // FileList.LastItem) then Exit;
        end;
      end;

    end;
    // ------------------------------------------------------------------------------------------------------------


  finally
    //FileList.Sort;
    MainDirectoryList.Free;
    SubdirsList.Free;
    TempList.Free;
    TempFileList.Free;
    Last.Free;
  end;
end;
{$endregion JPGetFileListObj}


{$region ' ---------------------------------------------- JPGetFileList2 ------------------------------------------------- '}
procedure JPGetFileList2(
  FileMask, StartDir: string; var FileList: TStringList; RecurseDepth: integer = DEFAULT_RECURSE_DEPTH;
  AcceptDirSymLinks: Boolean = True; AcceptFileSymLinks: Boolean = True;
  EnumFilesProc2: TJPEnumFilesProc2 = nil; AcceptFileProc: TJPAcceptFileProc = nil
);
var
  SubdirsList, TempList: TStringList;
  MainDirectoryList: TStringList;
  Last: TStringList;
  i, x, k: integer;
  CurrDirNo, CurrFileNo: integer;
  Aborted: Boolean;


  procedure _GetFiles(const Dir: string);
  var
    SR: TSearchRec;
    DirWithoutUNC: string;
  begin
    Inc(CurrDirNo);

    if FindFirst(Dir + PathDelim + FileMask, faAnyFile, SR) = 0 then
    try
      {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Exit;{$ENDIF}
      DirWithoutUNC := RemoveUncPrefix(Dir);

      if IsFile(SR, AcceptFileSymLinks) then
      begin

        if Assigned(AcceptFileProc) then
        begin
          if AcceptFileProc(DirWithoutUNC + PathDelim + SR.Name) then
          begin
            FileList.Add(DirWithoutUNC + PathDelim + SR.Name);
            Inc(CurrFileNo);
          end;
        end
        else
        begin
          FileList.Add(DirWithoutUNC + PathDelim + SR.Name);
          Inc(CurrFileNo);
        end;

      end;


      while FindNext(SR) = 0 do
      begin

        {$IFDEF FPC}if not MatchesMask(SR.Name, FileMask, JPFileSearchCaseSensitive) then Continue;{$ENDIF}

        if IsFile(SR, AcceptFileSymLinks) then
        begin
         if Assigned(AcceptFileProc) then
         begin
           if AcceptFileProc(DirWithoutUNC + PathDelim + SR.Name) then
           begin
             FileList.Add(DirWithoutUNC + PathDelim + SR.Name);
             Inc(CurrFileNo);
           end;
         end
         else
         begin
           FileList.Add(DirWithoutUNC + PathDelim + SR.Name);
           Inc(CurrFileNo);
         end;

         if Assigned(EnumFilesProc2) then
           if not EnumFilesProc2(CurrDirNo, CurrFileNo, DirWithoutUNC, DirWithoutUNC + PathDelim + SR.Name) then
           begin
             Aborted := True;
             Break;
           end;
        end;

      end;

    finally
      FindClose(SR);
    end;
  end;

begin

  Aborted := False;
  CurrDirNo := 0;
  CurrFileNo := 0;
  SubdirsList := TStringList.Create;
  TempList := TStringList.Create;
  MainDirectoryList := TStringList.Create;
  Last := TStringList.Create;
  try

    StartDir := AddUncPrefix(StartDir);

    // ---------------------------------- creating directory list and searching files -----------------------------------------

    if StartDir[Length(StartDir)] = PathDelim then StartDir := Copy(StartDir, 1, Length(StartDir) - 1);
    MainDirectoryList.Add(StartDir);
    _GetFiles(StartDir);

    if (RecurseDepth > 0) and (not Aborted) then
    begin
      StartDir := StartDir + PathDelim + '*';
      SubdirsList.Clear;
      FindDirs(StartDir, SubdirsList, AcceptDirSymLinks);
    end;


    if not Aborted then
      for i := 0 to SubdirsList.Count - 1 do
      begin
        _GetFiles(SubdirsList[i]);
        MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[i]));
        Last.Add(SubdirsList[i]);
      end;


    for i := 1 to RecurseDepth - 1 do
    begin

      if Aborted then Break;

      for x := 0 to Last.Count - 1 do
      begin

        StartDir := Last[x] + PathDelim + '*';
        SubdirsList.Clear;

        FindDirs(StartDir, SubdirsList, AcceptDirSymLinks);

        for k := 0 to SubdirsList.Count - 1 do
        begin
          _GetFiles(SubdirsList[k]);
          MainDirectoryList.Add(RemoveUncPrefix(SubdirsList[k]));
          TempList.Add(SubdirsList[k]);
        end;

      end; //for x


      Last.Clear;
      for k := 0 to TempList.Count - 1 do Last.Add(TempList[k]);
      TempList.Clear;


    end; // for i


    //for i := 0 to FileList.Count - 1 do FileList[i] := RemoveUncPrefix(FileList[i]); // Prefix UNC usuwany w funkcji _GetFiles

    if FileList.Count < 10000 then Filelist.Sort; // przy du¿ej liczbie plików sortowanie trwa bardzo d³ugo

    // ------------------------------------- file list created ---------------------------------------

  finally
    MainDirectoryList.Free;
    SubdirsList.Free;
    TempList.Free;
    Last.Free;
  end;
end;
{$endregion}




initialization

{$IFDEF FPC}

  {$IFDEF MSWINDOWS}JPFileSearchCaseSensitive := False;{$ELSE}JPFileSearchCaseSensitive := True{$ENDIF}

{$ENDIF}


end.

