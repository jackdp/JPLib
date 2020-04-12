unit JPL.Binary.Misc;

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$ENDIF}


interface

uses
  Windows, SysUtils;

//type
//  TByteArray = array of Byte;

procedure SwitchBool(var BoolVar: Boolean); overload;
procedure SwitchBool(PBoolVar: PBoolean); overload;
function GetStringOffset(const s: AnsiString; const Buffer: array of Byte): integer;
function ByteArrayToString(const Buffer: array of Byte; Separator: string = ' '): string; overload;
function ByteArrayToString(PBuffer: PByte; BufferSize: integer; Separator: string = ' '): string; overload;
function BufferFind(const Buffer: array of Byte; const s: AnsiString): integer;
function BufferFind2(const Buffer: array of Byte; const s: AnsiString): integer;
function BufferFindEx(const Buffer: array of Byte; const s: AnsiString; StartPos: integer = 0): Int64; overload;
function BufferFindEx(PBuffer: PByte; BufSize: integer; const s: AnsiString; StartPos: integer = 0): Int64; overload;
function BufferFindLast(PBuffer: PByte; BufSize: integer; const s: AnsiString; StartPos: integer = 0): Int64;

//procedure SaveBufferToFile(PBuffer: PByte; BufSize: integer; const FileName: string);

//function ScanFile(const FileName: string; const forString: AnsiString; caseSensitive: Boolean): Longint;


implementation


//{$warnings off}
//{
//  To jest chyba z DelphiPraxis, ale ju¿ dobrze nie pamiêtam.
//
//  ScanFile searches for a string in a file and returns the position of the string
//  in the file or -1, if not found.
//
//  ScanFile sucht in einer Datei nach dem Vorkommen
//  eines bestimmten Strings und gibt bei Erfolg die Position zurück, wo der String
//  gefunden wurde.
//}
//
//function ScanFile(const FileName: string; const forString: AnsiString; caseSensitive: Boolean): Longint;
//const
//  BufferSize = $8001; { 32K+1 bytes }
//var
//  pBuf, pEnd, pScan, pPos: PAnsiChar;
//  filesize: Longint;
//  bytesRemaining: Longint;
//  bytesToRead: integer;
//  F: file;
//  SearchFor: PAnsiChar;
//  oldMode: Word;
//begin
//  { assume failure }
//  Result := -1;
//  if (Length(forString) = 0) or (Length(FileName) = 0) then Exit;
//  SearchFor := nil;
//  pBuf := nil;
//  { open file as binary, 1 byte recordsize }
//  AssignFile(F, FileName);
//  oldMode := FileMode;
//  FileMode := 0; { read-only access }
//  Reset(F, 1);
//  FileMode := oldMode;
//
//  try { allocate memory for buffer and pchar search string }
//
//    SearchFor := AnsiStrAlloc(Length(forString) + 1);
//    StrPCopy(SearchFor, forString);
//    if not caseSensitive then { convert to upper case }
//        AnsiUpper(SearchFor);
//    GetMem(pBuf, BufferSize);
//    filesize := System.filesize(F);
//    bytesRemaining := filesize;
//    pPos := nil;
//    while bytesRemaining > 0 do
//    begin
//      { calc how many bytes to read this round }
//      if bytesRemaining >= BufferSize then bytesToRead := Pred(BufferSize)
//      else bytesToRead := bytesRemaining;
//      { read a buffer full and zero-terminate the buffer }
//      BlockRead(F, pBuf^, bytesToRead, bytesToRead);
//      pEnd := @pBuf[bytesToRead];
//      pEnd^ := #0;
//      pScan := pBuf;
//      while pScan < pEnd do
//      begin
//        if not caseSensitive then { convert to upper case }
//            AnsiUpper(pScan);
//        pPos := StrPos(pScan, SearchFor); { search for substring }
//        if pPos <> nil then
//        begin { Found it! }
//          Result := filesize - bytesRemaining + Longint(pPos) - Longint(pBuf);
//          Break;
//        end;
//        pScan := StrEnd(pScan);
//        Inc(pScan);
//      end;
//      if pPos <> nil then Break;
//      bytesRemaining := bytesRemaining - bytesToRead;
//      if bytesRemaining > 0 then
//      begin
//        Seek(F, FilePos(F) - Length(forString));
//        bytesRemaining := bytesRemaining + Length(forString);
//      end;
//    end; { While }
//
//  finally
//    CloseFile(F);
//    if SearchFor <> nil then StrDispose(SearchFor);
//    if pBuf <> nil then FreeMem(pBuf, BufferSize);
//  end;
//end; { ScanFile }
//{$warnings on}

function FindByteArrayInArray(pSearchFor: PByte; SearchLen: Cardinal; pBuffer: PByte; BufLen: Cardinal): PByte;
var
  i, j: Cardinal;
  bFound: Boolean;
begin
  Result := nil;

  if SearchLen > BufLen then Exit;

  if SearchLen = 0 then
  begin
    Result := pBuffer;
    Exit;
  end;

  for i := 0 to BufLen - SearchLen do //skip length of search string
  begin
    if PByte(Cardinal(pBuffer) + i)^ = pSearchFor^ then //find first byte
    begin
      bFound := True; //assume we ok
      for j := 1 to SearchLen - 1 do
      begin
        if PByte(Cardinal(pBuffer) + i + j)^ <> PByte(Cardinal(pSearchFor) + j)^ then
        begin
          bFound := False;
          Break;
        end;
      end;

      if bFound then //after successful loop
      begin
        Result := PByte(Cardinal(pBuffer) + i);
        Exit;
      end;
    end;
  end;
end;


function BufferFindEx(const Buffer: array of Byte; const s: AnsiString; StartPos: integer): Int64; overload;
var
  i, StrLen: integer;
begin
  Result := -1;

  StrLen := Length(s);
  if StrLen = 0 then Exit;
  if StartPos > Length(Buffer) - 1 then Exit;

  for i := StartPos to High(Buffer) - (StrLen - 1) do
    if CompareMem(@Buffer[i], Pointer(s), StrLen) then
    begin
      Result := i;
      Exit;
    end;

end;

function BufferFindEx(PBuffer: PByte; BufSize: integer; const s: AnsiString; StartPos: integer = 0): Int64; overload;
var
  i, StrLen: integer;
begin
  Result := -1;

  StrLen := Length(s);
  if StrLen = 0 then Exit;
  if StartPos > BufSize - 1 then Exit;

  for i := StartPos to BufSize - (StrLen - 1) do
    if CompareMem(@PBuffer[i], Pointer(s), StrLen) then
    begin
      Result := i;
      Exit;
    end;

end;

function BufferFindLast(PBuffer: PByte; BufSize: integer; const s: AnsiString; StartPos: integer = 0): Int64;
var
  i, StrLen: integer;
begin
  Result := -1;

  StrLen := Length(s);
  if StrLen = 0 then Exit;
  if StartPos > BufSize - 1 then Exit;

  for i := StartPos to BufSize - (StrLen - 1) do
    if CompareMem(@PBuffer[i], Pointer(s), StrLen) then
    begin
      Result := i;
      //Exit;
    end;
end;

function BufferFind(const Buffer: array of Byte; const s: AnsiString): integer;
//returns the 0-based index of the start of the first occurrence of S
//or -1 if there is no occurrence
var
  AnsiStr: AnsiString;
begin
  SetString(AnsiStr, PAnsiChar(@Buffer[0]), Length(Buffer));
  Result := Pos(s, AnsiStr) - 1;
end;

function BufferFind2(const Buffer: array of Byte; const s: AnsiString): integer;
//returns the 0-based index of the start of the first occurrence of S
//or -1 if there is no occurrence
var
  StrLen: integer;
begin
  StrLen := Length(s);
  if StrLen > 0 then
    for Result := Low(Buffer) to High(Buffer) - (StrLen - 1) do
      if CompareMem(@Buffer[Result], Pointer(s), StrLen) then Exit;
  Result := -1;
end;

function GetStringOffset(const s: AnsiString; const Buffer: array of Byte): integer;
begin
  Result := BufferFind2(Buffer, s);
end;

function ByteArrayToString(const Buffer: array of Byte; Separator: string = ' '): string; overload;
var
  i: integer;
  s: string;
  c: Char;
begin
  s := '';
  for i := 0 to Length(Buffer) - 1 do
  begin
    if Buffer[i] < 33 then c := '.'
    else c := Chr(Buffer[i]);
    s := s + c + Separator;
  end;
  Result := s;
end;

function ByteArrayToString(PBuffer: PByte; BufferSize: integer; Separator: string = ' '): string; overload;
var
  i: integer;
  s: string;
  c: Char;
begin
  s := '';
  for i := 1 to BufferSize do
  begin
    if PBuffer[i] < 33 then c := '.'
    else c := Chr(PBuffer[i]);
    s := s + c + Separator;
  end;
  Result := s;
end;

//procedure SaveBufferToFile(PBuffer: PByte; BufSize: integer; const FileName: string);
//var
//  ms: TMemoryStream;
//begin
//  ms := TMemoryStream.Create;
//  try
//    ms.Write(PBuffer^, BufSize);
//    ms.SaveToFile(FileName);
//  finally
//    ms.Free;
//  end;
//end;

procedure SwitchBool(var BoolVar: Boolean);
begin
  BoolVar := not BoolVar;
end;

procedure SwitchBool(PBoolVar: PBoolean);
begin
  PBoolVar^ := not PBoolVar^;
end;

end.
