unit JPL.Bytes;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
 }


{$I .\..\jp.inc}
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils;



function GetFileContentAsBytes(const FileName: string; var Bytes: TBytes): Boolean;
{$IFDEF DCC}
function FileTruncate(Handle: THandle; Size: Int64): Boolean;
{$ENDIF}
function SaveBytesToFile(const FileName: string; const Bytes: TBytes): Boolean;
procedure ConcatTBytes(const Bytes1, Bytes2: TBytes; var OutBytes: TBytes);



implementation



function GetFileContentAsBytes(const FileName: string; var Bytes: TBytes): Boolean;
var
  hFile: THandle;
  Buffer: array[0..511] of Byte;
  xRead, BufNo, BufLen: integer;
begin
  Result := False;
  SetLength(Bytes, 0);
  if not FileExists(FileName) then Exit;

  hFile := FileOpen(FileName, fmOpenRead);
  if hFile = THandle(-1) then Exit;

  try
    BufLen := Length(Buffer);
    BufNo := 0;

    while True do
    begin
      xRead := FileRead(hFile, Buffer, BufLen);
      if xRead <= 0 then Break;
      SetLength(Bytes, Length(Bytes) + xRead);
      Move(Buffer[0], Bytes[BufNo * BufLen], xRead);
      Inc(BufNo);
    end;

  finally
    FileClose(hFile);
  end;

  Result := True;
end;

{$IFDEF DCC}
function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
  if FileSeek(Handle, Size, FILE_BEGIN) = Size then Result := SetEndOfFile(Handle)
  else Result := False;
end;
{$ENDIF}

function SaveBytesToFile(const FileName: string; const Bytes: TBytes): Boolean;
var
  hFile: THandle;
begin
  Result := False;

  if not FileExists(FileName) then hFile := FileCreate(FileName)
  else hFile := FileOpen(FileName, fmOpenWrite);
  if hFile = THandle(-1) then Exit;

  try
    if not FileTruncate(hFile, 0) then Exit;
    FileWrite(hFile, Bytes[0], Length(Bytes));
  finally
    FileClose(hFile);
  end;

  Result := True;
end;

procedure ConcatTBytes(const Bytes1, Bytes2: TBytes; var OutBytes: TBytes);
begin
  SetLength(OutBytes, Length(Bytes1) + Length(Bytes2));
  Move(Bytes1[0], OutBytes[0], Length(Bytes1));
  Move(Bytes2[0], OutBytes[Length(Bytes1)], Length(Bytes2));
end;


end.
