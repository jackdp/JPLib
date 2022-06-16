unit JPL.SimpleZip;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  Writing a string to a ZIP file and reading a string from a ZIP file.
  Useful when writing / reading large configuration files in JSON or XML format.


  2022.06
}


{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}



interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  Zipper
  {$ELSE}
    {$IFDEF DELPHIXE_OR_BELOW}
    Unit for Delphi XE2 or newer!           
    {$ELSE}
    System.Zip
    {$ENDIF}
  {$ENDIF}
  ;


type

  {$IFDEF FPC}
  TSimpleUnzipper = class
  private type
    TUnzipMethod = (umStream, umFile);
  private
    FUnzipMethod: TUnzipMethod;
    FUnzippedStream: TMemoryStream;
    FUserStream: TStream;
    FUnZipper: TUnZipper;
    FZipFileName: string;
    FOutFileName: string;
    function Decompress(ZipEntryIndex: integer = 0): Boolean;
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  public
    constructor Create(const ZipFileName: string);
    destructor Destroy; override;

    function DecompressToStream(OutStream: TStream; ZipEntryIndex: integer = 0): Boolean;
    function DecompressToFile(const FileName: string; ZipEntryIndex: integer = 0): Boolean;
  end;
  {$ENDIF}

  {$IFDEF DCC}
  TSimpleUnzipper = class
  private
    FZipFileName: string;
  public
    constructor Create(const ZipFileName: string);
    function DecompressToStream(OutStream: TStream; ZipEntryIndex: integer = 0): Boolean;
    function DecompressToFile(const FileName: string; ZipEntryIndex: integer = 0): Boolean;
  end;
  {$ENDIF}


procedure SaveStringToZipFile(const ZipFileName, FileContent, ZipEntryFileName: string; ZipFileComment: string = '');
function GetStringFromZipFile(const ZipFileName: string; Encoding: TEncoding; Default: string = ''; ZipEntryIndex: integer = 0): string;

function IsZipStream(Stream: TStream; LittleEndian: Boolean = True): Boolean;
function IsZipFile(const FileName: string; LittleEndian: Boolean = True): Boolean;



implementation



function IsZipStream(Stream: TStream; LittleEndian: Boolean = True): Boolean;
const
  ZIP_FILE_SIGNATURE = $504B; // 'PK' Phil Katz
var
  dw: Word;
  xp: Int64;

  function SwapWord(Value: Word): Word;
  type
    Bytes = packed array[0..1] of Byte;
  begin
    Bytes(Result)[0]:= Bytes(Value)[1];
    Bytes(Result)[1]:= Bytes(Value)[0];
  end;

begin
  Result := False;
  if Stream.Size < 2 then Exit;
  xp := Stream.Position;
  Stream.Position := 0;
  if Stream.Read(dw, 2) <> 2 then Exit;
  Stream.Position := xp;
  if LittleEndian then dw := SwapWord(dw);
  Result := dw = ZIP_FILE_SIGNATURE;
end;

function IsZipFile(const FileName: string; LittleEndian: Boolean = True): Boolean;
var
  fs: TFileStream;
begin
  if not FileExists(FileName) then Exit(False);
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsZipStream(fs, LittleEndian);
  finally
    fs.Free;
  end;
end;


{$IFDEF FPC}
procedure SaveStringToZipFile(const ZipFileName, FileContent, ZipEntryFileName: string; ZipFileComment: string = '');
var
  Zipper: TZipper;
  ss: TStringStream;
begin
  ss := TStringStream.Create(FileContent);
  Zipper := TZipper.Create;
  try
    Zipper.Entries.AddFileEntry(ss, ZipEntryFileName);
    Zipper.FileComment := ZipFileComment;
    Zipper.SaveToFile(ZipFileName);
  finally
    Zipper.Free;
    ss.Free;
  end;
end;
{$ENDIF}

{$IFDEF DCC}
procedure SaveStringToZipFile(const ZipFileName, FileContent, ZipEntryFileName: string; ZipFileComment: string = '');
var
  ZipFile: TZipFile;
  ss: TStringStream;
begin
  ss := TStringStream.Create(FileContent);
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(ZipFileName, zmWrite);
    ZipFile.Comment := ZipFileComment;
    ZipFile.Add(ss, ZipEntryFileName);
    ZipFile.Close;
  finally
    ZipFile.Free;
    ss.Free;
  end;
end;
{$ENDIF}


function GetStringFromZipFile(const ZipFileName: string; Encoding: TEncoding; Default: string = ''; ZipEntryIndex: integer = 0): string;
var
  ss: TStringStream;
  ms: TMemoryStream;
  su: TSimpleUnzipper;
begin
  Result := Default;

  ss := TStringStream.Create('', Encoding);
  ms := TMemoryStream.Create;
  su := TSimpleUnzipper.Create(ZipFileName);
  try
    if not su.DecompressToStream(ms, ZipEntryIndex) then Exit;
    ss.LoadFromStream(ms);
    Result := ss.DataString;
  finally
    su.Free;
    ms.Free;
    ss.Free;
  end;
end;



{$IFDEF FPC}
constructor TSimpleUnzipper.Create(const ZipFileName: string);
begin
  inherited Create;
  FZipFileName := ZipFileName;
  FUnzippedStream := TMemoryStream.Create;
  FUnZipper := TUnZipper.Create;
  FUserStream := nil;
end;

destructor TSimpleUnzipper.Destroy;
begin
  FUnzippedStream.Free;
  FUnZipper.Free;
  inherited Destroy;
end;

function TSimpleUnzipper.Decompress(ZipEntryIndex: integer = 0): Boolean;
var
  Entry: TFullZipFileEntry;
  xCount: integer;
begin
  Result := False;

  FUnzippedStream.Clear;

  FUnZipper.FileName := FZipFileName;
  FUnZipper.Examine;
  FUnZipper.OnCreateStream := DoCreateOutZipStream;
  FUnZipper.OnDoneStream := DoDoneOutZipStream;

  xCount := FUnZipper.Entries.Count;
  if (xCount = 0) or (ZipEntryIndex > xCount - 1) then Exit;
  Entry := FUnZipper.Entries[ZipEntryIndex];
  FUnZipper.UnZipFile(Entry.ArchiveFileName);

  Result := True;
end;

function TSimpleUnzipper.DecompressToStream(OutStream: TStream; ZipEntryIndex: integer): Boolean;
begin
  FUnzipMethod := umStream;
  FUserStream := OutStream;
  Result := Decompress(ZipEntryIndex);
end;

function TSimpleUnzipper.DecompressToFile(const FileName: string; ZipEntryIndex: integer = 0): Boolean;
begin
  FUnzipMethod := umFile;
  FOutFileName := FileName;
  Result := Decompress(ZipEntryIndex);
end;

procedure TSimpleUnzipper.DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := FUnzippedStream;
end;

procedure TSimpleUnzipper.DoDoneOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  FUnzippedStream.Position := 0;

  if FUnzipMethod = umStream then FUserStream.CopyFrom(FUnzippedStream, FUnzippedStream.Size)
  else {if FUnzipMethod = umFile then} FUnzippedStream.SaveToFile(FOutFileName);

  FUnzippedStream.Clear;
end;
{$ENDIF}


{$IFDEF DCC}
constructor TSimpleUnzipper.Create(const ZipFileName: string);
begin
  inherited Create;
  FZipFileName := ZipFileName;
end;

function TSimpleUnzipper.DecompressToStream(OutStream: TStream; ZipEntryIndex: integer = 0): Boolean;
var
  ZipFile: TZipFile;
  TempStream: TMemoryStream;
  Bytes: TBytes;
begin
  Result := False;

  ZipFile := TZipFile.Create;
  TempStream := TMemoryStream.Create;
  try
    ZipFile.Open(FZipFileName, zmRead);
    try
      if (ZipEntryIndex < 0) or (ZipEntryIndex > ZipFile.FileCount - 1) then Exit;

      ZipFile.Read(ZipEntryIndex, Bytes);
      TempStream.Write(Bytes, Length(Bytes));
      SetLength(Bytes, 0);
      TempStream.Position := 0;
      OutStream.CopyFrom(TempStream, TempStream.Size);

    finally
      ZipFile.Close;
    end;

    Result := True;

  finally
    TempStream.Free;
    ZipFile.Free;
  end;
end;

{
Memory leak in XE5 and Rio! Why?

  An unexpected memory leak has occurred. The unexpected small block leaks are:
  1 - 12 bytes: TStream x 1
}
//function TSimpleUnzipper.DecompressToStream(OutStream: TStream; ZipEntryIndex: integer = 0): Boolean;
//var
//  ZipFile: TZipFile;
//  Header: TZipHeader;
//  TempStream: TStream;
//begin
//  Result := False;
//
//  ZipFile := TZipFile.Create;
//  TempStream := TStream.Create;
//  try
//    ZipFile.Open(FZipFileName, zmRead);
//    try
//      if (ZipEntryIndex < 0) or (ZipEntryIndex > ZipFile.FileCount - 1) then Exit;
//      ZipFile.Read(ZipEntryIndex, TempStream, Header);
//      TempStream.Position := 0;
//      OutStream.CopyFrom(TempStream, TempStream.Size);
//    finally
//      ZipFile.Close;
//    end;
//
//    Result := True;
//
//  finally
//    TempStream.Free;
//    ZipFile.Free;
//  end;
//end;

function TSimpleUnzipper.DecompressToFile(const FileName: string; ZipEntryIndex: integer = 0): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
     Result := DecompressToStream(fs, ZipEntryIndex);
  finally
    fs.Free;
  end;
end;
{$ENDIF}


end.

