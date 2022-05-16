unit JPL.Binary.MachFat;

{
  Jacek Pazera

  https://en.wikipedia.org/wiki/Mach-O
}


{$I .\..\jp.inc}

{$IFDEF FPC}
  {$IFNDEF HAS_RTL_GENERICS}For FPC 3.2.0 or newer only!{$ENDIF}
  {$MODE DELPHI}
{$ENDIF}


interface

uses
  Windows,
  Sysutils, Classes, Generics.Collections,

  JPL.Strings, JPL.Conversion,
  JPL.Binary.Types, JPL.Binary.Mach.Types, JPL.Binary.MachO, JPL.UPX, JPL.Binary.Procs, JPL.Math;


type
  {$region ' ------- types -------- '}

  TFatArchs = TList<fat_arch>;
  TMachObjects = TObjectList<TMachOFile>;

  TMachFatFile = class
  private
    FFileName: string;
    FFileSize: Int64;
    FBinaryType: Integer;
    FIsValidMachFatFile: Boolean;
    FSearchUpxInfo: Boolean;
    FFatHeader: fat_header;
    FFatArchs: TFatArchs;
    FMachObjects: TMachObjects;
    FIsPackedByUpx: Boolean;
    FUpxCompressedSize: Int64;
    FUpxUncompressedSize: Int64;
    FUpxCompressionRatio: Real;
    procedure SetFileName(const Value: string);
    procedure Clear;
    procedure SetSearchUpxInfo(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFileInfo;
    function AsString(bHexNumSep: Boolean = True; HexPrefix: string = ''; HexLowerCase: Boolean = False): string;

    property FileName: string read FFileName write SetFileName;
    property FileSize: Int64 read FFileSize;
    property IsValidMachFatFile: Boolean read FIsValidMachFatFile;
    property BinaryType: integer read FBinaryType;
    property FatHeader: fat_header read FFatHeader;
    property FatArchs: TFatArchs read FFatArchs;
    property MachObjects: TMachObjects read FMachObjects;

    property IsPackedByUpx: Boolean read FIsPackedByUpx;
    property SearchUpxInfo: Boolean read FSearchUpxInfo write SetSearchUpxInfo;
    property UpxCompressedSize: Int64 read FUpxCompressedSize;
    property UpxUncompressedSize: Int64 read FUpxUncompressedSize;
    property UpxCompressionRatio: Real read FUpxCompressionRatio;

  end;
  {$endregion types}


{$region '    helpers     '}

{$endregion}


implementation



{$region ' -------------- helpers ----------------- '}

{$endregion helpers}


{$region ' ------------------------------------ TMachOFile ---------------------------------------------- '}

constructor TMachFatFile.Create;
begin
  inherited;
  FFatArchs := TFatArchs.Create;
  FMachObjects := TMachObjects.Create;
  Clear;
  FSearchUpxInfo := True;
end;

destructor TMachFatFile.Destroy;
begin
  FFatArchs.Free;
  FMachObjects.Free;
  inherited;
end;

procedure TMachFatFile.Clear;
begin
  FBinaryType := BIN_UNKNOWN;
  FIsValidMachFatFile := False;
  FFileSize := 0;
  FFatArchs.Clear;
  FIsPackedByUpx := False;
  FMachObjects.Clear;
  FUpxUncompressedSize := 0;
  FUpxCompressedSize := 0;
  FUpxCompressionRatio := 0;
end;

procedure TMachFatFile.ReadFileInfo;
var
  fs: TFileStream;
  x, i: integer;
  Magic: UInt32;
  fa: fat_arch;
  mof: TMachOFile;
begin
  Clear;
  FileName := Trim(FileName);
  if FileName = '' then Exit;
  if not FileExists(FileName) then Exit;
  FFileSize := FileSizeInt(FileName);
  if FFileSize < SizeOf(mach_header) then Exit;

  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try

    fs.Position := 0;

    // -------------------------- Magic --------------------------
    x := fs.Read(Magic, SizeOf(Magic));
    if x <> SizeOf(Magic) then Exit;

    if Magic <> FAT_CIGAM then Exit;
    FBinaryType := BIN_MACHO_FAT_MULTIARCH;



    // ---------------------------------- Fat Header --------------------------------
    fs.Position := 0;
    x := fs.Read(FFatHeader, SizeOf(FFatHeader));
    if x <> SizeOf(FFatHeader) then Exit;

    FFatHeader.magic := SwapBytes(FFatHeader.magic);
    FFatHeader.nfat_arch := SwapBytes(FFatHeader.nfat_arch);

    if fs.Size < Int64(SizeOf(FFatHeader)) + (Int64(FFatHeader.nfat_arch) * Int64(SizeOf(fat_arch)))  then Exit;

    for i := 1 to FFatHeader.nfat_arch do
    begin
      x := fs.Read(fa, SizeOf(fa));
      if x <> SizeOf(fa) then Exit;
      fa.cputype := SwapBytes(fa.cputype);
      fa.cpusubtype := SwapBytes(fa.cpusubtype);
      fa.offset := SwapBytes(fa.offset);
      fa.size := SwapBytes(fa.size);
      fa.align := SwapBytes(fa.align);
      FFatArchs.Add(fa);
    end;

    for i := 0 to FFatArchs.Count - 1 do
    begin
      fa := FFatArchs[i];
      //Msg('fa.offset = ' + fa.offset.ToString);
      mof := TMachOFile.Create;
      mof.FileName := FFileName;
      mof.StartOffset := fa.offset;
      mof.EndOffset := Int64(fa.offset) + Int64(fa.size);
      mof.SearchUpxInfo := FSearchUpxInfo;
      mof.ReadFileInfo;
      if not mof.IsValidMachFile then
      begin
        mof.Free;
        Exit;
      end;
      if mof.IsPackedByUpx then
      begin
        FIsPackedByUpx := True;
        FUpxUncompressedSize := FUpxUncompressedSize + mof.UpxHeader.UncompressedFileSize;
        FUpxCompressedSize := FUpxCompressedSize + fa.size;
      end;
      FMachObjects.Add(mof);
    end;

    if IsPackedByUpx then FUpxCompressionRatio := PercentValue(FUpxCompressedSize, FUpxUncompressedSize);

    FIsValidMachFatFile := True;


  finally
    fs.Free;
  end;

end;

procedure TMachFatFile.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TMachFatFile.SetSearchUpxInfo(const Value: Boolean);
begin
  FSearchUpxInfo := Value;
end;

function TMachFatFile.AsString(bHexNumSep: Boolean = True; HexPrefix: string = ''; HexLowerCase: Boolean = False): string;
const
  CRLF = #13#10;
var
  s, hs: string;
  x: integer;
  fa: fat_arch;
  mof: TMachOFile;

  function DwordStr(x: DWORD; Desc: string = ''): string;
  begin
    hs := IntToHex(x, 8);
    if bHexNumSep then InsertNumSep(hs, ' ', 2);
    if HexLowerCase then hs := LowerCase(hs);
    hs := HexPrefix + hs;
    Result := 'hex: ' + hs + ' | dec: ' + Pad(IntToStrEx(x), 16, ' ') + ' | ';
    if Desc <> '' then Result := Result + Desc;
  end;

  function IntStr(x: Integer; Desc: string = ''): string;
  begin
    hs := IntToHex(x, 8);
    if bHexNumSep then InsertNumSep(hs, ' ', 2);
    if HexLowerCase then hs := LowerCase(hs);
    hs := HexPrefix + hs;
    Result := 'hex: ' + hs + ' | dec: ' + Pad(IntToStrEx(x), 16, ' ') + ' | ';
    if Desc <> '' then Result := Result + Desc;
  end;

  function Int64Str(x: Int64; Desc: string = ''): string;
  begin
    hs := IntToHex(x, 8);
    if bHexNumSep then InsertNumSep(hs, ' ', 2);
    if HexLowerCase then hs := LowerCase(hs);
    hs := HexPrefix + hs;
    Result := 'hex: ' + hs + ' | dec: ' + Pad(IntToStrEx(x), 16, ' ') + ' | ';
    if Desc <> '' then Result := Result + Desc;
  end;

begin
  Result := '';
  s := '';
  if FFileName = '' then Exit;

  s := 'File name: ' + FFileName + CRLF;
  s := s + 'File size: ' + IntToStrEx(FFileSize) + ' bytes  (' + GetFileSizeString(FFileSize) + ')' + CRLF;
  s := s + 'IsValidMachFatFile: ' + BoolToStr(IsValidMachFatFile) + CRLF;
  if FIsPackedByUpx then
  begin
    s := s + '          IsPackedByUpx: ' + BoolToStr(IsPackedByUpx) + CRLF;
    s := s + '  Compressed archs size: ' + Int64Str(FUpxCompressedSize) + CRLF;
    s := s + 'Uncompressed archs size: ' + Int64Str(FUpxUncompressedSize) + CRLF;
    s := s + '      Compression ratio: ' + FormatFloat('0.00%', FUpxCompressionRatio) + CRLF;
  end;


  if IsValidMachFatFile then
  begin

    //s := s + '  fat archs: ' + FFatArchs.Count.ToString + CRLF;

    s := s + '////////////////////////// Fat Header //////////////////////////' + CRLF;
    s := s + '       magic: ' + DwordStr(FFatHeader.magic) + CRLF;
    s := s + '   nfat_arch: ' + DwordStr(FFatHeader.nfat_arch) + CRLF + CRLF;


    s := s + '////////////////////////// Fat Archs //////////////////////////' + CRLF;
    x := 1;
    for fa in FFatArchs do
    begin
      s := s + '     FAT ARCH ' + itos(x) + CRLF;
      s := s + '        cputype: ' + IntStr(fa.cputype, MachCpuTypeToStr(fa.cputype)) + CRLF;
      s := s + '     cpusubtype: ' + IntStr(fa.cpusubtype) + CRLF;
      s := s + '         offset: ' + DwordStr(fa.offset) + CRLF;
      s := s + '           size: ' + DwordStr(fa.size) + CRLF;
      s := s + '          align: ' + DwordStr(fa.align) + CRLF + CRLF;
      Inc(x);
    end;

    s := s + '//////////////////////////////////// Mach Objects ////////////////////////////////////' + CRLF;
    x := 1;
    for mof in FMachObjects do
    begin
      s := s + '//////////////////// Mach Object ' + itos(x) + ' / ' + itos(FMachObjects.Count) +  ' ////////////////////' + CRLF;
      s := s + mof.AsString(bHexNumSep, HexPrefix, HexLowerCase, True) + CRLF;
      Inc(x);
    end;

  end;


  Result := s;
end;

{$endregion TMachFile}

end.
