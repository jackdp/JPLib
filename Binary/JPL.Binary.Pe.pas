unit JPL.Binary.Pe;

interface

uses
  Windows, Messages, SysUtils, Classes,
  JPL.Files,
  JPL.Binary.Types, JPL.UPX, JPL.Conversion, JPL.Binary.Misc
  ;


const
  {$region '   CONST   '}

  IMAGE_FILE_DLL = $2000;  { File is a DLL. }
  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;  { File is executable  (i.e. no unresolved externel references). }

  DEFAULT_MAX_MEMORY_STREAM_SIZE = 100 * 1024 * 1024; // 100 MB

  SIGNATURE_PE = $00004550;
  SIGNATURE_NE = $0A05454E;

  OPTIONAL_HEADER_MAGIC_PE32 = $010B;
  OPTIONAL_HEADER_MAGIC_PE32PLUS = $020B;


    {$region '   CONST - Machine   '}
  IMAGE_FILE_MACHINE_UNKNOWN   = 0;
  IMAGE_FILE_MACHINE_ALPHA     = $0184; // Alpha_AXP
  IMAGE_FILE_MACHINE_ALPHA64   = $0284; // ALPHA64
  IMAGE_FILE_MACHINE_AM33      = $01d3; // Matsushita AM33
  IMAGE_FILE_MACHINE_AMD64     = $8664; // AMD64 (K8)
  IMAGE_FILE_MACHINE_ARM       = $01c0; // ARM Little-Endian
  IMAGE_FILE_MACHINE_ARM64     = $aa64; // ARMv8 in 64-bit mode
  IMAGE_FILE_MACHINE_ARMNT     = $01c4; // ARMv7 (or higher) Thumb mode only
  IMAGE_FILE_MACHINE_AXP64     = IMAGE_FILE_MACHINE_ALPHA64;
  IMAGE_FILE_MACHINE_CEE       = $C0EE;
  IMAGE_FILE_MACHINE_CEF       = $0CEF;
  IMAGE_FILE_MACHINE_EBC       = $0EBC; // EFI Byte Code
  IMAGE_FILE_MACHINE_I386      = $014c; // Intel 386.
  IMAGE_FILE_MACHINE_I486      = $14D; // Intel 486
  IMAGE_FILE_MACHINE_I586      = $14E; // Intel 586
  IMAGE_FILE_MACHINE_IA64      = $0200; // Intel 64
  IMAGE_FILE_MACHINE_M32R      = $9041; // Mitsubishi M32R little-endian
  IMAGE_FILE_MACHINE_MIPS16    = $0266; // MIPS
  IMAGE_FILE_MACHINE_MIPSFPU   = $0366; // MIPS
  IMAGE_FILE_MACHINE_MIPSFPU16 = $0466; // MIPS
  IMAGE_FILE_MACHINE_POWERPC   = $01F0; // IBM PowerPC Little-Endian
  IMAGE_FILE_MACHINE_POWERPCFP = $01f1;
  IMAGE_FILE_MACHINE_R10000    = $0168; // MIPS little-endian
  IMAGE_FILE_MACHINE_R3000     = $0162; // MIPS little-endian, 0x160 big-endian
  IMAGE_FILE_MACHINE_R4000     = $0166; // MIPS little-endian
  IMAGE_FILE_MACHINE_SH3       = $01a2; // SH3 little-endian
  IMAGE_FILE_MACHINE_SH3DSP    = $01a3;
  IMAGE_FILE_MACHINE_SH3E      = $01a4; // SH3E little-endian
  IMAGE_FILE_MACHINE_SH4       = $01a6; // SH4 little-endian
  IMAGE_FILE_MACHINE_SH5       = $01a8; // SH5
  IMAGE_FILE_MACHINE_THUMB     = $01c2;
  IMAGE_FILE_MACHINE_TRICORE   = $0520; // Infineon
  IMAGE_FILE_MACHINE_WCEMIPSV2 = $0169; // MIPS little-endian WCE v2
    {$endregion}

  MAGIC_PE32 = $10b;
  MAGIC_PE32PLUS = $20b;


    {$region '   CONST - DOS Header   '}
  DosHeaderFieldCount = 31;

  DosHeaderFieldNames: array[0..30] of string =
  (
    'e_magic', 'e_cblp', 'e_cp', 'e_crlc', 'e_cparhdr', 'e_minalloc', 'e_maxalloc', 'e_ss', 'e_sp', 'e_csum',
    'e_ip', 'e_cs', 'e_lfarlc', 'e_ovno', 'e_res[0]', 'e_res[1]', 'e_res[2]', 'e_res[3]', 'e_oemid', 'e_oeminfo',
    'e_res2[0]', 'e_res2[1]', 'e_res2[2]', 'e_res2[3]', 'e_res2[4]', 'e_res2[5]', 'e_res2[6]', 'e_res2[7]', 'e_res2[8]', 'e_res2[9]',
    'e_lfanew'
  );

  DosHeaderTypeNames: array[0..30] of string =
  (
    'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD',
    'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD',
    'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD', 'WORD',
    'LONG' //<-- LongInt
  );

  DosHeaderFieldDesc: array[0..30] of string =
  (
    'Magic number', 'Bytes on last page of file', 'Pages in file', 'Relocations', 'Size of header in paragraphs', 'Minimum extra paragraphs needed',
    'Maximum extra paragraphs needed', 'Initial (relative) SS value', 'Initial SP value', 'Checksum', 'Initial IP value', 'Initial (relative) CS value',
    'File address of relocation table', 'Overlay number', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)', 'OEM identifier (for e_oeminfo)',
    'OEM information (e_oemid specific)', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)', '(Reserved)',
    '(Reserved)', '(Reserved)', 'File address of new exe header'
  );
    {$endregion}


  DataDirectoryNames: array[0..15] of string =
  (
    'Export', 'Import', 'Resource', 'Exception', 'Security', 'Base Reloc',
    'Debug', 'Copyright', 'Global Ptr', 'TLS', 'Load Config', 'Bound Import',
    'IAT', 'COM', 'Delay Import', '(reserved)'
  );

  {$endregion CONST}


type

  EPeFileException = Exception;
  TImageSections = array of TImageSectionHeader;
  //TImageSections = TList<TImageSectionHeader>;


  {$region '   TPeFile   '}
  TPeFile = class
  private
    FFileName: string;
    FIsValidPeFile: Boolean;
    FDosHeader: TImageDosHeader;
    FFileHeader: TImageFileHeader;
    FPeSignature: LongWord;
    FBits: Byte;
    FOptionalHeader32: TImageOptionalHeader32;
    FOptionalHeader64: TImageOptionalHeader64;
    FOptionalHeaderMagicStr: string;
    FInfoStr: string;
    FOffset_PeSignature: Int64;
    FOffset_FileHeader: Int64;
    FOffset_PeHeader: Int64;
    FOffset_SectionsTable: Int64;
    FSectionNames: TStringList;
    FSections: TImageSections;
    FMaxMemoryStreamSize: integer;
    FIsFileLoaded: Boolean;
    fs: TFileStream;
    ms: TMemoryStream;
    FFileSize: Int64;
    FFirstFile4Bits: TBitsInfo;
    FIsPackedByUpx: Boolean;
    FIsValidUpxHeader: Boolean;
    FUpxHeader: TPeUpxHeader;
    FUpxVersion: string;
    FOffset_UpxHeader: Int64;
    FDataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of IMAGE_DATA_DIRECTORY;

    function GetDataDirectory(Index: integer): IMAGE_DATA_DIRECTORY;
    procedure SetFileName(const Value: string);
    procedure ClearInfo;
    procedure SetMaxMemoryStreamSize(const Value: integer);
    function GetDataStream: TStream;
    property DataStream: TStream read GetDataStream;
    procedure FreeStreams;
    procedure GetUpxInfo;
    procedure RaiseException(const Message: string);
  public
    PeSignatureArray: array[0..3] of Byte;
    constructor Create;
    destructor Destroy; override;
    procedure GetFileInfo;


    property FileName: string read FFileName write SetFileName;
    property FileSize: Int64 read FFileSize;
    property IsValidPeFile: Boolean read FIsValidPeFile;

    property DosHeader: TImageDosHeader read FDosHeader;
    property PeSignature: LongWord read FPeSignature;
    property FileHeader: TImageFileHeader read FFileHeader;
    property Bits: Byte read FBits;
    property OptionalHeader32: TImageOptionalHeader32 read FOptionalHeader32;
    property OptionalHeader64: TImageOptionalHeader64 read FOptionalHeader64;
    property OptionalHeaderMagicStr: string read FOptionalHeaderMagicStr;
    property InfoStr: string read FInfoStr;
    property Offset_PeSignature: Int64 read FOffset_PeSignature;
    property Offset_FileHeader: Int64 read FOffset_FileHeader;
    property Offset_PeHeader: Int64 read FOffset_PeHeader;
    property Offset_SectionsTable: Int64 read FOffset_SectionsTable;
    property SectionNames: TStringList read FSectionNames;
    property Sections: TImageSections read FSections;
    property MaxMemoryStreamSize: integer read FMaxMemoryStreamSize write SetMaxMemoryStreamSize;
    property IsFileLoaded: Boolean read FIsFileLoaded;
    property FirstFile4Bits: TBitsInfo read FFirstFile4Bits;
    property IsPackedByUpx: Boolean read FIsPackedByUpx;
    property IsValidUpxHeader: Boolean read FIsValidUpxHeader;
    property UpxHeader: TPeUpxHeader read FUpxHeader;
    property UpxVersion: string read FUpxVersion;
    property Offset_UpxHeader: Int64 read FOffset_UpxHeader;
    property DataDirectory[Index: integer]: IMAGE_DATA_DIRECTORY read GetDataDirectory;
  end;
  {$endregion}



function CoffHeaderMachineToStr(const Machine: WORD): string;
function Is64Bit(Machine: Word): Boolean;
function GetStrFromBytes(AB: array of Byte): string;
function GetSectionFlagsStr(const Characteristics: DWORD): string;



implementation

//uses
//  JP.Binary.Procs;




{$region '         helpers            '}

function BufferFind2(const Buffer: array of Byte; const s: AnsiString): integer;
//returns the 0-based index of the start of the first occurrence of S or -1 if there is no occurrence
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

procedure ClearBitsInfo(var BitsInfo: TBitsInfo);
begin
  BitsInfo.B1 := 0;
  BitsInfo.B2 := 0;
  BitsInfo.B3 := 0;
  BitsInfo.B4 := 0;
  BitsInfo.BitsStr := '';
end;

function GetBitsInfo(Stream: TStream; var BitsInfo: TBitsInfo; NotStandardChar: Char = '.'): Boolean; overload;
var
  Arr: array[0..3] of Byte;
  c1, c2, c3, c4: Char;
  OldPos: Int64;

  function FixChar(c: Char): Char;
  begin
    if (Ord(c) < 33) or (Ord(c) > 126) then c := NotStandardChar;
    Result := c;
  end;

begin
  ClearBitsInfo(BitsInfo);
  Result := False;
  if not Assigned(Stream) then Exit;

  OldPos := Stream.Position;

  if Stream.Size < 4 then Exit;
  Stream.Position := 0;
  Stream.ReadBuffer(Arr{%H-}, Length(Arr));
  Stream.Position := OldPos;

  BitsInfo.B1 := Arr[0];
  BitsInfo.B2 := Arr[1];
  BitsInfo.B3 := Arr[2];
  BitsInfo.B4 := Arr[3];
  c1 := FixChar(Chr(BitsInfo.B1));
  c2 := FixChar(Chr(BitsInfo.B2));
  c3 := FixChar(Chr(BitsInfo.B3));
  c4 := FixChar(Chr(BitsInfo.B4));
  BitsInfo.BitsStr := c1 + c2 + c3 + c4;

  Result := True;
end;

function GetSectionFlagsStr(const Characteristics: DWORD): string;
var
  c: DWORD;
  s: string;
begin
  s := '';
  c := Characteristics;
  if (c and IMAGE_SCN_CNT_CODE) > 0 then s := 'C';
  if (c and IMAGE_SCN_CNT_INITIALIZED_DATA) > 0 then s := s + 'I';
  if (c and IMAGE_SCN_CNT_UNINITIALIZED_DATA) > 0 then s := s + 'U';
  if (c and IMAGE_SCN_LNK_INFO) > 0 then s := s + 'Comm';
  if (c and IMAGE_SCN_LNK_REMOVE) > 0 then s := s + 'Rmv';
  if (c and IMAGE_SCN_LNK_COMDAT) > 0 then s := s + 'Comdat';
  if (c and IMAGE_SCN_LNK_NRELOC_OVFL) > 0 then s := s + 'ExtReloc';
  if (c and IMAGE_SCN_MEM_DISCARDABLE) > 0 then s := s + 'D';
  if (c and IMAGE_SCN_MEM_NOT_CACHED) > 0 then s := s + 'NotC';
  if (c and IMAGE_SCN_MEM_NOT_PAGED) > 0 then s := s + 'NotP';
  if (c and IMAGE_SCN_MEM_SHARED) > 0 then s := s + 'S';
  if (c and IMAGE_SCN_MEM_EXECUTE) > 0 then s := s + 'E';
  if (c and IMAGE_SCN_MEM_READ) > 0 then s := s + 'R';
  if (c and IMAGE_SCN_MEM_WRITE) > 0 then s := s + 'W';
  Result := s;
end;

function Is64Bit(Machine: Word): Boolean;
begin
  case Machine of
    IMAGE_FILE_MACHINE_ALPHA64, IMAGE_FILE_MACHINE_AMD64, IMAGE_FILE_MACHINE_ARM64, IMAGE_FILE_MACHINE_IA64: Result := True;
  else
    Result := False;
  end;
end;

function CoffHeaderMachineToStr(const Machine: WORD): string;
begin
  case Machine of
    IMAGE_FILE_MACHINE_UNKNOWN: Result := 'Unknown (0)';
    IMAGE_FILE_MACHINE_I386: Result := 'Intel 386'; //Intel 386 or compatible
    IMAGE_FILE_MACHINE_I486: Result := 'Intel 486';
    IMAGE_FILE_MACHINE_I586: Result := 'Intel 586';
    IMAGE_FILE_MACHINE_R3000: Result := 'MIPS little-endian R3000';
    $160: Result := 'MIPS big-endian';
    IMAGE_FILE_MACHINE_R4000: Result := 'MIPS little-endian R4000';
    IMAGE_FILE_MACHINE_R10000: Result := 'MIPS little-endian R10000';
    IMAGE_FILE_MACHINE_WCEMIPSV2: Result := 'MIPS little-endian WCE v2';
    IMAGE_FILE_MACHINE_ALPHA: Result := 'Alpha_AXP';
    IMAGE_FILE_MACHINE_SH3: Result := 'Hitachi SH3 little-endian';
    IMAGE_FILE_MACHINE_SH3DSP: Result := 'Hitachi SH3 DSP';
    IMAGE_FILE_MACHINE_SH3E: Result := 'SH3E little-endian';
    IMAGE_FILE_MACHINE_SH4: Result := 'Hitachi SH4 little-endian';
    IMAGE_FILE_MACHINE_SH5: Result := 'Hitachi SH5';
    IMAGE_FILE_MACHINE_ARM: Result := 'ARM little-endian';
    IMAGE_FILE_MACHINE_THUMB: Result := 'ARM or Thumb';
    IMAGE_FILE_MACHINE_AM33: Result := 'Matsushita AM33';
    IMAGE_FILE_MACHINE_POWERPC: Result := 'IBM PowerPC little-endian';
    IMAGE_FILE_MACHINE_POWERPCFP: Result := 'IBM PowerPC with floating point support';
    IMAGE_FILE_MACHINE_IA64: Result := 'Intel Itanium 64';
    IMAGE_FILE_MACHINE_MIPS16: Result := 'MIPS16';
    IMAGE_FILE_MACHINE_ALPHA64: Result := 'ALPHA64';
    IMAGE_FILE_MACHINE_MIPSFPU: Result := 'MIPS with FPU';
    IMAGE_FILE_MACHINE_MIPSFPU16: Result := 'MIPS16 with FPU';
    IMAGE_FILE_MACHINE_TRICORE: Result := 'Infineon';
    IMAGE_FILE_MACHINE_CEF: Result := 'CEF';
    IMAGE_FILE_MACHINE_EBC: Result := 'EFI byte code';
    IMAGE_FILE_MACHINE_AMD64: Result := 'AMD64'; // AMD64 (K8)';
    IMAGE_FILE_MACHINE_M32R: Result := 'Mitsubishi M32R little-endian';
    IMAGE_FILE_MACHINE_CEE: Result := 'CEE';
  else
    Result := 'UNKNOWN (' + IntToStr(Machine) + ')';
  end;
end;

function GetStrFromBytes(AB: array of Byte): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(AB) to High(AB) do Result := Result + Chr(AB[i]);
end;

{$endregion}


{$region ' ---------------------------- TPeFile ------------------------------ '}
constructor TPeFile.Create;
begin
  FMaxMemoryStreamSize := DEFAULT_MAX_MEMORY_STREAM_SIZE;
  FSectionNames := TStringList.Create;
  //FSections := TList<TImageSectionHeader>.Create;
  SetLength(FSections, 0);
  ClearInfo;
end;

destructor TPeFile.Destroy;
begin
  if Assigned(FSectionNames) then FSectionNames.Free;
  //if Assigned(FSections) then FSections.Free;
  FreeStreams;
  inherited;
end;

procedure TPeFile.FreeStreams;
begin
  if Assigned(fs) then FreeAndNil(fs);
  if Assigned(ms) then FreeAndNil(ms);
  FIsFileLoaded := False;
end;

procedure TPeFile.ClearInfo;
begin
  FIsValidPeFile := False;
  FillChar(FDosHeader, SizeOf(FDosHeader), 0);
  FillChar(FFileHeader, SizeOf(FFileHeader), 0);
  FillChar(FOptionalHeader32, SizeOf(FOptionalHeader32), 0);
  FillChar(FOptionalHeader64, SizeOf(FOptionalHeader64), 0);
  FBits := 0;
  FPeSignature := 0;
  FOptionalHeaderMagicStr := '';
  FInfoStr := '';
  FOffset_PeSignature := BIN_INVALID_OFFSET;
  FOffset_FileHeader := BIN_INVALID_OFFSET;
  FOffset_PeHeader := BIN_INVALID_OFFSET;
  FOffset_SectionsTable := BIN_INVALID_OFFSET;
  FIsFileLoaded := False;
  FFileSize := 0;
  FSectionNames.Clear;
  SetLength(FSections, 0); //FSections.Clear;
  FreeStreams;
  ClearBitsInfo(FFirstFile4Bits);
  FIsPackedByUpx := False;
  FIsValidUpxHeader := False;
  FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
  FUpxVersion := '';
  FOffset_UpxHeader := BIN_INVALID_OFFSET;
end;

procedure TPeFile.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

function TPeFile.GetDataDirectory(Index: integer): IMAGE_DATA_DIRECTORY;
begin
  Result.Size := 0;
  Result.VirtualAddress := 0;
  if not FIsValidPeFile then RaiseException('GetDataDirectory - Not a valid PE file!');
  if not (Index in [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1]) then RaiseException('GetDataDirectory - Invalid Index!');
  Result := FDataDirectory[Index];
end;


procedure TPeFile.SetMaxMemoryStreamSize(const Value: integer);
begin
  FMaxMemoryStreamSize := Value;
end;

function TPeFile.GetDataStream: TStream;
begin
  if Assigned(ms) then Result := ms
  else if Assigned(fs) then Result := fs
  else Result := nil;
end;

procedure TPeFile.GetFileInfo;
var
  x, i: integer;
  xw: Word;
  s {, Section1, Section2} {, Section3}: string;
  ish: TImageSectionHeader;
begin
  ClearInfo;

  if not FileExists(FileName) then Exit;

  FBits := 0;
  FOptionalHeaderMagicStr := '';
  FFileSize := FileSizeInt(FFileName);

  if FFileSize <= FMaxMemoryStreamSize then
  begin
    ms := TMemoryStream.Create;
    ms.LoadFromFile(FFileName);
  end
  else fs := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);

  FIsFileLoaded := True;

  GetBitsInfo(DataStream, FFirstFile4Bits, '.');

  try


    // ------------------------------- DOS Header -------------------------------------
    if DataStream.Size < SizeOf(FDosHeader) then Exit;
    x := DataStream.Read(FDosHeader, SizeOf(FDosHeader));
    if x <> SizeOf(FDosHeader) then Exit;


    // ------------------------------ PE Signature ---------------------------------------
    FOffset_PeSignature := FDosHeader._lfanew;
    if DataStream.Size < FOffset_PeSignature + SizeOf(FPeSignature) then Exit;

    DataStream.Position := FOffset_PeSignature;
    x := DataStream.Read(FPeSignature, SizeOf(FPeSignature));
    if x <> SizeOf(FPeSignature) then Exit;
    DataStream.Position := FOffset_PeSignature;
    DataStream.Read(PeSignatureArray, Length(PeSignatureArray));

    if FPeSignature = SIGNATURE_NE then // NE file
    begin
      FBits := 16;
      FInfoStr := 'Win16 NE';
      Exit;
    end;

    if FPeSignature <> SIGNATURE_PE then Exit;

    FBits := 32;


    // -------------------------------- File (Coff) Header ------------------------------------
    FOffset_FileHeader := FOffset_PeSignature + SizeOf(FPeSignature);
    if DataStream.Size < FOffset_FileHeader + SizeOf(FFileHeader) then Exit; //FOffset_CoffHeader + SizeOf(FPeSignature) + SizeOf(FFileHeader) then Exit;

    x := DataStream.Read(FFileHeader, SizeOf(FFileHeader));
    if x <> SizeOf(FFileHeader) then Exit;



    // --------------------------------- Optional (PE) Header -----------------------------------------
    FOffset_PeHeader := FOffset_FileHeader + SizeOf(FFileHeader);

    if DataStream.Size < DataStream.Position + 2 then Exit;
    x := DataStream.Read(xw, 2);
    if x <> 2 then Exit;

    if xw = OPTIONAL_HEADER_MAGIC_PE32 then
    begin
      FOptionalHeaderMagicStr := 'PE32';
      FBits := 32;
    end
    else if xw = OPTIONAL_HEADER_MAGIC_PE32PLUS then
    begin
      FOptionalHeaderMagicStr := 'PE32+';
      FBits := 64;
    end
    else
    begin
      FOptionalHeaderMagicStr := '';
      FBits := 0;
    end;



    //if Is64Bit(FileHeader.Machine) then FBits := 64;

    DataStream.Position := DataStream.Position - 2;

    // 32 bit
    if FBits = 32 then
    begin
      if DataStream.Size < DataStream.Position + SizeOf(FOptionalHeader32) then Exit;
      x := DataStream.Read(FOptionalHeader32, SizeOf(FOptionalHeader32));
      if x <> SizeOf(FOptionalHeader32) then Exit;
      FOffset_SectionsTable := FOffset_PeHeader + SizeOf(FOptionalHeader32);
      for i := 0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1 do FDataDirectory[i] := FOptionalHeader32.DataDirectory[i];
    end

    // 64 bit
    else

    begin
      if DataStream.Size < DataStream.Position + SizeOf(FOptionalHeader64) then Exit;
      x := DataStream.Read(FOptionalHeader64, SizeOf(FOptionalHeader64));
      if x <> SizeOf(FOptionalHeader64) then Exit;
      FOffset_SectionsTable := FOffset_PeHeader + SizeOf(FOptionalHeader64);
      for i := 0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1 do FDataDirectory[i] := FOptionalHeader64.DataDirectory[i];
    end;

    s := '';

    xw := FFileHeader.Characteristics;
    if xw and IMAGE_FILE_DLL > 0 then s := 'Library'
    else if xw and IMAGE_FILE_EXECUTABLE_IMAGE > 0 then s := 'Executable';

    if FOptionalHeaderMagicStr <> '' then s := s + ' / Win ' + FOptionalHeaderMagicStr;
    if s <> '' then s := s + ' / ' + CoffHeaderMachineToStr(FFileHeader.Machine);
    FInfoStr := s;



    // -------------------------------------------- Sections ------------------------------------------
    DataStream.Position := FOffset_SectionsTable;
    for i := 0 to FFileHeader.NumberOfSections - 1 do
    begin
      x := DataStream.Read(ish, SizeOf(TImageSectionHeader));
      if x <> SizeOf(TImageSectionHeader) then Break;

      //FSections.Add(ish);
      SetLength(FSections, Length(FSections) + 1);
      FSections[High(FSections)] := ish;

      //s := '';
      //for x := 0 to High(ish.Name) do s := s + Chr(ish.Name[x]);
      s := Trim(GetStrFromBytes(ish.Name));
      FSectionNames.Add(s);
    end;


    FIsValidPeFile := True;

    GetUpxInfo;
    //GetResources;

  finally
    FreeStreams;
  end;




end;


procedure TPeFile.GetUpxInfo;
var
  s, Section1, Section2: string;
  xPos, UpxHeaderOffset: integer;
  x: Int64;
  Buffer: array[0..1023] of Byte;
  Buf4Chars: array[0..3] of AnsiChar;
begin

  // ----------- UPX Header -------------
  if FSectionNames.Count >= 3 then
  begin

    Section1 := Trim(FSectionNames[0]);
    Section2 := Trim(FSectionNames[1]);
    //Section3 := Trim(FSectionNames[2]);

    // new versions of the UPX
    if ( Copy(Section1, 1, 3) = 'UPX' ) and ( Copy(Section2, 1, 3) = 'UPX' ) then
    begin

      UpxHeaderOffset := Sections[1].PointerToRawData - SizeOf(FUpxHeader);
      if (UpxHeaderOffset < DataStream.Size) and (DataStream.Size > (Int64(UpxHeaderOffset) + SizeOf(FUpxHeader))) then
      begin
        FOffset_UpxHeader := UpxHeaderOffset;
        DataStream.Position := UpxHeaderOffset;
        DataStream.Read(FUpxHeader, SizeOf(FUpxHeader));

        // wyszukiwanie łańcucha 'UPX!'
        if FUpxHeader.Header.UpxMagic <> UPX_MAGIC_STR then
        begin

          FIsPackedByUpx := False;
          FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
          FOffset_UpxHeader := BIN_INVALID_OFFSET;

          x := FOffset_SectionsTable; // Sections[1].PointerToRawData - SizeOf(Buffer);

          if DataStream.Size >= Int64(x) + SizeOf(Buffer) then
          begin

            DataStream.Position := x;
            DataStream.Read(Buffer{%H-}, SizeOf(Buffer));

            xPos := GetStringOffset(UPX_MAGIC_STR, Buffer);
            if xPos >= 0 then
            begin
              // UpxVersion: array[0..4] of AnsiChar; - 5 bytes
              UpxHeaderOffset := x + xPos - 5;
              if DataStream.Size >= Int64(UpxHeaderOffset) + SizeOf(FUpxHeader) then
              begin
                DataStream.Position := UpxHeaderOffset;
                DataStream.Read(FUpxHeader, SizeOf(FUpxHeader));
                if FUpxHeader.Header.UpxMagic = UPX_MAGIC_STR then
                begin
                  FIsPackedByUpx := True;
                  FUpxVersion := string(FUpxHeader.UpxVersion);
                  FIsValidUpxHeader := JPL.UPX.IsValidUpxHeader(FUpxHeader.Header);
                  FOffset_UpxHeader := UpxHeaderOffset;
                end;
              end;
            end;

          end;
        end

        else

        begin
          FUpxVersion := string(FUpxHeader.UpxVersion);
          FIsPackedByUpx := True;
          FIsValidUpxHeader := JPL.UPX.IsValidUpxHeader(FUpxHeader.Header);
        end;

      end;

    end;


    // UPX old versions - searching UPX additional info & version
    if FIsPackedByUpx and (not IsValidFloatStr(string(FUpxHeader.UpxVersion))) then // and (Section1 = 'UPX0') and (Section2 = 'UPX1') and (Section3 = 'UPX2') then
    begin

      x := FOffset_SectionsTable;

      if DataStream.Size >= Int64(x) + SizeOf(Buffer) then
      begin
        DataStream.Position := x;
        DataStream.Read(Buffer, SizeOf(Buffer));

        s := 'Id: UPX ';
        xPos := GetStringOffset(AnsiString(s), Buffer);
        if xPos >= 0 then
          if DataStream.Size >= Int64(x) + xPos + Length(s) + SizeOf(Buf4Chars) then
          begin
            DataStream.Position := Int64(x) + xPos + Length(s);
            DataStream.Read(Buf4Chars{%H-}, SizeOf(Buf4Chars));
            FUpxVersion := string(Buf4Chars);
          end;

      end;

    end;


  end; // UPX header

end;


procedure TPeFile.RaiseException(const Message: string);
begin
  raise EPeFileException.Create('EPeFileException: [' + UnitName + '] ' + Message);
end;


function GetBit(Value: UInt64 {QWord}; Index: Byte): Boolean;
begin
  Result := ((Value shr Index) and 1) = 1;
end;





{$endregion TPeFile}
end.
