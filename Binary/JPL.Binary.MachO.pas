unit JPL.Binary.MachO;

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
  Windows, Sysutils, Classes, Generics.Collections,
  JPL.Strings, JPL.Conversion, JPL.Binary.Types, JPL.Binary.Mach.Types, JPL.UPX, JPL.Binary.Misc, JPL.Binary.Procs;


type
  {$region ' ------- types -------- '}

  TLoadCommandExt = record
    lc: load_command;
    FileOffset: UInt32;
  end;

  TLoadCommands = TList<TLoadCommandExt>;

  TMachOFile = class
  private
    FFileName: string;
    FMachHeader: mach_header;
    FMachHeader64Reserved: UInt32;
    FMachMagicStr: string;
    FFileTypeStr: string;
    FCpuTypeStr: string;
    FBits: Byte;
    FEndianType: Byte;
    FEndianStr: string;
    FInfoStr: string;
    FIsPackedByUpx: Boolean;
    FIsValidUpxHeader: Boolean;
    FUpxHeader: TUpxHeader;
    FIsValidMachFile: Boolean;
    FFileSize: Int64;
    FStartOffset: Int64;
    FBinaryType: Integer;
    FLoadCommands: TLoadCommands;
    FSearchUpxInfo: Boolean;
    FUpxHeaderOffset: Int64;
    FEndOffset: Int64;
    procedure SetFileName(const Value: string);
    procedure Clear;
    procedure SetStartOffset(const Value: Int64);
    procedure SetSearchUpxInfo(const Value: Boolean);
    procedure SetEndOffset(const Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFileInfo;
    function AsString(bHexNumSep: Boolean = True; HexPrefix: string = ''; HexLowerCase: Boolean = False; bFromMachFat: Boolean = False): string;

    property FileName: string read FFileName write SetFileName;

    // W plikach Mach-O StartOffset zawsze wynosi 0. W plikach Mach-Fat (multiarch) może znajdować się kilka obiektów Mach-O.
    // Wówczas każdy obiekt Mach-O rozpoczyna się na innym offsecie w pliku Mach-Fat.
    property StartOffset: Int64 read FStartOffset write SetStartOffset;
    property EndOffset: Int64 read FEndOffset write SetEndOffset;

    property Bits: Byte read FBits;
    property MachHeader: mach_header read FMachHeader;
    property MachHeader64Reserved: UInt32 read FMachHeader64Reserved;
    property MachMagicStr: string read FMachMagicStr;
    property IsValidMachFile: Boolean read FIsValidMachFile;
    property FileTypeStr: string read FFileTypeStr;
    property CpuTypeStr: string read FCpuTypeStr;
    property FileSize: Int64 read FFileSize;
    property InfoStr: string read FInfoStr;
    property EndianType: Byte read FEndianType;
    property EndianStr: string read FEndianStr;
    property BinaryType: Integer read FBinaryType; // Bin types defined in JP.Binary.Types
    property LoadCommands: TLoadCommands read FLoadCommands;

    property SearchUpxInfo: Boolean read FSearchUpxInfo write SetSearchUpxInfo;
    property IsPackedByUpx: Boolean read FIsPackedByUpx;
    property IsValidUpxHeader: Boolean read FIsValidUpxHeader;
    property UpxHeaderOffset: Int64 read FUpxHeaderOffset;
    property UpxHeader: TUpxHeader read FUpxHeader;

  end;
  {$endregion types}


{$region '    helpers     '}

function MachMagicToStr(const MachMagic: UInt32): string;
function MachFileTypeToStr(const FileType: UInt32): string;
function MachCpuTypeToStr(const CpuType: Int32): string;
function LoadCommandTypeToStr(const cmd: UInt32): string;

{$endregion}


implementation



{$region ' -------------- helpers ----------------- '}
function LoadCommandTypeToStr(const cmd: UInt32): string;
begin
  case cmd of
    // Constants for the cmd field of all load commands, the type
    LC_SEGMENT: Result := 'segment of this file to be mapped';
    LC_SYMTAB: Result := 'link-edit stab symbol table info';
    LC_SYMSEG: Result := 'link-edit gdb symbol table info (obsolete)';
    LC_THREAD: Result := 'thread';
    LC_UNIXTHREAD: Result := 'Unix thread (includes a stack)';
    LC_LOADFVMLIB: Result := 'load a specified fixed VM shared library';
    LC_IDFVMLIB: Result := 'fixed VM shared library identification';
    LC_IDENT: Result := 'object identification info (obsolete)';
    LC_FVMFILE: Result := 'fixed VM file inclusion (internal use)';
    LC_PREPAGE: Result := 'prepage command (internal use)';
    LC_DYSYMTAB: Result := 'dynamic link-edit symbol table info';
    LC_LOAD_DYLIB: Result := 'load a dynamically linked shared library';
    LC_ID_DYLIB: Result := 'dynamically linked shared lib ident';
    LC_LOAD_DYLINKER: Result := 'load a dynamic linker';
    LC_ID_DYLINKER: Result := 'dynamic linker identification';
    LC_PREBOUND_DYLIB: Result := 'modules prebound for a dynamically linked shared library';
    LC_ROUTINES: Result := 'image routines';
    LC_SUB_FRAMEWORK: Result := 'sub framework';
    LC_SUB_UMBRELLA: Result := 'sub umbrella';
    LC_SUB_CLIENT: Result := 'sub client';
    LC_SUB_LIBRARY: Result := 'sub library';
    LC_TWOLEVEL_HINTS: Result := 'two-level namespace lookup hints';
    LC_PREBIND_CKSUM: Result := 'prebind checksum';

    /// load a dynamically linked shared library that is allowed to be missing (all symbols are weak imported).
    // LC_LOAD_WEAK_DYLIB (0x18 | LC_REQ_DYLD)
    LC_LOAD_WEAK_DYLIB: Result := 'LC_LOAD_WEAK_DYLIB';
    LC_SEGMENT_64: Result := '64-bit segment of this file to be mapped';
    LC_ROUTINES_64: Result := '64-bit image routines';
    LC_UUID: Result := 'uuid';
    LC_RPATH: Result := 'runpath additions';
    LC_CODE_SIGNATURE: Result := 'local of code signature';
    LC_SEGMENT_SPLIT_INFO: Result := 'local of info to split segments';
    LC_REEXPORT_DYLIB: Result := 'load and re-export dylib';
    LC_LAZY_LOAD_DYLIB: Result := 'delay load of dylib until first use';
    LC_ENCRYPTION_INFO: Result := 'encrypted segment information';
    LC_DYLD_INFO: Result := 'compressed dyld information';
    LC_DYLD_INFO_ONLY: Result := 'compressed dyld information only';
    LC_LOAD_UPWARD_DYLIB: Result := 'load upward dylib';
    LC_VERSION_MIN_MACOSX: Result := 'build for MacOSX min OS version';
    LC_VERSION_MIN_IPHONEOS: Result := 'build for iPhoneOS min OS version';
    LC_FUNCTION_STARTS: Result := 'compressed table of function start addresses';
    LC_DYLD_ENVIRONMENT: Result := 'string for dyld to treat like environment variable';
    LC_MAIN: Result := 'replacement for LC_UNIXTHREAD (Unix thread)';
    LC_DATA_IN_CODE: Result := 'table of non-instructions in __text';
    LC_SOURCE_VERSION: Result := 'source version used to build binary';
    LC_DYLIB_CODE_SIGN_DRS: Result := 'Code signing DRs copied from linked dylibs';
    LC_ENCRYPTION_INFO_64: Result := '64-bit encrypted segment information';
    LC_LINKER_OPTION: Result := 'linker options in MH_OBJECT files';
    LC_LINKER_OPTIMIZATION_HINT: Result := 'optimization hints in MH_OBJECT files';
    LC_VERSION_MIN_WATCHOS: Result := 'build for Watch min OS version';
  else
    Result := 'unknown load command type';
  end;
end;

function MachCpuTypeToStr(const CpuType: Int32): string;
begin
  case CpuType of
    CPU_TYPE_ANY: Result := 'Any';
    CPU_TYPE_VAX: Result := 'Vax';
    CPU_TYPE_MC680x0: Result := 'MC680x0';
    CPU_TYPE_X86: Result := 'x86';
    //CPU_TYPE_I386: Result := 'i386';
    CPU_TYPE_X86_64: Result := 'x86-64';
    8: Result := 'MIPS';
    CPU_TYPE_MC98000: Result := 'MC98000';
    CPU_TYPE_HPPA: Result := 'HPPA';
    CPU_TYPE_ARM: Result := 'ARM';
    CPU_TYPE_ARM64: Result := 'ARM64';
    CPU_TYPE_MC88000: Result := 'MC88000';
    CPU_TYPE_SPARC: Result := 'SPARC';
    CPU_TYPE_I860: Result := 'i860';
    16: Result := 'ALPHA';
    CPU_TYPE_POWERPC: Result := 'PowerPC';
    CPU_TYPE_POWERPC64: Result := 'PowerPC 64';
  else
    Result := '';
  end;
end;

function MachFileTypeToStr(const FileType: UInt32): string;
begin
  case FileType of
    MH_OBJECT: Result := 'relocatable object file';
    MH_EXECUTE: Result := 'demand paged executable file';
    MH_FVMLIB: Result := 'fixed VM shared library file';
    MH_CORE: Result := 'core file';
    MH_PRELOAD: Result := 'preloaded executable file';
    MH_DYLIB: Result := 'dynamically bound shared library';
    MH_DYLINKER: Result := 'dynamic link editor';
    MH_BUNDLE: Result := 'dynamically bound bundle file';
    MH_DYLIB_STUB: Result := 'shared library stub for static linking';
    MH_DSYM: Result := 'companion file with only debug sections';
    MH_KEXT_BUNDLE: Result := 'x86_64 kexts';
  else
    Result := '';
  end;
end;

function MachMagicToStr(const MachMagic: UInt32): string;
begin
  case MachMagic of
    MH_MAGIC: Result := 'Mach32 MAGIC';
    MH_CIGAM: Result := 'Mach32 CIGAM';
    MH_MAGIC_64: Result := 'Mach64 MAGIC';
    MH_CIGAM_64: Result := 'Mach64 CIGAM';
  else
    Result := '';
  end;
end;
{$endregion helpers}


{$region ' ------------------------------------ TMachOFile ---------------------------------------------- '}

constructor TMachOFile.Create;
begin
  inherited;
  Clear;
  FLoadCommands := TLoadCommands.Create;
  FSearchUpxInfo := True;
  FStartOffset := 0;
  FEndOffset := 0;
end;

destructor TMachOFile.Destroy;
begin
  FLoadCommands.Free;
  inherited;
end;

procedure TMachOFile.Clear;
begin
  FillChar(FMachHeader, SizeOf(FMachHeader), 0);
  FIsValidMachFile := False;
  FMachHeader64Reserved := 0;
  FMachMagicStr := '';
  FFileTypeStr := '';
  FCpuTypeStr := '';
  FEndianType := MACHO_UNKNOWN_ENDIAN;
  FEndianStr := '';
  FBinaryType := BIN_UNKNOWN;

  FUpxHeaderOffset := -1;
  FFileSize := 0;
  FBits := 0;
  FInfoStr := '';
  FIsPackedByUpx := False;
  FIsValidUpxHeader := False;
  FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
end;

procedure TMachOFile.ReadFileInfo;
var
  fs: TFileStream;
  x, i: integer;
  //Buffer: array[0..1024*10] of Byte;
  Magic: UInt32;
  lce: TLoadCommandExt;
  dwX: UInt32;
  pBuffer: PByte;
  BufSize: Integer;
  BufStartOffset: Int64;
begin
  Clear;
  FileName := Trim(FileName);
  if FileName = '' then Exit;
  if not FileExists(FileName) then Exit;
  FFileSize := FileSizeInt(FileName);
  if FFileSize < SizeOf(mach_header) then Exit;

  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try

    if FEndOffset <= 0 then FEndOffset := fs.Size;
    fs.Position := StartOffset;

    // -------------------------- Magic --------------------------
    x := fs.Read(Magic, SizeOf(Magic));
    if x <> SizeOf(Magic) then Exit;
                 //Msg('StartOffset: ' + StartOffset.ToString + CRLF + 'Magic: ' + IntToHex(Magic, 8));
    case Magic of

      MH_MAGIC: // $FE ED FA CE (4 277 009 102)
        begin
          FBits := 32;
          FEndianType := MACHO_LITTLE_ENDIAN;
          FEndianStr := 'Little endian';
        end;

      MH_CIGAM: // $CE FA ED FE (3 472 551 422)
        begin
          FBits := 32;
          FEndianType := MACHO_BIG_ENDIAN;
          FEndianStr := 'Big endian';
        end;

      MH_MAGIC_64: // $FE ED FA CF (4 277 009 103)
        begin
          FBits := 64;
          FEndianType := MACHO_LITTLE_ENDIAN;
          FEndianStr := 'Little endian';
        end;

      MH_CIGAM_64: // $CF FA ED FE (3 489 328 638)
        begin
          FBits := 64;
          FEndianType := MACHO_BIG_ENDIAN;
          FEndianStr := 'Big endian';
        end;

      else
        begin
          // invalid Mach-O magic
          FEndianType := MACHO_UNKNOWN_ENDIAN;
          FEndianStr := 'Unknown endian';
          Exit;
        end;

    end;
                                                     //Msg(FEndianStr);
    if (FBits = 32) and (FEndianType = MACHO_LITTLE_ENDIAN) then FBinaryType := BIN_MACHO_32_LITTLE_ENDIAN
    else if (FBits = 32) and (FEndianType = MACHO_BIG_ENDIAN) then FBinaryType := BIN_MACHO_32_BIG_ENDIAN
    else if (FBits = 64) and (FEndianType = MACHO_LITTLE_ENDIAN) then FBinaryType := BIN_MACHO_64_LITTLE_ENDIAN
    else if (FBits = 64) and (FEndianType = MACHO_BIG_ENDIAN) then FBinaryType := BIN_MACHO_64_BIG_ENDIAN
    else FBinaryType := MACHO_UNKNOWN_ENDIAN;

    if FBinaryType = MACHO_UNKNOWN_ENDIAN then Exit;


    // ---------------------------------- Header --------------------------------
    fs.Position := StartOffset;
    x := fs.Read(FMachHeader, SizeOf(FMachHeader));
    if x <> SizeOf(FMachHeader) then Exit;

    // swapping bytes
    if FEndianType = MACHO_BIG_ENDIAN then
    begin
      //FMachHeader.magic := SwapBytes(FMachHeader.magic);
      FMachHeader.cputype := SwapBytes(FMachHeader.cputype);
      FMachHeader.cpusubtype := SwapBytes(FMachHeader.cpusubtype);
      FMachHeader.filetype := SwapBytes(FMachHeader.filetype);
      FMachHeader.ncmds := SwapBytes(FMachHeader.ncmds);
      FMachHeader.sizeofcmds := SwapBytes(FMachHeader.sizeofcmds);
      FMachHeader.flags := SwapBytes(FMachHeader.flags);
    end;

    if Bits = 64 then
    begin
      fs.Read(FMachHeader64Reserved, SizeOf(FMachHeader64Reserved));
      if FEndianType = MACHO_BIG_ENDIAN then FMachHeader64Reserved := SwapBytes(FMachHeader64Reserved);
    end;

    FMachMagicStr := MachMagicToStr(FMachHeader.magic);
    if FMachMagicStr = '' then Exit;

    x := SizeOf(MachHeader) + MachHeader.sizeofcmds;
    if Bits = {60} 64 then x := x + SizeOf(MachHeader64Reserved); // dlaczego tutaj dałem 60? Chyba coś mi się pop...
    if FileSize < x then Exit;

    FFileTypeStr := MachFileTypeToStr(FMachHeader.filetype);
    FCpuTypeStr := MachCpuTypeToStr(FMachHeader.cputype);



    // ---------------------------------- Load Commands ----------------------------------
    // Load Commands znajdują się bezpośrednio po nagłówku.
    // Całkowity rozmiar Load Commands: MachHeader.sizeofcmds
    // Liczba Load Commands: MachHeader.ncmds
    // Każde Load Commands rozpoczyna się od struktury load_command

    for i := 1 to MachHeader.ncmds do
    begin
      if {fs.Size} FEndOffset < fs.Position + SizeOf(load_command) then Exit;
      lce.FileOffset := fs.Position;
      x := fs.Read(lce.lc, SizeOf(lce.lc));
      if x <> SizeOf(lce.lc) then Exit;
      if FEndianType = MACHO_BIG_ENDIAN then
      begin
        lce.lc.cmd := SwapBytes(lce.lc.cmd);
        lce.lc.cmdsize := SwapBytes(lce.lc.cmdsize);
      end;
      FLoadCommands.Add(lce);
      dwX := fs.Position - SizeOf(lce.lc) + lce.lc.cmdsize;
      if {fs.Size} FEndOffset < dwX then Exit;
      fs.Position := dwX;
    end;


    FIsValidMachFile := True;




    // --------------------------- UPX -------------------------------
    if FSearchUpxInfo then
    begin


      x := 1024 * 10; // 10 KB
      BufSize := {fs.Size} (FEndOffset - FStartOffset) div 2;
      if BufSize > x then BufSize := x;
      BufStartOffset := {fs.Size} FEndOffset - BufSize;

      pBuffer := System.AllocMem(BufSize);
      try
        fs.Position := BufStartOffset;

        x := fs.Read(pBuffer^, BufSize);
        if x <> BufSize then Exit;
        //SaveBufferToFile(pBuffer, BufSize, 'buffer.hex');

        x := BufferFindLast(pBuffer, BufSize, 'UPX!', 0);
        if x > 0 then
        begin
          FUpxHeaderOffset := BufStartOffset + x;

          if {fs.Size} FEndOffset >= FUpxHeaderOffset + SizeOf(FUpxHeader) then
          begin
            fs.Position := FUpxHeaderOffset;
            x := fs.Read(FUpxHeader, SizeOf(FUpxHeader));
            if x <> SizeOf(FUpxHeader) then Exit;

            if FEndianType = MACHO_BIG_ENDIAN then
            begin
              FUpxHeader.u_len := SwapDword(FUpxHeader.u_len);
              FUpxHeader.c_len := SwapDword(FUpxHeader.c_len);
              FUpxHeader.u_adler := SwapDword(FUpxHeader.u_adler);
              FUpxHeader.c_adler := SwapDword(FUpxHeader.c_adler);
              FUpxHeader.UncompressedFileSize := SwapDword(FUpxHeader.UncompressedFileSize);
            end;

            if JPL.UPX.IsValidUpxHeader(FUpxHeader) then
            begin
              FIsPackedByUpx := True;
              FIsValidUpxHeader := True;
            end;

          end;

        end;

      finally
        FreeMem(pBuffer, BufSize);
      end;


    end;


//    if fs.Size >= SizeOf(FUpxHeader) then
//    begin
//      fs.Position := fs.Size - SizeOf(FUpxHeader);
//      x := fs.Read(FUpxHeader, SizeOf(FUpxHeader));
//      if x = SizeOf(FUpxHeader) then
//      begin
//        if JP.UPX.IsValidUpxHeader(FUpxHeader.Header) then
//        begin
//          FIsPackedByUpx := True;
//          FIsValidUpxHeader := True;
//        end;
//      end;
//    end;




  finally
    fs.Free;
  end;


//  FMachineStr := ELF_MachineToStr(FMachine);

  //FInfoStr := FObjectTypeStr + ' / Unix ELF ' + IntToStr(Bits) + ' / ' + FMachineStr;


end;

procedure TMachOFile.SetEndOffset(const Value: Int64);
begin
  FEndOffset := Value;
end;

procedure TMachOFile.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TMachOFile.SetSearchUpxInfo(const Value: Boolean);
begin
  FSearchUpxInfo := Value;
end;

procedure TMachOFile.SetStartOffset(const Value: Int64);
begin
  FStartOffset := Value;
end;

function TMachOFile.AsString(bHexNumSep: Boolean = True; HexPrefix: string = ''; HexLowerCase: Boolean = False; bFromMachFat: Boolean = False): string;
const
  CRLF = #13#10;
var
  s: string;
  lce: TLoadCommandExt;
  x: integer;
  uh: TUpxHeader;
  ufn: TUpxFormatName;
  hs: string;

  function DwordStr(x: DWORD; Desc: string = ''): string;
  begin
    hs := IntToHex(x, 8);
    if bHexNumSep then InsertNumSep(hs, ' ', 2);
    if HexLowerCase then hs := LowerCase(hs);
    hs := HexPrefix + hs;
    Result := 'hex: ' + hs + ' | dec: ' + Pad(IntToStrEx(x), 16, ' ') + ' | ';
    if Desc <> '' then Result := Result + Desc;
  end;

  function ByteStr(x: Byte; Desc: string = ''; HexPad: integer = -1; IntPad: integer = 3): string;
  begin
    hs := IntToHex(x, 2);
    if bHexNumSep then InsertNumSep(hs, ' ', 2);
    if HexLowerCase then hs := LowerCase(hs);
    hs := HexPrefix + hs;
    if HexPad > 0 then hs := Pad(hs, HexPad);
    Result := 'hex: ' + hs + ' | dec: ' + Pad(IntToStrEx(x), IntPad, ' ') + ' | ';
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

  s := '';

  if not bFromMachFat then
  begin
    s := s + 'File name: ' + FFileName + CRLF;
    s := s + 'File size: ' + IntToStrEx(FFileSize) + ' bytes  (' + GetFileSizeString(FFileSize) + ')' + CRLF;
    s := s + 'IsValidMachFile: ' + BoolToStr(IsValidMachFile) + CRLF;
  end
  else
  begin
    s := s + 'Is valid Mach Object: ' + BoolToStr(IsValidMachFile) + CRLF;
  end;

  if IsValidMachFile then
  begin

    s := s + 'Bits: ' + itos(Bits) + CRLF;
    s := s + 'Endian: ' + EndianStr + CRLF;
    if FSearchUpxInfo then s := s + 'Packed by UPX: ' + BoolToStr(FIsPackedByUpx) + CRLF;

    // --------------------------- mach header ----------------------------
    s := s + CRLF + '//////////////////////////// Mach Header ////////////////////////////' + CRLF;
    s := s + '       magic: ' + DwordStr(MachHeader.magic, MachMagicStr) + CRLF; // InsertNumSep(IntToHex(MachHeader.magic, 8), ' ', 2) + '   (' + MachMagicStr + ')' + CRLF;
    s := s + '     cputype: ' + IntStr(MachHeader.cputype, CpuTypeStr) + CRLF;
    s := s + '  cpusubtype: ' + IntStr(MachHeader.cpusubtype) + CRLF;
    s := s + '    filetype: ' + DwordStr(MachHeader.filetype, FileTypeStr) + CRLF;
    s := s + '       ncmds: ' + DwordStr(MachHeader.ncmds) + CRLF;
    s := s + '  sizeofcmds: ' + DwordStr(MachHeader.sizeofcmds) + CRLF; // MachHeader.sizeofcmds.ToString + CRLF;
    s := s + '       flags: ' + DwordStr(MachHeader.flags) + CRLF;
    if Bits = 64 then s := s + '    reserved: ' + DwordStr(MachHeader64Reserved) + CRLF;



    // --------------------------- UPX ----------------------------------
    if FSearchUpxInfo and FIsPackedByUpx then
    begin
      uh := FUpxHeader;
      s := s + CRLF + '//////////////////////////// UPX ////////////////////////////' + CRLF;
      s := s + ' UPX header offset: ' + Int64Str(FUpxHeaderOffset) + CRLF;
      s := s + '  ---- UPX Header ---- ' + CRLF;
      s := s + '           magic: ' + string(uh.UpxMagic) + CRLF;
      s := s + '         version: ' + ByteStr(uh.Compressor, '', 9, 16) + CRLF;
      GetUpxFormatName(uh.Format, ufn);
      s := s + '          format: ' + ByteStr(uh.Format, ufn.FullName + ' - ' + ufn.ShortName, 9, 16) + CRLF;
      s := s + '          method: ' + ByteStr(uh.Method, UpxCompressionMethodToStr(uh.Method), 9, 16) + CRLF;
      s := s + '           level: ' + ByteStr(uh.Level, '', 9, 16) + CRLF;
      s := s + '           u_len: ' + DwordStr(uh.u_len) + CRLF;
      s := s + '           c_len: ' + DwordStr(uh.c_len) + CRLF;
      s := s + '         u_adler: ' + DwordStr(uh.u_adler) + CRLF;
      s := s + '         c_adler: ' + DwordStr(uh.c_adler) + CRLF;
      s := s + '     u_file_size: ' + DwordStr(uh.UncompressedFileSize) + CRLF;
      s := s + '          filter: ' + ByteStr(uh.Filter1, '', 9, 16) + CRLF;
      s := s + '      filter_cto: ' + ByteStr(uh.Filter2, '', 9, 16) + CRLF;
      s := s + '           n_mru: ' + ByteStr(uh.n_mru, '', 9, 16) + CRLF;
      s := s + ' header_checksum: ' + ByteStr(uh.header_cheksum, '', 9, 16) + CRLF;
    end;



    // -------------------------- Load Commands --------------------------------
    s := s + CRLF + ' //////////////////////////// Load Commands //////////////////////////// ' + CRLF;
    x := 1;
    for lce in Self.LoadCommands do
    begin
      s := s + '  Load Command ' + itos(x) + CRLF;
      s := s + '   file offset: ' + DwordStr(lce.FileOffset) + CRLF;
      s := s + '           cmd: ' + DwordStr(lce.lc.cmd, LoadCommandTypeToStr(lce.lc.cmd)) + CRLF;
      s := s + '       cmdsize: ' + DwordStr(lce.lc.cmdsize) + CRLF;
      s := s + '  --------------------------------------------------------' + CRLF;

      Inc(x);
    end;


  end;


  Result := s;
end;

{$endregion TMachFile}

end.
