unit JPL.UPX;

{
  Jacek Pazera
  https://github.com/jackdp
  FUPX - http://www.pazera-software.com/products/free-upx/

  UPX - https://upx.github.io/
}


{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface


uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils
  ;



const

  {$region '   Constants from conf.h   '}

  (* exit codes of this program: 0 ok, 1 error, 2 warning *)
  EXIT_OK         = 0;
  EXIT_ERROR      = 1;
  EXIT_WARN       = 2;

  EXIT_USAGE      = 1;
  EXIT_FILE_READ  = 1;
  EXIT_FILE_WRITE = 1;
  EXIT_MEMORY     = 1;
  EXIT_CHECKSUM   = 1;
  EXIT_INIT       = 1;
  EXIT_INTERNAL   = 1;


  // magic constants for patching
  UPX_MAGIC_LE32          = $21585055;      (* "UPX!" *)
  UPX_MAGIC2_LE32         = $D5D0D8A1;


  // upx_compress() error codes
  UPX_E_OK                    = (0);
  UPX_E_ERROR                 = (-1);
  UPX_E_OUT_OF_MEMORY         = (-2);
  UPX_E_NOT_COMPRESSIBLE      = (-3);
  UPX_E_INPUT_OVERRUN         = (-4);
  UPX_E_OUTPUT_OVERRUN        = (-5);
  UPX_E_LOOKBEHIND_OVERRUN    = (-6);
  UPX_E_EOF_NOT_FOUND         = (-7);
  UPX_E_INPUT_NOT_CONSUMED    = (-8);
  UPX_E_NOT_YET_IMPLEMENTED   = (-9);
  UPX_E_INVALID_ARGUMENT      = (-10);


  // Executable formats. Note: big endian types are >= 128.
  UPX_F_DOS_COM           = 1;
  UPX_F_DOS_SYS           = 2;
  UPX_F_DOS_EXE           = 3;
  UPX_F_DJGPP2_COFF       = 4;
  UPX_F_WATCOM_LE         = 5;
  UPX_F_VXD_LE            = 6;
  UPX_F_DOS_EXEH          = 7;               (* OBSOLETE *)
  UPX_F_TMT_ADAM          = 8;
  UPX_F_WIN32_PE          = 9;
  UPX_F_LINUX_i386        = 10;
  UPX_F_WIN16_NE          = 11;
  UPX_F_LINUX_ELF_i386    = 12; // i386-linux.elf / linux/elf386
  UPX_F_LINUX_SEP_i386    = 13;
  UPX_F_LINUX_SH_i386     = 14;
  UPX_F_VMLINUZ_i386      = 15;
  UPX_F_BVMLINUZ_i386     = 16;
  UPX_F_ELKS_8086         = 17;
  UPX_F_PS1_EXE           = 18;
  UPX_F_VMLINUX_i386      = 19;
  UPX_F_LINUX_ELFI_i386   = 20;
  UPX_F_WINCE_ARM_PE      = 21;
  UPX_F_LINUX_ELF64_AMD   = 22; // amd64-linux.elf / linux/ElfAMD
  UPX_F_LINUX_ELF32_ARMEL = 23;
  UPX_F_BSD_i386          = 24; // i386-freebsd.elf / BSD/elf386
  UPX_F_BSD_ELF_i386      = 25; // i386-linux.elf / BSD/elf386
  UPX_F_BSD_SH_i386       = 26;

  UPX_F_VMLINUX_AMD64     = 27;
  UPX_F_VMLINUX_ARMEL     = 28;
  UPX_F_MACH_i386         = 29;
  UPX_F_LINUX_ELF32_MIPSEL = 30;
  UPX_F_VMLINUZ_ARMEL     = 31;
  UPX_F_MACH_ARMEL        = 32;

  UPX_F_DYLIB_i386        = 33;
  UPX_F_MACH_AMD64        = 34;
  UPX_F_DYLIB_AMD64       = 35;

  UPX_F_WIN64_PEP         = 36;

  UPX_F_PLAIN_TEXT        = 127;

  UPX_F_ATARI_TOS         = 129;
  UPX_F_SOLARIS_SPARC     = 130;
  UPX_F_MACH_PPC32        = 131;
  UPX_F_LINUX_ELFPPC32    = 132; // powerpc-linux.elf / linux/ElfPPC
  UPX_F_LINUX_ELF32_ARMEB = 133;
  UPX_F_MACH_FAT          = 134;
  UPX_F_VMLINUX_ARMEB     = 135;
  UPX_F_VMLINUX_PPC32     = 136;
  UPX_F_LINUX_ELF32_MIPSEB = 137;
  UPX_F_DYLIB_PPC32       = 138;


  // compression methods
  M_ALL           = (-1);
  M_END           = (-2);
  M_NONE          = (-3);
  M_SKIP          = (-4);
  M_ULTRA_BRUTE   = (-5);
  // compression methods - DO NOT CHANGE
  M_NRV2B_LE32    = 2;
  M_NRV2B_8       = 3;
  M_NRV2B_LE16    = 4;
  M_NRV2D_LE32    = 5;
  M_NRV2D_8       = 6;
  M_NRV2D_LE16    = 7;
  M_NRV2E_LE32    = 8;
  M_NRV2E_8       = 9;
  M_NRV2E_LE16    = 10;
  //M_CL1B_LE32     = 11;
  //M_CL1B_8        = 12;
  //M_CL1B_LE16     = 13;
  M_CL1B_LE32     = 11;
  M_CL1B_8        = 12;
  M_CL1B_LE16     = 13;
  M_LZMA          = 14;
  M_DEFLATE       = 15;      (* zlib *)

//  M_IS_NRV2B(x)   ((x) >= M_NRV2B_LE32 && (x) <= M_NRV2B_LE16) // func. Result := x in [M_NRV2B_LE32..M_NRV2B_LE16]
//  M_IS_NRV2D(x)   ((x) >= M_NRV2D_LE32 && (x) <= M_NRV2D_LE16)
//  M_IS_NRV2E(x)   ((x) >= M_NRV2E_LE32 && (x) <= M_NRV2E_LE16)
//  //M_IS_CL1B(x)    ((x) >= M_CL1B_LE32  && (x) <= M_CL1B_LE16)
//  M_IS_LZMA(x)    (((x) & 255) == M_LZMA)
//  M_IS_DEFLATE(x) ((x) == M_DEFLATE)


  // filters
  FT_END          = (-1);
  FT_NONE         = (-2);
  FT_SKIP         = (-3);
  FT_ULTRA_BRUTE  = (-4);
  {$endregion}


const
  UPX_MAGIC_STR = 'UPX!'; // 0x 55 50 58 21


type

// packer.h
//    int version;
//    int format;                 // executable format
//    int method;                 // compresison method
//    int level;                  // compresison level 1..10
//    unsigned u_len;
//    unsigned c_len;
//    unsigned u_adler;
//    unsigned c_adler;
//    off_t u_file_size;
//    int filter;        // int = byte
//    int filter_cto;
//    int n_mru;                  // FIXME: rename to filter_misc
//    int header_checksum;

  //TUpxHeader = array[0..63] of Byte;

  // 32 bytes
  TUpxHeader = packed record
    UpxMagic: array[0..3] of AnsiChar;
    Compressor: Byte; // version
    Format: Byte; // format
    Method: Byte; // method
    Level: Byte;
    u_len: DWORD;
    c_len: DWORD;
    u_adler: DWORD;
    c_adler: DWORD;
    UncompressedFileSize: DWORD; // u_file_size
    Filter1: Byte; // filter
    Filter2: Byte; // filter_cto
    n_mru: Byte;
    header_cheksum: Byte;
  end;

  // 5 + 32 = 37 bytes
  TPeUpxHeader = packed record
    UpxVersion: array[0..4] of AnsiChar;
    Header: TUpxHeader;
  end;

  // 32 + 4 = 36 bytes
  TElfUpxHeader = packed record
    Header: TUpxHeader;
    UNKNOWN_BYTES: array [0..3] of Byte;
  end;

  TUpxFormatName = record
    ShortName: string;
    FullName: string;
  end;



function IsValidUpxHeader(UpxHeader: TUpxHeader): Boolean;
function UpxCompressionMethodToStr(const Method: integer; UnknownMethod: string = 'Unknown method'): string;
function GetUpxFormatName(const Format: integer; var ufn: TUpxFormatName; UnknownFormat: string = 'Unknown format'): Boolean;
function IsValidUpxExecutableFormat(const Format: integer): Boolean;
function IsValidUpxCompressionMethod(const Method: integer): Boolean;


implementation



function IsValidUpxHeader(UpxHeader: TUpxHeader): Boolean;
//var
//  Ratio:
begin
  Result :=
    (UpxHeader.UpxMagic = UPX_MAGIC_STR) and
    (IsValidUpxExecutableFormat(UpxHeader.Format)) and
    (IsValidUpxCompressionMethod(UpxHeader.Method))
    // and Ratio in 0..100
  ;
end;

function IsValidUpxCompressionMethod(const Method: integer): Boolean;
begin
  Result := Method in [M_NRV2B_LE32..M_DEFLATE];
  //Result := Method in [2..15];
end;

function IsValidUpxExecutableFormat(const Format: integer): Boolean;
begin
  Result := (Format in [UPX_F_DOS_COM..UPX_F_WIN64_PEP]) or (Format = UPX_F_PLAIN_TEXT) or (Format in [UPX_F_ATARI_TOS..UPX_F_DYLIB_PPC32]);
  //Result := (Format in [1..36]) or (Format = 127) or (Format in [129..138]);
end;

function GetUpxFormatName(const Format: integer; var ufn: TUpxFormatName; UnknownFormat: string = 'Unknown format'): Boolean;

  procedure Save(const FullName, ShortName: string);
  begin
    ufn.FullName := FullName;
    ufn.ShortName := ShortName;
  end;

begin
  Result := True;
  ufn.ShortName := '';
  ufn.FullName := '';

  case Format of

    // ------------------ little endian -----------------------
    UPX_F_DOS_COM: Save('i086-dos16.com', 'dos/com');
    UPX_F_DOS_SYS: Save('i086-dos16.sys', 'dos/sys');
    UPX_F_DOS_EXE: Save('i086-dos16.exe', 'dos/exe');
    UPX_F_DJGPP2_COFF: Save('i386-dos32.djgpp2.coff', 'djgpp2/coff');
    UPX_F_WATCOM_LE: Save('i386-dos32.watcom.le', 'watcom/le');
    UPX_F_VXD_LE: Save('i386-dos32.watcom.le', 'vxd/le'); // PackVxd = class(PackWcle)
    UPX_F_DOS_EXEH: Save('i086-dos16.exe', 'dos/exe');               (* OBSOLETE *)
    UPX_F_TMT_ADAM: Save('i386-dos32.tmt.adam', 'tmt/adam');
    UPX_F_WIN32_PE: Save('i386-win32.pe', 'win32/pe'); // p_w32pe.h: virtual const char *getName() const { return isrtm ? "rtm32/pe" : "win32/pe"; }
    UPX_F_LINUX_i386: Save('i386-linux.elf.execve', 'linux/386');
    UPX_F_WIN16_NE: Save('i286-win16.ne', 'win16/ne');
    UPX_F_LINUX_ELF_i386: Save('i386-linux.elf', 'linux/elf386');
    UPX_F_LINUX_SEP_i386: Save('i386-linux.sep', 'linux/sep386'); // --- jacek
    UPX_F_LINUX_SH_i386: Save('i386-linux.elf.shell', 'linux/sh386');
    UPX_F_VMLINUZ_i386: Save('i386-linux.kernel.vmlinuz', 'vmlinuz/386');
    UPX_F_BVMLINUZ_i386: Save('i386-linux.kernel.bvmlinuz', 'bvmlinuz/386');
    UPX_F_ELKS_8086: Save('i086-elks ???', 'elks/8086');
    UPX_F_PS1_EXE: Save('mipsel.r3000-ps1', 'ps1/exe');
    UPX_F_VMLINUX_i386: Save('i386-linux.kernel.vmlinux', 'vmlinux/386');
    UPX_F_LINUX_ELFI_i386: Save('i386-linux.elf.interp', 'linux/elfi386');
    UPX_F_WINCE_ARM_PE: Save('arm-wince.pe', 'arm/pe');
    UPX_F_LINUX_ELF64_AMD: Save('amd64-linux.elf', 'linux/ElfAMD');
    UPX_F_LINUX_ELF32_ARMEL: Save('arm-linux.elf', 'linux/armel');
    UPX_F_BSD_i386: Save('i386-bsd.elf.execve', 'BSD/386');
    UPX_F_BSD_ELF_i386: Save('i386-linux.elf', 'BSD/elf386'); // PackBSDElf32x86 = class(PackLinuxElf32x86)
    UPX_F_BSD_SH_i386: Save('i386-bsd.elf.shell', 'BSD/sh386'); // --- jacek

    UPX_F_VMLINUX_AMD64: Save('amd64-linux.kernel.vmlinux', 'vmlinux/AMD64');
    UPX_F_VMLINUX_ARMEL: Save('arm-linux.kernel.vmlinux', 'vmlinux/armel');
    UPX_F_MACH_i386: Save('i386-darwin.macho', 'Mach/i386');
    UPX_F_LINUX_ELF32_MIPSEL: Save('mipsel-linux.elf', 'linux/mipsel');
    UPX_F_VMLINUZ_ARMEL: Save('armel-linux.kernel.vmlinuz', 'vmlinuz/armel');
    UPX_F_MACH_ARMEL: Save('ARMEL-darwin.macho', 'Mach/ARMEL');

    UPX_F_DYLIB_i386: Save('i386-darwin.dylib', 'Dylib/i386');
    UPX_F_MACH_AMD64: Save('AMD64-darwin.macho', 'Mach/AMD64');
    UPX_F_DYLIB_AMD64: Save('AMD64-darwin.dylib', 'Dylib/AMD64');

    UPX_F_WIN64_PEP: Save('amd64-win64.pe', 'win64/pe');

    UPX_F_PLAIN_TEXT: Save('plain text', 'plain text'); // --- jacek


    // --------------------- big endian ----------------------------
    UPX_F_ATARI_TOS: Save('m68k-atari.tos', 'atari/tos');
    UPX_F_SOLARIS_SPARC: Save('sparc.solaris', 'solaris/sparc'); // PackSolarisSparc = class( PackUnixBe32 <- PackUnix <- Packer )
    UPX_F_MACH_PPC32: Save('powerpc-darwin.macho', 'Mach/ppc32');
    UPX_F_LINUX_ELFPPC32: Save('powerpc-linux.elf', 'linux/ElfPPC');
    UPX_F_LINUX_ELF32_ARMEB: Save('armeb-linux.elf', 'linux/armeb');
    UPX_F_MACH_FAT: Save('fat-darwin.macho', 'Mach/fat');
    UPX_F_VMLINUX_ARMEB: Save('armeb-linux.kernel.vmlinux', 'vmlinux/armeb');
    UPX_F_VMLINUX_PPC32: Save('powerpc-linux.kernel.vmlinux', 'vmlinux/ppc32');
    UPX_F_LINUX_ELF32_MIPSEB: Save('mips-linux.elf', 'linux/mipseb');
    UPX_F_DYLIB_PPC32: Save('powerpc-darwin.dylib', 'Dylib/ppc32');

  else
    Save(UnknownFormat, UnknownFormat);
    Result := False;
  end;

end;

function UpxCompressionMethodToStr(const Method: integer; UnknownMethod: string = 'Unknown method'): string;
begin
  case Method of
    // negative values - probably for internal use only
    M_ALL: Result := 'ALL';
    M_END: Result := 'END';
    M_NONE: Result := 'NONE';
    M_SKIP: Result := 'SKIP';
    M_ULTRA_BRUTE: Result := 'ULTRA BRUTE';

    // positive values - displayed by upx.exe
    M_NRV2B_LE32: Result := 'NRV2B LE32';
    M_NRV2B_8: Result := 'NRV2B 8';
    M_NRV2B_LE16: Result := 'NRV2B LE16';
    M_NRV2D_LE32: Result := 'NRV2D LE32';
    M_NRV2D_8: Result := 'NRV2D 8';
    M_NRV2D_LE16: Result := 'NRV2D LE16';
    M_NRV2E_LE32: Result := 'NRV2E LE32';
    M_NRV2E_8: Result := 'NRV2E 8';
    M_NRV2E_LE16: Result := 'NRV2E LE16';
    M_CL1B_LE32: Result := 'CL1B LE32';
    M_CL1B_8: Result := 'CL1B 8';
    M_CL1B_LE16: Result := 'CL1B LE16';
    M_LZMA: Result := 'LZMA';
    M_DEFLATE: Result := 'DEFLATE (zlib)';
  else
    Result := UnknownMethod;
  end;
end;


end.
