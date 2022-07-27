unit JPL.Binary.Pe;

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$ENDIF}


interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes,
  {$IFDEF FPC}JwaWinNT,{$ENDIF}
  {$IFDEF MSWINDOWS}JPL.Win.VersionInfo,{$ENDIF}
  JPL.Strings, JPL.Files,
  JPL.Binary.Types, JPL.UPX, JPL.Conversion
  ;

{
  Useful links:

  https://docs.microsoft.com/en-us/windows/win32/debug/pe-format
  https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#coff-file-header-object-and-image
}


const
  {$region '   CONST   '}


  IMAGE_DOS_SIGNATURE =    $5A4D; // MZ
  IMAGE_OS2_SIGNATURE =    $454E; // NE
  IMAGE_VXD_SIGNATURE =    $454C; // LE

  IMAGE_FILE_DLL = $2000;  { File is a DLL. }
  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;  { File is executable  (i.e. no unresolved externel references). }

  DEFAULT_MAX_MEMORY_STREAM_SIZE = 100 * 1024 * 1024; // 100 MB

  SIGNATURE_PE = $00004550;
  SIGNATURE_NE = $0A05454E;

  OPTIONAL_HEADER_MAGIC_PE32 = $010B;      // 32-bit
  OPTIONAL_HEADER_MAGIC_PE32PLUS = $020B;  // 64-bit




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
  IMAGE_FILE_MACHINE_I486      = $14D;  // Intel 486
  IMAGE_FILE_MACHINE_I586      = $14E;  // Intel 586
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
  IMAGE_FILE_MACHINE_RISCV32   = $5032; // RISC-V 32-bit address space
  IMAGE_FILE_MACHINE_RISCV64   = $5064; // RISC-V 64-bit address space
  IMAGE_FILE_MACHINE_RISCV128  = $5128; // RISC-V 128-bit address space
    {$endregion}

  //MAGIC_PE32 = $10b;
  //MAGIC_PE32PLUS = $20b;

type

  TPeInfoItem = record
    FieldName: string;
    TypeName: string;
    Bytes: Byte;
    Desc: string;
  end;


const
   {$IFDEF FPC}IMAGE_FILE_AGGRESIVE_WS_TRIM = $0010;{$ENDIF}
   IMAGE_FILE_AGGRESSIVE_WS_TRIM = IMAGE_FILE_AGGRESIVE_WS_TRIM;


    {$region '   CONST - DOS Header   '}

  DosHeaderFieldCount = 31;

  DosHeaderInfo: array[0..DosHeaderFieldCount - 1] of TPeInfoItem = (
    (FieldName: 'e_magic';    TypeName: 'WORD';  Bytes: 2;   Desc: 'Magic number'),
    (FieldName: 'e_cblp';     TypeName: 'WORD';  Bytes: 2;   Desc: 'Bytes on last page of file'),
    (FieldName: 'e_cp';       TypeName: 'WORD';  Bytes: 2;   Desc: 'Pages in file'),
    (FieldName: 'e_crlc';     TypeName: 'WORD';  Bytes: 2;   Desc: 'Relocations'),
    (FieldName: 'e_cparhdr';  TypeName: 'WORD';  Bytes: 2;   Desc: 'Size of header in paragraphs'),
    (FieldName: 'e_minalloc'; TypeName: 'WORD';  Bytes: 2;   Desc: 'Minimum extra paragraphs needed'),
    (FieldName: 'e_maxalloc'; TypeName: 'WORD';  Bytes: 2;   Desc: 'Maximum extra paragraphs needed'),
    (FieldName: 'e_ss';       TypeName: 'WORD';  Bytes: 2;   Desc: 'Initial (relative) SS value'),
    (FieldName: 'e_sp';       TypeName: 'WORD';  Bytes: 2;   Desc: 'Initial SP value'),
    (FieldName: 'e_csum';     TypeName: 'WORD';  Bytes: 2;   Desc: 'Checksum'),
    (FieldName: 'e_ip';       TypeName: 'WORD';  Bytes: 2;   Desc: 'Initial IP value'),
    (FieldName: 'e_cs';       TypeName: 'WORD';  Bytes: 2;   Desc: 'Initial (relative) CS value'),
    (FieldName: 'e_lfarlc';   TypeName: 'WORD';  Bytes: 2;   Desc: 'File address of relocation table'),
    (FieldName: 'e_ovno';     TypeName: 'WORD';  Bytes: 2;   Desc: 'Overlay number'),
    (FieldName: 'e_res[0]';   TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res[1]';   TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res[2]';   TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res[3]';   TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_oemid';    TypeName: 'WORD';  Bytes: 2;   Desc: 'OEM identifier (for e_oeminfo)'),
    (FieldName: 'e_oeminfo';  TypeName: 'WORD';  Bytes: 2;   Desc: 'OEM information (e_oemid specific)'),
    (FieldName: 'e_res2[0]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[1]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[2]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[3]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[4]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[5]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[6]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[7]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[8]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_res2[9]';  TypeName: 'WORD';  Bytes: 2;   Desc: '(Reserved)'),
    (FieldName: 'e_lfanew';   TypeName: 'LONG';  Bytes: 4;   Desc: 'File address of PE header')
  );

    {$endregion CONST - DOS Header}


    {$region '   CONST - File (COFF) Header   '}

  CoffHeaderFieldCount = 7;

  CoffHeaderInfo: array[0..CoffHeaderFieldCount - 1] of TPeInfoItem = (
    (FieldName: 'Machine';               TypeName: 'WORD';   Bytes: 2;  Desc: 'Number identifying type of target machine'),
    (FieldName: 'NumberOfSections';      TypeName: 'WORD';   Bytes: 2;  Desc: 'Number of sections'),
    (FieldName: 'TimeDateStamp';         TypeName: 'DWORD';  Bytes: 4;  Desc: 'Time and date the file was created'),
    (FieldName: 'PointerToSymbolTable';  TypeName: 'DWORD';  Bytes: 4;  Desc: 'File offset of the COFF symbol table or 0 if none is present'),
    (FieldName: 'NumberOfSymbols';       TypeName: 'DWORD';  Bytes: 4;  Desc: 'Number of entries in the symbol table'),
    (FieldName: 'SizeOfOptionalHeader';  TypeName: 'WORD';   Bytes: 2;  Desc: 'Size of the optional header'),
    (FieldName: 'Characteristics';       TypeName: 'WORD';   Bytes: 2;  Desc: 'Flags indicating attributes of the file')
  );

    {$endregion CONST - File (COFF) Header}


    {$region '   CONST - Optional (PE) Header   '}

  OptionalHeaderFieldCount32 = 46 - IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
  OptionalHeaderFieldCount64 = 45 - IMAGE_NUMBEROF_DIRECTORY_ENTRIES;

  OptionalHeaderInfo32: array[0..OptionalHeaderFieldCount32 - 1] of TPeInfoItem = (
    // ----- Standard fields -----
    (FieldName: 'Magic';                        TypeName: 'WORD';     Bytes: 2;  Desc: 'Magic number'),
    (FieldName: 'MajorLinkerVersion';           TypeName: 'BYTE';     Bytes: 1;  Desc: 'Linker major version number'),
    (FieldName: 'MinorLinkerVersion';           TypeName: 'BYTE';     Bytes: 1;  Desc: 'Linker minor version number'),
    (FieldName: 'SizeOfCode';                   TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of CODE section, or the sum of all CODE sections'),
    (FieldName: 'SizeOfInitializedData';        TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of Initialized Data section, or the sum of all such sections'),
    (FieldName: 'SizeOfUninitializedData';      TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of Uninitialized Data section (BSS), or the sum of all such sections'),
    (FieldName: 'AddressOfEntryPoint';          TypeName: 'DWORD';    Bytes: 4;  Desc: 'Address of entry point, relative to image base'),
    (FieldName: 'BaseOfCode';                   TypeName: 'DWORD';    Bytes: 4;  Desc: 'Address of beginning of CODE section, relative to image base'),
    (FieldName: 'BaseOfData';                   TypeName: 'DWORD';    Bytes: 4;  Desc: 'Address of beginning of DATA section, relative to image base'),

    // ----- NT additional fields -----
    (FieldName: 'ImageBase';                    TypeName: 'DWORD';    Bytes: 4;  Desc: 'Preferred address of first byte of image when loades into memory'),
    (FieldName: 'SectionAlignment';             TypeName: 'DWORD';    Bytes: 4;  Desc: 'Alignment of sections when loaded into memory (in bytes)'),
    (FieldName: 'FileAlignment';                TypeName: 'DWORD';    Bytes: 4;  Desc: 'Alignment factor used to align the raw data of sections in the image file (in bytes)'),
    (FieldName: 'MajorOperatingSystemVersion';  TypeName: 'WORD';     Bytes: 2;  Desc: 'Major version number of required OS'),
    (FieldName: 'MinorOperatingSystemVersion';  TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of required OS'),
    (FieldName: 'MajorImageVersion';            TypeName: 'WORD';     Bytes: 2;  Desc: 'Major version number of image'),
    (FieldName: 'MinorImageVersion';            TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of image'),
    (FieldName: 'MajorSubsystemVersion';        TypeName: 'WORD';     Bytes: 2;  Desc: 'Major version number of subsystem'),
    (FieldName: 'MinorSubsystemVersion';        TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of subsystem'),
    (FieldName: 'Win32VersionValue';            TypeName: 'DWORD';    Bytes: 4;  Desc: '(Reserved)'),
    (FieldName: 'SizeOfImage';                  TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of image (in bytes)'),
    (FieldName: 'SizeOfHeaders';                TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of MS-DOS stub, PE Header, and section headers rounded to a multiple of FileAlignment'),
    (FieldName: 'CheckSum';                     TypeName: 'DWORD';    Bytes: 4;  Desc: 'Image file checksum'),
    (FieldName: 'Subsystem';                    TypeName: 'WORD';     Bytes: 2;  Desc: 'Subsystem required to run the image'),
    (FieldName: 'DllCharacteristics';           TypeName: 'WORD';     Bytes: 2;  Desc: 'DLL characteristics'),
    (FieldName: 'SizeOfStackReserve';           TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of stack to reserve'),
    (FieldName: 'SizeOfStackCommit';            TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of stack to commit'),
    (FieldName: 'SizeOfHeapReserve';            TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of local heap space to reserve'),
    (FieldName: 'SizeOfHeapCommit';             TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of local heap space to commit'),
    (FieldName: 'LoaderFlags';                  TypeName: 'DWORD';    Bytes: 4;  Desc: '(Obsolete)'),
    (FieldName: 'NumberOfRvaAndSizes';          TypeName: 'DWORD';    Bytes: 4;  Desc: 'Number of data-dictionary entries')
//    (FieldName: 'DataDirectory [0]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [1]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [2]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [3]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [4]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [5]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [6]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [7]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [8]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [9]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [10]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [11]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [12]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [13]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [14]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [15]';           TypeName: 'pointer';  Desc: '')
  );

  OptionalHeaderInfo64: array[0..OptionalHeaderFieldCount64 - 1] of TPeInfoItem = (
    // ----- Standard fields -----
    (FieldName: 'Magic';                        TypeName: 'WORD';     Bytes: 2 ; Desc: 'Magic number'),
    (FieldName: 'MajorLinkerVersion';           TypeName: 'BYTE';     Bytes: 1;  Desc: 'Linker major version number'),
    (FieldName: 'MinorLinkerVersion';           TypeName: 'BYTE';     Bytes: 1;  Desc: 'Linker minor version number'),
    (FieldName: 'SizeOfCode';                   TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of CODE section, or the sum of all CODE sections'),
    (FieldName: 'SizeOfInitializedData';        TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of Initialized Data section, or the sum of all such sections'),
    (FieldName: 'SizeOfUninitializedData';      TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of Uninitialized Data section (BSS), or the sum of all such sections'),
    (FieldName: 'AddressOfEntryPoint';          TypeName: 'DWORD';    Bytes: 4;  Desc: 'Address of entry point, relative to image base'),
    (FieldName: 'BaseOfCode';                   TypeName: 'DWORD';    Bytes: 4;  Desc: 'Address of beginning of CODE section, relative to image base'),
    //(FieldName: 'BaseOfData';                   TypeName: 'DWORD';    Desc: 'Address of beginning of DATA section, relative to image base'),

    // ----- NT additional fields -----
    (FieldName: 'ImageBase';                    TypeName: 'UInt64';   Bytes: 8;  Desc: 'Preferred address of first byte of image when loades into memory'),
    (FieldName: 'SectionAlignment';             TypeName: 'DWORD';    Bytes: 4;  Desc: 'Alignment of sections when loaded into memory (in bytes)'),
    (FieldName: 'FileAlignment';                TypeName: 'DWORD';    Bytes: 4;  Desc: 'Alignment factor used to align the raw data of sections in the image file (in bytes)'),
    (FieldName: 'MajorOperatingSystemVersion';  TypeName: 'WORD';     Bytes: 2; Desc: 'Major version number of required OS'),
    (FieldName: 'MinorOperatingSystemVersion';  TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of required OS'),
    (FieldName: 'MajorImageVersion';            TypeName: 'WORD';     Bytes: 2;  Desc: 'Major version number of image'),
    (FieldName: 'MinorImageVersion';            TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of image'),
    (FieldName: 'MajorSubsystemVersion';        TypeName: 'WORD';     Bytes: 2;  Desc: 'Major version number of subsystem'),
    (FieldName: 'MinorSubsystemVersion';        TypeName: 'WORD';     Bytes: 2;  Desc: 'Minor version number of subsystem'),
    (FieldName: 'Win32VersionValue';            TypeName: 'DWORD';    Bytes: 4;  Desc: '(Reserved)'),
    (FieldName: 'SizeOfImage';                  TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of image (in bytes)'),
    (FieldName: 'SizeOfHeaders';                TypeName: 'DWORD';    Bytes: 4;  Desc: 'Size of MS-DOS stub, PE Header, and section headers rounded to a multiple of FileAlignment'),
    (FieldName: 'CheckSum';                     TypeName: 'DWORD';    Bytes: 4;  Desc: 'Image file checksum'),
    (FieldName: 'Subsystem';                    TypeName: 'WORD';     Bytes: 2;  Desc: 'Subsystem required to run the image'),
    (FieldName: 'DllCharacteristics';           TypeName: 'WORD';     Bytes: 2;  Desc: 'DLL characteristics'),

    (FieldName: 'SizeOfStackReserve';           TypeName: 'UInt64';   Bytes: 8;  Desc: 'Size of stack to reserve'),
    (FieldName: 'SizeOfStackCommit';            TypeName: 'UInt64';   Bytes: 8;  Desc: 'Size of stack to commit'),
    (FieldName: 'SizeOfHeapReserve';            TypeName: 'UInt64';   Bytes: 8;  Desc: 'Size of local heap space to reserve'),
    (FieldName: 'SizeOfHeapCommit';             TypeName: 'UInt64';   Bytes: 8;  Desc: 'Size of local heap space to commit'),

    (FieldName: 'LoaderFlags';                  TypeName: 'DWORD';    Bytes: 4;  Desc: '(Obsolete)'),
    (FieldName: 'NumberOfRvaAndSizes';          TypeName: 'DWORD';    Bytes: 4;  Desc: 'Number of data-dictionary entries')
//    (FieldName: 'DataDirectory [0]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [1]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [2]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [3]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [4]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [5]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [6]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [7]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [8]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [9]';            TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [10]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [11]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [12]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [13]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [14]';           TypeName: 'pointer';  Desc: ''),
//    (FieldName: 'DataDirectory [15]';           TypeName: 'pointer';  Desc: '')
  );


    {$endregion CONST - Optional (PE) Header}



  ResourceDataEntryFieldsNames: array[0..3] of string = ('OffsetToData', 'Size', 'CodePage', 'Reserved');

  ResourceDataEntryTypesNames: array[0..3] of string = ('DWORD', 'DWORD', 'DWORD', 'DWORD');

  ResourceDataEntryFieldsDesc: array[0..3] of string =
  (
    'Address of a unit of resource data in the Resource Data area',
    'Size of the resource data pointed to by the OffsetToData',
    'Code Page used to decode code point values within the resource data',
    '(Reserved)'
  );


  PeDataDirectoryCount = IMAGE_NUMBEROF_DIRECTORY_ENTRIES; // 16
  PeDataDirectoryNames: array[0..PeDataDirectoryCount - 1] of string =
  (
    'Export', 'Import', 'Resource', 'Exception', 'Security', 'Base Reloc',
    'Debug', 'Copyright', 'Global Ptr', 'TLS', 'Load Config', 'Bound Import',
    'IAT', 'COM', 'Delay Import', '(reserved)'
  );

  {$endregion CONST}




  {$region '   COFF Machine   '}


type

  TCoffMachineInfoItem = record
    ConstantName: string;
    Machine: Word;
    Desc: string;
  end;

const

  CoffMachineInfoArray: array[0..35] of TCoffMachineInfoItem = (
    (ConstantName: 'IMAGE_FILE_MACHINE_UNKNOWN'; Machine: IMAGE_FILE_MACHINE_UNKNOWN;   Desc: 'Unknown machine'),
    (ConstantName: 'IMAGE_FILE_MACHINE_I386';    Machine: IMAGE_FILE_MACHINE_I386;   Desc: 'Intel 386 or later processors and compatible processors'),
    (ConstantName: 'IMAGE_FILE_MACHINE_I486';    Machine: IMAGE_FILE_MACHINE_I486;   Desc: 'Intel 486'),
    (ConstantName: 'IMAGE_FILE_MACHINE_I586';    Machine: IMAGE_FILE_MACHINE_I586;   Desc: 'Intel 586'),
    (ConstantName: 'IMAGE_FILE_MACHINE_R3000';    Machine: IMAGE_FILE_MACHINE_R3000;   Desc: 'MIPS little endian R3000'),

    (ConstantName: '0x0160';    Machine: $0160;   Desc: 'MIPS big-endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_R4000';    Machine: IMAGE_FILE_MACHINE_R4000;   Desc: 'MIPS little endian R4000'),
    (ConstantName: 'IMAGE_FILE_MACHINE_R10000';    Machine: IMAGE_FILE_MACHINE_R10000;   Desc: 'MIPS little endian R10000'),
    (ConstantName: 'IMAGE_FILE_MACHINE_WCEMIPSV2';    Machine: IMAGE_FILE_MACHINE_WCEMIPSV2;   Desc: 'MIPS little endian WCE v2'),
    (ConstantName: 'IMAGE_FILE_MACHINE_RISCV32';    Machine: IMAGE_FILE_MACHINE_RISCV32;   Desc: 'RISC-V 32-bit address space'),
    (ConstantName: 'IMAGE_FILE_MACHINE_RISCV64';    Machine: IMAGE_FILE_MACHINE_RISCV64;   Desc: 'RISC-V 64-bit address space'),
    (ConstantName: 'IMAGE_FILE_MACHINE_RISCV128';    Machine: IMAGE_FILE_MACHINE_RISCV128;   Desc: 'RISC-V 128-bit address space'),
    (ConstantName: 'IMAGE_FILE_MACHINE_ALPHA';    Machine: IMAGE_FILE_MACHINE_ALPHA;   Desc: 'Alpha_AXP'),
    (ConstantName: 'IMAGE_FILE_MACHINE_SH3';    Machine: IMAGE_FILE_MACHINE_SH3;   Desc: 'Hitachi SH3 little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_SH3DSP';    Machine: IMAGE_FILE_MACHINE_SH3DSP;   Desc: 'Hitachi SH3 DSP'),
    (ConstantName: 'IMAGE_FILE_MACHINE_SH3E';    Machine: IMAGE_FILE_MACHINE_SH3E;   Desc: 'SH3E little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_SH4';    Machine: IMAGE_FILE_MACHINE_SH4;   Desc: 'Hitachi SH4 little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_SH5';    Machine: IMAGE_FILE_MACHINE_SH5;   Desc: 'Hitachi SH5'),

    (ConstantName: 'IMAGE_FILE_MACHINE_ARM';    Machine: IMAGE_FILE_MACHINE_ARM;   Desc: 'ARM little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_ARM64';    Machine: IMAGE_FILE_MACHINE_ARM64;   Desc: 'ARM64 little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_ARMNT';    Machine: IMAGE_FILE_MACHINE_ARMNT;   Desc: 'ARM Thumb-2 little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_THUMB';    Machine: IMAGE_FILE_MACHINE_THUMB;   Desc: 'ARM or Thumb'),

    (ConstantName: 'IMAGE_FILE_MACHINE_AM33';    Machine: IMAGE_FILE_MACHINE_AM33;   Desc: 'Matsushita AM33'),
    (ConstantName: 'IMAGE_FILE_MACHINE_POWERPC';    Machine: IMAGE_FILE_MACHINE_POWERPC;   Desc: 'IBM PowerPC little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_POWERPCFP';    Machine: IMAGE_FILE_MACHINE_POWERPCFP;   Desc: 'IBM PowerPC with floating point support'),
    (ConstantName: 'IMAGE_FILE_MACHINE_IA64';    Machine: IMAGE_FILE_MACHINE_IA64;   Desc: 'Intel Itanium 64 processor family'),
    (ConstantName: 'IMAGE_FILE_MACHINE_MIPS16';    Machine: IMAGE_FILE_MACHINE_MIPS16;   Desc: 'MIPS16'),
    (ConstantName: 'IMAGE_FILE_MACHINE_ALPHA64';    Machine: IMAGE_FILE_MACHINE_ALPHA64;   Desc: 'ALPHA64'),
    (ConstantName: 'IMAGE_FILE_MACHINE_MIPSFPU';    Machine: IMAGE_FILE_MACHINE_MIPSFPU;   Desc: 'MIPS with FPU'),
    (ConstantName: 'IMAGE_FILE_MACHINE_MIPSFPU16';    Machine: IMAGE_FILE_MACHINE_MIPSFPU16;   Desc: 'MIPS16 with FPU'),
    (ConstantName: 'IMAGE_FILE_MACHINE_TRICORE';    Machine: IMAGE_FILE_MACHINE_TRICORE;   Desc: 'Infineon'),
    (ConstantName: 'IMAGE_FILE_MACHINE_CEF';    Machine: IMAGE_FILE_MACHINE_CEF;   Desc: 'CEF'),
    (ConstantName: 'IMAGE_FILE_MACHINE_EBC';    Machine: IMAGE_FILE_MACHINE_EBC;   Desc: 'EFI byte code'),
    (ConstantName: 'IMAGE_FILE_MACHINE_AMD64';    Machine: IMAGE_FILE_MACHINE_AMD64;   Desc: 'AMD64 (x64)'),
    (ConstantName: 'IMAGE_FILE_MACHINE_M32R';    Machine: IMAGE_FILE_MACHINE_M32R;   Desc: 'Mitsubishi M32R little endian'),
    (ConstantName: 'IMAGE_FILE_MACHINE_CEE';    Machine: IMAGE_FILE_MACHINE_CEE;   Desc: 'CEE')
  );



type
  // https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#machine-types

  // How to use:
  //   Set Machine property than read Description and MachineConstantName
  TCoffMachine = record
  strict private
    class var FMachine: Word;
    class procedure SetMachine(const Value: Word); static;
    class function GetDescription: string; static;
    class function GetMachineConstantName: string; static;
  public
    class function IsIntel386Machine: Boolean; static;
    class function IsIntel486Machine: Boolean; static;
    class function IsIntel586Machine: Boolean; static;
    class function IsAmd64Machine: Boolean; static;
    class function InfoURL: string; static;

    class property Machine: Word read FMachine write SetMachine;
    class property Description: string read GetDescription;
    class property MachineConstantName: string read GetMachineConstantName;
  end;
  {$endregion COFF Machine}



  {$region '   COFF Flags (Characteristics)   '}

type

  TCoffFlagItem = record
    FlagName: string;
    Flag: Word;
    Desc: string;
  end;


const
  CoffCharacteristicsInfoArray: array[0..15] of TCoffFlagItem = (
    (FlagName: 'IMAGE_FILE_RELOCS_STRIPPED';       Flag: IMAGE_FILE_RELOCS_STRIPPED;    Desc: 'Relocation info stripped from file'),
    (FlagName: 'IMAGE_FILE_EXECUTABLE_IMAGE';      Flag: IMAGE_FILE_EXECUTABLE_IMAGE;   Desc: 'File is executable'),
    (FlagName: 'IMAGE_FILE_LINE_NUMS_STRIPPED';    Flag: IMAGE_FILE_LINE_NUMS_STRIPPED;   Desc: 'COFF line numbers have been removed'), //. This flag is deprecated and should be zero.'),
    (FlagName: 'IMAGE_FILE_LOCAL_SYMS_STRIPPED';   Flag: IMAGE_FILE_LOCAL_SYMS_STRIPPED;   Desc: 'COFF symbol table entries for local symbols have been removed'), //. This flag is deprecated and should be zero.'),
    (FlagName: 'IMAGE_FILE_AGGRESSIVE_WS_TRIM';    Flag: IMAGE_FILE_AGGRESSIVE_WS_TRIM;   Desc: 'Obsolete. Aggressively trim working set.'), // This flag is deprecated for Windows 2000 and later and must be zero.'),
    (FlagName: 'IMAGE_FILE_LARGE_ADDRESS_AWARE';   Flag: IMAGE_FILE_LARGE_ADDRESS_AWARE;   Desc: 'Application can handle > 2-GB addresses.'),
    (FlagName: '0x0040';                           Flag: $0040;   Desc: 'This flag is reserved for future use.'),
    (FlagName: 'IMAGE_FILE_BYTES_REVERSED_LO';     Flag: IMAGE_FILE_BYTES_REVERSED_LO;   Desc: 'Little endian: the least significant bit (LSB) precedes the most significant bit (MSB) in memory.'), // This flag is deprecated and should be zero.'),
    (FlagName: 'IMAGE_FILE_32BIT_MACHINE';         Flag: IMAGE_FILE_32BIT_MACHINE;   Desc: 'Machine is based on a 32-bit-word architecture.'),
    (FlagName: 'IMAGE_FILE_DEBUG_STRIPPED';        Flag: IMAGE_FILE_DEBUG_STRIPPED;   Desc: 'Debugging information is removed from the image file.'),
    (FlagName: 'IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP';   Flag: IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP;   Desc: 'If the image is on removable media, fully load it and copy it to the swap file.'),
    (FlagName: 'IMAGE_FILE_NET_RUN_FROM_SWAP';         Flag: IMAGE_FILE_NET_RUN_FROM_SWAP;   Desc: 'If the image is on network media, fully load it and copy it to the swap file.'),
    (FlagName: 'IMAGE_FILE_SYSTEM';             Flag: IMAGE_FILE_SYSTEM;   Desc: 'The image file is a system file, not a user program.'),
    (FlagName: 'IMAGE_FILE_DLL';                Flag: IMAGE_FILE_DLL;   Desc: 'The image file is a dynamic-link library (DLL). Such files are considered executable files for almost all purposes, although they cannot be directly run.'),
    (FlagName: 'IMAGE_FILE_UP_SYSTEM_ONLY';     Flag: IMAGE_FILE_UP_SYSTEM_ONLY;   Desc: 'The file should be run only on a uniprocessor machine.'),
    (FlagName: 'IMAGE_FILE_BYTES_REVERSED_HI';  Flag: IMAGE_FILE_BYTES_REVERSED_HI;   Desc: 'Big endian: the MSB precedes the LSB in memory.') // This flag is deprecated and should be zero.')
  );


type

  // https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#characteristics
  TCoffCharacteristics = record
  strict private
    class var FSeparatorIsSet: Boolean;
    class var FCharacteristics: Word;
    class var FFlagNameDescSeparator: string;
    class procedure SetCharacteristics(const Value: Word); static;
    class function GetDesc(bIncludeFlagNames: Boolean): string; static;
    class function GetDescription: string; static;
    class function GetDescriptionWithFlagNames: string; static;
    class procedure SetFlagNameDescSeparator(const Value: string); static;
  public
    class function InfoURL: string; static;
    class property FlagNameDescSeparator: string read FFlagNameDescSeparator write SetFlagNameDescSeparator;
    class property Characteristics: Word read FCharacteristics write SetCharacteristics;
    class property Description: string read GetDescription;
    class property DescriptionWithFlagNames: string read GetDescriptionWithFlagNames;
  end;

  {$endregion COFF Flags (Characteristics)}



  {$region '   PE Section Flags (Characteristics)   '}
type

  TPeSectionFlagItem = record
    FlagName: string;
    Flag: DWORD;
    Desc: string;
  end;

const
  // https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#section-flags

  IMAGE_SCN_GPREL = $00008000;
  IMAGE_SCN_ALIGN_128BYTES = $00800000;
  IMAGE_SCN_ALIGN_256BYTES = $00900000;
  IMAGE_SCN_ALIGN_512BYTES = $00A00000;
  IMAGE_SCN_ALIGN_1024BYTES = $00B00000;
  IMAGE_SCN_ALIGN_2048BYTES = $00C00000;
  IMAGE_SCN_ALIGN_4096BYTES = $00D00000;
  IMAGE_SCN_ALIGN_8192BYTES = $00E00000;

  PeSectionFlagsArray: array[0..40] of TPeSectionFlagItem = (
    (FlagName: '0x00000000';              Flag: $00000000;    Desc: 'Reserved for future use (0x00000000).'),
    (FlagName: '0x00000001';              Flag: $00000001;    Desc: 'Reserved for future use (0x00000001).'),
    (FlagName: '0x00000002';              Flag: $00000002;    Desc: 'Reserved for future use (0x00000002).'),
    (FlagName: '0x00000004';              Flag: $00000004;    Desc: 'Reserved for future use (0x00000004).'),
    (FlagName: 'IMAGE_SCN_TYPE_NO_PAD';   Flag: IMAGE_SCN_TYPE_NO_PAD;     Desc: 'The section should not be padded to the next boundary. This flag is obsolete and is replaced by IMAGE_SCN_ALIGN_1BYTES. This is valid only for object files.'),
    (FlagName: '0x00000010';              Flag: $00000010;                 Desc: 'Reserved for future use (0x00000010).'),
    (FlagName: 'IMAGE_SCN_CNT_CODE';      Flag: IMAGE_SCN_CNT_CODE;        Desc: 'The section contains executable code.'),
    (FlagName: 'IMAGE_SCN_CNT_INITIALIZED_DATA';    Flag: IMAGE_SCN_CNT_INITIALIZED_DATA;      Desc: 'The section contains initialized data.'),
    (FlagName: 'IMAGE_SCN_CNT_UNINITIALIZED_DATA';  Flag: IMAGE_SCN_CNT_UNINITIALIZED_DATA;    Desc: 'The section contains uninitialized data.'),
    (FlagName: 'IMAGE_SCN_LNK_OTHER';        Flag: IMAGE_SCN_LNK_OTHER;          Desc: 'Reserved for future use.'),
    (FlagName: 'IMAGE_SCN_LNK_INFO';         Flag: IMAGE_SCN_LNK_INFO;           Desc: 'The section contains comments or other information. The .drectve section has this type. This is valid for object files only.'),
    (FlagName: '0x00000400';                 Flag: $00000400;                    Desc: 'Reserved for future use (0x00000400).'),
    (FlagName: 'IMAGE_SCN_LNK_REMOVE';       Flag: IMAGE_SCN_LNK_REMOVE;         Desc: 'The section will not become part of the image. This is valid only for object files.'),
    (FlagName: 'IMAGE_SCN_LNK_COMDAT';       Flag: IMAGE_SCN_LNK_COMDAT;         Desc: 'The section contains COMDAT data. This is valid only for object files.'),
    (FlagName: 'IMAGE_SCN_GPREL';            Flag: IMAGE_SCN_GPREL;              Desc: 'The section contains data referenced through the global pointer (GP).'),
    (FlagName: 'IMAGE_SCN_MEM_PURGEABLE';    Flag: IMAGE_SCN_MEM_PURGEABLE;      Desc: 'Reserved for future use.'),
    (FlagName: 'IMAGE_SCN_MEM_16BIT';        Flag: IMAGE_SCN_MEM_16BIT;          Desc: 'Reserved for future use.'),
    (FlagName: 'IMAGE_SCN_MEM_LOCKED';       Flag: IMAGE_SCN_MEM_LOCKED;         Desc: 'Reserved for future use.'),
    (FlagName: 'IMAGE_SCN_MEM_PRELOAD';      Flag: IMAGE_SCN_MEM_PRELOAD;        Desc: 'Reserved for future use.'),
    (FlagName: 'IMAGE_SCN_ALIGN_1BYTES';     Flag: IMAGE_SCN_ALIGN_1BYTES;       Desc: 'Align data on a 1-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_2BYTES';     Flag: IMAGE_SCN_ALIGN_2BYTES;       Desc: 'Align data on a 2-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_4BYTES';     Flag: IMAGE_SCN_ALIGN_4BYTES;       Desc: 'Align data on a 4-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_8BYTES';     Flag: IMAGE_SCN_ALIGN_8BYTES;       Desc: 'Align data on an 8-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_16BYTES';    Flag: IMAGE_SCN_ALIGN_16BYTES;      Desc: 'Align data on a 16-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_32BYTES';    Flag: IMAGE_SCN_ALIGN_32BYTES;      Desc: 'Align data on a 32-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_64BYTES';    Flag: IMAGE_SCN_ALIGN_64BYTES;      Desc: 'Align data on a 64-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_128BYTES';   Flag: IMAGE_SCN_ALIGN_128BYTES;     Desc: 'Align data on a 128-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_256BYTES';   Flag: IMAGE_SCN_ALIGN_256BYTES;     Desc: 'Align data on a 256-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_512BYTES';   Flag: IMAGE_SCN_ALIGN_512BYTES;     Desc: 'Align data on a 512-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_1024BYTES';  Flag: IMAGE_SCN_ALIGN_1024BYTES;    Desc: 'Align data on a 1024-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_2048BYTES';  Flag: IMAGE_SCN_ALIGN_2048BYTES;    Desc: 'Align data on a 2048-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_4096BYTES';  Flag: IMAGE_SCN_ALIGN_4096BYTES;    Desc: 'Align data on a 4096-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_ALIGN_8192BYTES';  Flag: IMAGE_SCN_ALIGN_8192BYTES;    Desc: 'Align data on an 8192-byte boundary. Valid only for object files.'),
    (FlagName: 'IMAGE_SCN_LNK_NRELOC_OVFL';  Flag: IMAGE_SCN_LNK_NRELOC_OVFL;    Desc: 'The section contains extended relocations.'),
    (FlagName: 'IMAGE_SCN_MEM_DISCARDABLE';  Flag: IMAGE_SCN_MEM_DISCARDABLE;    Desc: 'The section can be discarded as needed.'),
    (FlagName: 'IMAGE_SCN_MEM_NOT_CACHED';   Flag: IMAGE_SCN_MEM_NOT_CACHED;     Desc: 'The section cannot be cached.'),
    (FlagName: 'IMAGE_SCN_MEM_NOT_PAGED';    Flag: IMAGE_SCN_MEM_NOT_PAGED;      Desc: 'The section is not pageable.'),
    (FlagName: 'IMAGE_SCN_MEM_SHARED';       Flag: IMAGE_SCN_MEM_SHARED;         Desc: 'The section can be shared in memory.'),
    (FlagName: 'IMAGE_SCN_MEM_EXECUTE';      Flag: IMAGE_SCN_MEM_EXECUTE;        Desc: 'The section can be executed as code.'),
    (FlagName: 'IMAGE_SCN_MEM_READ';         Flag: IMAGE_SCN_MEM_READ;           Desc: 'The section can be read.'),
    (FlagName: 'IMAGE_SCN_MEM_WRITE';        Flag: IMAGE_SCN_MEM_WRITE;          Desc: 'The section can be written to.')
  );

type

  TPeSectionCharacteristics = record
  private
    class var FSeparatorIsSet: Boolean;
    class var FCharacteristics: DWORD;
    class var FFlagNameDescSeparator: string;
    class procedure SetCharacteristics(const Value: DWORD); static;
    class procedure SetFlagNameDescSeparator(const Value: string); static;
    class function GetDesc(bIncludeFlagNames: Boolean): string; static;
    class function GetDescription: string; static;
    class function GetDescriptionWithFlagNames: string; static;
  public
    class function InfoURL: string; static;
    class property FlagNameDescSeparator: string read FFlagNameDescSeparator write SetFlagNameDescSeparator;
    class property Characteristics: DWORD read FCharacteristics write SetCharacteristics;
    class property Description: string read GetDescription;
    class property DescriptionWithFlagNames: string read GetDescriptionWithFlagNames;
  end;

  {$endregion PE Section Flags (Characteristics)}


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
    FIs16bit: Boolean;
    FIs32bit: Boolean;
    FIs64bit: Boolean;
    FOptionalHeader32: TImageOptionalHeader32;
    FOptionalHeader64: TImageOptionalHeader64;
    FOptionalHeaderMagicStr: string;
    FInfoStr: string;
    FOffset_PeSignature: Int64;
    FOffset_FileHeader: Int64;
    FOffset_PeHeader: Int64;
    FOffset_SectionsTable: Int64;
    FImageBase: NativeUInt;
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

    FCreationTime: TDateTime;
    FLastWriteTime: TDateTime;
    FLastAccessTime: TDateTime;
    {$IFDEF MSWINDOWS}
    FAttributes: TFileAttributesRec;
    FHasFileVersion: Boolean;
    FFileVersion: TVIFileVersion;
    FHasVersionInfo: Boolean;
    FVersionInfo: TVIStringInfoItem;
    {$ENDIF}
    FAddressOfEntryPoint: DWORD;



    function GetDataDirectory(Index: integer): IMAGE_DATA_DIRECTORY;
    procedure SetFileName(const Value: string);
    procedure SetMaxMemoryStreamSize(const Value: integer);
    function GetDataStream: TStream;
    function GetDataDirectoryCount: integer;
    function GetSectionCount: integer;
    property DataStream: TStream read GetDataStream;
    procedure FreeStreams;
    procedure GetUpxInfo;
    procedure RaiseException(const Message: string);
    procedure ClearInfo;
    procedure GetFileInfo;
  public
    PeSignatureArray: array[0..3] of Byte;

    constructor Create; overload;
    constructor Create(const FileName: string); overload;
    destructor Destroy; override;

    procedure ReadFileInfo;

    property FileName: string read FFileName write SetFileName;
    property FileSize: Int64 read FFileSize;
    property IsValidPeFile: Boolean read FIsValidPeFile;

    property Bits: Byte read FBits;
    property Is16bit: Boolean read FIs16bit;
    property Is32bit: Boolean read FIs32bit;
    property Is64bit: Boolean read FIs64bit;

    property CreationTime: TDateTime read FCreationTime;
    property LastWriteTime: TDateTime read FLastWriteTime;
    property LastAccessTime: TDateTime read FLastAccessTime;

    property DosHeader: TImageDosHeader read FDosHeader;
    property PeSignature: LongWord read FPeSignature;
    property FileHeader: TImageFileHeader read FFileHeader;
    property OptionalHeader32: TImageOptionalHeader32 read FOptionalHeader32;
    property OptionalHeader64: TImageOptionalHeader64 read FOptionalHeader64;
    property OptionalHeaderMagicStr: string read FOptionalHeaderMagicStr;

    property Offset_PeSignature: Int64 read FOffset_PeSignature;
    property Offset_FileHeader: Int64 read FOffset_FileHeader;
    property Offset_PeHeader: Int64 read FOffset_PeHeader;
    property Offset_SectionsTable: Int64 read FOffset_SectionsTable;

    property ImageBase: NativeUInt read FImageBase;
    property AddressOfEntryPoint: DWORD read FAddressOfEntryPoint;

    property SectionNames: TStringList read FSectionNames;
    property Sections: TImageSections read FSections;
    property SectionCount: integer read GetSectionCount;

    property DataDirectoryCount: integer read GetDataDirectoryCount;
    property DataDirectory[Index: integer]: IMAGE_DATA_DIRECTORY read GetDataDirectory;

    property MaxMemoryStreamSize: integer read FMaxMemoryStreamSize write SetMaxMemoryStreamSize;
    property IsFileLoaded: Boolean read FIsFileLoaded;
    property FirstFile4Bits: TBitsInfo read FFirstFile4Bits;

    property IsPackedByUpx: Boolean read FIsPackedByUpx;
    property IsValidUpxHeader: Boolean read FIsValidUpxHeader;
    property UpxHeader: TPeUpxHeader read FUpxHeader;
    property UpxVersion: string read FUpxVersion;
    property Offset_UpxHeader: Int64 read FOffset_UpxHeader;

    {$IFDEF MSWINDOWS}
    property Attributes: TFileAttributesRec read FAttributes;
    property HasFileVersion: Boolean read FHasFileVersion;
    property FileVersion: TVIFileVersion read FFileVersion;
    property HasVersionInfo: Boolean read FHasVersionInfo; // Has English TVIStringInfoItem
    property VersionInfo: TVIStringInfoItem read FVersionInfo;
    {$ENDIF}

    property InfoStr: string read FInfoStr;
  end;
  {$endregion}




function CoffHeaderMachineToStr(const Machine: WORD): string;
procedure GetCoffHeaderCharacteristics(const Characteristics: WORD; var sl: TStrings);
function GetSectionCommonFlagsStr(const Characteristics: DWORD): string;
function ImageSignatureToStr(const Signature: WORD): string;
function CoffHeaderTimeStampToDateTime(const TimeStamp: DWORD): TDateTime;

function IsMachine64Bit(const Machine: Word): Boolean;
function GetStrFromBytes(AB: array of Byte): string;



implementation



{$region '                         TPeSectionCharacteristics                           '}

class function TPeSectionCharacteristics.GetDesc(bIncludeFlagNames: Boolean): string;
var
  i: integer;
  Sep: string;
  Item: TPeSectionFlagItem;
begin
  Result := '';
  if not FSeparatorIsSet then Sep := ' - ' else Sep := FFlagNameDescSeparator;

  for i := 0 to High(PeSectionFlagsArray) do
  begin
    Item := PeSectionFlagsArray[i];
    if (Item.Flag and FCharacteristics) > 0 then
      if bIncludeFlagNames then Result := Result + Item.FlagName + Sep + Item.Desc + ENDL
      else Result := Result + Item.Desc + ENDL;
  end;

  Result := Trim(Result);
end;

class function TPeSectionCharacteristics.GetDescription: string;
begin
  Result := GetDesc(False);
end;

class function TPeSectionCharacteristics.GetDescriptionWithFlagNames: string;
begin
  Result := GetDesc(True);
end;

class procedure TPeSectionCharacteristics.SetCharacteristics(const Value: DWORD);
begin
  FCharacteristics := Value;
end;

class procedure TPeSectionCharacteristics.SetFlagNameDescSeparator(const Value: string);
begin
  FFlagNameDescSeparator := Value;
  FSeparatorIsSet := True;
end;

class function TPeSectionCharacteristics.InfoURL: string;
begin
  Result := 'https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#section-flags';
end;

{$endregion TPeSectionCharacteristics}


{$region '                         TCoffCharacteristics                         '}

class procedure TCoffCharacteristics.SetFlagNameDescSeparator(const Value: string);
begin
  FFlagNameDescSeparator := Value;
  FSeparatorIsSet := True;
end;

class function TCoffCharacteristics.GetDesc(bIncludeFlagNames: Boolean): string;
var
  i: integer;
  Sep: string;
  Item: TCoffFlagItem;
begin
  Result := '';
  if not FSeparatorIsSet then Sep := ' - ' else Sep := FFlagNameDescSeparator;

  for i := 0 to High(CoffCharacteristicsInfoArray) do
  begin
    Item := CoffCharacteristicsInfoArray[i];
    if (Item.Flag and FCharacteristics) > 0 then
      if bIncludeFlagNames then Result := Result + Item.FlagName + Sep + Item.Desc + ENDL
      else Result := Result + Item.Desc + ENDL;
  end;

  Result := Trim(Result);
end;

class function TCoffCharacteristics.GetDescription: string;
begin
  Result := GetDesc(False);
end;

class function TCoffCharacteristics.GetDescriptionWithFlagNames: string;
begin
  Result := GetDesc(True);
end;

class function TCoffCharacteristics.InfoURL: string;
begin
  Result := 'https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#characteristics';
end;

class procedure TCoffCharacteristics.SetCharacteristics(const Value: Word);
begin
  FCharacteristics := Value;
end;



{$endregion TCoffCharacteristics}



{$region '                           TCoffMachine                             '}

class function TCoffMachine.GetDescription: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(CoffMachineInfoArray) do
    if CoffMachineInfoArray[i].Machine = FMachine then
    begin
      Result := CoffMachineInfoArray[i].Desc;
      Break;
    end;
end;

class function TCoffMachine.GetMachineConstantName: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(CoffMachineInfoArray) do
    if CoffMachineInfoArray[i].Machine = FMachine then
    begin
      Result := CoffMachineInfoArray[i].ConstantName;
      Break;
    end;
end;

class function TCoffMachine.InfoURL: string;
begin
  Result := 'https://docs.microsoft.com/en-us/windows/win32/debug/pe-format#machine-types';
end;

class function TCoffMachine.IsAmd64Machine: Boolean;
begin
  Result := FMachine = IMAGE_FILE_MACHINE_AMD64;
end;

class function TCoffMachine.IsIntel386Machine: Boolean;
begin
  Result := FMachine = IMAGE_FILE_MACHINE_I386;
end;

class function TCoffMachine.IsIntel486Machine: Boolean;
begin
  Result := FMachine = IMAGE_FILE_MACHINE_I486;
end;

class function TCoffMachine.IsIntel586Machine: Boolean;
begin
  Result := FMachine = IMAGE_FILE_MACHINE_I586;
end;

class procedure TCoffMachine.SetMachine(const Value: Word);
begin
  FMachine := Value;
end;

{$endregion TCoffMachine}




{$region '            Routines              '}

function CoffHeaderTimeStampToDateTime(const TimeStamp: DWORD): TDateTime;
// UnixDateDelta = 25569; // Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970)
begin
  Result := TimeStamp / SecsPerDay + UnixDateDelta;
end;

function ImageSignatureToStr(const Signature: WORD): string;
begin
  case Signature of
    IMAGE_DOS_SIGNATURE: Result := 'MZ';
    IMAGE_OS2_SIGNATURE: Result := 'NE';
    IMAGE_VXD_SIGNATURE: Result := 'LE';
  else
    Result := '';
  end;
end;

function GetSectionCommonFlagsStr(const Characteristics: DWORD): string;
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



function CoffHeaderMachineToStr(const Machine: WORD): string;
begin
  TCoffMachine.Machine := Machine;
  Result := TCoffMachine.Description;
end;

procedure GetCoffHeaderCharacteristics(const Characteristics: WORD; var sl: TStrings);
begin
  TCoffCharacteristics.Characteristics := Characteristics;
  sl.Text := TCoffCharacteristics.Description;
end;

function IsMachine64Bit(const Machine: Word): Boolean;
begin
  case Machine of
    IMAGE_FILE_MACHINE_ALPHA64, IMAGE_FILE_MACHINE_AMD64, IMAGE_FILE_MACHINE_ARM64, IMAGE_FILE_MACHINE_IA64: Result := True;
  else
    Result := False;
  end;
end;

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

function GetStrFromBytes(AB: array of Byte): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(AB) to High(AB) do Result := Result + Chr(AB[i]);
end;

{$endregion Routines}


{$region ' ---------------------------- TPeFile ------------------------------ '}
constructor TPeFile.Create;
begin
  FMaxMemoryStreamSize := DEFAULT_MAX_MEMORY_STREAM_SIZE;
  FSectionNames := TStringList.Create;
  SetLength(FSections, 0);
  fs := nil;
  ms := nil;
  ClearInfo;
end;

constructor TPeFile.Create(const FileName: string);
begin
  Create;
  FFileName := FileName;
  ReadFileInfo;
end;

destructor TPeFile.Destroy;
begin
  if Assigned(FSectionNames) then FSectionNames.Free;
  FreeStreams;
  inherited;
end;

procedure TPeFile.FreeStreams;
begin
  if Assigned(fs) then FreeAndNil(fs);
  if Assigned(ms) then FreeAndNil(ms);
  FIsFileLoaded := False;
end;

{$region '                       ClearInfo                         '}
procedure TPeFile.ClearInfo;
begin
  FIsValidPeFile := False;
  FillChar(FDosHeader, SizeOf(FDosHeader), 0);
  FillChar(FFileHeader, SizeOf(FFileHeader), 0);
  FillChar(FOptionalHeader32, SizeOf(FOptionalHeader32), 0);
  FillChar(FOptionalHeader64, SizeOf(FOptionalHeader64), 0);

  FPeSignature := 0;
  FOptionalHeaderMagicStr := '';
  FInfoStr := '';

  FOffset_PeSignature := BIN_INVALID_OFFSET;
  FOffset_FileHeader := BIN_INVALID_OFFSET;
  FOffset_PeHeader := BIN_INVALID_OFFSET;
  FOffset_SectionsTable := BIN_INVALID_OFFSET;

  FImageBase := 0;
  FAddressOfEntryPoint := 0;

  FIsFileLoaded := False;
  FFileSize := 0;
  FSectionNames.Clear;
  SetLength(FSections, 0);
  ClearBitsInfo(FFirstFile4Bits);

  FIsPackedByUpx := False;
  FIsValidUpxHeader := False;
  FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
  FUpxVersion := '';
  FOffset_UpxHeader := BIN_INVALID_OFFSET;

  FBits := 0;
  FIs16bit := False;
  FIs32bit := False;
  FIs64bit := False;

  FCreationTime := EMPTY_DATE;
  FLastWriteTime := EMPTY_DATE;
  FLastAccessTime := EMPTY_DATE;

  {$IFDEF MSWINDOWS}
  FAttributes.ValidAttributes := False;
  FHasFileVersion := False;
  FFileVersion.Clear;
  FHasVersionInfo := False;
  FVersionInfo.Clear;
  {$ENDIF}

  FreeStreams;
end;
{$endregion ClearInfo}

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


function TPeFile.GetDataDirectoryCount: integer;
begin
  if not FIsValidPeFile then Result := -1
  else Result := IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
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


{$region '                             GetFileInfo                           '}
procedure TPeFile.ReadFileInfo;
begin
  GetFileInfo;
end;

procedure TPeFile.GetFileInfo;
var
  x, i: integer;
  xw: Word;
  s: string;
  ish: TImageSectionHeader;
  fir: TFileInfoRec;
  Stream: TStream;
  {$IFDEF MSWINDOWS}
  vi: TJPVersionInfo;
  {$ENDIF}
begin
  ClearInfo;

  if not FileExists(FFileName) then Exit;

  FBits := 0;
  FOptionalHeaderMagicStr := '';

  // --------- Attributes, size, dates ----------
  if fir.ReadFileInfo(FFileName) then
  begin
    {$IFDEF MSWINDOWS}
    FAttributes := fir.Attributes;
    {$ENDIF}
    FFileSize := fir.Size;
    FCreationTime := fir.CreationTime;
    FLastWriteTime := fir.LastWriteTime;
    FLastAccessTime := fir.LastAccessTime;
  end
  else FFileSize := FileSizeInt(FFileName);

  // ----------- VersionInfo -------------
  {$IFDEF MSWINDOWS}
  vi := TJPVersionInfo.Create(FFileName);
  try
    if vi.ValidVersionInfo then
    begin
      if vi.FixedFileInfoExists then
      begin
        FHasFileVersion := True;
        FFileVersion := vi.FileVersion;
      end;
      FHasVersionInfo := vi.TryGetEnglishStringInfoItem(FVersionInfo);
    end;
  finally
    vi.Free;
  end;
  {$ENDIF} // MSWINDOWS


  // -------------- Stream -----------------
  if FFileSize <= FMaxMemoryStreamSize then
  begin
    ms := TMemoryStream.Create;
    ms.LoadFromFile(FFileName);
    Stream := ms;
  end
  else
  begin
    fs := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    Stream := fs;
  end;


  FIsFileLoaded := True;

  GetBitsInfo(Stream, FFirstFile4Bits, '.');

  try


    // ------------------------------- DOS Header -------------------------------------
    if Stream.Size < SizeOf(FDosHeader) then Exit;
    x := Stream.Read(FDosHeader, SizeOf(FDosHeader));
    if x <> SizeOf(FDosHeader) then Exit;


    // ------------------------------ PE Signature ---------------------------------------
    FOffset_PeSignature := FDosHeader.{$IFDEF FPC}e_lfanew{$ELSE}_lfanew{$ENDIF};
    if Stream.Size < FOffset_PeSignature + SizeOf(FPeSignature) then Exit;

    Stream.Position := FOffset_PeSignature;
    x := Stream.Read(FPeSignature, SizeOf(FPeSignature));
    if x <> SizeOf(FPeSignature) then Exit;
    Stream.Position := FOffset_PeSignature;
    Stream.Read(PeSignatureArray, Length(PeSignatureArray));

    if FPeSignature = SIGNATURE_NE then // NE file
    begin
      FBits := 16;
      FIs16bit := True;
      FInfoStr := 'Win16 NE';
      Exit;
    end;

    if FPeSignature <> SIGNATURE_PE then Exit;

    //FBits := 32;


    // -------------------------------- File (Coff) Header ------------------------------------
    FOffset_FileHeader := FOffset_PeSignature + SizeOf(FPeSignature);
    if Stream.Size < FOffset_FileHeader + SizeOf(FFileHeader) then Exit; //FOffset_CoffHeader + SizeOf(FPeSignature) + SizeOf(FFileHeader) then Exit;

    x := Stream.Read(FFileHeader, SizeOf(FFileHeader));
    if x <> SizeOf(FFileHeader) then Exit;



    // --------------------------------- Optional (PE) Header -----------------------------------------
    FOffset_PeHeader := FOffset_FileHeader + SizeOf(FFileHeader);

    if Stream.Size < Stream.Position + 2 then Exit;
    x := Stream.Read(xw, 2);
    if x <> 2 then Exit;

    if xw = OPTIONAL_HEADER_MAGIC_PE32 then
    begin
      FOptionalHeaderMagicStr := 'PE32';
      FBits := 32;
      FIs32bit := True;
    end
    else if xw = OPTIONAL_HEADER_MAGIC_PE32PLUS then
    begin
      FOptionalHeaderMagicStr := 'PE32+';
      FBits := 64;
      FIs64bit := True;
    end
    else
    begin
      FOptionalHeaderMagicStr := '';
      FBits := 0;
    end;



    //if Is64Bit(FileHeader.Machine) then FBits := 64;

    Stream.Position := Stream.Position - 2;

    // 32 bit
    if FBits = 32 then
    begin
      if Stream.Size < Stream.Position + SizeOf(FOptionalHeader32) then Exit;
      x := Stream.Read(FOptionalHeader32, SizeOf(FOptionalHeader32));
      if x <> SizeOf(FOptionalHeader32) then Exit;
      FOffset_SectionsTable := FOffset_PeHeader + SizeOf(FOptionalHeader32);
      FImageBase := FOptionalHeader32.ImageBase;
      FAddressOfEntryPoint := FOptionalHeader32.AddressOfEntryPoint;
      for i := 0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1 do FDataDirectory[i] := FOptionalHeader32.DataDirectory[i];
    end

    else
    // 64 bit
    begin
      if Stream.Size < Stream.Position + SizeOf(FOptionalHeader64) then Exit;
      x := Stream.Read(FOptionalHeader64, SizeOf(FOptionalHeader64));
      if x <> SizeOf(FOptionalHeader64) then Exit;
      FOffset_SectionsTable := FOffset_PeHeader + SizeOf(FOptionalHeader64);
      FImageBase := FOptionalHeader64.ImageBase;
      FAddressOfEntryPoint := FOptionalHeader64.AddressOfEntryPoint;
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
    Stream.Position := FOffset_SectionsTable;
    for i := 0 to FFileHeader.NumberOfSections - 1 do
    begin
      x := Stream.Read(ish, SizeOf(TImageSectionHeader));
      if x <> SizeOf(TImageSectionHeader) then Break;

      SetLength(FSections, Length(FSections) + 1);
      FSections[High(FSections)] := ish;

      s := Trim(GetStrFromBytes(ish.Name));
      FSectionNames.Add(s);
    end;


    FIsValidPeFile := True;

    GetUpxInfo;

  finally
    FreeStreams;
  end;

end;
{$endregion GetFileInfo}


function TPeFile.GetSectionCount: integer;
begin
  Result := Length(FSections);
end;

{$region '                        GetUpxInfo                           '}
procedure TPeFile.GetUpxInfo;
var
  s, Section1, Section2: string;
  xPos, UpxHeaderOffset: integer;
  x: Int64;
  Buffer: array[0..1023] of Byte;
  Buf4Chars: array[0..3] of AnsiChar;
  Stream: TStream;
begin

  Stream := DataStream;
  if not Assigned(Stream) then Exit;

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
      if (UpxHeaderOffset < Stream.Size) and (Stream.Size > (Int64(UpxHeaderOffset) + SizeOf(FUpxHeader))) then
      begin
        FOffset_UpxHeader := UpxHeaderOffset;
        Stream.Position := UpxHeaderOffset;
        Stream.Read(FUpxHeader, SizeOf(FUpxHeader));

        // wyszukiwanie łańcucha 'UPX!'
        if FUpxHeader.Header.UpxMagic <> UPX_MAGIC_STR then
        begin

          FIsPackedByUpx := False;
          FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
          FOffset_UpxHeader := BIN_INVALID_OFFSET;

          x := FOffset_SectionsTable; // Sections[1].PointerToRawData - SizeOf(Buffer);

          if Stream.Size >= Int64(x) + SizeOf(Buffer) then
          begin

            Stream.Position := x;
            Stream.Read(Buffer{%H-}, SizeOf(Buffer));

            xPos := GetStringOffset(UPX_MAGIC_STR, Buffer);
            if xPos >= 0 then
            begin
              // UpxVersion: array[0..4] of AnsiChar; - 5 bytes
              UpxHeaderOffset := x + xPos - 5;
              if Stream.Size >= Int64(UpxHeaderOffset) + SizeOf(FUpxHeader) then
              begin
                Stream.Position := UpxHeaderOffset;
                Stream.Read(FUpxHeader, SizeOf(FUpxHeader));
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

      if Stream.Size >= Int64(x) + SizeOf(Buffer) then
      begin
        Stream.Position := x;
        Stream.Read(Buffer, SizeOf(Buffer));

        s := 'Id: UPX ';
        xPos := GetStringOffset(AnsiString(s), Buffer);
        if xPos >= 0 then
          if Stream.Size >= Int64(x) + xPos + Length(s) + SizeOf(Buf4Chars) then
          begin
            Stream.Position := Int64(x) + xPos + Length(s);
            Stream.Read(Buf4Chars{%H-}, SizeOf(Buf4Chars));
            FUpxVersion := string(Buf4Chars);
          end;

      end;

    end;


  end; // UPX header

end;
{$endregion GetUpxInfo}

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
