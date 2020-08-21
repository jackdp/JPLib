unit JPL.Binary.Types;

{$I .\..\jp.inc}

interface

uses
  SysUtils
  {$IFDEF DCC}{$IFNDEF DELPHIXE2_OR_ABOVE}
  , Windows
  {$ENDIF}{$ENDIF}
  ;


type

  TBitsInfo = record
    B1, B2, B3, B4: BYTE;
    BitsStr: string;
  end;

const
  MAGIC_STR_MZ = 'MZ'; // Marek Żbikowski - https://en.wikipedia.org/wiki/Mark_Zbikowski
  MAGIS_STR_ELF = 'ELF';

  // Windows & DOS
  BIN_WIN32 = 0;
  BIN_DOS = 1;
  BIN_WIN16 = 2;
  BIN_PIF = 3;
  BIN_POSIX = 4;
  BIN_OS2_16 = 5;
  BIN_WIN64 = 6;

  // Unix & Linux
  BIN_UNIX16 = 102;
  BIN_UNIX32 = 100;
  BIN_UNIX64 = 106;

  BIN_MACH_FAT = 201; // CA FE BA BE

  // Mach objects
  BIN_MACHO_32_LITTLE_ENDIAN = 210;
  BIN_MACHO_32_BIG_ENDIAN = 220;
  BIN_MACHO_64_LITTLE_ENDIAN = 230;
  BIN_MACHO_64_BIG_ENDIAN = 240;
  BIN_MACHO_FAT_MULTIARCH = 300; // Big endian
  //BIN_MACHO_FAT_MULTIARCH_BE = 310;

  BIN_UNKNOWN = -1;

  UNKNOWN_EXECUTABLE = 'Unknown executable format!';

  BIN_INVALID_OFFSET = -1;

{$IFDEF DCC}
{$IFNDEF DELPHIXE2_OR_ABOVE}
type
  PImageOptionalHeader32 = ^TImageOptionalHeader32;
  _IMAGE_OPTIONAL_HEADER32 = record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    { NT additional fields. }
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TImageOptionalHeader32 = _IMAGE_OPTIONAL_HEADER32;

  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  _IMAGE_OPTIONAL_HEADER64 = record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    { NT additional fields. }
    ImageBase: ULONGLONG;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: ULONGLONG;
    SizeOfStackCommit: ULONGLONG;
    SizeOfHeapReserve: ULONGLONG;
    SizeOfHeapCommit: ULONGLONG;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
{$ENDIF}
{$ENDIF}



implementation




end.
