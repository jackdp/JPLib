unit JPL.Binary.Types;

interface

uses
  SysUtils
  //, Dialogs
  ;


type

  TBitsInfo = record
    B1, B2, B3, B4: BYTE;
    BitsStr: string;
  end;

const
  MAGIC_STR_MZ = 'MZ'; // Marek Żbikowski - https://en.wikipedia.org/wiki/Mark_Zbikowski
  MAGIS_STR_ELF = 'ELF';

  // WIndows & DOS
  BIN_WIN32 = 0;
  BIN_WIN64 = 6;
  BIN_DOS = 1;
  BIN_WIN16 = 2;

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




implementation




end.
