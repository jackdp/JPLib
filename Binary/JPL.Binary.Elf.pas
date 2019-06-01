unit JPL.Binary.Elf;

// Jacek Pazera
// http://www.pazera-software.com
// 03.2016
// Delphi XE7

// Based on:
// https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
// http://www.sco.com/developers/gabi/latest/contents.html


interface

uses
  Windows, Sysutils, Classes,
  System.Generics.Collections,
  JPL.Strings, JPL.Conversion, JPL.Binary.Types, JPL.UPX, JPL.Binary.Procs;



const
  {$region ' ------ const ------ '}
  ELF_LITTLE_ENDIAN = 1;
  ELF_BIG_ENDIAN = 2;
  ELF_UNKNOWN_ENDIAN = 0;

  ELF_MAGIC = $7F454C46; // 7F'ELF'

  ELF_CLASS_NONE = 0; // Invalid class
  ELF_CLASS_32 = 1; // 32-bit object
  ELF_CLASS_64 = 2; // 64-bit object

  ELF_DATA_NONE = ELF_UNKNOWN_ENDIAN; // Invalid data encoding
  ELF_DATA_2LSB = ELF_LITTLE_ENDIAN;
  ELF_DATA_2MSB = ELF_BIG_ENDIAN;


  // OSABI - OS Application Binary Interface
  //ELF_OSABI_SYSTEMV = $00; // 0x00 	System V
  ELF_OSABI_NONE = 0;      // No extensions or unspecified
  ELF_OSABI_HPUX = 1;      // Hewlett-Packard HP-UX
  ELF_OSABI_NETBSD = 2;    // NetBSD
  ELF_OSABI_GNU = 3;       // GNU
  //ELF_OSABI_LINUX = 3; historical - alias for ELFOSABI_GNU
  ELF_OSABI_SOLARIS = 6;   // Sun Solaris
  ELF_OSABI_AIX = 7;       // AIX
  ELF_OSABI_IRIX = 8;      // IRIX
  ELF_OSABI_FREEBSD = 9;   // FreeBSD
  ELF_OSABI_TRU64 = 10;    // Compaq TRU64 UNIX
  ELF_OSABI_MODESTO = 11;  // Novell Modesto
  ELF_OSABI_OPENBSD = 12;  // OpenBSD
  ELF_OSABI_OPENVMS = 13;  // OpenVMS
  ELF_OSABI_NSK = 14;      // Hewlett-Packard Non-Stop Kernel
  ELF_OSABI_AROS = 15;     // Amiga Research OS
  ELF_OSABI_FENIXOS = 16;  // The FenixOS highly scalable multi-core OS
  ELF_OSABI_CLOUDABI = 17; // Nuxi CloudABI
  ELF_OSABI_OPENVOS = 18;  // Stratus Technologies OpenVOS
  // Architecture-specific value range 64-255


  // Object type
  ELF_OBJECT_TYPE_UNKNOWN = 0;
  ELF_OBJECT_TYPE_RELOCATABLE = 1;
  ELF_OBJECT_TYPE_EXECUTABLE = 2;
  ELF_OBJECT_TYPE_SHARED = 3;
  ELF_OBJECT_TYPE_CORE = 4;
  ELF_OBJECT_TYPE_LOOS = $FE00; // Operating system-specific
  ELF_OBJECT_TYPE_HIOS = $FEFF; // Operating system-specific
  ELF_OBJECT_TYPE_LOPROC = $FF00; // Processor-specific
  ELF_OBJECT_TYPE_HIPROC = $FFFF; // Processor-specific


  {$region '   const - Machines   '}
  // Target instruction set architecture
  ELF_MACHINE_NONE = 0; // No machine
  ELF_MACHINE_M32 = 1; // AT&T WE 32100
  ELF_MACHINE_SPARC = 2; // SPARC
  ELF_MACHINE_386 = 3; // Intel 80386
  ELF_MACHINE_68K = 4; // Motorola 68000
  ELF_MACHINE_88K = 5; // Motorola 88000
  ELF_MACHINE_IAMCU = 6; // Intel MCU
  ELF_MACHINE_860 = 7; // Intel 80860
  ELF_MACHINE_MIPS = 8; // MIPS I Architecture
  ELF_MACHINE_S370 = 9; // IBM System/370 Processor
  ELF_MACHINE_MIPS_RS3_LE = 10; // MIPS RS3000 Little-endian
  // reserved 11-14 Reserved for future use
  ELF_MACHINE_PARISC = 15; // Hewlett-Packard PA-RISC
  // reserved 16 Reserved for future use
  ELF_MACHINE_VPP500 = 17; // Fujitsu VPP500
  ELF_MACHINE_SPARC32PLUS = 18; // Enhanced instruction set SPARC
  ELF_MACHINE_960 = 19; // Intel 80960
  ELF_MACHINE_PPC = 20; // PowerPC
  ELF_MACHINE_PPC64 = 21; // 64-bit PowerPC
  ELF_MACHINE_S390 = 22; // IBM System/390 Processor
  ELF_MACHINE_SPU = 23; // IBM SPU/SPC
  // reserved 24-35 Reserved for future use
  ELF_MACHINE_V800 = 36; // NEC V800
  ELF_MACHINE_FR20 = 37; // Fujitsu FR20
  ELF_MACHINE_RH32 = 38; // TRW RH-32
  ELF_MACHINE_RCE = 39; // Motorola RCE
  ELF_MACHINE_ARM = 40; // ARM 32-bit architecture (AARCH32)
  ELF_MACHINE_ALPHA = 41; // Digital Alpha
  ELF_MACHINE_SH = 42; // Hitachi SH
  ELF_MACHINE_SPARCV9 = 43; // SPARC Version 9
  ELF_MACHINE_TRICORE = 44; // Siemens TriCore embedded processor
  ELF_MACHINE_ARC = 45; // Argonaut RISC Core, Argonaut Technologies Inc.
  ELF_MACHINE_H8_300 = 46; // Hitachi H8/300
  ELF_MACHINE_H8_300H = 47; // Hitachi H8/300H
  ELF_MACHINE_H8S = 48; // Hitachi H8S
  ELF_MACHINE_H8_500 = 49; // Hitachi H8/500
  ELF_MACHINE_IA_64 = 50; // Intel IA-64 processor architecture
  ELF_MACHINE_MIPS_X = 51; // Stanford MIPS-X
  ELF_MACHINE_COLDFIRE = 52; // Motorola ColdFire
  ELF_MACHINE_68HC12 = 53; // Motorola M68HC12
  ELF_MACHINE_MMA = 54; // Fujitsu MMA Multimedia Accelerator
  ELF_MACHINE_PCP = 55; // Siemens PCP
  ELF_MACHINE_NCPU = 56; // Sony nCPU embedded RISC processor
  ELF_MACHINE_NDR1 = 57; // Denso NDR1 microprocessor
  ELF_MACHINE_STARCORE = 58; // Motorola Star*Core processor
  ELF_MACHINE_ME16 = 59; // Toyota ME16 processor
  ELF_MACHINE_ST100 = 60; // STMicroelectronics ST100 processor
  ELF_MACHINE_TINYJ = 61; // Advanced Logic Corp. TinyJ embedded processor family
  ELF_MACHINE_X86_64 = 62; // AMD x86-64 architecture
  ELF_MACHINE_PDSP = 63; // Sony DSP Processor
  ELF_MACHINE_PDP10 = 64; // Digital Equipment Corp. PDP-10
  ELF_MACHINE_PDP11 = 65; // Digital Equipment Corp. PDP-11
  ELF_MACHINE_FX66 = 66; // Siemens FX66 microcontroller
  ELF_MACHINE_ST9PLUS = 67; // STMicroelectronics ST9+ 8/16 bit microcontroller
  ELF_MACHINE_ST7 = 68; // STMicroelectronics ST7 8-bit microcontroller
  ELF_MACHINE_68HC16 = 69; // Motorola MC68HC16 Microcontroller
  ELF_MACHINE_68HC11 = 70; // Motorola MC68HC11 Microcontroller
  ELF_MACHINE_68HC08 = 71; // Motorola MC68HC08 Microcontroller
  ELF_MACHINE_68HC05 = 72; // Motorola MC68HC05 Microcontroller
  ELF_MACHINE_SVX = 73; // Silicon Graphics SVx
  ELF_MACHINE_ST19 = 74; // STMicroelectronics ST19 8-bit microcontroller
  ELF_MACHINE_VAX = 75; // Digital VAX
  ELF_MACHINE_CRIS = 76; // Axis Communications 32-bit embedded processor
  ELF_MACHINE_JAVELIN = 77; // Infineon Technologies 32-bit embedded processor
  ELF_MACHINE_FIREPATH = 78; // Element 14 64-bit DSP Processor
  ELF_MACHINE_ZSP = 79; // LSI Logic 16-bit DSP Processor
  ELF_MACHINE_MMIX = 80; // Donald Knuth's educational 64-bit processor
  ELF_MACHINE_HUANY = 81; // Harvard University machine-independent object files
  ELF_MACHINE_PRISM = 82; // SiTera Prism
  ELF_MACHINE_AVR = 83; // Atmel AVR 8-bit microcontroller
  ELF_MACHINE_FR30 = 84; // Fujitsu FR30
  ELF_MACHINE_D10V = 85; // Mitsubishi D10V
  ELF_MACHINE_D30V = 86; // Mitsubishi D30V
  ELF_MACHINE_V850 = 87; // NEC v850
  ELF_MACHINE_M32R = 88; // Mitsubishi M32R
  ELF_MACHINE_MN10300 = 89; // Matsushita MN10300
  ELF_MACHINE_MN10200 = 90; // Matsushita MN10200
  ELF_MACHINE_PJ = 91; // picoJava
  ELF_MACHINE_OPENRISC = 92; // OpenRISC 32-bit embedded processor
  ELF_MACHINE_ARC_COMPACT = 93; // ARC International ARCompact processor (old spelling/synonym: EM_ARC_A5)
  ELF_MACHINE_XTENSA = 94; // Tensilica Xtensa Architecture
  ELF_MACHINE_VIDEOCORE = 95; // Alphamosaic VideoCore processor
  ELF_MACHINE_TMM_GPP = 96; // Thompson Multimedia General Purpose Processor
  ELF_MACHINE_NS32K = 97; // National Semiconductor 32000 series
  ELF_MACHINE_TPC = 98; // Tenor Network TPC processor
  ELF_MACHINE_SNP1K = 99; // Trebia SNP 1000 processor
  ELF_MACHINE_ST200 = 100; // STMicroelectronics (www.st.com) ST200 microcontroller
  ELF_MACHINE_IP2K = 101; // Ubicom IP2xxx microcontroller family
  ELF_MACHINE_MAX = 102; // MAX Processor
  ELF_MACHINE_CR = 103; // National Semiconductor CompactRISC microprocessor
  ELF_MACHINE_F2MC16 = 104; // Fujitsu F2MC16
  ELF_MACHINE_MSP430 = 105; // Texas Instruments embedded microcontroller msp430
  ELF_MACHINE_BLACKFIN = 106; // Analog Devices Blackfin (DSP) processor
  ELF_MACHINE_SE_C33 = 107; // S1C33 Family of Seiko Epson processors
  ELF_MACHINE_SEP = 108; // Sharp embedded microprocessor
  ELF_MACHINE_ARCA = 109; // Arca RISC Microprocessor
  ELF_MACHINE_UNICORE = 110; // Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University
  ELF_MACHINE_EXCESS = 111; // eXcess: 16/32/64-bit configurable embedded CPU
  ELF_MACHINE_DXP = 112; // Icera Semiconductor Inc. Deep Execution Processor
  ELF_MACHINE_ALTERA_NIOS2 = 113; // Altera Nios II soft-core processor
  ELF_MACHINE_CRX = 114; // National Semiconductor CompactRISC CRX microprocessor
  ELF_MACHINE_XGATE = 115; // Motorola XGATE embedded processor
  ELF_MACHINE_C166 = 116; // Infineon C16x/XC16x processor
  ELF_MACHINE_M16C = 117; // Renesas M16C series microprocessors
  ELF_MACHINE_DSPIC30F = 118; // Microchip Technology dsPIC30F Digital Signal Controller
  ELF_MACHINE_CE = 119; // Freescale Communication Engine RISC core
  ELF_MACHINE_M32C = 120; // Renesas M32C series microprocessors
  // reserved 121-130 Reserved for future use
  ELF_MACHINE_TSK3000 = 131; // Altium TSK3000 core
  ELF_MACHINE_RS08 = 132; // Freescale RS08 embedded processor
  ELF_MACHINE_SHARC = 133; // Analog Devices SHARC family of 32-bit DSP processors
  ELF_MACHINE_ECOG2 = 134; // Cyan Technology eCOG2 microprocessor
  ELF_MACHINE_SCORE7 = 135; // Sunplus S+core7 RISC processor
  ELF_MACHINE_DSP24 = 136; // New Japan Radio (NJR) 24-bit DSP Processor
  ELF_MACHINE_VIDEOCORE3 = 137; // Broadcom VideoCore III processor
  ELF_MACHINE_LATTICEMICO32 = 138; // RISC processor for Lattice FPGA architecture
  ELF_MACHINE_SE_C17 = 139; // Seiko Epson C17 family
  ELF_MACHINE_TI_C6000 = 140; // The Texas Instruments TMS320C6000 DSP family
  ELF_MACHINE_TI_C2000 = 141; // The Texas Instruments TMS320C2000 DSP family
  ELF_MACHINE_TI_C5500 = 142; // The Texas Instruments TMS320C55x DSP family
  ELF_MACHINE_TI_ARP32 = 143; // Texas Instruments Application Specific RISC Processor, 32bit fetch
  ELF_MACHINE_TI_PRU = 144; // Texas Instruments Programmable Realtime Unit
  // reserved 145-159 Reserved for future use
  // reserved 145-159 Reserved for future use
  ELF_MACHINE_MMDSP_PLUS = 160; // STMicroelectronics 64bit VLIW Data Signal Processor
  ELF_MACHINE_CYPRESS_M8C = 161; // Cypress M8C microprocessor
  ELF_MACHINE_R32C = 162; // Renesas R32C series microprocessors
  ELF_MACHINE_TRIMEDIA = 163; // NXP Semiconductors TriMedia architecture family
  ELF_MACHINE_QDSP6 = 164; // QUALCOMM DSP6 Processor
  ELF_MACHINE_8051 = 165; // Intel 8051 and variants
  ELF_MACHINE_STXP7X = 166; // STMicroelectronics STxP7x family of configurable and extensible RISC processors
  ELF_MACHINE_NDS32 = 167; // Andes Technology compact code size embedded RISC processor family
  ELF_MACHINE_ECOG1 = 168; // Cyan Technology eCOG1X family
  ELF_MACHINE_ECOG1X = ELF_MACHINE_ECOG1; // Cyan Technology eCOG1X family
  ELF_MACHINE_MAXQ30 = 169; // Dallas Semiconductor MAXQ30 Core Micro-controllers
  ELF_MACHINE_XIMO16 = 170; // New Japan Radio (NJR) 16-bit DSP Processor
  ELF_MACHINE_MANIK = 171; // M2000 Reconfigurable RISC Microprocessor
  ELF_MACHINE_CRAYNV2 = 172; // Cray Inc. NV2 vector architecture
  ELF_MACHINE_RX = 173; // Renesas RX family
  ELF_MACHINE_METAG = 174; // Imagination Technologies META processor architecture
  ELF_MACHINE_MCST_ELBRUS = 175; // MCST Elbrus general purpose hardware architecture
  ELF_MACHINE_ECOG16 = 176; // Cyan Technology eCOG16 family
  ELF_MACHINE_CR16 = 177; // National Semiconductor CompactRISC CR16 16-bit microprocessor
  ELF_MACHINE_ETPU = 178; // Freescale Extended Time Processing Unit
  ELF_MACHINE_SLE9X = 179; // Infineon Technologies SLE9X core
  ELF_MACHINE_L10M = 180; // Intel L10M
  ELF_MACHINE_K10M = 181; // Intel K10M
  // reserved 182 Reserved for future Intel use
  ELF_MACHINE_AARCH64 = 183; // ARM 64-bit architecture (AARCH64)
  // reserved 184 Reserved for future ARM use
  ELF_MACHINE_AVR32 = 185; // Atmel Corporation 32-bit microprocessor family
  ELF_MACHINE_STM8 = 186; // STMicroeletronics STM8 8-bit microcontroller
  ELF_MACHINE_TILE64 = 187; // Tilera TILE64 multicore architecture family
  ELF_MACHINE_TILEPRO = 188; // Tilera TILEPro multicore architecture family
  ELF_MACHINE_MICROBLAZE = 189; // Xilinx MicroBlaze 32-bit RISC soft processor core
  ELF_MACHINE_CUDA = 190; // NVIDIA CUDA architecture
  ELF_MACHINE_TILEGX = 191; // Tilera TILE-Gx multicore architecture family
  ELF_MACHINE_CLOUDSHIELD = 192; // CloudShield architecture family
  ELF_MACHINE_COREA_1ST = 193; // KIPO-KAIST Core-A 1st generation processor family
  ELF_MACHINE_COREA_2ND = 194; // KIPO-KAIST Core-A 2nd generation processor family
  ELF_MACHINE_ARC_COMPACT2 = 195; // Synopsys ARCompact V2
  ELF_MACHINE_OPEN8 = 196; // Open8 8-bit RISC soft processor core
  ELF_MACHINE_RL78 = 197; // Renesas RL78 family
  ELF_MACHINE_VIDEOCORE5 = 198; // Broadcom VideoCore V processor
  ELF_MACHINE_78KOR = 199; // Renesas 78KOR family
  ELF_MACHINE_56800EX = 200; // Freescale 56800EX Digital Signal Controller (DSC)
  ELF_MACHINE_BA1 = 201; // Beyond BA1 CPU architecture
  ELF_MACHINE_BA2 = 202; // Beyond BA2 CPU architecture
  ELF_MACHINE_XCORE = 203; // XMOS xCORE processor family
  ELF_MACHINE_MCHP_PIC = 204; // Microchip 8-bit PIC(r) family
  ELF_MACHINE_INTEL205 = 205; // Reserved by Intel
  ELF_MACHINE_INTEL206 = 206; // Reserved by Intel
  ELF_MACHINE_INTEL207 = 207; // Reserved by Intel
  ELF_MACHINE_INTEL208 = 208; // Reserved by Intel
  ELF_MACHINE_INTEL209 = 209; // Reserved by Intel
  ELF_MACHINE_KM32 = 210; // KM211 KM32 32-bit processor
  ELF_MACHINE_KMX32 = 211; // KM211 KMX32 32-bit processor
  ELF_MACHINE_KMX16 = 212; // KM211 KMX16 16-bit processor
  ELF_MACHINE_KMX8 = 213; // KM211 KMX8 8-bit processor
  ELF_MACHINE_KVARC = 214; // KM211 KVARC processor
  ELF_MACHINE_CDP = 215; // Paneve CDP architecture family
  ELF_MACHINE_COGE = 216; // Cognitive Smart Memory Processor
  ELF_MACHINE_COOL = 217; // Bluechip Systems CoolEngine
  ELF_MACHINE_NORC = 218; // Nanoradio Optimized RISC
  ELF_MACHINE_CSR_KALIMBA  = 219; // CSR Kalimba architecture family
  ELF_MACHINE_Z80  = 220; // Zilog Z80
  ELF_MACHINE_VISIUM  = 221; // Controls and Data Services VISIUMcore processor
  ELF_MACHINE_FT32  = 222; // FTDI Chip FT32 high performance 32-bit RISC architecture
  ELF_MACHINE_MOXIE = 223; // Moxie processor family
  ELF_MACHINE_AMDGPU = 224; // AMD GPU architecture
  {$endregion const - Machines}

  // ELF Program Header - Segment types
  PT_NULL = 0;
  PT_LOAD = 1;
  PT_DYNAMIC = 2;
  PT_INTERP = 3;
  PT_NOTE = 4;
  PT_SHLIB = 5;
  PT_PHDR = 6;
  PT_TLS = 7;
  PT_LOOS = $60000000;
  PT_HIOS = $6fffffff;
  PT_LOPROC = $70000000;
  PT_HIPROC = $7fffffff;


//  Segment Flag Bits, p_flags
//
//  Name 	Value 	Meaning
//  PF_X 	0x1 	Execute
//  PF_W 	0x2 	Write
//  PF_R 	0x4 	Read
//  PF_MASKOS 	0x0ff00000 	Unspecified
//  PF_MASKPROC 	0xf0000000 	Unspecified
  PF_X = $1;
  PF_W = $2;
  PF_R = $4;
  PF_MASKOS = $0ff00000;
  PF_MASKPROC = $f0000000;


  {$endregion const}


type
  {$region ' ------- types -------- '}



  {$region '   ELF specific types   '}


  {$region '   ELF data types   '}
  // http://www.sco.com/developers/gabi/latest/ch4.intro.html#data_representation

//  Elf 32 bit data types
//    Half - unsigned int 2 bytes
//    Word, Addr, Off - unsigned int 4
//    Sword - signed int 4
//
//  Elf 64 bit data types
//    Half - unsigned int 2
//    Word - unsigned int 4
//    Sword - signed int 4
//    Addr, Off, Xword - unsigned int 8
//    Sxword - signed int 8

  Elf32_Half = UInt16;
  Elf32_Word = UInt32;
  Elf32_Addr = UInt32;
  Elf32_Off = UInt32;
  Elf32_Sword = Int32;


  Elf64_Half = UInt16;
  Elf64_Word = UInt32;
  Elf64_Sword = Int32;
  Elf64_Addr = UInt64;
  Elf64_Off = UInt64;
  Elf64_Xword = UInt64;
  Elf64_Sxword = Int64;
  {$endregion ELF data types}


  // ELF Header
  // http://www.sco.com/developers/gabi/latest/ch4.eheader.html

//  #define EI_NIDENT 16

//  typedef struct {
//          unsigned char   e_ident[EI_NIDENT];
//          Elf32_Half      e_type;
//          Elf32_Half      e_machine;
//          Elf32_Word      e_version;
//          Elf32_Addr      e_entry;
//          Elf32_Off       e_phoff;
//          Elf32_Off       e_shoff;
//          Elf32_Word      e_flags;
//          Elf32_Half      e_ehsize;
//          Elf32_Half      e_phentsize;
//          Elf32_Half      e_phnum;
//          Elf32_Half      e_shentsize;
//          Elf32_Half      e_shnum;
//          Elf32_Half      e_shstrndx;
//  } Elf32_Ehdr;

  // SizeOf(TE_IDENT) = EI_NIDENT = 16;
  PE_IDENT = ^TE_IDENT;
  TE_IDENT = packed record
    EI_MAGIC: array[0..3] of Byte; // $7F 45 4C 46 ($7F'ELF')
    EI_CLASS: Byte; // File class or capacity
    EI_DATA: Byte; // Little or big endian. Specifies the encoding of both the data structures used by object file container and data contained in object file sections
    EI_VERSION: Byte; // ELF header version (must be 1)
    EI_OSABI: Byte; // The OS- or ABI-specific ELF extensions used by this file
    EI_ABIVERSION: Byte; // Version of the ABI to which the object is targeted
    EI_PAD: array[0..6] of Byte; // Reserved (unused) bytes
  end;

  // SizeOf(TElfHeader32) = 36
  PElfHeader32 = ^TElfHeader32;
  TElfHeader32 = packed record
    e_type: Elf32_Half; // Word; // 1, 2, 3, 4 specify whether the object is relocatable, executable, shared, or core, respectively.
    e_machine: Elf32_Half; // Target instruction set architecture.
    e_version: Elf32_Word; // Set to 1 for the original version of ELF.
    e_entry: Elf32_Addr; // Memory address of the entry point from where the process starts executing (32 or 64 bits long depending on the format defined earlier)
    e_phoff: Elf32_Off; // Points to the start of the program header table. It usually follows the file header immediately making the offset 0x40 for 64-bit ELF executables.
    e_shoff: Elf32_Off; // Points to the start of the section header table.
    e_flags: Elf32_Word; // Processor-specific flags associated with the file. Flag names take the form EF_machine_flag.
    e_ehsize: Elf32_Half; // ELF header size, normally 64 bytes for 64-bit and 52 for 32-bit format. e_ehsize = SizeOf(TE_IDENT) + SizeOf(TElfHeader32)
    e_phentsize: Elf32_Half; // Size of a program header table entry.
    e_phnum: Elf32_Half; // Number of entries in the program header table.
    e_shentsize: Elf32_Half; // Size of a section header table entry.
    e_shnum: Elf32_Half; // Number of entries in the section header table.
    e_shstrndx: Elf32_Half; // Index of the section header table entry that contains the section names.
  end;


//  typedef struct {
//          unsigned char   e_ident[EI_NIDENT];
//          Elf64_Half      e_type;
//          Elf64_Half      e_machine;
//          Elf64_Word      e_version;
//          Elf64_Addr      e_entry;
//          Elf64_Off       e_phoff;
//          Elf64_Off       e_shoff;
//          Elf64_Word      e_flags;
//          Elf64_Half      e_ehsize;
//          Elf64_Half      e_phentsize;
//          Elf64_Half      e_phnum;
//          Elf64_Half      e_shentsize;
//          Elf64_Half      e_shnum;
//          Elf64_Half      e_shstrndx;
//  } Elf64_Ehdr;

  // SizeOf(TElfHeader64) = 48
  PElfHeader64 = ^TElfHeader64;
  TElfHeader64 = packed record
    e_type: Elf64_Half; // 1, 2, 3, 4 specify whether the object is relocatable, executable, shared, or core, respectively.
    e_machine: Elf64_Half; // Target instruction set architecture.
    e_version: Elf64_Word; // Set to 1 for the original version of ELF.
    e_entry: Elf64_Addr; // Memory address of the entry point from where the process starts executing (32 or 64 bits long depending on the format defined earlier)
    e_phoff: Elf64_Off; // Points to the start of the program header table. It usually follows the file header immediately making the offset 0x40 for 64-bit ELF executables.
    e_shoff: Elf64_Off; // Points to the start of the section header table.
    e_flags: Elf64_Word; // Processor-specific flags associated with the file. Flag names take the form EF_machine_flag.
    e_ehsize: Elf64_Half; // ELF header size, normally 64 bytes for 64-bit and 52 for 32-bit format.
    e_phentsize: Elf64_Half; // Size of a program header table entry.
    e_phnum: Elf64_Half; // Number of entries in the program header table.
    e_shentsize: Elf64_Half; // Size of a section header table entry.
    e_shnum: Elf64_Half; // Number of entries in the section header table.
    e_shstrndx: Elf64_Half; // Index of the section header table entry that contains the section names.
  end;


  

  // Program Header Table
  // http://www.sco.com/developers/gabi/latest/ch5.pheader.html

//  typedef struct {
//    Elf32_Word	p_type;
//    Elf32_Off	p_offset;
//    Elf32_Addr	p_vaddr;
//    Elf32_Addr	p_paddr;
//    Elf32_Word	p_filesz;
//    Elf32_Word	p_memsz;
//    Elf32_Word	p_flags;
//    Elf32_Word	p_align;
//  } Elf32_Phdr;
  PElfProgramHeader32 = ^TElfProgramHeader32;
  TElfProgramHeader32 = packed record
    p_type: Elf32_Word; // Segment type
    p_offset: Elf32_Off; // Segment file offset
    p_vaddr: Elf32_Addr; // Segment virtual address
    p_paddr: Elf32_Addr; // Segment physical address
    p_filesz: Elf32_Word; // Segment size in file
    p_memsz: Elf32_Word; // Segment size in memory
    p_flags: Elf32_Word; // Segment flags
    p_align: Elf32_Word; // Segment alignment
  end;

//  typedef struct {
//    Elf64_Word	p_type;
//    Elf64_Word	p_flags;
//    Elf64_Off	p_offset;
//    Elf64_Addr	p_vaddr;
//    Elf64_Addr	p_paddr;
//    Elf64_Xword	p_filesz;
//    Elf64_Xword	p_memsz;
//    Elf64_Xword	p_align;
//  } Elf64_Phdr;
  PElfProgramHeader64 = ^TElfProgramHeader64;
  TElfProgramHeader64 = packed record
    p_type: Elf64_Word; // Segment type
    p_flags: Elf64_Word; // Segment flags
    p_offset: Elf64_Off; // Segment file offset
    p_vaddr: Elf64_Addr; // Segment virtual address
    p_paddr: Elf64_Addr; // Segment physical address
    p_filesz: Elf64_Xword; // Segment size in file
    p_memsz: Elf64_Xword; // Segment size in memory
    p_align: Elf64_Xword; // Segment alignment
  end;


  // Section Header Table
  // http://www.sco.com/developers/gabi/latest/ch4.sheader.html
//  typedef struct {
//    Elf32_Word	sh_name;
//    Elf32_Word	sh_type;
//    Elf32_Word	sh_flags;
//    Elf32_Addr	sh_addr;
//    Elf32_Off	sh_offset;
//    Elf32_Word	sh_size;
//    Elf32_Word	sh_link;
//    Elf32_Word	sh_info;
//    Elf32_Word	sh_addralign;
//    Elf32_Word	sh_entsize;
//  } Elf32_Shdr;
  PElfSectionHeader32 = ^TElfSectionHeader32;
  TElfSectionHeader32 = packed record
    sh_name: Elf32_Word;
    sh_type: Elf32_Word;
    sh_flags: Elf32_Word;
    sh_addr: Elf32_Addr;
    sh_offset: Elf32_Off;
    sh_size: Elf32_Word;
    sh_link: Elf32_Word;
    sh_info: Elf32_Word;
    sh_addralign: Elf32_Word;
    sh_entsize: Elf32_Word;
  end;


//  typedef struct {
//    Elf64_Word	sh_name;
//    Elf64_Word	sh_type;
//    Elf64_Xword	sh_flags;
//    Elf64_Addr	sh_addr;
//    Elf64_Off	sh_offset;
//    Elf64_Xword	sh_size;
//    Elf64_Word	sh_link;
//    Elf64_Word	sh_info;
//    Elf64_Xword	sh_addralign;
//    Elf64_Xword	sh_entsize;
//  } Elf64_Shdr;
  PElfSectionHeader64 = ^TElfSectionHeader64;
  TElfSectionHeader64 = packed record
    sh_name: Elf64_Word;
    sh_type: Elf64_Word;
    sh_flags: Elf64_Xword;
    sh_addr: Elf64_Addr;
    sh_offset: Elf64_Off;
    sh_size: Elf64_Xword;
    sh_link: Elf64_Word;
    sh_info: Elf64_Word;
    sh_addralign: Elf64_Xword;
    sh_entsize: Elf64_Xword;
  end;


  {$endregion ELF specific types}

  {$region '   INT - TElfFile   '}
  TElfProgramHeaderTable32Entries = TList<TElfProgramHeader32>;
  TElfProgramHeaderTable64Entries = TList<TElfProgramHeader64>;

  TElfSectionHeaderTable32Entries = TList<TElfSectionHeader32>;
  TElfSectionHeaderTable64Entries = TList<TElfSectionHeader64>;

  TElfFile = class
  private
    FFileName: string;
    FFileSize: Int64;
    FIsValidElfFile: Boolean;
    FE_IDENT: TE_IDENT;
    FElfHeader32: TElfHeader32;
    FElfHeader64: TElfHeader64;
    FMagic: string;
    FBits: Byte;
    FEndian: Byte;
    FEndianStr: string;
    FElfHeaderVersion: Byte;
    FOsAbi: Byte;
    FOsAbiStr: string;
    FAbiVersion: Byte;
    FPaddingBytesStr: string;
    FSizeOfHeader: Byte;
    FObjectType: Word;
    FObjectTypeStr: string;
    FMachine: Word;
    FMachineStr: string;
    FObjectFileVersion: LongWord;
    FEntryPoint: UInt64;
    FProgramHeaderTableOffset: UInt64;
    FSectionHeaderTableOffset: UInt64;
    FProcessorFlags: LongWord;
    FElfHeaderSize: Word;
    FProgramHeaderTableEntrySize: Word;
    FProgramHeaderTableEntries: Word;
    FSectionHeaderTableEntrySize: Word;
    FSectionHeaderTableEntries: Word;
    FSectionNamesEntryIndex: Word;
    FInfoStr: string;

    FProgramHeaderTable32Entries: TElfProgramHeaderTable32Entries;
    FProgramHeaderTable64Entries: TElfProgramHeaderTable64Entries;

    FSectionHeaderTable32Entries: TElfSectionHeaderTable32Entries;
    FSectionHeaderTable64Entries: TElfSectionHeaderTable64Entries;

    FIsPackedByUpx: Boolean;
    FIsValidUpxHeader: Boolean;
    FUpxHeader: TElfUpxHeader;
    FUpxHeaderOffset: Int64;

    procedure SetFileName(const Value: string);
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReadFileInfo;
    function AsString: string;
    function FullElfHeaderSize: Integer;

    property FileName: string read FFileName write SetFileName;
    property FileSize: Int64 read FFileSize;

    // ELF Header = E_IDENT + ElfHeader
    property E_IDENT: TE_IDENT read FE_IDENT;
    property ElfHeader32: TElfHeader32 read FElfHeader32;
    property ElfHeader64: TElfHeader64 read FElfHeader64;

    property IsValidElfFile: Boolean read FIsValidElfFile;

    // E_IDENT
    property Magic: string read FMagic;
    property Bits: Byte read FBits;
    property Endian: Byte read FEndian;
    property EndianStr: string read FEndianStr;
    property ElfHeaderVersion: Byte read FElfHeaderVersion;
    property OsAbi: Byte read FOsAbi;
    property OsAbiStr: string read FOsAbiStr;
    property AbiVersion: Byte read FAbiVersion;
    property PaddingBytesStr: string read FPaddingBytesStr;

    // ElfHeader2
    property ObjectType: Word read FObjectType;                                   // ElfHeader.e_type
    property ObjectTypeStr: string read FObjectTypeStr;
    property Machine: Word read FMachine;                                         // ElfHeader.e_machine
    property MachineStr: string read FMachineStr;
    property ObjectFileVersion: LongWord read FObjectFileVersion;                 // ElfHeader.e_version
    property EntryPoint: UInt64 read FEntryPoint;                                 // ElfHeader.e_entry
    property ProgramHeaderTableOffset: UInt64 read FProgramHeaderTableOffset;     // ElfHeader.e_phoff
    property SectionHeaderTableOffset: UInt64 read FSectionHeaderTableOffset;     // ElfHeader.e_shoff
    property ProcessorFlags: LongWord read FProcessorFlags;                       // ElfHeader.e_flags
    property ElfHeaderSize: Word read FElfHeaderSize;                             // ElfHeader.e_ehsize
    property ProgramHeaderTableEntrySize: Word read FProgramHeaderTableEntrySize; // ElfHeader.e_phentsize
    property ProgramHeaderTableEntries: Word read FProgramHeaderTableEntries;     // ElfHeader.e_phnum
    property SectionHeaderTableEntrySize: Word read FSectionHeaderTableEntrySize; // ElfHeader.e_shentsize
    property SectionHeaderTableEntries: Word read FSectionHeaderTableEntries;     // ElfHeader.e_shnum
    property SectionNamesEntryIndex: Word read FSectionNamesEntryIndex;           // ElfHeader.e_shstrndx
    property InfoStr: string read FInfoStr;

    property ProgramHeaderTable32Entries: TElfProgramHeaderTable32Entries read FProgramHeaderTable32Entries;
    property ProgramHeaderTable64Entries: TElfProgramHeaderTable64Entries read FProgramHeaderTable64Entries;

    property SectionHeaderTable32Entries: TElfSectionHeaderTable32Entries read FSectionHeaderTable32Entries;
    property SectionHeaderTable64Entries: TElfSectionHeaderTable64Entries read FSectionHeaderTable64Entries;

    property IsPackedByUpx: Boolean read FIsPackedByUpx;
    property IsValidUpxHeader: Boolean read FIsValidUpxHeader;
    property UpxHeader: TElfUpxHeader read FUpxHeader;
    property UpxHeaderOffset: Int64 read FUpxHeaderOffset;
//    property UpxVersion: string read FUpxVersion;

  end;
  {$endregion TElfFile}

  {$endregion types}




function ELF_EIdentToStr(E_IDENT: TE_IDENT): string;
function ELF_EiMagicToStr(Magic: array of Byte; IncludeFirstByte: Boolean = False): string;
function ELF_OsAbiToStr(OsAbi: Byte): string;
function ELF_ObjectTypeToStr(ObjectType: Word): string;
function ELF_MachineToStr(Machine: Word): string;
function ELF_EiClassToStr(EI_CLASS: Byte): string;
function ELF_EiDataToStr(EI_DATA: Byte): string;
function ELF_SegmentTypeToStr(SegmentType: DWORD): string;
function ELF_SegmentFlagToStr(Flags: UInt32): string;

procedure SwapElfHeader32Bytes(var eh32: TElfHeader32);
procedure SwapElfHeader64Bytes(var eh64: TElfHeader64);
procedure SwapElfSectionHeader32Bytes(var sh32: TElfSectionHeader32);
procedure SwapElfSectionHeader64Bytes(var sh64: TElfSectionHeader64);
procedure SwapElfProgramHeader32Bytes(var ph32: TElfProgramHeader32);
procedure SwapElfProgramHeader64Bytes(var ph64: TElfProgramHeader64);

implementation



{$region ' -------------- helpers ----------------- '}


procedure SwapElfHeader64Bytes(var eh64: TElfHeader64);
begin
  eh64.e_type := SwapBytes(eh64.e_type);
  eh64.e_machine := SwapBytes(eh64.e_machine);
  eh64.e_version := SwapDword(eh64.e_version);
  eh64.e_entry := SwapUInt64(eh64.e_entry);
  eh64.e_phoff := SwapUInt64(eh64.e_phoff);
  eh64.e_shoff := SwapUInt64(eh64.e_shoff);
  eh64.e_flags := SwapDword(eh64.e_flags);
  eh64.e_ehsize := SwapBytes(eh64.e_ehsize);
  eh64.e_phentsize := SwapBytes(eh64.e_phentsize);
  eh64.e_phnum := SwapBytes(eh64.e_phnum);
  eh64.e_shentsize := SwapBytes(eh64.e_shentsize);
  eh64.e_shnum := SwapBytes(eh64.e_shnum);
  eh64.e_shstrndx := SwapBytes(eh64.e_shstrndx);
end;

procedure SwapElfHeader32Bytes(var eh32: TElfHeader32);
begin
  eh32.e_type := SwapBytes(eh32.e_type);
  eh32.e_machine := SwapBytes(eh32.e_machine);
  eh32.e_version := SwapDword(eh32.e_version);
  eh32.e_entry := SwapDword(eh32.e_entry);
  eh32.e_phoff := SwapDword(eh32.e_phoff);
  eh32.e_shoff := SwapDword(eh32.e_shoff);
  eh32.e_flags := SwapDword(eh32.e_flags);
  eh32.e_ehsize := SwapBytes(eh32.e_ehsize);
  eh32.e_phentsize := SwapBytes(eh32.e_phentsize);
  eh32.e_phnum := SwapBytes(eh32.e_phnum);
  eh32.e_shentsize := SwapBytes(eh32.e_shentsize);
  eh32.e_shnum := SwapBytes(eh32.e_shnum);
  eh32.e_shstrndx := SwapBytes(eh32.e_shstrndx);
end;


procedure SwapElfProgramHeader64Bytes(var ph64: TElfProgramHeader64);
begin
  ph64.p_type := SwapDword(ph64.p_type);
  ph64.p_flags := SwapDword(ph64.p_flags);
  ph64.p_offset := SwapUInt64(ph64.p_offset);
  ph64.p_vaddr := SwapUInt64(ph64.p_vaddr);
  ph64.p_paddr := SwapUInt64(ph64.p_paddr);
  ph64.p_filesz := SwapUInt64(ph64.p_filesz);
  ph64.p_memsz := SwapUInt64(ph64.p_memsz);
  ph64.p_align := SwapUInt64(ph64.p_align);
end;

procedure SwapElfProgramHeader32Bytes(var ph32: TElfProgramHeader32);
begin
  ph32.p_type := SwapDword(ph32.p_type);
  ph32.p_offset := SwapDword(ph32.p_offset);
  ph32.p_vaddr := SwapDword(ph32.p_vaddr);
  ph32.p_paddr := SwapDword(ph32.p_paddr);
  ph32.p_filesz := SwapDword(ph32.p_filesz);
  ph32.p_memsz := SwapDword(ph32.p_memsz);
  ph32.p_flags := SwapDword(ph32.p_flags);
  ph32.p_align := SwapDword(ph32.p_align);
end;

procedure SwapElfSectionHeader64Bytes(var sh64: TElfSectionHeader64);
begin
  sh64.sh_name := SwapDword(sh64.sh_name);
  sh64.sh_type := SwapDword(sh64.sh_type);
  sh64.sh_flags := SwapUInt64(sh64.sh_flags);
  sh64.sh_addr := SwapUInt64(sh64.sh_addr);
  sh64.sh_offset := SwapUInt64(sh64.sh_offset);
  sh64.sh_size := SwapUInt64(sh64.sh_size);
  sh64.sh_link := SwapDword(sh64.sh_link);
  sh64.sh_info := SwapDword(sh64.sh_info);
  sh64.sh_addralign := SwapUInt64(sh64.sh_addralign);
  sh64.sh_entsize := SwapUInt64(sh64.sh_entsize);
end;

procedure SwapElfSectionHeader32Bytes(var sh32: TElfSectionHeader32);
begin
  sh32.sh_name := SwapDword(sh32.sh_name);
  sh32.sh_type := SwapDword(sh32.sh_type);
  sh32.sh_flags := SwapDword(sh32.sh_flags);
  sh32.sh_addr := SwapDword(sh32.sh_addr);
  sh32.sh_offset := SwapDword(sh32.sh_offset);
  sh32.sh_size := SwapDword(sh32.sh_size);
  sh32.sh_link := SwapDword(sh32.sh_link);
  sh32.sh_info := SwapDword(sh32.sh_info);
  sh32.sh_addralign := SwapDword(sh32.sh_addralign);
  sh32.sh_entsize := SwapDword(sh32.sh_entsize);
end;

function ELF_SegmentFlagToStr(Flags: UInt32): string;
var
  s: string;
begin
//  Name 	Value 	Meaning
//  PF_X 	0x1 	Execute
//  PF_W 	0x2 	Write
//  PF_R 	0x4 	Read
//  PF_MASKOS 	0x0ff00000 	Unspecified
//  PF_MASKPROC 	0xf0000000 	Unspecified
  s := '';
  if (Flags and PF_R) > 0 then s := s + ', Read';
  if (Flags and PF_X) > 0 then s := s + ', Execute';
  if (Flags and PF_W) > 0 then s := s + ', Write';
  if Copy(s, 1, 2) = ', ' then Delete(s, 1, 2);
  Result := s;
end;

function ELF_SegmentTypeToStr(SegmentType: DWORD): string;
begin
  case SegmentType of
    PT_NULL: Result := 'Unused segment';
    PT_LOAD: Result := 'Loadable';
    PT_DYNAMIC: Result := 'Dynamic linking information';
    PT_INTERP: Result := 'PT_INTERP - null-terminated path name location & size';
    PT_NOTE: Result := 'Auxiliary information';
    PT_SHLIB: Result := 'PT_SHLIB - reserved';
    PT_PHDR: Result := 'Program header table location & size';
    PT_TLS: Result := 'Thread-Local Storage template';
    PT_LOOS..PT_HIOS: Result := 'OS specific';
    PT_LOPROC..PT_HIPROC: Result := 'Processor specific';
  else
    Result := 'Unknown segment type';
  end;
end;


function ELF_EiDataToStr(EI_DATA: Byte): string;
begin
  case EI_DATA of
    ELF_DATA_NONE: Result := 'Unknown endian - Invalid data encoding';
    ELF_DATA_2LSB: Result := '2LSB - Little endian';
    ELF_DATA_2MSB: Result := '2MSB - Big endian';
  else
    Result := 'Unknown data encoding';
  end;
end;

function ELF_EiClassToStr(EI_CLASS: Byte): string;
begin
  case EI_CLASS of
    ELF_CLASS_NONE: Result := 'Invalid class';
    ELF_CLASS_32: Result := '32-bit object';
    ELF_CLASS_64: Result := '64-bit object';
  else
    Result := 'Unknown EI_CLASS';
  end;
end;

function ELF_MachineToStr(Machine: Word): string;
begin
  case Machine of
    ELF_MACHINE_NONE: Result := 'No machine';
    ELF_MACHINE_M32: Result := 'AT&T WE 32100';
    ELF_MACHINE_SPARC: Result := 'SPARC';
    ELF_MACHINE_386: Result := 'Intel 80386';
    ELF_MACHINE_68K: Result := 'Motorola 68000';
    ELF_MACHINE_88K: Result := 'Motorola 88000';
    ELF_MACHINE_IAMCU: Result := 'Intel MCU';
    ELF_MACHINE_860: Result := 'Intel 80860';
    ELF_MACHINE_MIPS: Result := 'MIPS I Architecture';
    ELF_MACHINE_S370: Result := 'IBM System/370 Processor';
    ELF_MACHINE_MIPS_RS3_LE: Result := 'MIPS RS3000 Little-endian';
    ELF_MACHINE_PARISC: Result := 'Hewlett-Packard PA-RISC';
    ELF_MACHINE_VPP500: Result := 'Fujitsu VPP500';
    ELF_MACHINE_SPARC32PLUS: Result := 'Enhanced instruction set SPARC';
    ELF_MACHINE_960: Result := 'Intel 80960';
    ELF_MACHINE_PPC: Result := 'PowerPC';
    ELF_MACHINE_PPC64: Result := '64-bit PowerPC';
    ELF_MACHINE_S390: Result := 'IBM System/390 Processor';
    ELF_MACHINE_SPU: Result := 'IBM SPU/SPC';
    ELF_MACHINE_V800: Result := 'NEC V800';
    ELF_MACHINE_FR20: Result := 'Fujitsu FR20';
    ELF_MACHINE_RH32: Result := 'TRW RH-32';
    ELF_MACHINE_RCE: Result := 'Motorola RCE';
    ELF_MACHINE_ARM: Result := 'ARM 32-bit architecture (AARCH32)';
    ELF_MACHINE_ALPHA: Result := 'Digital Alpha';
    ELF_MACHINE_SH: Result := 'Hitachi SH';
    ELF_MACHINE_SPARCV9: Result := 'SPARC Version 9';
    ELF_MACHINE_TRICORE: Result := 'Siemens TriCore embedded processor';
    ELF_MACHINE_ARC: Result := 'Argonaut RISC Core, Argonaut Technologies Inc.';
    ELF_MACHINE_H8_300: Result := 'Hitachi H8/300';
    ELF_MACHINE_H8_300H: Result := 'Hitachi H8/300H';
    ELF_MACHINE_H8S: Result := 'Hitachi H8S';
    ELF_MACHINE_H8_500: Result := 'Hitachi H8/500';
    ELF_MACHINE_IA_64: Result := 'Intel IA-64'; //Intel IA-64 processor architecture
    ELF_MACHINE_MIPS_X: Result := 'Stanford MIPS-X';
    ELF_MACHINE_COLDFIRE: Result := 'Motorola ColdFire';
    ELF_MACHINE_68HC12: Result := 'Motorola M68HC12';
    ELF_MACHINE_MMA: Result := 'Fujitsu MMA Multimedia Accelerator';
    ELF_MACHINE_PCP: Result := 'Siemens PCP';
    ELF_MACHINE_NCPU: Result := 'Sony nCPU embedded RISC processor';
    ELF_MACHINE_NDR1: Result := 'Denso NDR1 microprocessor';
    ELF_MACHINE_STARCORE: Result := 'Motorola Star*Core processor';
    ELF_MACHINE_ME16: Result := 'Toyota ME16 processor';
    ELF_MACHINE_ST100: Result := 'STMicroelectronics ST100 processor';
    ELF_MACHINE_TINYJ: Result := 'Advanced Logic Corp. TinyJ embedded processor family';
    ELF_MACHINE_X86_64: Result := 'AMD x86-64'; //AMD x86-64 architecture
    ELF_MACHINE_PDSP: Result := 'Sony DSP Processor';
    ELF_MACHINE_PDP10: Result := 'Digital Equipment Corp. PDP-10';
    ELF_MACHINE_PDP11: Result := 'Digital Equipment Corp. PDP-11';
    ELF_MACHINE_FX66: Result := 'Siemens FX66 microcontroller';
    ELF_MACHINE_ST9PLUS: Result := 'STMicroelectronics ST9+ 8/16 bit microcontroller';
    ELF_MACHINE_ST7: Result := 'STMicroelectronics ST7 8-bit microcontroller';
    ELF_MACHINE_68HC16: Result := 'Motorola MC68HC16 Microcontroller';
    ELF_MACHINE_68HC11: Result := 'Motorola MC68HC11 Microcontroller';
    ELF_MACHINE_68HC08: Result := 'Motorola MC68HC08 Microcontroller';
    ELF_MACHINE_68HC05: Result := 'Motorola MC68HC05 Microcontroller';
    ELF_MACHINE_SVX: Result := 'Silicon Graphics SVx';
    ELF_MACHINE_ST19: Result := 'STMicroelectronics ST19 8-bit microcontroller';
    ELF_MACHINE_VAX: Result := 'Digital VAX';
    ELF_MACHINE_CRIS: Result := 'Axis Communications 32-bit embedded processor';
    ELF_MACHINE_JAVELIN: Result := 'Infineon Technologies 32-bit embedded processor';
    ELF_MACHINE_FIREPATH: Result := 'Element 14 64-bit DSP Processor';
    ELF_MACHINE_ZSP: Result := 'LSI Logic 16-bit DSP Processor';
    ELF_MACHINE_MMIX: Result := 'Donald Knuth''s educational 64-bit processor';
    ELF_MACHINE_HUANY: Result := 'Harvard University machine-independent object files';
    ELF_MACHINE_PRISM: Result := 'SiTera Prism';
    ELF_MACHINE_AVR: Result := 'Atmel AVR 8-bit microcontroller';
    ELF_MACHINE_FR30: Result := 'Fujitsu FR30';
    ELF_MACHINE_D10V: Result := 'Mitsubishi D10V';
    ELF_MACHINE_D30V: Result := 'Mitsubishi D30V';
    ELF_MACHINE_V850: Result := 'NEC v850';
    ELF_MACHINE_M32R: Result := 'Mitsubishi M32R';
    ELF_MACHINE_MN10300: Result := 'Matsushita MN10300';
    ELF_MACHINE_MN10200: Result := 'Matsushita MN10200';
    ELF_MACHINE_PJ: Result := 'picoJava';
    ELF_MACHINE_OPENRISC: Result := 'OpenRISC 32-bit embedded processor';
    ELF_MACHINE_ARC_COMPACT: Result := 'ARC International ARCompact processor (old spelling/synonym: EM_ARC_A5)';
    ELF_MACHINE_XTENSA: Result := 'Tensilica Xtensa Architecture';
    ELF_MACHINE_VIDEOCORE: Result := 'Alphamosaic VideoCore processor';
    ELF_MACHINE_TMM_GPP: Result := 'Thompson Multimedia General Purpose Processor';
    ELF_MACHINE_NS32K: Result := 'National Semiconductor 32000 series';
    ELF_MACHINE_TPC: Result := 'Tenor Network TPC processor';
    ELF_MACHINE_SNP1K: Result := 'Trebia SNP 1000 processor';
    ELF_MACHINE_ST200: Result := 'STMicroelectronics (www.st.com) ST200 microcontroller';
    ELF_MACHINE_IP2K: Result := 'Ubicom IP2xxx microcontroller family';
    ELF_MACHINE_MAX: Result := 'MAX Processor';
    ELF_MACHINE_CR: Result := 'National Semiconductor CompactRISC microprocessor';
    ELF_MACHINE_F2MC16: Result := 'Fujitsu F2MC16';
    ELF_MACHINE_MSP430: Result := 'Texas Instruments embedded microcontroller msp430';
    ELF_MACHINE_BLACKFIN: Result := 'Analog Devices Blackfin (DSP) processor';
    ELF_MACHINE_SE_C33: Result := 'S1C33 Family of Seiko Epson processors';
    ELF_MACHINE_SEP: Result := 'Sharp embedded microprocessor';
    ELF_MACHINE_ARCA: Result := 'Arca RISC Microprocessor';
    ELF_MACHINE_UNICORE: Result := 'Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University';
    ELF_MACHINE_EXCESS: Result := 'eXcess: 16/32/64-bit configurable embedded CPU';
    ELF_MACHINE_DXP: Result := 'Icera Semiconductor Inc. Deep Execution Processor';
    ELF_MACHINE_ALTERA_NIOS2: Result := 'Altera Nios II soft-core processor';
    ELF_MACHINE_CRX: Result := 'National Semiconductor CompactRISC CRX microprocessor';
    ELF_MACHINE_XGATE: Result := 'Motorola XGATE embedded processor';
    ELF_MACHINE_C166: Result := 'Infineon C16x/XC16x processor';
    ELF_MACHINE_M16C: Result := 'Renesas M16C series microprocessors';
    ELF_MACHINE_DSPIC30F: Result := 'Microchip Technology dsPIC30F Digital Signal Controller';
    ELF_MACHINE_CE: Result := 'Freescale Communication Engine RISC core';
    ELF_MACHINE_M32C: Result := 'Renesas M32C series microprocessors';
    ELF_MACHINE_TSK3000: Result := 'Altium TSK3000 core';
    ELF_MACHINE_RS08: Result := 'Freescale RS08 embedded processor';
    ELF_MACHINE_SHARC: Result := 'Analog Devices SHARC family of 32-bit DSP processors';
    ELF_MACHINE_ECOG2: Result := 'Cyan Technology eCOG2 microprocessor';
    ELF_MACHINE_SCORE7: Result := 'Sunplus S+core7 RISC processor';
    ELF_MACHINE_DSP24: Result := 'New Japan Radio (NJR) 24-bit DSP Processor';
    ELF_MACHINE_VIDEOCORE3: Result := 'Broadcom VideoCore III processor';
    ELF_MACHINE_LATTICEMICO32: Result := 'RISC processor for Lattice FPGA architecture';
    ELF_MACHINE_SE_C17: Result := 'Seiko Epson C17 family';
    ELF_MACHINE_TI_C6000: Result := 'The Texas Instruments TMS320C6000 DSP family';
    ELF_MACHINE_TI_C2000: Result := 'The Texas Instruments TMS320C2000 DSP family';
    ELF_MACHINE_TI_C5500: Result := 'The Texas Instruments TMS320C55x DSP family';
    ELF_MACHINE_TI_ARP32: Result := 'Texas Instruments Application Specific RISC Processor, 32bit fetch';
    ELF_MACHINE_TI_PRU: Result := 'Texas Instruments Programmable Realtime Unit';
    ELF_MACHINE_MMDSP_PLUS: Result := 'STMicroelectronics 64bit VLIW Data Signal Processor';
    ELF_MACHINE_CYPRESS_M8C: Result := 'Cypress M8C microprocessor';
    ELF_MACHINE_R32C: Result := 'Renesas R32C series microprocessors';
    ELF_MACHINE_TRIMEDIA: Result := 'NXP Semiconductors TriMedia architecture family';
    ELF_MACHINE_QDSP6: Result := 'QUALCOMM DSP6 Processor';
    ELF_MACHINE_8051: Result := 'Intel 8051 and variants';
    ELF_MACHINE_STXP7X: Result := 'STMicroelectronics STxP7x family of configurable and extensible RISC processors';
    ELF_MACHINE_NDS32: Result := 'Andes Technology compact code size embedded RISC processor family';
    ELF_MACHINE_ECOG1: Result := 'Cyan Technology eCOG1X family';
    //ELF_MACHINE_ECOG1X: Result := 'Cyan Technology eCOG1X family';
    ELF_MACHINE_MAXQ30: Result := 'Dallas Semiconductor MAXQ30 Core Micro-controllers';
    ELF_MACHINE_XIMO16: Result := 'New Japan Radio (NJR) 16-bit DSP Processor';
    ELF_MACHINE_MANIK: Result := 'M2000 Reconfigurable RISC Microprocessor';
    ELF_MACHINE_CRAYNV2: Result := 'Cray Inc. NV2 vector architecture';
    ELF_MACHINE_RX: Result := 'Renesas RX family';
    ELF_MACHINE_METAG: Result := 'Imagination Technologies META processor architecture';
    ELF_MACHINE_MCST_ELBRUS: Result := 'MCST Elbrus general purpose hardware architecture';
    ELF_MACHINE_ECOG16: Result := 'Cyan Technology eCOG16 family';
    ELF_MACHINE_CR16: Result := 'National Semiconductor CompactRISC CR16 16-bit microprocessor';
    ELF_MACHINE_ETPU: Result := 'Freescale Extended Time Processing Unit';
    ELF_MACHINE_SLE9X: Result := 'Infineon Technologies SLE9X core';
    ELF_MACHINE_L10M: Result := 'Intel L10M';
    ELF_MACHINE_K10M: Result := 'Intel K10M';
    ELF_MACHINE_AARCH64: Result := 'ARM 64-bit architecture (AARCH64)';
    ELF_MACHINE_AVR32: Result := 'Atmel Corporation 32-bit microprocessor family';
    ELF_MACHINE_STM8: Result := 'STMicroeletronics STM8 8-bit microcontroller';
    ELF_MACHINE_TILE64: Result := 'Tilera TILE64 multicore architecture family';
    ELF_MACHINE_TILEPRO: Result := 'Tilera TILEPro multicore architecture family';
    ELF_MACHINE_MICROBLAZE: Result := 'Xilinx MicroBlaze 32-bit RISC soft processor core';
    ELF_MACHINE_CUDA: Result := 'NVIDIA CUDA architecture';
    ELF_MACHINE_TILEGX: Result := 'Tilera TILE-Gx multicore architecture family';
    ELF_MACHINE_CLOUDSHIELD: Result := 'CloudShield architecture family';
    ELF_MACHINE_COREA_1ST: Result := 'KIPO-KAIST Core-A 1st generation processor family';
    ELF_MACHINE_COREA_2ND: Result := 'KIPO-KAIST Core-A 2nd generation processor family';
    ELF_MACHINE_ARC_COMPACT2: Result := 'Synopsys ARCompact V2';
    ELF_MACHINE_OPEN8: Result := 'Open8 8-bit RISC soft processor core';
    ELF_MACHINE_RL78: Result := 'Renesas RL78 family';
    ELF_MACHINE_VIDEOCORE5: Result := 'Broadcom VideoCore V processor';
    ELF_MACHINE_78KOR: Result := 'Renesas 78KOR family';
    ELF_MACHINE_56800EX: Result := 'Freescale 56800EX Digital Signal Controller (DSC)';
    ELF_MACHINE_BA1: Result := 'Beyond BA1 CPU architecture';
    ELF_MACHINE_BA2: Result := 'Beyond BA2 CPU architecture';
    ELF_MACHINE_XCORE: Result := 'XMOS xCORE processor family';
    ELF_MACHINE_MCHP_PIC: Result := 'Microchip 8-bit PIC(r) family';
    ELF_MACHINE_INTEL205: Result := 'Reserved by Intel';
    ELF_MACHINE_INTEL206: Result := 'Reserved by Intel';
    ELF_MACHINE_INTEL207: Result := 'Reserved by Intel';
    ELF_MACHINE_INTEL208: Result := 'Reserved by Intel';
    ELF_MACHINE_INTEL209: Result := 'Reserved by Intel';
    ELF_MACHINE_KM32: Result := 'KM211 KM32 32-bit processor';
    ELF_MACHINE_KMX32: Result := 'KM211 KMX32 32-bit processor';
    ELF_MACHINE_KMX16: Result := 'KM211 KMX16 16-bit processor';
    ELF_MACHINE_KMX8: Result := 'KM211 KMX8 8-bit processor';
    ELF_MACHINE_KVARC: Result := 'KM211 KVARC processor';
    ELF_MACHINE_CDP: Result := 'Paneve CDP architecture family';
    ELF_MACHINE_COGE: Result := 'Cognitive Smart Memory Processor';
    ELF_MACHINE_COOL: Result := 'Bluechip Systems CoolEngine';
    ELF_MACHINE_NORC: Result := 'Nanoradio Optimized RISC';
    ELF_MACHINE_CSR_KALIMBA : Result := 'CSR Kalimba architecture family';
    ELF_MACHINE_Z80 : Result := 'Zilog Z80';
    ELF_MACHINE_VISIUM : Result := 'Controls and Data Services VISIUMcore processor';
    ELF_MACHINE_FT32 : Result := 'FTDI Chip FT32 high performance 32-bit RISC architecture';
    ELF_MACHINE_MOXIE: Result := 'Moxie processor family';
    ELF_MACHINE_AMDGPU: Result := 'AMD GPU architecture';
  else
    Result := 'UNKNOWN machine';
  end;
end;

function ELF_ObjectTypeToStr(ObjectType: Word): string;
begin
  case ObjectType of
    ELF_OBJECT_TYPE_UNKNOWN: Result := 'Not specified';
    ELF_OBJECT_TYPE_RELOCATABLE: Result := 'Relocatable';
    ELF_OBJECT_TYPE_EXECUTABLE: Result := 'Executable';
    ELF_OBJECT_TYPE_SHARED: Result := 'Shared';
    ELF_OBJECT_TYPE_CORE: Result := 'Core';
    ELF_OBJECT_TYPE_LOOS..ELF_OBJECT_TYPE_HIOS: Result := 'OS specific';
    ELF_OBJECT_TYPE_LOPROC..ELF_OBJECT_TYPE_HIPROC: Result := 'Processor specific';
  //ELF_OBJECT_TYPE_UNKNOWN = 0;
  else
    Result := 'UNKNOWN object type';
  end;
end;

function ELF_OsAbiToStr(OsAbi: Byte): string;
begin
  case OsAbi of
    ELF_OSABI_NONE: Result := 'No extensions or unspecified';
    ELF_OSABI_HPUX: Result := 'Hewlett-Packard HP-UX';
    ELF_OSABI_NETBSD: Result := 'NetBSD';
    ELF_OSABI_GNU: Result := 'GNU';
    //ELF_OSABI_LINUX = 3; historical - alias for ELFOSABI_GNU
    ELF_OSABI_SOLARIS: Result := 'Sun Solaris';
    ELF_OSABI_AIX: Result := 'AIX';
    ELF_OSABI_IRIX: Result := 'IRIX';
    ELF_OSABI_FREEBSD: Result := 'FreeBSD';
    ELF_OSABI_TRU64: Result := 'Compaq TRU64 UNIX';
    ELF_OSABI_MODESTO: Result := 'Novell Modesto';
    ELF_OSABI_OPENBSD: Result := 'OpenBSD';
    ELF_OSABI_OPENVMS: Result := 'OpenVMS';
    ELF_OSABI_NSK: Result := 'Hewlett-Packard Non-Stop Kernel';
    ELF_OSABI_AROS: Result := 'Amiga Research OS';
    ELF_OSABI_FENIXOS: Result := 'The FenixOS highly scalable multi-core OS';
    ELF_OSABI_CLOUDABI: Result := 'Nuxi CloudABI';
    ELF_OSABI_OPENVOS: Result := 'Stratus Technologies OpenVOS';
  else
    Result := 'UNKNOWN ABI';
  end;
end;

function ELF_EiMagicToStr(Magic: array of Byte; IncludeFirstByte: Boolean = False): string;
var
  i: integer;
begin
  Result := '';
  if Length(Magic) <> 4 then Exit;
  if IncludeFirstByte then Result := IntToHex(Magic[0], 2) + ' ';

  for i := 1 to Length(Magic) - 1 do Result := Result + Chr(Magic[i]);
end;

function ELF_EIdentToStr(E_IDENT: TE_IDENT): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(E_IDENT.EI_MAGIC) - 1 do Result := Result + IntToHex(E_IDENT.EI_MAGIC[i], 2) + ' ';
  Result := Result + IntToHex(E_IDENT.EI_CLASS, 2) + ' ';
  Result := Result + IntToHex(E_IDENT.EI_DATA, 2) + ' ';
  Result := Result + IntToHex(E_IDENT.EI_VERSION, 2) + ' ';
  Result := Result + IntToHex(E_IDENT.EI_OSABI, 2) + ' ';
  Result := Result + IntToHex(E_IDENT.EI_ABIVERSION, 2) + ' ';
  for i := 0 to Length(E_IDENT.EI_PAD) - 1 do Result := Result + IntToHex(E_IDENT.EI_PAD[i], 2) + ' ';

  Delete(Result, Length(Result), 1);
end;


{$endregion helpers}


{$region ' ------------------------------------ TElfFile ---------------------------------------------- '}

constructor TElfFile.Create;
begin
  inherited;

  FProgramHeaderTable32Entries := TElfProgramHeaderTable32Entries.Create;
  FProgramHeaderTable64Entries := TElfProgramHeaderTable64Entries.Create;

  FSectionHeaderTable32Entries := TElfSectionHeaderTable32Entries.Create;
  FSectionHeaderTable64Entries := TElfSectionHeaderTable64Entries.Create;

  Clear;
  FFileName := '';
end;



destructor TElfFile.Destroy;
begin
  FProgramHeaderTable32Entries.Free;
  FProgramHeaderTable64Entries.Free;
  FSectionHeaderTable32Entries.Free;
  FSectionHeaderTable64Entries.Free;
  inherited;
end;

function TElfFile.FullElfHeaderSize: Integer;
begin
  if Bits = 32 then Result := SizeOf(TE_IDENT) + SizeOf(TElfHeader32)
  else if Bits = 64 then Result := SizeOf(TE_IDENT) + SizeOf(TElfHeader64)
  else Result := -1;
end;

procedure TElfFile.Clear;
begin
  FillChar(FE_IDENT, SizeOf(FE_IDENT), 0);
  FillChar(FElfHeader32, SizeOf(FElfHeader32), 0);
  FillChar(FElfHeader64, SizeOf(FElfHeader64), 0);

  FProgramHeaderTable32Entries.Clear;
  FProgramHeaderTable64Entries.Clear;
  FSectionHeaderTable32Entries.Clear;
  FSectionHeaderTable64Entries.Clear;
  FFileSize := 0;
  FIsValidElfFile := False;
  FSizeOfHeader := 0;
  FMagic := '';
  FBits := 0;
  FEndian := ELF_UNKNOWN_ENDIAN;
  FEndianStr := '';
  FOsAbiStr := '';
  FPaddingBytesStr := '';
  FObjectTypeStr := '';
  FMachineStr := '';
  FInfoStr := '';
  FIsPackedByUpx := False;
  FIsValidUpxHeader := False;
  FillChar(FUpxHeader, SizeOf(FUpxHeader), 0);
  FUpxHeaderOffset := 0;
end;

  {$region '                ReadFileInfo                     '}
procedure TElfFile.ReadFileInfo;
var
  fs: TFileStream;
  x, i, xHeaderSize: integer;
  ph32: TElfProgramHeader32;
  ph64: TElfProgramHeader64;
  sh32: TElfSectionHeader32;
  sh64: TElfSectionHeader64;
begin
  Clear;
  FileName := Trim(FileName);
  if FileName = '' then Exit;
  if not FileExists(FileName) then Exit;
  FFileSize := FileSizeInt(FileName);


  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try

    // ---------------- IDENT ------------------
    if fs.Size < SizeOf(FE_IDENT) then Exit;
    x := fs.Read(FE_IDENT, SizeOf(FE_IDENT)); // read IDENT
    if x <> SizeOf(FE_IDENT) then Exit;

    FMagic := ELF_EiMagicToStr(FE_IDENT.EI_MAGIC, True);

    
    // ----------------- bits ------------------
    if FE_IDENT.EI_CLASS = ELF_CLASS_32 then
    begin
      FBits := 32;
      xHeaderSize := SizeOf(FE_IDENT) + SizeOf(FElfHeader32);
    end
    else if FE_IDENT.EI_CLASS = ELF_CLASS_64 then
    begin
      FBits := 64;
      xHeaderSize := SizeOf(FE_IDENT) + SizeOf(FElfHeader64);
    end
    else
    begin
      FBits := 0;
      Exit;
    end;

    if fs.Size < xHeaderSize then Exit;


    // ------------------ odczyt nagłówka 32 lub 64-bit ------------------
    if FBits = 32 then
    begin
      x := fs.Read(FElfHeader32, SizeOf(FElfHeader32));
      if x <> SizeOf(FElfHeader32) then Exit;
    end
    else
    begin
      x := fs.Read(FElfHeader64, SizeOf(FElfHeader64));
      if x <> SizeOf(FElfHeader64) then Exit;
    end;



    
    // -------------------- endian -----------------------
    if FE_IDENT.EI_DATA = ELF_LITTLE_ENDIAN then
    begin
      FEndian := ELF_LITTLE_ENDIAN;
      FEndianStr := 'little endian';
    end
    else if FE_IDENT.EI_DATA = ELF_BIG_ENDIAN then
    begin
      FEndian := ELF_BIG_ENDIAN;
      FEndianStr := 'big endian';
    end
    else
    begin
      FEndian := ELF_UNKNOWN_ENDIAN;
      FEndianStr := 'UNKNOWN endian';
    end;



    
    FElfHeaderVersion := FE_IDENT.EI_VERSION;

    FOsAbi := FE_IDENT.EI_OSABI;
    FOsAbiStr := ELF_OsAbiToStr(FOsAbi);

    FAbiVersion := FE_IDENT.EI_ABIVERSION;

    FPaddingBytesStr := '';
    for i := 0 to Length(FE_IDENT.EI_PAD) - 1 do FPaddingBytesStr := FPaddingBytesStr + IntToHex(FE_IDENT.EI_PAD[i], 2) + ' ';
    Delete(FPaddingBytesStr, Length(FPaddingBytesStr), 1);


    // ------------------------ swapping bytes -----------------------
    if FEndian = ELF_BIG_ENDIAN then
      if Bits = 32 then SwapElfHeader32Bytes(FElfHeader32)
      else SwapElfHeader64Bytes(FElfHeader64);


    if Bits = 32 then
    begin
      FObjectType := FElfHeader32.e_type;
      FMachine := FElfHeader32.e_machine;
      FObjectFileVersion := FElfHeader32.e_version;
      FEntryPoint := FElfHeader32.e_entry;
      FProgramHeaderTableOffset := FElfHeader32.e_phoff;
      FSectionHeaderTableOffset := FElfHeader32.e_shoff;
      FProcessorFlags := FElfHeader32.e_flags;
      FElfHeaderSize := FElfHeader32.e_ehsize;
      FProgramHeaderTableEntrySize := FElfHeader32.e_phentsize;
      FProgramHeaderTableEntries := FElfHeader32.e_phnum;
      FSectionHeaderTableEntrySize := FElfHeader32.e_shentsize;
      FSectionHeaderTableEntries := FElfHeader32.e_shnum;
      FSectionNamesEntryIndex := FElfHeader32.e_shstrndx;
    end
    else
    // 64 bit
    begin
      FObjectType := FElfHeader64.e_type;
      FMachine := FElfHeader64.e_machine;
      FObjectFileVersion := FElfHeader64.e_version;
      FEntryPoint := FElfHeader64.e_entry;
      FProgramHeaderTableOffset := FElfHeader64.e_phoff;
      FSectionHeaderTableOffset := FElfHeader64.e_shoff;
      FProcessorFlags := FElfHeader64.e_flags;
      FElfHeaderSize := FElfHeader64.e_ehsize;
      FProgramHeaderTableEntrySize := FElfHeader64.e_phentsize;
      FProgramHeaderTableEntries := FElfHeader64.e_phnum;
      FSectionHeaderTableEntrySize := FElfHeader64.e_shentsize;
      FSectionHeaderTableEntries := FElfHeader64.e_shnum;
      FSectionNamesEntryIndex := FElfHeader64.e_shstrndx;
    end;


    FObjectTypeStr := ELF_ObjectTypeToStr(FObjectType);
    FMachineStr := ELF_MachineToStr(FMachine);

    FInfoStr := FObjectTypeStr + ' / Unix ELF ' + IntToStr(Bits) + ' / ' + FMachineStr;


    // --------------- Odczyt wpisów w Program Header Table ---------------------------
    if fs.Size < ProgramHeaderTableOffset then Exit;
    fs.Position := ProgramHeaderTableOffset;

    if Bits = 32 then
      for i := 1 to ProgramHeaderTableEntries do
      begin
        if fs.Size < fs.Position + SizeOf(ph32) then Exit;
        x := fs.Read(ph32, SizeOf(ph32));
        if x <> SizeOf(ph32) then Exit;
        if Endian = ELF_BIG_ENDIAN then SwapElfProgramHeader32Bytes(ph32);
        ProgramHeaderTable32Entries.Add(ph32);
      end

    // 64 bit
    else
      for i := 1 to ProgramHeaderTableEntries do
      begin
        if fs.Size < fs.Position + SizeOf(ph64) then Exit;
        x := fs.Read(ph64, SizeOf(ph64));
        if x <> SizeOf(ph64) then Exit;
        if Endian = ELF_BIG_ENDIAN then SwapElfProgramHeader64Bytes(ph64);
        ProgramHeaderTable64Entries.Add(ph64);
      end;





    // --------------- Odczyt wpisów w Section Header Table ---------------------------
    if fs.Size < SectionHeaderTableOffset then Exit;
    fs.Position := SectionHeaderTableOffset;

    if Bits = 32 then
      for i := 1 to SectionHeaderTableEntries do
      begin
        if fs.Size < fs.Position + SizeOf(sh32) then Exit;
        x := fs.Read(sh32, SizeOf(sh32));
        if x <> SizeOf(sh32) then Exit;
        if Endian = ELF_BIG_ENDIAN then SwapElfSectionHeader32Bytes(sh32);
        SectionHeaderTable32Entries.Add(sh32);
      end

    // 64 bit
    else
      for i := 1 to SectionHeaderTableEntries do
      begin
        if fs.Size < fs.Position + SizeOf(sh64) then Exit;
        x := fs.Read(sh64, SizeOf(sh64));
        if x <> SizeOf(sh64) then Exit;
        if Endian = ELF_BIG_ENDIAN then SwapElfSectionHeader64Bytes(sh64);
        SectionHeaderTable64Entries.Add(sh64);
      end;



    FIsValidElfFile := True;

    
    // --------------------------- UPX ------------------------------
    if fs.Size >= SizeOf(FUpxHeader) then
    begin
      fs.Position := fs.Size - SizeOf(FUpxHeader);
      x := fs.Read(FUpxHeader, SizeOf(FUpxHeader));
      if x = SizeOf(FUpxHeader) then
      begin
        if FEndian = ELF_BIG_ENDIAN then
        begin
          FUpxHeader.Header.u_len := SwapDword(FUpxHeader.Header.u_len);
          FUpxHeader.Header.c_len := SwapDword(FUpxHeader.Header.c_len);
          FUpxHeader.Header.u_adler := SwapDword(FUpxHeader.Header.u_adler);
          FUpxHeader.Header.c_adler := SwapDword(FUpxHeader.Header.c_adler);
          FUpxHeader.Header.UncompressedFileSize := SwapDword(FUpxHeader.Header.UncompressedFileSize);
        end;

        if JPL.UPX.IsValidUpxHeader(FUpxHeader.Header) then
        begin
          FUpxHeaderOffset := fs.Size - SizeOf(FUpxHeader);
          FIsPackedByUpx := True;
          FIsValidUpxHeader := True;
        end;
      end;
    end;







  finally
    fs.Free;
  end;


end;
  {$endregion ReadFileInfo}

procedure TElfFile.SetFileName(const Value: string);
begin
  FFileName := Value;
end;


  {$region '              AsString                      '}
function TElfFile.AsString: string;
const
  CRLF = #13#10;
  BoundLen = 36;
  BoundChar = '/';
  BoundSep = ' ';
  ShortLineChar = '-';
var
  s: string;
  xp1, hsPad, xNum: integer;
  uh: TUpxHeader;
  ufn: TUpxFormatName;
  pht32: TElfProgramHeader32;
  pht64: TElfProgramHeader64;
  sh32: TElfSectionHeader32;
  sh64: TElfSectionHeader64;


  function LineText(s: string): string;
  begin
    Result := AddBounds(s, BoundSep, BoundChar, BoundLen);
  end;

  function ShortLine(CharCount: integer = BoundLen): string;
  begin
    Result := StringOfChar(ShortLineChar, CharCount);
  end;

  procedure Line_B(FieldName: string; Value: Byte; Desc: string = '');
  var
    hs: string;
  begin
    //hs := InsertNumSep(Value.ToHexString(SizeOf(Value) * 2), ' ', 4);
    hs := InsertNumSep(IntToHex(Value, SizeOf(Value) * 2), ' ', 4);
    s := s + Pad(FieldName, xp1, ' ') + ':  hex: ' + Pad(hs, 2, ' ') + ' | dec: ' + Pad(IntToStrEx(Value), 3, ' ');
    if Desc <> '' then s := s + ' | ' + Desc;
    s := s + CRLF;
  end;

  procedure Line_W(FieldName: string; Value: Word; Desc: string = '');
  var
    hs: string;
  begin
    //hs := InsertNumSep(Value.ToHexString(SizeOf(Value) * 2), ' ', 4);
    hs := InsertNumSep(IntToHex(Value, SizeOf(Value) * 2), ' ', 4);
    s := s + Pad(FieldName, xp1, ' ') + ':  hex: ' + Pad(hs, hsPad, ' ') + ' | dec: ' + Pad(IntToStrEx(Value), 14, ' ');
    if Desc <> '' then s := s + ' | ' + Desc;
    s := s + CRLF;
  end;

  procedure Line_DW(FieldName: string; Value: DWORD; Desc: string = '');
  var
    hs: string;
  begin
    //hs := InsertNumSep(Value.ToHexString(SizeOf(Value) * 2), ' ', 4);
    hs := InsertNumSep(IntToHex(Value, SizeOf(Value) * 2), ' ', 4);
    s := s + Pad(FieldName, xp1, ' ') + ':  hex: ' + Pad(hs, hsPad, ' ') + ' | dec: ' + Pad(IntToStrEx(Value), 14, ' ');
    if Desc <> '' then s := s + ' | ' + Desc;
    s := s + CRLF;
  end;

  procedure Line_UI64(FieldName: string; Value: UInt64; Desc: string = '');
  var
    hs: string;
  begin
    //hs := InsertNumSep(Value.ToHexString(SizeOf(Value) * 2), ' ', 4);
    hs := InsertNumSep(IntToHex(Value, SizeOf(Value) * 2), ' ', 4);
    s := s + Pad(FieldName, xp1, ' ') + ':  hex: ' + Pad(hs, hsPad, ' ') + ' | dec: ' + Pad(IntToStrEx(Value), 14, ' ');
    if Desc <> '' then s := s + ' | ' + Desc;
    s := s + CRLF;
  end;

  procedure Line_S(FieldName, Value: string; Desc: string = '');
  begin
    s := s + Pad(FieldName, xp1, ' ') + ':  ' + Value;
    if Desc <> '' then s := s + ' | ' + Desc;
    s := s + CRLF;
  end;

begin
  Result := '';
  if FFileName = '' then Exit;
  if not IsValidElfFile then Exit;

  s := '';
  if Bits = 32 then hsPad := 10 else hsPad := 20;

  xp1 := 20;
  Line_S('File name', FFileName);
  Line_S('File size', IntToStrEx(FFileSize) + ' bytes  -  ' + GetFileSizeString(FFileSize));
  Line_S('Valid ELF file', BoolToStr(IsValidElfFile, 'Yes', 'No'));
  Line_S('Bits', IntToStr(Bits));
  Line_S('Endian', EndianStr); // EI_IDENT.EI_DATA
  //Line_S('ELF header version', ElfHeaderVersion.ToString); = EI_IDENT.EI_VERSION

  xp1 := 16;
  s := s + CRLF + LineText('IDENT') + CRLF;
  Line_S('E_IDENT', ELF_EIdentToStr(E_IDENT));
  Line_S('Magic', Magic);
  Line_B('EI_CLASS', E_IDENT.EI_CLASS, ELF_EiClassToStr(E_IDENT.EI_CLASS));
  Line_B('EI_DATA', E_IDENT.EI_DATA, ELF_EiDataToStr(E_IDENT.EI_DATA));
  Line_B('EI_VERSION', E_IDENT.EI_VERSION, 'ELF header version');
  Line_B('EI_OSABI', E_IDENT.EI_OSABI, OsAbiStr);
  Line_B('EI_ABIVERSION', E_IDENT.EI_ABIVERSION);
  Line_S('Padding bytes', PaddingBytesStr);


  if Bits = 32 then s := s + CRLF + LineText('ELF HEADER 32') + CRLF
  else s := s + CRLF + LineText('ELF HEADER 64') + CRLF;

  Line_W('e_type', FObjectType, FObjectTypeStr);
  Line_W('e_machine', FMachine, FMachineStr);
  Line_DW('e_version', FObjectFileVersion, '1 = original ELF version');
  if Bits = 32 then
  begin
    Line_DW('e_entry', FEntryPoint, 'Entry point');
    Line_DW('e_phoff', FProgramHeaderTableOffset, 'Program header table offset');
    Line_DW('e_shoff', FSectionHeaderTableOffset, 'Section header table offset');
  end
  else {if Bits = 64 then}
  begin
    Line_UI64('e_entry', FEntryPoint, 'Entry point');
    Line_UI64('e_phoff', FProgramHeaderTableOffset, 'Program header table offset');
    Line_UI64('e_shoff', FSectionHeaderTableOffset, 'Section header table offset');
  end;
  Line_DW('e_flags', FProcessorFlags, 'Processor-specific flags');
  Line_W('e_ehsize', FElfHeaderSize, 'ELF header size');
  Line_W('e_phentsize', FProgramHeaderTableEntrySize, 'Program header table size');
  Line_W('e_phnum', FProgramHeaderTableEntries, 'Program header table entries');
  Line_W('e_shentsize', FSectionHeaderTableEntrySize, 'Section header table entry size');
  Line_W('e_shnum', FSectionHeaderTableEntries, 'Section header table entries');
  Line_W('e_shstrndx', FSectionNamesEntryIndex, 'Index of the section header table entry with section names');

  if Self.ProgramHeaderTableEntries > 0 then
  begin

    xNum := 0;
    s := s + CRLF + LineText('Program Header Table') + CRLF;
    if Bits = 32 then
      for pht32 in Self.ProgramHeaderTable32Entries do
      begin
        s := s + '  Program Header Table - Entry index ' + itos(xNum) + CRLF;
        Line_DW('p_type', pht32.p_type, ELF_SegmentTypeToStr(pht32.p_type));
        Line_DW('p_offset', pht32.p_offset, 'Segment file offset');
        Line_DW('p_vaddr', pht32.p_vaddr, 'Segment virtual address');
        Line_DW('p_paddr', pht32.p_paddr, 'Segment physical address');
        Line_DW('p_filesz', pht32.p_filesz, 'Segment size in file');
        Line_DW('p_memsz', pht32.p_memsz, 'Segment size in memory');
        Line_DW('p_flags', pht32.p_flags, 'Segment permissions: ' + ELF_SegmentFlagToStr(pht32.p_flags));
        Line_DW('p_align', pht32.p_align, 'Segment alignment');
        s := s + '  ' + ShortLine(65) + CRLF;
        Inc(xNum);
      end


      else for pht64 in Self.ProgramHeaderTable64Entries do
      begin
        s := s + '  Program Header Table - Entry index ' + itos(xNum) + CRLF;
        Line_DW('p_type', pht64.p_type, ELF_SegmentTypeToStr(pht64.p_type));
        Line_DW('p_offset', pht64.p_offset, 'Segment file offset');
        Line_UI64('p_vaddr', pht64.p_vaddr, 'Segment virtual address');
        Line_UI64('p_paddr', pht64.p_paddr, 'Segment physical address');
        Line_UI64('p_filesz', pht64.p_filesz, 'Segment size in file');
        Line_UI64('p_memsz', pht64.p_memsz, 'Segment size in memory');
        Line_UI64('p_flags', pht64.p_flags, 'Segment permissions: ' + ELF_SegmentFlagToStr(pht64.p_flags));
        Line_UI64('p_align', pht64.p_align, 'Segment alignment');
        s := s + '  ' + ShortLine(65) + CRLF;
        Inc(xNum);
      end;
    
  end; // Program Header Table entries


  if Self.SectionHeaderTableEntries > 0 then
  begin

    xNum := 0;
    s := s + CRLF + LineText('Section Header Table') + CRLF;
    if Bits = 32 then
      for sh32 in SectionHeaderTable32Entries do
      begin
        s := s + '  Section Header Table - Entry index ' + itos(xNum) + CRLF;
        Line_DW('sh_name', sh32.sh_name);
        Line_DW('sh_type', sh32.sh_type);
        Line_DW('sh_flags', sh32.sh_flags);
        Line_DW('sh_addr', sh32.sh_addr);
        Line_DW('sh_offset', sh32.sh_offset);
        Line_DW('sh_size', sh32.sh_size);
        Line_DW('sh_link', sh32.sh_link);
        Line_DW('sh_info', sh32.sh_info);
        Line_DW('sh_addralign', sh32.sh_addralign);
        Line_DW('sh_entsize', sh32.sh_entsize);
        s := s + '  ' + ShortLine(65) + CRLF;
        Inc(xNum);
      end


//  PElfSectionHeader64 = ^TElfSectionHeader64;
//  TElfSectionHeader64 = packed record
//    sh_name: Elf64_Word;
//    sh_type: Elf64_Word;
//    sh_flags: Elf64_Xword;
//    sh_addr: Elf64_Addr;
//    sh_offset: Elf64_Off;
//    sh_size: Elf64_Xword;
//    sh_link: Elf64_Word;
//    sh_info: Elf64_Word;
//    sh_addralign: Elf64_Xword;
//    sh_entsize: Elf64_Xword;
//  end;

      else for sh64 in SectionHeaderTable64Entries do
      begin
        s := s + '  Section Header Table - Entry index ' + itos(xNum) + CRLF;
        Line_DW('sh_name', sh64.sh_name);
        Line_DW('sh_type', sh64.sh_type);
        Line_UI64('sh_flags', sh64.sh_flags);
        Line_UI64('sh_addr', sh64.sh_addr);
        Line_UI64('sh_offset', sh64.sh_offset);
        Line_UI64('sh_size', sh64.sh_size);
        Line_DW('sh_link', sh64.sh_link);
        Line_DW('sh_info', sh64.sh_info);
        Line_UI64('sh_addralign', sh64.sh_addralign);
        Line_UI64('sh_entsize', sh64.sh_entsize);
        s := s + '  ' + ShortLine(65) + CRLF;
        Inc(xNum);
      end;

  end;

  
  if IsPackedByUpx then
  begin
    xp1 := 16;
    hsPad := 10;
    s := s + CRLF + LineText('UPX') + CRLF;
    //xp1 := 10;
    //Line_S('UPX header offset', 'hex: ' + InsertNumSep(Self.UpxHeaderOffset.ToHexString(16), ' ', 4) + '  dec: ' + IntToStrEx(Self.UpxHeaderOffset));
    Line_S('UPX header offset', 'hex: ' + InsertNumSep( IntToHex(Self.UpxHeaderOffset, 16), ' ', 4) + '  dec: ' + IntToStrEx(Self.UpxHeaderOffset) );
    uh := FUpxHeader.Header;
    s := s + '  ---- UPX Header ---- ' + CRLF;
    Line_S('Magic', string(uh.UpxMagic));
    Line_B('version', uh.Compressor, 'Compressor version');
    GetUpxFormatName(uh.Format, ufn);
    Line_B('format', uh.Format, ufn.FullName + ' - ' + ufn.ShortName);
    Line_B('method', uh.Method, UpxCompressionMethodToStr(uh.Method));
    Line_B('level', uh.Level, 'Compression level (1..10)');
    Line_DW('u_len', uh.u_len);
    Line_DW('c_len', uh.c_len);
    Line_DW('u_adler', uh.u_adler);
    Line_DW('c_adler', uh.c_adler);
    Line_DW('u_file_size', uh.UncompressedFileSize, 'Uncompressed file size');
    Line_B('filter', uh.Filter1);
    Line_B('filter_cto', uh.Filter2);
    Line_B('n_mru', uh.n_mru);
    Line_B('header_checksum', uh.header_cheksum);
  end;


  Result := s;
end;
  {$endregion AsString}

{$endregion TElfFile}

end.
