unit JPL.Binary.Mach.Types;

// Jacek Pazera
// 03.2016
// https://en.wikipedia.org/wiki/Mach-O

// https://github.com/opensource-apple/xnu

// http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/EXTERNAL_HEADERS/mach-o/
// http://www.opensource.apple.com/source/xnu/xnu-3248.20.55/

// https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachORuntime/
// http://stackoverflow.com/questions/27669766/how-to-read-mach-o-header-from-object-file

// http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/EXTERNAL_HEADERS/mach-o/loader.h
// http://www.opensource.apple.com/source/xnu/xnu-3248.20.55/EXTERNAL_HEADERS/mach-o/loader.h

// http://www.opensource.apple.com/source/dtrace/dtrace-78/head/arch.h

// http://www.opensource.apple.com/source/xnu/xnu-792.13.8/osfmk/mach/machine.h
// http://www.opensource.apple.com/source/xnu/xnu-3248.20.55/osfmk/mach/machine.h

// http://www.opensource.apple.com/source/xnu/xnu-124.1/osfmk/mach/i386/machine_types.defs

// https://www.mikeash.com/pyblog/friday-qa-2012-11-30-lets-build-a-mach-o-executable.html


{$I .\..\jp.inc}

interface

uses
  Sysutils, Classes;



type
  cpu_subtype_t = Int32; // 32 bit signed int
  cpu_type_t = Int32; // 32 bit signed int
  // Int32 = Integer;
  // UInt32 = Cardinal = DWORD

const
  {$region ' ------ const ------ '}

  MACHO_LITTLE_ENDIAN = 1;
  MACHO_BIG_ENDIAN = 2;
  MACHO_UNKNOWN_ENDIAN = 0;
  MACHO_BITS_MULTIARCH = 32 + 64;


  FAT_MAGIC = $cafebabe;
  FAT_CIGAM = $bebafeca; // NXSwapLong(FAT_MAGIC)
  FAT_MAGIC_STR = 'CAFEBABE';
  FAT_CIGAM_STR = 'BEBAFECA';


  // Constant for the magic field of the mach_header (32-bit architectures)
  MH_MAGIC =	$feedface; // FE ED FA CE (4 277 009 102) // the mach magic number
  MH_CIGAM = $cefaedfe; // CE FA ED FE (3 472 551 422) // NXSwapInt(MH_MAGIC)
  MH_MAGIC_STR = 'FEEDFACE';
  MH_CIGAM_STR = 'CEFAEDFE';

  // Constant for the magic field of the mach_header_64 (64-bit architectures)
  MH_MAGIC_64 = $feedfacf; // FE ED FA CF (4 277 009 103) // the 64-bit mach magic number
  MH_CIGAM_64 = $cffaedfe; // CF FA ED FE (3 489 328 638) // NXSwapInt(MH_MAGIC_64)
  MH_MAGIC_64_STR = 'FEEDFACF';
  MH_CIGAM_64_STR = 'CFFAEDFE';


//  * The layout of the file depends on the filetype.  For all but the MH_OBJECT
//  * file type the segments are padded out and aligned on a segment alignment
//  * boundary for efficient demand pageing.  The MH_EXECUTE, MH_FVMLIB, MH_DYLIB,
//  * MH_DYLINKER and MH_BUNDLE file types also have the headers included as part
//  * of their first segment.
//  *
//  * The file type MH_OBJECT is a compact format intended as output of the
//  * assembler and input (and possibly output) of the link editor (the .o
//  * format).  All sections are in one unnamed segment with no segment padding.
//  * This format is used as an executable format when the file is so small the
//  * segment padding greatly increases its size.
//  *
//  * The file type MH_PRELOAD is an executable format intended for things that
//  * are not executed under the kernel (proms, stand alones, kernels, etc).  The
//  * format can be executed under the kernel but may demand paged it and not
//  * preload it before execution.
//  *
//  * A core file is in MH_CORE format and can be any in an arbritray legal
//  * Mach-O file.

  // ------------------------------- File type ----------------------------------------
  // Constants for the filetype field of the mach_header
  MH_OBJECT = $1; // relocatable object file
  MH_EXECUTE = $2; // demand paged executable file
  MH_FVMLIB = $3; // fixed VM shared library file
  MH_CORE = $4; // core file
  MH_PRELOAD = $5; // preloaded executable file
  MH_DYLIB = $6; // dynamically bound shared library
  MH_DYLINKER = $7; // dynamic link editor
  MH_BUNDLE = $8; // dynamically bound bundle file
  MH_DYLIB_STUB = $9; // shared library stub for static linking only, no section contents
  MH_DSYM = $a; // companion file with only debug sections
  MH_KEXT_BUNDLE = $b; // x86_64 kexts


  // ------------------------------- Flags ---------------------------------------
  // Constants for the flags field of the mach_header
  MH_NOUNDEFS = $1; // the object file has no undefined references
  MH_INCRLINK = $2; // the object file is the output of an incremental link against a base file and can't be link edited again
  MH_DYLDLINK = $4; // the object file is input for the dynamic linker and can't be staticly link edited again
  MH_BINDATLOAD = $8; // the object file's undefined references are bound by the dynamic linker when loaded.
  MH_PREBOUND = $10; // the file has its dynamic undefined references prebound.
  MH_SPLIT_SEGS = $20; // the file has its read-only and read-write segments split
  MH_LAZY_INIT = $40; // the shared library init routine is to be run lazily via catching memory faults to its writeable segments (obsolete)
  MH_TWOLEVEL = $80; // the image is using two-level name space bindings
  MH_FORCE_FLAT = $100; // the executable is forcing all images to use flat name space bindings
  MH_NOMULTIDEFS = $200; // this umbrella guarantees no multiple defintions of symbols in its sub-images so the two-level namespace hints can always be used.
  MH_NOFIXPREBINDING = $400; // do not have dyld notify the prebinding agent about this executable
  MH_PREBINDABLE = $800; // the binary is not prebound but can have its prebinding redone. only used when MH_PREBOUND is not set.
  MH_ALLMODSBOUND = $1000; // indicates that this binary binds to all two-level namespace modules of its dependent libraries. Only used when MH_PREBINDABLE and MH_TWOLEVEL are both set.
  MH_SUBSECTIONS_VIA_SYMBOLS = $2000; // safe to divide up the sections into sub-sections via symbols for dead code stripping
  MH_CANONICAL = $4000; // the binary has been canonicalized via the unprebind operation
  MH_WEAK_DEFINES = $8000; // the final linked image contains external weak symbols
  MH_BINDS_TO_WEAK = $10000; // the final linked image uses weak symbols
  MH_ALLOW_STACK_EXECUTION = $20000; // When this bit is set, all stacks in the task will be given stack execution privilege. Only used in MH_EXECUTE filetypes.
  MH_DEAD_STRIPPABLE_DYLIB = $400000; // Only for use on dylibs. When linking against a dylib that has this bit set, the static linker will automatically not create a
                                      // LC_LOAD_DYLIB load command to the dylib if no symbols are being referenced from the dylib.
  MH_ROOT_SAFE = $40000; // When this bit is set, the binary declares it is safe for use in processes with uid zero
  MH_SETUID_SAFE = $80000; // When this bit is set, the binary declares it is safe for use in processes when issetugid() is true
  MH_NO_REEXPORTED_DYLIBS = $100000; // When this bit is set on a dylib, the static linker does not need to examine dependent dylibs to see if any are re-exported
  MH_PIE = $200000; // When this bit is set, the OS will load the main executable at a random address. Only used in MH_EXECUTE filetypes.

  MH_HAS_TLV_DESCRIPTORS = $800000; // Contains a section of type S_THREAD_LOCAL_VARIABLES
  MH_NO_HEAP_EXECUTION = $1000000; // When this bit is set, the OS will run the main executable with a non-executable heap even on platforms (e.g. i386) that don't
                                   // require it. Only used in MH_EXECUTE filetypes.
  MH_APP_EXTENSION_SAFE = $02000000; // The code was linked for use in an application extension.


  // ----------------------------------- CPU type ---------------------------------------------
  // * Capability bits used in the definition of cpu_type.
  CPU_ARCH_MASK = $ff000000; // mask for architecture bits
  CPU_ARCH_ABI64 = $01000000; // 64 bit ABI

  // *	Machine types known by all.

  CPU_TYPE_ANY = cpu_type_t(-1);
  CPU_TYPE_VAX = cpu_type_t(1);
  //  /* skip				((cpu_type_t) 2)	*/
  //  /* skip				((cpu_type_t) 3)	*/
  //  /* skip				((cpu_type_t) 4)	*/
  //  /* skip				((cpu_type_t) 5)	*/
  CPU_TYPE_MC680x0 = cpu_type_t(6);
  CPU_TYPE_X86 = cpu_type_t(7);
  CPU_TYPE_I386 = CPU_TYPE_X86; // compatibility
  CPU_TYPE_X86_64 = CPU_TYPE_X86 or CPU_ARCH_ABI64;

  //  /* skip CPU_TYPE_MIPS		((cpu_type_t) 8)	*/
  //  /* skip 			((cpu_type_t) 9)	*/
  CPU_TYPE_MC98000 = cpu_type_t(10);
  CPU_TYPE_HPPA = cpu_type_t(11);
  CPU_TYPE_ARM = cpu_type_t(12);
  CPU_TYPE_ARM64 = CPU_TYPE_ARM or CPU_ARCH_ABI64;
  CPU_TYPE_MC88000 = cpu_type_t(13);
  CPU_TYPE_SPARC = cpu_type_t(14);
  CPU_TYPE_I860 = cpu_type_t(15);
  //  /* skip	CPU_TYPE_ALPHA		((cpu_type_t) 16)	*/
  //  /* skip				((cpu_type_t) 17)	*/
  CPU_TYPE_POWERPC = cpu_type_t(18);
  CPU_TYPE_POWERPC64 = CPU_TYPE_POWERPC or CPU_ARCH_ABI64;



  // ---------------------------- CPU subtype ------------------------------------
  //  *	Machine subtypes (these are defined here, instead of in a machine
  //  *	dependent directory, so that any program can get all definitions
  //  *	regardless of where is it compiled).

  // * Capability bits used in the definition of cpu_subtype.
  CPU_SUBTYPE_MASK = $ff000000; // mask for feature flags
  CPU_SUBTYPE_LIB64 = $80000000; // 64 bit libraries




  // ------------------------------- Load Commands -------------------------------------
  //  /*
  //  * After MacOS X 10.1 when a new load command is added that is required to be
  //  * understood by the dynamic linker for the image to execute properly the
  //  * LC_REQ_DYLD bit will be or'ed into the load command constant.  If the dynamic
  //  * linker sees such a load command it it does not understand will issue a
  //  * "unknown load command required for execution" error and refuse to use the
  //  * image.  Other load commands without this bit that are not understood will
  //  * simply be ignored.
  //  */
  LC_REQ_DYLD = $80000000;

  // Constants for the cmd field of all load commands, the type
  LC_SEGMENT = $1; // segment of this file to be mapped
  LC_SYMTAB = $2; // link-edit stab symbol table info
  LC_SYMSEG = $3; // link-edit gdb symbol table info (obsolete)
  LC_THREAD = $4; // thread
  LC_UNIXTHREAD = $5; // unix thread (includes a stack)
  LC_LOADFVMLIB = $6; // load a specified fixed VM shared library
  LC_IDFVMLIB = $7; // fixed VM shared library identification
  LC_IDENT = $8; // object identification info (obsolete)
  LC_FVMFILE = $9; // fixed VM file inclusion (internal use)
  LC_PREPAGE = $a; // prepage command (internal use)
  LC_DYSYMTAB = $b; // dynamic link-edit symbol table info
  LC_LOAD_DYLIB = $c; // load a dynamically linked shared library
  LC_ID_DYLIB = $d; // dynamically linked shared lib ident
  LC_LOAD_DYLINKER = $e; // load a dynamic linker
  LC_ID_DYLINKER = $f; // dynamic linker identification
  LC_PREBOUND_DYLIB = $10; // modules prebound for a dynamically linked shared library
  LC_ROUTINES = $11; // image routines
  LC_SUB_FRAMEWORK = $12; // sub framework
  LC_SUB_UMBRELLA = $13; // sub umbrella
  LC_SUB_CLIENT = $14; // sub client
  LC_SUB_LIBRARY = $15; // sub library
  LC_TWOLEVEL_HINTS = $16; // two-level namespace lookup hints
  LC_PREBIND_CKSUM = $17; // prebind checksum


  /// load a dynamically linked shared library that is allowed to be missing
  /// (all symbols are weak imported).
  // LC_LOAD_WEAK_DYLIB (0x18 | LC_REQ_DYLD)
  LC_LOAD_WEAK_DYLIB = $18 or LC_REQ_DYLD;

  LC_SEGMENT_64 = $19; // 64-bit segment of this file to be mapped
  LC_ROUTINES_64 = $1a; // 64-bit image routines
  LC_UUID = $1b; // the uuid
  // LC_RPATH       (0x1c | LC_REQ_DYLD) // runpath additions
  LC_RPATH = $1c or LC_REQ_DYLD; // runpath additions

  LC_CODE_SIGNATURE = $1d; // local of code signature
  LC_SEGMENT_SPLIT_INFO = $1e; // local of info to split segments

  // LC_REEXPORT_DYLIB (0x1f | LC_REQ_DYLD) // load and re-export dylib
  LC_REEXPORT_DYLIB = $1f or LC_REQ_DYLD; // load and re-export dylib

  LC_LAZY_LOAD_DYLIB = $20; // delay load of dylib until first use
  LC_ENCRYPTION_INFO = $21; // encrypted segment information
  LC_DYLD_INFO = $22; // compressed dyld information

  // LC_DYLD_INFO_ONLY (0x22|LC_REQ_DYLD)	// compressed dyld information only
  // LC_LOAD_UPWARD_DYLIB (0x23 | LC_REQ_DYLD) // load upward dylib
  LC_DYLD_INFO_ONLY = $22 or LC_REQ_DYLD;	// compressed dyld information only
  LC_LOAD_UPWARD_DYLIB = $23 or LC_REQ_DYLD; // load upward dylib

  LC_VERSION_MIN_MACOSX = $24; // build for MacOSX min OS version
  LC_VERSION_MIN_IPHONEOS = $25; // build for iPhoneOS min OS version
  LC_FUNCTION_STARTS = $26; // compressed table of function start addresses
  LC_DYLD_ENVIRONMENT = $27; // string for dyld to treat like environment variable

  // LC_MAIN (0x28|LC_REQ_DYLD) // replacement for LC_UNIXTHREAD
  LC_MAIN = $28 or LC_REQ_DYLD; // replacement for LC_UNIXTHREAD

  LC_DATA_IN_CODE = $29; // table of non-instructions in __text
  LC_SOURCE_VERSION = $2A; // source version used to build binary
  LC_DYLIB_CODE_SIGN_DRS = $2B; // Code signing DRs copied from linked dylibs
  LC_ENCRYPTION_INFO_64 = $2C; // 64-bit encrypted segment information
  LC_LINKER_OPTION = $2D; // linker options in MH_OBJECT files
  LC_LINKER_OPTIMIZATION_HINT = $2E; // optimization hints in MH_OBJECT files
  LC_VERSION_MIN_WATCHOS = $30; // build for Watch min OS version

  {$endregion const}


type
  {$region ' ------- types -------- '}

  // ----------------------------------------- FAT Architecture ---------------------------------------------------

//  fat.h
//  * This header file describes the structures of the file format for "fat" architecture specific file (wrapper design). At the begining of the file
//  * there is one fat_header structure followed by a number of fat_arch structures. For each architecture in the file, specified by a pair of
//  * cputype and cpusubtype, the fat_header describes the file offset, file size and alignment in the file of the architecture specific member.
//  * The padded bytes in the file to place each member on it's specific alignment are defined to be read as zeros and can be left as "holes" if the file system
//  * can support them as long as they read as zeros.
//  * All structures defined here are always written and read to/from disk in big-endian order.

//  struct fat_header {
//    uint32_t	magic;		/* FAT_MAGIC */
//    uint32_t	nfat_arch;	/* number of structs that follow */
//  };
  fat_header = record
    magic: UInt32; // FAT_MAGIC
    nfat_arch: UInt32; // number of structs that follow
  end;

//  struct fat_arch {
//    cpu_type_t	cputype;	/* cpu specifier (int) */
//    cpu_subtype_t	cpusubtype;	/* machine specifier (int) */
//    uint32_t	offset;		/* file offset to this object file */
//    uint32_t	size;		/* size of this object file */
//    uint32_t	align;		/* alignment as a power of 2 */
//  };
  fat_arch = record
    cputype: Int32; // cpu specifier (int)
    cpusubtype: Int32; // machine specifier (int)
    offset: UInt32; // file offset to this object file
    size: UInt32; // size of this object file
    align: UInt32; // file offset to this object file
  end;


  // ----------------------------------------------- Mach-O -----------------------------------------------------
//  machine_types.defs
//    type integer_t = int32;
//
//  machine.h
//    typedef integer_t	cpu_type_t;
//    typedef integer_t	cpu_subtype_t;


//   * The 32-bit mach header appears at the very beginning of the object file for
//   * 32-bit architectures.
//  struct mach_header {
//    uint32_t	magic;		/* mach magic number identifier */
//    cpu_type_t	cputype;	/* cpu specifier */
//    cpu_subtype_t	cpusubtype;	/* machine specifier */
//    uint32_t	filetype;	/* type of file */
//    uint32_t	ncmds;		/* number of load commands */
//    uint32_t	sizeofcmds;	/* the size of all the load commands */
//    uint32_t	flags;		/* flags */
//  };
  // SizeOf(mach_header) = 28
  mach_header = packed record
    magic: UInt32; // mach magic number identifier
    cputype: Int32; // cpu specifier
    cpusubtype: Int32; // machine specifier
    filetype: UInt32; // type of file
    ncmds: UInt32; // number of load commands
    sizeofcmds: UInt32; // the size of all the load commands
    flags: UInt32; // flags
  end;

//   * The 64-bit mach header appears at the very beginning of object files for
//   * 64-bit architectures.
//  struct mach_header_64 {
//    uint32_t	magic;		/* mach magic number identifier */
//    cpu_type_t	cputype;	/* cpu specifier */
//    cpu_subtype_t	cpusubtype;	/* machine specifier */
//    uint32_t	filetype;	/* type of file */
//    uint32_t	ncmds;		/* number of load commands */
//    uint32_t	sizeofcmds;	/* the size of all the load commands */
//    uint32_t	flags;		/* flags */
//    uint32_t	reserved;	/* reserved */
//  };
  // SizeOf(mach_header_64) = 32
  mach_header_64 = packed record
    magic: UInt32;
    cputype: Int32;
    cpusubtype: Int32;
    filetype: UInt32;
    ncmds: UInt32;
    sizeofcmds: UInt32;
    flags: UInt32;
    reserved: UInt32; // reserved
  end;


//  /*
//  * The load commands directly follow the mach_header.  The total size of all
//  * of the commands is given by the sizeofcmds field in the mach_header.  All
//  * load commands must have as their first two fields cmd and cmdsize.  The cmd
//  * field is filled in with a constant for that command type.  Each command type
//  * has a structure specifically for it.  The cmdsize field is the size in bytes
//  * of the particular load command structure plus anything that follows it that
//  * is a part of the load command (i.e. section structures, strings, etc.).  To
//  * advance to the next load command the cmdsize can be added to the offset or
//  * pointer of the current load command.  The cmdsize for 32-bit architectures
//  * MUST be a multiple of 4 bytes and for 64-bit architectures MUST be a multiple
//  * of 8 bytes (these are forever the maximum alignment of any load commands).
//  * The padded bytes must be zero.  All tables in the object file must also
//  * follow these rules so the file can be memory mapped.  Otherwise the pointers
//  * to these tables will not work well or at all on some machines.  With all
//  * padding zeroed like objects will compare byte for byte.
//  */
//  struct load_command {
//    uint32_t cmd;		/* type of load command */
//    uint32_t cmdsize;	/* total size of command in bytes */
//  };

  load_command = record
    cmd: UInt32; // type of load command
    cmdsize: UInt32; // total size of command (łącznie ze strukturą load_command)
  end;

  {$endregion types}





implementation



end.
