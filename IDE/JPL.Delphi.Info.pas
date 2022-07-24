unit JPL.Delphi.Info;

{

  The unit reads information about installed versions of Delphi (mainly from the system registry,
  some of the data from the file system).

  ----------------------------------------------------------------------------------

  `$RegSoft` = `HKEY_CURRENT_USER\Software`  OR  `HKEY_LOCAL_MACHINE\Software`

| Delphi        | Year | BDS ver. | Product ver. | Compiler ver. | DPROJ ProjectVersion   | Registry path                     |
| :------------ | ---- | :------: | :----------: | :-----------: | :--------------------- | :-------------------------------- |
| 1             | 1995 |          |              |      N/A      |                        |                                   |
| 2             | 1996 |          |              |      N/A      |                        |                                   |
| 3             | 1997 |          |              |      N/A      |                        |                                   |
| 4             | 1998 |          |              |      N/A      |                        |                                   |
| 5             | 1999 |          |      5       |      N/A      |                        | `$RegSoft`\Borland\Delphi\5.0     |
| 6             | 2001 |          |      6       |     14.0      |                        | `$RegSoft`\Borland\Delphi\6.0     |
| 7             | 2002 |          |      7       |     15.0      |                        | `$RegSoft`\Borland\Delphi\7.0     |
| 8 (.NET)      | 2003 |   2.0    |      8       |     16.0      |                        | `$RegSoft`\Borland\Delphi\8.0 (?) |
| 2005          | 2005 |   3.0    |      9       |     17.0      |                        | `$RegSoft`\Borland\BDS\3.0        |
| 2006          | 2005 |   4.0    |      10      |     18.0      |                        | `$RegSoft`\Borland\BDS\4.0        |
| 2007 (Win32)  | 2007 |   5.0    |      11      |     18.5      |                        | `$RegSoft`\Borland\BDS\5.0        |
| 2007 (.NET )  | 2007 |   5.0    |      11      |     19.0      |                        | `$RegSoft`\Borland\BDS\5.0        |
| 2009          | 2009 |   6.0    |      12      |     20.0      | 12.0                   | `$RegSoft`\CodeGear\BDS\6.0       |
| 2010          | 2009 |   7.0    |      14      |     21.0      | 12.0                   | `$RegSoft`\CodeGear\BDS\7.0       |
| XE            | 2010 |   8.0    |      15      |     22.0      | 12.2, 12.3             | `$RegSoft`\Embarcadero\BDS\8.0    |
| XE2           | 2011 |   9.0    |      16      |     23.0      | 13.4                   | `$RegSoft`\Embarcadero\BDS\9.0    |
| XE3           | 2012 |   10.0   |      17      |     24.0      | 14.3, 14.4             | `$RegSoft`\Embarcadero\BDS\10.0   |
| XE4           | 2013 |   11.0   |      18      |     25.0      | 14.4, 14.6             | `$RegSoft`\Embarcadero\BDS\11.0   |
| XE5           | 2013 |   12.0   |      19      |     26.0      | 15.0 (?), 15.1, 15.3   | `$RegSoft`\Embarcadero\BDS\12.0   |
| AppMethod     |      |   13.0   |      ?       |       ?       | 15.2                   | `$RegSoft`\Embarcadero\BDS\13.0   |
| XE6           | 2014 |   14.0   |      20      |     27.0      | 15.4                   | `$RegSoft`\Embarcadero\BDS\14.0   |
| XE7           | 2014 |   15.0   |      21      |     28.0      | 16.0, 16.1             | `$RegSoft`\Embarcadero\BDS\15.0   |
| XE8           | 2015 |   16.0   |      22      |     29.0      | 17.0, 17.1, 17.2       | `$RegSoft`\Embarcadero\BDS\16.0   |
| 10.0 Seattle  | 2015 |   17.0   |      23      |     30.0      | 18.0, 18.1             | `$RegSoft`\Embarcadero\BDS\17.0   |
| 10.1 Berlin   | 2016 |   18.0   |      24      |     31.0      | 18.1, 18.2             | `$RegSoft`\Embarcadero\BDS\18.0   |
| 10.2 Tokyo    | 2017 |   19.0   |      25      |     32.0      | 18.2, 18.3, 18.4       | `$RegSoft`\Embarcadero\BDS\19.0   |
| 10.3 Rio      | 2018 |   20.0   |      26      |     33.0      | 18.5, 18.6, 18.7, 18.8 | `$RegSoft`\Embarcadero\BDS\20.0   |
| 10.4 Sydney   | 2020 |   21.0   |      27      |     34.0      | 19.0, 19.1, 19.2       | `$RegSoft`\Embarcadero\BDS\21.0   |
| 11 Alexandria | 2021 |   22.0   |      28      |     35.0      | 19.3, 19.4             | `$RegSoft`\Embarcadero\BDS\22.0   |

  AppMethod (free plan) installed on 2022.07.01 - ProductVersion = 22 (???)

  BDS 1.0 - C# Builder

  Delphi 2005 Personal - Product version: 9.0.1
  Turbo Delphi 2006 Explorer - Product version: 10.0.3

  See also:
  https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Compiler_Versions
  https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Conditional_compilation_(Delphi)
  http://docwiki.embarcadero.com/RADStudio/Alexandria/en/Delphi_Compiler_Directives_(List)_Index

  http://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions
  http://docwiki.embarcadero.com/RADStudio/Sydney/en/Conditional_compilation_(Delphi)
  http://docwiki.embarcadero.com/RADStudio/Sydney/en/Delphi_Compiler_Directives_(List)_Index

  https://delphi.fandom.com/wiki/How_to_find_out_which_Delphi_version_was_used_to_create_a_project%3F

  ---------------------------------------------------------------------------------------------

  (from Delphi 2007 readme.htm)
  Release Notes for CodeGear RAD Studio 2007

  If you are a VCL Component Vendor and your component installer updates paths in the registry to include paths to your components, there is one
  additional registry key your installer should update in HKEY_LOCAL_MACHINE under: Software\Borland\BDS\5.0\Globals

  Add (or update if it already exists) a string value called ForceEnvOptionsUpdate. Assign it a string value of "1." When this registry key
  has a value of "1," the next time the IDE runs, it updates the EnvOptions.proj file on disk to include the path(s) that your installer added.
  The EnvOptions.proj file is the mechanism by which the new MSBuild build engine in the IDE is able to include paths that are listed
  on the Library - Win32 page in the IDE's Tools>Options dialog.

  If your installer updates any of the following registry keys, it should also add or update the Globals\ForceEnvOptionsUpdate key:

  Software\Borland\BDS\5.0\Library\Browsing Path
  Software\Borland\BDS\5.0\Library\Debug DCU Path
  Software\Borland\BDS\5.0\Library\Namespace Search Path
  Software\Borland\BDS\5.0\Library\Package DCP Output
  Software\Borland\BDS\5.0\Library\Package DPL Output
  Software\Borland\BDS\5.0\Library\Search Path

  ---------------------------------------------------------------------------------------------


  http://docwiki.embarcadero.com/RADStudio/Sydney/en/Environment_Variables
  BDS is one of the variables displayed in this dialog box. The **BDS environment variable represents the installed location of RAD Studio.**

}


  // Done: DPROJ ProjectVersion
  // TODO: Experts
  // TODO: Closed Files
  // TODO: Closed Projects
  // TODO: save to INI




{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}


interface

{$IFDEF MSWINDOWS} // Unit for Windows only!

uses
  // WinAPI
  Windows, Registry,

  // System
  SysUtils, Classes, Generics.Collections, IniFiles, Types, StrUtils,

  // VCL
  Graphics,

  // JPLib
  //JPL.Strings,
  JPL.TStr, JPL.Conversion, JPL.Containers.MultiValueList,
  JPL.Win.System, JPL.Win.FileSystem,
  JPL.Win.Registry;



const

  {$IFDEF CPU32}
  REG_ACCES_FLAG_READ_ONLY = KEY_READ or KEY_WOW64_64KEY;
  {$ELSE}
  REG_ACCES_FLAG_READ_ONLY = KEY_READ or KEY_WOW64_32KEY;
  {$ENDIF}

  // reg root key: HKEY_CURRENT_USER  or  HKEY_LOCAL_MACHINE
  REG_PATH_EMBT_BDS = 'Software\Embarcadero\BDS';
  REG_PATH_CODEGEAR_BDS = 'Software\CodeGear\BDS';
  REG_PATH_BORLAND_BDS = 'Software\Borland\BDS';

  REG_KEY_USER_VARS = 'Environment Variables';
  REG_KEY_LIBRARY = 'Library';
  REG_KEY_INSTALLED_UPDATES = 'InstalledUpdates';
  REG_KEY_IDE_PACKAGES = 'Known IDE Packages';
  REG_KEY_IDE_DELPHI_PACKAGES = REG_KEY_IDE_PACKAGES + '\Delphi';
  REG_KEY_IDE_CBUILDER_PACKAGES = REG_KEY_IDE_PACKAGES + '\CBuilder';
  REG_KEY_PACKAGES = 'Known Packages';
  REG_KEY_DISABLED_PACKAGES = 'Disabled Packages';
  REG_KEY_DISABLED_IDE_PACKAGES = 'Disabled IDE Packages';
  REG_KEY_PERSONALITIES = 'Personalities';
  REG_KEY_EDITOR_HIGHLIGHT = 'Editor\Highlight';


  COMPILER_EXE_DELPHI_WIN32 = 'dcc32.exe';
  COMPILER_EXE_DELPHI_WIN64 = 'dcc64.exe';
  COMPILER_EXE_DELPHI_OSX32 = 'dccosx.exe';
  COMPILER_EXE_DELPHI_OSX64 = 'dccosx64.exe';
  COMPILER_EXE_DELPHI_LINUX64 = 'dcclinux64.exe';
  COMPILER_EXE_DELPHI_IOS32 = 'dcciosarm.exe';
  COMPILER_EXE_DELPHI_IOS64 = 'dcciosarm64.exe';
  COMPILER_EXE_DELPHI_IOS_SIMULATOR32 = 'dccios32.exe';
  COMPILER_EXE_DELPHI_ANDROID32 = 'dccaarm.exe';
  COMPILER_EXE_DELPHI_ANDROID64 = 'dccaarm64.exe';

  COMPILER_EXE_CPP_WIN32_CLANG = 'bcc32c.exe';
  COMPILER_EXE_CPP_WIN32 = 'bcc32.exe';
  COMPILER_EXE_CPP_WIN64 = 'bcc64.exe';
  COMPILER_EXE_CPP_OSX32 = 'bccosx.exe';
  COMPILER_EXE_CPP_IOS32 = 'bcciosarm.exe';
  COMPILER_EXE_CPP_IOS64 = 'bcciosarm64.exe';
  COMPILER_EXE_CPP_ANDROID32 = 'bccaarm.exe';


  ArrHighlightKeyNames: array[0..39] of string = (
    'Additional search match highlight', 'Assembler', 'Attribute Names', 'Attribute Values', 'Brace Highlight', 'Character',
    'Code folding tree', 'Comment', 'Diff addition', 'Diff deletion', 'Diff move', 'Disabled break', 'Enabled break',
    'Error line', 'Execution point', 'Float', 'Folded code', 'Hex', 'Hot Link', 'Identifier', 'Illegal Char',
    'Invalid break', 'Line Highlight', 'Line Number', 'Marked block', 'Modified line', 'Number', 'Octal', 'Plain text',
    'Preprocessor', 'Reserved word', 'Right margin', 'Scripts', 'Search match', 'String', // 'Structural Highlighting' - Castalia ?,
    'Symbol', 'Sync edit background', 'Sync edit highlight', 'Tags', 'Whitespace'
  );

  CSIDL_COMMON_DOCUMENTS = $002e; // Win10: C:\Users\Public\Documents
  CSIDL_PERSONAL = $0005;         // Win10: C:\Users\<user>\Documents
  CSIDL_MYDOCUMENTS = CSIDL_PERSONAL;


type

  TDelphiVersion = (
    dvUnknown, dv7, dv2005, dv2006, dv2007, dv2009, dv2010,
    dvXE, dvXE2, dvXE3, dvXE4, dvXE5, dvAppMethod, dvXE6, dvXE7, dvXE8,
    dvSeattle, dvBerlin, dvTokyo, dvRio, dvSydney, dvAlexandria
  );

  TBdsVersion = (
    bdsvUnknown,
    bdsv2,  // Delphi 7
    bdsv3,  // 2005
    bdsv4,  // 2006
    bdsv5,  // 2007
    bdsv6,  // 2009
    bdsv7,  // 2010
    bdsv8,  // XE
    bdsv9,  // XE2
    bdsv10, // XE3
    bdsv11, // XE4
    bdsv12, // XE5
    bdsv13, // AppMethod
    bdsv14, // XE6
    bdsv15, // XE7
    bdsv16, // XE8
    bdsv17, // 10.0 Seattle
    bdsv18, // 10.1 Berlin
    bdsv19, // 10.2 Tokyo
    bdsv20, // 10.3 Rio
    bdsv21, // 10.4 Sydney
    bdsv22  // 11.0 Alexandria
  );


  TDelphiVersions = record
    First: TDelphiVersion;
    Second: TDelphiVersion;
    procedure Clear;
    function AsString(const UseLongNames: Boolean = False; const RemoveBrandName: Boolean = False): string;
    function FirstAsString(const UseLongName: Boolean = False; const RemoveBrandName: Boolean = False): string;
    class function DelphiVersionToStr(const dv: TDelphiVersion; const UseLongName: Boolean = False; const RemoveBrandName: Boolean = False): string; static;
  end;


  TDelphiEditorHighlightFormat = (dehfIni, dehfJson, dehfXml, dehfReg);

  TDelphiHighlightItem = record
    Enabled: Boolean;
    RegKeyName: string;
    DisplayName: string;
    ConfigItemName: string;
    DefaultBackground: Boolean;
    BackgroundColorNew: TColor;
    DefaultForeground: Boolean;
    ForegroundColorNew: TColor;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    procedure Clear(bClearRegKeyName: Boolean = True; bClearDisplayName: Boolean = True);
    function AsIniStr: string;
    function AsRegStr(RootKeyStr: string = 'HKEY_CURRENT_USER'): string;
    function AsJsonStr(Indent: string = '  '): string;
    function AsXmlStr(Indent: string = '  '): string;
    procedure LoadFromIni(const Ini: TIniFile);
  end;

  TDelphiEditorHighlightList = TList<TDelphiHighlightItem>;

  TDelphiTargetPlatform = (
    dtpUnknown, dtpAndroid32, dtpAndroid64, dtpLinux32, dtpLinux64, dtpiOSDevice32, dtpiOSDevice64, dtpiOSSimulator,
    dtpOSX32, dtpOSX64, dtpWin32, dtpWin64
  );


  TDelphiPackageItem = record
    Description: string;
    RegistryFileName: string;    // File name read from the Registry (not expanded, with user/IDE variables)
    ExpandedFileName: string;    // File name with expanded variables (real file name)
    FileExists: Boolean;
    function IsExpanded: Boolean;
    function AsString(const RegistryValue: Boolean = True; const ExpandedValue: Boolean = False; const RegExpandedValueSeparator: string = ' :: '): string;
  end;

  TDelphiPackageList = class(TList<TDelphiPackageItem>)
  private
    FNonExistentPackages: integer;
    FExistentPackages: integer;
  protected
    procedure UpdateStats;
  public
    function AsArray(const RegistryValues: Boolean = True; const ExpandedValues: Boolean = False; const RegExpandedValSeparator: string =' :: '): TStringDynArray;

    property NonExistentPackages: integer read FNonExistentPackages;
    property ExistentPackages: integer read FExistentPackages;
  end;


  TDelphiPathItem = record
    RegistryPath: string;   // Path read from the Registry (not expanded, with user/IDE variables)
    ExpandedPath: string;   // Path with expanded variables (real directory name)
    PathExists: Boolean;
    procedure Clear;
    function IsExpanded: Boolean;
    function AsString(const RegistryValue: Boolean = True; const ExpandedValue: Boolean = False; const RegExpandedValueSeparator: string = ' -> '): string;
  end;

  TDelphiPathList = class(TList<TDelphiPathItem>)
  private
    FNonExistentPaths: integer;
    FExistentPaths: integer;
    FRegKeyName: string;
  protected
    procedure UpdateStats;
    procedure SetRegKeyName(const ARegKeyName: string);
  public
    function AsArray(const RegistryValues: Boolean = True; const ExpandedValues: Boolean = False; const RegExpandedValueSeparator: string =' -> '): TStringDynArray;

    property NonExistentPaths: integer read FNonExistentPaths;
    property ExistentPaths: integer read FExistentPaths;
    property RegKeyName: string read FRegKeyName;
  end;

  TDelphiTargetLibraryPaths = class
  private const
    REG_VAL_LIBRARY_PATH = 'Search Path';
    REG_VAL_BROWSING_PATH = 'Browsing Path';
    REG_VAL_DEBUG_DCU_PATH = 'Debug DCU Path';
    REG_VAL_BPL_OUTPUT_DIR = 'Package DPL Output';
    REG_VAL_DCP_OUTPUT_DIR = 'Package DCP Output';
    REG_VAL_HPP_OUTPUT_DIR = 'HPP Output Directory';
  private
    FTargetPlatform: TDelphiTargetPlatform;
    FTargetPlatformName: string;
    FRegPath: string;
    FRegFullPath: string;
    FRegRoot: HKEY;
    FUserVarList: TMultiValueList;
    FIDEVarList: TMultiValueList;
    FLibraryPath: TDelphiPathItem;
    FLibraryPathList: TDelphiPathList;
    FBrowsingPath: TDelphiPathItem;
    FBrowsingPathList: TDelphiPathList;
    FPackageOutputDir: TDelphiPathItem;
    FDcpOutputDir: TDelphiPathItem;
    FDebugDcuPath: TDelphiPathItem;
    FDebugDcuPathList: TDelphiPathList;
    FTargetExists: Boolean;
    FHppOutputDir: TDelphiPathItem;
    function GetLibraryPathItem(const Index: integer): TDelphiPathItem;
    function GetLibraryPathListCount: integer;
    function GetBrowsingPathItem(const Index: integer): TDelphiPathItem;
    function GetBrowsingPathListCount: integer;
    function GetDebugDcuPathItem(const Index: integer): TDelphiPathItem;
    function GetDebugDcuPathListCount: integer;
    procedure Clear;
    procedure ReadInfos;
  public
    constructor Create(const TargetPlatform: TDelphiTargetPlatform; const RegPath: string; const RegRoot: HKEY; const UserVarList, IDEVarList: TMultiValueList);
    destructor Destroy; override;

    property TargetExists: Boolean read FTargetExists;
    function AsInfoStr: string;

    // Reg: Library\Search Path, IDE: Library path
    property LibraryPath: TDelphiPathItem read FLibraryPath;
    property LibraryPathItems[const Index: integer]: TDelphiPathItem read GetLibraryPathItem;
    property LibraryPathItemsCount: integer read GetLibraryPathListCount;
    property LibraryPathList: TDelphiPathList read FLibraryPathList;

    // Reg: Library\Browsing Path
    property BrowsingPath: TDelphiPathItem read FBrowsingPath;
    property BrowsingPathItems[const Index: integer]: TDelphiPathItem read GetBrowsingPathItem;
    property BrowsingPathItemsCount: integer read GetBrowsingPathListCount;
    property BrowsingPathList: TDelphiPathList read FBrowsingPathList;

    // Reg: Debug DCU Path
    property DebugDcuPath: TDelphiPathItem read FDebugDcuPath;
    property DebugDcuPathItems[const Index: integer]: TDelphiPathItem read GetDebugDcuPathItem;
    property DebugDcuPathItemsCount: integer read GetDebugDcuPathListCount;
    property DebugDcuPathList: TDelphiPathList read FDebugDcuPathList;

    property PackageOutputDir: TDelphiPathItem read FPackageOutputDir; // Reg: Library\Package DPL Output     (BPL output directory)
    property DcpOutputDir: TDelphiPathItem read FDcpOutputDir;         // Reg: Library\Package DCP Output
    property HppOutputDir: TDelphiPathItem read FHppOutputDir;         // Reg: Library\HPP Output Directory

    property TargetPlatform: TDelphiTargetPlatform read FTargetPlatform;
    property TargetPlatformName: string read FTargetPlatformName;
    property RegistryPath: string read FRegPath;
    property RegistryFullPath: string read FRegFullPath;
  end;


  {$region '   TDelphiInfo   '}
  TDelphiInfo = class
  private
    FIdeID: integer;
    FAppDataDir: string;
    FBdsExecutable: string;
    FBdsVersion: TBdsVersion;
    FBdsVersionStr: string;
    FConfigDir: string;
    FDelphiName: string;
    FDelphiVersion: TDelphiVersion;
    FEdition: string;
    FIDECBuilderPackages: TDelphiPackageList;
    FIDEDelphiPackages: TDelphiPackageList;
    FIDEPackages: TDelphiPackageList;
    FInstallDir: string;
    FInstalledUpdates: TMultiValueList;
    FInstallLanguage: string;
    FPackages: TDelphiPackageList;
    FPersonalities: TMultiValueList;
    FRegRootKey: HKEY;
    FRegPath_EnvVars: string;
    FRegPath_IDECBuilderPackages: string;
    FRegPath_IDEDelphiPackages: string;
    FRegPath_IDEPackages: string;
    FRegPath_InstalledUpdates: string;
    FRegPath_Library: string;
    FRegPath_Packages: string;
    FRegPath_Personalities: string;
    FRegPath_Base: string;
    FUserVariables: TMultiValueList;
    FProductVersion: string;
    FRegPath_DisabledPackages: string;
    FDisabledPackages: TDelphiPackageList;
    FRegPath_DisabledIDEPackages: string;
    FDisabledIDEPackages: TDelphiPackageList;
    FEditorHighlightList: TDelphiEditorHighlightList;
    FRegPath_EditorHighlight: string;
    FEnvironmentProjFile: string;
    FIDEVariables: TMultiValueList;

    FLibraryPaths_Win32: TDelphiTargetLibraryPaths;
    FLibraryPaths_Win64: TDelphiTargetLibraryPaths;

    FLibraryPaths_Android32: TDelphiTargetLibraryPaths;
    FLibraryPaths_Android64: TDelphiTargetLibraryPaths;

    FCompiler_Delphi_Win32: string;
    FCompiler_Delphi_Win64: string;
    FCompiler_Delphi_Linux64: string;
    FResourceCompiler: string;
    FCBuilderCompiler_Win32: string;
    FCBuilderCompiler_Win64: string;
    FRsVars_BAT: string;
    FDprojProjectVersions: TStringDynArray;
    FCompiler_Delphi_iOS64: string;
    FCompiler_Delphi_Android32: string;
    FCompiler_Delphi_Android64: string;
    FCompiler_Delphi_iOS32: string;
    FCompiler_Delphi_iOS_Simulator32: string;
    FCompiler_Delphi_OSX32: string;
    FCompiler_Delphi_OSX64: string;
    FCBuilderCompiler_Win32_CLANG: string;
    FCBuilderCompiler_OSX32: string;
    FCBuilderCompiler_iOS32: string;
    FCBuilderCompiler_iOS64: string;
    FCBuilderCompiler_Android32: string;

    procedure GetUserVars;
    procedure GetGeneralInfo;
    procedure GetInstalledUpdates;
    procedure GetIDEPackages;
    procedure GetIDEDelphiPackages;
    procedure GetIDECBuilderPackages;
    procedure GetPackages;
    procedure GetDisabledPackages;
    procedure GetDisabledIDEPackages;
    procedure GetPersonalities;
    procedure GetEditorHighlighList;
    procedure GetEnvironmentProjFile;
    procedure GetDprojProjectVersions;
    procedure ReadInfos;
    procedure PrepareEditorHighlightList;
    procedure FillMVList(const KeyName: string; const List: TMultiValueList; bClear: Boolean = True);
    procedure FillPackageList(const KeyName: string; const PackageList: TDelphiPackageList; bClear: Boolean = True);
    procedure GetIDEVariables;
    function GetDefaultVar_BDSCOMMONDIR: string;
    function GetDefaultVar_BDSUSERDIR: string;
  public
    constructor Create(const DelphiVersion: TDelphiVersion; RegRootKey: HKEY = HKEY_CURRENT_USER); overload;
    constructor Create(const DelphiName: string); overload;
    destructor Destroy; override;

    function GetVariableValue(const VarName: string): string;

    procedure SaveEditorHighlightListToFile(const FileName: string; OutputFormat: TDelphiEditorHighlightFormat = dehfJson);
    function LoadEditorHighlightFromFile(const FileName: string): Boolean;

    function AsInfoStr(const bShowRegKeys, bShowUserVars, bShowIDEVars, bShowInstalledUpdates, bShowIDEPackages, bShowIDEDelphiPackages,
      bShowIDECBuilderPackages, bShowPackages, bShowDisabledPackages, bShowDisabledIDEPackages, bShowPersonalities: Boolean): string;

    property DelphiVersion: TDelphiVersion read FDelphiVersion;
    property DelphiName: string read FDelphiName;
    property BdsVersion: TBdsVersion read FBdsVersion;
    property BdsVersionStr: string read FBdsVersionStr;
    property AppDataDir: string read FAppDataDir;
    property ConfigDir: string read FConfigDir;
    property UserVariables: TMultiValueList read FUserVariables; // variables from REG Environment Variables
    property IDEVariables: TMultiValueList read FIDEVariables; // variables from environment.proj file
    property InstalledUpdates: TMultiValueList read FInstalledUpdates;

    property RegPath_Base: string read FRegPath_Base;
    property RegPath_EnvVars: string read FRegPath_EnvVars;
    property RegPath_Library: string read FRegPath_Library;
    property RegPath_InstalledUpdates: string read FRegPath_InstalledUpdates;
    property RegPath_IDEPackages: string read FRegPath_IDEPackages;
    property RegPath_IDEDelphiPackages: string read FRegPath_IDEDelphiPackages;
    property RegPath_IDECBuilderPackages: string read FRegPath_IDECBuilderPackages;
    property RegPath_Packages: string read FRegPath_Packages;
    property RegPath_DisabledPackages: string read FRegPath_DisabledPackages;
    property RegPath_DisabledIDEPackages: string read FRegPath_DisabledIDEPackages;
    property RegPath_Personalities: string read FRegPath_Personalities;
    property RegPath_EditorHighlight: string read FRegPath_EditorHighlight;
    property RegRootKey: HKEY read FRegRootKey;

    property InstallDir: string read FInstallDir;
    property BdsExecutable: string read FBdsExecutable;
    property ProductVersion: string read FProductVersion;
    property Edition: string read FEdition;
    property InstallLanguage: string read FInstallLanguage;

    property Personalities: TMultiValueList read FPersonalities;

    property EditorHighightList: TDelphiEditorHighlightList read FEditorHighlightList;
    property EnvironmentProjFile: string read FEnvironmentProjFile;
    property RsVars_BAT: string read FRsVars_BAT;
    property DprojProjectVersions: TStringDynArray read FDprojProjectVersions;


    // Packages
    property IDEPackages: TDelphiPackageList read FIDEPackages;
    property IDEDelphiPackages: TDelphiPackageList read FIDEDelphiPackages;
    property IDECBuilderPackages: TDelphiPackageList read FIDECBuilderPackages;
    property Packages: TDelphiPackageList read FPackages;
    property DisabledPackages: TDelphiPackageList read FDisabledPackages;
    property DisabledIDEPackages: TDelphiPackageList read FDisabledIDEPackages;

    // Library paths
    property LibraryPaths_Win32: TDelphiTargetLibraryPaths read FLibraryPaths_Win32;
    property LibraryPaths_Win64: TDelphiTargetLibraryPaths read FLibraryPaths_Win64;
    property LibraryPaths_Android32: TDelphiTargetLibraryPaths read FLibraryPaths_Android32;
    property LibraryPaths_Android64: TDelphiTargetLibraryPaths read FLibraryPaths_Android64;

    // Compilers
    // https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Command-Line_Utilities_Index
    property Compiler_Delphi_Win32: string read FCompiler_Delphi_Win32;          // dcc32.exe
    property Compiler_Delphi_Win64: string read FCompiler_Delphi_Win64;          // dcc64.exe
    property Compiler_Delphi_Linux64: string read FCompiler_Delphi_Linux64;      // dcclinux64
    property Compiler_Delphi_iOS32: string read FCompiler_Delphi_iOS32;          // dcciosarm.exe
    property Compiler_Delphi_iOS64: string read FCompiler_Delphi_iOS64;          // dcciosarm64.exe
    property Compiler_Delphi_iOS_Simulator32: string read FCompiler_Delphi_iOS_Simulator32; // dccios32.exe
    property Compiler_Delphi_OSX32: string read FCompiler_Delphi_OSX32;          // dccosx.exe
    property Compiler_Delphi_OSX64: string read FCompiler_Delphi_OSX64;          // dccosx64.exe
    property Compiler_Delphi_Android32: string read FCompiler_Delphi_Android32;  // dccarm.exe
    property Compiler_Delphi_Android64: string read FCompiler_Delphi_Android64;  // dccarm64.exe

    property CBuilderCompiler_Win32_CLANG: string read FCBuilderCompiler_Win32_CLANG; // bcc32c.exe
    property CBuilderCompiler_Win32: string read FCBuilderCompiler_Win32;    // bcc32.exe
    property CBuilderCompiler_Win64: string read FCBuilderCompiler_Win64;    // bcc64.exe
    property CBuilderCompiler_OSX32: string read FCBuilderCompiler_OSX32;    // bccosx.exe
    property CBuilderCompiler_iOS32: string read FCBuilderCompiler_iOS32;    // bcciosarm.exe
    property CBuilderCompiler_iOS64: string read FCBuilderCompiler_iOS64;    // bcciosarm64.exe
    property CBuilderCompiler_Android32: string read FCBuilderCompiler_Android32; // bccaarm.exe

    property ResourceCompiler: string read FResourceCompiler;                // brcc32.exe

  end;
  {$endregion TDelphiInfo}



function DelphiNameToDelphiVersion(DelphiName: string): TDelphiVersion;
function DelphiNameToBdsVersion(DelphiName: string): TBdsVersion;
function BDSVersionToBdsNumStr(const bdv: TBdsVersion): string;
function DelphiNameToBdsNumStr(DelphiName: string): string;
function DelphiVersionToBdsVersion(const dv: TDelphiVersion): TBdsVersion;
function DelphiVersionToDelphiName(const dv: TDelphiVersion): string;
function DelphiVersionToDelphiFullName(const dv: TDelphiVersion): string;
function TargetPlatformToStr(const TargetPlatform: TDelphiTargetPlatform): string; // returns registry KEY NAME
function StrToTargetPlatform(const TargetPlatformName: string): TDelphiTargetPlatform;
function MVListToNameValueStr(const List: TMultiValueList; const DataSeparator: string; Prefix: string = ''): string;
function PackageListToStr(const PackageList: TDelphiPackageList; Prefix: string = ''): string;

function ExportEditorHighlightList(const FileName: string; List: TDelphiEditorHighlightList; OutputFormat: TDelphiEditorHighlightFormat): Boolean;
function ExportEditorHighlightList_AsIni(const FileName: string; List: TDelphiEditorHighlightList): Boolean;
function ExportEditorHighlightList_AsJson(const FileName: string; List: TDelphiEditorHighlightList): Boolean;
function EditorHighlightListToStr(List: TDelphiEditorHighlightList; const OutputFormat: TDelphiEditorHighlightFormat): string;

function LoadEditorHighlightListFromString(const Text: string; List: TDelphiEditorHighlightList): Boolean;
function LoadEditorHighlightListFromFile(const FileName: string; List: TDelphiEditorHighlightList): Boolean;

function ExpandDelphiVar(const Path, VarName, VarValue: string): string;
function ExpandDelphiVars(const Path: string; const VarList: TMultiValueList): string;

function TryStringToColorDef(const ColorStr: string; const Default: TColor): TColor;
function IsDelphiVariable(const VarName: string): Boolean;

function TryGetDelphiVersionsFromProjectVersion(const ProjectVersion: string; out DelphiVersions: TDelphiVersions): Boolean;


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}




{$region '                                          TDelphiInfo                                                 '}


  {$region '                          TDelphiInfo: Create & Destroy                           '}
constructor TDelphiInfo.Create(const DelphiVersion: TDelphiVersion; RegRootKey: HKEY = HKEY_CURRENT_USER);
begin
  inherited Create;
  FIdeID := integer(DelphiVersion);
  FDelphiVersion := DelphiVersion;
  FBdsVersion := DelphiVersionToBdsVersion(FDelphiVersion);
  FBdsVersionStr := BDSVersionToBdsNumStr(FBdsVersion);
  FDelphiName := DelphiVersionToDelphiName(FDelphiVersion);

  FAppDataDir := TStr.RemoveTrailingPathDelimiter(GetEnvironmentString('APPDATA', True));
  FRegRootKey := RegRootKey;

  // Config dir
  if DelphiVersion = dv2007 then FConfigDir := FAppDataDir + '\Borland\BDS\' + FBdsVersionStr
  else if DelphiVersion in [dv2009, dv2010] then FConfigDir := FAppDataDir + '\CodeGear\BDS\' + FBdsVersionStr
  //else if DelphiVersion in [dvXE..dvSydney] then FConfigDir := FAppDataDir + '\Embarcadero\BDS\' + FBdsVersionStr;
  else if DelphiVersion >= dvXE then FConfigDir := FAppDataDir + '\Embarcadero\BDS\' + FBdsVersionStr;

  // Registry root
  if DelphiVersion = dv7 then FRegPath_Base := 'Software\Borland\Delphi\7.0'
  else if DelphiVersion in [dv2005, dv2006, dv2007] then FRegPath_Base := REG_PATH_BORLAND_BDS + '\' + FBdsVersionStr
  else if DelphiVersion in [dv2009, dv2010] then FRegPath_Base := REG_PATH_CODEGEAR_BDS + '\' + FBdsVersionStr
  else FRegPath_Base := REG_PATH_EMBT_BDS + '\' + FBdsVersionStr;

  FRegPath_EnvVars := FRegPath_Base + '\' + REG_KEY_USER_VARS;
  FRegPath_Library := FRegPath_Base + '\' + REG_KEY_LIBRARY;
  FRegPath_InstalledUpdates := FRegPath_Base + '\' + REG_KEY_INSTALLED_UPDATES;

  FRegPath_IDEPackages := FRegPath_Base + '\' + REG_KEY_IDE_PACKAGES;

  if DelphiVersion = dv7 then FRegPath_IDEDelphiPackages := FRegPath_Base + '\' + REG_KEY_IDE_PACKAGES
  else FRegPath_IDEDelphiPackages := FRegPath_Base + '\' + REG_KEY_IDE_DELPHI_PACKAGES;

  if DelphiVersion = dv7 then FRegPath_IDECBuilderPackages := ''
  else FRegPath_IDECBuilderPackages := FRegPath_Base + '\' + REG_KEY_IDE_CBUILDER_PACKAGES;

  FRegPath_Packages := FRegPath_Base + '\' + REG_KEY_PACKAGES;
  FRegPath_DisabledPackages := FRegPath_Base + '\' + REG_KEY_DISABLED_PACKAGES;
  FRegPath_DisabledIDEPackages := FRegPath_Base + '\' + REG_KEY_DISABLED_IDE_PACKAGES;

  if DelphiVersion = dv7 then FRegPath_Personalities := ''
  else FRegPath_Personalities := FRegPath_Base + '\' + REG_KEY_PERSONALITIES;

  FRegPath_EditorHighlight := FRegPath_Base + '\' + REG_KEY_EDITOR_HIGHLIGHT;


  FUserVariables := TMultiValueList.Create;
  FIDEVariables := TMultiValueList.Create;
  FInstalledUpdates := TMultiValueList.Create;
  FIDEPackages := TDelphiPackageList.Create;
  FIDEDelphiPackages := TDelphiPackageList.Create;
  FIDECBuilderPackages := TDelphiPackageList.Create;
  FPackages := TDelphiPackageList.Create;
  FDisabledPackages := TDelphiPackageList.Create;
  FDisabledIDEPackages := TDelphiPackageList.Create;
  FPersonalities := TMultiValueList.Create;
  FEditorHighlightList := TDelphiEditorHighlightList.Create;

  FEnvironmentProjFile := '';

  ReadInfos;

  // TODO: Sprawdzić, od której wersji Delphi dostępny jest target Android64

  if FIdeID < integer(dvXE2) then
    FLibraryPaths_Win32 := TDelphiTargetLibraryPaths.Create(dtpWin32, FRegPath_Base + '\Library', FRegRootKey, FUserVariables, FIDEVariables)
  else
    FLibraryPaths_Win32 := TDelphiTargetLibraryPaths.Create(dtpWin32, FRegPath_Base + '\Library\Win32', FRegRootKey, FUserVariables, FIDEVariables);

  FLibraryPaths_Win64 := TDelphiTargetLibraryPaths.Create(dtpWin64, FRegPath_Base + '\Library\Win64', FRegRootKey, FUserVariables, FIDEVariables);
  FLibraryPaths_Android32 := TDelphiTargetLibraryPaths.Create(dtpAndroid32, FRegPath_Base + '\Library\Android32', FRegRootKey, FUserVariables, FIDEVariables);
  FLibraryPaths_Android64 := TDelphiTargetLibraryPaths.Create(dtpAndroid64, FRegPath_Base + '\Library\Android64', FRegRootKey, FUserVariables, FIDEVariables);
end;

constructor TDelphiInfo.Create(const DelphiName: string);
begin
  Create(DelphiNameToDelphiVersion(DelphiName));
end;

destructor TDelphiInfo.Destroy;
begin
  FEditorHighlightList.Free;
  FDisabledIDEPackages.Free;
  FDisabledPackages.Free;
  FPackages.Free;
  FIDECBuilderPackages.Free;
  FIDEDelphiPackages.Free;
  FIDEPackages.Free;
  FInstalledUpdates.Free;
  FUserVariables.Free;
  FIDEVariables.Free;
  FPersonalities.Free;

  FLibraryPaths_Win32.Free;
  FLibraryPaths_Win64.Free;
  FLibraryPaths_Android32.Free;
  FLibraryPaths_Android64.Free;

  inherited Destroy;
end;
  {$endregion TDelphiInfo: Create & Destroy}


procedure TDelphiInfo.ReadInfos;
begin
  GetGeneralInfo;

  GetUserVars;
  GetEnvironmentProjFile;
  GetIDEVariables;
  GetDprojProjectVersions;

  GetIDEPackages;
  GetIDEDelphiPackages;
  GetIDECBuilderPackages;
  GetPackages;
  GetDisabledPackages;
  GetDisabledIDEPackages;

  GetPersonalities;
  GetInstalledUpdates;
  GetEditorHighlighList;
end;

procedure TDelphiInfo.SaveEditorHighlightListToFile(const FileName: string; OutputFormat: TDelphiEditorHighlightFormat = dehfJson);
begin
  ExportEditorHighlightList(FileName, FEditorHighlightList, OutputFormat);
end;


function TDelphiInfo.LoadEditorHighlightFromFile(const FileName: string {; OutputFormat: TEditorHighlightFormat}): Boolean;
begin
  Result := LoadEditorHighlightListFromFile(FileName, FEditorHighlightList);
end;

procedure TDelphiInfo.GetGeneralInfo;
var
  Reg: TRegistry;
  s: string;
begin
  FProductVersion := '';
  FBdsExecutable := '';
  FInstallDir := '';
  FEdition := '';
  FInstallLanguage := '';

  Reg := TRegistry.Create(REG_ACCES_FLAG_READ_ONLY);
  try

    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKeyReadOnly(FRegPath_Base) then Exit;
    try

      Reg.TryGetString('ProductVersion', FProductVersion);
      Reg.TryGetString('RootDir', FInstallDir);
      FInstallDir := TStr.RemoveTrailingPathDelimiter(FInstallDir);
      Reg.TryGetString('App', FBdsExecutable);

      if FDelphiVersion = dv7 then Reg.TryGetString('Version', FEdition)
      else Reg.TryGetString('Edition', FEdition);
      if Length(FEdition) = 3 then
      begin
        if FEdition = 'STD' then FEdition := 'Personal or Standard'
        else if FEdition = 'PRO' then FEdition := 'Professional'
        else if FEdition = 'CSS' then FEdition := 'Enterprise or Client/Server'
        else if FEdition = 'ARC' then FEdition := 'Architect';
      end;

      Reg.TryGetString('InstallLanguage', FInstallLanguage);

    finally
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;


  // Delphi compilers
  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_WIN32;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_Win32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_WIN64;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_Win64 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_LINUX64;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_Linux64 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_OSX32;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_OSX32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_OSX64;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_OSX64 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_IOS32;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_iOS32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_IOS64;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_iOS64 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_IOS_SIMULATOR32;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_iOS_Simulator32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_ANDROID32;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_Android32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_DELPHI_ANDROID64;
  if not FileExists(s) then s := '';
  FCompiler_Delphi_Android64 := s;


  // CPP compilers
  s := FInstallDir + '\bin\' + COMPILER_EXE_CPP_WIN32;
  if not FileExists(s) then s := '';
  FCBuilderCompiler_Win32 := s;

  s := FInstallDir + '\bin\' + COMPILER_EXE_CPP_WIN64;
  if not FileExists(s) then s := '';
  FCBuilderCompiler_Win64 := s;



  s := FInstallDir + '\bin\brcc32.exe';
  if not FileExists(s) then s := '';
  FResourceCompiler := s;

  s := FInstallDir + '\bin\rsvars.bat';
  if not FileExists(s) then s := '';
  FRsVars_BAT := s;
end;

procedure TDelphiInfo.GetInstalledUpdates;
begin
  FillMVList(FRegPath_InstalledUpdates, FInstalledUpdates, True);
end;

procedure TDelphiInfo.GetIDEPackages;
begin
  FillPackageList(FRegPath_IDEPackages, FIDEPackages, True);
end;

procedure TDelphiInfo.GetIDEDelphiPackages;
begin
  FillPackageList(FRegPath_IDEDelphiPackages, FIDEDelphiPackages, True);
end;

procedure TDelphiInfo.GetIDECBuilderPackages;
begin
  FillPackageList(FRegPath_IDECBuilderPackages, FIDECBuilderPackages, True);
end;

procedure TDelphiInfo.GetPackages;
begin
  FillPackageList(FRegPath_Packages, FPackages, True);
end;

procedure TDelphiInfo.GetDisabledIDEPackages;
begin
  FillPackageList(FRegPath_DisabledIDEPackages, FDisabledIDEPackages, True);
end;

procedure TDelphiInfo.GetDisabledPackages;
begin
  FillPackageList(FRegPath_DisabledPackages, FDisabledPackages, True);
end;

procedure TDelphiInfo.GetPersonalities;
begin
  FillMVList(FRegPath_Personalities, FPersonalities, True);
end;

procedure TDelphiInfo.GetUserVars;
begin
  FillMVList(FRegPath_EnvVars, FUserVariables, True);
end;

function TDelphiInfo.GetVariableValue(const VarName: string): string;
begin
  Result := FUserVariables.GetStrValue(VarName, False, '');
end;

procedure TDelphiInfo.GetEnvironmentProjFile;
begin
  FEnvironmentProjFile := '';
  if not DirectoryExists(FConfigDir) then Exit;
  FEnvironmentProjFile := FConfigDir + '\environment.proj';
  if not FileExists(FEnvironmentProjFile) then FEnvironmentProjFile := '';
end;

procedure TDelphiInfo.GetDprojProjectVersions;

  procedure SetValues(Values: array of string);
  var
    i: integer;
  begin
    SetLength(FDprojProjectVersions, Length(Values));
    for i := 0 to High(Values) do
      FDprojProjectVersions[i] := Values[i];
  end;

begin
  SetLength(FDprojProjectVersions, 0);
  case FDelphiVersion of
    dv2009: SetValues(['12.0']);
    dv2010: SetValues(['12.0']);
    dvXE: SetValues(['12.2', '12.3']);
    dvXE2: SetValues(['13.4']);
    dvXE3: SetValues(['14.3', '14.4']);
    dvXE4: SetValues(['14.4', '14.6']);
    dvXE5: SetValues(['15.0', '15.1', '15.3']);
    dvAppMethod: SetValues(['15.2']);
    dvXE6: SetValues(['15.4']);
    dvXE7: SetValues(['16.0', '16.1']);
    dvXE8: SetValues(['17.0', '17.1', '17.2']);
    dvSeattle: SetValues(['18.0', '18.1']);
    dvBerlin: SetValues(['18.1', '18.2']);
    dvTokyo: SetValues(['18.2', '18.3', '18.4']);
    dvRio: SetValues(['18.5', '18.6', '18.7', '18.8']);
    dvSydney: SetValues(['19.0', '19.1', '19.2']);
    dvAlexandria: SetValues(['19.3', '19.4']);
  end;
end;

procedure TDelphiInfo.GetIDEVariables;
// Sample lines:
//   <BDSCOMMONDIR Condition="'$(BDSCOMMONDIR)'==''">C:\Users\Public\Documents\RAD Studio\8.0</BDSCOMMONDIR>
//   <BDSBIN Condition="'$(BDSBIN)'==''">e:\embarcadero\xe1\bin</BDSBIN>
//   <DELPHI Condition="'$(DELPHI)'==''">e:\embarcadero\xe1</DELPHI>
const
  sf = 'Condition="''$(';
var
  sl: TStringList;
  i, xp: integer;
  Line, VarName, VarValue: string;
  bPropG: Boolean;
begin
  if (FEnvironmentProjFile <> '') and FileExists(FEnvironmentProjFile) then
  begin

    sl := TStringList.Create;
    try
      sl.LoadFromFile(FEnvironmentProjFile);

      bPropG := False;
      for i := 0 to sl.Count - 1 do
      begin
        Line := Trim(sl[i]);
        if Line = '</PropertyGroup>' then Break;
        if Line = '<PropertyGroup>' then
        begin
          bPropG := True;
          Continue;
        end;
        if not bPropG then Continue;

        xp := Pos(sf, Line);
        if xp = 0 then Continue;

        Line := Copy(Line, xp + Length(sf), Length(Line));
        xp := Pos(')', Line);
        if xp = 0 then Continue;

        VarName := Copy(Line, 1, xp - 1);
        Line := Copy(Line, xp + 1, Length(Line));
        xp := Pos('>', Line);
        if xp = 0 then Continue;

        Line := Copy(Line, xp + 1, Length(Line));
        xp := Pos('<', Line);
        if xp = 0 then Continue;
        VarValue := Copy(Line, 1, xp - 1);

        FIDEVariables.AddNameValue(VarName, VarValue);
      end;

    finally
      sl.Free;
    end;

  end;

  if not FIDEVariables.NameExists('DELPHI') then FIDEVariables.AddNameValue('DELPHI', TStr.RemoveTrailingPathDelimiter(FInstallDir));

  if FIdeID >= integer(dv2005) then
    if not FIDEVariables.NameExists('BDS') then FIDEVariables.AddNameValue('BDS', TStr.RemoveTrailingPathDelimiter(FInstallDir));

  VarName := 'BDSCOMMONDIR';
  if not FIDEVariables.NameExists(VarName) then
  begin
    VarValue := GetDefaultVar_BDSCOMMONDIR;
    if (VarValue <> '') and DirectoryExists(VarValue) then FIDEVariables.AddNameValue(VarName, VarValue);
  end;

  VarName := 'BDSUSERDIR';
  if not FIDEVariables.NameExists(VarName) then
  begin
    VarValue := GetDefaultVar_BDSUSERDIR;
    if (VarValue <> '') and DirectoryExists(VarValue) then FIDEVariables.AddNameValue(VarName, VarValue);
  end;


  // Removing duplicate variables
  for i := 0 to FUserVariables.Count - 1 do
  begin
    VarName := FUserVariables[i].Name;
    VarValue := FUserVariables[i].StrValue;
    if TStr.Contains(VarValue, '$(') then Continue;
    FIDEVariables.RemoveItemsWithName(VarName);
  end;

end;

function TDelphiInfo.GetDefaultVar_BDSCOMMONDIR: string;
var
  s: string;
begin
  Result := '';
  if FIdeID <= integer(dv7) then Exit;

  s := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
  if s = '' then Exit;
  s := TStr.RemoveTrailingPathDelimiter(s);
  if FIdeID >= integer(dvXE6) then s := s + '\Embarcadero\Studio\' + FBdsVersionStr
  else if FIdeID >= integer(dv2009) then s := s + '\RAD Studio\' + FBdsVersionStr;
  Result := s;
end;

function TDelphiInfo.GetDefaultVar_BDSUSERDIR: string;
var
  s: string;
begin
  Result := '';
  if FIdeID <= integer(dv7) then Exit;

  s := GetSpecialFolder(CSIDL_MYDOCUMENTS);
  if s = '' then Exit;
  s := TStr.RemoveTrailingPathDelimiter(s);
  if FIdeID >= integer(dvXE6) then s := s + '\Embarcadero\Studio\' + FBdsVersionStr
  else if FIdeID >= integer(dv2009) then s := s + '\RAD Studio\' + FBdsVersionStr;
  Result := s;
end;

procedure TDelphiInfo.PrepareEditorHighlightList;
var
  i: integer;
  hi: TDelphiHighlightItem;
  RegShortKeyName: string;
begin
  FEditorHighlightList.Clear;

  for i := 0 to High(ArrHighlightKeyNames) do
  begin
    RegShortKeyName := ArrHighlightKeyNames[i];
    hi.Clear;
    hi.RegKeyName := FRegPath_EditorHighlight + '\' + RegShortKeyName;
    hi.DisplayName := RegShortKeyName;
    hi.ConfigItemName := TStr.Capitalize(RegShortKeyName);
    FEditorHighlightList.Add(hi);
  end;
end;


  {$region '                       TDelphiInfo.GetEditorHighlighList                            '}
procedure TDelphiInfo.GetEditorHighlighList;
var
  Reg: TRegistry;
  KeyName, sValue: string;
  i: integer;
  hi: TDelphiHighlightItem;
  cl: TColor;
begin
  PrepareEditorHighlightList;

  Reg := TRegistry.Create(REG_ACCES_FLAG_READ_ONLY);
  try

    Reg.RootKey := HKEY_CURRENT_USER;

    for i := 0 to FEditorHighlightList.Count - 1 do
    begin
      hi := FEditorHighlightList[i];
      KeyName := hi.RegKeyName;
      if not Reg.KeyExists(KeyName) then Continue;

      if not Reg.OpenKeyReadOnly(KeyName) then Continue;
      try

        hi.Enabled := True;

        // ------------------ Background color ----------------------
        if Reg.TryGetString('Background Color New', sValue) then
        begin
          cl := TryStringToColorDef(sValue, clNone);
          hi.BackgroundColorNew := cl;
        end;
        Reg.TryGetBool('Default Background', hi.DefaultBackground);

        // ------------------ Foreground color -----------------------
        if Reg.TryGetString('Foreground Color New', sValue) then
        begin
          cl := TryStringToColorDef(sValue, clNone);
          hi.ForegroundColorNew := cl;
        end;
        Reg.TryGetBool('Default Foreground', hi.DefaultForeground);

        // ------------------ Font styles ----------------------------
        Reg.TryGetBool('Bold', hi.Bold);
        Reg.TryGetBool('Italic', hi.Italic);
        Reg.TryGetBool('Underline', hi.Underline);


        FEditorHighlightList[i] := hi;

      finally
        Reg.CloseKey;
      end;
    end;


  finally
    Reg.Free;
  end;
end;



{$endregion TDelphiInfo.GetEditorHighlighList}


  {$region '                           TDelphiInfo.FillMVList                             '}
procedure TDelphiInfo.FillMVList(const KeyName: string; const List: TMultiValueList; bClear: Boolean = True);
var
  Reg: TRegistry;
  slNames: TStringList;
  sName, sValue: string;
  i: integer;
begin
  if bClear then List.Clear;
  if KeyName = '' then Exit;

  Reg := TRegistry.Create(REG_ACCES_FLAG_READ_ONLY);
  try

    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.KeyExists(KeyName) then Exit;
    if not Reg.OpenKeyReadOnly(KeyName) then Exit;

    slNames := TStringList.Create;
    try
      Reg.GetValueNames(slNames);
      for i := 0 to slNames.Count - 1 do
      begin
        sName := Trim(slNames[i]);
        if sName = '' then Continue;
        if Reg.GetDataType(sName) <> TRegDataType.rdString then Continue;
        sValue := Reg.ReadString(sName);
        List.AddNameValue(sName, sValue);
      end;
    finally
      slNames.Free;
    end;

    Reg.CloseKey;

  finally
    Reg.Free;
  end;
end;
  {$endregion TDelphiInfo.FillMVList}


  {$region '                               TDelphiInfo.FillPackageList                                  '}
procedure TDelphiInfo.FillPackageList(const KeyName: string; const PackageList: TDelphiPackageList; bClear: Boolean);
var
  Reg: TRegistry;
  slNames: TStringList;
  sFileName, sDesc: string;
  PackageItem: TDelphiPackageItem;
  i: integer;
begin
  if bClear then PackageList.Clear;

  Reg := TRegistry.Create(REG_ACCES_FLAG_READ_ONLY);
  try

    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.KeyExists(KeyName) then Exit;
    if not Reg.OpenKeyReadOnly(KeyName) then Exit;

    try

      slNames := TStringList.Create;
      try

        Reg.GetValueNames(slNames);

        for i := 0 to slNames.Count - 1 do
        begin

          sFileName := Trim(slNames[i]);
          if sFileName = '' then Continue;
          if Reg.GetDataType(sFileName) <> TRegDataType.rdString then Continue;
          sDesc := Reg.ReadString(sFileName);

          PackageItem.Description := sDesc;
          PackageItem.RegistryFileName := sFileName;

          PackageItem.ExpandedFileName := PackageItem.RegistryFileName;
          PackageItem.ExpandedFileName := ExpandDelphiVars(PackageItem.ExpandedFileName, FUserVariables); // ExpandUserVars(PackageItem.ExpandedFileName);
          PackageItem.ExpandedFileName := ExpandDelphiVars(PackageItem.ExpandedFileName, FIDEVariables);  // ExpandSystemVars(sFileName);

          PackageItem.FileExists := FileExists(PackageItem.ExpandedFileName);

          PackageList.Add(PackageItem);

        end; // for


      finally
        slNames.Free;
      end;

    finally
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;

  PackageList.UpdateStats;
end;
  {$endregion TDelphiInfo.FillPackageList}




  {$region '                             TDelphiInfo.AsInfoStr                                 '}
function TDelphiInfo.AsInfoStr(const bShowRegKeys, bShowUserVars, bShowIDEVars, bShowInstalledUpdates, bShowIDEPackages, bShowIDEDelphiPackages,
  bShowIDECBuilderPackages, bShowPackages, bShowDisabledPackages, bShowDisabledIDEPackages, bShowPersonalities: Boolean): string;
var
  s, s2: string;
begin
  s :=
    'Delphi name: ' + FDelphiName + ENDL +
    'BDS version: ' + FBdsVersionStr + ENDL +
    'Product version: ' + FProductVersion + ENDL +
    'Edition: ' + FEdition + ENDL +
    'Install language: ' + FInstallLanguage + ENDL +
    'Install dir: ' + FInstallDir + ENDL +
    'BDS executable: ' + FBdsExecutable + ENDL +
    'Delphi compiler Win32: ' + FCompiler_Delphi_Win32 + ENDL +
    'Delphi compiler Win64: ' + FCompiler_Delphi_Win64 + ENDL +
    'Delphi compiler Linux64: ' + FCompiler_Delphi_Linux64 + ENDL +
    'C++Builder compiler Win32: ' + FCBuilderCompiler_Win32 + ENDL +
    'C++Builder compiler Win64: ' + FCBuilderCompiler_Win64 + ENDL +
    'Resource compiler: ' + FResourceCompiler + ENDL +
    'Config dir: ' + FConfigDir + ENDL +
    'environment.proj file: ' + FEnvironmentProjFile + ENDL +
    'BPL output directory Win32: ' + FLibraryPaths_Win32.PackageOutputDir.ExpandedPath + ENDL +
    'BPL output directory Win64: ' + FLibraryPaths_Win64.PackageOutputDir.ExpandedPath + ENDL +
    'DCP output directory Win32: ' + FLibraryPaths_Win32.DcpOutputDir.ExpandedPath + ENDL +
    'DCP output directory Win64: ' + FLibraryPaths_Win64.DcpOutputDir.ExpandedPath + ENDL +
    'DPROJ ProductVersion: ' + TSTr.StringArrayToStr(FDprojProjectVersions, ', ') +
    ENDL;

  if bShowRegKeys then
  begin
    //s2 := 'HKEY_CURRENT_USER\';
    s := s + ENDL + 'Registry keys:' + ENDL +
    '  Root key: ' + TReg.RootKeyToStr(FRegRootKey) + ENDL +
    '  Base path: ' + s2 + FRegPath_Base + ENDL +
    '  Environment Variables: ' + s2 + FRegPath_EnvVars + ENDL +
    '  Library: ' + s2 + FRegPath_Library + ENDL +
    '  InstalledUpdates: ' + s2 + FRegPath_InstalledUpdates + ENDL +
    '  IDE Packages: ' + s2 + FRegPath_IDEPackages + ENDL +
    '  IDE Delphi Packages: ' + s2 + FRegPath_IDEDelphiPackages + ENDL +
    '  IDE CBuilder Packages: ' + s2 + FRegPath_IDECBuilderPackages + ENDL +
    '  Packages: ' + s2 + FRegPath_Packages + ENDL +
    '  Disabled Packages: ' + s2 + FRegPath_DisabledPackages + ENDL +
    '  Disabled IDE Packages: ' + s2 + FRegPath_DisabledIDEPackages + ENDL +
    '  Personalities: ' + s2 + FRegPath_Personalities +
    ENDL;
  end;

  s := s + ENDL + 'Personalities: ' + itos(FPersonalities.count);
  if bShowPersonalities then
     if FPersonalities.Count > 0 then
     begin
       s := s + ENDL + MVListToNameValueStr(FPersonalities, ' - ', '  ');
       s := TStr.TrimENDL(s);
       s := s + ENDL;
     end;

  if bShowUserVars then
    if FUserVariables.Count > 0 then
    begin
      s := s + ENDL + 'User variables:' + ENDL + MVListToNameValueStr(FUserVariables, '=', '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  if bShowIDEVars then
    if FIDEVariables.Count > 0 then
    begin
      s := s + ENDL + 'IDE variables:' + ENDL + MVListToNameValueStr(FIDEVariables, '=', '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  if bShowInstalledUpdates then
    if FInstalledUpdates.Count > 0 then
    begin
      s := s + ENDL + 'Installed updates: ' + ENDL + MVListToNameValueStr(FInstalledUpdates, ' - ', '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'IDE Packages: ' + itos(FIDEPackages.Count);
  if bShowIDEPackages then
    if FIDEPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FIDEPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'IDE Delphi Packages: ' + itos(FIDEDelphiPackages.Count);
  if bShowIDEDelphiPackages then
    if FIDEDelphiPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FIDEDelphiPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'IDE CBuilder Packages: ' + itos(FIDECBuilderPackages.Count);
  if bShowIDECBuilderPackages then
    if FIDECBuilderPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FIDECBuilderPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'Packages: ' + itos(FPackages.Count);
  if bShowPackages then
    if FPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'Disabled Packages: ' + itos(FDisabledPackages.Count);
  if bShowDisabledPackages then
    if FDisabledPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FDisabledPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  s := s + ENDL + 'Disabled IDE Packages: ' + itos(FDisabledIDEPackages.Count);
  if bShowDisabledIDEPackages then
    if FDisabledIDEPackages.Count > 0 then
    begin
      s := s + ENDL + PackageListToStr(FDisabledIDEPackages, '  ');
      s := TStr.TrimENDL(s);
      s := s + ENDL;
    end;

  Result := s;

end;
  {$endregion TDelphiInfo.AsInfoStr}


{$endregion TDelphiInfo}





{$region '                            TDelphiHighlightItem                              '}
function TDelphiHighlightItem.AsIniStr: string;
var
  s: string;
begin
  s := '[' + ConfigItemName + ']' + ENDL;

  s := s + 'DefaultBackground=' + BoolToStr10(DefaultBackground) + ENDL;
  if BackgroundColorNew = clNone then s := s + 'BackgroundColorNew=' + ENDL
  else s := s + 'BackgroundColorNew=' + ColorToString(BackgroundColorNew) + ENDL;

  s := s + 'DefaultForeground=' + BoolToStr10(DefaultForeground) + ENDL;
  if ForegroundColorNew = clNone then s := s + 'ForegroundColorNew=' + ENDL
  else s := s + 'ForegroundColorNew=' + ColorToString(ForegroundColorNew) + ENDL;

  s := s +
    'Bold=' + BoolToStr10(Bold) + ENDL +
    'Italic=' + BoolToStr10(Italic) + ENDL +
    'Underline=' + BoolToStr10(Underline) + ENDL;

  Result := s;
end;

function TDelphiHighlightItem.AsRegStr(RootKeyStr: string = 'HKEY_CURRENT_USER'): string;
var
  s: string;
  // "Name"="DataType:Value" / REG_SZ (string) - Default Registry data type (can be omitted)
begin
  s := '[' + RootKeyStr + '\' + RegKeyName + ']' + ENDL;

  s := s + '"DefaultBackground"="' + BoolToStrTF(DefaultBackground) + '"' + ENDL;
  if BackgroundColorNew = clNone then s := s + '"BackgroundColorNew"=""' + ENDL
  else s := s + '"BackgroundColorNew"="' + ColorToString(BackgroundColorNew) + '"' + ENDL;

  s := s + '"DefaultForeground"="' + BoolToStrTF(DefaultForeground) + '"' + ENDL;
  if ForegroundColorNew = clNone then s := s + '"ForegroundColorNew"="' + ENDL + '"'
  else s := s + '"ForegroundColorNew"="' + ColorToString(ForegroundColorNew) + '"' + ENDL;

  s := s +
    '"Bold"="' + BoolToStrTF(Bold) + '"' + ENDL +
    '"Italic"="' + BoolToStrTF(Italic) + '"' + ENDL +
    '"Underline"="' + BoolToStrTF(Underline) + '"' + ENDL;

  Result := s;
end;

function TDelphiHighlightItem.AsJsonStr(Indent: string): string;
var
  s, sColor: string;

  function BoolToJsonStr(const b: Boolean): string; begin if b then Result := 'true' else Result := 'false'; end;

begin
  s := Indent + '"' + ConfigItemName + '": {' + ENDL;

  s := s + Indent + '  "DefaultBackground": ' + BoolToJsonStr(DefaultBackground) + ',' + ENDL;

  if BackgroundColorNew = clNone then sColor := '' else sColor := ColorToString(BackgroundColorNew);
  s := s + Indent + '  "BackgroundColorNew": "' + sColor + '",' + ENDL;

  s := s + Indent + '  "DefaultForeground": ' + BoolToJsonStr(DefaultForeground) + ',' + ENDL;

  if ForegroundColorNew = clNone then sColor := '' else sColor := ColorToString(ForegroundColorNew);
  s := s + Indent + '  "ForegroundColorNew": "' + sColor + '",' + ENDL;

  s := s + Indent + '  "Bold": ' + BoolToJsonStr(Bold) + ',' + ENDL;
  s := s + Indent + '  "Italic": ' + BoolToJsonStr(Italic) + ',' + ENDL;
  s := s + Indent + '  "Underline": ' + BoolToJsonStr(Underline) + '' + ENDL;

  s := s + Indent + '},';
  Result := s;
end;


function TDelphiHighlightItem.AsXmlStr(Indent: string): string;
var
  s, sColor: string;

  function stB(const sTag: string): string; begin Result := '<' + sTag + '>'; end;
  function stE(const sTag: string): string; begin Result := '</' + sTag + '>'; end;
  function Tag(const TagName, TagValue: string): string; begin Result := '<' + TagName + '>' + TagValue + '</' + TagName + '>'; end;
  function BoolToXmlStr(const b: Boolean): string; begin if b then Result := 'True' else Result := 'False'; end;

begin
  s := '';

  s := Indent + stB(ConfigItemName) + ENDL;

  s := s + Indent + '  ' + Tag('DefaultBackground', BoolToXmlStr(DefaultBackground)) + ENDL;

  if BackgroundColorNew = clNone then sColor := '' else sColor := ColorToString(BackgroundColorNew);
  s := s + Indent + '  ' + Tag('BackgroundColorNew', sColor) + ENDL;

  s := s + Indent + '  ' + Tag('DefaultForeground', BoolToXmlStr(DefaultForeground)) + ENDL;

  if ForegroundColorNew = clNone then sColor := '' else sColor := ColorToString(ForegroundColorNew);
  s := s + Indent + '  ' + Tag('ForegroundColorNew', sColor) + ENDL;

  s := s + Indent + '  ' + Tag('Bold', BoolToXmlStr(Bold)) + ENDL;
  s := s + Indent + '  ' + Tag('Italic', BoolToXmlStr(Italic)) + ENDL;
  s := s + Indent + '  ' + Tag('Underline', BoolToXmlStr(Underline)) + ENDL;


  s := s + Indent + stE(ConfigItemName);
  Result := s;
end;

procedure TDelphiHighlightItem.Clear(bClearRegKeyName: Boolean = True; bClearDisplayName: Boolean = True);
begin
  Enabled := False;
  if bClearRegKeyName then RegKeyName := '';
  if bClearDisplayName then DisplayName := '';
  ConfigItemName := '';
  DefaultBackground := True;
  BackgroundColorNew := clNone;
  DefaultForeground := True;
  ForegroundColorNew := clNone;
  Bold := False;
  Italic := False;
  Underline := False;
end;

procedure TDelphiHighlightItem.LoadFromIni(const Ini: TIniFile);
var
  Section, sColor, sDefaultColor: string;
begin
  Clear(False, False);
  Section := TStr.Capitalize(RegKeyName);
  if not Ini.SectionExists(Section) then Exit;

  sDefaultColor := ColorToString(clNone);

  DefaultBackground := Ini.ReadBool(Section, 'DefaultBackground', DefaultBackground);
  sColor := Ini.ReadString(Section, 'BackgroundColorNew', sDefaultColor);
  BackgroundColorNew := TryStringToColorDef(sColor, BackgroundColorNew);

  DefaultForeground := Ini.ReadBool(Section, 'DefaultForeground', DefaultForeground);
  sColor := Ini.ReadString(Section, 'ForegroundColorNew', sDefaultColor);
  ForegroundColorNew := TryStringToColorDef(sColor, ForegroundColorNew);

  Bold := Ini.ReadBool(Section, 'Bold', Bold);
  Italic := Ini.ReadBool(Section, 'Italic', Italic);
  Underline := Ini.ReadBool(Section, 'Underline', Underline);
end;

{$endregion TDelphiHighlightItem}



{$region '                   TDelphiPackageItem                   '}

function TDelphiPackageItem.AsString(const RegistryValue: Boolean = True; const ExpandedValue: Boolean = False; const RegExpandedValueSeparator: string = ' :: '): string;
begin
  if RegistryValue and ExpandedValue then Result := Self.RegistryFileName + RegExpandedValueSeparator + Self.ExpandedFileName
  else if RegistryValue then Result := Self.RegistryFileName
  else if ExpandedValue then Result := Self.ExpandedFileName
  else Result := '';
end;

function TDelphiPackageItem.IsExpanded: Boolean;
begin
  Result := RegistryFileName <> ExpandedFileName;
end;

{$endregion TDelphiPackageItem}



{$region '                       TDelphiTargetLibraryPaths                           '}

constructor TDelphiTargetLibraryPaths.Create(const TargetPlatform: TDelphiTargetPlatform; const RegPath: string; const RegRoot: HKEY;
  const UserVarList, IDEVarList: TMultiValueList);
begin
  FRegPath := RegPath;
  FRegFullPath := TReg.RootKeyToStr(RegRoot) + '\' + RegPath;
  FRegRoot := RegRoot;
  FTargetPlatform := TargetPlatform;
  FTargetPlatformName := TargetPlatformToStr(FTargetPlatform);
  FUserVarList := UserVarList;
  FIDEVarList := IDEVarList;
  FLibraryPathList := TDelphiPathList.Create;
  FBrowsingPathList := TDelphiPathList.Create;
  FDebugDcuPathList := TDelphiPathList.Create;
  Clear;
  ReadInfos;
end;

destructor TDelphiTargetLibraryPaths.Destroy;
begin
  Clear;
  FLibraryPathList.Free;
  FBrowsingPathList.Free;
  FDebugDcuPathList.Free;
  inherited;
end;

procedure TDelphiTargetLibraryPaths.Clear;
begin
  FLibraryPath.Clear;
  FBrowsingPath.Clear;
  FDebugDcuPath.Clear;
  FPackageOutputDir.Clear;
  FDcpOutputDir.Clear;
  FHppOutputDir.Clear;
  FTargetExists := False;
  FLibraryPathList.Clear;
  FBrowsingPathList.Clear;
  FDebugDcuPathList.Clear;
end;

function TDelphiTargetLibraryPaths.AsInfoStr: string;
begin
  Result :=
    'Target platform: ' + FTargetPlatformName + ENDL +
    'Target exists: ' + BoolToStrYesNo(FTargetExists);

  if not FTargetExists then Exit;

  Result := Result + ENDL +
    'BPL output directory: ' + FPackageOutputDir.AsString(True, True) + ENDL +
    'DCP output directory: ' + FDcpOutputDir.AsString(True, True) + ENDL +
    'HPP output directory: ' + FHppOutputDir.AsString(True, True) + ENDL +
    'Library paths: ' + itos(LibraryPathList.Count);

end;

function TDelphiTargetLibraryPaths.GetBrowsingPathListCount: integer;
begin
  Result := FBrowsingPathList.Count;
end;

function TDelphiTargetLibraryPaths.GetBrowsingPathItem(const Index: integer): TDelphiPathItem;
begin
  Result := FBrowsingPathList[Index];
end;

function TDelphiTargetLibraryPaths.GetDebugDcuPathItem(const Index: integer): TDelphiPathItem;
begin
  Result := FDebugDcuPathList[Index];
end;

function TDelphiTargetLibraryPaths.GetDebugDcuPathListCount: integer;
begin
  Result := FDebugDcuPathList.Count;
end;

function TDelphiTargetLibraryPaths.GetLibraryPathItem(const Index: integer): TDelphiPathItem;
begin
  Result := FLibraryPathList[Index];
end;

function TDelphiTargetLibraryPaths.GetLibraryPathListCount: integer;
begin
  Result := FLibraryPathList.Count;
end;

procedure TDelphiTargetLibraryPaths.ReadInfos;
var
  Reg: TRegistry;
  s: string;
  Arr: TStringDynArray;
  i: integer;
  PathItem: TDelphiPathItem;
begin
  Reg := TRegistry.Create(REG_ACCES_FLAG_READ_ONLY);
  try
    Reg.RootKey := FRegRoot;
    if not Reg.OpenKeyReadOnly(FRegPath) then Exit;

    FTargetExists := True;


    // Reg: Search Path / IDE: Library path
    FLibraryPath.Clear;
    Reg.TryGetString(REG_VAL_LIBRARY_PATH, FLibraryPath.RegistryPath);
    if FLibraryPath.RegistryPath <> '' then
    begin
      s := FLibraryPath.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FLibraryPath.ExpandedPath := s;
      FLibraryPath.PathExists := DirectoryExists(s);
    end;

    // Library path list
    TStr.SplitStrToArrayEx(FLibraryPath.RegistryPath, Arr, ';');
    TStr.RemoveEmptyStrings(Arr);
    FLibraryPathList.SetRegKeyName(REG_VAL_LIBRARY_PATH);
    FLibraryPathList.Count := Length(Arr);
    for i := 0 to Length(Arr) - 1 do
    begin
      s := Arr[i];
      PathItem.RegistryPath := s;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      PathItem.ExpandedPath := s;
      PathItem.PathExists := DirectoryExists(s);
      FLibraryPathList.Items[i] := PathItem;
    end;
    FLibraryPathList.UpdateStats;



    // Reg: Browsing Path / IDE: Browsing path
    FBrowsingPath.Clear;
    Reg.TryGetString(REG_VAL_BROWSING_PATH, FBrowsingPath.RegistryPath);
    if FBrowsingPath.RegistryPath <> '' then
    begin
      s := FBrowsingPath.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FBrowsingPath.ExpandedPath := s;
      FBrowsingPath.PathExists := DirectoryExists(s);
    end;

    // Browsing path list
    TStr.SplitStrToArrayEx(FBrowsingPath.RegistryPath, Arr, ';');
    TStr.RemoveEmptyStrings(Arr);
    FBrowsingPathList.SetRegKeyName(REG_VAL_BROWSING_PATH);
    FBrowsingPathList.Count := Length(Arr);
    for i := 0 to Length(Arr) - 1 do
    begin
      s := Arr[i];
      PathItem.RegistryPath := s;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      PathItem.ExpandedPath := s;
      PathItem.PathExists := DirectoryExists(s);
      FBrowsingPathList.Items[i] := PathItem;
    end;
    FBrowsingPathList.UpdateStats;



    // Reg: Debug DCU Path
    FDebugDcuPath.Clear;
    Reg.TryGetString(REG_VAL_DEBUG_DCU_PATH, FDebugDcuPath.RegistryPath);
    if FDebugDcuPath.RegistryPath <> '' then
    begin
      s := FDebugDcuPath.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FDebugDcuPath.ExpandedPath := s;
      FDebugDcuPath.PathExists := DirectoryExists(s);
    end;

    // Debug DCU path list
    TStr.SplitStrToArrayEx(FDebugDcuPath.RegistryPath, Arr, ';');
    TStr.RemoveEmptyStrings(Arr);
    FDebugDcuPathList.SetRegKeyName(REG_VAL_DEBUG_DCU_PATH);
    FDebugDcuPathList.Count := Length(Arr);
    for i := 0 to Length(Arr) - 1 do
    begin
      s := Arr[i];
      PathItem.RegistryPath := s;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      PathItem.ExpandedPath := s;
      PathItem.PathExists := DirectoryExists(s);
      FDebugDcuPathList.Items[i] := PathItem;
    end;
    FDebugDcuPathList.UpdateStats;



    // Reg: Package DPL Output / IDE: Package output directory      (BPL output dir)
    FPackageOutputDir.Clear;
    Reg.TryGetString(REG_VAL_BPL_OUTPUT_DIR, FPackageOutputDir.RegistryPath);
    if FPackageOutputDir.RegistryPath <> '' then
    begin
      s := FPackageOutputDir.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FPackageOutputDir.ExpandedPath := s;
      FPackageOutputDir.PathExists := DirectoryExists(s);
    end;


    // Reg: Package DCP Output / IDE: DCP output directory
    FDcpOutputDir.Clear;
    Reg.TryGetString(REG_VAL_DCP_OUTPUT_DIR, FDcpOutputDir.RegistryPath);
    if FDcpOutputDir.RegistryPath <> '' then
    begin
      s := FDcpOutputDir.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FDcpOutputDir.ExpandedPath := s;
      FDcpOutputDir.PathExists := DirectoryExists(s);
    end;


    // Reg: Hpp Output Directory
    FHppOutputDir.Clear;
    Reg.TryGetString(REG_VAL_HPP_OUTPUT_DIR, FHppOutputDir.RegistryPath);
    if FHppOutputDir.RegistryPath <> '' then
    begin
      s := FHppOutputDir.RegistryPath;
      s := ExpandDelphiVar(s, 'Platform', FTargetPlatformName);
      s := ExpandDelphiVars(s, FUserVarList);
      s := ExpandDelphiVars(s, FIDEVarList);
      FHppOutputDir.ExpandedPath := s;
      FHppOutputDir.PathExists := DirectoryExists(s);
    end;


  finally
    Reg.Free;
  end;
end;

{$endregion TDelphiTargetLibraryPaths}


{$region '                TDelphiPathList                    '}

function TDelphiPathList.AsArray(const RegistryValues: Boolean = True; const ExpandedValues: Boolean = False;
  const RegExpandedValueSeparator: string = ' -> '): TStringDynArray;
var
  i: integer;
begin
  SetLength(Result, 0);
  if (Count = 0) or ( (not RegistryValues) and (not ExpandedValues) ) then Exit;

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Items[i].AsString(RegistryValues, ExpandedValues, RegExpandedValueSeparator);
end;

procedure TDelphiPathList.SetRegKeyName(const ARegKeyName: string);
begin
  FRegKeyName := ARegKeyName;
end;

procedure TDelphiPathList.UpdateStats;
var
  Item: TDelphiPathItem;
begin
  FNonExistentPaths := 0;
  FExistentPaths := 0;
  for Item in Self do
    if Item.PathExists then Inc(FExistentPaths)
    else Inc(FNonExistentPaths);
end;

{$endregion TDelphiPathList}



{$region '                   TDelphiPathItem                   '}

function TDelphiPathItem.AsString(const RegistryValue: Boolean = True; const ExpandedValue: Boolean = False; const RegExpandedValueSeparator: string = ' -> '): string;
begin
  if RegistryValue and ExpandedValue then Result := Self.RegistryPath + RegExpandedValueSeparator + Self.ExpandedPath
  else if RegistryValue then Result := Self.RegistryPath
  else if ExpandedValue then Result := Self.ExpandedPath
  else Result := '';
end;

procedure TDelphiPathItem.Clear;
begin
  RegistryPath := '';
  ExpandedPath := '';
  PathExists := False;
end;

function TDelphiPathItem.IsExpanded: Boolean;
begin
  Result := RegistryPath <> ExpandedPath;
end;

{$endregion TDelphiPathItem}



{$region '              TDelphiPackageList                 '}

function TDelphiPackageList.AsArray(const RegistryValues: Boolean = True; const ExpandedValues: Boolean = False;
  const RegExpandedValSeparator: string =' :: '): TStringDynArray;
var
  Item: TDelphiPackageItem;
  i: integer;
  s: string;
begin
  SetLength(Result, 0);
  if (Count = 0) or ( (not RegistryValues) and (not ExpandedValues) ) then Exit;

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Item := Items[i];
    if RegistryValues and ExpandedValues then s := Item.RegistryFileName + RegExpandedValSeparator + Item.ExpandedFileName
    else if RegistryValues then s := Item.RegistryFileName
    else s := Item.ExpandedFileName;
    Result[i] := s;
  end;
end;

procedure TDelphiPackageList.UpdateStats;
var
  Item: TDelphiPackageItem;
begin
  FExistentPackages := 0;
  FNonExistentPackages := 0;
  for Item in Self do
    if Item.FileExists then Inc(FExistentPackages)
    else Inc(FNonExistentPackages);
end;

{$endregion TDelphiPackageList}







{$region ' ----------- helpers ------------- '}


function ExpandDelphiVar(const Path, VarName, VarValue: string): string;
begin
  Result := TStr.ReplaceAll(Path, TStr.EnsureBounds(VarName, '$(', ')'), VarValue, True);
end;

function ExpandDelphiVars(const Path: string; const VarList: TMultiValueList): string;
var
  mv: TMultiValue;
begin
  Result := Path;
  if Pos('$(', Path) = 0 then Exit;
  for mv in VarList do
    Result := TStr.ReplaceAll(Result, TStr.EnsureBounds(mv.Name, '$(', ')'), mv.StrValue, True);
end;

function TryStringToColorDef(const ColorStr: string; const Default: TColor): TColor;
var
  xColor: integer;
begin
  Result := Default;
  if IdentToColor(ColorStr, xColor) then Exit(TColor(xColor));
  if TryHexToInt(ColorStr, xColor) then Result := TColor(xColor);
end;


// TODO: uzupełnić
function LoadEditorHighlightListFromString(const Text: string; List: TDelphiEditorHighlightList): Boolean;
var
  sl: TStringList;
  //Format: TDelphiEditorHighlightFormat;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Text := Trim(Text);
  finally
    sl.Free;
  end;
end;


function LoadEditorHighlightListFromFile(const FileName: string; List: TDelphiEditorHighlightList): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  List.Clear;
  if not FileExists(FileName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Result := LoadEditorHighlightListFromString(sl.Text, List);
  finally
    sl.Free;
  end;
end;

function EditorHighlightListToStr(List: TDelphiEditorHighlightList; const OutputFormat: TDelphiEditorHighlightFormat): string;
var
  s: string;
  i: integer;
  hi: TDelphiHighlightItem;
begin
  s := '';

  case OutputFormat of
    dehfJson: s := s + '{' + ENDL + '  "DelphiEditorHighlight": {' + ENDL;
    dehfXml: s := s + '<?xml version="1.0" encoding="utf-8"?>' + ENDL + '<DelphiEditorHighlight>' + ENDL;

    // https://support.microsoft.com/en-us/help/310516/how-to-add-modify-or-delete-registry-subkeys-and-values-by-using-a-reg
    dehfReg: s := s + 'Windows Registry Editor Version 5.00' + ENDL; // ver. 5.00 - Win2000 and UP
  end;

  for i := 0 to List.Count - 1 do
  begin
    hi := List[i];

    if not hi.Enabled then Continue;

    case OutputFormat of
      dehfIni: s := s + ENDL + hi.AsIniStr;
      dehfReg: s := s + ENDL + hi.AsRegStr;
      dehfJson: s := s + ENDL + hi.AsJsonStr('    ');
      dehfXml: s := s + ENDL + hi.AsXmlStr('  ');
    end;

  end;

  case OutputFormat of
    dehfJson: s := TStr.TrimFromEnd(s, ',') + ENDL + '  }' + ENDL + ENDL + '}';
    dehfXml: s := s + ENDL + ENDL + '</DelphiEditorHighlight>' + ENDL;
    dehfReg: s := s + ENDL;
  end;

  Result := s;

end;

function ExportEditorHighlightList(const FileName: string; List: TDelphiEditorHighlightList; OutputFormat: TDelphiEditorHighlightFormat): Boolean;
var
  s: string;
  sl: TStringList;
begin
  Result := True;
  try
    s := EditorHighlightListToStr(List, OutputFormat);
    sl := TStringList.Create;
    try
      {$IFDEF HAS_TSTRINGS_WRITEBOM}sl.WriteBOM := True;{$ENDIF}
      sl.Text := s;

      // UTF-16 Little Endian with BOM
      // The same character encoding as the files exported by Regedit on Windows XP..10.
      if OutputFormat = dehfReg then sl.SaveToFile(FileName, TEncoding.Unicode)
      else sl.SaveToFile(FileName, TEncoding.UTF8)

    finally
      sl.Free;
    end;
  except
    Result := False;
  end;

end;

function ExportEditorHighlightList_AsIni(const FileName: string; List: TDelphiEditorHighlightList): Boolean;
begin
  Result := ExportEditorHighlightList(FileName, List, dehfIni);
end;


function ExportEditorHighlightList_AsJson(const FileName: string; List: TDelphiEditorHighlightList): Boolean;
begin
  Result := ExportEditorHighlightList(FileName, List, dehfJson);
end;


function PackageListToStr(const PackageList: TDelphiPackageList; Prefix: string = ''): string;
var
  pi: TDelphiPackageItem;
  s: string;
  x: integer;
begin
  s := '';
  x := 0;

  for pi in PackageList do
  begin
    Inc(x);
    s := s +
      Prefix + '-----------------------------------' + ENDL +
      Prefix + 'Package No: ' + itos(x) + ENDL +
      Prefix + 'Description: ' + pi.Description + ENDL +
      Prefix + 'File name: ' + pi.RegistryFileName + ENDL +
      Prefix + 'Expanded file name: ' + pi.ExpandedFileName + ENDL +
      Prefix + 'Is expanded: ' + BoolToStrYN(pi.IsExpanded) + ENDL +
      Prefix + 'File exists: ' + BoolToStrYN(pi.FileExists) + ENDL;
  end;

  Result := s;
end;

function MVListToNameValueStr(const List: TMultiValueList; const DataSeparator: string; Prefix: string): string;
var
  mv: TMultiValue;
begin
  Result := '';
  for mv in List do
    Result := Result + Prefix + mv.Name + DataSeparator + mv.StrValue + ENDL;
end;


function TargetPlatformToStr(const TargetPlatform: TDelphiTargetPlatform): string;
begin
  case TargetPlatform of
    dtpWin32: Result := 'Win32';
    dtpWin64: Result := 'Win64';
    dtpAndroid32: Result := 'Android32';
    dtpAndroid64: Result := 'Android64';
    dtpLinux32: Result := 'Linux32';
    dtpLinux64: Result := 'Linux64';
    dtpiOSDevice32: Result := 'iOSDevice32';
    dtpiOSDevice64: Result := 'iOSDevice64';
    dtpiOSSimulator: Result := 'iOSSimulator';
    dtpOSX32: Result := 'OSX32';
    dtpOSX64: Result := 'OSX64';
  else
    Result := '';
  end;
end;

function StrToTargetPlatform(const TargetPlatformName: string): TDelphiTargetPlatform;
var
  s: string;
begin
  s := Trim(UpperCase(TargetPlatformName));
  if s = 'WIN32' then Result := dtpWin32
  else if s = 'WIN64' then Result := dtpWin64
  else if s = 'ANDROID32' then Result := dtpAndroid32
  else if s = 'ANDROID64' then Result := dtpAndroid64
  else if s = 'LINUX32' then Result := dtpLinux32
  else if s = 'LINUX64' then Result := dtpLinux64
  else if s = 'IOSDEVICE32' then Result := dtpiOSDevice32
  else if s = 'IOSDEVICE64' then Result := dtpiOSDevice64
  else if s = 'IOSSIMULATOR' then Result := dtpiOSSimulator
  else if s = 'OSX32' then Result := dtpOSX32
  else if s = 'OSX64' then Result := dtpOSX64
  else Result := dtpUnknown;
end;

function DelphiNameToDelphiVersion(DelphiName: string): TDelphiVersion;
begin
  DelphiName := UpperCase(DelphiName);
  DelphiName := TStr.RemoveAll(DelphiName,
    [' ', '/', '\', '-', 'DELPHI', 'BORLAND', 'INPRISE', 'CODEGEAR', 'EMBARCADERO', 'RADSTUDIO', 'PERSONAL', 'STARTER', 'COMMUNITY',
    'DEVELOPER', 'PROFESSIONAL', 'CLIENTSERVER', 'ENTERPRISE', 'ARCHITECT',
    '10.0', '10.1', '10.2', '10.3', '10.4', '11.0' {<-- Alexandria}, '11.1', '11.2', '11.3', '11.4']
  );

  if DelphiName = '7' then Result := dv7
  else if DelphiName = '2005' then Result := dv2005
  else if DelphiName = '2006' then Result := dv2006
  else if DelphiName = '2007' then Result := dv2007
  else if DelphiName = '2009' then Result := dv2009
  else if DelphiName = '2010' then Result := dv2010
  else if (DelphiName = 'XE') or (DelphiName = 'XE1') then Result := dvXE
  else if DelphiName = 'XE2' then Result := dvXE2
  else if DelphiName = 'XE3' then Result := dvXE3
  else if DelphiName = 'XE4' then Result := dvXE4
  else if DelphiName = 'XE5' then Result := dvXE5
  else if DelphiName = 'APPMETHOD' then Result := dvAppMethod
  else if DelphiName = 'XE6' then Result := dvXE6
  else if DelphiName = 'XE7' then Result := dvXE7
  else if DelphiName = 'XE8' then Result := dvXE8
  else if DelphiName = 'SEATTLE' then Result := dvSeattle
  else if DelphiName = 'BERLIN' then Result := dvBerlin
  else if DelphiName = 'TOKYO' then Result := dvTokyo
  else if DelphiName = 'RIO' then Result := dvRio
  else if DelphiName = 'SYDNEY' then Result := dvSydney
  else if DelphiName = 'ALEXANDRIA' then Result := dvAlexandria

  else Result := dvUnknown;
end;

function DelphiVersionToDelphiName(const dv: TDelphiVersion): string;
begin
  case dv of
    dv7: Result := '7';
    dv2005: Result := '2005';
    dv2006: Result := '2006';
    dv2007: Result := '2007';
    dv2009: Result := '2009';
    dv2010: Result := '2010';
    dvXE: Result := 'XE';
    dvXE2: Result := 'XE2';
    dvXE3: Result := 'XE3';
    dvXE4: Result := 'XE4';
    dvXE5: Result := 'XE5';
    dvAppMethod: Result := 'AppMethod';
    dvXE6: Result := 'XE6';
    dvXE7: Result := 'XE7';
    dvXE8: Result := 'XE8';
    dvSeattle: Result := 'Seattle';
    dvBerlin: Result := 'Berlin';
    dvTokyo: Result := 'Tokyo';
    dvRio: Result := 'Rio';
    dvSydney: Result := 'Sydney';
    dvAlexandria: Result := 'Alexandria';
  else
    Result := '';
  end;
end;

function DelphiVersionToDelphiFullName(const dv: TDelphiVersion): string;
begin
  case dv of
    dv7: Result := 'Borland Delphi 7';
    dv2005: Result := 'Borland Delphi 2005';
    dv2006: Result := 'Borland Delphi 2006';
    dv2007: Result := 'Borland Delphi 2007';
    dv2009: Result := 'CodeGear Delphi 2009';
    dv2010: Result := 'CodeGear Delphi 2010';
    dvXE: Result := 'Embarcadero Delphi XE';
    dvXE2: Result := 'Embarcadero Delphi XE2';
    dvXE3: Result := 'Embarcadero Delphi XE3';
    dvXE4: Result := 'Embarcadero Delphi XE4';
    dvXE5: Result := 'Embarcadero Delphi XE5';
    dvAppMethod: Result := 'Embarcadero AppMethod';
    dvXE6: Result := 'Embarcadero Delphi XE6';
    dvXE7: Result := 'Embarcadero Delphi XE7';
    dvXE8: Result := 'Embarcadero Delphi XE8';
    dvSeattle: Result := 'Embarcadero Delphi 10.0 Seattle';
    dvBerlin: Result := 'Embarcadero Delphi 10.1 Berlin';
    dvTokyo: Result := 'Embarcadero Delphi 10.2 Tokyo';
    dvRio: Result := 'Embarcadero Delphi 10.3 Rio';
    dvSydney: Result := 'Embarcadero Delphi 10.4 Sydney';
    dvAlexandria: Result := 'Embarcadero Delphi 11.0 Alexandria';
  else
    Result := '';
  end;
end;

function DelphiNameToBdsVersion(DelphiName: string): TBdsVersion;
var
  dv: TDelphiVersion;
begin
  dv := DelphiNameToDelphiVersion(DelphiName);
  Result := DelphiVersionToBdsVersion(dv);
end;

function BDSVersionToBdsNumStr(const bdv: TBdsVersion): string;
begin
  case bdv of
    bdsv2: Result := '2.0';    // Delphi 7
    bdsv3: Result := '3.0';    // 2005
    bdsv4: Result := '4.0';    // 2006
    bdsv5: Result := '5.0';    // 2007
    bdsv6: Result := '6.0';    // 2009
    bdsv7: Result := '7.0';    // 2010
    bdsv8: Result := '8.0';    // XE
    bdsv9: Result := '9.0';    // XE2
    bdsv10: Result := '10.0';  // XE3
    bdsv11: Result := '11.0';  // XE4
    bdsv12: Result := '12.0';  // XE5
    bdsv13: Result := '13.0';  // AppMethod
    bdsv14: Result := '14.0';  // XE6
    bdsv15: Result := '15.0';  // XE7
    bdsv16: Result := '16.0';  // XE8
    bdsv17: Result := '17.0';  // 10.0 Seattle
    bdsv18: Result := '18.0';  // 10.1 Berlin
    bdsv19: Result := '19.0';  // 10.2 Tokyo
    bdsv20: Result := '20.0';  // 10.3 Rio
    bdsv21: Result := '21.0';  // 10.4 Sydney
    bdsv22: Result := '22.0';  // 11 Alexandria
  else
    Result := '';
  end;
end;

function DelphiVersionToBdsVersion(const dv: TDelphiVersion): TBdsVersion;
begin
  case dv of
    dv7: Result := bdsv2;
    dv2005: Result := bdsv3;
    dv2006: Result := bdsv4;
    dv2007: Result := bdsv5;
    dv2009: Result := bdsv6;
    dv2010: Result := bdsv7;
    dvXE: Result := bdsv8;
    dvXE2: Result := bdsv9;
    dvXE3: Result := bdsv10;
    dvXE4: Result := bdsv11;
    dvXE5: Result := bdsv12;
    dvAppMethod: Result := bdsv13;
    dvXE6: Result := bdsv14;
    dvXE7: Result := bdsv15;
    dvXE8: Result := bdsv16;
    dvSeattle: Result := bdsv17;
    dvBerlin: Result := bdsv18;
    dvTokyo: Result := bdsv19;
    dvRio: Result := bdsv20;
    dvSydney: Result := bdsv21;
    dvAlexandria: Result := bdsv22;
  else
    Result := bdsvUnknown;
  end;
end;

function DelphiNameToBdsNumStr(DelphiName: string): string;
var
  dv: TDelphiVersion;
  bdv: TBdsVersion;
begin
  dv := DelphiNameToDelphiVersion(DelphiName);
  bdv := DelphiVersionToBdsVersion(dv);
  Result := BDSVersionToBdsNumStr(bdv);
end;

function IsDelphiVariable(const VarName: string): Boolean;
begin
  Result :=
    TStr.Contains(VarName, 'BDS', True) or TStr.Contains(VarName, 'BCB', True) or TStr.Contains(VarName, 'DELPHI', True) or
    SameText(VarName, 'DEMOSDIR') or SameText(VarName, 'ProductVersion');
end;

function TryGetDelphiVersionsFromProjectVersion(const ProjectVersion: string; out DelphiVersions: TDelphiVersions): Boolean;
begin
  DelphiVersions.Clear;

  if MatchText(ProjectVersion, ['12.0']) then
  begin
    DelphiVersions.First := dv2009;
    DelphiVersions.Second := dv2010;
  end
  else if MatchText(ProjectVersion, ['12.2', '12.3']) then DelphiVersions.First := dvXE
  else if MatchText(ProjectVersion, ['13.4']) then DelphiVersions.First := dvXE2
  else if MatchText(ProjectVersion, ['14.3']) then DelphiVersions.First := dvXE3
  else if MatchText(ProjectVersion, ['14.4']) then
  begin
    DelphiVersions.First := dvXE3;
    DelphiVersions.Second := dvXE4;
  end
  else if MatchText(ProjectVersion, ['14.6']) then DelphiVersions.First := dvXE4
  else if MatchText(ProjectVersion, ['15.0', '15.1', '15.3']) then begin DelphiVersions.First := dvXE5; {ShowMessage('XE5');} end
  else if MatchText(ProjectVersion, ['15.2']) then DelphiVersions.First := dvAppMethod
  else if MatchText(ProjectVersion, ['15.4']) then DelphiVersions.First := dvXE6
  else if MatchText(ProjectVersion, ['16.0', '16.1']) then DelphiVersions.First := dvXE7
  else if MatchText(ProjectVersion, ['17.0', '17.1', '17.2']) then DelphiVersions.First := dvXE8
  else if MatchText(ProjectVersion, ['18.0']) then DelphiVersions.First := dvSeattle
  else if MatchText(ProjectVersion, ['18.1']) then
  begin
    DelphiVersions.First := dvSeattle;
    DelphiVersions.Second := dvBerlin;
  end
  else if MatchText(ProjectVersion, ['18.2']) then
  begin
    DelphiVersions.First := dvBerlin;
    DelphiVersions.Second := dvTokyo;
  end
  else if MatchText(ProjectVersion, ['18.3', '18.4']) then DelphiVersions.First := dvTokyo
  else if MatchText(ProjectVersion, ['18.5', '18.6', '18.7', '18.8']) then DelphiVersions.First := dvRio
  else if MatchText(ProjectVersion, ['19.0', '19.1', '19.2']) then DelphiVersions.First := dvSydney
  else if MatchText(ProjectVersion, ['19.3', '19.4']) then DelphiVersions.First := dvAlexandria;

  Result := DelphiVersions.First <> dvUnknown;
end;

{$endregion helpers}




{ TDelphiVersions }

procedure TDelphiVersions.Clear;
begin
  Self.First := dvUnknown;
  Self.Second := dvUnknown;
end;

function TDelphiVersions.AsString(const UseLongNames: Boolean = False; const RemoveBrandName: Boolean = False): string;
begin
  Result := '';
  if First = dvUnknown then Exit;
  if UseLongNames then
  begin
    Result := DelphiVersionToDelphiFullName(First);
    if Second <> dvUnknown then Result := Result + ' or ' + DelphiVersionToDelphiFullName(Second);
    if RemoveBrandName then
    begin
      Result := TStr.RemoveAll(Result, 'Borland ', True);
      Result := TStr.RemoveAll(Result, 'CodeGear ', True);
      Result := TStr.RemoveAll(Result, 'Embarcadero ', True);
    end;
  end
  else
  begin
    Result := DelphiVersionToDelphiName(First);
    if Second <> dvUnknown then Result := Result + ' or ' + DelphiVersionToDelphiName(Second);
  end;
end;

function TDelphiVersions.FirstAsString(const UseLongName: Boolean = False; const RemoveBrandName: Boolean = False): string;
begin
  Result := DelphiVersionToStr(First, UseLongName, RemoveBrandName);
end;



class function TDelphiVersions.DelphiVersionToStr(const dv: TDelphiVersion; const UseLongName: Boolean = False; const RemoveBrandName: Boolean = False): string;
begin
  Result := '';
  if dv = dvUnknown then Exit;

  if UseLongName then
  begin
    Result := DelphiVersionToDelphiFullName(dv);
    if RemoveBrandName then
    begin
      Result := TStr.RemoveAll(Result, 'Borland ', True);
      Result := TStr.RemoveAll(Result, 'CodeGear ', True);
      Result := TStr.RemoveAll(Result, 'Embarcadero ', True);
    end;
  end
  else
  begin
    Result := DelphiVersionToDelphiName(dv);
  end;
end;

{$ENDIF} // MSWINDOWS


end.


