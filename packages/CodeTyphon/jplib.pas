{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit jplib;

{$warn 5023 off : no warning about unused units}
interface

uses
  JPL.CmdLineParser, JPL.Console.ColorParser, JPL.Console, JPL.ConsoleApp, JPL.Conversion, JPL.DateTime, JPL.Dialogs, JPL.Files, JPL.FileSearch, 
  JPL.FileSearcher, JPL.IniFile, JPL.JsonHelpers, JPL.LangMgr, JPL.Language, JPL.Math, JPL.MemIniFile, JPL.RTTI, JPL.StrHash, JPL.Strings.Ext, 
  JPL.Strings, JPL.Units, JPL.Utils, JPL.Win.Dialogs, JPL.Win.FileSystem, JPL.Win.Processes, JPL.Win.Shortcuts, JPL.Win.System, JPL.Win.VersionInfo, 
  JPL.StrList, JPL.Win.FileIcons, JPL.ColorArrays, JPL.Colors.ColorClass, JPL.Colors.List, JPL.Colors, JPL.SimpleLogger, JPL.TimeLogger, JPL.TStr, 
  JPL.TStringsHelper, JPL.Win.SimpleTimer, JPL.Rects, JPL.ArrayIniFile, JPL.Bytes, JPL.Win.Registry, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('jplib', @Register);
end.
