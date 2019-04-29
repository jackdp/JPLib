unit JPL.Win.Shortcuts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Registry,
  ShlObj, ActiveX, ComObj;
  //JPL.Strings;



procedure DeleteShortcut(const Folder, LnkShortName: string);
function ShortcutExists(const Folder, LnkShortName: string): Boolean;
procedure CreateShortcut(const Name, FileName, Folder, LnkShortName: string; Description: string = '');



implementation



procedure DeleteShortcut(const Folder, LnkShortName: string);
var
  Reg: TRegistry;
  dir: string;
begin
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False) then
      begin
        dir := ReadString(Folder);
        CloseKey;
        if FileExists(dir + '\' + LnkShortName) then DeleteFile(dir + '\' + LnkShortName);
      end;
    end;
  finally
    Reg.Free;
  end;
end;


function ShortcutExists(const Folder, LnkShortName: string): Boolean;
var
  Reg: TRegistry;
  dir: string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    with reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(
        'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
        false) then
      begin
        dir := ReadString(Folder);
        CloseKey;
        Result := FileExists(dir + '\' + LnkShortName);
        //ShowMessage(dir + '\' + LnkShortName);
      end;
    end;
  finally
    Reg.Free;
  end;
end;



procedure CreateShortcut(const Name, FileName, Folder, LnkShortName: string; Description: string = '');
var
  obj: IUnknown;
  link: IShellLink;
  pf: IPersistFile;
  fName: WideString;
  Reg: TRegistry;
  dir: string;
begin
  try
    CoInitialize(nil);
    obj := CreateComObject(CLSID_ShellLink);
    link := obj as IShellLink;
    pf := obj as IPersistFile;

    with link do
    begin
      SetPath(PChar(FileName));
      SetArguments('');
      SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
      if Description <> '' then SetDescription(PChar(Description)); //link.SetDescription(PChar(Name));
    end;

    Reg := TRegistry.Create;
    try
      with Reg do
      begin
        RootKey := HKEY_CURRENT_USER;
        OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', false);
        dir := ReadString(Folder);
        if not DirectoryExists(dir) then CreateDir(dir);
        CloseKey;
      end;
    finally
      Reg.Free;
    end;

    //fName := dir + '\' + Name + '.lnk';
    fName := dir + '\' + LnkShortName;

    pf.Save(PWChar(fName), false);
  except
  end;
end;



end.