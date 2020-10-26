unit JPL.Win.Registry;

{$I .\..\jp.inc}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses 
  Windows, ShellAPI, Messages, SysUtils, StrUtils, Classes, Registry, Types,      JPL.Console,
  JPL.Strings, JPL.TStr, JPL.Conversion;


{
  Lazarus
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdBinary, rdInteger, rdIntegerBigEndian,
                  rdLink, rdMultiString, rdResourceList, rdFullResourceDescriptor,  rdResourceRequirementList, rdInt64);

  XE2, Rio
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);
}


const
  HKEY_NONE = HKEY(integer($2785493));
  REG_QWORD = 11;
  REG_QWORD_LITTLE_ENDIAN = 11;


type

  TRegPath = record
    RootKey: HKEY;
    ComputerName: string;
    Key: string;
    procedure Clear;
  end;

  TRegistryHelper = class helper for TRegistry
    function ValueTypeExists(const ValueName: string; DataType: TRegDataType): Boolean;
    function TryGetString(const ValueName: string; out StrValue: string): Boolean;

    // Returns True  if:  int > 0    OR   string = '1'   OR   UpperCase(string) = 'TRUE'   OR   UpperCase(string) = 'YES'
    // Returns False if:  int <= 0   OR   string = '0'   OR   UpperCase(string) = 'FALSE'  OR   UpperCase(string) = 'NO'
    function TryGetBool(const ValueName: string; out BoolValue: Boolean): Boolean;
    function SaveKeyEx(const Key, FileName: string; var ErrorCode: integer): Boolean;
  end;

  TReg = record
    class function ExpandRootKey(const Key: string): string; static;
    class function IsPredefinedKey(const Key: HKEY): Boolean; static;
    class function GetRootKeyFromStr(const Key: string): HKEY; static;
    class function StrToRootKey(const RootKeyStr: string): HKEY; static;
    class function RootKeyToStr(const RootKey: HKEY): string; static;
    class function KeyExists(const RootKey: HKEY; const Key: string): Boolean; static;
    class function PathExists(const Path: string): Boolean; static;
    class procedure SplitPath(const Path: string; var RootKey: HKEY; var ComputerName, Key: string); overload; static;
    class procedure SplitPath(const Path: string; var RegPath: TRegPath); overload; static;
    class function RegJump(const Key: string; const ShowCmd: DWORD = SW_SHOWNORMAL; const SleepTime: DWORD = 80): Boolean; static;
  end;








implementation






{$Region '                           TReg                                 '}

class function TReg.ExpandRootKey(const Key: string): string;
var
  s: string;
begin
  Result := Key;

  s := UpperCase(Result);
  if s = 'HKCR' then Result := 'HKEY_CLASSES_ROOT'
  else if s = 'HKCU' then Result := 'HKEY_CURRENT_USER'
  else if s = 'HKLM' then Result := 'HKEY_LOCAL_MACHINE'
  else if s = 'HKU' then Result := 'HKEY_USERS'
  else if s = 'HKCC' then Result := 'HKEY_CURRENT_CONFIG'
  else if s = 'HKPD' then Result := 'HKEY_PERFORMANCE_DATA'
  else if s = 'HKDD' then Result := 'HKEY_DYN_DATA'
  else
  begin
    Result := TStr.ReplaceFirst(Result, 'HKCR\', 'HKEY_CLASSES_ROOT\', True);
    Result := TStr.ReplaceFirst(Result, 'HKCU\', 'HKEY_CURRENT_USER\', True);
    Result := TStr.ReplaceFirst(Result, 'HKLM\', 'HKEY_LOCAL_MACHINE\', True);
    Result := TStr.ReplaceFirst(Result, 'HKU\', 'HKEY_USERS\', True);
    Result := TStr.ReplaceFirst(Result, 'HKCC\', 'HKEY_CURRENT_CONFIG\', True);
    Result := TStr.ReplaceFirst(Result, 'HKPD\', 'HKEY_PERFORMANCE_DATA\', True);
  end;
end;

class function TReg.IsPredefinedKey(const Key: HKEY): Boolean;
begin
  Result :=
   (Key = HKEY_CLASSES_ROOT) or (Key = HKEY_CURRENT_CONFIG) or (Key = HKEY_CURRENT_USER) or //(Key = HKEY_CURRENT_USER_LOCAL_SETTINGS) or
   (Key = HKEY_LOCAL_MACHINE) or (Key = HKEY_PERFORMANCE_DATA) or //(Key = HKEY_PERFORMANCE_NLSTEXT) or (Key = HKEY_PERFORMANCE_TEXT) or
   (Key = HKEY_USERS);
end;

class function TReg.GetRootKeyFromStr(const Key: string): HKEY;
begin
  if ContainsText(Key, 'HKEY_CLASSES_ROOT') or ContainsText(Key, 'HKCR\') then Result := HKEY_CLASSES_ROOT
  else if ContainsText(Key, 'HKEY_CURRENT_USER') or ContainsText(Key, 'HKCU\') then Result := HKEY_CURRENT_USER
  else if ContainsText(Key, 'HKEY_LOCAL_MACHINE') or ContainsText(Key, 'HKLM\') then Result := HKEY_LOCAL_MACHINE
  else if ContainsText(Key, 'HKEY_USERS') or ContainsText(Key, 'HKU\') then Result := HKEY_USERS
  else if ContainsText(Key, 'HKEY_CURRENT_CONFIG') or ContainsText(Key, 'HKCC\') then Result := HKEY_CURRENT_CONFIG
  else if ContainsText(Key, 'HKEY_PERFORMANCE_DATA') or ContainsText(Key, 'HKPD\') then Result := HKEY_PERFORMANCE_DATA
  else if ContainsText(Key, 'HKEY_DYN_DATA') or ContainsText(Key, 'HKDD\') then Result := HKEY_DYN_DATA
  else Result := HKEY_NONE;
end;

class function TReg.StrToRootKey(const RootKeyStr: string): HKEY;
var
  us: string;
begin
  us := UpperCase(RootKeyStr);
  if (us = 'HKEY_CLASSES_ROOT') or (us = 'HKCR') then Result := HKEY_CLASSES_ROOT
  else if (us = 'HKEY_CURRENT_USER') or (us = 'HKCU') then Result := HKEY_CURRENT_USER
  else if (us = 'HKEY_LOCAL_MACHINE') or (us = 'HKLM') then Result := HKEY_LOCAL_MACHINE
  else if (us = 'HKEY_USERS') or (us = 'HKU') then Result := HKEY_USERS
  else if (us = 'HKEY_CURRENT_CONFIG') or (us = 'HKCC') then Result := HKEY_CURRENT_CONFIG
  else if (us = 'HKEY_PERFORMANCE_DATA') or (us = 'HKPD') then Result := HKEY_PERFORMANCE_DATA
  else if (us = 'HKEY_DYN_DATA') or (us = 'HKDD') then Result := HKEY_DYN_DATA
  else Result := HKEY_NONE;
end;

class function TReg.RootKeyToStr(const RootKey: HKEY): string;
begin
  if RootKey = HKEY_CLASSES_ROOT then Result := 'HKEY_CLASSES_ROOT'
  else if RootKey = HKEY_CURRENT_USER then Result := 'HKEY_CURRENT_USER'
  else if RootKey = HKEY_LOCAL_MACHINE then Result := 'HKEY_LOCAL_MACHINE'
  else if RootKey = HKEY_USERS then Result := 'HKEY_USERS'
  else if RootKey = HKEY_CURRENT_CONFIG then Result := 'HKEY_CURRENT_CONFIG'
  else if RootKey = HKEY_PERFORMANCE_DATA then Result := 'HKEY_PERFORMANCE_DATA'
  else if RootKey = HKEY_DYN_DATA then Result := 'HKEY_DYN_DATA'
  else Result := '';
end;

class function TReg.KeyExists(const RootKey: HKEY; const Key: string): Boolean;
const
  {$IFDEF CPU32}
  REG_ACCES_FLAG_READ_ONLY = STANDARD_RIGHTS_READ; // or KEY_WOW64_64KEY;
  {$ELSE}
  REG_ACCES_FLAG_READ_ONLY = STANDARD_RIGHTS_READ; // or KEY_WOW64_32KEY;
  {$ENDIF}
var
  phkResult: HKEY;
  samDesired: DWORD;
  x: integer;
begin
  Result := False;

  samDesired := REG_ACCES_FLAG_READ_ONLY;

  // https://docs.microsoft.com/en-us/windows/win32/api/winreg/nf-winreg-regopenkeyexw
  //   phkResult - A pointer to a variable that receives a handle to the opened key.
  //   If the key is not one of the predefined registry keys, call the RegCloseKey function
  //   after you have finished using the handle.
  x := RegOpenKeyEx(RootKey, PChar(Key), 0, samDesired, phkResult);
  if (phkResult <> 0) { and (not TReg.IsPredefinedKey(phkResult)) } then RegCloseKey(phkResult);

  if x <> ERROR_SUCCESS then Exit;

  Result := phkResult <> 0;
end;

class function TReg.PathExists(const Path: string): Boolean;
var
  RootKey: HKEY;
  ComputerName, RegKey: string;
begin
  Result := False;

  SplitPath(Path, RootKey, ComputerName, RegKey);
  if RootKey = HKEY_NONE then Exit;

  if RegKey <> '' then Result := TReg.KeyExists(RootKey, RegKey)
  else Result := True;
end;

class procedure TReg.SplitPath(const Path: string; var RootKey: HKEY; var ComputerName, Key: string);
var
  xLen, i: integer;
  Arr: TStringDynArray;
begin
  RootKey := HKEY_NONE;
  ComputerName := '';
  Key := '';

  TStr.SplitStrToArrayEx(Path, Arr, '\');
  RemoveEmptyStrings(Arr);
  xLen := Length(Arr);

  if xLen = 0 then Exit;

  RootKey := TReg.StrToRootKey(Arr[0]);

  if xLen = 1 then
  begin
    if RootKey = HKEY_NONE then Key := Arr[0];
    Exit;
  end;

  if RootKey <> HKEY_NONE then
  begin
    for i := 1 to Length(Arr) - 1 do Key := Key + Arr[i] + '\';
    Key := TStr.TrimFromEnd(Key, '\');
    Exit;
  end;

  RootKey := TReg.StrToRootKey(Arr[1]);
  if RootKey <> HKEY_NONE then
  begin
    ComputerName := Arr[0];
    for i := 2 to Length(Arr) - 1 do Key := Key + Arr[i] + '\';
  end
  else
    for i := 0 to Length(Arr) - 1 do Key := Key + Arr[i] + '\';

  Key := TStr.TrimFromEnd(Key, '\');

end;

class procedure TReg.SplitPath(const Path: string; var RegPath: TRegPath);
begin
  RegPath.Clear;
  TReg.SplitPath(Path, RegPath.RootKey, RegPath.ComputerName, RegPath.Key);
end;

  {$region '             TReg.RegJump                '}
class function TReg.RegJump(const Key: string; const ShowCmd: DWORD = SW_SHOWNORMAL; const SleepTime: DWORD = 80): Boolean;
const
  MAIN_WINDOW_CLASS = 'RegEdit_RegEdit'; // RegEdit main window class = RegEdit_RegEdit (Win2000..Win10)
var
  i: integer;
  hMainWindow, hEdit, hTreeWindow: HWND;
  sei: ShellExecuteInfo;
begin

  Result := False;

  hMainWindow := FindWindow(MAIN_WINDOW_CLASS, nil);

  // Launching the RegEdit (if the main window cannot be found)
  if hMainWindow = 0 then
  begin
    FillChar(sei, SizeOf(sei), 0);
    with sei do
    begin
      cbSize := SizeOf(sei);
      fMask := SEE_MASK_NOCLOSEPROCESS;
      lpVerb := PChar('open');
      lpFile := PChar('regedit.exe');
      nShow := ShowCmd;
    end;
    ShellExecuteEx(@sei);
    WaitForInputIdle(sei.hProcess, 2000);
    hMainWindow := FindWindow(MAIN_WINDOW_CLASS, nil);
  end;

  if hMainWindow = 0 then Exit;

  ShowWindow(hMainWindow, ShowCmd);



  // TreeView on the left: SysTreeView32 (window with the values: SysListView32)
  hTreeWindow := FindWindowEx(hMainWindow, 0, 'SysTreeView32', nil);
  if hTreeWindow = 0 then Exit;

  SetForegroundWindow(hTreeWindow); // <-- Activate and set focus. Probably not necessary

  // Collapsing the tree: "Left Arrow" key down x 32
  for i := 1 to 32 do
  begin
    SendMessage(hTreeWindow, WM_KEYDOWN, VK_LEFT, 0);
    Sleep(15);
  end;



  // Windows 10 Creators Update: The Edit control below the main menu
  hEdit := FindWindowEx(hMainWindow, 0, 'Edit', nil);

  // Setting the edit text and pressing the Enter key
  if hEdit > 0 then
  begin
    SetForegroundWindow(hEdit); // Activate edit
    SendMessage(hEdit, WM_SETTEXT, 0, 0);                                     // Clear the edit
    for i := 1 to Length(Key) do SendMessage(hEdit, WM_CHAR, Ord(Key[i]), 0); // Set edit text
    SendMessage(hEdit, WM_KEYDOWN, VK_RETURN, 0);                             // Press Enter
    Result := True;
  end

  else

  begin

    // Expand tree: 1-st level
    Sleep(SleepTime);
    SendMessage(hTreeWindow, WM_KEYDOWN, VK_RIGHT, 0);
    // Now all main registry branches should be visible: HKEY_CLASSES_ROOT, HKEY_CURRENT_USER...
    Sleep(SleepTime);


    for i := 1 to Length(Key) do
    begin
      // The "\" character marks the end of a key name.
      // Press the "Right Arrow" key and RegEdit automatically expands the selected key.
      if Key[i] = '\' then
      begin
        SendMessage(hTreeWindow, WM_KEYDOWN, VK_RIGHT, 0);
        Sleep(SleepTime);
      end
      else
        // Sending the names of successive keys character by character.
        // RegEdit automatically selects the key in the current branch.
        // You can do it manually, but you have to enter characters very quickly!
        SendMessage(hTreeWindow, WM_CHAR, Ord(Key[i]), 0);
    end;

    Result := True;

  end;

end;
  {$endregion TReg.RegJump}

{$endregion TReg}


{$region '                     TRegistryHelper                       '}

function TRegistryHelper.ValueTypeExists(const ValueName: string; DataType: TRegDataType): Boolean;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;
  if GetDataType(ValueName) <> DataType then Exit;
  Result := True;
end;

function TRegistryHelper.SaveKeyEx(const Key, FileName: string; var ErrorCode: integer): Boolean;
var
  TempKey: HKEY;
begin
  Result := False;
  TempKey := GetKey(Key);
  if TempKey = 0 then Exit;
  try
    ErrorCode := RegSaveKey(TempKey, PChar(FileName), nil);
  finally
    RegCloseKey(TempKey);
  end;
  Result := ErrorCode = ERROR_SUCCESS;
end;

function TRegistryHelper.TryGetBool(const ValueName: string; out BoolValue: Boolean): Boolean;
var
  DataType: TRegDataType;
  x: integer;
  s: string;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;

  DataType := GetDataType(ValueName);

  if DataType = TRegDataType.rdInteger then
  begin
    x := ReadInteger(ValueName);
    BoolValue := x > 0;
    Result := True;
  end

  else if DataType = TRegDataType.rdString then
  begin
    s := ReadString(ValueName);
    s := Trim(UpperCase(s));

    if (s = '1') or (s = 'TRUE') or (s = 'YES') then
    begin
      BoolValue := True;
      Exit(True);
    end
    else if (s = '0') or (s = 'FALSE') or (s = 'NO') then
    begin
      BoolValue := False;
      Exit(True);
    end;
  end;

end;

function TRegistryHelper.TryGetString(const ValueName: string; out StrValue: string): Boolean;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;
  if GetDataType(ValueName) <> TRegDataType.rdString then Exit;
  StrValue := ReadString(ValueName);
  Result := True;
end;

{$endregion TRegistryHelper}



{ TRegPath }

procedure TRegPath.Clear;
begin
  RootKey := HKEY_NONE;
  ComputerName := '';
  Key := '';
end;



end.
