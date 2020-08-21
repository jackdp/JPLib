unit JPL.Win.System;

interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}


uses
  Windows, SysUtils, Classes, Messages, ShellApi,
  JPL.Strings;


procedure GetEnvironmentList(sl: TStringList);
function GetEnvironmentString(EnvVar: string; AddPercents: Boolean = True): string;
function ExpandEnvironmentString(const EnvVar: string; AddPercents: Boolean = True): string;
function SysDir: string;
function WinDir: string;
function TempDir(ErrorResult: string = ''): string;
function UserName: string;
function ComputerName: string;
function MyDir: string;
function GetWindowsVersion: integer;
function SearchPathForFile(const ShortFileName: string; DefResult: string = ''; PathToSearch: string = ''): string;
function ExitWindows2: Boolean;
function RestartWindows: Boolean;
procedure ShowLastError(const Error: LongWord; MsgPrefix: string = ''; MsgTitle: string = 'System error'; dwHandle: DWORD = 0);
function ScreenWidth: integer;
function ScreenHeight: integer;
function SetWindowOnTop(const WinHandle: HWND; const OnTop: Boolean): Boolean;
function ShowFileInExplorer(const FileName: string; Handle: HWND = 0): Boolean;


const
  PBM_SETBKCOLOR = $2000 + 1;
  PBM_SETBARCOLOR = WM_USER + 9;

{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

function ShowFileInExplorer(const FileName: string; Handle: HWND = 0): Boolean;
var
  s: string;
begin
  s := '/select,' + FileName;
  Result := ShellExecute(Handle, 'open', 'explorer.exe', PChar(s), '', SW_SHOW) > 32;
end;


function SetWindowOnTop(const WinHandle: HWND; const OnTop: Boolean): Boolean;
var
  Flags: HWND;
begin
  if OnTop then Flags := HWND_TOPMOST else Flags := HWND_NOTOPMOST;
  Result := SetWindowPos(WinHandle, Flags, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
end;

function ScreenWidth: integer;
begin
  Result := GetSystemMetrics(SM_CXSCREEN);
end;

function ScreenHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYSCREEN);
end;

procedure ShowLastError(const Error: LongWord; MsgPrefix: string = ''; MsgTitle: string = 'System error'; dwHandle: DWORD = 0);
var
  lpBuffer: PChar;
begin
  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
    nil, Error,
    LANG_NEUTRAL or (SUBLANG_DEFAULT shl 10),
    PChar(@lpBuffer), 0, nil
  );
  MessageBox(dwHandle, PChar(MsgPrefix + string(lpBuffer)), PChar(MsgTitle), MB_OK or MB_ICONEXCLAMATION);
  LocalFree({%H-}Cardinal(lpBuffer));
end;


function RestartWindows: Boolean;
var
  vi: TOSVersionInfo;
  hToken: THandle;
  tp: TTokenPrivileges;
  ReturnLength: Cardinal;
begin
  Result := False;

  FillChar(tp{%H-}, SizeOf(tp), 0);
  vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(vi);

  if vi.dwPlatformId <> VER_PLATFORM_WIN32_NT then Result := ExitWindowsEx(EWX_REBOOT or EWX_FORCE, 0)
  else
    if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken{%H-}) then
      if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.PrivilegeCount := 1;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        if AdjustTokenPrivileges(hToken, False, tp, SizeOf(tp), tp, ReturnLength{%H-}) then
          Result := ExitWindowsEx(EWX_REBOOT or EWX_FORCE, 0);
      end;
end;


function ExitWindows2: Boolean;
var
  vi: TOSVersionInfo;
  hToken: THandle;
  tp: TTokenPrivileges;
  ReturnLength: Cardinal;
begin
  Result := False;

  vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if not GetVersionEx(vi) then Exit;

  FillChar(tp{%H-}, SizeOf(tp), 0);

  if vi.dwPlatformId <> VER_PLATFORM_WIN32_NT then Result := ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0)
  else
    if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken{%H-}) then
      if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.PrivilegeCount := 1;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        if AdjustTokenPrivileges(hToken, False, tp, SizeOf(tp), tp, ReturnLength{%H-}) then
          Result := ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0);
      end;
end;

function SearchPathForFile(const ShortFileName: string; DefResult: string; PathToSearch: string): string;
var
  Buffer: array[0..511] of Char;
  lpFilePart: PChar;
  dwX: DWORD;
begin
  Result := DefResult;
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  dwX := SearchPath(PChar(PathToSearch), PChar(ShortFileName), nil, Length(Buffer), Buffer, lpFilePart{%H-});
  if dwX = 0 then Exit;
  Result := Buffer;
end;

function GetWindowsVersion: integer;
var
  ovi: TOsVersionInfo;
begin
  {--
    VER_PLATFORM_WIN32s = 0;
    VER_PLATFORM_WIN32_WINDOWS = 1;
    VER_PLATFORM_WIN32_NT = 2;
  --}
  ovi.dwOSVersionInfoSize := SizeOf(ovi);
  if GetVersionEx(ovi) then Result := ovi.dwPlatformId
  else Result := -1; //<-- jeœli ERROR to Result = - 1
end;

function MyDir: string;
begin
  Result := rbs(ExtractFileDir(ParamStr(0)));
end;

function ComputerName: string;
var
  buffer: array[0..254] of Char;
  BufSize: DWORD;
begin
  BufSize := SizeOf(buffer);
  FillChar(buffer{%H-}, BufSize, 0);
  GetComputerName(buffer, BufSize);
  Result := buffer;
end;

function UserName: string;
var
  Buffer: array[0..254] of Char;
  BufSize: DWORD;
begin
  BufSize := SizeOf(Buffer);
  FillChar(Buffer{%H-}, BufSize, 0);
  GetUserName(Buffer, BufSize);
  Result := Buffer;
end;

function TempDir(ErrorResult: string = ''): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  Windows.GetTempPath(Length(Buffer), Buffer);
  if Buffer <> '' then Result := Buffer
  else Result := ErrorResult;
end;

function WinDir: string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  GetWindowsDirectory(Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

function SysDir: string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  GetSystemDirectory(Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

procedure GetEnvironmentList(sl: TStringList);
var
  Base, P: PChar;
  EnvStr: string;
begin
  Base := GetEnvironmentStrings;
  if Base = nil then Exit;
  P := Base;
  while P^ <> #0 do
  begin
    EnvStr := P;
    if EnvStr[1] <> '=' then sl.Add(EnvStr);
    P := P + Length(EnvStr) + 1;
  end;
  FreeEnvironmentStrings(Base);
end;

function GetEnvironmentString(EnvVar: string; AddPercents: Boolean = True): string;
var
  Buffer: array[0..2047] of Char;
begin
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  if AddPercents then EnvVar := '%' + EnvVar + '%';
  ExpandEnvironmentStrings(PChar(EnvVar), Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

function ExpandEnvironmentString(const EnvVar: string; AddPercents: Boolean = True): string;
begin
  Result := GetEnvironmentString(EnvVar, AddPercents);
end;

{$ENDIF} // MSWINDOWS

end.

