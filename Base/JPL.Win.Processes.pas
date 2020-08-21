unit JPL.Win.Processes;

interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Windows, SysUtils, Classes, {$IFDEF FPC}JwaTlHelp32, JwaPsApi{$ELSE}TlHelp32, PsAPI{$ENDIF}, ShellAPI;


function GetProcessFileName(const ProcessID: DWORD): string;
function IsAppRunning(const FileName: string): Boolean;
function GetThreadFileName(const tID: DWORD): string;
procedure RunAsAdmin(const hWnd: HWND; const FileName, Parameters: string);
function GetProcessModulesCount(const ProcessID: DWORD): integer;


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}


function GetProcessModulesCount(const ProcessID: DWORD): integer;
var
  h: THandle;
  me32: TModuleEntry32;
begin
  Result := 0;

  h := CreateToolHelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
  try
    me32.dwSize := SizeOf(TModuleEntry32);
    if Module32First(h, me32) then
    repeat
      if me32.th32ProcessID = ProcessID then Inc(Result);
    until Integer(Module32Next(h, me32)) = 0;
  finally
    CloseHandle(h);
  end;
end;

procedure RunAsAdmin(const hWnd: HWND; const FileName, Parameters: string);
var
  {$IFDEF UNICODE}
  sei: TShellExecuteInfoW;
  {$ELSE}
  sei: TShellExecuteInfoA;
  {$ENDIF}
begin
  FillChar(sei{%H-}, SizeOf(sei), 0);
  sei.cbSize := sizeof(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(FileName);
  sei.lpParameters := PChar(Parameters);
  sei.nShow := SW_SHOWNORMAL;
  {$IFDEF UNICODE}
  if not ShellExecuteExW(@sei) then RaiseLastOSError;
  {$ELSE}
  if not ShellExecuteExA(@sei) then RaiseLastOSError;
  {$ENDIF}
end;

function GetThreadFileName(const tID: DWORD): string;
var
  snap: THandle;
  pe32: TProcessEntry32;
  te32: TThreadEntry32;
  arrProcesses: array of integer;
  i: integer;
  pID: DWORD;
begin
  Result := '';
  pID := 0;

  snap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    pe32.dwSize := SizeOf(TProcessEntry32);
    if integer(Process32First(snap, pe32)) <> 0 then
      repeat
        SetLength(arrProcesses, Length(arrProcesses) + 1);
        arrProcesses[Length(arrProcesses) - 1] := integer(pe32.th32ProcessID);
      until integer(Process32Next(snap, pe32)) = 0;
  finally
    CloseHandle(snap);
  end;

  for i := 0 to Length(arrProcesses) - 1 do
  begin
    snap := CreateToolHelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    try
      te32.dwSize := SizeOf(TThreadEntry32);
      if integer(Thread32First(snap, te32)) <> 0 then
        repeat
          if te32.th32ThreadID = tID then
          begin
            pID := te32.th32OwnerProcessID;
            Break;
          end;
        until integer(Thread32Next(snap, te32)) = 0;
    finally
      CloseHandle(snap);
    end;
  end;

  SetLength(arrProcesses, 0);
  if pID <> 0 then Result := GetProcessFileName(pID);
end;

function IsAppRunning(const FileName: string): Boolean;
var
  snap: THandle;
  pe32: TProcessEntry32;
begin
  Result := False;

  snap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  pe32.dwSize := SizeOf(TProcessEntry32);
  if integer(Process32First(snap, pe32)) <> 0 then
    repeat
      if AnsiUpperCase(FileName) = AnsiUpperCase(GetProcessFileName(pe32.th32ProcessID)) then
      begin
        Result := True;
        Break;
      end;
    until integer(Process32Next(snap, pe32)) = 0;
  CloseHandle(snap);
end;

function GetProcessFileName(const ProcessID: DWORD): string;
var
  hProc: HWND;
  buffer: array[0..1023] of Char;
  snap: THandle;
  pe32: TProcessEntry32;
begin
  Result := '';

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 3) then
  begin
    hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
    if hProc <> 0 then
    begin
      FillChar(buffer{%H-}, SizeOf(buffer), 0);
      GetModuleFileNameEx(hProc, 0, buffer, SizeOf(buffer));
      Result := buffer;
    end;
    CloseHandle(hProc);
  end
  else
  begin
    snap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    pe32.dwSize := SizeOf(TProcessEntry32);
    if integer(Process32First(snap, pe32)) <> 0 then
      repeat
        if integer(ProcessID) = integer(pe32.th32ProcessID) then
        begin
          Result := pe32.szExeFile;
          Break;
        end;
      until integer(Process32Next(snap, pe32)) = 0;
    CloseHandle(snap);
  end;
end;

{$ENDIF} // MSWINDOWS

end.
