unit JPL.Win.Processes;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  Windows, SysUtils, Classes, TlHelp32, PsAPI, ShellAPI;


function GetProcessFileName(const pID: DWORD): string;
function IsAppRunning(const FileName: string): Boolean;
function GetThreadFileName(const tID: DWORD): string;
procedure RunAsAdmin(hWnd: HWND; FileName, Parameters: string);



implementation


procedure RunAsAdmin(hWnd: HWND; FileName, Parameters: string);
var
  sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := sizeof(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(FileName);
  sei.lpParameters := PChar(Parameters);
  sei.nShow := SW_SHOWNORMAL;
  if not ShellExecuteEx(@sei) then RaiseLastOSError;
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
      if UpperCase(FileName) = UpperCase(GetProcessFileName(pe32.th32ProcessID)) then
      begin
        Result := True;
        Break;
      end;
    until integer(Process32Next(snap, pe32)) = 0;
  CloseHandle(snap);
end;

function GetProcessFileName(const pID: DWORD): string;
var
  hProc: HWND;
  buffer: array[0..1023] of Char;
  snap: THandle;
  pe32: TProcessEntry32;
begin
  Result := '';

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 3) then
  begin
    hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, pID);
    if hProc <> 0 then
    begin
      FillChar(buffer, SizeOf(buffer), 0);
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
        if integer(pID) = integer(pe32.th32ProcessID) then
        begin
          Result := pe32.szExeFile;
          Break;
        end;
      until integer(Process32Next(snap, pe32)) = 0;
    CloseHandle(snap);
  end;
end;



end.
