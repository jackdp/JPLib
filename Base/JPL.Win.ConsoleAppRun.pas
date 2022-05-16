unit JPL.Win.ConsoleAppRun;

{
  Capture console application output by the GUI program.

  Based on the ExecConsoleApp function by Martin Lafferty (http://www.prel.co.uk).
  You can found original source at http://cc.embarcadero.com/item/14692
  Martin has not provided any license information, but on Embarcadero's page it is copyrighted
  as "No significant restrictions".

  My (jp) code: "Total free" - you can do with my code whatever you want.
}

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

{$IFDEF MSWINDOWS}

uses
  Windows, SysUtils;

type

  TConsoleNewLineProc = procedure(const ConProcessHandle: THandle; const OutputLine: string) of object;

  TConsoleAppRunner = class
  private const
    CR = #$0D;
    LF = #$0A;
    CRLF = #13#10;
    BUFFER_SIZE = 20480;
  private
    FConProcessHandle: DWORD;
    FConProcessID: DWORD;
    FConThreadHandle: DWORD;
    FConThreadID: DWORD;
    FConExitCode: DWORD;
    FExeCurrentDir: string;
    FIncludeStdErrorInOutput: Boolean;
    FOnConsoleNewLine: TConsoleNewLineProc;
    FShowConsole: Boolean;
    FTerminationWaitTime: DWORD;
    FWritePipe: THandle;
    FExecutable: string;
    FParams: string;
    FConsoleOutput: string;
    function GetCommandLine: string;
    procedure SetExeCurrentDir(const AValue: string);
    procedure SetExecutable(const AValue: string);
    procedure SetIncludeStdErrorInOutput(AValue: Boolean);
    procedure SetOnConsoleNewLine(AValue: TConsoleNewLineProc);
    procedure SetParams(const AValue: string);
    procedure SetShowConsole(AValue: Boolean);
    procedure Clear;
    procedure SetTerminationWaitTime(AValue: DWORD);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: Boolean; overload;
    function Execute(const AExecutable, AParams, AExeCurrentDir: string; AShowConsole: Boolean = False): Boolean; overload;

    property ConProcessHandle: DWORD read FConProcessHandle;
    property ConProcessID: DWORD read FConProcessID;
    property ConThreadHandle: DWORD read FConThreadHandle;
    property ConThreadID: DWORD read FConThreadID;
    property ConExitCode: DWORD read FConExitCode;
    property WritePipe: THandle read FWritePipe;
    property IncludeStdErrorInOutput: Boolean read FIncludeStdErrorInOutput write SetIncludeStdErrorInOutput;

    property Executable: string read FExecutable write SetExecutable;
    property Params: string read FParams write SetParams;
    property ExeCurrentDir: string read FExeCurrentDir write SetExeCurrentDir;
    property ShowConsole: Boolean read FShowConsole write SetShowConsole;

    property CommandLine: string read GetCommandLine;
    property OnConsoleNewLine: TConsoleNewLineProc read FOnConsoleNewLine write SetOnConsoleNewLine;
    property TerminationWaitTime: DWORD read FTerminationWaitTime write SetTerminationWaitTime;
    property ConsoleOutput: string read FConsoleOutput;
  end;


function GetConsoleAppOutput(const Exe: string; Params: string = ''; ExeCurrentDir: string = ''; IncludeStdErrInOutput: Boolean = False): string;

{$ENDIF} // MSWINDOWS



implementation


{$IFDEF MSWINDOWS}

constructor TConsoleAppRunner.Create;
begin
  inherited Create;
  FOnConsoleNewLine := nil;
  FTerminationWaitTime := 5000;
  FExecutable := '';
  FParams := '';
  FExeCurrentDir := '';
  FShowConsole := False;
  FIncludeStdErrorInOutput := False;
  Clear;
end;

destructor TConsoleAppRunner.Destroy;
begin
  inherited Destroy;
end;

procedure TConsoleAppRunner.Clear;
begin
  FConProcessHandle := 0;
  FConProcessID := 0;
  FConThreadHandle := 0;
  FConThreadID := 0;
  FConExitCode := 1;
  FWritePipe := 0;
  FConsoleOutput := '';
end;

function TConsoleAppRunner.GetCommandLine: string;
begin
  Result := '';
  if FExecutable = '' then Exit;
  Result := FExecutable;
  if Trim(FParams) <> '' then Result := Result + ' ' + FParams;
end;


{$region '              Execute                '}
function TConsoleAppRunner.Execute: Boolean;
var
  CmdLine: string;
  WShow: Word;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttributes: TSecurityAttributes;
  ThreadSecurityAttributes: TSecurityAttributes;

  TempHandle, PipeStdOut, ReadPipe, PipeStdInput: THandle;

  ReadBuf: array[0..BUFFER_SIZE - 1] of AnsiChar;
  BytesRead: DWORD;
  LineBuf: array[0..BUFFER_SIZE - 1] of AnsiChar;
  LineBufPtr: integer;
  AChar: AnsiChar;

  NewLine: Boolean;
  i: integer;


  procedure ProcessOutputLine;
  var
    sLine: string;
  begin
    LineBuf[LineBufPtr] := #0;
    {$IFDEF FPC}
    sLine := LineBuf;
    {$ELSE}
    sLine := Utf8ToString(LineBuf);
    //sLine := LineBuf;
    {$ENDIF}
    if NewLine then sLine := sLine + CRLF;
    NewLine := False;
    LineBufPtr := 0;

    if Assigned(OnConsoleNewLine) then OnConsoleNewLine(ProcessInfo.hProcess, sLine);
    FConsoleOutput := FConsoleOutput + sLine;// + CRLF;
  end;

begin
  Result := False;
  Clear;

  CmdLine := GetCommandLine;
  if CmdLine = '' then Exit;

  if FShowConsole then WShow := SW_SHOW else WShow := SW_HIDE;
  PipeStdOut := 0;
  PipeStdInput := 0;
  ReadPipe := 0;

  FillChar(StartupInfo{%H-}, SizeOf(StartupInfo), 0);
  FillChar(ReadBuf{%H-}, SizeOf(ReadBuf), 0);
  FillChar(SecurityAttributes{%H-}, SizeOf(SecurityAttributes), 0);

  LineBufPtr := 0;
  NewLine := True;

  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;
  if not CreatePipe(ReadPipe, PipeStdOut, @SecurityAttributes, 0) then RaiseLastOSError;

  ThreadSecurityAttributes.nLength := SizeOf(ThreadSecurityAttributes);
  ThreadSecurityAttributes.lpSecurityDescriptor := nil;


  try

    if Win32Platform = VER_PLATFORM_WIN32_NT then
      if not SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0) then RaiseLastOSError
      else
      begin
        if not DuplicateHandle(GetCurrentProcess, ReadPipe, GetCurrentProcess, @TempHandle, 0, True, DUPLICATE_SAME_ACCESS) then RaiseLastOSError;
        CloseHandle(ReadPipe);
        ReadPipe := TempHandle
      end;

    ///////////////////////////////////////////////////////////////////////
    SecurityAttributes.nLength := SizeOf(SecurityAttributes);
    SecurityAttributes.bInheritHandle := True;
    SecurityAttributes.lpSecurityDescriptor := nil;
    CreatePipe(PipeStdInput, FWritePipe, @SecurityAttributes, 0);
    ///////////////////////////////////////////////////////////////////////


    with StartupInfo do
    begin
      cb := SizeOf(StartupInfo);
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := WShow;
      hStdInput := PipeStdInput;
      hStdOutput := PipeStdOut;
      if FIncludeStdErrorInOutput then hStdError := PipeStdOut;
    end;

    if not CreateProcess(
      nil,
      PChar(CmdLine),
      nil,
      @ThreadSecurityAttributes,
      True,
      CREATE_NO_WINDOW or DETACHED_PROCESS,
      nil,
      PChar(FExeCurrentDir),
      StartupInfo,
      ProcessInfo{%H-}
    )
    then
      RaiseLastOSError
    else
      Result := True;

    /////////////////////////////////////////////
    FConProcessHandle := ProcessInfo.hProcess;
    FConProcessID := ProcessInfo.dwProcessId;
    FConThreadHandle := ProcessInfo.hThread;
    FConThreadID := ProcessInfo.dwThreadId;
    /////////////////////////////////////////////

    CloseHandle(PipeStdOut);
    CloseHandle(PipeStdInput);

    BytesRead := 0;

    try

      while ReadFile(ReadPipe, ReadBuf, SizeOf(ReadBuf), BytesRead, nil) do
      begin

        for i := 0 to BytesRead - 1 do
        begin

          AChar := ReadBuf[i];

          case AChar of
            //LF: NewLine := True;
            LF:
              begin
                NewLine := True;
                ProcessOutputLine;
              end;
            CR: ProcessOutputLine;
          else
            begin
              LineBuf[LineBufPtr] := AChar;
              Inc(LineBufPtr);
              if LineBufPtr >= (SizeOf(LineBuf) - 1) then
              begin
                NewLine := True;
                ProcessOutputLine;
              end;
            end;
          end;

        end; // for

      end; // while

      WaitForSingleObject(ProcessInfo.hProcess, FTerminationWaitTime);

      GetExitCodeProcess(ProcessInfo.hProcess, FConExitCode);
      ProcessOutputLine;


    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end



  finally
    CloseHandle(ReadPipe);
    CloseHandle(FWritePipe);

    FConProcessHandle := 0;
    FConProcessID := 0;
    FConThreadHandle := 0;
    FConThreadID := 0;
    FWritePipe := 0;
  end

end;

function TConsoleAppRunner.Execute(const AExecutable, AParams, AExeCurrentDir: string; AShowConsole: Boolean = False): Boolean;
begin
  SetExecutable(AExecutable);
  SetParams(AParams);
  SetExeCurrentDir(AExeCurrentDir);
  FShowConsole := AShowConsole;
  Result := Execute;
end;
{$endregion Execute}


procedure TConsoleAppRunner.SetTerminationWaitTime(AValue: DWORD);
begin
  if FTerminationWaitTime = AValue then Exit;
  FTerminationWaitTime := AValue;
end;

procedure TConsoleAppRunner.SetExecutable(const AValue: string);
begin
  if FExecutable = AValue then Exit;
  FExecutable := AValue;
  if (FExeCurrentDir = '') or (not DirectoryExists(FExeCurrentDir)) then FExeCurrentDir := ExtractFileDir(FExecutable);
end;

procedure TConsoleAppRunner.SetIncludeStdErrorInOutput(AValue: Boolean);
begin
  if FIncludeStdErrorInOutput = AValue then Exit;
  FIncludeStdErrorInOutput := AValue;
end;

procedure TConsoleAppRunner.SetOnConsoleNewLine(AValue: TConsoleNewLineProc);
begin
  FOnConsoleNewLine := AValue;
end;

procedure TConsoleAppRunner.SetExeCurrentDir(const AValue: string);
begin
  if FExeCurrentDir = AValue then Exit;
  FExeCurrentDir := AValue;
  if FExeCurrentDir = '' then FExeCurrentDir := ExtractFileDir(FExecutable);
end;

procedure TConsoleAppRunner.SetParams(const AValue: string);
begin
  if FParams = AValue then Exit;
  FParams := AValue;
end;

procedure TConsoleAppRunner.SetShowConsole(AValue: Boolean);
begin
  if FShowConsole = AValue then Exit;
  FShowConsole := AValue;
end;



function GetConsoleAppOutput(const Exe: string; Params: string = ''; ExeCurrentDir: string = ''; IncludeStdErrInOutput: Boolean = False): string;
var
  car: TConsoleAppRunner;
begin
  Result := '';
  car := TConsoleAppRunner.Create;
  try
    car.IncludeStdErrorInOutput := IncludeStdErrInOutput;
    car.Execute(Exe, Params, ExeCurrentDir);
    Result := car.ConsoleOutput;
  finally
    car.Free;
  end;
end;


{$ENDIF} // MSWINDOWS

end.

