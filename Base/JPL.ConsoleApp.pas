unit JPL.ConsoleApp;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  2020.08.18
  Added JPL.Conversion to uses, removed helpers: Pad, itos
  Removed const ENDL
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}



interface

uses
  {$IFDEF MSWINDOWS}
  Windows, {$IFDEF DCC}ShellApi,{$ENDIF}
  {$ENDIF}
  SysUtils, {%H-}Types,
  JPL.Strings,
  JPL.Conversion,
  JPL.Console,
  JPL.SimpleLogger,
  JPL.CmdLineParser;


const

  OS_ID_UNKNOWN = 0;
  OS_ID_WINDOWS = 1;
  OS_ID_LINUX = 2;

  CON_DEFAULT_COLOR_ERROR_TEXT = TConsole.clWhiteText;
  CON_DEFAULT_COLOR_ERROR_BG = TConsole.clDarkRedBg;

  CON_DEFAULT_COLOR_WARNING_TEXT = TConsole.clWhiteText;
  CON_DEFAULT_COLOR_WARNING_BG = TConsole.clDarkMagentaBg;

  CON_DEFAULT_COLOR_HINT_TEXT = TConsole.clWhiteText;
  CON_DEFAULT_COLOR_HINT_BG = TConsole.clDarkGreenBg;

  {$IFDEF CPUX64}
  APP_BITS = 64;
  APP_BITS_STR = '64';
  {$ELSE}
  APP_BITS = 32;
  APP_BITS_STR = '32';
  {$ENDIF}


  // Windows and Linux only
  {$IFDEF MSWINDOWS}
  APP_OS = 'Windows';
  APP_OS_SHORT = 'Win';
  OS_ID = OS_ID_WINDOWS;
  {$ELSE}
    {$IFDEF LINUX}
    APP_OS = 'Linux';
    APP_OS_SHORT = 'Lin';
    OS_ID = OS_ID_LINUX;
    {$ELSE}
    APP_OS = '';
    APP_OS_SHORT = '';
    OS_ID = OS_ID_UNKNOWN;
    {$ENDIF}
  {$ENDIF}


{
  App variables:
    %AppName% - application name
    %AppFullName% - full application name (with version, date, OS...). Can not be used in FullNameFormat!
    %MajorVersion% - app. major version
    %MinorVersion% - app. minor version
    %OSShort% - operating system short name (eg. Win)
    %OS% - OS full name (eg. Windows)
    %Bits% - 32 or 64
    %AppDate% - release date of the application
    %ENDL% - end of line (CRLF on Windows)
    %Description% - short description of the application (displayed in banner)
    %Author% - author of the application
    %HomePage% - application homepage
    %HelpPage% - application help page
}
  CON_APP_FULL_NAME_DEFAULT_FORMAT = '%AppName% %MajorVersion%.%MinorVersion% [%OSShort% %Bits%-bit] (%AppDate%)';
  CON_APP_BANNER_DEFAULT_FORMAT = '%AppFullName%%ENDL%%Description%%ENDL%%Author%, %HomePage%';


type

  {$IFDEF MSWINDOWS}TJPConsoleTerminateProc = TProcedure;{$ENDIF}

  TJPConsoleAppVersion = record
    MajorVersion: Word;
    MinorVersion: Word;
    RevisionNumber: Word;
    BuildNumber: Word;
  end;


  { TJPConsoleApp }

  TJPConsoleApp = class
  private
    FAuthor: string;
    FBannerFormat: string;
    FCmd: TJPCmdLineParser;
    FDescription: string;
    FExamplesStr: string;
    FSourcePage: string;
    {$IFDEF MSWINDOWS}
    FTerminateProcedure: TJPConsoleTerminateProc;
    {$ENDIF}
    FExtraInfoStr: string;
    FHelpPage: string;
    FHomePage: string;
    FLicense: string;
    FLicenseName: string;
    FName: string;
    FShortUsageStr: string;
    FTerminated: Boolean;
    FTrimExtFromExeShortName: Boolean;
    FTryHelpStr: string;
    FUsageStr: string;
    FVersion: TJPConsoleAppVersion;
    FBits: Byte;
    FDate: TDateTime;
    FFullNameFormat: string;
    FConColor_Error: TConsoleColors;
    FConColor_Warning: TConsoleColors;
    FConColor_Hint: TConsoleColors;
    {$IFDEF MSWINDOWS}
    FRestoreOutputCodePageOnExit: Boolean;
    {$ENDIF}
    FUseColors: Boolean;
    FLogger: TJPSimpleLogger;
    FConsole: TConsole;
    function GetAppFullName: string;
    function GetCurrentDirectory: string;
    function GetExeDirectory: string;
    function GetExeFullName: string;
    function GetExePath: string;
    function GetExeShortName: string;
    function GetParamCount: integer;
    function GetParams(Index: integer): string;
    procedure SetFullNameFormat(AValue: string);
    function ExpandAppVars(const StrWithVars: string): string;

    {$IFDEF MSWINDOWS}
    procedure SetTrimExtFromExeShortName(AValue: Boolean);
    {$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init; // <-- user initialization
    procedure Run;  // MAIN procedure
    procedure Done; // <-- user finalization (freeing resources etc.)
    procedure Terminate; // Sets FTerminated to True and calls TerminateProcedure
    procedure TerminateWithExitCode(const ExCode: integer);

    function AppDateStr: string;
    function VersionStr(bShowRevAndBuildNumber: Boolean = False): string;
    function BannerStr: string;

    procedure Display(bBanner, bShortUsage, bUsage, bExtraInfo, bExamples, bTryHelp: Boolean);
    procedure DisplayBanner;
    procedure DisplayShortUsage;
    procedure DisplayUsage;
    procedure DisplayExtraInfo;
    procedure DisplayExamples;
    procedure DisplayTryHelp;
    procedure DisplayLicense;

    // Wyświetla łańcuch "s" w kolorach zdefiniowanych w "cc", ale tylko wtedy gdy UseColors = True.
    procedure DisplayColoredText(const s: string; const cc: TConsoleColors);
    procedure DisplayError(const ErrStr: string);
    procedure DisplayWarning(const WarnStr: string);
    procedure DisplayHint(const HintStr: string);
    procedure DisplayTaggedText(const TaggedText: string);

    {$IFDEF MSWINDOWS}
    procedure GoToUrl(const URL: string);
    procedure GoToHomePage;
    procedure GoToHelpPage;
    procedure GoToSourcePage;
    {$ENDIF}

    property Cmd: TJPCmdLineParser read FCmd;
    property AppName: string read FName write FName;
    property FullNameFormat: string read FFullNameFormat write SetFullNameFormat;
    property AppFullName: string read GetAppFullName;
    property MajorVersion: Word read FVersion.MajorVersion write FVersion.MajorVersion;
    property MinorVersion: Word read FVersion.MinorVersion write FVersion.MinorVersion;
    property RevisionNumber: Word read FVersion.RevisionNumber write FVersion.RevisionNumber;
    property BuildNumber: Word read FVersion.BuildNumber write FVersion.BuildNumber;
    property Date: TDateTime read FDate write FDate;
    property Description: string read FDescription write FDescription;
    property LicenseName: string read FLicenseName write FLicenseName;
    property License: string read FLicense write FLicense;
    property Author: string read FAuthor write FAuthor;
    property HomePage: string read FHomePage write FHomePage;
    property HelpPage: string read FHelpPage write FHelpPage;
    property SourcePage: string read FSourcePage write FSourcePage; // Github, Gitlab, s-f, OSDN...
    property BannerFormat: string read FBannerFormat write FBannerFormat;
    property UsageStr: string read FUsageStr write FUsageStr;
    property ShortUsageStr: string read FShortUsageStr write FShortUsageStr;
    property ExtraInfoStr: string read FExtraInfoStr write FExtraInfoStr;
    property ExamplesStr: string read FExamplesStr write FExamplesStr;
    property TryHelpStr: string read FTryHelpStr write FTryHelpStr;

    property ExeShortName: string read GetExeShortName;
    {$IFDEF MSWINDOWS}
    property TrimExtFromExeShortName: Boolean read FTrimExtFromExeShortName write SetTrimExtFromExeShortName;
    {$ENDIF}
    property ExeFullName: string read GetExeFullName;
    property ExeDirectory: string read GetExeDirectory;
    property ExePath: string read GetExePath;
    property CurrentDirectory: string read GetCurrentDirectory;

    {$IFDEF MSWINDOWS}
    // TerminateProcedure is called in the Terminate method.
    property ExitProcedure: TJPConsoleTerminateProc read FTerminateProcedure write FTerminateProcedure;
    {$ENDIF}
    property Terminated: Boolean read FTerminated;

    property ParamCount: integer read GetParamCount;
    property Params[Index: integer]: string read GetParams;

    {$IFDEF MSWINDOWS}
    property RestoreOutputCodePageOnExit: Boolean read FRestoreOutputCodePageOnExit write FRestoreOutputCodePageOnExit;
    {$ENDIF}
    property UseColors: Boolean read FUseColors write FUseColors;
    property ErrorTextColor: Byte read FConColor_Error.Text write FConColor_Error.Text;
    property ErrorBackgroundColor: Byte read FConColor_Error.Background write FConColor_Error.Background;
    property WarningTextColor: Byte read FConColor_Warning.Text write FConColor_Warning.Text;
    property WarningBackgroundColor: Byte read FConColor_Warning.Background write FConColor_Warning.Background;
    property HintTextColor: Byte read FConColor_Hint.Text write FConColor_Hint.Text;
    property HintBackgroundColor: Byte read FConColor_Hint.Background write FConColor_Hint.Background;
    property Logger: TJPSimpleLogger read FLogger;
    property Console: TConsole read FConsole;
  end;



implementation


constructor TJPConsoleApp.Create;
begin
  inherited Create;

  FTerminated := False;
  FCmd := TJPCmdLineParser.Create;
  FLogger := TJPSimpleLogger.Create;

  {$IFDEF MSWINDOWS}
  FTerminateProcedure := nil;
  {$ENDIF}
  FName := ClassName;
  FFullNameFormat := CON_APP_FULL_NAME_DEFAULT_FORMAT;
  FBits := APP_BITS;
  FDate := Now;
  FDescription := '';
  FillChar(FVersion, SizeOf(FVersion), 0);
  FLicenseName := '';
  FLicense := '';
  FAuthor := '';
  FHomePage := '';
  FHelpPage := '';
  FBannerFormat := CON_APP_BANNER_DEFAULT_FORMAT;
  FUsageStr := '';
  FShortUsageStr := '';
  FExtraInfoStr := '';
  FExamplesStr := '';
  FTryHelpStr := 'Try "' + ExeShortName + ' --help" for more information.';

  FUseColors := True;

  {$IFDEF MSWINDOWS}
  FRestoreOutputCodePageOnExit := True;
  {$ENDIF}

  FConColor_Error.Text := CON_DEFAULT_COLOR_ERROR_TEXT;
  FConColor_Error.Background := CON_DEFAULT_COLOR_ERROR_BG;
  FConColor_Warning.Text := CON_DEFAULT_COLOR_WARNING_TEXT;
  FConColor_Warning.Background := CON_DEFAULT_COLOR_WARNING_BG;
  FConColor_Hint.Text := CON_DEFAULT_COLOR_HINT_TEXT;
  FConColor_Hint.Background := CON_DEFAULT_COLOR_HINT_BG;

  Init; // <-- user initialization
end;

destructor TJPConsoleApp.Destroy;
begin

  Done; // <-- user finalization

  {$IFDEF MSWINDOWS}
  if FRestoreOutputCodePageOnExit then Console.RestoreOriginalOutputCodePage;
  {$ENDIF}

  FLogger.Free;
  FCmd.Free;
  inherited Destroy;
end;

function TJPConsoleApp.GetAppFullName: string;
begin
  // remove %AppFullName% from template
  FFullNameFormat := StringReplace(FFullNameFormat, '%AppFullName%', '', [rfIgnoreCase, rfReplaceAll]);
  Result := ExpandAppVars(FFullNameFormat);
end;

function TJPConsoleApp.GetCurrentDirectory: string;
begin
  Result := GetCurrentDir;
end;

function TJPConsoleApp.GetExeDirectory: string;
begin
  Result := ExtractFileDir(ParamStr(0));
end;

function TJPConsoleApp.GetExeFullName: string;
begin
  Result := ParamStr(0);
end;

function TJPConsoleApp.GetExePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function TJPConsoleApp.GetExeShortName: string;
begin
  Result := ExtractFileName(ParamStr(0));
  {$IFDEF MSWINDOWS}
  if TrimExtFromExeShortName then Result := ChangeFileExt(Result, '');
  {$ENDIF}
end;

function TJPConsoleApp.GetParamCount: integer;
begin
  Result := System.ParamCount;
end;

function TJPConsoleApp.GetParams(Index: integer): string;
begin
  Result := ParamStr(Index);
end;

function TJPConsoleApp.BannerStr: string;
begin
  Result := ExpandAppVars(FBannerFormat);
  Result := StringReplace(Result, '%AppFullName%', GetAppFullName, [rfIgnoreCase, rfReplaceAll]);
end;

procedure TJPConsoleApp.DisplayBanner;
begin
  Display(True, False, False, False, False, False);
end;

procedure TJPConsoleApp.DisplayShortUsage;
begin
  Display(False, True, False, False, False, False);
end;


procedure TJPConsoleApp.DisplayTaggedText(const TaggedText: string);
var
  {$IFDEF DELPHI2009_OR_BELOW}
  Arr: TStringDynArray;
  {$ELSE}
  Arr: {$IFDEF DCC}TArray<string>; {$ELSE}TStringArray; {$ENDIF}
  {$ENDIF}
  i: integer;
  s: string;
begin
  if not FUseColors then
  begin
    Write(TaggedText); // ToDo: Dodać usuwanie znaczników <color>
    Exit;
  end;

  JPL.Strings.SplitStrToArray(TaggedText, Arr{%H-});
  for i := 0 to High(Arr) do
  begin
    s := Arr[i];
    if Pos('<color=', s) > 0 then Console.WriteTaggedTextLine(s)
    else Writeln(s);
  end;
end;

procedure TJPConsoleApp.DisplayUsage;
begin
  Display(False, False, True, False, False, False);
end;

procedure TJPConsoleApp.DisplayExtraInfo;
begin
  Display(False, False, False, True, False, False);
end;

procedure TJPConsoleApp.DisplayExamples;
begin
  Display(False, False, False, False, True, False);
end;

procedure TJPConsoleApp.DisplayTryHelp;
begin
  Display(False, False, False, False, False, True);
end;

procedure TJPConsoleApp.DisplayLicense;
begin
  DisplayTaggedText(FLicense);
end;

procedure TJPConsoleApp.Display(bBanner, bShortUsage, bUsage, bExtraInfo, bExamples, bTryHelp: Boolean);
begin
  if bBanner then DisplayTaggedText(BannerStr);
  if bShortUsage then DisplayTaggedText(ShortUsageStr);
  if bUsage then DisplayTaggedText(UsageStr);
  if bExtraInfo then DisplayTaggedText(ExtraInfoStr);
  if bExamples then DisplayTaggedText(ExamplesStr);
  if bTryHelp then DisplayTaggedText(TryHelpStr);
end;

procedure TJPConsoleApp.DisplayColoredText(const s: string; const cc: TConsoleColors);
var
  {$IFDEF DELPHI2009_OR_BELOW}
  Arr: TStringDynArray;
  {$ELSE}
  Arr: {$IFDEF FPC}array of string;{$ELSE}TArray<string>;{$ENDIF}
  {$ENDIF}
  i: integer;
  s2: string;
begin
  if UseColors then
  begin
    JPL.Strings.SplitStrToArray(s, Arr{%H-});
    for i := 0 to High(Arr) do
    begin
      s2 := Arr[i];
      if cc.Text <> Console.clNone then Console.SetTextColor(cc.Text);
      if cc.Background <> Console.clNone then Console.SetBackgroundColor(cc.Background);
      Write(s2);
      Console.ResetColors;
      Writeln;
    end;
  end
  else Writeln(s);
end;

procedure TJPConsoleApp.DisplayError(const ErrStr: string);
begin
  DisplayColoredText(ErrStr, FConColor_Error);
end;

procedure TJPConsoleApp.DisplayWarning(const WarnStr: string);
begin
  DisplayColoredText(WarnStr, FConColor_Warning);
end;

procedure TJPConsoleApp.DisplayHint(const HintStr: string);
begin
  DisplayColoredText(HintStr, FConColor_Hint);
end;


procedure TJPConsoleApp.SetFullNameFormat(AValue: string);
begin
  if FFullNameFormat = AValue then Exit;
  FFullNameFormat := AValue;
end;

function TJPConsoleApp.ExpandAppVars(const StrWithVars: string): string;
var
  s: string;

  procedure rep(const Old, New: string);
  begin
    s := StringReplace(s, Old, New, [rfIgnoreCase, rfReplaceAll]);
  end;

begin
  s := StrWithVars;
  rep('%AppName%', FName);
  rep('%MajorVersion%', IntToStr(FVersion.MajorVersion));
  rep('%MinorVersion%', IntToStr(FVersion.MinorVersion));
  rep('%OSShort%', APP_OS_SHORT);
  rep('%OS%', APP_OS);
  rep('%Bits%', IntToStr(FBits));
  rep('%AppDate%', AppDateStr);
  rep('%ENDL%', ENDL);
  rep('%Description%', FDescription);
  rep('%Author%', FAuthor);
  rep('%HomePage%', FHomePage);
  rep('%HelpPage%', FHelpPage);
  Result := s;
end;

{$IFDEF MSWINDOWS}
procedure TJPConsoleApp.GoToUrl(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), '', '', SW_SHOW);
end;

procedure TJPConsoleApp.SetTrimExtFromExeShortName(AValue: Boolean);
begin
  if FTrimExtFromExeShortName = AValue then Exit;
  FTrimExtFromExeShortName := AValue;
end;

procedure TJPConsoleApp.GoToHomePage;
begin
  GoToUrl(HomePage);
end;

procedure TJPConsoleApp.GoToHelpPage;
begin
  GoToUrl(HelpPage);
end;

procedure TJPConsoleApp.GoToSourcePage;
begin
  GoToUrl(SourcePage);
end;

{$ENDIF}

procedure TJPConsoleApp.Init;
begin
  //
end;

procedure TJPConsoleApp.Run;
begin
  //
end;

procedure TJPConsoleApp.Done;
begin
  //
end;

procedure TJPConsoleApp.Terminate;
begin
  FTerminated := True;
  {$IFDEF MSWINDOWS}
  if Assigned(FTerminateProcedure) then FTerminateProcedure;
  {$ENDIF}
end;

procedure TJPConsoleApp.TerminateWithExitCode(const ExCode: integer);
begin
  ExitCode := ExCode;
  Terminate;
end;

function TJPConsoleApp.AppDateStr: string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FDate, Year, Month, Day);
  Result :=
    Pad(itos(Year), 2, '0') + '.' +
    Pad(itos(Month), 2, '0') + '.' +
    Pad(itos(Day), 2, '0');
end;

function TJPConsoleApp.VersionStr(bShowRevAndBuildNumber: Boolean): string;
begin
  Result := itos(FVersion.MajorVersion) + '.' + itos(FVersion.MinorVersion);
  if bShowRevAndBuildNumber then
    Result := Result + '.' + itos(FVersion.RevisionNumber) + '.' + itos(FVersion.BuildNumber);
end;



end.
