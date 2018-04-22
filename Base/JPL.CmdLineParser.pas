unit JPL.CmdLineParser;
{
  Jacek Pazera
  http://www.pazera-software.com

  Command line parser - Parser of parameters passed to the program on the command line.

  Assumptions: Using as few units as possible. No generics.collections, fgl, etc.

  Last mod: 2018.02.28

  2018.02.28 Removed dependency on JPL.Strings and JP.Utils
  2018.01.22
  [+] AcceptAllNonOptions - if True, all parameters that do not start with the "-" or "/" sign will be saved in FUnknownParams array,
                            and they will not be treated as errors.

}

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

{:<@include(..\doc\unit_JP.CmdLineParser.txt)}

interface

// function CommandLineToArgvW(lpCmdLine: LPCWSTR; var pNumArgs: Integer): PPWideChar; stdcall;
// function CommandLineToArgvW; external shell32 name 'CommandLineToArgvW';
// https://msdn.microsoft.com/pl-pl/library/windows/desktop/ms683156(v=vs.85).aspx - GetCommandLine function
// https://msdn.microsoft.com/pl-pl/library/windows/desktop/bb776391(v=vs.85).aspx - CommandLineToArgvW function
// https://msdn.microsoft.com/pl-pl/library/windows/desktop/17w5ykft(v=vs.85).aspx - Parsing C++ Command-Line Arguments

{ TODO : Parsowanie dowolnego łańcucha (nie tylko ParamStr/GetCommandLine)}
{ TODO : Dodać obsługę parametrów typu: -pValue (-p=Value) }// http://docwiki.embarcadero.com/Libraries/Berlin/en/System.SysUtils.FindCmdLineSwitch
{ DONE : Dodać obsługę Sticked Params (opcji "przypiętych" do określonych pozycji)}
{ DONE : Dodać obsługę poleceń (commands) }
{ TODO : Poszukać odpowiednika CommandLineToArgvW dla Linuxa }

// Mandatory arguments to long options are mandatory for short options too.

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils //, JPL.Strings //, JP.Utils
  ;


var
  {$IFDEF LINUX}
  ENDL: string = #10; //:< End of Line marker. Windows: #13#10 (CRLF), Linux: #10 (LF)
  {$ELSE}
  ENDL: string = #13#10; //:< End of Line marker. Windows: #13#10 (CRLF), Linux: #10 (LF)
  {$ENDIF}
  LongOptPrefix: string = '--'; //:< Long option prefix. Default '@--'
  EqChar: Char = '='; //:< Option-Value separator. @--option_nameEqCharOption_value
  DefaultStopParsingSwitch: string = '--'; //:< Domyślna wartość parametru, po którym przerywana jest analiza dalszych parametrów
  ShortOptPrefixes: set of {Ansi}Char = ['-', '/']; //:< Set of short option prefixes. Default @code('-'), @code('/').
  DefaultUsageArgName: string = 'ARG';

type


  //TArray<T> = array of T; // Delphi 2009

  TClpParsingMode = (cpmCustom, cpmDelphi); { TODO: Zablokować cpmCustom dla Linuksa. Na razie działa prawidłowo tylko na Windowsie. }
                                            { Does the CommandLineToArgvW equivalent for Linux exists? }

  TClpUsageFormat = (cufNone, cufSimple, cufWget);

  TArgv = array of string; // TArray<string>; <- Delphi 2009 exception???

  TClpParamType = (
    cptCommand,
    cptSticked, // "sticked" parameter to given position
    cptNone, // unknown
    cptShort, // short option
    cptMixedShort, // short_option=value
    cptLong, // long option
    cptMixedLong, // long option with value
    cptInvalid, // eg. --=
    cptStopMarker, // default '--' : Stop parsing options (like 7z.exe). Rar uses one dash: '-'.
    cptString //< "some value" OR "-abc=value" when cpmCustom parsing mode is enabled (Windows only)
  );

  TClpValueType = (cvtNone, cvtRequired, cvtOptional);
  TClpOptionType = (cotNone, cotShort, cotLong, cotBoth);


  TClpCommand = record
    ShortName: string;
    LongName: string;
    UsageInfo: string;
    Hidden: Boolean;
  end;

  TClpStickedParam = record
    Position: integer;
    ParamValue: string;
    UsageName: string;
    UsageInfo: string;
  end;

  TClpUnknownParam = record
    ParamStr: string;
    Index: integer;
  end;

  TClpParam = record
    ParamStr: string;
    OptionName: string;
    OptionValue: string;
    ParamType: TClpParamType;
    Index: integer;
    Parsed: Boolean;
  end;

  TClpOption = record
    ShortName: string;
    LongName: string;
    Value: string;
    ValueType: TClpValueType;
    OptionType: TClpOptionType;
    IsOptionNeeded: Boolean;
    Exists: Boolean;
    Parsed: Boolean;
    Hidden: Boolean;
    UsageInfo: string;
    UsageArgName: string;
    UsageCategory: string;
  end;

  TClpError = record
    Param: string;
    ErrorMsg: string;
    Index: integer;
  end;

  TClpOptionExtraInfo = record
    OptionName: string;
    LeftPadding: integer;
    InfoStr: string;
  end;


    {$REGION ' --- TJPCmdLineParser --- '}
    TJPCmdLineParser = class
    private
      FAcceptAllNonOptions: Boolean;
      FRunTimeParamsStr: string;
      FRunTimeParams: TArgv;
      FRunTimeParamCount: integer;

      FErrors: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpError>;     // array of TClpError;
      CLParams: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpParam>;    // wszystkie paramerty przechwycone z linii poleceń
      FOptions: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpOption>;   // lista zarejestrowanych opcji
      FSkippedParams: {$IFDEF FPC}specialize{$ENDIF} TArray<string>; // parametry, które wystąpiły po StopParsingSwitch
      FCommands: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpCommand>; // można zarejestrować wiele poleceń (commands), ale tylko jedno może (musi) być podane w linii poleceń
      FStickedParams: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpStickedParam>; // parametry przypisane ("przyklejone") do określonej pozycji
      FUnknownParams: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpUnknownParam>; // lista nieznanych parametrów

      // List of additional information for options. They will be used in the OptionsUsageStr function.
      FOptionsExtraInfo: {$IFDEF FPC}specialize{$ENDIF} TArray<TClpOptionExtraInfo>;

      FCommandLineParsingMode: TClpParsingMode;
      FStopParsingSwitch: string;
      FIgnoreCase: Boolean;
      FAllowDuplicates: Boolean;
      FUsageFormat: TClpUsageFormat;
      FCommandPosition: integer;
      FCommandValue: string;


      procedure ClearArrays;
      function GetRunTimeParam(Index: integer): string;
      function GetRunTimeParams: string;
      procedure FillRunTimeParamsArray;
      procedure SetAcceptAllNonOptions(AValue: Boolean);
      procedure SetCommandLineParsingMode(const Value: TClpParsingMode);
      procedure SetStopParsingSwitch(const Value: string);
      function StrToParamType(Param: string): TClpParamType;
      procedure Parse_1;
      procedure Parse_2;
      function IsShortOptPrefix(C: Char): Boolean;
      procedure SetIgnoreCase(const Value: Boolean);
      function GetError(Index: integer): TClpError;
      function GetErrorCount: integer;
      procedure LogError(const Param, ErrorMsg: string; const Index: integer);
      function GetErrorsStr: string;
      function GetOption(Index: integer): TClpOption;
      procedure SetAllowDuplicates(const Value: Boolean);
      function LongOptionsCount: integer;
      function ShortOptionsCount: integer;
      procedure SetUsageFormat(const Value: TClpUsageFormat);
      function GetSkippedParam(Index: integer): string;
      function GetSkippedParamsCount: integer;
      procedure FillSkippedParamsArray(const StartIndex: integer);
      function GetCommand(Index: integer): TClpCommand;
      function GetCommandCount: integer;
      function GetStickedParam(Index: integer): TClpStickedParam;
      function GetStickedParamCount: integer;
      function GetUnknownParam(Index: integer): TClpUnknownParam;
      function GetUnknownParamCount: integer;
      procedure LogUnknownParam(const ParamStr: string; const ParamIndex: integer);
      function GetOptionCount: integer;
      function GetParsedParam(Index: integer): TClpParam;
      function GetParsedParamCount: integer;

    public

      constructor Create;
      destructor Destroy; override;
      procedure ResetAll;
      procedure Parse;
      function ParamsAsString: string;


      // ------------------------------------- OPTIONS ----------------------------------------------
      function GetShortOptionIndex(OptionName: string): integer;
      function GetLongOptionIndex(OptionName: string): integer;
      function GetOptionIndex(const OptionName: string): integer;

      function GetShortOptionValue(OptionName: string; NoValueStr: string = ''): string;
      function GetLongOptionValue(OptionName: string; NoValueStr: string = ''): string;
      function GetOptionValue(const OptionName: string; NoValueStr: string = ''): string;

      function TryGetOptionValueAsBool(
        const OptionName: string; out ResultValue: Boolean; TrueStr: string = '1|y|yes'; FalseStr: string = '0|n|no';
        IgnoreBoolStrCase: Boolean = True
      ): Boolean;


      function IsShortOptionExists(OptionName: string): Boolean;
      function IsLongOptionExists(OptionName: string): Boolean;
      function IsOptionExists(OptionName: string): Boolean;

      procedure RegisterShortOption(const OptionName: string; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False; Hidden: Boolean = False;
        UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');

      procedure RegisterLongOption(const OptionName: string; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False; Hidden: Boolean = False;
        UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');

      procedure RegisterOption(ShortName: string; LongName: string = ''; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False; Hidden: Boolean = False;
        UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');

      function OptionsAsString: string;
      function OptionsUsageStr(Prefix: string = '  '; Category: string = ''; MaxLineLen: integer = 90; OptionsToInfoSep: string = '  '; MaxPadding: integer = 30): string;


      // ------------------------------------- COMMANDS ----------------------------------------------------
      procedure RegisterCommand(ShortName: string; LongName: string = ''; Position: integer = 1; Hidden: Boolean = False; Description: string = '');
      procedure AddCommand(ShortName: string; LongName: string = ''; Hidden: Boolean = False; Description: string = '');
      function GetCommandIndex(CommandName: string): integer;
      function CommandExists(CommandName: string): Boolean;
      function CommandsAsString: string;
      function CommandsUsageStr(Prefix: string = '  '; MaxLineLen: integer = 90; CommandToInfoSep: string = '  '; MaxPadding: integer = 30): string;

      // --------------------------------------- STICKED PARAMS -------------------------------------------
      function GetStickedParamIndex(const ParamPosition: integer): integer;
      function StickedParamExists(const ParamPosition: integer): Boolean;
      function GetStickedParamValue(const ParamPosition: integer): string;
      procedure RegisterStickedParam(const ParamPosition: integer; UsageName: string; UsageInfo: string = '');
      function StickedParamsAsString: string;


      // ----------------- Additional information for options. Used by the OptionsUsageStr func. -------------------------
      function GetOptionExtraInfoIndex(const OptionName: string): integer;
      procedure SetOptionExtraInfo(const OptionName, Info: string; Padding: integer = 4);
      function TryGetOptionExtraInfo(const OptionName: string; out oei: TClpOptionExtraInfo): Boolean;


      property UsageFormat: TClpUsageFormat read FUsageFormat write SetUsageFormat;

      // Run time params - parametry przekazane w linii poleceń (not parsed)
      property RunTimeParamsStr: string read FRunTimeParamsStr;
      property RunTimeParams[Index: integer]: string read GetRunTimeParam;
      property RunTimeParamCount: integer read FRunTimeParamCount;

      // Parsed params - parametry przetworzone (z wartościami)
      property ParsedParamCount: integer read GetParsedParamCount;
      property ParsedParam[Index: integer]: TClpParam read GetParsedParam;

      property CommandLineParsingMode: TClpParsingMode read FCommandLineParsingMode write SetCommandLineParsingMode;
      property StopParsingSwitch: string read FStopParsingSwitch write SetStopParsingSwitch;
      property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase;

      property Errors[Index: integer]: TClpError read GetError;
      property ErrorCount: integer read GetErrorCount;
      property ErrorsStr: string read GetErrorsStr;

      property OptionCount: integer read GetOptionCount;
      property Options[Index: integer]: TClpOption read GetOption;

      property AllowDuplicates: Boolean read FAllowDuplicates write SetAllowDuplicates;

      property SkippedParams[Index: integer]: string read GetSkippedParam;
      property SkippedParamsCount: integer read GetSkippedParamsCount;

      property Commands[Index: integer]: TClpCommand read GetCommand;
      property CommandCount: integer read GetCommandCount;
      property CommandPosition: integer read FCommandPosition;
      property CommandValue: string read FCommandValue;

      property StickedParams[Index: integer]: TClpStickedParam read GetStickedParam;
      property StickedParamCount: integer read GetStickedParamCount;

      property UnknownParams[Index: integer]: TClpUnknownParam read GetUnknownParam;
      property UnknownParamCount: integer read GetUnknownParamCount;

      // If AcceptAllNonOptions = True, all parameters that do not start with the "-" or "/" sign
      // will be saved in FUnknownParams array.
      property AcceptAllNonOptions: Boolean read FAcceptAllNonOptions write SetAcceptAllNonOptions;


    end;
    {$ENDREGION}



{$region ' --- INT helpers --- '}
function BoolToStr(const B: Boolean; ResultIfTrue: string = 'Yes'; ResultIfFalse: string = 'No'): string;
function SplitText(Text: string; MaxLen, Padding: integer; PadFirstLine: Boolean = False; PaddingChar: Char = ' '; ENDL: string = #13#10): string;
procedure SplitCmdLine(CmdLine: string; var argv: TArgv);
function ParamTypeToStr(const ParamType: TClpParamType): string;
function CLParamToStr(CLParam: TClpParam; PaddingStr: string = '  '): string;
function OptionRecToStr(Option: TClpOption; PaddingStr: string = '  '): string;
procedure ClearOptionRec(var Option: TClpOption);
function CommandToStr(Command: TClpCommand; PaddingStr: string = '  '): string;
function StickedParamToStr(StickedParam: TClpStickedParam; PaddingStr: string = '  '): string;
function StripQuotes(s: string; QChar: Char = '"'): string;
{$endregion helpers}


implementation

{$region '      copied from JPL units         '}

function PadRight(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  s := '';
  if Length(Text) < i then
  begin
    x := Length(Text);
    y := i - x;
    for k := 1 to y do s := s + znak;
    Text := Text + s;
  end;
  Result := Text;
end;

procedure SplitStrToArray(s: string; var Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>; const EndLineStr: string = sLineBreak);
var
  x: integer;
begin
  SetLength(Arr, 0);
  if s = '' then Exit;

  x := Pos(EndLineStr, s);
  while x > 0 do
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := Copy(s, 1, x - 1);
    s := Copy(s, x + Length(EndLineStr), Length(s));
    x := Pos(EndLineStr, s);
  end;

  if s <> '' then
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := s;
  end;
end;
{$endregion copied from JPL units}



constructor TJPCmdLineParser.Create;
begin
  inherited;
  ResetAll;
end;

destructor TJPCmdLineParser.Destroy;
begin
  ClearArrays;
  inherited;
end;

procedure TJPCmdLineParser.ResetAll;
begin
  ClearArrays;
  FCommandPosition := -1;
  FCommandValue := '';
  FAllowDuplicates := False;
  FUsageFormat := cufSimple;
  FStopParsingSwitch := DefaultStopParsingSwitch;
  FCommandLineParsingMode := cpmCustom;
  FRunTimeParamsStr := GetRunTimeParams;
  FillRunTimeParamsArray;
end;

procedure TJPCmdLineParser.ClearArrays;
begin
  SetLength(FRunTimeParams, 0);
  SetLength(FErrors, 0);
  SetLength(CLParams, 0);
  SetLength(FOptions, 0);
  SetLength(FCommands, 0);
  SetLength(FStickedParams, 0);
  SetLength(FUnknownParams, 0);
  SetLength(FOptionsExtraInfo, 0);
end;

{$region ' ------------------------------- PARSE -------------------------------- '}
procedure TJPCmdLineParser.Parse;
begin
  Parse_1;
  Parse_2;
end;

procedure TJPCmdLineParser.Parse_1; // faza wstępna
var
  i, Position: integer;
  RTParam: string;
  CLP: TClpParam;
begin
  SetLength(CLParams, 0);

  for i := 0 to Length(FRunTimeParams) - 1 do
  begin

    RTParam := FRunTimeParams[i];
    if RTParam = '' then Continue;

    //Writeln(RTParam);

    SetLength(CLParams, Length(CLParams) + 1);

    Position := i + 1;

    CLP.ParamStr := RTParam;

    if FCommandPosition = Position then CLP.ParamType := cptCommand
    else if StickedParamExists(Position) then CLP.ParamType := cptSticked
    else CLP.ParamType := StrToParamType(RTParam);

    CLP.Index := i;
    CLP.Parsed := False;
    CLP.OptionName := '';
    CLP.OptionValue := '';
    CLParams[Length(CLParams) - 1] := CLP;

    //if CLP.ParamType = cptInvalid then LogError(RTParam, 'Invalid parameter', i);

  end;
end;

{$hints off}
procedure TJPCmdLineParser.Parse_2; // faza 2-ga (końcowa)

  function StripPrefix(ClpParam: TClpParam): string;
  var
    bLong, bShort: Boolean;
    s: string;
  begin
    bLong := (ClpParam.ParamType = cptLong) or (ClpParam.ParamType = cptMixedLong);
    bShort := (ClpParam.ParamType = cptShort) or (ClpParam.ParamType = cptMixedShort);
    s := ClpParam.ParamStr;
    if bLong then Delete(s, 1, Length(LongOptPrefix))
    else if bShort then Delete(s, 1, 1)
    else s := '';
    Result := s;
  end;

  function IsOption(ClpParam: TClpParam): Boolean;
  begin
    Result := (ClpParam.ParamType = cptShort) or (ClpParam.ParamType = cptMixedShort) or
      (ClpParam.ParamType = cptLong) or (ClpParam.ParamType = cptMixedLong);
  end;

  function IsMixedOption(ClpParam: TClpParam): Boolean;
  begin
    Result := (ClpParam.ParamType = cptMixedShort) or (ClpParam.ParamType = cptMixedLong);
  end;

  function IsNotMixedOption(ClpParam: TClpParam): Boolean;
  begin
    Result := (ClpParam.ParamType = cptShort) or (ClpParam.ParamType = cptLong);
  end;

  function IsShortOption(ClpParam: TClpParam): Boolean;
  begin
    Result := (ClpParam.ParamType = cptShort) or (ClpParam.ParamType = cptMixedShort);
  end;

  function IsLongOption(ClpParam: TClpParam): Boolean;
  begin
    Result := (ClpParam.ParamType = cptLong) or (ClpParam.ParamType = cptMixedLong);
  end;

  procedure GetNameVal(const MixedOpt: string; var sOptName, sOptValue: string);
  var
    x: integer;
  begin
    sOptName := '';
    sOptValue := '';
    x := Pos(EqChar, MixedOpt);
    if x < 0 then Exit;
    sOptName := Copy(MixedOpt, 1, x - 1);
    sOptValue := Copy(MixedOpt, x + 1, Length(MixedOpt));
  end;

  function IsOptionAcceptsValue(Option: TClpOption): Boolean;
  begin
    Result := (Option.ValueType = cvtRequired) or (Option.ValueType = cvtOptional);
  end;

  function IsOptionValueRequired(Option: TClpOption): Boolean;
  begin
    Result := Option.ValueType = cvtRequired;
  end;


var
  i, IndNext, xOptInd, xInd: integer;
  ParamRec, ParamRecNext: TClpParam;
  Option: TClpOption;
  sOpt, sVal: string;
  bVal, bParamParsed: Boolean;

begin
//  Writeln('--------- PARSE_2 ------------');
//  Writeln('Params: ', Length(CLParams));

  for i := 0 to Length(CLParams) - 1 do
  begin

    if i < Length(CLParams) - 1 then IndNext := i + 1 else IndNext := -1;


    ParamRec := CLParams[i];
    if ParamRec.Parsed then Continue;

    bVal := False;
    bParamParsed := True;


    // -------------------------------------- COMMAND --------------------------------------
    if ParamRec.ParamType = cptCommand then
    begin
      ParamRec.Parsed := True;
      if CommandExists(ParamRec.ParamStr) then FCommandValue := ParamRec.ParamStr
      else
      begin
        FCommandValue := '';
        LogError(ParamRec.ParamStr, 'Unknown command', i);
      end;
      CLParams[i] := ParamRec;
      Continue;
    end


    // ----------------------------------- STICKED PARAM ------------------------------------------
    else if ParamRec.ParamType = cptSticked then
    begin
      ParamRec.Parsed := True;
      xInd := GetStickedParamIndex(i + 1);
      if xInd >= 0 then FStickedParams[xInd].ParamValue := ParamRec.ParamStr
      else LogError(ParamRec.ParamStr, 'Sticked parameter not registered at position ' + IntToStr(i + 1), i);
      CLParams[i] := ParamRec;
      Continue;
    end


    // -------------------------- STOP PARSING MARKER ------------------------------------------------------
    // Koniec analizy parametrów. Wszystkie pozostałe parametry trafiają do tablicy FSkippedParams
    else if ParamRec.ParamType = cptStopMarker then
    begin
      CLParams[i].Parsed := True;
      FillSkippedParamsArray(i + 1);
      Break;
    end



    // ------------------------------------- OPTIONS -------------------------------------------------------
    // bieżący parametr jest opcją (rozpoczyna się od '-' lub '--')
    else if IsOption(ParamRec) then
    begin

      sOpt := StripPrefix(ParamRec);
      sVal := '';

      if IsMixedOption(ParamRec) then
      begin
        bVal := True;
        GetNameVal(sOpt, sOpt, sVal);
      end;

      ParamRec.OptionName := sOpt;


      if IsShortOption(ParamRec) then xOptInd := GetShortOptionIndex(ParamRec.OptionName)
      else xOptInd := GetLongOptionIndex(ParamRec.OptionName);

      if xOptInd >= 0 then
      begin

        Option := FOptions[xOptInd];
        Option.Exists := True;

        if IsOptionAcceptsValue(Option) then
          if bVal then Option.Value := sVal
          else
            // sprawdzenie, czy następny parametr zawiera wartość (value) dla bieżącej opcji
            if IndNext > 0 then
            begin
              ParamRecNext := CLParams[IndNext];
              if (ParamRecNext.ParamType = cptNone) or (ParamRecNext.ParamType = cptString) then
              begin
                sVal := ParamRecNext.ParamStr;
                Option.Value := sVal;
                bVal := True;
                CLParams[IndNext].Parsed := True;
              end
              else if IsOptionValueRequired(Option) then LogError(sOpt, 'Option requires value', i);
            end
            // brak następnego parametru, a opcja wymaga wartości - error
            else if IsOptionValueRequired(Option) then LogError(sOpt, 'Option requires value', i);

        if bVal then
          if not IsOptionAcceptsValue(Option) then LogError(sOpt, 'Option does not accept value', i)
          else Option.Value := sVal;


        Option.Parsed := True;
        FOptions[xOptInd] := Option;

        bParamParsed := True;
      end
      else
      begin
        // brak takiej opcji (xOptInd < 0)
        LogError(sOpt, 'Unregistered option', i);
        bParamParsed := True;
      end;

      if bVal then ParamRec.OptionValue := sVal;


    end // if IsOption

    else

    begin
      if FAcceptAllNonOptions then
      begin
        bParamParsed := True;
        LogUnknownParam(CLParams[i].ParamStr, i);
      end
      else bParamParsed := False;
    end;



    ParamRec.Parsed := bParamParsed;
    CLParams[i] := ParamRec;

  end; // for i


  // sprawdzanie niesparsowanych parametrów
  for i := 0 to Length(CLParams) - 1 do
    if not CLParams[i].Parsed then
    begin
      LogError(CLParams[i].ParamStr, 'Unknown parameter', i);
      LogUnknownParam(CLParams[i].ParamStr, i);
    end;
end;
{$hints on}

{$endregion PARSE}


{$region ' ---------------------------- COMMANDS ----------------------- '}
function TJPCmdLineParser.GetCommand(Index: integer): TClpCommand;
begin
  Result := FCommands[Index];
end;

function TJPCmdLineParser.GetCommandCount: integer;
begin
  Result := Length(FCommands);
end;

function TJPCmdLineParser.CommandExists(CommandName: string): Boolean;
begin
  Result := GetCommandIndex(CommandName) >= 0;
end;

function TJPCmdLineParser.GetCommandIndex(CommandName: string): integer;
var
  i: integer;
  ShortName, LongName: string;
begin
  Result := -1;
  if FIgnoreCase then CommandName := UpperCase(CommandName);
  for i := 0 to Length(FCommands) - 1 do
  begin
    ShortName := FCommands[i].ShortName;
    LongName := FCommands[i].LongName;
    if FIgnoreCase then
    begin
      ShortName := UpperCase(ShortName);
      LongName := UpperCase(LongName);
    end;
    if (CommandName = ShortName) or (CommandName = LongName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TJPCmdLineParser.RegisterCommand(ShortName: string; LongName: string = ''; Position: integer = 1; Hidden: Boolean = False; Description: string = '');
var
  xInd: integer;
begin
  FCommandPosition := Position;

  xInd := GetCommandIndex(ShortName);
  if xInd < 0 then xInd := GetCommandIndex(LongName);
  if xInd < 0 then
  begin
    SetLength(FCommands, Length(FCommands) + 1);
    xInd := Length(FCommands) - 1;
  end;

  FCommands[xInd].ShortName := ShortName;
  FCommands[xInd].LongName := LongName;
  FCommands[xInd].UsageInfo := Description;
  FCommands[xInd].Hidden := Hidden;
end;

procedure TJPCmdLineParser.AddCommand(ShortName: string; LongName: string = ''; Hidden: Boolean = False; Description: string = '');
begin
  RegisterCommand(ShortName, LongName, FCommandPosition, Hidden, Description);
end;

function TJPCmdLineParser.CommandsAsString: string;
var
  i: integer;
  s: string;
begin
  s := 'Commands: ' + IntToStr(Length(FCommands)) + ENDL;
  s := s + 'Command position: ' + IntToStr(FCommandPosition) + ENDL;
  for i := 0 to Length(FCommands) - 1 do
  begin
    s := s + 'Command ' + IntToStr(i) + ENDL;
    s := s + CommandToStr(FCommands[i]) + ENDL;
  end;
  Result := s;
end;
{$endregion COMMANDS}


{$region ' ------------------------------- OPTIONS --------------------------------- '}

procedure TJPCmdLineParser.RegisterOption(ShortName: string; LongName: string = ''; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False;
  Hidden: Boolean = False; UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');
var
  xInd: integer;
  OptionType: TClpOptionType;
  bHasShortName, bHasLongName: Boolean;
begin
  ShortName := Trim(ShortName);
  LongName := Trim(LongName);
  bHasShortName := ShortName <> '';
  bHasLongName := LongName <> '';
  if (not bHasShortName) and (not bHasLongName) then Exit; // the name (short or/and long) is required

  xInd := -1;
  if not FAllowDuplicates then
  begin
    if ShortName <> '' then xInd := GetShortOptionIndex(ShortName);
    if (LongName <> '') and (xInd < 0) then xInd := GetLongOptionIndex(LongName);
  end;

  if xInd < 0 then
  begin
    SetLength(FOptions, Length(FOptions) + 1);
    xInd := Length(FOptions) - 1;
    ClearOptionRec(FOptions[xInd]);
  end;

  FOptions[xInd].LongName := LongName;
  FOptions[xInd].ShortName := ShortName;
  FOptions[xInd].ValueType := ValueType;
  FOptions[xInd].IsOptionNeeded := IsOptionNeeded;
  FOptions[xInd].Exists := False;
  FOptions[xInd].Parsed := False;
  FOptions[xInd].Hidden := Hidden;
  FOptions[xInd].UsageInfo := UsageInfo;
  FOptions[xInd].UsageArgName := UsageArgName;
  FOptions[xInd].UsageCategory := UsageCategory;

  if bHasShortName and bHasLongName then OptionType := cotBoth
  else if bHasShortName then OptionType := cotShort
  else OptionType := cotLong;
  FOptions[xInd].OptionType := OptionType;


end;

procedure TJPCmdLineParser.RegisterShortOption(const OptionName: string; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False; Hidden: Boolean = False;
  UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');
var
  xInd: integer;
  OptionType: TClpOptionType;
  bNewRec: Boolean;
begin
  xInd := -1;
  bNewRec := False;
  if not FAllowDuplicates then xInd := GetOptionIndex(OptionName);

  if xInd < 0 then
  begin
    bNewRec := True;
    SetLength(FOptions, Length(FOptions) + 1);
    xInd := Length(FOptions) - 1;
    ClearOptionRec(FOptions[xInd]);
    FOptions[xInd].OptionType := cotShort;
  end;

  FOptions[xInd].ShortName := OptionName;
  FOptions[xInd].ValueType := ValueType;
  FOptions[xInd].IsOptionNeeded := IsOptionNeeded;
  FOptions[xInd].Exists := False;
  FOptions[xInd].Parsed := False;
  FOptions[xInd].Hidden := Hidden;
  FOptions[xInd].UsageInfo := UsageInfo;
  FOptions[xInd].UsageArgName := UsageArgName;
  FOptions[xInd].UsageCategory := UsageCategory;

  if not bNewRec then
  begin
    OptionType := FOptions[xInd].OptionType;
    if OptionType <> cotBoth then
    begin
      if OptionType = cotLong then OptionType := cotBoth else OptionType := cotShort;
      FOptions[xInd].OptionType := OptionType;
    end;
  end;
end;

procedure TJPCmdLineParser.RegisterLongOption(const OptionName: string; ValueType: TClpValueType = cvtNone; IsOptionNeeded: Boolean = False; Hidden: Boolean = False;
  UsageInfo: string = ''; UsageArgName: string = ''; UsageCategory: string = '');
var
  xInd: integer;
  OptionType: TClpOptionType;
  bNewRec: Boolean;
begin
  xInd := -1;
  bNewRec := False;
  if not FAllowDuplicates then xInd := GetOptionIndex(OptionName);

  if xInd < 0 then
  begin
    bNewRec := True;
    SetLength(FOptions, Length(FOptions) + 1);
    xInd := Length(FOptions) - 1;
    ClearOptionRec(FOptions[xInd]);
    FOptions[xInd].OptionType := cotLong;
  end;

  FOptions[xInd].LongName := OptionName;
  FOptions[xInd].ValueType := ValueType;
  FOptions[xInd].IsOptionNeeded := IsOptionNeeded;
  FOptions[xInd].Exists := False;
  FOptions[xInd].Parsed := False;
  FOptions[xInd].Hidden := Hidden;
  FOptions[xInd].UsageInfo := UsageInfo;
  FOptions[xInd].UsageArgName := UsageArgName;
  FOptions[xInd].UsageCategory := UsageCategory;

  if not bNewRec then
  begin
    OptionType := FOptions[xInd].OptionType;
    if OptionType <> cotBoth then
    begin
      if OptionType = cotShort then OptionType := cotBoth else OptionType := cotLong;
      FOptions[xInd].OptionType := OptionType;
    end;
  end;
end;

function TJPCmdLineParser.GetOptionValue(const OptionName: string; NoValueStr: string = ''): string;
var
  xInd: integer;
begin
  xInd := GetOptionIndex(OptionName);
  if xInd >= 0 then Result := StripQuotes(FOptions[xInd].Value) else Result := NoValueStr;
end;

function TJPCmdLineParser.TryGetOptionValueAsBool(
  const OptionName: string; out ResultValue: Boolean; TrueStr: string = '1|y|yes'; FalseStr: string = '0|n|no';
  IgnoreBoolStrCase: Boolean = True): Boolean;
var
  Arr: array of string;
  sVal: string;
  i: integer;
begin
  Result := False;
  sVal := GetOptionValue(OptionName);
  if sVal = '' then Exit;
  if IgnoreBoolStrCase then
  begin
    sVal := UpperCase(sVal);
    TrueStr := UpperCase(TrueStr);
    FalseStr := UpperCase(FalseStr);
  end;

  SetLength(Arr, 0);
  SplitStrToArray(TrueStr, Arr, '|');
  for i := 0 to High(Arr) do
  begin
    if Arr[i] = sVal then
    begin
      Result := True;
      ResultValue := True;
      Exit;
    end;
  end;

  SplitStrToArray(FalseStr, Arr, '|');
  for i := 0 to High(Arr) do
  begin
    if Arr[i] = sVal then
    begin
      Result := True;
      ResultValue := False;
      Exit;
    end;
  end;
end;

function TJPCmdLineParser.GetShortOptionValue(OptionName: string; NoValueStr: string = ''): string;
var
  xInd: integer;
begin
  xInd := GetShortOptionIndex(OptionName);
  if xInd >= 0 then Result := StripQuotes(FOptions[xInd].Value) else Result := NoValueStr;
end;

function TJPCmdLineParser.GetLongOptionValue(OptionName: string; NoValueStr: string = ''): string;
var
  xInd: integer;
begin
  xInd := GetLongOptionIndex(OptionName);
  if xInd >= 0 then Result := StripQuotes(FOptions[xInd].Value) else Result := NoValueStr;
end;

function TJPCmdLineParser.GetOptionIndex(const OptionName: string): integer;
var
  xInd: integer;
begin
  xInd := GetShortOptionIndex(OptionName);
  if xInd < 0 then xInd := GetLongOptionIndex(OptionName);
  Result := xInd;
end;

function TJPCmdLineParser.GetShortOptionIndex(OptionName: string): integer;
var
  i: integer;
  sName: string;
begin
  Result := -1;
  if OptionName = '' then Exit;
  if FIgnoreCase then OptionName := UpperCase(OptionName);
  for i := 0 to Length(FOptions) - 1 do
  begin
    sName := FOptions[i].ShortName;
    if FIgnoreCase then sName := UpperCase(sName);
    if sName = OptionName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJPCmdLineParser.GetLongOptionIndex(OptionName: string): integer;
var
  i: integer;
  sName: string;
begin
  Result := -1;
  if OptionName = '' then Exit;
  if FIgnoreCase then OptionName := UpperCase(OptionName);
  for i := 0 to Length(FOptions) - 1 do
  begin
    sName := FOptions[i].LongName;
    if FIgnoreCase then sName := UpperCase(sName);
    if sName = OptionName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJPCmdLineParser.OptionsAsString: string;
var
  i: integer;
  s: string;
begin
  s := 'Registered options (' + IntToStr(Length(FOptions)) + '):' + ENDL;
  for i := 0 to Length(FOptions) - 1 do
    s := s + 'Option ' + IntToStr(i) + ENDL + OptionRecToStr(FOptions[i], '  ') + ENDL;
  Result := TrimRight(s);
end;


function TJPCmdLineParser.IsOptionExists(OptionName: string): Boolean;
begin
  Result := IsShortOptionExists(OptionName) or IsLongOptionExists(OptionName);
end;

function TJPCmdLineParser.IsShortOptionExists(OptionName: string): Boolean;
var
  xInd: integer;
begin
  Result := False;
  xInd := GetShortOptionIndex(OptionName);
  if xInd >= 0 then Result := FOptions[xInd].Exists;
end;

function TJPCmdLineParser.IsLongOptionExists(OptionName: string): Boolean;
var
  xInd: integer;
begin
  Result := False;
  xInd := GetLongOptionIndex(OptionName);
  if xInd >= 0 then Result := FOptions[xInd].Exists;
end;

function TJPCmdLineParser.GetOption(Index: integer): TClpOption;
begin
  Result := FOptions[Index];
end;

function TJPCmdLineParser.LongOptionsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FOptions) - 1 do
    if FOptions[i].LongName <> '' then Inc(Result);
end;

function TJPCmdLineParser.ShortOptionsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FOptions) - 1 do
    if FOptions[i].ShortName <> '' then Inc(Result);
end;

function TJPCmdLineParser.GetOptionCount: integer;
begin
  Result := Length(FOptions);
end;


function TJPCmdLineParser.GetOptionExtraInfoIndex(const OptionName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(FOptionsExtraInfo) do
  begin
    if FOptionsExtraInfo[i].OptionName = OptionName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TJPCmdLineParser.SetOptionExtraInfo(const OptionName, Info: string; Padding: integer = 4);
var
  x: integer;
begin
  x := GetOptionExtraInfoIndex(OptionName);

  if x < 0 then
  begin
    SetLength(FOptionsExtraInfo, Length(FOptionsExtraInfo) + 1);
    x := High(FOptionsExtraInfo);
  end;

  FOptionsExtraInfo[x].OptionName := OptionName;
  FOptionsExtraInfo[x].LeftPadding := Padding;
  FOptionsExtraInfo[x].InfoStr := Info;
end;

function TJPCmdLineParser.TryGetOptionExtraInfo(const OptionName: string; out oei: TClpOptionExtraInfo): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetOptionExtraInfoIndex(OptionName);
  if x >= 0 then
  begin
    oei.InfoStr := FOptionsExtraInfo[x].InfoStr;
    oei.LeftPadding := FOptionsExtraInfo[x].LeftPadding;
    oei.OptionName := FOptionsExtraInfo[x].OptionName;
    Result := True;
  end;
end;

{$endregion OPTIONS}


{$region ' ----------------------- STICKED Params ---------------------------- '}
function TJPCmdLineParser.GetStickedParam(Index: integer): TClpStickedParam;
begin
  Result := FStickedParams[Index];
end;

function TJPCmdLineParser.GetStickedParamCount: integer;
begin
  Result := Length(FStickedParams);
end;

function TJPCmdLineParser.GetStickedParamIndex(const ParamPosition: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(FStickedParams) - 1 do
    if FStickedParams[i].Position = ParamPosition then
    begin
      Result := i;
      Break;
    end;
end;

function TJPCmdLineParser.GetStickedParamValue(const ParamPosition: integer): string;
var
  xInd: integer;
begin
  Result := '';
  xInd := GetStickedParamIndex(ParamPosition);
  if xInd >= 0 then Result := FStickedParams[xInd].ParamValue;
end;

function TJPCmdLineParser.StickedParamExists(const ParamPosition: integer): Boolean;
begin
  Result := GetStickedParamIndex(ParamPosition) >= 0;
end;

procedure TJPCmdLineParser.RegisterStickedParam(const ParamPosition: integer; UsageName: string; UsageInfo: string);
var
  xInd: integer;
begin
  xInd := GetStickedParamIndex(ParamPosition);
  if xInd < 0 then
  begin
    SetLength(FStickedParams, Length(FStickedParams) + 1);
    xInd := Length(FStickedParams) - 1;
  end;
  FStickedParams[xInd].Position := ParamPosition;
  FStickedParams[xInd].UsageName := UsageName;
  FStickedParams[xInd].UsageInfo := UsageInfo;
end;

function TJPCmdLineParser.StickedParamsAsString: string;
var
  SP: TClpStickedParam;
  i: integer;
  s: string;
begin
  s := 'Sticked params: ' + IntToStr(Length(FStickedParams)) + ENDL;
  for i := 0 to Length(FStickedParams) - 1 do
  begin
    SP := FStickedParams[i];
    s := s + 'Sticked param ' + IntToStr(i) + ENDL;
    s := s + '  Position: ' + IntToStr(SP.Position) + ENDL;
    s := s + '  ParamValue: ' + SP.ParamValue + ENDL;
    s := s + '  UsageName: ' + SP.UsageName + ENDL;
    s := s + '  UsageInfo: ' + SP.UsageInfo + ENDL;
  end;
  Result := s;
end;

{$endregion Sticked Params}


{$region ' ----------------------- SKIPPED Params ---------------------------- '}
procedure TJPCmdLineParser.FillSkippedParamsArray(const StartIndex: integer);
var
  i: integer;
begin
  if StartIndex > Length(CLParams) - 1 then Exit;
  for i := StartIndex to Length(CLParams) - 1 do
  begin
    SetLength(FSkippedParams, Length(FSkippedParams) + 1);
    FSkippedParams[Length(FSkippedParams) - 1] := CLParams[i].ParamStr;
    CLParams[i].Parsed := True;
  end;
end;

function TJPCmdLineParser.GetSkippedParam(Index: integer): string;
begin
  Result := FSkippedParams[Index];
end;

function TJPCmdLineParser.GetSkippedParamsCount: integer;
begin
  Result := Length(FSkippedParams);
end;
{$endregion Skipped Params}


{$region ' ------------------ UNKNOWN Params -------------------------- '}
function TJPCmdLineParser.GetUnknownParam(Index: integer): TClpUnknownParam;
begin
  Result := FUnknownParams[Index];
end;

function TJPCmdLineParser.GetUnknownParamCount: integer;
begin
  Result := Length(FUnknownParams);
end;

procedure TJPCmdLineParser.LogUnknownParam(const ParamStr: string; const ParamIndex: integer);
var
  xInd: integer;
begin
  SetLength(FUnknownParams, Length(FUnknownParams) + 1);
  xInd := Length(FUnknownParams) - 1;
  FUnknownParams[xInd].ParamStr := ParamStr;
  FUnknownParams[xInd].Index := ParamIndex;
end;

{$endregion UNKNOWN Params}


{$region ' ------------------------------- ERRORS ------------------------------ '}
procedure TJPCmdLineParser.LogError(const Param, ErrorMsg: string; const Index: integer);
begin
  SetLength(FErrors, Length(FErrors) + 1);
  FErrors[Length(FErrors) - 1].Param := Param;
  FErrors[Length(FErrors) - 1].ErrorMsg := ErrorMsg;
  FErrors[Length(FErrors) - 1].Index := Index;
end;

function TJPCmdLineParser.GetError(Index: integer): TClpError;
begin
  Result := FErrors[Index];
end;

function TJPCmdLineParser.GetErrorCount: integer;
begin
  Result := Length(FErrors);
end;

function TJPCmdLineParser.GetErrorsStr: string;
var
  i: integer;
  s: string;
  ER: TClpError;
begin
  s := '';
  for i := 0 to Length(FErrors) - 1 do
  begin
    ER := FErrors[i];
    s := s + ER.Param + ' - ' + ER.ErrorMsg + '  (Position: ' + IntToStr(ER.Index + 1) + ')' + ENDL;
  end;
  Result := TrimRight(s);
end;
{$endregion ERRORS}


{$region ' ------------------ StrToParamType -------------------- '}
function TJPCmdLineParser.StrToParamType(Param: string): TClpParamType;
var
  ParamLen: integer;
begin
  ParamLen := Length(Param);

  if Param = FStopParsingSwitch then Result := cptStopMarker // default '--'
  else if Copy(Param, 1, 1) = '"' then Result := cptString
  else if Param = LongOptPrefix then Result := cptInvalid // --
  else if (ParamLen = 1) and IsShortOptPrefix(Param[1]) then Result := cptInvalid // '-' or '/'
  else if (ParamLen = 2) and IsShortOptPrefix(Param[1]) and (Param[2] = EqChar) then Result := cptInvalid // -= or /=
  //else if Param = ShortOptPrefix + EqChar then Result := clptInvalid // -=
  else if Param = LongOptPrefix + EqChar then Result := cptInvalid // --=

  else if Copy(Param, 1, Length(LongOptPrefix)) = LongOptPrefix then
  begin
    Delete(Param, 1, Length(LongOptPrefix));
    if Pos(EqChar, Param) > 0 then Result := cptMixedLong // --param=value
    else Result := cptLong; // --param
  end

  else if IsShortOptPrefix(Param[1]) then
  //else if Param[1] = ShortOptPrefix then
  begin
    Delete(Param, 1, 1);
    if Pos(EqChar, Param) > 0 then Result := cptMixedShort // -param=value
    else Result := cptShort; // -param
  end

  else Result := cptNone;
end;

{$endregion StrToParamType}

{$region ' --------------- FillRunTimeParamsArray ---------------- '}
procedure TJPCmdLineParser.FillRunTimeParamsArray;
var
  i: integer;
  s: string;
  {$IFDEF MSWINDOWS}
  CmdLine: PWideChar;
  A: TArgv;
  {$ENDIF}
begin
  SetLength(FRunTimeParams, 0);

  {$IFDEF MSWINDOWS}
  if FCommandLineParsingMode = cpmCustom then
  begin
    SetLength(A, 0);
    CmdLine := GetCommandLineW;
    SplitCmdLine(CmdLine, A);
    if Length(A) > 1 then
      for i := 1 to Length(A) - 1 do
      begin
        SetLength(FRunTimeParams, Length(FRunTimeParams) + 1);
        FRunTimeParams[Length(FRunTimeParams) - 1] := A[i];
      end;

  end;
  {$ENDIF}

  if FCommandLineParsingMode = cpmDelphi then
  begin

    for i := 1 to ParamCount do
    begin
      SetLength(FRunTimeParams, Length(FRunTimeParams) + 1);
      s := ParamStr(i);
      //if Pos(' ', s) > 0 then s := '"' + s + '"';
      FRunTimeParams[Length(FRunTimeParams) - 1] := s;
    end;

  end;

  FRunTimeParamCount := Length(FRunTimeParams);
end;

procedure TJPCmdLineParser.SetAcceptAllNonOptions(AValue: Boolean);
begin
  if FAcceptAllNonOptions = AValue then Exit;
  FAcceptAllNonOptions := AValue;
end;


{$endregion FillRunTimeParamsArray}

{$region ' --------- GetRunTimeParams ------------- '}
function TJPCmdLineParser.GetRunTimeParams: string;
var
  i: integer;
  s: string;
  {$IFDEF MSWINDOWS}CmdLine: PWideChar;{$ENDIF}
begin
  Result := '';

  {$IFDEF MSWINDOWS}
  if FCommandLineParsingMode = cpmCustom then
  begin
    CmdLine := GetCommandLineW;
    Result := string(CmdLine);
  end

  else
  {$ENDIF}

    for i := 0 to ParamCount do
    begin
      s := ParamStr(i);
      if Pos(' ', s) > 0 then s := '"' + s + '"';
      Result := Result + ' ' + s;
    end;

  Result := TrimLeft(Result);
end;

{$endregion GetRunTimeParams}


function TJPCmdLineParser.GetParsedParam(Index: integer): TClpParam;
begin
  Result := CLParams[Index];
end;

function TJPCmdLineParser.GetParsedParamCount: integer;
begin
  Result := Length(CLParams);
end;

function TJPCmdLineParser.GetRunTimeParam(Index: integer): string;
begin
  Result := FRunTimeParams[Index];
end;

function TJPCmdLineParser.ParamsAsString: string;
var
  i: integer;
  s: string;
begin
  s := 'CLParams array' + ENDL;
  for i := 0 to Length(CLParams) - 1 do
  begin
    s := s + 'Param ' + IntToStr(i) + ENDL;
    s := s + CLParamToStr(CLParams[i], '  ') + ENDL;
  end;
  Result := s;
end;

function TJPCmdLineParser.IsShortOptPrefix(C: Char): Boolean;
begin
  Result := {AnsiChar}(C) in ShortOptPrefixes;
  //Result := CharInSet(C, ShortOptPrefixes);
end;

procedure TJPCmdLineParser.SetAllowDuplicates(const Value: Boolean);
begin
  if FAllowDuplicates = Value then Exit;
  FAllowDuplicates := Value;
end;

procedure TJPCmdLineParser.SetCommandLineParsingMode(const Value: TClpParsingMode);
begin
  if FCommandLineParsingMode = Value then Exit;
  FCommandLineParsingMode := Value;
  FRunTimeParamsStr := GetRunTimeParams;
  FillRunTimeParamsArray;
end;

procedure TJPCmdLineParser.SetIgnoreCase(const Value: Boolean);
begin
  if FIgnoreCase = Value then Exit;
  FIgnoreCase := Value;
end;

procedure TJPCmdLineParser.SetStopParsingSwitch(const Value: string);
begin
  if FStopParsingSwitch = Value then Exit;
  if Trim(Value) = '' then Exit; // spaces and empty strings are not allowed
  FStopParsingSwitch := Value;
end;


{$region ' ------------------------- CommandsUsageStr ------------------------------- '}
function TJPCmdLineParser.CommandsUsageStr(Prefix: string = '  '; MaxLineLen: integer = 90; CommandToInfoSep: string = '  '; MaxPadding: integer = 30): string;
var
  s, sInfo, sShortName, sLongName, ShortLongSep: string;
  i, xPad, xCommandMaxLen, xShortMaxLen, xLongMaxLen, xLongStart: integer;
  xShortCommandCount, xLongCommandCount: integer;
  Command: TClpCommand;
  bHasShortName, bHasLongName: Boolean;
begin
  Result := '';
  if Length(FCommands) = 0 then Exit;

  if MaxPadding >= MaxLineLen then
  begin
    MaxLineLen := 90;
    MaxPadding := 30;
  end;

  ShortLongSep := ', ';
  sInfo := '';
  s := '';

  xCommandMaxLen := 0;
  xShortMaxLen := 0;
  xLongMaxLen := 0;
  xShortCommandCount := 0;
  xLongCommandCount := 0;
  for i := 0 to Length(FCommands) - 1 do
  begin

    Command := FCommands[i];
    if Command.Hidden then Continue;

    sShortName := Command.ShortName;
    sLongName := Command.LongName;
    bHasShortName := sShortName <> '';
    bHasLongName := sLongName <> '';

    if Length(sShortName) > xShortMaxLen then xShortMaxLen := Length(sShortName);
    if Length(sLongName) > xLongMaxLen then xLongMaxLen := Length(sLongName);

    s := sShortName + sLongName;
    if bHasShortName and bHasLongName then s := s + ShortLongSep;
    if Length(s) > xCommandMaxLen then xCommandMaxLen := Length(s);

    if bHasShortName then Inc(xShortCommandCount);
    if bHasLongName then Inc(xLongCommandCount);

  end;


  for i := 0 to Length(FCommands) - 1 do
  begin

    Command := FCommands[i];
    if Command.Hidden then Continue;

    sShortName := Command.ShortName;
    sLongName := Command.LongName;
    bHasShortName := sShortName <> '';
    bHasLongName := sLongName <> '';

    // ---------------------------------------------
    if FUsageFormat = cufNone then
    begin
      s := Prefix;
      if bHasShortName then s := s + sShortName;
      if bHasShortName and bHasLongName then s := s + ShortLongSep;
      if bHasLongName then s := s + sLongName;
      s := s + CommandToInfoSep + Command.UsageInfo;
    end

    // -----------------------------------------------
    else if FUsageFormat = cufSimple then
    begin
      s := '';
      if bHasShortName then s := sShortName;
      if bHasShortName and bHasLongName then s := s + ShortLongSep;
      if bHasLongName then s := s + sLongName;

      s := Prefix + s;
      xPad := Length(Prefix) + xCommandMaxLen;
      if xPad > MaxPadding then xPad := MaxPadding;
      s := PadRight(s, xPad, ' ');
      s := s + CommandToInfoSep + Command.UsageInfo;
      s := SplitText(s, MaxLineLen, xPad + Length(CommandToInfoSep), False, ' ', ENDL);
    end

    // --------------------------------------------------------
    else if FUsageFormat = cufWget then
    begin
      xLongStart := Length(Prefix) + xShortMaxLen;
      if xShortCommandCount > 0 then Inc(xLongStart, Length(ShortLongSep));

      s := Prefix;
      if bHasShortName then
      begin
        s := s + sShortName;
        if bHasLongName then s := s + ShortLongSep;
        s := PadRight(s, xShortMaxLen + Length(Prefix), ' ');
      end;

      if bHasLongName then
      begin
        if bHasShortName then s := PadRight(s, xLongStart, ' ') + sLongName
        else s := StringOfChar(' ', xLongStart) + sLongName;
      end;

      xPad := Length(Prefix) + xShortMaxLen + xLongMaxLen;
      if xLongCommandCount > 0 then Inc(xPad, Length(ShortLongSep));
      if xPad > MaxPadding then xPad := MaxPadding;
      s := PadRight(s, xPad, ' ');
      s := s + CommandToInfoSep + Command.UsageInfo;
      s := SplitText(s, MaxLineLen, xPad + Length(CommandToInfoSep), False, ' ', ENDL);
    end;



    sInfo := sInfo + s + ENDL;

  end;


  sInfo := TrimRight(sInfo);
  Result := sInfo;
end;
{$endregion CommandsUsageStr}

{$region ' ------------------------------ OptionsUsageStr --------------------------------------- '}

function TJPCmdLineParser.OptionsUsageStr(Prefix: string = '  '; Category: string = ''; MaxLineLen: integer = 90; OptionsToInfoSep: string = '  '; MaxPadding: integer = 30): string;
var
  s, {sInfo,} sShortOpt, sLongOpt: string;
  ArgStr, ShortLongOptSep: string;
  i, y, xPad, xOptionsMaxLen, xShortOptMaxLen, xLongOptMaxLen, xLongOptStart, xLongOptsCount, xShortOptsCount: integer;
  Option: TClpOption;
  sp: {Ansi}Char;
  ShortPrefix: Char;
  bHasShortName, bHasLongName, bCategory, bHasExtraInfo: Boolean;
  oei: TClpOptionExtraInfo;
  ArrExtInfo: specialize TArray<string>;
begin
  Result := '';
  if Length(FOptions) = 0 then Exit;

  if MaxPadding >= MaxLineLen then
  begin
    MaxLineLen := 90;
    MaxPadding := 30;
  end;

  //sInfo := '';
  xLongOptsCount := LongOptionsCount;
  xShortOptsCount := ShortOptionsCount;

  sp := '-';
  if ShortOptPrefixes = [] then sp := '-';
  //else for sp in ShortOptPrefixes do Break; // FIXME
  //ShortPrefix := Char(sp);
  ShortPrefix := sp;

  ShortLongOptSep := ', ';
  bCategory := Category <> '';

  //get MaxLen
  xOptionsMaxLen := 0;
  xShortOptMaxLen := 0;
  xLongOptMaxLen := 0;

  for i := 0 to Length(FOptions) - 1 do
  begin

    Option := FOptions[i];
    if Option.Hidden then Continue;
    if bCategory then if Category <> Option.UsageCategory then Continue;

    bHasShortName := Option.ShortName <> '';
    bHasLongName := Option.LongName <> '';
    sShortOpt := '';
    sLongOpt := '';

    ArgStr := Option.UsageArgName;
    if ArgStr = '' then ArgStr := DefaultUsageArgName;
    s := '';

    if bHasShortName then
    begin
      sShortOpt := ShortPrefix + Option.ShortName;
      if (not bHasLongName) and (xLongOptsCount = 0) then
      begin
        if Option.ValueType = cvtRequired then sShortOpt := sShortOpt + ' ' + ArgStr
        else if Option.ValueType = cvtOptional then sShortOpt := sShortOpt + ' [' + ArgStr + ']';
      end;
      if Length(sShortOpt) > xShortOptMaxLen then xShortOptMaxLen := Length(sShortOpt);
    end;

    if bHasLongName then
    begin
      sLongOpt := LongOptPrefix + Option.LongName;
      if Option.ValueType = cvtRequired then sLongOpt := sLongOpt + EqChar + ArgStr
      else if Option.ValueType = cvtOptional then sLongOpt := sLongOpt + EqChar + '[' + ArgStr + ']';
      if Length(sLongOpt) > xLongOptMaxLen then xLongOptMaxLen := Length(sLongOpt);
    end;

    s := sShortOpt + sLongOpt;
    if bHasShortName and bHasLongName then s := s + ShortLongOptSep;
    if Length(s) > xOptionsMaxLen then xOptionsMaxLen := Length(s);

  end;



  // INFO
  for i := 0 to Length(FOptions) - 1 do
  begin
    Option := FOptions[i];

    if Option.Hidden then Continue;
    if bCategory then if Category <> Option.UsageCategory then Continue;

    bHasExtraInfo := ( TryGetOptionExtraInfo(Option.ShortName, oei) ) or ( TryGetOptionExtraInfo(Option.LongName, oei) );


    s := '';
    ArgStr := Option.UsageArgName;
    if ArgStr = '' then ArgStr := 'ARG';

    bHasShortName := Option.ShortName <> '';
    bHasLongName := Option.LongName <> '';
    sShortOpt := '';
    sLongOpt := '';

    if bHasShortName then
    begin
      sShortOpt := ShortPrefix + Option.ShortName;
      if not bHasLongName then
      begin
        if Option.ValueType = cvtRequired then sShortOpt := sShortOpt + ' ' + ArgStr
        else if Option.ValueType = cvtOptional then sShortOpt := sShortOpt + ' [' + ArgStr + ']';
      end;
    end;

    if bHasLongName then
    begin
      sLongOpt := LongOptPrefix + Option.LongName;
      if Option.ValueType = cvtRequired then sLongOpt := sLongOpt + EqChar + ArgStr
      else if Option.ValueType = cvtOptional then sLongOpt := sLongOpt + EqChar + '[' + ArgStr + ']';
    end;


    // --------------------------------------------
    if FUsageFormat = cufNone then
    begin
      s := Prefix;
      if bHasShortName then s := s + sShortOpt;
      if bHasShortName and bHasLongName then s := s + ShortLongOptSep;
      if bHasLongName then s := s + sLongOpt;
      s := s + OptionsToInfoSep + Option.UsageInfo;
    end

    // ------------------------------------------------
    else if FUsageFormat = cufSimple then
    begin
      s := '';
      if bHasShortName then s := sShortOpt;
      if bHasShortName and bHasLongName then s := s + ShortLongOptSep;
      if bHasLongName then s := s + sLongOpt;

      s := Prefix + s;
      xPad := Length(Prefix) + xOptionsMaxLen;
      if xPad > MaxPadding then xPad := MaxPadding;
      s := PadRight(s, xPad, ' ');
      s := s + OptionsToInfoSep + Option.UsageInfo;
      s := SplitText(s, MaxLineLen, xPad + Length(OptionsToInfoSep), False, ' ', ENDL);
    end

    // ------------------------------------------------------------
    else if FUsageFormat = cufWget then
    begin
      xLongOptStart := Length(Prefix) + xShortOptMaxLen;
      if xShortOptsCount > 0 then Inc(xLongOptStart, Length(ShortLongOptSep));

      s := Prefix;
      if bHasShortName then
      begin
        s := s + sShortOpt;
        if bHasLongName then s := s + ShortLongOptSep;
        s := PadRight(s, xShortOptMaxLen + Length(Prefix), ' ');
      end;

      if bHasLongName then
      begin
        if bHasShortName then s := PadRight(s, xLongOptStart, ' ') + sLongOpt
        else s := StringOfChar(' ', xLongOptStart) + sLongOpt;
      end;

      xPad := Length(Prefix) + xShortOptMaxLen + xLongOptMaxLen;
      if xLongOptsCount > 0 then Inc(xPad, Length(ShortLongOptSep));
      if xPad > MaxPadding then xPad := MaxPadding;
      s := PadRight(s, xPad, ' ');
      s := s + OptionsToInfoSep + Option.UsageInfo;
      s := SplitText(s, MaxLineLen, xPad + Length(OptionsToInfoSep), False, ' ', ENDL);
    end;


    if bHasExtraInfo then
    begin
      SetLength(ArrExtInfo, 0);
      SplitStrToArray(oei.InfoStr, ArrExtInfo, ENDL);
      for y := 0 to High(ArrExtInfo) do
        s += ENDL + StringOfChar(' ', oei.LeftPadding) + ArrExtInfo[y];
    end;

    //sInfo := sInfo + s + ENDL;
    Result := Result + s + ENDL;

  end; // for i


  //sInfo := TrimRight(sInfo);
  Result := TrimRight(Result);

  //if bCategory then Result := Category + ENDL + sInfo
  //else Result := sInfo;
  //Result := sInfo;
end;
{$endregion OptionsUsageStr}



procedure TJPCmdLineParser.SetUsageFormat(const Value: TClpUsageFormat);
begin
  if FUsageFormat <> Value then FUsageFormat := Value;
end;






{$region ' ------- helpers ------- '}



function BoolToStr(const B: Boolean; ResultIfTrue: string = 'Yes'; ResultIfFalse: string = 'No'): string;
begin
  if B then Result := ResultIfTrue else Result := ResultIfFalse;
end;

function StripQuotes(s: string; QChar: Char = '"'): string;
begin
  if Length(s) >= 2 then
    if (s[1] = QChar) and (s[Length(s)] = QChar) then s := Copy(s, 2, Length(s) - 2);
  Result := s;
end;

function StickedParamToStr(StickedParam: TClpStickedParam; PaddingStr: string = '  '): string;
begin
  Result :=
    PaddingStr + 'Position: ' + IntToStr(StickedParam.Position) + ENDL +
    PaddingStr + 'ParamValue: ' + StickedParam.ParamValue + ENDL +
    PaddingStr + 'UsageName: ' + StickedParam.UsageName + ENDL +
    PaddingStr + 'UsageInfo: ' + StickedParam.UsageInfo;
end;

function CommandToStr(Command: TClpCommand; PaddingStr: string = '  '): string;
begin
  Result :=
    PaddingStr + 'ShortName: ' + Command.ShortName + ENDL +
    PaddingStr + 'LongName: ' + Command.LongName + ENDL +
    PaddingStr + 'UsageInfo: ' + Command.UsageInfo + ENDL +
    PaddingStr + 'Hidden: ' + BoolToStr(Command.Hidden);
end;

function CLParamToStr(CLParam: TClpParam; PaddingStr: string = '  '): string;
var
  s: string;
begin
  s := PaddingStr + 'ParamStr: ' + CLParam.ParamStr + ENDL;
  s := s + PaddingStr + 'ParamType: ' + ParamTypeToStr(CLParam.ParamType) + ENDL;
  s := s + PaddingStr + 'OptionName: ' + CLParam.OptionName + ENDL;
  s := s + PaddingStr + 'OptionValue: ' + CLParam.OptionValue + ENDL;
  s := s + PaddingStr + 'Index: ' + IntToStr(CLParam.Index) + ENDL;
  s := s + PaddingStr + 'Parsed: ' + BoolToStr(CLParam.Parsed, 'Yes', 'No');
  Result := s;
end;

  {$region ' --- ClearOptionRec --- '}
procedure ClearOptionRec(var Option: TClpOption);
begin
  Option.LongName := '';
  Option.ShortName := '';
  Option.Value := '';
  Option.ValueType := cvtNone;
  Option.OptionType := cotNone;
  Option.IsOptionNeeded := False;
  Option.Exists := False;
  Option.Parsed := False;
  Option.Hidden := False;
  Option.UsageInfo := '';
  Option.UsageArgName := DefaultUsageArgName;
  Option.UsageCategory := '';
end;
  {$endregion}

  {$region ' --- OptionRecToStr --- '}
function OptionRecToStr(Option: TClpOption; PaddingStr: string = '  '): string;
var
  svt, sot: string;
begin
  case Option.ValueType of
    cvtRequired: svt := 'Required';
    cvtOptional: svt := 'Optional';
  else
    //cvtNone:
    svt := 'NO VALUE';
  end;

  case Option.OptionType of
    cotShort: sot := 'Short';
    cotLong: sot := 'Long';
    cotBoth: sot := 'Both (Short+Long)';
  else
    //cotNone:
    sot := 'NONE';
  end;

  Result :=
    PaddingStr + 'LongName: ' + Option.LongName + ENDL +
    PaddingStr + 'ShortName: ' + Option.ShortName + ENDL +
    PaddingStr + 'Value in []: [' + Option.Value + ']' + ENDL +
    PaddingStr + 'ValueType: ' + svt + ENDL +
    PaddingStr + 'OptionType: ' + sot + ENDL +
    PaddingStr + 'IsOptionNeeded: ' + BoolToStr(Option.IsOptionNeeded, 'Yes', 'No') + ENDL +
    PaddingStr + 'Exists: ' + BoolToStr(Option.Exists, 'Yes', 'No') + ENDL +
    PaddingStr + 'Parsed: ' + BoolToStr(Option.Parsed, 'Yes', 'No') + ENDL +
    PaddingStr + 'Hidden: ' + BoolToStr(Option.Hidden, 'Yes', 'No') + ENDL +
    PaddingStr + 'UsageInfo: ' + Option.UsageInfo + ENDL +
    PaddingStr + 'UsageArgName: ' + Option.UsageArgName + ENDL +
    PaddingStr + 'UsageCategory: ' + Option.UsageCategory;
end;
  {$endregion OptionRecToStr}

  {$region ' --- ParamTypeToStr --- '}
function ParamTypeToStr(const ParamType: TClpParamType): string;
begin
  case ParamType of
    cptCommand: Result := 'Command';
    cptSticked: Result := 'StickedParam';
    cptNone: Result := 'None';
    cptShort: Result := 'Short';
    cptMixedShort: Result := 'MixedShort';
    cptLong: Result := 'Long';
    cptMixedLong: Result := 'MixedLong';
    cptInvalid: Result := 'Invalid';
    cptStopMarker: Result := 'StopParsingSwitch';
    cptString: Result := 'String';
  else
    Result := '?';
  end;
end;
  {$endregion ParamTypeToStr}

  {$region ' ------- SplitCmdLine ------- '}
procedure SplitCmdLine(CmdLine: string; var argv: TArgv);
type
  TParserState = (psNormal, psString, psBreakSpace);
var
  i, xLen: integer;
  znak: Char;
  State: TParserState;
  sc: string;
  bSkipChar: Boolean;
begin
  sc := '';
  State := psNormal;
  bSkipChar := False;


  xLen := Length(CmdLine);
  CmdLine := '[' + CmdLine + ']';
  for i := 2 to xLen + 1 do
  begin

    if bSkipChar then
    begin
      bSkipChar := False;
      Continue;
    end;

    znak := CmdLine[i];

    case znak of

      '"':
        begin

          sc := sc + znak;

          if State = psString then
          begin
            if CmdLine[i + 1] = ' ' then
            begin
              State := psBreakSpace;
              bSkipChar := True;
            end;
          end

          else
           if (CmdLine[i - 1] = ' ') or (CmdLine[i - 1] = EqChar) then State := psString; // -opt="some value"

        end;


      ' ':
        begin
          if State = psString then sc := sc + ' '
          else State := psBreakSpace;
        end;

    else

      begin
        if State <> psString then State := psNormal;
        sc := sc + znak;
      end;

    end;


    if State = psBreakSpace then
    begin
      if sc <> '' then
      begin
        SetLength(argv, Length(argv) + 1);
        argv[Length(argv) - 1] := sc;
      end;
      sc := '';
      State := psNormal;
    end;

  end; // for


  if sc <> '' then
  begin
    SetLength(argv, Length(argv) + 1);
    argv[Length(argv) - 1] := sc;
  end;


end;
  {$endregion SplitCmdLine}

  {$region ' ------- SplitText -------- '}
function SplitText(Text: string; MaxLen, Padding: integer; PadFirstLine: Boolean = False; PaddingChar: Char = ' '; ENDL: string = #13#10): string;
const
  Delims: TSysCharset = [
    ' ',
    '(', ')', '[', ']', '{', '}', '<', '>',
    {'`',} '~', '!', '@', '#', '$', '%', '^', '*', '-', {'_',} '+', '=',
    {'''',} '"', '\', '|', ';', ':', ',', '.'
  ];
var
  i, xDelimPos: integer;
  s, sr, PaddingStr: string;
  A: array of string;

  procedure Add(Line: string);
  begin
    SetLength(A, Length(A) + 1);
    A[Length(A) - 1] := Line;
  end;

  function DelimPos(InStr: string; SearchPosStart: integer): integer;
  var
    i: integer;
  begin
    Result := -1;
    if (InStr = '') or (SearchPosStart < 1) or (SearchPosStart > Length(InStr)) then Exit;
    for i := SearchPosStart downto 1 do
    begin
      if CharInSet(InStr[i], Delims) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

begin
  if (Length(Text) <= Padding) or (MaxLen = 0) then
  begin
    Result := Text;
    Exit;
  end;

  SetLength(A, 0);
              //if Padding > MaxLen then exit;
  if not PadFirstLine then
  begin
    xDelimPos := DelimPos(Text, MaxLen);
    if xDelimPos < 0 then xDelimPos := MaxLen;
    s := Copy(Text, 1, xDelimPos);

    Add(s);
    Text := Copy(Text, xDelimPos + 1, Length(Text));
  end;

  sr := '';
  PaddingStr := StringOfChar(PaddingChar, Padding);


  while Text <> '' do
  begin

    xDelimPos := DelimPos(Text, MaxLen - Padding);
    if xDelimPos < 0 then xDelimPos := MaxLen;

    s := PaddingStr + Trim( Copy(Text, 1, xDelimPos) );
    Add(s);
    Text := Copy(Text, xDelimPos + 1, Length(Text));
    //Writeln(Text);

  end;


  for i := 0 to Length(A) - 1 do sr := sr + A[i] + ENDL;
  sr := TrimRight(sr);

  Result := sr;
end;
  {$endregion SplitText}


{$endregion helpers}



end.
