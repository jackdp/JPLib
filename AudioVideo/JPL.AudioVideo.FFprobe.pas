unit JPL.AudioVideo.FFprobe;


{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}


interface

{$IFDEF MSWINDOWS}

uses
  Windows, SysUtils, Classes,
  IniFiles,
  JPL.Win.ConsoleAppRun,
  JPL.Strings, JPL.Conversion,
  Generics.Collections
  ;

type

  TFFData = TDictionary<string, string>;
  TFFStreamType = (ffstUnknown, ffstAudio, ffstVideo, ffstSubtitle);


  {$region ' --- TFFChapter --- '}
  TFFChapterInfo = TList<TFFData>;
  TFFChapterTags = TList<TFFData>;

  TFFChapter = class(TObject)
  private
    FChapterInfo: TFFChapterInfo;
    FChapterTags: TFFChapterTags;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    function AsText(Sep: string = '  '): string;

    property ChapterInfo: TFFChapterInfo read FChapterInfo;
    property ChapterTags: TFFChapterTags read FChapterTags;
  end;
  {$endregion TFFChapter}

  {$region ' --- TFFChapters --- '}
  TFFChapters = class(TObject)
  private
    FItems: TList<TFFChapter>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    procedure ParseIniString(s: string);
    function AsText(Sep: string = '  '): string;

    property Items: TList<TFFChapter> read FItems;
  end;
  {$endregion TFFChapters}


  {$region ' --- TFFFormat --- '}
  TFFFormatInfo = TList<TFFData>;
  TFFFormatTags = TList<TFFData>;

  TFFFormat = class(TObject)
  private
    FFormatInfo: TFFFormatInfo;
    FFormatTags: TFFFormatTags;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    procedure ParseIniString(s: string);
    function AsText(Sep: string = '  '): string;

    property FormatInfo: TFFFormatInfo read FFormatInfo;
    property FormatTags: TFFFormatTags read FFormatTags;
  end;
  {$endregion TFFFormat}


  {$region ' --- TFFStream --- '}
  TFFStreamInfo = TList<TFFData>;
  TFFStreamDisposition = TList<TFFData>;
  TFFStreamTags = TList<TFFData>;

  TFFStream = class(TObject)
  private
    FStreamInfo: TFFStreamInfo;
    FStreamDisposition: TFFStreamDisposition;
    FStreamTags: TFFStreamTags;
    FStreamType: TFFStreamType;
    procedure SetStreamType(const Value: TFFStreamType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    function AsText(Sep: string = '  '): string;

    property StreamInfo: TFFStreamInfo read FStreamInfo;
    property StreamDisposition: TFFStreamDisposition read FStreamDisposition;
    property StreamTags: TFFStreamTags read FStreamTags;
    property StreamType: TFFStreamType read FStreamType write SetStreamType;
  end;
  {$endregion TFFStream}

  {$region ' --- TFFStreams --- '}
  TFFStreams = class(TObject)
  private
    FItems: TList<TFFStream>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    procedure ParseIniString(s: string);
    function AsText(Sep: string = '  '): string;

    property Items: TList<TFFStream> read FItems;
  end;
  {$endregion TFFStreams}


  {$region ' --- TFFProbe --- '}
  TFFProbe = class(TObject)
  private
    FReadFormatInfo: Boolean;
    FReadStreamsInfo: Boolean;
    FReadChapters: Boolean;
    FStreams: TFFStreams;
    FFormat: TFFFormat;
    FChapters: TFFChapters;
    FFileName: string;
    FForceTerminate_ReadFormat: Boolean;
    FForceTerminate_ReadStreams: Boolean;
    FForceTerminate_ReadChapters: Boolean;
    procedure SetReadFormatInfo(const Value: Boolean);
    procedure SetReadStreamsInfo(const Value: Boolean);
    procedure SetReadChapters(const Value: Boolean);
    procedure SetForceTerminate_ReadFormat(const Value: Boolean);
    procedure SetForceTerminate_ReadStreams(const Value: Boolean);
    procedure SetForceTerminate_ReadChapters(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
    function ReadFile(const FFProbeExe, FileName: string): Boolean;
    function AsText(Sep: string = '  '): string;

    property ReadFormatInfo: Boolean read FReadFormatInfo write SetReadFormatInfo;
    property ReadStreamsInfo: Boolean read FReadStreamsInfo write SetReadStreamsInfo;
    property ReadChapters: Boolean read FReadChapters write SetReadChapters;
    property Streams: TFFStreams read FStreams;
    property Format: TFFFormat read FFormat;
    property Chapters: TFFChapters read FChapters;
    property ForceTerminate_ReadFormat: Boolean read FForceTerminate_ReadFormat write SetForceTerminate_ReadFormat;
    property ForceTerminate_ReadStreams: Boolean read FForceTerminate_ReadStreams write SetForceTerminate_ReadStreams;
    property ForceTerminate_ReadChapters: Boolean read FForceTerminate_ReadChapters write SetForceTerminate_ReadChapters;
  end;
  {$endregion TFFProbe}

var
  FFProbeHandle1, FFprobeHandle2, FFProbeHandle3: THandle;


procedure TerminateFFprobeProcesses;

{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

procedure TerminateFFprobeProcesses;
var
  EC: UINT;
begin
  EC := 0;
  if FFProbeHandle1 > 0 then TerminateProcess(FFProbeHandle1, EC);
  if FFProbeHandle2 > 0 then TerminateProcess(FFProbeHandle2, EC);
  if FFProbeHandle3 > 0 then TerminateProcess(FFProbeHandle3, EC);
end;



function Utf8StrToStr(const s: string): string;
begin
  Result := UTF8ToString(RawByteString(s));
end;

function utos(const s: string): string;
begin
  //Result := Utf8StrToStr(s);
  Result := s;
end;

function Unescape(s: string): string;
begin
  //s := StringReplace(s, '\t', TAB, [rfReplaceAll]);
  s := StringReplace(s, '\:', ':', [rfReplaceAll]);
  s := StringReplace(s, '\''', '''', [rfReplaceAll]);
  s := StringReplace(s, '\"', '"', [rfReplaceAll]);
  s := StringReplace(s, '\?', '?', [rfReplaceAll]);

  s := StringReplace(s, '\n', CRLF, [rfReplaceAll]); // LF
  s := StringReplace(s, '\r', CRLF, [rfReplaceAll]); // CR

  s := StringReplace(s, '\\', '\', [rfReplaceAll]);

  Result := s;
end;



{$region '             TFFFormat             '}
constructor TFFFormat.Create;
begin
  inherited;
  FFormatInfo := TFFFormatInfo.Create;
  FFormatTags := TFFFormatTags.Create;
end;

destructor TFFFormat.Destroy;
begin
  ClearAll;
  FFormatTags.Free;
  FFormatInfo.Free;
  inherited;
end;

procedure TFFFormat.ClearAll;
var
  Data: TFFData;
begin
  for Data in FFormatInfo do Data.Free;
  FFormatInfo.Clear;

  for Data in FFormatTags do Data.Free;
  FFormatTags.Clear;
end;


function MBCSString(const s: UnicodeString; CodePage: Word): RawByteString;
var
  enc: TEncoding;
  bytes: TBytes;
begin
  enc := TEncoding.GetEncoding(CodePage);
  try
    bytes := enc.GetBytes(s);
    SetLength(Result, Length(bytes));
    Move(Pointer(bytes)^, Pointer(Result)^, Length(bytes));
    SetCodePage(Result, CodePage, False);
  finally
    enc.Free;
  end;
end;

procedure TFFFormat.ParseIniString(s: string);
const
  SECTION_FORMAT = 'format';
  SECTION_FORMAT_TAGS = 'format.tags';
var
  Ini: TMemIniFile;
  slIni, slKeys: TStringList;
  i: integer;
  sKey, sVal: string;
  Data: TFFData;
begin
  ClearAll;
  slIni := TStringList.Create;
  slKeys := TStringList.Create;
  Ini := TMemIniFile.Create('');
  try

    slIni.Text := s;
    Ini.SetStrings(slIni);

    if Ini.SectionExists(SECTION_FORMAT) then
    begin
      Ini.ReadSection(SECTION_FORMAT, slKeys);
      for i := 0 to slKeys.Count - 1 do
      begin
        sKey := Trim(slKeys[i]);
        if sKey = '' then Continue;
        sVal := Ini.ReadString(SECTION_FORMAT, sKey, '');
        Data := TFFData.Create;
        Data.Add(sKey, utos(sVal));
        FFormatInfo.Add(Data);
      end;
    end;

    if Ini.SectionExists(SECTION_FORMAT_TAGS) then
    begin
      slKeys.Clear;
      Ini.ReadSection(SECTION_FORMAT_TAGS, slKeys);
      for i := 0 to slKeys.Count - 1 do
      begin
        sKey := Trim(slKeys[i]);
        if sKey = '' then Continue;
        sVal := Ini.ReadString(SECTION_FORMAT_TAGS, sKey, '');
        Data := TFFData.Create;
        Data.Add(sKey, utos(sVal));
        FFormatTags.Add(Data);
      end;
    end;

  finally
    Ini.Free;
    slKeys.Free;
    slIni.Free;
  end;
end;

function TFFFormat.AsText(Sep: string = '  '): string;
var
  i: integer;
  s, sKey, sVal: string;
  Data: TFFData;
begin
  s := s + 'FORMAT' + CRLF;

  if FFormatInfo.Count > 0 then
  begin

    s := s + Sep + 'FORMAT INFO' + CRLF;

    for i := 0 to FFormatInfo.Count - 1 do
    begin
      Data := FFormatInfo.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;

  end;

  if FFormatTags.Count > 0 then
  begin
    s := s + Sep + 'FORMAT TAGS' + CRLF;
    for i := 0 to FFormatTags.Count - 1 do
    begin
      Data := FFormatTags.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;


  Result := s;
end;

{$endregion TFFFormat}



{$region '              TFFStream             '}
constructor TFFStream.Create;
begin
  inherited Create;
  FStreamInfo := TFFStreamInfo.Create;
  FStreamDisposition := TFFStreamDisposition.Create;
  FStreamTags := TFFStreamTags.Create;
end;

destructor TFFStream.Destroy;
begin
  ClearAll;
  FStreamInfo.Free;
  FStreamDisposition.Free;
  FStreamTags.Free;
  inherited;
end;

procedure TFFStream.ClearAll;
var
  Data: TFFData;
begin
  for Data in FStreamInfo do Data.Free;
  FStreamInfo.Clear;

  for Data in FStreamDisposition do Data.Free;
  FStreamDisposition.Clear;

  for Data in FStreamTags do Data.Free;
  FStreamTags.Clear;
end;

procedure TFFStream.SetStreamType(const Value: TFFStreamType);
begin
  FStreamType := Value;
end;

function TFFStream.AsText(Sep: string): string;
var
  i: integer;
  s, sKey, sVal: string;
  Data: TFFData;
begin
  s := '';

  if FStreamInfo.Count > 0 then
  begin
    s := s + Sep + 'STREAM INFO' + CRLF;

    for i := 0 to FStreamInfo.Count - 1 do
    begin
      Data := FStreamInfo.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;

  if FStreamDisposition.Count > 0 then
  begin
    s := s + Sep + 'STREAM DISPOSITION' + CRLF;

    for i := 0 to FStreamDisposition.Count - 1 do
    begin
      Data := FStreamDisposition.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;

  if FStreamTags.Count > 0 then
  begin
    s := s + Sep + 'STREAM TAGS' + CRLF;

    for i := 0 to FStreamTags.Count - 1 do
    begin
      Data := FStreamTags.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;

  Result := s;

end;
{$endregion TFFStream}

{$region '              TFFStreams                  '}
constructor TFFStreams.Create;
begin
  inherited Create;
  FItems := TList<TFFStream>.Create;
end;

destructor TFFStreams.Destroy;
begin
  ClearAll;
  FItems.Free;
  inherited;
end;

procedure TFFStreams.ClearAll;
var
  Item: TFFStream;
begin
  for Item in FItems do Item.Free;
  FItems.Clear;
end;

procedure TFFStreams.ParseIniString(s: string);
const
  SECTION_INFO_MASK = 'streams.stream.%x';
  SECTION_DISPOSITION_MASK = SECTION_INFO_MASK + '.disposition';
  SECTION_TAGS_MASK = SECTION_INFO_MASK + '.tags';
var
  Ini: TMemIniFile;
  slIni, slKeys: TStringList;
  i, x: integer;
  Section, sKey, sVal: string;
  Data: TFFData;
  Stream: TFFStream;
begin
  Ini := TMemIniFile.Create('');
  slIni := TStringList.Create;
  slKeys := TStringList.Create;
  try

    slIni.Text := s;
    Ini.SetStrings(slIni);

    for i := 0 to 250 do
    begin

      Section := StringReplace(SECTION_INFO_MASK, '%x', itos(i), []);
      if not Ini.SectionExists(Section) then Break;

      Stream := TFFStream.Create;
      Stream.StreamType := ffstUnknown;

      Ini.ReadSection(Section, slKeys);
      for x := 0 to slKeys.Count - 1 do
      begin
        sKey := Trim(slKeys[x]);
        if sKey = '' then Continue;
        sVal := Ini.ReadString(Section, sKey, '');
        Data := TFFData.Create;
        Data.Add(sKey, utos(sVal));
        Stream.StreamInfo.Add(Data);
        if LowerCase(sKey) = 'codec_type' then
        begin
          sVal := LowerCase(sVal);
          if sVal = 'audio' then Stream.StreamType := ffstAudio
          else if sVal = 'video' then Stream.StreamType := ffstVideo
          else if sVal = 'subtitle' then Stream.StreamType := ffstSubtitle
          else Stream.StreamType := ffstUnknown;
        end;
      end;

      Section := StringReplace(SECTION_DISPOSITION_MASK, '%x', itos(i), []);
      if Ini.SectionExists(Section) then
      begin
        slKeys.Clear;
        Ini.ReadSection(Section, slKeys);
        for x := 0 to slKeys.Count - 1 do
        begin
          sKey := Trim(slKeys[x]);
          if sKey = '' then Continue;
          sVal := Ini.ReadString(Section, sKey, '');
          Data := TFFData.Create;
          Data.Add(sKey, utos(sVal));
          Stream.StreamDisposition.Add(Data);
        end;
      end;

      Section := StringReplace(SECTION_TAGS_MASK, '%x', itos(i), []);
      if Ini.SectionExists(Section) then
      begin
        slKeys.Clear;
        Ini.ReadSection(Section, slKeys);
        for x := 0 to slKeys.Count - 1 do
        begin
          sKey := Trim(slKeys[x]);
          if sKey = '' then Continue;
          sVal := Ini.ReadString(Section, sKey, '');
          Data := TFFData.Create;
          Data.Add(sKey, utos(sVal));
          Stream.StreamTags.Add(Data);
        end;
      end;

      FItems.Add(Stream);

    end;


  finally
    slIni.Free;
    slKeys.Free;
    Ini.Free;
  end;
end;

function TFFStreams.AsText(Sep: string): string;
var
  i: integer;
  s: string;
  Stream: TFFStream;
begin
  s := 'STREAMS (' + itos(FItems.Count) + ') ' + CRLF;

  if FItems.Count > 0 then
  begin

    for i := 0 to FItems.Count - 1 do
    begin
      s := s + Sep + 'STREAM ' + itos(i + 1) + CRLF;

      Stream := FItems.Items[i];
      s := s + Stream.AsText(Sep + Sep) + CRLF;
    end;

  end;

  Result := s;
end;

{$endregion TFFStreams}



{$region '             TFFChapter               '}
constructor TFFChapter.Create;
begin
  inherited;
  FChapterInfo := TFFChapterInfo.Create;
  FChapterTags := TFFChapterTags.Create;
end;

destructor TFFChapter.Destroy;
begin
  ClearAll;
  FChapterInfo.Free;
  FChapterTags.Free;
  inherited;
end;

procedure TFFChapter.ClearAll;
var
  Data: TFFData;
begin
  for Data in FChapterInfo do Data.Free;
  FChapterInfo.Clear;
  for Data in FChapterTags do Data.Free;
  FChapterTags.Clear;
end;

function TFFChapter.AsText(Sep: string): string;
var
  i: integer;
  s, sKey, sVal: string;
  Data: TFFData;
begin
  s := '';

  if FChapterInfo.Count > 0 then
  begin
    s := s + Sep + 'CHAPTER INFO' + CRLF;

    for i := 0 to FChapterInfo.Count - 1 do
    begin
      Data := FChapterInfo.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;

  if FChapterTags.Count > 0 then
  begin
    s := s + Sep + 'CHAPTER TAGS' + CRLF;

    for i := 0 to FChapterTags.Count - 1 do
    begin
      Data := FChapterTags.Items[i];
      for sKey in Data.Keys do
      begin
        sVal := Data.Items[sKey];
        s := s + Sep + Sep + sKey + '=' + sVal + CRLF;
      end;
    end;
  end;

  Result := s;

end;

{$endregion TFFChapter}

{$region '                 TFFChapters                 '}
constructor TFFChapters.Create;
begin
  inherited;
  FItems := TList<TFFChapter>.Create;
end;

destructor TFFChapters.Destroy;
begin
  ClearAll;
  FItems.Free;
  inherited;
end;

procedure TFFChapters.ClearAll;
var
  Item: TFFChapter;
begin
  for Item in FItems do Item.Free;
  FItems.Clear;
end;

procedure TFFChapters.ParseIniString(s: string);
const
  SECTION_INFO_MASK = 'chapters.chapter.%x';
  SECTION_TAGS_MASK = SECTION_INFO_MASK + '.tags';
var
  Ini: TMemIniFile;
  slIni, slKeys: TStringList;
  i, x: integer;
  Section, sKey, sVal: string;
  Data: TFFData;
  Chapter: TFFChapter;
begin
  Ini := TMemIniFile.Create('');
  slIni := TStringList.Create;
  slKeys := TStringList.Create;
  try

    slIni.Text := s;
    Ini.SetStrings(slIni);

    for i := 0 to 200 do
    begin

      Section := StringReplace(SECTION_INFO_MASK, '%x', itos(i), []);
      if not Ini.SectionExists(Section) then Break;

      Chapter := TFFChapter.Create;

      Ini.ReadSection(Section, slKeys);
      for x := 0 to slKeys.Count - 1 do
      begin
        sKey := Trim(slKeys[x]);
        if sKey = '' then Continue;
        sVal := Ini.ReadString(Section, sKey, '');
        Data := TFFData.Create;
        Data.Add(sKey, utos(sVal));
        Chapter.ChapterInfo.Add(Data);
      end;


      Section := StringReplace(SECTION_TAGS_MASK, '%x', itos(i), []);
      if Ini.SectionExists(Section) then
      begin
        slKeys.Clear;
        Ini.ReadSection(Section, slKeys);
        for x := 0 to slKeys.Count - 1 do
        begin
          sKey := Trim(slKeys[x]);
          if sKey = '' then Continue;
          sVal := Ini.ReadString(Section, sKey, '');
          Data := TFFData.Create;
          Data.Add(sKey, utos(sVal));
          Chapter.ChapterTags.Add(Data);
        end;
      end;

      FItems.Add(Chapter);

    end;


  finally
    slIni.Free;
    slKeys.Free;
    Ini.Free;
  end;
end;

function TFFChapters.AsText(Sep: string): string;
var
  i: integer;
  s: string;
  Chapter: TFFChapter;
begin
  s := 'CHAPTERS (' + itos(FItems.Count) + ') ' + CRLF;

  if FItems.Count > 0 then
  begin

    for i := 0 to FItems.Count - 1 do
    begin
      s := s + Sep + 'CHAPTER ' + itos(i + 1) + CRLF;

      Chapter := FItems.Items[i];
      s := s + Chapter.AsText(Sep + Sep) + CRLF;
    end;

  end;

  Result := s;
end;

{$endregion TFFChapters}





{$region '               TFFProbe                  '}
constructor TFFProbe.Create;
begin
  inherited Create;
  FReadFormatInfo := True;
  FReadStreamsInfo := True;
  FReadChapters := True;
  FFormat := TFFFormat.Create;
  FStreams := TFFStreams.Create;
  FChapters := TFFChapters.Create;
  FFileName := '';
  FForceTerminate_ReadFormat := False;
  FForceTerminate_ReadStreams := False;
  FForceTerminate_ReadChapters := False;
  FFProbeHandle1 := 0;
  FFprobeHandle2 := 0;
  FFProbeHandle3 := 0;
end;

destructor TFFProbe.Destroy;
begin
  FFormat.ClearAll;
  FFormat.Free;
  FStreams.Free;
  FChapters.Free;
  inherited;
end;

function TFFProbe.AsText(Sep: string): string;
begin
  Result := FFormat.AsText(Sep) + CRLF + FStreams.AsText(Sep);
  if FChapters.Items.Count > 0 then Result := Result + CRLF + FChapters.AsText(Sep);
end;

procedure TFFProbe.ClearAll;
begin
  FFormat.ClearAll;
  FChapters.ClearAll;
  FStreams.ClearAll;
  FFileName := '';
end;

function TFFProbe.ReadFile(const FFProbeExe, FileName: string): Boolean;
var
  car: TConsoleAppRunner;
  sl: TStringList;
  Params, ExeDir: string;
begin
  TerminateFFprobeProcesses;
  Result := False;
  ClearAll;
  FFileName := FileName;
  if not FileExists(FFileName) then Exit;
  if not FileExists(FFProbeExe) then Exit;

  car := TConsoleAppRunner.Create;
  sl := TStringList.Create;
  try

    car.IncludeStdErrorInOutput := False;
    ExeDir := ExtractFileDir(FFProbeExe);

    if FReadFormatInfo then
    begin
      Params := '-hide_banner -show_format -print_format ini -i "' + FileName + '"';

      //ExecConsoleApp(FFProbeExe, Params, sl, nil, '', SW_HIDE);

      car.Execute(FFProbeExe, Params, ExeDir, False);
      sl.Text := car.ConsoleOutput;
      FFProbeHandle1 := car.ConProcessHandle;

      if FForceTerminate_ReadFormat then
      begin
        Sleep(1000);
        if FFProbeHandle1 > 0 then TerminateProcess(FFProbeHandle1, 0);
      end;

      sl.Text := Unescape(sl.Text);
      FFormat.ParseIniString(sl.Text);
    end;


    if FReadStreamsInfo then
    begin
      Params := '-hide_banner -show_streams -print_format ini -i "' + FileName + '"';
      //ExecConsoleApp(FFProbeExe, Params, sl, nil, '');

      car.Execute(FFProbeExe, Params, ExeDir, False);
      sl.Text := car.ConsoleOutput;
      FFProbeHandle2 := car.ConProcessHandle;

      if FForceTerminate_ReadStreams then
      begin
        Sleep(1500);
        if FFprobeHandle2 > 0 then TerminateProcess(FFprobeHandle2, 0);
      end;

      sl.Text := Unescape(sl.Text);
      FStreams.ParseIniString(sl.Text);
    end;


    if FReadChapters then
    begin
      Params := '-hide_banner -show_chapters -print_format ini -i "' + FileName + '"';
      //ExecConsoleApp(FFProbeExe, Params, sl, nil, '');

      car.Execute(FFProbeExe, Params, ExeDir, False);
      sl.Text := car.ConsoleOutput;
      FFProbeHandle3 := car.ConProcessHandle;

      if FForceTerminate_ReadChapters then
      begin
        Sleep(1000);
        if FFProbeHandle3 > 0 then TerminateProcess(FFProbeHandle3, 0);
      end;

      sl.Text := Unescape(sl.Text);
      FChapters.ParseIniString(sl.Text);
    end;

  finally
    sl.Free;
    car.Free;
  end;

  Result := True;
end;


procedure TFFProbe.SetForceTerminate_ReadChapters(const Value: Boolean);
begin
  FForceTerminate_ReadChapters := Value;
end;

procedure TFFProbe.SetForceTerminate_ReadFormat(const Value: Boolean);
begin
  FForceTerminate_ReadFormat := Value;
end;


procedure TFFProbe.SetForceTerminate_ReadStreams(const Value: Boolean);
begin
  FForceTerminate_ReadStreams := Value;
end;

procedure TFFProbe.SetReadChapters(const Value: Boolean);
begin
  FReadChapters := Value;
end;

procedure TFFProbe.SetReadFormatInfo(const Value: Boolean);
begin
  FReadFormatInfo := Value;
end;

procedure TFFProbe.SetReadStreamsInfo(const Value: Boolean);
begin
  FReadStreamsInfo := Value;
end;
{$endregion TFFProbe}


{$ENDIF} // MSWINDOWS

initialization


end.
