unit JPL.ArrayIniFile;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  -----------------------------------------------

  TArrayIniFile

  A lightweight class for managing INI files, based on arrays.
  Designed for small console applications.
  No Classes, only SysUtils!

  -----------------------------------------------

  Tips & Infos:

  1. Property Ini.FileName is Read and Write.
     If you change FileName, TArrayIniFile will load the specified file and add all sections from this file to the existing ones. So, you
     can load data from many files.
     To remove all sections, call Ini.Clear.

  2. To parse custom text (not file content), call Ini.AsString := String_With_The_Valid_Ini_Content;
     If you do not want to load data from a file, pass the name of a non-existent file as a parameter in the constructor:
       Ini := TArrayIniFile.Create('');
     then
       Ini.AsString := String_With_The_Valid_Ini_Content;
     Similar to FileName, you can call Ini.AsString many times with the different values.

  -----------------------------------------------

  How to use:

  1. First, create instance:

     Ini := TArrayIniFile.Create('SomeFile.ini');

       OR

     Ini := TArrayIniFile.Create('');

     Invalid file name. You can assign a valid INI string: Ini.AsString = String_With_The_Valid_Ini_Content;

       OR

     Ini := TArrayIniFile.Create('SomeFile.ini, TEncoding.UTF8);
     Ini := TArrayIniFile.Create('', TEncoding.Unicode);

     Ecoding will be used while saving INI file (Ini.UpdateFile). When loading a file, the encoding is detected automatically.


  2. Adding sections:

     var
       Section: TIniSection;
     ...
     Section := Ini.AddSection('SectionName');
     ...
     Section := Ini.AddSection('Second_Section');


  3. Getting section:

     var Section: TIniSection;

     A)
       Section := Ini.GetSection('SectionName');
       if Assigned(Section) then .. do something ...

     B)
       Section := Ini.GetSection('SectionName', True); // Second parameter = "True": The section will be created if it does not already exist.
       .. do something ...


  4. Reading / writing values:

     var
       Section: TIniSection;
       s: string;
       x: integer;

     Section := Ini.GetSection('SectionName', True);

     s := Section.ReadString('Identifier', 'Default value');
     Writeln(s);

     x := Section.ReadInteger('Identifier2', -1);
     Writeln(x);

     Section.WriteString('Identifier', 'Value);
     Section.WriteInteger('Identifier2', 1024);


  5. Save INI file:

     var b: Boolean;

     b := Ini.UpdateFile;

     if b then Writeln('The file "', Ini.FileName, '" has been saved.')
     else Writeln('An error occurred while saving the file: "', Ini.FileName, '"!');


  6. Ini.Free

 }


{$I .\..\jp.inc}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  SysUtils, Types,
  JPL.Strings,
  JPL.TStr;


type

  TArrayIniFileException = class(Exception);

  TIniSectionItem = record
    Ident: string;
    Value: string;
    procedure Clear;
    function AsString: string;
  end;

  TIniSectionItems = array of TIniSectionItem;

  {$region '   TIniSection   '}
  TIniSection = class
  private
    FItems: TIniSectionItems;
    FItemsCount: integer;
    FSectionName: string;
    FDeltaSize: integer;
    function GetCapacity: integer;
    function GetItems(Index: integer): TIniSectionItem;
    procedure SetItems(Index: integer; Item: TIniSectionItem);
  protected
    procedure Clear;
    function AddIdent(const Ident, Value: string): integer; // Returns the index of the added item. If Ident does not exists, it will be created
    procedure SetIdentValue(const Ident, Value: string); // Calls AddIdent
    function GetIdentValue(const Ident: string; Default: string): string;
  public
    constructor Create(const ASectionName: string);
    destructor Destroy; override;

    function AsString: string;
    function GetIdentIndex(const Ident: string): integer;
    function IdentExists(const Ident: string): Boolean;


    // ------------ Read & Write routines -----------------
    procedure WriteString(const Ident, Value: string);
    function ReadString(const Ident, Default: string): string;
    procedure WriteInteger(const Ident: string; const Value: integer);
    function ReadInteger(const Ident: string; Default: integer): integer;
    procedure WriteBool(const Ident: string; const Value: Boolean);
    function ReadBool(const Ident: string; Default: Boolean): Boolean;


    property SectionName: string read FSectionName write FSectionName;
    property ItemsCount: integer read FItemsCount;
    property Items[Index: integer]: TIniSectionItem read GetItems write SetItems; default;
    property Capacity: integer read GetCapacity;
  end;
  {$endregion TIniSection}

  TIniSections = array of TIniSection;

  {$region '   TArrayIniFile   '}
  TArrayIniFile = class
  private
    FFileName: string;
    FSections: TIniSections;
    FEncoding: TEncoding;
    function GetSectionCount: integer;
    function GetAsString: string;
    function GetSections(Index: integer): TIniSection;
    procedure SetAsString(const IniFileContent: string);
    procedure SetFileName(const AFileName: string);
  protected
    function PerformAddSection(const SectionName: string): integer; // Returns the index of the added section
    procedure LoadFile;
    procedure ParseText(const Text: string);
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; AEncoding: TEncoding); overload;
    destructor Destroy; override;
    procedure Clear;

    function GetSectionIndex(const SectionName: string): integer;
    function SectionExists(const SectionName: string): Boolean;
    function AddSection(const SectionName: string): TIniSection;
    function GetSection(const SectionName: string; CreateIfNotExists: Boolean = True): TIniSection;

    function UpdateFile: Boolean;

    property FileName: string read FFileName write SetFileName;
    property SectionCount: integer read GetSectionCount;
    property AsString: string read GetAsString write SetAsString;
    property Sections[Index: integer]: TIniSection read GetSections;
  end;
  {$endregion TArrayIniFile}



implementation



{$region '                          TArrayIniFile                            '}

constructor TArrayIniFile.Create(const AFileName: string);
begin
  Create(AFileName, TEncoding.Default);
end;

constructor TArrayIniFile.Create(const AFileName: string; AEncoding: TEncoding);
begin
  inherited Create;
  FFileName := AFileName;
  SetLength(FSections, 0);
  FEncoding := AEncoding;
  if FileExists(FFileName) then LoadFile;
end;

destructor TArrayIniFile.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TArrayIniFile.Clear;
var
  i: integer;
begin
  for i := 0 to SectionCount - 1 do
  begin
    if not Assigned(FSections[i]) then Continue;
    FSections[i].Clear;
    FSections[i].Free;
  end;
  SetLength(FSections, 0);
end;

function TArrayIniFile.GetSectionCount: integer;
begin
  Result := Length(FSections);
end;

function TArrayIniFile.GetSectionIndex(const SectionName: string): integer;
var
  uSectionName: string;
  i: integer;
begin
  Result := -1;
  if SectionCount <= 0 then Exit;
  uSectionName := AnsiUpperCase(SectionName);
  for i := 0 to SectionCount - 1 do
    if AnsiUpperCase(FSections[i].SectionName) = uSectionName then
    begin
      Result := i;
      Break;
    end;
end;

function TArrayIniFile.SectionExists(const SectionName: string): Boolean;
begin
  Result := GetSectionIndex(SectionName) >= 0;
end;

function TArrayIniFile.AddSection(const SectionName: string): TIniSection;
begin
  Result := GetSection(SectionName, True);
end;

function TArrayIniFile.GetSection(const SectionName: string; CreateIfNotExists: Boolean): TIniSection;
var
  xInd: integer;
begin
  Result := nil;

  xInd := GetSectionIndex(SectionName);
  if xInd >= 0 then
  begin
    Result := FSections[xInd];
    Exit;
  end;

  if not CreateIfNotExists then Exit;

  xInd := PerformAddSection(SectionName);
  Result := FSections[xInd];
end;

function TArrayIniFile.PerformAddSection(const SectionName: string): integer;
var
  Section: TIniSection;
begin
  Section := TIniSection.Create(SectionName);
  SetLength(FSections, Length(FSections) + 1);
  FSections[Length(FSections) - 1] := Section;
  Result := Length(FSections) - 1;
end;

procedure TArrayIniFile.SetFileName(const AFileName: string);
begin
  if FFileName = AFileName then Exit;
  FFileName := AFileName;
  LoadFile;
end;

procedure TArrayIniFile.LoadFile;
var
  s: string;
begin
  s := '';
  if not FileExists(FFileName) then Exit;
  if not LoadStringFromFile(FFileName, s, TEncoding.UTF8) then Exit;
  ParseText(s);
end;

function TArrayIniFile.GetAsString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to SectionCount - 1 do
    Result := Result + FSections[i].AsString;
end;

function TArrayIniFile.GetSections(Index: integer): TIniSection;
begin
  if Index > Length(FSections) - 1 then
    raise TArrayIniFileException.Create('TArrayIniFile.GetSections: Index out of bounds (' + IntToStr(Index) + ').');
  Result := FSections[Index];
end;

procedure TArrayIniFile.SetAsString(const IniFileContent: string);
begin
  ParseText(IniFileContent);
end;

procedure TArrayIniFile.ParseText(const Text: string);
var
  Section: TIniSection;
  i, xp: integer;
  Line: string;
  SectionName, sIdent, sValue: string;
  Arr: TStringDynArray {$IFDEF FPC}= nil{$ENDIF};
begin
  Section := nil;

  SplitStrToArrayEx(Text, Arr, sLineBreak);

  for i := 0 to Length(Arr) - 1 do
  begin

    Line := Trim(Arr[i]);

    if Line = '' then Continue;
    if Line[1] = ';' then Continue;

    if TStr.IsBoundedWith(Line, '[', ']') then
    begin
      SectionName := TStr.TrimBounds(Line, '[', ']');
      Section := Self.AddSection(SectionName);
      Continue;
    end;

    if not Assigned(Section) then Continue;

    xp := Pos('=', Line);
    if xp = 0 then Continue;

    sIdent := Copy(Line, 1, xp - 1);
    sValue := Copy(Line, xp + 1, Length(Line));

    Section.AddIdent(sIdent, sValue);

  end;

end;

function TArrayIniFile.UpdateFile: Boolean;
begin
  Result := SaveStringToFile(FFileName, AsString, FEncoding);
end;

{$endregion TArrayIniFile}



{$region '                             TIniSection                            '}

constructor TIniSection.Create(const ASectionName: string);
begin
  inherited Create;
  FSectionName := ASectionName;
  FItemsCount := 0;
  FDeltaSize := 100;
  SetLength(FItems, FDeltaSize);
end;

destructor TIniSection.Destroy;
begin
  Clear;
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TIniSection.AsString: string;
var
  i: integer;
begin
  Result := '[' + FSectionName + ']' + sLineBreak;
  for i := 0 to FItemsCount - 1 do
    Result := Result +  FItems[i].AsString + sLineBreak;
end;

procedure TIniSection.Clear;
var
  i: integer;
begin
  for i := 0 to FItemsCount - 1 do FItems[i].Clear;
  FItemsCount := 0;
  SetLength(FItems, FDeltaSize);
end;

function TIniSection.AddIdent(const Ident, Value: string): integer;
var
  xInd: integer;
begin
  xInd := GetIdentIndex(Ident);

  if xInd >= 0 then
  begin
    FItems[xInd].Ident := Ident;
    FItems[xInd].Value := Value;
    Result := xInd;
    Exit;
  end;

  if Length(FItems) = FItemsCount then SetLength(FItems, Length(FItems) + FDeltaSize);
  Inc(FItemsCount);

  xInd := FItemsCount - 1;
  FItems[xInd].Ident := Ident;
  FItems[xInd].Value := Value;

  Result := xInd;
end;

procedure TIniSection.SetIdentValue(const Ident, Value: string);
begin
  AddIdent(Ident, Value);
end;

function TIniSection.GetIdentValue(const Ident: string; Default: string): string;
var
  xInd: integer;
begin
  xInd := GetIdentIndex(Ident);
  if xInd >= 0 then Result := FItems[xInd].Value
  else Result := Default;
end;

function TIniSection.GetIdentIndex(const Ident: string): integer;
var
  i: integer;
  uIdent: string;
begin
  Result := -1;
  if FItemsCount = 0 then Exit;
  uIdent := AnsiUpperCase(Ident);
  for i := 0 to FItemsCount - 1 do
    if AnsiUpperCase(FItems[i].Ident) = uIdent then
    begin
      Result := i;
      Break;
    end;
end;

function TIniSection.IdentExists(const Ident: string): Boolean;
begin
  Result := GetIdentIndex(Ident) >= 0;
end;

function TIniSection.GetItems(Index: integer): TIniSectionItem;
begin
  if Index > FItemsCount - 1 then
    raise TArrayIniFileException.Create('TIniSection.GetItems: Index out of bounds (' + IntToStr(Index) + ').');
  Result := FItems[Index];
end;

function TIniSection.GetCapacity: integer;
begin
  Result := Length(FItems);
end;

procedure TIniSection.SetItems(Index: integer; Item: TIniSectionItem);
begin
  if Index > FItemsCount - 1 then
    raise TArrayIniFileException.Create('TIniSection.SetItems: Index out of bounds (' + IntToStr(Index) + ').');
  FItems[Index].Ident := Item.Ident;
  FItems[Index].Value := Item.Value;
end;

procedure TIniSection.WriteString(const Ident, Value: string);
begin
  AddIdent(Ident, Value);
end;

function TIniSection.ReadString(const Ident, Default: string): string;
begin
  Result := GetIdentValue(Ident, Default);
end;

procedure TIniSection.WriteInteger(const Ident: string; const Value: integer);
begin
  AddIdent(Ident, IntToStr(Value));
end;

function TIniSection.ReadInteger(const Ident: string; Default: integer): integer;
begin
  Result := StrToIntDef(GetIdentValue(Ident, ''), Default);
end;

procedure TIniSection.WriteBool(const Ident: string; const Value: Boolean);
var
  s: string;
begin
  if Value then s := '1' else s := '0';
  WriteString(Ident, s);
end;

function TIniSection.ReadBool(const Ident: string; Default: Boolean): Boolean;
var
  s: string;
begin
  if Default then s := '1' else s := '0';
  s := ReadString(Ident, s);
  if s = '1' then Result := True
  else if s = '0' then Result := False
  else Result := Default;
end;

{$endregion TIniSection}



{$region '                 TIniSectionItem                  '}

procedure TIniSectionItem.Clear;
begin
  Ident := '';
  Value := '';
end;

function TIniSectionItem.AsString: string;
begin
  Result := Ident + '=' + Value;
end;

{$endregion TIniSectionItem}


end.
