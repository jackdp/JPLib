unit JPL.LangMgr;

interface

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFNDEF HAS_RTTI}
  This unit is only for FPC 3.2 or above
  {$ENDIF}
  {$WARN 5079 off : Unit "$1" is experimental}
{$ENDIF}

{$IFDEF DCC}
  {$IFDEF DELPHI2009_OR_BELOW}
  Unit for Delphi 2010 or newer!
  {$ENDIF}
{$ENDIF}


uses
  SysUtils, Classes, Generics.Collections, IniFiles, Rtti, TypInfo,
  StdCtrls, ExtCtrls, ActnList,
  Dialogs,
  JPL.Strings, JPL.Conversion, JPL.IniFile, JPL.FileSearch, JPL.RTTI;

const
  INVALID_COMPONENT_PROP_STR = '!45758679@@##INVALID##@@8989568!';


type

  TLangIniListItem = record
    FileName: string;
    LangName_InEnglish: string;
    LangName_Native: string;
    function AsInfoStr(Indent: string = '  '): string;
  end;

  TLangIniListItems = TList<TLangIniListItem>;

  {$region ' --- TLangIniList --- '}
  TLangIniList = class
  type
    //TLangIniListItems = TList<TLangIniListItem>;
  private
    FItems: TLangIniListItems;
    FIniInfoSectionName: string;
    FIniKeyLangNameEn: string;
    FIniKeyLangNameNative: string;
    function GetCount: integer;
    function GetItems(Index: integer): TLangIniListItem;
  public
    constructor Create;
    destructor Destroy; override;
    function AsInfoStr: string;

    procedure AddFilesFromDir(const Dir: string; Ext: string = 'ini');
    function TryGetLangIniListItem(LangName: string; out Item: TLangIniListItem): Boolean;
    procedure Clear;
    procedure GetLanguageNames_EN(const Strings: TStrings);
    procedure GetLanguageNames_Native(const Strings: TStrings);
    procedure GetLanguageNames_EnglishAndNative(const Strings: TStrings; const Separator: string = ' - ');
    procedure GetLanguageFileNames(const Strings: TStrings);

    property IniInfoSectionName: string read FIniInfoSectionName write FIniInfoSectionName;
    property IniKeyLangNameEn: string read FIniKeyLangNameEn write FIniKeyLangNameEn;
    property IniKeyLangNameNative: string read FIniKeyLangNameNative write FIniKeyLangNameNative;
    property Count: integer read GetCount;
    property Items[Index: integer]: TLangIniListItem read GetItems; default; // write SetItems; default;
  end;
  {$endregion TLangIniList}


  TLangItemProperties = TDictionary<string,string>;

  TLangComponentItem = record
  private
    procedure SetCaption(const Value: string);
    function GetCaption: string;
  public
    Component: TComponent;
    Properties: TLangItemProperties;
    function AsInfoStr(Indent: string = '  '): string;

    function AddProperty(const PropertyName, PropertyString: string): TLangComponentItem; overload;
    function ap(const PropertyName, PropertyString: string): TLangComponentItem; overload; // As above, but shorter name
    function AddProperty(const PropertyName: string): TLangComponentItem; overload;
    function ap(const PropertyName: string): TLangComponentItem; overload; // As above, but shorter name

    function AddCaption(const CaptionStr: string): TLangComponentItem;
    function AddHint(const HintStr: string): TLangComponentItem;
    function GetProperty(const PropertyName: string; Default: string = ''): string;

    property Caption: string read GetCaption write SetCaption;
  end;

  // KEY: INI key name, VALUE: TLangComponentItem
  TLangComponentItems = TDictionary<string,TLangComponentItem>;


  TLangStringItems = TDictionary<string,string>;

  {$region ' --- TLangSection --- '}
  TLangSection = class
  private
    FLangStringItems: TLangStringItems;
    FLangComponentItems: TLangComponentItems;
    FSectionName: string;
    FComponentNamePropertyNameSeparator: string;
    procedure SetSectionName(const Value: string);
    function GetStringItemsCount: integer;
    function GetComponentItemsCount: integer;
    function GetAllItemsCount: integer;
    procedure SetComponentNamePropertyNameSeparator(const Value: string);
  public
    constructor Create(const SectionName: string);
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromIniSection(const Ini: TJPIniFile);
    procedure UpdateComponents;

    procedure AddString(const KeyName, StrValue: string);
    function GetString(const KeyName: string; Default: string = ''): string;


    function AddComponent(const ComponentName: string): TLangComponentItem; overload;
    function AddComponent(Component: TComponent): TLangComponentItem; overload;
    function AddComponent(Component: TComponent; const ACaption: string): TLangComponentItem; overload;
    function AddComponent(Component: TComponent; const ACaption, AHint: string): TLangComponentItem; overload;

    // ac = AddComponent (short name)
    function ac(Component: TComponent): TLangComponentItem; overload;
    function ac(Component: TComponent; const ACaption: string): TLangComponentItem; overload;
    function ac(Component: TComponent; const ACaption, AHint: string): TLangComponentItem; overload;


    function AddComponentWithCaption(Component: TComponent): TLangComponentItem;
    function acc(Component: TComponent): TLangComponentItem; // calls AddComponentWithCaption

    function AddComponentWithCaptionAndHint(Component: TComponent): TLangComponentItem;
    function acch(Component: TComponent): TLangComponentItem; // calls AddComponentWithCaptionAndHint


    function AddLabel(const ALabel: TCustomLabel; bCaption: Boolean = True; bHint: Boolean = False): TLangComponentItem;
    function AddStaticText(const StaticText: TStaticText; bCaption: Boolean = True; bHint: Boolean = False): TLangComponentItem;
    function AddAction(const Action: TCustomAction; bCaption: Boolean = True; bHint: Boolean = True): TLangComponentItem;
    function AddCheckBox(const CheckBox: TCheckBox; bCaption: Boolean = True; bHint: Boolean = True): TLangComponentItem;
    procedure AddCheckBoxArray(Arr: array of TCheckBox; bCaption: Boolean = True; bHint: Boolean = True);
    function AddRadioButton(const RadioButton: TRadioButton; bCaption: Boolean = True; bHint: Boolean = True): TLangComponentItem;
    function AddLabeledEdit(const LabeledEdit: TCustomLabeledEdit; bLabelCaption: Boolean = True; bEditText: Boolean = False): TLangComponentItem;

    function TryGetComponentItem(const KeyName: string; out Item: TLangComponentItem): Boolean;
    function GetComponentProperty(const ComponentName, PropertyName: string; Default: string = ''): string;

    function AsInfoStr: string;

    property SectionName: string read FSectionName write SetSectionName;
    property LangStringItems: TLangStringItems read FLangStringItems;
    property LangComponentItems: TLangComponentItems read FLangComponentItems;

    property StringItemsCount: integer read GetStringItemsCount;
    property ComponentItemsCount: integer read GetComponentItemsCount;
    property AllItemsCount: integer read GetAllItemsCount;
    property ComponentNamePropertyNameSeparator: string read FComponentNamePropertyNameSeparator write SetComponentNamePropertyNameSeparator;
  end;
  {$endregion TLangSection}


  TLangSections = TList<TLangSection>;


  {$region ' --- TLangMgr --- '}
  TLangMgr = class
  private
    FLangIniList: TLangIniList;
    FLangSections: TLangSections;
    FComboBox: TComboBox;
    FActiveLanguageIndex: integer;
    function GetLanguageCount: integer;
    function GetSection(Index: integer): TLangSection;
    function GetSectionCount: integer;
    procedure SetComboBox(const Value: TComboBox);
    procedure LoadStringsFromFile(const IniFileName: string);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AsInfoStr: string;
    procedure FillComboBox;

    function ReloadSectionFromFile(const SectionName, IniFileName: string): Boolean;

    procedure SetActiveLanguageByIndex(const Index: integer);
    procedure SetActiveLanguageByIniFileName(const IniFileName: string);

    procedure AddFilesFromDir(const Dir: string; Ext: string = 'ini');
    procedure GetLanguageNames_EN(const Strings: TStrings);
    procedure GetLanguageNames_Native(const Strings: TStrings);
    procedure GetLanguageNames_EnglishAndNative(const Strings: TStrings; const Separator: string = ' - ');
    procedure GetLanguageFileNames(const Strings: TStrings);
    function GetLanguageFileNameByIndex(const Index: integer): string;
    function GetLangIndexByFileName(const IniFileName: string): integer;

    function AddSection(const SectionName: string): TLangSection;
    function GetSectionByName(const SectionName: string): TLangSection;
    procedure RemoveSection(LangSection: TLangSection);

    function GetString(const SectionName, KeyName: string; Default: string = ''): string;

    property LanguageCount: integer read GetLanguageCount;

    property SectionCount: integer read GetSectionCount;
    property Section[Index: integer]: TLangSection read GetSection; default;
    property Sections: TLangSections read FLangSections;
    property ComboBox: TComboBox read FComboBox write SetComboBox;
    property ActiveLanguageIndex: integer read FActiveLanguageIndex;
  end;
  {$endregion TLangMgr}


function fixs(s: string; p1: string = ''; p2: string = ''): string;


implementation


function fixs(s: string; p1: string = ''; p2: string = ''): string;
begin
  s := StringReplace(s, '\n', ENDL, [rfReplaceAll]);
  s := StringReplace(s, '\t', TAB, [rfReplaceAll]);
  if p1 <> '' then
  begin
    s := StringReplace(s, '%s', p1, []);
    if p2 <> '' then s := StringReplace(s, '%s', p2, []);
  end;
  Result := s;
end;



{$region ' ------------------------------- TLangSection ----------------------------- '}

constructor TLangSection.Create(const SectionName: string);
begin
  inherited Create;
  FSectionName := SectionName;
  FComponentNamePropertyNameSeparator := '.';

  FLangStringItems := TLangStringItems.Create;
  FLangComponentItems := TLangComponentItems.Create;
end;

destructor TLangSection.Destroy;
var
  Key: string;
  lci: TLangComponentItem;
begin
  FSectionName := '';
  for Key in FLangComponentItems.Keys do
  begin
    lci := FLangComponentItems[Key];
    if Assigned(lci.Properties) then lci.Properties.Free;
  end;
  FLangComponentItems.Free;
  FLangStringItems.Free;
  inherited;
end;

procedure TLangSection.Clear;
begin
  FLangStringItems.Clear;
  FLangComponentItems.Clear;
end;

function TLangSection.GetAllItemsCount: integer;
begin
  Result := StringItemsCount + ComponentItemsCount;
end;

function TLangSection.GetComponentItemsCount: integer;
begin
  Result := FLangComponentItems.Count;
end;

function TLangSection.GetComponentProperty(const ComponentName, PropertyName: string; Default: string): string;
var
  Item: TLangComponentItem;
begin
  Result := Default;
  if not TryGetComponentItem(ComponentName, Item) then Exit;
  Result := Item.GetProperty(PropertyName, Default);
end;

function TLangSection.GetString(const KeyName: string; Default: string): string;
begin
  if not FLangStringItems.TryGetValue(KeyName, Result) then Result := Default;
  Result := fixs(Result);
end;

function TLangSection.GetStringItemsCount: integer;
begin
  Result := FLangStringItems.Count;
end;

procedure TLangSection.SetComponentNamePropertyNameSeparator(const Value: string);
begin
  FComponentNamePropertyNameSeparator := Value;
end;

procedure TLangSection.SetSectionName(const Value: string);
begin
  if FSectionName = Value then Exit;
  FSectionName := Value;
end;

function TLangSection.TryGetComponentItem(const KeyName: string; out Item: TLangComponentItem): Boolean;
begin
  Result := FLangComponentItems.TryGetValue(KeyName, Item);
end;


procedure TLangSection.LoadFromIniSection(const Ini: TJPIniFile); // const SectionName: string);
var
  Section: TJPIniSection;
  Key, CompName, PropName, PropValue, IniKey: string;
  lci: TLangComponentItem;
begin
  Section := Ini.GetSection(SectionName, False);
  if not Assigned(Section) then Exit;


  for Key in FLangStringItems.Keys do
  begin
    IniKey := Key;
    if not Section.KeyExists(IniKey) then Continue;
    PropValue := Section.ReadString(IniKey, '');
    FLangStringItems[Key] := PropValue
  end;


  for Key in FLangComponentItems.Keys do
  begin
    lci := FLangComponentItems[Key];
    CompName := Key;
    for PropName in lci.Properties.Keys do
    begin
      IniKey := CompName + FComponentNamePropertyNameSeparator + PropName;
      if not Section.KeyExists(IniKey) then Continue;
      PropValue := Section.ReadString(IniKey, '');
      lci.Properties.AddOrSetValue(PropName, PropValue);
    end;
  end;
end;


procedure TLangSection.UpdateComponents;
var
  Key, PropName, PropValue, ObjName, PropName2: string;
  lci: TLangComponentItem;
  x: integer;
  Obj: TObject;
begin
  for Key in FLangComponentItems.Keys do
  begin
    lci := FLangComponentItems[Key];

    if not Assigned(lci.Component) then Continue;
    if not Assigned(lci.Properties) then Continue;

    for PropName in lci.Properties.Keys do
    begin

      PropValue := lci.Properties[PropName];
      PropValue := fixs(PropValue);

      //if PropName.Contains('.') then
      x := Pos('.', PropName);
      if x > 0 then
      begin
        //x := Pos('.', PropName);
        ObjName := Copy(PropName, 1, x - 1);
        PropName2 := Copy(PropName, x + 1, Length(PropName));
        if TryGetPropertyAsObject(lci.Component, ObjName, Obj) then SetPropertyText(Obj, PropName2, PropValue);
      end
      else
        SetPropertyText(lci.Component, PropName, PropValue);

    end;

  end;

end;


  {$region '                    Add component                    '}

function TLangSection.AddComponent(Component: TComponent): TLangComponentItem; // overload
var
  Item: TLangComponentItem;
begin
  if FLangComponentItems.ContainsKey(Component.Name) then Item := FLangComponentItems.Items[Component.Name]
  else
  begin
    Item.Component := Component;
    Item.Properties := TLangItemProperties.Create;
    FLangComponentItems.AddOrSetValue(Component.Name, Item);
  end;
  Result := Item;
end;

function TLangSection.AddComponent(Component: TComponent; const ACaption: string): TLangComponentItem;
begin
  Result := AddComponent(Component);
  Result.AddCaption(ACaption);
end;

function TLangSection.AddComponent(Component: TComponent; const ACaption, AHint: string): TLangComponentItem;
begin
  Result := AddComponent(Component);
  Result.AddCaption(ACaption);
  Result.AddHint(AHint);
end;

function TLangSection.AddComponent(const ComponentName: string): TLangComponentItem; // overload
var
  Item: TLangComponentItem;
begin
  if FLangComponentItems.ContainsKey(ComponentName) then Item := FLangComponentItems.Items[ComponentName]
  else
  begin
    Item.Component := nil;
    Item.Properties := TLangItemProperties.Create;
    FLangComponentItems.AddOrSetValue(ComponentName, Item);
  end;
  Result := Item;
end;

function TLangSection.ac(Component: TComponent): TLangComponentItem;
begin
  Result := AddComponent(Component);
end;

function TLangSection.ac(Component: TComponent; const ACaption: string): TLangComponentItem;
begin
  Result := AddComponent(Component, ACaption);
end;

function TLangSection.ac(Component: TComponent; const ACaption, AHint: string): TLangComponentItem;
begin
  Result := AddComponent(Component, ACaption, AHint);
end;

function TLangSection.AddComponentWithCaption(Component: TComponent): TLangComponentItem;
var
  s: string;
begin
  Result := AddComponent(Component);
  s := GetPropertyText(Component, 'Caption', INVALID_COMPONENT_PROP_STR);
  if s <> INVALID_COMPONENT_PROP_STR then Result.AddCaption(s);
end;

function TLangSection.acc(Component: TComponent): TLangComponentItem;
begin
  Result := AddComponentWithCaption(Component);
end;

function TLangSection.AddComponentWithCaptionAndHint(Component: TComponent): TLangComponentItem;
var
  s: string;
begin
  Result := AddComponent(Component);

  s := GetPropertyText(Component, 'Caption', INVALID_COMPONENT_PROP_STR);
  if s <> INVALID_COMPONENT_PROP_STR then Result.AddCaption(s);

  s := GetPropertyText(Component, 'Hint', INVALID_COMPONENT_PROP_STR);
  if s <> INVALID_COMPONENT_PROP_STR then Result.AddHint(s);
end;

function TLangSection.acch(Component: TComponent): TLangComponentItem;
begin
  Result := AddComponentWithCaptionAndHint(Component);
end;

  {$endregion Add component}



function TLangSection.AddAction(const Action: TCustomAction; bCaption: Boolean = True; bHint: Boolean = True): TLangComponentItem;
begin
  Result := AddComponent(Action);
  if bCaption then Result.AddProperty('Caption', Action.Caption);
  if bHint then Result.AddProperty('Hint', Action.Hint);
end;

function TLangSection.AddLabel(const ALabel: TCustomLabel; bCaption: Boolean = True; bHint: Boolean = False): TLangComponentItem;
begin
  Result := AddComponent(ALabel);
  if bCaption then Result.AddProperty('Caption', ALabel.Caption);
  if bHint then Result.AddProperty('Hint', ALabel.Hint);
end;

function TLangSection.AddStaticText(const StaticText: TStaticText; bCaption: Boolean = True; bHint: Boolean = False): TLangComponentItem;
begin
  Result := AddComponent(StaticText);
  if bCaption then Result.AddProperty('Caption', StaticText.Caption);
  if bHint then Result.AddProperty('Hint', StaticText.Hint);
end;

function TLangSection.AddLabeledEdit(const LabeledEdit: TCustomLabeledEdit; bLabelCaption, bEditText: Boolean): TLangComponentItem;
begin
  Result := AddComponent(LabeledEdit);
  if bLabelCaption then Result.AddProperty('EditLabel.Caption', LabeledEdit.EditLabel.Caption);
  if bEditText then Result.AddProperty('Text', LabeledEdit.Text);
end;

function TLangSection.AddRadioButton(const RadioButton: TRadioButton; bCaption, bHint: Boolean): TLangComponentItem;
begin
  Result := AddComponent(RadioButton);
  if bCaption then Result.AddProperty('Caption', RadioButton.Caption);
  if bHint then Result.AddProperty('Hint', RadioButton.Hint);
end;

function TLangSection.AddCheckBox(const CheckBox: TCheckBox; bCaption: Boolean = True; bHint: Boolean = True): TLangComponentItem;
begin
  Result := AddComponent(CheckBox);
  if bCaption then Result.AddProperty('Caption', CheckBox.Caption);
  if bHint then Result.AddProperty('Hint', CheckBox.Hint);
end;

procedure TLangSection.AddCheckBoxArray(Arr: array of TCheckBox; bCaption: Boolean = True; bHint: Boolean = True);
var
  cb: TCheckBox;
  i: integer;
begin
  for i := 0 to Length(Arr) - 1 do
  begin
    cb := Arr[i];
    AddCheckBox(cb, bCaption, bHint);
  end;
end;





procedure TLangSection.AddString(const KeyName, StrValue: string);
begin
  FLangStringItems.AddOrSetValue(KeyName, StrValue);
end;

function TLangSection.AsInfoStr: string;
var
  s, Key, Value: string;
  x: integer;
  lci: TLangComponentItem;
begin
  s := 'Section: ' + FSectionName + ENDL;

  s := s + ENDL + 'String items: ' + itos(StringItemsCount) + ENDL;


  x := 0;
  for Key in FLangStringItems.Keys do
  begin
    Inc(x);
    Value := FLangStringItems.Items[Key];
    s := s + itos(x) + ': ' + Key + '=' + Value + ENDL;
  end;


  s := s + ENDL + 'Component items: ' + itos(ComponentItemsCount) + ENDL;

  x := 0;
  for Key in FLangComponentItems.Keys do
  begin
    Inc(x);
    lci := FLangComponentItems.Items[Key];
    s := s + 'Component ' + itos(x) + ENDL + lci.AsInfoStr('  ') + ENDL;
  end;

  Result := Trim(s);
end;


{$endregion TLangSection}


{$region ' ------------------- TLangComponentItem --------------------- '}

function TLangComponentItem.AddProperty(const PropertyName, PropertyString: string): TLangComponentItem;
begin
  Result := Self;
  if not Assigned(Properties) then Properties := TLangItemProperties.Create;
  Properties.AddOrSetValue(PropertyName, PropertyString);
end;

function TLangComponentItem.ap(const PropertyName, PropertyString: string): TLangComponentItem;
begin
  Result := AddProperty(PropertyName, PropertyString);
end;

function TLangComponentItem.AddProperty(const PropertyName: string): TLangComponentItem;
var
  s: string;
begin
  Result := Self;
  if not Assigned(Self.Component) then Exit;
  s := GetPropertyText(Self.Component, PropertyName, INVALID_COMPONENT_PROP_STR);
  if s <> INVALID_COMPONENT_PROP_STR then Result := AddProperty(PropertyName, s);
end;

function TLangComponentItem.ap(const PropertyName: string): TLangComponentItem;
begin
  Result := AddProperty(PropertyName);
end;

function TLangComponentItem.AddCaption(const CaptionStr: string): TLangComponentItem;
begin
  Result := AddProperty('Caption', CaptionStr);
end;

function TLangComponentItem.AddHint(const HintStr: string): TLangComponentItem;
begin
  Result := AddProperty('Hint', HintStr);
end;

function TLangComponentItem.AsInfoStr(Indent: string): string;
var
  s, Key, Value: string;
begin
  if not Assigned(Component) then Exit('<component not assigned!>');
  s := Indent + Component.Name + ': ' + Component.ClassName + ENDL;

  if Assigned(Properties) then
  begin
    s := s + Indent + 'Properties: ' + itos(Properties.Count) + ENDL;
    for Key in Properties.Keys do
    begin
      Value := Properties[Key];
      s := s + Indent + Key + ' = ' + Value + ENDL;
    end;
  end;

  Result := TrimRight(s);
end;

function TLangComponentItem.GetProperty(const PropertyName: string; Default: string): string;
begin
  Result := Default;
  if not Assigned(Properties) then Exit;
  if not Properties.TryGetValue(PropertyName, Result) then Exit;
end;

function TLangComponentItem.GetCaption: string;
begin
  Result := GetProperty('Caption');
end;

procedure TLangComponentItem.SetCaption(const Value: string);
begin
  AddProperty('Caption', Value);
end;

{$endregion TLangComponentItem}


{$region ' ---------------------------------- TLangMgr ------------------------------------ '}

constructor TLangMgr.Create;
begin
  FLangIniList := TLangIniList.Create;
  FLangSections := TLangSections.Create; // TList<TLangSection>
  FActiveLanguageIndex := -1;
  FComboBox := nil;
end;

destructor TLangMgr.Destroy;
begin
  Clear;
  if Assigned(FLangIniList) then FLangIniList.Free;
  if Assigned(FLangSections) then FLangSections.Free;
  inherited;
end;

procedure TLangMgr.FillComboBox;
var
  ChangeProc: TNotifyEvent;
begin
  if not Assigned(FComboBox) then Exit;

  ChangeProc := nil;
  if Assigned(FComboBox.OnChange) then
  begin
    ChangeProc := FComboBox.OnChange;
    FComboBox.OnChange := nil;
  end;
  GetLanguageNames_EnglishAndNative(FComboBox.Items, '  -  ');
  FComboBox.OnChange := ChangeProc;
end;

procedure TLangMgr.Clear;
var
  i: integer;
begin
  for i := 0 to FLangSections.Count - 1 do
    if Assigned(Section[i]) then Section[i].Free;
end;

function TLangMgr.AddSection(const SectionName: string): TLangSection;
var
  Section: TLangSection;
begin
  Section := TLangSection.Create(SectionName);
  FLangSections.Add(Section);
  Result := Section;
end;

function TLangMgr.AsInfoStr: string;
const
  Line: string = '===============================================';
var
  s: string;
  i: integer;
  Section: TLangSection;
begin
  s := 'Language count: ' + itos(LanguageCount);
  s := s + ENDL + 'Sections: ' + itos(SectionCount) + ENDL + Line;

  s := s + ENDL + FLangIniList.AsInfoStr;


  s := s + ENDL + Line + ENDL;
  s := s + 'SECTIONS' + ENDL;
  for i := 0 to FLangSections.Count - 1 do
  begin
    Section := FLangSections[i];
    s := s + 'Section No ' + itos(i + 1) + ENDL;
    s := s + Section.AsInfoStr + ENDL;
  end;

  Result := s;
end;

function TLangMgr.GetLangIndexByFileName(const IniFileName: string): integer;
var
  i: integer;
  UFileName: string;
begin
  Result := -1;
  UFileName := AnsiUpperCase(IniFileName);
  for i := 0 to FLangIniList.Count - 1 do
    if AnsiUpperCase(FLangIniList[i].FileName) = UFileName then
    begin
      Result := i;
      Break;
    end;
end;

function TLangMgr.GetLanguageCount: integer;
begin
  Result := FLangIniList.Count;
end;



procedure TLangMgr.GetLanguageFileNames(const Strings: TStrings);
begin
  FLangIniList.GetLanguageFileNames(Strings);
end;

procedure TLangMgr.GetLanguageNames_EN(const Strings: TStrings);
begin
  FLangIniList.GetLanguageNames_EN(Strings);
end;

procedure TLangMgr.GetLanguageNames_Native(const Strings: TStrings);
begin
  FLangIniList.GetLanguageNames_Native(Strings);
end;

procedure TLangMgr.GetLanguageNames_EnglishAndNative(const Strings: TStrings; const Separator: string = ' - ');
begin
  FLangIniList.GetLanguageNames_EnglishAndNative(Strings, Separator);
end;

function TLangMgr.GetSection(Index: integer): TLangSection;
begin
  Result := FLangSections.Items[Index];
end;

function TLangMgr.GetSectionByName(const SectionName: string): TLangSection;
var
  UName: string;
  Section: TLangSection;
begin
  Result := nil;
  UName := AnsiUpperCase(SectionName);
  for Section in FLangSections do
    if AnsiUpperCase(Section.SectionName) = UName then
    begin
      Result := Section;
      Break;
    end;
end;



procedure TLangMgr.RemoveSection(LangSection: TLangSection);
begin
  FLangSections.Remove(LangSection);
end;

function TLangMgr.GetSectionCount: integer;
begin
  Result := FLangSections.Count;
end;

function TLangMgr.GetString(const SectionName, KeyName: string; Default: string): string;
var
  Section: TLangSection;
begin
  Result := Default;
  Section := GetSectionByName(SectionName);
  if not Assigned(Section) then Exit;
  Result := Section.GetString(KeyName, Default);
end;

procedure TLangMgr.LoadStringsFromFile(const IniFileName: string);
var
  Ini: TJPIniFile;
  Section: TLangSection;
begin
  if not FileExists(IniFileName) then Exit;
  Ini := TJPIniFile.Create(IniFileName, TEncoding.UTF8);
  try
    Ini.UpdateFileOnExit := False;
    for Section in FLangSections do
    begin
      Section.LoadFromIniSection(Ini);
      Section.UpdateComponents;      //ShowMessage(Section.SectionName + '    ' + IniFileName);
    end;
    Ini.UpdateFileOnExit := False;
  finally
    Ini.Free;
  end;
end;

function TLangMgr.ReloadSectionFromFile(const SectionName, IniFileName: string): Boolean;
var
  Ini: TJPIniFile;
  Section: TLangSection;
begin
  Result := False;
  if not FileExists(IniFileName) then Exit;
  Ini := TJPIniFile.Create(IniFileName, TEncoding.UTF8);
  try
    Ini.UpdateFileOnExit := False;

    for Section in FLangSections do
      if Section.SectionName = SectionName then
      begin
        Section.LoadFromIniSection(Ini);
        Section.UpdateComponents;
        Result := True;
        Break;
      end;

    Ini.UpdateFileOnExit := False;
  finally
    Ini.Free;
  end;
end;

function TLangMgr.GetLanguageFileNameByIndex(const Index: integer): string;
begin
  if Index in [0..FLangIniList.Count - 1] then Result := FLangIniList[Index].FileName
  else Result := '';
end;

procedure TLangMgr.SetActiveLanguageByIndex(const Index: integer);
begin
  if Index in [0..FLangIniList.Count - 1] then
  begin
    FActiveLanguageIndex := Index;
    LoadStringsFromFile(FLangIniList[Index].FileName);
  end
  else
    FActiveLanguageIndex := -1;
end;

procedure TLangMgr.SetActiveLanguageByIniFileName(const IniFileName: string);
var
  x: integer;
begin
  x := GetLangIndexByFileName(IniFileName);
  SetActiveLanguageByIndex(x);
end;

procedure TLangMgr.SetComboBox(const Value: TComboBox);
begin
  if FComboBox = Value then Exit;
  FComboBox := Value;
end;

procedure TLangMgr.AddFilesFromDir(const Dir: string; Ext: string);
begin
  FLangIniList.AddFilesFromDir(Dir, Ext);
  FillComboBox;
end;


{$endregion TLangMgr}







{$region ' ------------------------------ TLangIniList ----------------------------------- '}

constructor TLangIniList.Create;
begin
  inherited Create;

  FIniInfoSectionName := 'Info';
  FIniKeyLangNameEn := 'LangName_InEnglish';
  FIniKeyLangNameNative := 'LangName_Native';

  FItems := TLangIniListItems.Create;
end;

destructor TLangIniList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLangIniList.AsInfoStr: string;
var
  i: integer;
  lili: TLangIniListItem;
begin
  Result :=
    'TLangIniList' + ENDL +
    'INI Info section name: ' + FIniInfoSectionName + ENDL +
    'INI key - English language name: ' + FIniKeyLangNameEn + ENDL +
    'INI key - Native language name: ' + FIniKeyLangNameNative + ENDL +
    'Count: ' + itos(Count) + ENDL;

  if Count > 0 then
  for i := 0 to FItems.Count - 1 do
  begin
    lili := FItems[i];
    Result := Result + itos(i + 1) + ENDL + lili.AsInfoStr('  ') + ENDL + ENDL;
  end;
end;

procedure TLangIniList.Clear;
begin
  FItems.Clear;
end;

function TLangIniList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TLangIniList.GetItems(Index: integer): TLangIniListItem;
begin
  Result := FItems[Index];
end;

procedure TLangIniList.GetLanguageFileNames(const Strings: TStrings);
var
  i: integer;
begin
  Strings.Clear;
  for i := 0 to FItems.Count - 1 do
    Strings.Add(FItems[i].FileName);
end;

procedure TLangIniList.GetLanguageNames_EN(const Strings: TStrings);
var
  i: integer;
begin
  Strings.Clear;
  for i := 0 to FItems.Count - 1 do
    Strings.Add(FItems[i].LangName_InEnglish);
end;

procedure TLangIniList.GetLanguageNames_Native(const Strings: TStrings);
var
  i: integer;
begin
  Strings.Clear;
  for i := 0 to FItems.Count - 1 do
    Strings.Add(FItems[i].LangName_Native);
end;

procedure TLangIniList.GetLanguageNames_EnglishAndNative(const Strings: TStrings; const Separator: string = ' - ');
var
  i: integer;
  sEn, sNat, s: string;
begin
  Strings.Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    sEn := FItems[i].LangName_InEnglish;
    sNat := FItems[i].LangName_Native;
    if sEn = sNat then s := sEn
    else s := sEn + Separator + sNat;
    Strings.Add(s);
  end;
end;

function TLangIniList.TryGetLangIniListItem(LangName: string; out Item: TLangIniListItem): Boolean;
var
  i: integer;
  lili: TLangIniListItem;
begin
  Result := False;
  LangName := Trim(AnsiUpperCase(LangName));
  for i := 0 to FItems.Count - 1 do
  begin
    lili := FItems[i];
    if (AnsiUpperCase(lili.LangName_InEnglish) = LangName) or (AnsiUpperCase(lili.LangName_Native) = LangName) then
    begin
      Item := lili;
      Result := True;
      Break;
    end;
  end;
end;


procedure TLangIniList.AddFilesFromDir(const Dir: string; Ext: string = 'ini');
var
  sl: TStringList;
  i: integer;
  fName: string;
  Item: TLangIniListItem;
  Ini: TMemIniFile;
begin
  if not DirectoryExists(Dir) then Exit;
  sl := TStringList.Create;
  try

    JPGetFileList('*.' + Ext, Dir, sl, 0);
    sl.Sort;

    for i := 0 to sl.Count - 1 do
    begin
      fName := sl[i];
      if not FileExists(fName) then Continue;

      Ini := TMemIniFile.Create(fName);
      try
        if not Ini.SectionExists(FIniInfoSectionName) then Continue;
        if not Ini.ValueExists(FIniInfoSectionName, FIniKeyLangNameEn) then Continue;
        if not Ini.ValueExists(FIniInfoSectionName, FIniKeyLangNameNative) then Continue;
        Item.FileName := fName;
        Item.LangName_InEnglish := Ini.ReadString(FIniInfoSectionName, FIniKeyLangNameEn, '');
        Item.LangName_Native := Ini.ReadString(FIniInfoSectionName, FIniKeyLangNameNative, '');
        FItems.Add(Item);
      finally
        Ini.Free;
      end;

    end;

  finally
    sl.Free;
  end;
end;

{$endregion TLangIniList}










{ TLangIniListItem }

function TLangIniListItem.AsInfoStr(Indent: string): string;
begin
  Result :=
    Indent + 'File name: ' + FileName + ENDL +
    Indent + 'English language name: ' + LangName_InEnglish + ENDL +
    Indent + 'Native language name: ' + LangName_Native;
end;



end.
