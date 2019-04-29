unit JPL.JsonHelpers;

{:<
  JSON related helper class and routines
}
// Last mod: 2019.04

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner;


type
  // TJSONtype = (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject);

  TJsonObjectHelper = class helper for TJSONObject
  private
  public

    procedure SaveToFile(const FileName: string; Formatted: Boolean = True);
    procedure AddObject(const ObjName: TJSONStringType; Obj: TJSONObject; AddCopyOfObj: Boolean = True);
    procedure AddArray(const ArrName: TJSONStringType; Arr: TJSONArray; AddCopyOfArr: Boolean = True);
    function TryInitFromStr(const JSONSource: TJSONStringType; Silent: Boolean = False): Boolean;
    function NameTypeExists(const Name: string; const JSONType: TJSONType; CaseSensitive: Boolean = False): Boolean;
    function GetNameTypeIndex(const Name: string; const JSONType: TJSONType; CaseSensitive: Boolean = False): integer;


    function TryGetValue(const Name: string; out Value: TJSONObject; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: TJSONArray; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: string; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: UnicodeString; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: integer; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Int64; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: UInt64; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Double; CaseSensitive: Boolean = False): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Boolean; CaseSensitive: Boolean = False): Boolean; overload;

    function TryGetObject(const Name: string; out JSONObject: TJSONObject; CaseSensitive: Boolean = False): Boolean;
    function TryGetArray(const Name: string; out JSONArray: TJSONArray; CaseSensitive: Boolean = False): Boolean;
    function TryGetString(const Name: string; out StrValue: string; CaseSensitive: Boolean = False): Boolean;
    function TryGetUnicodeString(const Name: string; out StrValue: UnicodeString; CaseSensitive: Boolean = False): Boolean;
    function TryGetInteger(const Name: string; out IntValue: integer; CaseSensitive: Boolean = False): Boolean;
    function TryGetInt64(const Name: string; out Int64Value: Int64; CaseSensitive: Boolean = False): Boolean;
    function TryGetUInt64(const Name: string; out UInt64Value: UInt64; CaseSensitive: Boolean = False): Boolean;
    function TryGetFloat(const Name: string; out FloatValue: Double; CaseSensitive: Boolean = False): Boolean;
    function TryGetBool(const Name: string; out BoolValue: Boolean; CaseSensitive: Boolean = False): Boolean;

    function ReadString(const Name, DefResult: string; CaseSensitive: Boolean = False): string;
    procedure WriteString(const Name, Value: string; CaseSensitive: Boolean = False);

    function ReadUnicodeString(const Name: string; const DefResult: UnicodeString; CaseSensitive: Boolean = False): UnicodeString;
    procedure WriteUnicodeString(const Name: string; const Value: UnicodeString; CaseSensitive: Boolean = False);

    function ReadInteger(const Name: string; const DefResult: integer; CaseSensitive: Boolean = False): integer;
    procedure WriteInteger(const Name: string; const Value: integer; CaseSensitive: Boolean = False);

    function ReadInt64(const Name: string; const DefResult: Int64; CaseSensitive: Boolean = False): Int64;
    procedure WriteInt64(const Name: string; const Value: Int64; CaseSensitive: Boolean = False);

    function ReadUInt64(const Name: string; const DefResult: UInt64; CaseSensitive: Boolean = False): UInt64;
    procedure WriteUInt64(const Name: string; const Value: UInt64; CaseSensitive: Boolean = False);

    function ReadFloat(const Name: string; const DefResult: Double; CaseSensitive: Boolean = False): Double;
    procedure WriteFloat(const Name: string; const Value: Double; CaseSensitive: Boolean = False);

    function ReadBool(const Name: string; const DefResult: Boolean; CaseSensitive: Boolean = False): Boolean;
    procedure WriteBool(const Name: string; const Value: Boolean; CaseSensitive: Boolean = False);

    /////////////////
    function GetArray(const AName: string; CaseSensitive: Boolean = False): TJSONArray;
    function GetObject(const AName: string; CaseSensitive: Boolean = False): TJSONObject;
  end;

  TJsonDataHelper = class helper for TJSONData
  private
  public
    function IsObject: Boolean;
    function IsNumber: Boolean;
    function IsBool: Boolean;
    function IsArray: Boolean;
    function IsString: Boolean;
    function IsUnknown: Boolean;
  end;


function GetJSONObjectFromFile(const FileName: string; Silent: Boolean = True): TJSONObject; overload;
function GetJSONObjectFromFile(const FileName: string; var ErrStr: string): TJSONObject; overload;
function GetJSONObjectFromStr(const JSONSource: string; Silent: Boolean = True): TJSONObject; overload;
function GetJSONObjectFromStr(const JSONSource: string; var ErrStr: string): TJSONObject; overload;
function GetJSONArrayFromStr(const JSONSource: string; Silent: Boolean = True): TJSONArray;
function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: string; CaseSensitive: Boolean = False): Boolean; overload;
function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: integer; CaseSensitive: Boolean = False): Boolean; overload;
function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: TJSONArray; CaseSensitive: Boolean = False): Boolean; overload;
function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: TJSONObject; CaseSensitive: Boolean = False): Boolean; overload;
function TryGetJsonObject(JSONObject: TJSONObject; const Name: string; out NewJsonObject: TJSONObject; CaseSensitive: Boolean = False): Boolean;
function TryGetJsonArray(JSONObject: TJSONObject; const Name: string; out JSONArray: TJSONArray; CaseSensitive: Boolean = False): Boolean;


implementation

{$region '                         TJsonDataHelper                            '}
function TJsonDataHelper.IsObject: Boolean;
begin
  Result := Self.JSONType = jtObject;
end;

function TJsonDataHelper.IsNumber: Boolean;
begin
  Result := Self.JSONType = jtNumber;
end;

function TJsonDataHelper.IsBool: Boolean;
begin
  Result := Self.JSONType = jtBoolean;
end;

function TJsonDataHelper.IsArray: Boolean;
begin
  Result := Self.JSONType = jtArray;
end;

function TJsonDataHelper.IsString: Boolean;
begin
  Result := Self.JSONType = jtString;
end;

function TJsonDataHelper.IsUnknown: Boolean;
begin
  Result := Self.JSONType = jtUnknown;
end;

{$endregion TJsonDataHelper}

//uses Dialogs; procedure Msg(s:string);begin ShowMessage(s);end;


procedure TJsonObjectHelper.SaveToFile(const FileName: string; Formatted: Boolean = True);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if Formatted then sl.Text := Self.FormatJSON
    else sl.Text := Self.AsJSON;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TJsonObjectHelper.AddObject(const ObjName: TJSONStringType; Obj: TJSONObject; AddCopyOfObj: Boolean = True);
begin
  if not Assigned(Obj) then Exit;
  if AddCopyOfObj then Self.Add(ObjName, GetJSON(Obj.AsJSON)) // Adding a copy of Obj. The caller is responsible for freeing the Obj.
  else Self.Add(ObjName, Obj); // Adding the Obj object (not a copy). From now the liftime of the Obj is managed by the root JSON object (Self).
end;

procedure TJsonObjectHelper.AddArray(const ArrName: TJSONStringType; Arr: TJSONArray; AddCopyOfArr: Boolean = True);
begin
  if not Assigned(Arr) then Exit;
  if AddCopyOfArr then Self.Add(ArrName, GetJSON(Arr.AsJSON)) // Adding a copy of Arr. The caller is responsible for freeing the Arr.
  else Self.Add(ArrName, Arr); // Adding the Arr array (not a copy). From now the liftime of the Arr is managed by the root JSON object (Self).
end;

function TJsonObjectHelper.NameTypeExists(const Name: string; const JSONType: TJSONType; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  Result := Self.Items[x].JSONType = JSONType;
end;

function TJsonObjectHelper.GetNameTypeIndex(const Name: string; const JSONType: TJSONType; CaseSensitive: Boolean = False): integer;
var
  x: integer;
begin
  Result := -1;
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  if Self.Items[x].JSONType <> JSONType then Exit;
  Result := x;
end;

{$region '         TJsonObjectHelper - TryGetValue          '}

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: TJSONObject; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtObject, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x] as TJSONObject;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: TJSONArray; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtArray, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x] as TJSONArray;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: string; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtString, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsString;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: UnicodeString; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtString, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsUnicodeString;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: integer; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtNumber, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsInteger;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: Int64; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtNumber, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsInt64;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: UInt64; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtNumber, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsQWord;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: Double; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtNumber, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsFloat;
  Result := True;
end;

function TJsonObjectHelper.TryGetValue(const Name: string; out Value: Boolean; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;
  x := GetNameTypeIndex(Name, jtBoolean, CaseSensitive);
  if x < 0 then Exit;
  Value := Self.Items[x].AsBoolean;
  Result := True;
end;

{$endregion TJsonObjectHelper - TryGetValue}

{$region '         TJsonObjectHelper - TryGet[TYPE]         '}

function TJsonObjectHelper.TryGetObject(const Name: string; out JSONObject: TJSONObject; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, JSONObject, CaseSensitive);
end;

function TJsonObjectHelper.TryGetArray(const Name: string; out JSONArray: TJSONArray; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, JSONArray, CaseSensitive);
end;

function TJsonObjectHelper.TryGetString(const Name: string; out StrValue: string; CaseSensitive: Boolean = False): Boolean;
var
  x: integer;
begin
  Result := False;

  x := GetNameTypeIndex(Name, jtString, CaseSensitive);
  if x >= 0 then
  begin
    StrValue := Self.Items[x].AsString;
    Result := True;
    Exit;
  end;

  x := GetNameTypeIndex(Name, jtNumber, CaseSensitive);
  if x >= 0 then
  begin
    //StrValue := FormatFloat('0.0000000000', Self.Items[x].AsFloat);
    StrValue := FloatToStr(Self.Items[x].AsFloat);
    Result := True;
  end;
end;
//begin
//  Result := TryGetValue(Name, StrValue, CaseSensitive);
//end;

function TJsonObjectHelper.TryGetUnicodeString(const Name: string; out StrValue: UnicodeString; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, StrValue, CaseSensitive);
end;

function TJsonObjectHelper.TryGetInteger(const Name: string; out IntValue: integer; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, IntValue, CaseSensitive);
end;

function TJsonObjectHelper.TryGetInt64(const Name: string; out Int64Value: Int64; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, Int64Value, CaseSensitive);
end;

function TJsonObjectHelper.TryGetUInt64(const Name: string; out UInt64Value: UInt64; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, UInt64Value, CaseSensitive);
end;

function TJsonObjectHelper.TryGetFloat(const Name: string; out FloatValue: Double; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, FloatValue, CaseSensitive);
end;

function TJsonObjectHelper.TryGetBool(const Name: string; out BoolValue: Boolean; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetValue(Name, BoolValue, CaseSensitive);
end;

{$endregion TJsonObjectHelper - TryGet[TYPE]}

{$region '         TJsonObjectHelper - Read / Write (INI-like style)         '}

function TJsonObjectHelper.ReadString(const Name, DefResult: string; CaseSensitive: Boolean = False): string;
begin
  if not TryGetString(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteString(const Name, Value: string; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
    if Self.Items[x].JSONType = jtString then Self.Items[x].AsString := Value
    else
    begin
      Self.Delete(x);
      Self.Add(Name, Value);
    end;
end;

function TJsonObjectHelper.ReadUnicodeString(const Name: string; const DefResult: UnicodeString; CaseSensitive: Boolean = False): UnicodeString;
begin
  if not TryGetUnicodeString(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteUnicodeString(const Name: string; const Value: UnicodeString; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
    if Self.Items[x].JSONType = jtString then Self.Items[x].AsUnicodeString := Value
    else
    begin
      Self.Delete(x);
      Self.Add(Name, Value);
    end;
end;

function TJsonObjectHelper.ReadInteger(const Name: string; const DefResult: integer; CaseSensitive: Boolean = False): integer;
begin
  if not TryGetInteger(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteInteger(const Name: string; const Value: integer; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
    //if Self.Items[x].JSONType = jtNumber then Self.Items[x].AsInteger := Value
    //else
    begin
      Self.Delete(x);
      Self.Add(Name, Value);
    end;
end;

function TJsonObjectHelper.ReadInt64(const Name: string; const DefResult: Int64; CaseSensitive: Boolean = False): Int64;
begin
  if not TryGetInt64(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteInt64(const Name: string; const Value: Int64; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
  begin
    Self.Delete(x);
    Self.Add(Name, Value);
  end;
end;

function TJsonObjectHelper.ReadUInt64(const Name: string; const DefResult: UInt64; CaseSensitive: Boolean = False): UInt64;
begin
  if not TryGetUInt64(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteUInt64(const Name: string; const Value: UInt64; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
  begin
    Self.Delete(x);
    Self.Add(Name, Value);
  end;
end;

function TJsonObjectHelper.ReadFloat(const Name: string; const DefResult: Double; CaseSensitive: Boolean = False): Double;
begin
  if not TryGetFloat(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteFloat(const Name: string; const Value: Double; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
  begin
    Self.Delete(x);
    Self.Add(Name, Value);
  end;
end;

function TJsonObjectHelper.ReadBool(const Name: string; const DefResult: Boolean; CaseSensitive: Boolean = False): Boolean;
begin
  if not TryGetBool(Name, Result, CaseSensitive) then Result := DefResult;
end;

procedure TJsonObjectHelper.WriteBool(const Name: string; const Value: Boolean; CaseSensitive: Boolean = False);
var
  x: integer;
begin
  x := Self.IndexOfName(Name, CaseSensitive);
  if x < 0 then Self.Add(Name, Value)
  else
    if Self.Items[x].JSONType = jtBoolean then Self.Items[x].AsBoolean := Value
    else
    begin
      Self.Delete(x);
      Self.Add(Name, Value);
    end;
end;



{$endregion TJsonObjectHelper - Read / Write}


{
function TJSONObject.Get(const AName: String; ADefault: TJSONArray): TJSONArray;
Var
  D : TJSONData;
begin
  D:=Find(AName,jtArray);
  If (D<>Nil) then Result:=TJSONArray(D)
  else Result:=ADefault;
end;
}

function TJsonObjectHelper.GetArray(const AName: string; CaseSensitive: Boolean = False): TJSONArray;
var
  x: integer;
begin
  Result := nil;
  x := Self.IndexOfName(AName, CaseSensitive);
  if x < 0 then Exit;
  if Items[x].JSONType <> jtArray then Exit;
  Result := TJSONArray(Items[x]);
end;

function TJsonObjectHelper.GetObject(const AName: string; CaseSensitive: Boolean = False): TJSONObject;
var
  x: integer;
begin
  Result := nil;
  x := Self.IndexOfName(AName, CaseSensitive);
  if x < 0 then Exit;
  if Items[x].JSONType <> jtObject then Exit;
  Result := TJSONObject(Items[x]);
end;



function TJsonObjectHelper.TryInitFromStr(const JSONSource: TJSONStringType; Silent: Boolean = False): Boolean;
var
  jData: TJSONData;
  Parser: TJSONParser;
  JSONOptions: TJSONOptions;
begin
  Result := False;
  Self.Clear;
  JSONOptions := [joUTF8, joComments, joIgnoreTrailingComma];
  Parser := TJSONParser.Create(JSONSource, JSONOptions);
  try
    try
      jData := Parser.Parse;
    except
      on E: Exception do
        if Silent then Exit
        else raise Exception.Create(E.Message);
    end;
    if jData is TJSONObject then
    begin
      //Self := TJSONObject(jData);

      Self := jData as TJSONObject;
      Result := True;
    end;
  finally
    Parser.Free;
  end;
end;


function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: TJSONObject; CaseSensitive: Boolean = False): Boolean; overload;
var
  x: integer;
begin
  Result := False;
  x := JSONObject.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  if JSONObject.Items[x].JSONType <> jtObject then Exit;
  Value := JSONObject.Items[x] as TJSONObject;
  Result := True;
end;

function TryGetJsonObject(JSONObject: TJSONObject; const Name: string; out NewJsonObject: TJSONObject; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetJsonValue(JSONObject, Name, NewJsonObject, CaseSensitive);
end;

function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: TJSONArray; CaseSensitive: Boolean = False): Boolean; overload;
var
  x: integer;
begin
  Result := False;
  x := JSONObject.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  if JSONObject.Items[x].JSONType <> jtArray then Exit;
  Value := JSONObject.Items[x] as TJSONArray;
  Result := True;
end;

function TryGetJsonArray(JSONObject: TJSONObject; const Name: string; out JSONArray: TJSONArray; CaseSensitive: Boolean = False): Boolean;
begin
  Result := TryGetJsonValue(JSONObject, Name, JSONArray, CaseSensitive);
end;

function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: string; CaseSensitive: Boolean = False): Boolean; overload;
var
  x: integer;
begin
  Result := False;
  x := JSONObject.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  if JSONObject.Types[Name] <> jtString then Exit;
  Value := JSONObject.Get(Name);
  Result := True;
end;

function TryGetJsonValue(JSONObject: TJSONObject; const Name: string; out Value: integer; CaseSensitive: Boolean = False): Boolean; overload;
var
  x: integer;
  s: string;
begin
  Result := False;
  x := JSONObject.IndexOfName(Name, CaseSensitive);
  if x < 0 then Exit;
  if JSONObject.Types[Name] <> jtNumber then Exit;
  s := JSONObject.Get(Name);
  if not TryStrToInt(s, Value) then Exit;
  Result := True;
end;

function GetJSONObjectFromFile(const FileName: string; Silent: Boolean = True): TJSONObject;
var
  sl: TStringList;
begin
  Result := nil;
  if not FileExists(FileName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Result := GetJSONObjectFromStr(sl.Text, Silent);
  finally
    sl.Free;
  end;
end;

function GetJSONObjectFromFile(const FileName: string; var ErrStr: string): TJSONObject;
var
  sl: TStringList;
begin
  Result := nil;
  ErrStr := '';
  if not FileExists(FileName) then
  begin
    ErrStr := 'File not exists: "' + FileName + '"!';
    Exit;
  end;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Result := GetJSONObjectFromStr(sl.Text, ErrStr);
  finally
    sl.Free;
  end;
end;

function GetJSONObjectFromStr(const JSONSource: string; Silent: Boolean = True): TJSONObject;
var
  jData: TJSONData;
  Parser: TJSONParser;
  JSONOptions: TJSONOptions;
begin
  Result := nil;

  JSONOptions := [joUTF8, joComments, joIgnoreTrailingComma];
  Parser := TJSONParser.Create(JSONSource, JSONOptions);
  try
    try
      jData := Parser.Parse;
    except
      on E: Exception do
        if Silent then Exit(nil)
        else raise Exception.Create(E.Message);
    end;
    if jData is TJSONObject then Result := TJSONObject(jData);
  finally
    Parser.Free;
  end;
end;

function GetJSONObjectFromStr(const JSONSource: string; var ErrStr: string): TJSONObject;
var
  jData: TJSONData;
  Parser: TJSONParser;
  JSONOptions: TJSONOptions;
begin
  Result := nil;
  ErrStr := '';

  JSONOptions := [joUTF8, joComments, joIgnoreTrailingComma];
  Parser := TJSONParser.Create(JSONSource, JSONOptions);
  try
    try
      jData := Parser.Parse;
    except
      on E: Exception do ErrStr := E.Message;
        //if Silent then Exit(nil)
        //else raise Exception.Create(E.Message);
    end;
    if jData is TJSONObject then Result := TJSONObject(jData);
  finally
    Parser.Free;
  end;
end;

function GetJSONArrayFromStr(const JSONSource: string; Silent: Boolean): TJSONArray;
var
  jData: TJSONData;
  Parser: TJSONParser;
  JSONOptions: TJSONOptions;
begin
  Result := nil;

  JSONOptions := [joUTF8, joComments, joIgnoreTrailingComma];
  Parser := TJSONParser.Create(JSONSource, JSONOptions);
  try
    try
      jData := Parser.Parse;
    except
      on E: Exception do
        if Silent then Exit(nil)
        else raise Exception.Create(E.Message);
    end;
    if jData is TJSONArray then Result := TJSONArray(jData);
  finally
    Parser.Free;
  end;
end;


end.

