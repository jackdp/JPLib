unit JPL.JsonDataObjects;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  Helper class and routines for JsonDataObjects unit from https://github.com/ahausladen/JsonDataObjects

  2022.06
}

interface

uses
  Classes, SysUtils,
  JsonDataObjects
  ;

type

  TJPJsonDataObjectHelper = class helper for TJsonObject
  private
  public
    function NameTypeExists(const Name: string; const JsonDataType: TJsonDataType): Boolean;
    function TryGetObject(const Name: string; var JsonObject: TJsonObject): Boolean;
    function TryGetArray(const Name: string; var JsonArray: TJsonArray): Boolean;

    procedure AddObject(const ObjName: string; Obj: TJsonObject; AddCopyOfObj: Boolean = True);
    procedure AddArray(const ArrName: string; Arr: TJsonArray; AddCopyOfArr: Boolean = True);

    function GetJsonObject(const Name: string; CreateIfNotExists: Boolean = True): TJsonObject;
    function GetJsonArray(const Name: string; CreateIfNotExists: Boolean = True): TJsonArray;

    function ReadBool(const Name: string; const Default: Boolean): Boolean;
    procedure WriteBool(const Name: string; const Value: Boolean);

    function ReadString(const Name, Default: string): string;
    procedure WriteString(const Name, Value: string);
    function TryGetAsString(const Name: string; var Value: string): Boolean;

    function ReadInteger(const Name: string; const Default: integer): integer;
    procedure WriteInteger(const Name: string; const Value: integer);

    function ReadInt64(const Name: string; const Default: Int64): Int64;
    procedure WriteInt64(const Name: string; const Value: Int64);

    function ReadUInt64(const Name: string; const Default: UInt64): UInt64;
    procedure WriteUInt64(const Name: string; const Value: UInt64);

    function ReadFloat(const Name: string; const Default: Double): Double;
    procedure WriteFloat(const Name: string; const Value: Double);

    function ReadDateTime(const Name: string; const Default: TDateTime): TDateTime;
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    function TryGetAsDateTime(const Name: string; var Value: TDateTime): Boolean;
  end;


function GetJSONObjectFromStr(const JSONSource: string; Silent: Boolean = True): TJSONObject; overload;
function GetJSONObjectFromStr(const JSONSource: string; var ErrStr: string): TJSONObject; overload;
function IsNumberDataType(const AJsonDataType: TJsonDataType): Boolean;


implementation


//  TJsonDataType = (jdtNone, jdtString, jdtInt, jdtLong, jdtULong, jdtFloat, jdtDateTime, jdtBool, jdtArray, jdtObject);


function GetJSONObjectFromStr(const JSONSource: string; Silent: Boolean = True): TJSONObject; overload;
begin
  Result := TJsonObject.Create;
  try
    Result.FromJSON(JSONSource);
  except
    on E: Exception do
    begin
      if Assigned(Result) then Result.Free;
      if Silent then Exit
      else
        raise Exception.Create(E.Message);
    end;
  end;
end;

function GetJSONObjectFromStr(const JSONSource: string; var ErrStr: string): TJSONObject;
begin
  Result := TJsonObject.Create;
  try
    Result.FromJSON(JSONSource);
  except
    on E: Exception do
    begin
      if Assigned(Result) then Result.Free;
      ErrStr := E.Message;
    end;
  end;
end;

function IsNumberDataType(const AJsonDataType: TJsonDataType): Boolean;
begin
  Result :=
    (AJsonDataType = jdtInt) or (AJsonDataType = jdtLong) or (AJsonDataType = jdtULong) or
    (AJsonDataType = jdtFloat);
end;


{$region '                  TJPJsonDataObjectHelper                         ' }

function TJPJsonDataObjectHelper.NameTypeExists(const Name: string; const JsonDataType: TJsonDataType): Boolean;
begin
  if not Contains(Name) then Exit(False);
  if Types[Name] <> JsonDataType then Exit(False);
  Result := True;
end;

function TJPJsonDataObjectHelper.TryGetObject(const Name: string; var JsonObject: TJsonObject): Boolean;
begin
  if NameTypeExists(Name, jdtObject) then
  begin
    JsonObject := O[Name];
    Result := True;
  end
  else Result := False;
end;

function TJPJsonDataObjectHelper.TryGetArray(const Name: string; var JsonArray: TJsonArray): Boolean;
begin
  if NameTypeExists(Name, jdtArray) then
  begin
    JsonArray := A[Name];
    Result := True;
  end
  else Result := False;
end;

//----------------------------------------------------------------------------

procedure TJPJsonDataObjectHelper.AddArray(const ArrName: string; Arr: TJsonArray; AddCopyOfArr: Boolean = True);
begin
  Self.A[ArrName].Assign(Arr);
  if not AddCopyOfArr then Arr.Free;
end;

procedure TJPJsonDataObjectHelper.AddObject(const ObjName: string; Obj: TJsonObject; AddCopyOfObj: Boolean = True);
begin
  Self.O[ObjName].Assign(Obj);
  if not AddCopyOfObj then Obj.Free;
end;

function TJPJsonDataObjectHelper.GetJsonArray(const Name: string; CreateIfNotExists: Boolean): TJsonArray;
begin
  if NameTypeExists(Name, jdtArray) then Result := A[Name]
  else
    if CreateIfNotExists then Result := A[Name]
    else Result := nil;
end;

function TJPJsonDataObjectHelper.GetJsonObject(const Name: string; CreateIfNotExists: Boolean = True): TJsonObject;
begin
  if NameTypeExists(Name, jdtObject) then Result := O[Name]
  else
    if CreateIfNotExists then Result := O[Name]
    else Result := nil;
end;

//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadBool(const Name: string; const Default: Boolean): Boolean;
begin
  if NameTypeExists(Name, jdtBool) then Result := B[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteBool(const Name: string; const Value: Boolean);
begin
  B[Name] := Value;
end;

//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadString(const Name, Default: string): string;
begin
  if NameTypeExists(Name, jdtString) then Result := S[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteString(const Name, Value: string);
begin
  S[Name] := Value;
end;

function TJPJsonDataObjectHelper.TryGetAsString(const Name: string; var Value: string): Boolean;
begin
  if NameTypeExists(Name, jdtString) then
  begin
    Value := S[Name];
    Result := True;
  end
  else Result := False;
end;
//----------------------------------------------------------------------------

function TJPJsonDataObjectHelper.ReadInteger(const Name: string; const Default: integer): integer;
begin
  if NameTypeExists(Name, jdtInt) then Result := I[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteInteger(const Name: string; const Value: integer);
begin
  I[Name] := Value;
end;
//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadInt64(const Name: string; const Default: Int64): Int64;
begin
  if NameTypeExists(Name, jdtLong) then Result := L[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteInt64(const Name: string; const Value: Int64);
begin
  L[Name] := Value;
end;
//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadUInt64(const Name: string; const Default: UInt64): UInt64;
begin
  if NameTypeExists(Name, jdtULong) then Result := U[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteUInt64(const Name: string; const Value: UInt64);
begin
  U[Name] := Value;
end;
//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadFloat(const Name: string; const Default: Double): Double;
begin
  if NameTypeExists(Name, jdtFloat) then Result := F[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteFloat(const Name: string; const Value: Double);
begin
  F[Name] := Value;
end;
//----------------------------------------------------------------------------
function TJPJsonDataObjectHelper.ReadDateTime(const Name: string; const Default: TDateTime): TDateTime;
begin
  //if NameTypeExists(Name, jdtDateTime) then Result := D[Name] else Result := Default;
  if NameTypeExists(Name, jdtString) then Result := D[Name] else Result := Default;
end;

procedure TJPJsonDataObjectHelper.WriteDateTime(const Name: string; const Value: TDateTime);
begin
  D[Name] := Value;
end;

function TJPJsonDataObjectHelper.TryGetAsDateTime(const Name: string; var Value: TDateTime): Boolean;
begin
  //if NameTypeExists(Name, jdtDateTime) then
  if NameTypeExists(Name, jdtString) then
  begin
    Value := D[Name];
    Result := True;
  end
  else Result := False;
end;


{$endregion TJPJsonDataObjectHelper}

end.
