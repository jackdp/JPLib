unit JPL.RTTI;

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

{$IFDEF DCC}
  {$IFDEF DELPHI2009_OR_BELOW}
  Unit for Delphi 2010 or newer!
  {$ENDIF}
  {$DEFINE HAS_RTTI}
{$ENDIF}

interface

uses
  SysUtils, Classes
  {$IFDEF HAS_RTTI}, {%H-}Rtti, TypInfo{$ENDIF}
  ;


{$IFDEF HAS_RTTI}
function GetPropertyAsObject(const Obj: TObject; const PropertyName: string): TObject;
function TryGetPropertyAsObject(const Obj: TObject; const PropertyName: string; out OutObj: TObject): Boolean;

function GetPropertyAsClass(const Obj: TObject; const PropertyName: string): TClass;
function TryGetPropertyAsClass(const Obj: TObject; const PropertyName: string; out OutClass: TClass): Boolean;

function SetPropertyText(Obj: TObject; PropertyName: string; Text: string): Boolean;
function GetPropertyText(Obj: TObject; const PropertyName, Default: string): string;

function GetRttiMethod(Obj: TObject; const MethodName: string): TRttiMethod;
{$ENDIF}

  
implementation

{$IFDEF HAS_RTTI}
function SetPropertyText(Obj: TObject; PropertyName: string; Text: string): Boolean;
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  Kind: TTypeKind;
  UPropName: string;
begin
  Result := False;
  if not Assigned(Obj) then Exit;
  UPropName := UpperCase(PropertyName);

  RContext := TRttiContext.Create;
  try
    RType := RContext.GetType(Obj.ClassType);

    for RProperty in RType.GetProperties do
    begin
      if UpperCase(RProperty.Name) = UPropName then
      begin
        if not RProperty.IsWritable then Exit;
        Kind := RProperty.GetValue(Obj).Kind;

        // http://docwiki.embarcadero.com/Libraries/Rio/en/System.TypInfo.TTypeKinds
        if (Kind = tkUString) or (Kind = tkString) or (Kind = tkLString) then RProperty.SetValue(Obj, Text)
        else if Kind = tkWString then RProperty.SetValue(Obj, WideString(Text){%H-})
        else Exit;

        Result := True;
      end;
    end;

  finally
    RContext.Free;
  end;

end;

function GetPropertyText(Obj: TObject; const PropertyName, Default: string): string;
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  Kind: TTypeKind;
  UPropName: string;
  Value: TValue;
begin
  Result := Default;
  if not Assigned(Obj) then Exit;
  UPropName := UpperCase(PropertyName);

  RContext := TRttiContext.Create;
  try
    RType := RContext.GetType(Obj.ClassType);

    for RProperty in RType.GetProperties do
    begin
      if UpperCase(RProperty.Name) = UPropName then
      begin
        if not RProperty.IsReadable then Exit;
        Kind := RProperty.GetValue(Obj).Kind;
        if (Kind = tkUString) or (Kind = tkString) or (Kind = tkLString) or (Kind = tkWString) then
        begin
          Value := RProperty.GetValue(Obj);
          Result := Value.AsString;
        end;
      end;
    end;

  finally
    RContext.Free;
  end;

end;


function GetPropertyAsObject(const Obj: TObject; const PropertyName: string): TObject;
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  UPropName: string;
begin
  Result := nil;
  if not Assigned(Obj) then Exit;
  UPropName := UpperCase(PropertyName);

  RContext := TRttiContext.Create;
  try
    RType := RContext.GetType(Obj.ClassType);

    for RProperty in RType.GetProperties do
    begin
      if UpperCase(RProperty.Name) = UPropName then
      begin
        {$IFDEF FPC}
        if RProperty.PropertyType.TypeKind = tkClass then Result := RProperty.GetValue(Obj).AsObject;
        {$ELSE}
        if RProperty.PropertyType.TypeKind = tkClass then Result := RProperty.GetValue(Obj).AsType<TObject>;
        {$ENDIF}
        Break;
      end;
    end;

  finally
    RContext.Free;
  end;

end;

function TryGetPropertyAsObject(const Obj: TObject; const PropertyName: string; out OutObj: TObject): Boolean;
begin
  OutObj := GetPropertyAsObject(Obj, PropertyName);
  Result := OutObj <> nil;
end;

function GetPropertyAsClass(const Obj: TObject; const PropertyName: string): TClass;
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  UPropName: string;
begin
  Result := nil;
  if not Assigned(Obj) then Exit;
  UPropName := UpperCase(PropertyName);

  RContext := TRttiContext.Create;
  try
    RType := RContext.GetType(Obj.ClassType);

    for RProperty in RType.GetProperties do
    begin
      if UpperCase(RProperty.Name) = UPropName then
      begin
        if RProperty.PropertyType.TypeKind = tkClass then Result := TRttiInstanceType(RProperty.PropertyType).MetaclassType;
        Break;
      end;
    end;

  finally
    RContext.Free;
  end;

end;

function TryGetPropertyAsClass(const Obj: TObject; const PropertyName: string; out OutClass: TClass): Boolean;
begin
  OutClass := GetPropertyAsClass(Obj, PropertyName);
  Result := OutClass <> nil;
end;

function GetRttiMethod(Obj: TObject; const MethodName: string): TRttiMethod;
var
  RContext: TRttiContext;
  RType: TRttiType;
  Method: TRttiMethod;
  uName: string;
begin
  Result := nil;
  if not Assigned(Obj) then Exit;

  uName := UpperCase(MethodName);

  RContext := TRttiContext.Create;
  try

    RType := RContext.GetType(Obj.ClassType);

    for Method in RType.GetMethods do
    begin
      if UpperCase(Method.Name) = uName then
      begin
        Result := Method;
        Break;
      end;
    end;

  finally
    RContext.Free;
  end;
end;

{$ENDIF} // HAS_RTTI


end.
