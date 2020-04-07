unit JPL.RTTI;

{$IFDEF FPC} {$mode delphi} {$I JppFPC.inc} {$ENDIF}

{$IFDEF DCC}{$DEFINE HAS_RTTI}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF DCC}
  System.SysUtils, System.Classes,
  System.Rtti, System.TypInfo;
  {$ELSE}
  SysUtils, Classes {$IFDEF HAS_RTTI}, Rtti, TypInfo{$ENDIF};
  {$ENDIF}



{$IFDEF HAS_RTTI}
function GetPropertyAsObject(const Obj: TObject; const PropertyName: string): TObject;
function TryGetPropertyAsObject(const Obj: TObject; const PropertyName: string; out OutObj: TObject): Boolean;

function GetPropertyAsClass(const Obj: TObject; const PropertyName: string): TClass;
function TryGetPropertyAsClass(const Obj: TObject; const PropertyName: string; out OutClass: TClass): Boolean;

function SetPropertyText(Obj: TObject; PropertyName: string; Text: string): Boolean;
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

//        if (Kind <> tkUString) and (Kind <> tkWString) then Exit;
//        RProperty.SetValue(Obj, Text);

        // http://docwiki.embarcadero.com/Libraries/Rio/en/System.TypInfo.TTypeKinds
        if (Kind = tkUString) or (Kind = tkString) or (Kind = tkLString) then RProperty.SetValue(Obj, Text)
        else if Kind = tkWString then RProperty.SetValue(Obj, WideString(Text))
        else Exit;

        Result := True;
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

{$ENDIF} // HAS_RTTI


end.
