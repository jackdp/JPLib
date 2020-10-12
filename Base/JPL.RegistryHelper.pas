unit JPL.RegistryHelper;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses 
  Windows, SysUtils, Classes, Registry;


{
Lazarus
TRegDataType = (rdUnknown, rdString, rdExpandString, rdBinary, rdInteger, rdIntegerBigEndian,
                rdLink, rdMultiString, rdResourceList, rdFullResourceDescriptor,  rdResourceRequirementList, rdInt64);

XE2, Rio
TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);
}

type

  TRegistryHelper = class helper for TRegistry
    function ValueTypeExists(const ValueName: string; DataType: TRegDataType): Boolean;
    function TryGetString(const ValueName: string; out StrValue: string): Boolean;

    // Returns True if:   int > 0    OR   string = '1'   OR   UpperCase(string) = 'TRUE'
    // Returns False if:  int <= 0   OR   string = '0'   OR   UpperCase(string) = 'FALSE'
    function TryGetBool(const ValueName: string; out BoolValue: Boolean): Boolean;
  end;



implementation


function TRegistryHelper.ValueTypeExists(const ValueName: string; DataType: TRegDataType): Boolean;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;
  if GetDataType(ValueName) <> DataType then Exit;
  Result := True;
end;

function TRegistryHelper.TryGetBool(const ValueName: string; out BoolValue: Boolean): Boolean;
var
  DataType: TRegDataType;
  x: integer;
  s: string;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;

  DataType := GetDataType(ValueName);

  if DataType = TRegDataType.rdInteger then
  begin
    x := ReadInteger(ValueName);
    BoolValue := x > 0;
    Result := True;
  end

  else if DataType = TRegDataType.rdString then
  begin
    s := ReadString(ValueName);
    s := Trim(UpperCase(s));

    if (s = '1') or (s = 'TRUE') then
    begin
      BoolValue := True;
      Exit(True);
    end
    else if (s = '0') or (s = 'FALSE') then
    begin
      BoolValue := False;
      Exit(True);
    end;
  end;

end;

function TRegistryHelper.TryGetString(const ValueName: string; out StrValue: string): Boolean;
begin
  Result := False;
  if not ValueExists(ValueName) then Exit;
  if GetDataType(ValueName) <> TRegDataType.rdString then Exit;
  StrValue := ReadString(ValueName);
  Result := True;
end;


end.
