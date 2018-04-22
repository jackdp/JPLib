unit JP.StrList.MFPC.Helper;

{
  Jacek Pazera
  http://www.pazera-software.com
  Last mod: 2018.02.11

}



{$mode objfpc}{$H+}

interface

uses
  //LazUTF8,  
  JPL.StrList,
  SysUtils,
  MFPC.Classes.SHARED,
  MFPC.Classes.Streams,
  JPL.Strings,
  JPL.StrHash;


type

  TJPStrListHelper = class helper for TJPStrList
    procedure SaveToStream(AStream: TM_Stream);
    procedure LoadFromStream(const AStream: TM_Stream);
  end;

implementation



procedure TJPStrListHelper.SaveToStream(AStream: TM_Stream);
var
  sLineEnd: string;
  ss: TM_StringStream;
begin
  sLineEnd := Self.EndOfLineMarker;
  Self.EndOfLineMarker := sLineBreak;
  try
    ss := TM_StringStream.Create;
    try
      ss.WriteString(Self.Text);
      ss.SaveToStream(AStream);
    finally
      ss.Free;
    end;
  finally
    Self.EndOfLineMarker := sLineEnd;
  end;
end;

procedure TJPStrListHelper.LoadFromStream(const AStream: TM_Stream);
var
  sLineEnd: string;
  ss: TM_StringStream;
begin
  sLineEnd := Self.EndOfLineMarker;
  Self.EndOfLineMarker := sLineBreak;
  try
    ss := TM_StringStream.Create;
    try
      ss.LoadFromStream(AStream);
      Self.Text := ss.DataString;
    finally
      ss.Free;
    end;
  finally
    Self.EndOfLineMarker := sLineEnd;
  end;
end;

end.
