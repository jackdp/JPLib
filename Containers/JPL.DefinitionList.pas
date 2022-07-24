unit JPL.DefinitionList;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  A simple class for managing definition list.
  Definition item: term + description.

  Designed for small console applications.
  No Classes, Generics.Collections etc.
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}


interface

uses
  SysUtils, JPL.Strings;


type

  TJPDefinitionItem = record
    Term: string;
    Description: string;
    procedure Clear;
    procedure Assign(Src: TJPDefinitionItem);
  end;

  TJPDefinitionArray = array of TJPDefinitionItem;

  TJPDefinitionList = class
  private
    FArr: TJPDefinitionArray;
    FDescriptionPadding: integer;
    FMaxDescriptionLineLength: integer;
    FMaxTermLineLength: integer;
    FTermPadding: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;

    procedure Add(const Term, Description: string);
    function AsString(EmptyLinesBetweenItems: integer = 0): string;

    property TermPadding: integer read FTermPadding write FTermPadding;
    property DescriptionPadding: integer read FDescriptionPadding write FDescriptionPadding;
    property MaxTermLineLength: integer read FMaxTermLineLength write FMaxTermLineLength;
    property MaxDescriptionLineLength: integer read FMaxDescriptionLineLength write FMaxDescriptionLineLength;
  end;



implementation



constructor TJPDefinitionList.Create;
begin
  inherited Create;
  SetLength(FArr, 0);
  FTermPadding := 0;
  FDescriptionPadding := 2;
  FMaxTermLineLength := 50;
  FMaxDescriptionLineLength := 80;
end;

destructor TJPDefinitionList.Destroy;
begin
  ClearAll;
  inherited Destroy;
end;

procedure TJPDefinitionList.ClearAll;
var
  i: integer;
begin
  for i := 0 to Length(FArr) - 1 do FArr[i].Clear;
  SetLength(FArr, 0);
end;

procedure TJPDefinitionList.Add(const Term, Description: string);
var
  Ind: integer;
begin
  SetLength(FArr, Length(FArr) + 1);
  Ind := Length(FArr) - 1;
  FArr[Ind].Term := Term;
  FArr[Ind].Description := Description;
end;

function TJPDefinitionList.AsString(EmptyLinesBetweenItems: integer = 0): string;
var
  i, x: integer;
  Term, Desc: string;
begin
  Result := '';

  for i := 0 to High(FArr) do
  begin

    Term := FArr[i].Term;
    Desc := FArr[i].Description;

    Result := Result +
      SplitText(Term, FMaxTermLineLength, FTermPadding, False) + ENDL +
      SplitText(Desc, FMaxDescriptionLineLength, FDescriptionPadding, True) + ENDL;

    if EmptyLinesBetweenItems > 0 then
      for x := 1 to EmptyLinesBetweenItems do
        Result := Result + ENDL;

  end;

  Result := Trim(Result);
end;


{ TJPDefinitionItem }

procedure TJPDefinitionItem.Clear;
begin
  Term := '';
  Description := '';
end;

procedure TJPDefinitionItem.Assign(Src: TJPDefinitionItem);
begin
  Term := Src.Term;
  Description := Src.Description;
end;


end.

