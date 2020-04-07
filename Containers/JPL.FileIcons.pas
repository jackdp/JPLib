unit JPL.FileIcons;

interface

{
  Dla każdego rozszerzenia, oprócz EXE, dodawana jest tylko jedna ikona.
  Aby pobrać indeks ikony danego pliku, wywołać GetFileIconIndex.
  Jeśli zwrócona wartość będzie < 0, oznacza to, że nie udało się pobrać ikony danego pliku.
  Gdy zwrócona wartość >= 0, to jest ona indeksem ikony w wewnętrznej liście FImageList i można jej użyc
  w komponentach obsługujących TImageList.
}

uses
  // Win API
  Windows, Messages, ShellAPI,

  // System
  SysUtils, Classes, Generics.Collections,

  // VCL
  Graphics, Controls, ComCtrls, StdCtrls, ExtCtrls,

  // JPLib
  JPL.Strings, JPL.Conversion;


type

  TIconList = TDictionary<string, integer>;


  TFileIcons = class(TObject)
  private
    FIconList: TIconList;
    FImageList: TImageList;
    FMaxIconCount: integer;
    procedure SetMaxIconCount(const Value: integer);
  public
    constructor Create(ImageList: TImageList; xMaxIconCount: integer = -1);
    destructor Destroy; override;
    procedure ClearAll;
    function GetFileIconIndex(const fName: string): integer;
    function AddIcon(Ext: string; Icon: TIcon): integer;
    function IconCount: integer;

    property MaxIconCount: integer read FMaxIconCount write SetMaxIconCount;
  end;


implementation



constructor TFileIcons.Create(ImageList: TImageList; xMaxIconCount: integer = -1);
begin
  inherited Create;
  FIconList := TIconList.Create;
  FImageList := ImageList;
  FMaxIconCount := xMaxIconCount;
end;

destructor TFileIcons.Destroy;
begin
  FIconList.Free;
  inherited;
end;

procedure TFileIcons.ClearAll;
begin
  FImageList.Clear;
  FIconList.Clear;
end;

function TFileIcons.AddIcon(Ext: string; Icon: TIcon): integer;
var
  IconIndex: integer;
begin
  Result := -1;
  if not Assigned(Icon) then Exit;
  Ext := TrimUp(Ext);
  if Copy(Ext, 1, 1) <> '.' then Ext := '.' + Ext;

  if FIconList.TryGetValue(Ext, IconIndex) then Result := IconIndex
  else
  begin
    IconIndex := FImageList.AddIcon(Icon);
    FIconList.AddOrSetValue(Ext, IconIndex);
    Result := IconIndex;
  end;
end;

function TFileIcons.GetFileIconIndex(const fName: string): integer;
var
  Ext: string;
  IconIndex: integer;
  Icon: TIcon;
  SHFileInfo: TSHFileInfo;
  bExe: Boolean;
begin
  Result := -1;
  if FMaxIconCount > 0 then
    if FIconList.Count >= FMaxIconCount then Exit;

  Ext := TrimUp(ExtractFileExt(fName));
  bExe := Ext = '.EXE';
  if bExe then Ext := fName;

  if FIconList.TryGetValue(Ext, IconIndex) then Result := IconIndex

  else

  begin

    FIconList.Add(Ext, -1);

    try

      Icon := TIcon.Create;
      try
        ShGetFileInfo(PChar(fName), 0, SHFileInfo, SizeOf(TSHFileInfo), SHGFI_SMALLICON or SHGFI_ICON);
        if SHFileInfo.hIcon > 0 then
        begin
          Icon.Handle := SHFileInfo.hIcon;
          IconIndex := FImageList.AddIcon(Icon);
          FIconList.AddOrSetValue(Ext, IconIndex);
          Result := IconIndex;
        end;
      finally
        Icon.Free;
      end;

    except
      // no exceptions
    end;

  end;

end;




function TFileIcons.IconCount: integer;
begin
  Result := FIconList.Count;
end;

procedure TFileIcons.SetMaxIconCount(const Value: integer);
begin
  FMaxIconCount := Value;
end;

end.

