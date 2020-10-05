unit JPL.Rects;

{
  Helper record and routines for operations on rectangles.
}

{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses 
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Types;

type

  TRectPos = (
    rpTopLeft, rpTopCenter, rpTopRight,
    rpLeftCenter, rpCenter, rpRightCenter,
    rpBottomLeft, rpBottomCenter, rpBottomRight
  );

  TRectHelper = record helper for TRect
  private
    {$IFNDEF HAS_ADVANCED_TRECT}
    function GetWidth: integer;
    procedure SetWidth(const Value: integer);
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
    {$ENDIF}
  public
    {$IFNDEF HAS_ADVANCED_TRECT}
    constructor Create(const Origin: TPoint); overload; // empty rect at given origin
    constructor Create(const Origin: TPoint; Width, Height: Integer); overload;
    constructor Create(const Left, Top, Right, Bottom: Integer); overload;
    constructor Create(const P1, P2: TPoint; Normalize: Boolean = False); overload;
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    class function Empty: TRect; inline; static;
    procedure NormalizeRect;

    function Contains(const Pt: TPoint): Boolean; overload;
    function Contains(const R: TRect): Boolean; overload;

    class function Union(const R1: TRect; const R2: TRect): TRect; overload; static;
    procedure Union(const R: TRect); overload;
    class function Union(const Points: array of TPoint): TRect; overload; static;

    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;

    procedure SetLocation(const X, Y: Integer); overload;
    procedure SetLocation(const Point: TPoint); overload;

    procedure Inflate(const DX, DY: Integer); overload;
    procedure Inflate(const DL, DT, DR, DB: Integer); overload;
    {$ENDIF}

    function InflatedRect(const dx, dy: integer): TRect; overload;
    function InflatedRect(const Pt: TPoint): TRect; overload;

    class procedure CenterInRect(const MainRect: TRect; var CenteredRect: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True); overload; static;
    procedure CenterInRect(const R: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True); overload;

    class procedure Align(const MainRect: TRect; var AlignedRect: TRect; const RectPos: TRectPos); static;
    procedure AlignInRect(const R: TRect; const RectPos: TRectPos);


    {$IFNDEF HAS_ADVANCED_TRECT}
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    {$ENDIF}
  end;


function RectWidth(const R: TRect): integer;
function RectHeight(const R: TRect): integer;
function PointInRect(const Point: TPoint; const Rect: TRect; IncludeRightAndBottomSides: Boolean = False): Boolean;
procedure CenterRect(const MainRect: TRect; var CenteredRect: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True);
procedure AlignRect(const MainRect: TRect; var AlignedRect: TRect; const RectPos: TRectPos);
procedure InflateRectEx(var ARect: TRect; const DeltaLeft, DeltaRight, DeltaTop, DeltaBottom: integer);
procedure DeflateRect(var ARect: TRect; const DX, DY: integer);
function GetTextMiddlePosY(const R: TRect; const TextHeight: integer): integer;
function GetTextMiddlePosX(const R: TRect; const TextWidth: integer): integer;



implementation


{$Region '                   Helper routines                        '}
function RectWidth(const R: TRect): integer;
begin
  Result := R.Right - R.Left;
end;

function RectHeight(const R: TRect): integer;
begin
  Result := R.Bottom - R.Top;
end;

{
  A point is within a rectangle if it lies on the left or top side or is within all four sides.
  A point on the right or bottom side is considered **outside** the rectangle.
  https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-ptinrect
}
function PointInRect(const Point: TPoint; const Rect: TRect; IncludeRightAndBottomSides: Boolean = False): Boolean;
begin
  if IncludeRightAndBottomSides then
    Result := (Point.X >= Rect.Left) and (Point.X <= Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom)
  else
    Result := (Point.X >= Rect.Left) and (Point.X < Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y < Rect.Bottom);
end;

procedure CenterRect(const MainRect: TRect; var CenteredRect: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True);
var
  xp: integer;
begin
  if CenterHorizontally then
  begin
    xp := MainRect.Left + (MainRect.Width div 2) - (CenteredRect.Width div 2);
    CenteredRect.SetLocation(xp, CenteredRect.Top);
  end;

  if CenterVertically then
  begin
    xp := MainRect.Top + (MainRect.Height div 2) - (CenteredRect.Height div 2);
    CenteredRect.SetLocation(CenteredRect.Left, xp);
  end;
end;

procedure AlignRect(const MainRect: TRect; var AlignedRect: TRect; const RectPos: TRectPos);
var
  x, y: integer;
begin
  x := AlignedRect.Left;
  y := AlignedRect.Top;

  case RectPos of
    rpTopLeft:
      begin
        x := MainRect.Left;
        y := MainRect.Top;
      end;
    rpTopCenter:
      begin
        x := MainRect.Left + (MainRect.Width div 2) - (AlignedRect.Width div 2);
        y := MainRect.Top;
      end;
    rpTopRight:
      begin
        x := MainRect.Left + MainRect.Width - AlignedRect.Width;
        y := MainRect.Top;
      end;
    rpLeftCenter:
      begin
        x := MainRect.Left;
        y := MainRect.Top + (MainRect.Height div 2) - (AlignedRect.Height div 2);
      end;
    rpCenter:
      begin
        x := MainRect.Left + (MainRect.Width div 2) - (AlignedRect.Width div 2);
        y := MainRect.Top + (MainRect.Height div 2) - (AlignedRect.Height div 2);
      end;
    rpRightCenter:
      begin
        x := MainRect.Left + MainRect.Width - AlignedRect.Width;
        y := MainRect.Top + (MainRect.Height div 2) - (AlignedRect.Height div 2);
      end;
    rpBottomLeft:
      begin
        x := MainRect.Left;
        y := MainRect.Top + MainRect.Height - AlignedRect.Height;
      end;
    rpBottomCenter:
      begin
        x := MainRect.Left + (MainRect.Width div 2) - (AlignedRect.Width div 2);
        y := MainRect.Top + MainRect.Height - AlignedRect.Height;
      end;
    rpBottomRight:
      begin
        x := MainRect.Left + MainRect.Width - AlignedRect.Width;
        y := MainRect.Top + MainRect.Height - AlignedRect.Height;
      end;
  end;

  AlignedRect.SetLocation(x, y);
end;

procedure InflateRectEx(var ARect: TRect; const DeltaLeft, DeltaRight, DeltaTop, DeltaBottom: integer);
begin
{
  https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-inflaterect
  The InflateRect function increases or decreases the width and height of the specified rectangle.
  The InflateRect function adds -dx units to the left end and dx to the right end of the rectangle and -dy units
  to the top and dy to the bottom. The dx and dy parameters are signed values; positive values increase the width
  and height, and negative values decrease them.
}
  ARect.Inflate(DeltaLeft, DeltaTop, DeltaRight, DeltaBottom);
end;

procedure DeflateRect(var ARect: TRect; const DX, DY: integer);
begin
  InflateRect(ARect, -DX, -DY);
end;

function GetTextMiddlePosY(const R: TRect; const TextHeight: integer): integer;
begin
  Result := R.Top + (RectHeight(R) div 2) - (TextHeight div 2);
end;

function GetTextMiddlePosX(const R: TRect; const TextWidth: integer): integer;
begin
  Result := R.Left + (RectWidth(R) div 2) - (TextWidth div 2);
end;
{$endregion Helper routines}



{$region '               TRectHelper                '}

{$IFNDEF HAS_ADVANCED_TRECT}

constructor TRectHelper.Create(const Origin: TPoint; Width, Height: Integer);
begin
  Create(Origin.X, Origin.Y, Origin.X + Width, Origin.Y + Height);
end;

constructor TRectHelper.Create(const Origin: TPoint);
begin
  Create(Origin.X, Origin.Y, Origin.X, Origin.Y);
end;

constructor TRectHelper.Create(const Left, Top, Right, Bottom: Integer);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

constructor TRectHelper.Create(const R: TRect; Normalize: Boolean);
begin
  Self.TopLeft := R.TopLeft;
  Self.BottomRight := R.BottomRight;
  if Normalize then Self.NormalizeRect;
end;

constructor TRectHelper.Create(const P1, P2: TPoint; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then Self.NormalizeRect;
end;

class function TRectHelper.Empty: TRect;
begin
  Result := TRect.Create(0, 0, 0, 0);
end;

procedure TRectHelper.NormalizeRect;
begin
  if Top > Bottom then
  begin
    Top := Top xor Bottom;
    Bottom := Top xor Bottom;
    Top := Top xor Bottom;
  end;
  if Left > Right then
  begin
    Left := Left xor Right;
    Right:= Left xor Right;
    Left := Left xor Right;
  end
end;

function TRectHelper.Contains(const Pt: TPoint): Boolean;
begin
  Result := PointInRect(Pt, Self);
end;

function TRectHelper.Contains(const R: TRect): Boolean;
begin
  Result := Contains(R.TopLeft) and Contains(R.BottomRight);
end;

class function TRectHelper.Union(const R1, R2: TRect): TRect;
begin
  UnionRect(Result, R1, R2);
end;

procedure TRectHelper.Union(const R: TRect);
begin
  Self := Union(Self, R);
end;

class function TRectHelper.Union(const Points: array of TPoint): TRect;
var
  i: Integer;
begin
  if Length(Points) > 0 then
  begin
    Result.TopLeft := Points[Low(Points)];
    Result.BottomRight := Points[Low(Points)];

    for i := Low(Points) + 1 to High(Points) do
    begin
      if Points[i].X < Result.Left then Result.Left := Points[i].X;
      if Points[i].X > Result.Right then Result.Right := Points[i].X;
      if Points[i].Y < Result.Top then Result.Top := Points[i].Y;
      if Points[i].Y > Result.Bottom then Result.Bottom := Points[i].Y;
    end;
  end
  else
    Result := Empty;
end;

procedure TRectHelper.Offset(const DX, DY: Integer);
begin
  Inc(Left, DX);
  Inc(Right, DX);
  Inc(Top, DY);
  Inc(Bottom, DY);
end;

procedure TRectHelper.Offset(const Point: TPoint);
begin
  Inc(Left, Point.X);
  Inc(Right, Point.X);
  Inc(Top, Point.Y);
  Inc(Bottom, Point.Y);
end;

procedure TRectHelper.SetLocation(const X, Y: Integer);
begin
  Offset(X - Left, Y - Top);
end;

procedure TRectHelper.SetLocation(const Point: TPoint);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

procedure TRectHelper.Inflate(const DX, DY: Integer);
begin
  Dec(Left, DX);
  Dec(Top, DY);
  Inc(Right, DX);
  Inc(Bottom, DY);
end;

procedure TRectHelper.Inflate(const DL, DT, DR, DB: Integer);
begin
  Dec(Left, DL);
  Dec(Top, DT);
  Inc(Right, DR);
  Inc(Bottom, DB);
end;

function TRectHelper.GetWidth: integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectHelper.SetWidth(const Value: integer);
begin
  Self.Right := Self.Left + Value;
end;

function TRectHelper.GetHeight: integer;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectHelper.SetHeight(const Value: integer);
begin
  Self.Bottom := Self.Top + Value;
end;


{$ENDIF} // HAS_ADVANCED_TRECT


function TRectHelper.InflatedRect(const dx, dy: integer): TRect;
begin
  Result.Left := Left - dx;
  Result.Right := Right + dx;
  Result.Top := Top - dy;
  Result.Bottom := Bottom + dy;
end;

function TRectHelper.InflatedRect(const Pt: TPoint): TRect;
begin
  Result := InflatedRect(Pt.X, Pt.Y);
end;

procedure TRectHelper.CenterInRect(const R: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True);
begin
  CenterRect(R, Self, CenterHorizontally, CenterVertically);
end;

class procedure TRectHelper.CenterInRect(const MainRect: TRect; var CenteredRect: TRect; CenterHorizontally: Boolean = True; CenterVertically: Boolean = True);
begin
  CenterRect(MainRect, CenteredRect, CenterHorizontally, CenterVertically);
end;

procedure TRectHelper.AlignInRect(const R: TRect; const RectPos: TRectPos);
begin
  AlignRect(R, Self, RectPos);
end;

class procedure TRectHelper.Align(const MainRect: TRect; var AlignedRect: TRect; const RectPos: TRectPos);
begin
  AlignRect(MainRect, AlignedRect, RectPos);
end;

{$endregion TRectHelper}




end.