unit JPL.ImageManip.Classes;

{

  Delphi GDI+ Library 1.2.1
  https://github.com/jackdp/GDIPlus-Library-for-Delphi-and-Lazarus

  //http://www.bilsen.com/gdiplus/index.shtml
  //https://sourceforge.net/projects/delphigdiplus/

  Vampyre Imaging Library
  https://github.com/galfar/imaginglib

  GDI+ Effects
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/

  --------------------------------------------------------------
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
}


{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}


interface


uses
  Windows, SysUtils, Classes, //Graphics,
  //Generics.Collections,

  // Vampyre Imaging
  //Imaging, ImagingClasses, ImagingTypes, ImagingComponents, ImagingCanvases, ImagingTiff, ImagingExtras,

  // Delphi GDI+
  GdiPlus, GdiPlusHelpers,

  // JPLib
  //JPL.Strings, JPL.TStr,
  JPL.Conversion, //JPL.Math, //JPL.TimeLogger, JPL.Win.Dialogs,

  JPL.ImageManip.Base;


type

  TIMBaseManipulation = class abstract
  private
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName write FName;
  end;


  TIMBrightnessContrastManipulation = class(TIMBaseManipulation)
  private
    FBrightness: SmallInt;
    FContrast: SmallInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetBrightness(const Value: SmallInt);
    procedure SetContrast(const Value: SmallInt);
  public
    constructor Create(const ABrightness, AContrast: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
    property Brightness: SmallInt read FBrightness write SetBrightness;
    property Contrast: SmallInt read FContrast write SetContrast;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMBlurManipulation = class(TIMBaseManipulation)
  private
    FRadius: Single;
    FExpandEdge: Boolean;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetRadius(const Value: Single);
  public
    constructor Create(const ARadius: Single; AExpandEdge: Boolean = False; ACount: Word = 1; ARectPtr: PRect = nil);
    property Radius: Single read FRadius write SetRadius;
    property ExpandEdge: Boolean read FExpandEdge write FExpandEdge;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMSharpenManipulation = class(TIMBaseManipulation)
  private
    FRadius: Single;
    FAmount: Single;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetRadius(const Value: Single);
    procedure SetAmount(const Value: Single);
  public
    constructor Create(const ARadius, AAmount: Single; ACount: Word = 1; ARectPtr: PRect = nil);
    property Radius: Single read FRadius write SetRadius;
    property Amount: Single read FAmount write SetAmount;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMHueSaturationLightnessManipulation = class(TIMBaseManipulation)
  private
    FHue: SmallInt;
    FSaturation: SmallInt;
    FLightness: SmallInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetHue(const Value: SmallInt);
    procedure SetSaturation(const Value: SmallInt);
    procedure SetLightness(const Value: SmallInt);
  public
    constructor Create(const AHue, ASaturation, ALightness: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
    property Hue: SmallInt read FHue write SetHue;
    property Saturation: SmallInt read FSaturation write SetSaturation;
    property Lightness: SmallInt read FLightness write SetLightness;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMColorBalanceManipulation = class(TIMBaseManipulation)
  private
    FCyanRed: ShortInt;
    FMagentaGreen: ShortInt;
    FYellowBlue: ShortInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetCyanRed(const Value: ShortInt);
    procedure SetMagentaGreen(const Value: ShortInt);
    procedure SetYellowBlue(const Value: ShortInt);
  public
    constructor Create(const ACyanRed, AMagentaGreen, AYellowBlue: ShortInt; ACount: Word = 1; ARectPtr: PRect = nil);
    property CyanRed: ShortInt read FCyanRed write SetCyanRed;
    property MagentaGreen: ShortInt read FMagentaGreen write SetMagentaGreen;
    property YellowBlue: ShortInt read FYellowBlue write SetYellowBlue;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMTintManipulation = class(TIMBaseManipulation)
  private
    FHue: SmallInt;
    FAmount: SmallInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetHue(const Value: SmallInt);
    procedure SetAmount(const Value: SmallInt);
  public
    constructor Create(const AHue, AAmount: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
    property Hue: SmallInt read FHue write SetHue;
    property Amount: SmallInt read FAmount write SetAmount;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMLevelsManipulation = class(TIMBaseManipulation)
  private
    FHighlight: ShortInt;
    FMidtone: ShortInt;
    FShadow: ShortInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetHighlight(const Value: ShortInt);
    procedure SetMidtone(const Value: ShortInt);
    procedure SetShadow(const Value: ShortInt);
  public
    constructor Create(const AHighlight, AMidtone, AShadow: ShortInt; ACount: Word = 1; ARectPtr: PRect = nil);
    property Highlight: ShortInt read FHighlight write SetHighlight;
    property Midtone: ShortInt read FMidtone write SetMidtone;
    property Shadow: ShortInt read FShadow write SetShadow;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  {$region '   GDI+ Color curve   '}
  TIMColorCurveManipulation = class(TIMBaseManipulation)
  private
    FAdjustment: TGPCurveAdjustments;
    FCurveChannel: TGPCurveChannel;
    FAdjustValue: SmallInt;
    FCount: Word;
    FRectPtr: PRect;
    procedure SetAdjustValue(const Value: SmallInt);
  public
    constructor Create(const AName: string; const AAdjustment: TGPCurveAdjustments; AAdjustValue: SmallInt;
      ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
    property Adjustment: TGPCurveAdjustments read FAdjustment write FAdjustment;
    property CurveChannel: TGPCurveChannel read FCurveChannel write FCurveChannel;
    property AdjustValue: SmallInt read FAdjustValue write SetAdjustValue;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;

  TIMColorCurveManipulation_Exposure = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_Density = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_Contrast = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_Highlight = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_Shadow = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_Midtone = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_WhiteSaturation = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;

  TIMColorCurveManipulation_BlackSaturation = class(TIMColorCurveManipulation)
  public
    constructor Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
  end;
  {$endregion GDI+ Color curve}

  TIMColorMatrixManipulation = class(TIMBaseManipulation)
  private
    FCount: Word;
    FRectPtr: PRect;
    FColorMatrix: TGPColorMatrix;
  public
    constructor Create(const AColorMatrix: TGPColorMatrix; ACount: Word = 1; ARectPtr: PRect = nil);
    property ColorMatrix: TGPColorMatrix read FColorMatrix write FColorMatrix;
    property Count: Word read FCount write FCount;
    property RectPtr: PRect read FRectPtr write FRectPtr;
  end;


implementation



{ TIMBaseManipulation }

constructor TIMBaseManipulation.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

destructor TIMBaseManipulation.Destroy;
begin
  inherited;
end;



{ TIMBrightnessContrastManipulation }

constructor TIMBrightnessContrastManipulation.Create(const ABrightness, AContrast: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Brightness / Contrast');
  SetBrightness(ABrightness);
  SetContrast(AContrast);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMBrightnessContrastManipulation.SetBrightness(const Value: SmallInt);
begin
  FBrightness := GetIntInRange(Value, IM_BRIGHTNESS_MIN, IM_BRIGHTNESS_MAX);
end;

procedure TIMBrightnessContrastManipulation.SetContrast(const Value: SmallInt);
begin
  FContrast := GetIntInRange(Value, IM_CONTRAST_MIN, IM_CONTRAST_MAX);
end;



{ TIMBlurManipulation }

constructor TIMBlurManipulation.Create(const ARadius: Single; AExpandEdge: Boolean = False; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Blur');
  SetRadius(ARadius);
  FExpandEdge := AExpandEdge;
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMBlurManipulation.SetRadius(const Value: Single);
begin
  FRadius := GetFloatInRange(Value, IM_BLUR_RADIUS_MIN, IM_BLUR_RADIUS_MAX);
end;



{ TIMSharpenManipulation }

constructor TIMSharpenManipulation.Create(const ARadius, AAmount: Single; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Sharpen');
  SetRadius(ARadius);
  SetAmount(AAmount);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMSharpenManipulation.SetRadius(const Value: Single);
begin
  FRadius := GetFloatInRange(Value, IM_SHARPEN_RADIUS_MIN, IM_SHARPEN_RADIUS_MAX);
end;

procedure TIMSharpenManipulation.SetAmount(const Value: Single);
begin
  FAmount := GetFloatInRange(Value, IM_SHARPEN_AMOUNT_MIN, IM_SHARPEN_AMOUNT_MAX);
end;


{ TIMHueSaturationLightnessManipulation }

constructor TIMHueSaturationLightnessManipulation.Create(const AHue, ASaturation, ALightness: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ HSL (Hue, Saturation, Lightness)');
  SetHue(AHue);
  SetSaturation(ASaturation);
  SetLightness(ALightness);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMHueSaturationLightnessManipulation.SetHue(const Value: SmallInt);
begin
  FHue := GetIntInRange(Value, IM_HSL_HUE_MIN, IM_HSL_HUE_MAX);
end;

procedure TIMHueSaturationLightnessManipulation.SetSaturation(const Value: SmallInt);
begin
  FSaturation := GetIntInRange(Value, IM_HSL_SATURATION_MIN, IM_HSL_SATURATION_MAX);
end;

procedure TIMHueSaturationLightnessManipulation.SetLightness(const Value: SmallInt);
begin
  FLightness := GetIntInRange(Value, IM_HSL_LIGHTNESS_MIN, IM_HSL_LIGHTNESS_MAX);
end;



{ TIMColorBalanceManipulation }

constructor TIMColorBalanceManipulation.Create(const ACyanRed, AMagentaGreen, AYellowBlue: ShortInt; ACount: Word; ARectPtr: PRect);
begin
  inherited Create('GDI+ Color balance');
  SetCyanRed(ACyanRed);
  SetMagentaGreen(AMagentaGreen);
  SetYellowBlue(AYellowBlue);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMColorBalanceManipulation.SetCyanRed(const Value: ShortInt);
begin
  FCyanRed := GetIntInRange(Value, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
end;

procedure TIMColorBalanceManipulation.SetMagentaGreen(const Value: ShortInt);
begin
  FMagentaGreen := GetIntInRange(Value, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
end;

procedure TIMColorBalanceManipulation.SetYellowBlue(const Value: ShortInt);
begin
  FYellowBlue := GetIntInRange(Value, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
end;


{ TIMTintManipulation }

constructor TIMTintManipulation.Create(const AHue, AAmount: SmallInt; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Tint');
  SetHue(AHue);
  SetAmount(AAmount);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMTintManipulation.SetAmount(const Value: SmallInt);
begin
  FAmount := GetIntInRange(Value, IM_TINT_AMOUNT_MIN, IM_TINT_AMOUNT_MAX);
end;

procedure TIMTintManipulation.SetHue(const Value: SmallInt);
begin
  FHue := GetIntInRange(Value, IM_TINT_HUE_MIN, IM_TINT_HUE_MAX);
end;


{ TIMLevelsManipulation }

constructor TIMLevelsManipulation.Create(const AHighlight, AMidtone, AShadow: ShortInt; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Levels');
  SetHighlight(AHighlight);
  SetMidtone(AMidtone);
  SetShadow(AShadow);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMLevelsManipulation.SetHighlight(const Value: ShortInt);
begin
  FHighlight := GetIntInRange(Value, IM_LEVELS_HIGHLIGHT_MIN, IM_LEVELS_HIGHLIGHT_MAX);
end;

procedure TIMLevelsManipulation.SetMidtone(const Value: ShortInt);
begin
  FMidtone := GetIntInRange(Value, IM_LEVELS_MIDTONE_MIN, IM_LEVELS_MIDTONE_MAX);
end;

procedure TIMLevelsManipulation.SetShadow(const Value: ShortInt);
begin
  FShadow := GetIntInRange(Value, IM_LEVELS_SHADOW_MIN, IM_LEVELS_SHADOW_MAX);
end;


{$region'                 GDI+ Color curve                   '}

{ TIMColorCurveManipulation }

constructor TIMColorCurveManipulation.Create(const AName: string; const AAdjustment: TGPCurveAdjustments; AAdjustValue: SmallInt;
  ACurveChannel: TGPCurveChannel = CurveChannelAll; ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create(AName);
  FAdjustment := AAdjustment;
  FCurveChannel := ACurveChannel;
  SetAdjustValue(AAdjustValue);
  FCount := ACount;
  FRectPtr := ARectPtr;
end;

procedure TIMColorCurveManipulation.SetAdjustValue(const Value: SmallInt);
begin
  case FAdjustment of
    AdjustExposure: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_EXPOSURE_MIN, IM_COLOR_CURVE_EXPOSURE_MAX);
    AdjustDensity: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_DENSITY_MIN, IM_COLOR_CURVE_DENSITY_MAX);
    AdjustContrast: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_CONTRAST_MIN, IM_COLOR_CURVE_CONTRAST_MAX);
    AdjustHighlight: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_HIGHLIGHT_MIN, IM_COLOR_CURVE_HIGHLIGHT_MAX);
    AdjustShadow: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_SHADOW_MIN, IM_COLOR_CURVE_SHADOW_MAX);
    AdjustMidtone: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_MIDTONE_MIN, IM_COLOR_CURVE_MIDTONE_MAX);
    AdjustWhiteSaturation: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_WHITE_SATURATION_MIN, IM_COLOR_CURVE_WHITE_SATURATION_MAX);
    AdjustBlackSaturation: FAdjustValue := GetIntInRange(Value, IM_COLOR_CURVE_BLACK_SATURATION_MIN, IM_COLOR_CURVE_BLACK_SATURATION_MAX);
  end;
end;

{ TIMColorCurveManipulation_Exposure }
constructor TIMColorCurveManipulation_Exposure.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
   ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Exposure', AdjustExposure, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_Density }
constructor TIMColorCurveManipulation_Density.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Density', AdjustDensity, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_Contrast }
constructor TIMColorCurveManipulation_Contrast.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Contrast', AdjustContrast, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_Highlight }
constructor TIMColorCurveManipulation_Highlight.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Highlight', AdjustHighlight, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_Shadow }
constructor TIMColorCurveManipulation_Shadow.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Shadow', AdjustShadow, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_Midtone }
constructor TIMColorCurveManipulation_Midtone.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Midtone', AdjustMidtone, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_WhiteSaturation }
constructor TIMColorCurveManipulation_WhiteSaturation.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - White saturation', AdjustWhiteSaturation, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;

{ TIMColorCurveManipulation_BlackSaturation }
constructor TIMColorCurveManipulation_BlackSaturation.Create(const AAdjustValue: SmallInt; ACurveChannel: TGPCurveChannel = CurveChannelAll;
  ACount: Word = 1; ARectPtr: PRect = nil);
begin
  inherited Create('GDI+ Color curve - Black saturation', AdjustBlackSaturation, AAdjustValue, ACurveChannel, ACount, ARectPtr);
end;
{$endregion GDI+ Color curve}



{ TIMColorMatrixManipulation }

constructor TIMColorMatrixManipulation.Create(const AColorMatrix: TGPColorMatrix; ACount: Word; ARectPtr: PRect);
begin
  inherited Create('GDI+ Color matrix');
  FCount := ACount;
  FRectPtr := ARectPtr;
  FColorMatrix := AColorMatrix;
end;


end.
