unit JPL.ImageManip.Base;

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

{$DEFINE IM_USE_GDI_PLUS}
{$DEFINE IM_USE_VAMPYRE_IMAGING}

interface


uses
  Windows, ActiveX,
  SysUtils, Classes, Graphics,
  {$IFDEF HAS_UNIT_SCOPE}Vcl.Imaging.pngimage,{$ELSE}pngimage,{$ENDIF}
  //Generics.Collections,

  {$IFDEF IM_USE_VAMPYRE_IMAGING}
  // Vampyre Imaging Library: https://github.com/galfar/imaginglib
  Imaging, ImagingClasses, ImagingTypes, ImagingComponents, ImagingCanvases, ImagingTiff, //ImagingExtras,
  {$ENDIF}

  {$IFDEF IM_USE_GDI_PLUS}
  // Delphi GDI+: https://github.com/jackdp/GDIPlus-Library-for-Delphi-and-Lazarus
  GdiPlus, GdiPlusHelpers,
  {$ENDIF}

  // JPLib
  JPL.Conversion,
  JPL.Win.Dialogs, // TODO: potem usun¹æ/wykomentowaæ
  JPL.TimeLogger;
  //JPL.Strings, JPL.TStr, JPL.Math, JPL.Win.Dialogs;

const
  IM_BRIGHTNESS_MIN = -255;
  IM_BRIGHTNESS_MAX = 255;
  IM_CONTRAST_MIN = -100;
  IM_CONTRAST_MAX = 100;
  IM_BLUR_RADIUS_MIN = 0;
  IM_BLUR_RADIUS_MAX = 255;
  IM_SHARPEN_RADIUS_MIN = 0;
  IM_SHARPEN_RADIUS_MAX = 255;
  IM_SHARPEN_AMOUNT_MIN = 0;
  IM_SHARPEN_AMOUNT_MAX = 100;
  IM_HSL_HUE_MIN = -180;
  IM_HSL_HUE_MAX = 180;
  IM_HSL_SATURATION_MIN = -100;
  IM_HSL_SATURATION_MAX = 100;
  IM_HSL_LIGHTNESS_MIN = -100;
  IM_HSL_LIGHTNESS_MAX = 100;
  IM_COLOR_BALANCE_MIN = -100;
  IM_COLOR_BALANCE_MAX = 100;
  IM_TINT_HUE_MIN = -180;
  IM_TINT_HUE_MAX = 180;
  IM_TINT_AMOUNT_MIN = -100;
  IM_TINT_AMOUNT_MAX = 100;
  IM_LEVELS_HIGHLIGHT_MIN = 0;
  IM_LEVELS_HIGHLIGHT_MAX = 100;
  IM_LEVELS_MIDTONE_MIN = -100;
  IM_LEVELS_MIDTONE_MAX = 100;
  IM_LEVELS_SHADOW_MIN = 0;
  IM_LEVELS_SHADOW_MAX = 100;
  IM_COLOR_CURVE_EXPOSURE_MIN = -255;
  IM_COLOR_CURVE_EXPOSURE_MAX = 255;
  IM_COLOR_CURVE_DENSITY_MIN = -255;
  IM_COLOR_CURVE_DENSITY_MAX = 255;
  IM_COLOR_CURVE_CONTRAST_MIN = -100;
  IM_COLOR_CURVE_CONTRAST_MAX = 100;
  IM_COLOR_CURVE_HIGHLIGHT_MIN = -100;
  IM_COLOR_CURVE_HIGHLIGHT_MAX = 100;
  IM_COLOR_CURVE_SHADOW_MIN = -100;
  IM_COLOR_CURVE_SHADOW_MAX = 100;
  IM_COLOR_CURVE_MIDTONE_MIN = -100;
  IM_COLOR_CURVE_MIDTONE_MAX = 100;
  IM_COLOR_CURVE_WHITE_SATURATION_MIN = 0;
  IM_COLOR_CURVE_WHITE_SATURATION_MAX = 255;
  IM_COLOR_CURVE_BLACK_SATURATION_MIN = 0;
  IM_COLOR_CURVE_BLACK_SATURATION_MAX = 255;


  ColorMatrix_Clear: TGPColorMatrix = (
    M: ((1, 0, 0, 0, 0),
        (0, 1, 0, 0, 0),
        (0, 0, 1, 0, 0),
        (0, 0, 0, 1, 0),
        (0, 0, 0, 0, 1)));

  ColorMatrix_Negative: TGPColorMatrix = (
    M: ((-1,  0,  0,  0,  0),
        ( 0, -1,  0,  0,  0),
        ( 0,  0, -1,  0,  0),
        ( 0,  0,  0,  1,  0),
        ( 1,  1,  1,  0,  1)));

  ColorMatrix_Grayscale: TGPColorMatrix = (
    M: (( 0.33, 0.33, 0.33,  0,  0 ),
        ( 0.33, 0.33, 0.33,  0,  0 ),
        ( 0.33, 0.33, 0.33,  0,  0 ),
        (    0,    0,    0,  1,  0 ),
        (    0,    0,    0,  0,  1 )));

  ColorMatrix_Grayscale_NTSC: TGPColorMatrix = (
    M: (( 0.299, 0.299, 0.299,  0,  0 ),
        ( 0.587, 0.587, 0.587,  0,  0 ),
        ( 0.114, 0.114, 0.114,  0,  0 ),
        (     0,     0,     0,  1,  0 ),
        (     0,     0,     0,  0,  1 )));

  ColorMatrix_Sepia: TGPColorMatrix = (
    M: (( 0.393, 0.349, 0.272,  0,  0 ),
        ( 0.769, 0.686, 0.534,  0,  0 ),
        ( 0.189, 0.168, 0.131,  0,  0 ),
        (     0,     0,     0,  1,  0 ),
        (     0,     0,     0,  0,  1 )));


{
  ShortInt: -127..128      (1 byte)
  SmallInt: -32767..32768  (2 bytes)
}

//type
//  // http://melander.dk/articles/alphasplash2/2/
//  TFixedStreamAdapter = class(TStreamAdapter)
//  public
//    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; override; stdcall;
//  end;


{$IFDEF IM_USE_GDI_PLUS}

{$region '          GDI+ Effect: Brightness / Contrast          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-brightnesscontrastparams
  Brightness
    INT -255..255 (0 = no change)
    Integer in the range -255 through 255 that specifies the brightness level. If the value is 0, the brightness remains the same.
    As the value moves from 0 to 255, the brightness of the image increases. As the value moves from 0 to -255, the brightness of the image decreases.
  Contrast
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies the contrast level. If the value is 0, the contrast remains the same.
    As the value moves from 0 to 100, the contrast of the image increases. As the value moves from 0 to -100, the contrast of the image decreases.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_BrightnessContrast(const GPBitmap: IGPBitmap; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_BrightnessContrast(const Bmp: TBitmap; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_BrightnessContrast(const Png: TPngImage; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Brightness / Contrast}


{$region '          GDI+ Effect: Blur          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-blurparams
  Radius
    FLOAT 0..255 (0 = no change)
    Real number that specifies the blur radius (the radius of the Gaussian convolution kernel) in pixels.
    The radius must be in the range 0 through 255. As the radius increases, the resulting bitmap becomes more blurry.
  ExpandEdge: Boolean value that specifies whether the bitmap expands by an amount equal to the blur radius.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_Blur(const GPBitmap: IGPBitmap; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Blur(const Bmp: TBitmap; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Blur(const Png: TPngImage; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Blur}


{$region '          GDI+ Effect: Sharpen          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-sharpenparams
  Radius
    FLOAT 0..255 (0 = no change)
    Real number that specifies the sharpening radius (the radius of the convolution kernel) in pixels. The radius must be in the
    range 0 through 255. As the radius increases, more surrounding pixels are involved in calculating the new value of a given pixel.
  Amount
    FLOAT 0..100 (0 = no change)
    Real number in the range 0 through 100 that specifies the amount of sharpening to be applied. A value of 0 specifies no sharpening.
    As the value of amount increases, the sharpness increases.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_Sharpen(const GPBitmap: IGPBitmap; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Sharpen(const Bmp: TBitmap; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Sharpen(const Png: TPngImage; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Sharpen}


{$region '          GDI+ Effect: HSL (Hue / Saturation / Lightness)          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-huesaturationlightnessparams
  Hue
    INT -180..180 (0 = no change)
    Integer in the range -180 through 180 that specifies the change in hue. A value of 0 specifies no change. Positive values
    specify counterclockwise rotation on the color wheel. Negative values specify clockwise rotation on the color wheel.
  Saturation
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies the change in saturation. A value of 0 specifies no change. Positive values
    specify increased saturation and negative values specify decreased saturation.
  Lightness
    INT -100..100
    Integer in the range -100 through 100 that specifies the change in lightness. A value of 0 specifies no change. Positive values
    specify increased lightness and negative values specify decreased lightness.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_HueSaturationLightness(const GPBitmap: IGPBitmap; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_HueSaturationLightness(const Bmp: TBitmap; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_HueSaturationLightness(const Png: TPngImage; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: HSL (Hue / Saturation / Lightness)}


{$region '          GDI+ Effect: Color balance          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-colorbalanceparams
  CyanRed
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies a change in the amount of red in the image. If the value is 0, there is no change.
    As the value moves from 0 to 100, the amount of red in the image increases and the amount of cyan decreases. As the value moves from 0 to -100,
    the amount of red in the image decreases and the amount of cyan increases.
  MagentaGreen
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies a change in the amount of green in the image. If the value is 0, there is no change.
    As the value moves from 0 to 100, the amount of green in the image increases and the amount of magenta decreases. As the value moves from 0 to -100,
    the amount of green in the image decreases and the amount of magenta increases.
  YellowBlue
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies a change in the amount of blue in the image. If the value is 0, there is no change.
    As the value moves from 0 to 100, the amount of blue in the image increases and the amount of yellow decreases. As the value moves from 0 to -100,
    the amount of blue in the image decreases and the amount of yellow increases.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_ColorBalance(const GPBitmap: IGPBitmap; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_ColorBalance(const Bmp: TBitmap; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_ColorBalance(const Png: TPngImage; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Color balance}


{$region '          GDI+ Effect: Tint          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-tintparams
  Hue
    INT -180..180
    Integer in the range -180 through 180 that specifies the hue to be strengthened or weakened. A value of 0 specifies blue.
    A positive value specifies a clockwise angle on the color wheel. For example, positive 60 specifies cyan and positive 120 specifies green.
    A negative value specifies a counter-clockwise angle on the color wheel. For example, negative 60 specifies magenta and negative 120 specifies red.
  Amount
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies how much the hue (given by the hue parameter) is strengthened or weakened.
    A value of 0 specifies no change. Positive values specify that the hue is strengthened and negative values specify that the hue is weakened.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_Tint(const GPBitmap: IGPBitmap; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Tint(const Bmp: TBitmap; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Tint(const Png: TPngImage; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Tint}


{$region '          GDI+ Effect: Levels          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ns-gdipluseffects-levelsparams
  Highlight
    INT 0..100 (100 = no change)
    Integer in the range 0 through 100 that specifies which pixels should be lightened. You can use this adjustment to lighten pixels that are
    already lighter than a certain threshold. Setting highlight to 100 specifies no change. Setting highlight to t specifies that a color channel
    value is increased if it is already greater than t percent of full intensity. For example, setting highlight to 90 specifies that all color channel
    values greater than 90 percent of full intensity are increased.
  Midtone
    INT -100..100 (0 = no change)
    Integer in the range -100 through 100 that specifies how much to lighten or darken an image. Color channel values in the middle of the intensity
    range are altered more than color channel values near the minimum or maximum intensity. You can use this adjustment to lighten (or darken) an image
    without loosing the contrast between the darkest and lightest portions of the image. A value of 0 specifies no change.
    Positive values specify that the midtones are made lighter, and negative values specify that the midtones are made darker.
  Shadow
    INT 0..100 (0 = no change)
    Integer in the range 0 through 100 that specifies which pixels should be darkened. You can use this adjustment to darken pixels that are already darker
    than a certain threshold. Setting shadow to 0 specifies no change. Setting shadow to t specifies that a color channel value is decreased if it is already
    less than t percent of full intensity. For example, setting shadow to 10 specifies that all color channel values less than 10 percent of full intensity
    are decreased.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_Levels(const GPBitmap: IGPBitmap; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Levels(const Bmp: TBitmap; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_Levels(const Png: TPngImage; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Levels}


{$region '          GDI+ Effect: Color curve          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluseffects/ne-gdipluseffects-curveadjustments
  Adjustment
    AdjustExposure, AdjustDensity, AdjustContrast, AdjustHighlight, AdjustShadow, AdjustMidtone, AdjustWhiteSaturation, AdjustBlackSaturation
  CurveChannel
    CurveChannelAll, CurveChannelRed, CurveChannelGreen, CurveChannelBlue
  AdjustValue
    INT - depends on the Adjustment
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).
}
function IM_PerformManip_ColorCurve(
  const GPBitmap: IGPBitmap;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD; overload

function IM_PerformManip_ColorCurve(
  const Bmp: TBitmap;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD; overload;

function IM_PerformManip_ColorCurve(
  const Png: TPngImage;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD; overload;

{
  AdjustValue
    INT -255..255 (0 = no change)
    Simulates increasing or decreasing the exposure of a photograph. When you set the adjustment member of a ColorCurveParams object to AdjustExposure,
    you should set the adjustValue member to an integer in the range -255 through 255. A value of 0 specifies no change in exposure.
    Positive values specify increased exposure and negative values specify decreased exposure.
}
function IM_PerformManip_ColorCurve_Exposure(const GPBitmap: IGPBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Exposure(const Bmp: TBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Exposure(const Png: TPngImage; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT -255..255 (0 = no change)
    Simulates increasing or decreasing the film density of a photograph. When you set the adjustment member of a ColorCurveParams object to AdjustDensity,
    you should set the adjustValue member to an integer in the range -255 through 255. A value of 0 specifies no change in density.
    Positive values specify increased density (lighter picture) and negative values specify decreased density (darker picture).
}
function IM_PerformManip_ColorCurve_Density(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Density(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Density(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT -100..100 (0 = no change)
    Increases or decreases the contrast of a bitmap. When you set the adjustment member of a ColorCurveParams object to AdjustContrast,
    you should set the adjustValue member to an integer in the range -100 through 100. A value of 0 specifies no change in contrast.
    Positive values specify increased contrast and negative values specify decreased contrast.
}
function IM_PerformManip_ColorCurve_Contrast(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Contrast(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Contrast(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT -100..100 (0 = no change)
    Increases or decreases the value of a color channel if that channel already has a value that is above half intensity.
    You can use this adjustment to get more definition in the light areas of an image without affecting the dark areas. When you set the adjustment
    member of a ColorCurveParams object to AdjustHighlight, you should set the adjustValue member to an integer in the range -100 through 100.
    A value of 0 specifies no change. Positive values specify that the light areas are made lighter, and negative values specify that the light areas
    are made darker.
}
function IM_PerformManip_ColorCurve_Highlight(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Highlight(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Highlight(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT -100..100 (0 = no change)
    Increases or decreases the value of a color channel if that channel already has a value that is below half intensity. You can use this adjustment to get
    more definition in the dark areas of an image without affecting the light areas. When you set the adjustment member of a ColorCurveParams object to
    AdjustShadow, you should set the adjustValue member to an integer in the range -100 through 100. A value of 0 specifies no change.
    Positive values specify that the dark areas are made lighter, and negative values specify that the dark areas are made darker.
}
function IM_PerformManip_ColorCurve_Shadow(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Shadow(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Shadow(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT -100..100 (0 = no change)
    Lightens or darkens an image. Color channel values in the middle of the intensity range are altered more than color channel values near the minimum
    or maximum intensity. You can use this adjustment to lighten (or darken) an image without loosing the contrast between the darkest and lightest portions
    of the image. When you set the adjustment member of a ColorCurveParams object to AdjustMidtone, you should set the adjustValue member to an integer
    in the range -100 through 100. A value of 0 specifies no change.
    Positive values specify that the midtones are made lighter, and negative values specify that the midtones are made darker.
}
function IM_PerformManip_ColorCurve_Midtone(const GPBitmap: IGPBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Midtone(const Bmp: TBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_Midtone(const Png: TPngImage; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT 0..255 (255 = no change)
    When you set the adjustment member of a ColorCurveParams object to AdjustWhiteSaturation, you should set the adjustValue member to an integer in the
    range 0 through 255. A value of t specifies that the interval [0, t] is mapped linearly to the interval [0, 255]. For example, if adjustValue is equal
    to 240, then color channel values in the interval [0, 240] are adjusted so that they spread out over the interval [0, 255].
    Color channel values greater than 240 are set to 255.
}
function IM_PerformManip_ColorCurve_WhiteSaturation(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_WhiteSaturation(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_WhiteSaturation(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{
  AdjustValue
    INT 0..255 (0 = no change)
    When you set the adjustment member of a ColorCurveParams object to AdjustBlackSaturation, you should set the adjustValue member to an integer in the
    range 0 through 255. A value of t specifies that the interval [t, 255] is mapped linearly to the interval [0, 255]. For example, if adjustValue is equal
    to 15, then color channel values in the interval [15, 255] are adjusted so that they spread out over the interval [0, 255].
    Color channel values less than 15 are set to 0.
}
function IM_PerformManip_ColorCurve_BlackSaturation(const GPBitmap: IGPbitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_BlackSaturation(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

function IM_PerformManip_ColorCurve_BlackSaturation(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;

{$endregion GDI+ Effect: Color curve}


{$region '          GDI+ Effect: Color matrix          '}
{
  https://docs.microsoft.com/en-us/windows/win32/api/gdipluscolormatrix/ns-gdipluscolormatrix-colormatrix
  ColorMatrix
    A 5×5 color matrix is a homogeneous matrix for a 4-space transformation. The element in the fifth row and fifth column of a 5×5 homogeneous matrix
    must be 1, and all of the other elements in the fifth column must be 0. Color matrices are used to transform color vectors.
    The first four components of a color vector hold the red, green, blue, and alpha components (in that order) of a color.
    The fifth component of a color vector is always 1.
  Count: How many times to perform the manipulation.
  RectPtr: Restrict the manipulation to the specified rectangle.
  Returns: Elapsed time in ms (high precision).

}

function IM_PerformManip_ColorMatrix(const GPBitmap: IGPBitmap; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_ColorMatrix(const Bmp: TBitmap; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
function IM_PerformManip_ColorMatrix(const Png: TPngImage; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD; overload;
{$endregion GDI+ Effect: Color matrix}


{$region '          GDI+ Resize          '}
function IM_PerformManip_Resize(
  const GPBitmap: IGPBitmap; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD; overload;

function IM_PerformManip_Resize(
  const Bmp: TBitmap; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD; overload;

function IM_PerformManip_Resize(
  const Png: TPngImage; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD; overload;
{$endregion GDI+ Resize}

{
  https://docs.microsoft.com/en-us/windows/win32/api/gdiplusimaging/ne-gdiplusimaging-rotatefliptype
}
function IM_PerformManip_FlipVertical(const Bmp: TBitmap): DWORD;
function IM_PerformManip_FlipHorizontal(const Bmp: TBitmap): DWORD;

// clockwise rotations
function IM_PerformManip_Rotate90(const Bmp: TBitmap): DWORD;
function IM_PerformManip_Rotate180(const Bmp: TBitmap): DWORD;
function IM_PerformManip_Rotate270(const Bmp: TBitmap): DWORD;


{$ENDIF IM_USE_GDI_PLUS}



{$IFDEF IM_USE_VAMPYRE_IMAGING}
// VI - Vampyre Imaging
function IM_PerformManip_VI_FlipVertical(const Bmp: TBitmap): DWORD;
function IM_PerformManip_VI_FlipHorizontal(const Bmp: TBitmap): DWORD;
{$ENDIF}




{$region '              Conversion routines                '}

{$IFDEF IM_USE_GDI_PLUS}
procedure IM_ConvertGPBitmapToBitmap(const GPBitmap: IGPBitmap; const OutputBmp: TBitmap); overload;
function IM_ConvertGPBitmapToBitmap(const GPBitmap: IGPBitmap): TBitmap; overload;

function IM_ConvertPngToGPBitmap(const Png: TPngImage): IGPBitmap;

procedure IM_ConvertGPBitmapToPng(const GPBitmap: IGPBitmap; const OutputPng: TPngImage); overload;
function IM_ConvertGPBitmapToPng(const GPBitmap: IGPBitmap): TPngImage; overload;

function IM_ConvertBitmapToGPBitmap(const Bmp: TBitmap): IGPBitmap;
{$ENDIF IM_USE_GDI_PLUS}

function IM_ConvertBitmapToPng(const Bmp: TBitmap): TPngImage;
{$endregion Conversion routines}



implementation



//function TFixedStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: Integer): HResult;
//begin
//  Result := inherited Stat(statstg, grfStatFlag);
//  statstg.pwcsName := nil;
//end;


{$region '                 Conversion routines                    '}

{$IFDEF IM_USE_GDI_PLUS}
procedure IM_ConvertGPBitmapToBitmap(const GPBitmap: IGPBitmap; const OutputBmp: TBitmap);
begin
  OutputBmp.Handle := GPBitmap.GetHBitmap(0);
end;

function IM_ConvertGPBitmapToBitmap(const GPBitmap: IGPBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  IM_ConvertGPBitmapToBitmap(GPBitmap, Result);
end;

//function IM_ConvertPngToGPBitmap2(const Png: TPngImage): IGPBitmap;
//var
//  fName: string;
//begin
//  fName := ExpandFileName('__temp.png');
//  Png.SaveToFile(fName);
//  Result := TGPBitmap.Create(fName);
//  //if not DeleteFile(fName) then WinMsg('DeleteFile failed!', '');
//end;

// https://social.msdn.microsoft.com/Forums/vstudio/en-US/549db796-0b4d-462f-9dae-b79b9689eba6/saveload-a-bitmap-from-memorystream-imageconverter-generic-gdi-error-if-the-stream-is-closed?forum=csharpgeneral
function IM_ConvertPngToGPBitmap(const Png: TPngImage): IGPBitmap;
var
  ms: TMemoryStream;
  istr: IStream;
begin
//  Result := IM_ConvertPngToGPBitmap2(Png); // <-- OK
//  Exit;

  ms := TMemoryStream.Create;
  try
    Png.SaveToStream(ms);
    ms.Position := 0;
    //istr := TStreamAdapter.Create(ms, TStreamOwnership.soReference) as IStream; // <-- Fail! Memory leak

    // http://docwiki.embarcadero.com/Libraries/Rio/en/System.Classes.TStreamAdapter.StreamOwnership
    // soOwned - TStreamAdapter is responsible for freeing the TMemoryStream (ms).
    istr := TStreamAdapter.Create(ms, TStreamOwnership.soOwned) as IStream; // <-- OK
    Result := TGPBitmap.Create(istr);
  finally
    //ms.Free; // <-- It must be commented out!
    //istr := nil;
  end;
end;

procedure IM_ConvertGPBitmapToPng(const GPBitmap: IGPBitmap; const OutputPng: TPngImage);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
    OutputPng.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

function IM_ConvertGPBitmapToPng(const GPBitmap: IGPBitmap): TPngImage;
begin
  Result := TPngImage.Create;
  IM_ConvertGPBitmapToPng(GPBitmap, Result);
end;

function IM_ConvertBitmapToGPBitmap(const Bmp: TBitmap): IGPBitmap;
begin
  if (Bmp.PixelFormat in [pf1Bit, pf4Bit, pf8Bit]) then Result := TGPBitmap.Create(Bmp.Handle, Bmp.Palette)
  else Result := TGPBitmap.Create(Bmp.Handle, 0);
end;
{$ENDIF IM_USE_GDI_PLUS}

function IM_ConvertBitmapToPng(const Bmp: TBitmap): TPngImage;
begin
  Result := TPngImage.Create;
  Result.Assign(Bmp);
end;
{$endregion Conversion routines}


{$IFDEF IM_USE_GDI_PLUS}

{$region '          GDI+ Brightness / Contrast          '}
function IM_PerformManip_BrightnessContrast(const GPBitmap: IGPBitmap; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPBrightnessContrast;
  Params: TGPBrightnessContrastParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Brightness = 0) and (Contrast = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPBrightnessContrast.Create;
    Params.BrightnessLevel := GetIntInRange(Brightness, IM_BRIGHTNESS_MIN, IM_BRIGHTNESS_MAX);
    Params.ContrastLevel := GetIntInRange(Contrast, IM_CONTRAST_MIN, IM_CONTRAST_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_BrightnessContrast(const Bmp: TBitmap; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Brightness = 0) and (Contrast = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_BrightnessContrast(GPBitmap, Brightness, Contrast, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_BrightnessContrast(const Png: TPngImage; Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Brightness = 0) and (Contrast = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_BrightnessContrast(GPBitmap, Brightness, Contrast, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Brightness / Contrast}


{$region '          GDI+ Blur          '}
function IM_PerformManip_Blur(const GPBitmap: IGPBitmap; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPBlur;
  Params: TGPBlurParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPBlur.Create;
    Params.Radius := GetFloatInRange(Radius, IM_BLUR_RADIUS_MIN, IM_BLUR_RADIUS_MAX);
    Params.ExpandEdge := ExpandEdge;
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Blur(const Bmp: TBitmap; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_Blur(GPBitmap, Radius, ExpandEdge, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_Blur(const Png: TPngImage; Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_Blur(GPBitmap, Radius, ExpandEdge, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP - tmStart);
end;
{$endregion GDI+ Blur}


{$region '          GDI+ Sharpen          '}
function IM_PerformManip_Sharpen(const GPBitmap: IGPBitmap; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPSharpen;
  Params: TGPSharpenParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Amount = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPSharpen.Create;
    Params.Radius := GetFloatInRange(Radius, IM_SHARPEN_RADIUS_MIN, IM_SHARPEN_RADIUS_MAX);
    Params.Amount := GetFloatInRange(Amount, IM_SHARPEN_AMOUNT_MIN, IM_SHARPEN_AMOUNT_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Sharpen(const Bmp: TBitmap; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil) : DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Amount = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_Sharpen(GPBitmap, Radius, Amount, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_Sharpen(const Png: TPngImage; Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if (Radius = 0) or (Amount = 0) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_Sharpen(GPBitmap, Radius, Amount, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Sharpen}


{$region '          GDI+ HSL (Hue / Saturation / Lightness)          '}
function IM_PerformManip_HueSaturationLightness(const GPBitmap: IGPBitmap; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPHueSaturationLightness;
  Params: TGPHueSaturationLightnessParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Saturation = 0) and (Lightness = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPHueSaturationLightness.Create;
    Params.HueLevel := GetIntInRange(Hue, IM_HSL_HUE_MIN, IM_HSL_HUE_MAX);
    Params.SaturationLevel := GetIntInRange(Saturation, IM_HSL_SATURATION_MIN, IM_HSL_SATURATION_MAX);
    Params.LightnessLevel := GetIntInRange(Lightness, IM_HSL_LIGHTNESS_MIN, IM_HSL_LIGHTNESS_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_HueSaturationLightness(const Bmp: TBitmap; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Saturation = 0) and (Lightness = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_HueSaturationLightness(GPBitmap, Hue, Saturation, Lightness, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_HueSaturationLightness(const Png: TPngImage; Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Saturation = 0) and (Lightness = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_HueSaturationLightness(GPBitmap, Hue, Saturation, Lightness, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ HSL (Hue / Saturation / Lightness)}


{$region '          GDI+ Color balance          '}
function IM_PerformManip_ColorBalance(const GPBitmap: IGPBitmap; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPColorBalance;
  Params: TGPColorBalanceParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if ( (CyanRed = 0) and (MagentaGreen = 0) and (YellowBlue = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPColorBalance.Create;
    Params.CyanRed := GetIntInRange(CyanRed, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
    Params.MagentaGreen := GetIntInRange(MagentaGreen, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
    Params.YellowBlue := GetIntInRange(YellowBlue, IM_COLOR_BALANCE_MIN, IM_COLOR_BALANCE_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_ColorBalance(const Bmp: TBitmap; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (CyanRed = 0) and (MagentaGreen = 0) and (YellowBlue = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_ColorBalance(GPBitmap, CyanRed, MagentaGreen, YellowBlue, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_ColorBalance(const Png: TPngImage; CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (CyanRed = 0) and (MagentaGreen = 0) and (YellowBlue = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_ColorBalance(GPBitmap, CyanRed, MagentaGreen, YellowBlue, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Color balance}


{$region '          GDI+ Tint          '}
function IM_PerformManip_Tint(const GPBitmap: IGPBitmap; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPTint;
  Params: TGPTintParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Amount = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPTint.Create;
    Params.Hue := GetIntInRange(Hue, IM_TINT_HUE_MIN, IM_TINT_HUE_MAX);
    Params.Amount := GetIntInRange(Amount, IM_TINT_AMOUNT_MIN, IM_TINT_AMOUNT_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Tint(const Bmp: TBitmap; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Amount = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_Tint(GPBitmap, Hue, Amount, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_Tint(const Png: TPngImage; Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Hue = 0) and (Amount = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_Tint(GPBitmap, Hue, Amount, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Tint}


{$region '          GDI+ Levels          '}
function IM_PerformManip_Levels(const GPBitmap: IGPBitmap; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPLevels;
  Params: TGPLevelsParams;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Highlight = 100) and (Midtone = 0) and (Shadow = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPLevels.Create;
    Params.Highlight := GetIntInRange(Highlight, IM_LEVELS_HIGHLIGHT_MIN, IM_LEVELS_HIGHLIGHT_MAX);
    Params.Midtone := GetIntInRange(Midtone, IM_LEVELS_MIDTONE_MIN, IM_LEVELS_MIDTONE_MAX);
    Params.Shadow := GetIntInRange(Shadow, IM_LEVELS_SHADOW_MIN, IM_LEVELS_SHADOW_MAX);
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Levels(const Bmp: TBitmap; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Highlight = 100) and (Midtone = 0) and (Shadow = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_Levels(GPBitmap, Highlight, Midtone, Shadow, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_Levels(const Png: TPngImage; const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if ( (Highlight = 100) and (Midtone = 0) and (Shadow = 0) ) or (Count = 0) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_Levels(GPBitmap, Highlight, Midtone, Shadow, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Levels}


{$region '          GDI+ Color curve          '}

function IM_PerformManip_ColorCurve(
  const GPBitmap: IGPBitmap;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD;
var
  Effect: IGPColorCurve;
  Params: TGPColorCurveParams;
  i: integer;
  AVal: SmallInt;
  tmStart: Int64;
begin
  Result := 0;
  if
    ( Count = 0 ) or
    ( (Adjustment = AdjustWhiteSaturation) and (AdjustValue = 255) )  or
    ( (Adjustment <> AdjustWhiteSaturation) and (AdjustValue = 0) ) then Exit;

  tmStart := GetTickCountHP;

  AVal := 0;
  case Adjustment of
    AdjustExposure: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_EXPOSURE_MIN, IM_COLOR_CURVE_EXPOSURE_MAX);
    AdjustDensity: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_DENSITY_MIN, IM_COLOR_CURVE_DENSITY_MAX);
    AdjustContrast: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_CONTRAST_MIN, IM_COLOR_CURVE_CONTRAST_MAX);
    AdjustHighlight: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_HIGHLIGHT_MIN, IM_COLOR_CURVE_HIGHLIGHT_MAX);
    AdjustShadow: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_SHADOW_MIN, IM_COLOR_CURVE_SHADOW_MAX);
    AdjustMidtone: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_MIDTONE_MIN, IM_COLOR_CURVE_MIDTONE_MAX);
    AdjustWhiteSaturation: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_WHITE_SATURATION_MIN, IM_COLOR_CURVE_WHITE_SATURATION_MAX);
    AdjustBlackSaturation: AVal := GetIntInRange(AdjustValue, IM_COLOR_CURVE_BLACK_SATURATION_MIN, IM_COLOR_CURVE_BLACK_SATURATION_MAX);
  end;

  try
    Effect := TGPColorCurve.Create;
    Params.Adjustment := Adjustment;
    Params.Channel := CurveChannel;
    Params.AdjustValue := AVal;
    Effect.Parameters := Params;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_ColorCurve(
  const Bmp: TBitmap;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if
    ( Count = 0 ) or
    ( (Adjustment = AdjustWhiteSaturation) and (AdjustValue = 255) )  or
    ( (Adjustment <> AdjustWhiteSaturation) and (AdjustValue = 0) ) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_ColorCurve(GPBitmap, Adjustment, AdjustValue, CurveChannel, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP - tmStart);
end;

function IM_PerformManip_ColorCurve(
  const Png: TPngImage;
  const Adjustment: TGPCurveAdjustments; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil
): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if
    ( Count = 0 ) or
    ( (Adjustment = AdjustWhiteSaturation) and (AdjustValue = 255) )  or
    ( (Adjustment <> AdjustWhiteSaturation) and (AdjustValue = 0) ) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_ColorCurve(GPBitmap, Adjustment, AdjustValue, CurveChannel, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;


// ------------------- Color curve: Exposure ------------------
function IM_PerformManip_ColorCurve_Exposure(const GPBitmap: IGPBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustExposure, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Exposure(const Bmp: TBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustExposure, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Exposure(const Png: TPngImage; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustExposure, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Density ------------------
function IM_PerformManip_ColorCurve_Density(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustDensity, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Density(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustDensity, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Density(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustDensity, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Contrast ------------------
function IM_PerformManip_ColorCurve_Contrast(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustContrast, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Contrast(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustContrast, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Contrast(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustContrast, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Highlight ------------------
function IM_PerformManip_ColorCurve_Highlight(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustHighlight, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Highlight(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustHighlight, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Highlight(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustHighlight, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Shadow ------------------
function IM_PerformManip_ColorCurve_Shadow(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustShadow, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Shadow(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustShadow, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Shadow(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustShadow, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Midtone ------------------
function IM_PerformManip_ColorCurve_Midtone(const GPBitmap: IGPBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustMidtone, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Midtone(const Bmp: TBitmap; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustMidtone, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_Midtone(const Png: TPngImage; const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustMidtone, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: White saturation ------------------
function IM_PerformManip_ColorCurve_WhiteSaturation(const GPBitmap: IGPBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustWhiteSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_WhiteSaturation(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustWhiteSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_WhiteSaturation(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustWhiteSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

// ------------------- Color curve: Black saturation ------------------
function IM_PerformManip_ColorCurve_BlackSaturation(const GPBitmap: IGPbitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(GPBitmap, AdjustBlackSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_BlackSaturation(const Bmp: TBitmap; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Bmp, AdjustBlackSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

function IM_PerformManip_ColorCurve_BlackSaturation(const Png: TPngImage; AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil): DWORD;
begin
  Result := IM_PerformManip_ColorCurve(Png, AdjustBlackSaturation, AdjustValue, CurveChannel, Count, RectPtr);
end;

{$endregion GDI+ Color curve}


{$region '          GDI+ Color matrix          '}
function IM_PerformManip_ColorMatrix(const GPBitmap: IGPBitmap; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  Effect: IGPColorMatrixEffect;
  i: integer;
  tmStart: Int64;
begin
  Result := 0;
  if Count = 0 then Exit;

  tmStart := GetTickCountHP;
  try
    Effect := TGPColorMatrixEffect.Create;
    Effect.Parameters := ColorMatrix;
    for i := 1 to Count do GPBitmap.ApplyEffect(Effect, RectPtr);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_ColorMatrix(const Bmp: TBitmap; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if Count = 0 then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_ColorMatrix(GPBitmap, ColorMatrix, Count, RectPtr);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Result := DWORD(GetTickCountHP = tmStart);
end;

function IM_PerformManip_ColorMatrix(const Png: TPngImage; const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  Result := 0;
  if Count = 0 then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_ColorMatrix(GPBitmap, ColorMatrix, Count, RectPtr);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Color matrix}


{$region '                GDI+ Resize                     '}
function IM_PerformManip_Resize(
  const GPBitmap: IGPBitmap; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD;
var
  gr: IGPGraphics;
  tmStart: Int64;
begin
  Result := 0;
  if (NewWidth < 1) or (NewHeight < 1) then Exit;
  tmStart := GetTickCountHP;
  try
    gr := TGPGraphics.Create(GPBitmap);

    gr.SetCompositingMode(CompositingModeSourceCopy);
    gr.SetInterpolationMode(InterpolationMode);
    gr.SetPixelOffsetMode(PixelOffsetMode);
    gr.SetSmoothingMode(SmoothingMode);

    gr.DrawImage(GPBitmap, 0, 0, NewWidth, NewHeight);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Resize(
  const Bmp: TBitmap; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD;
var
  GPBitmap: IGPBitmap;
  gr: IGPGraphics;
  tmStart: Int64;
begin
  Result := 0;
  if (NewWidth < 1) or (NewHeight < 1) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertBitmapToGPBitmap(Bmp);
  IM_PerformManip_Resize(GPBitmap, NewWidth, NewHeight, InterpolationMode, PixelOffsetMode, SmoothingMode);
  IM_ConvertGPBitmapToBitmap(GPBitmap, Bmp);
  Bmp.SetSize(NewWidth, NewHeight);
  Result := DWORD(GetTickCountHP = tmStart);
end;

function IM_PerformManip_Resize(
  const Png: TPngImage; const NewWidth, NewHeight: integer;
  InterpolationMode: TGPInterpolationMode = InterpolationModeHighQualityBicubic;
  PixelOffsetMode: TGPPixelOffsetMode = PixelOffsetModeHighQuality;
  SmoothingMode: TGPSmoothingMode = SmoothingModeHighQuality
): DWORD;
var
  GPBitmap: IGPBitmap;
  gr: IGPGraphics;
  tmStart: Int64;
begin
  Result := 0;
  if (NewWidth < 1) or (NewHeight < 1) then Exit;

  tmStart := GetTickCountHP;
  GPBitmap := IM_ConvertPngToGPBitmap(Png);
  IM_PerformManip_Resize(GPBitmap, NewWidth, NewHeight, InterpolationMode, PixelOffsetMode, SmoothingMode);
  IM_ConvertGPBitmapToPng(GPBitmap, Png);
  Png.SetSize(NewWidth, NewHeight);
  Result := DWORD(GetTickCountHP = tmStart);
end;
{$endregion GDI+ Resize}


function IM_PerformManip_FlipVertical(const Bmp: TBitmap): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  try
    GPBitmap := Bmp.ToGPBitmap;
    GPBitmap.RotateFlip(RotateNoneFlipY);
    Bmp.FromGPBitmap(GPBitmap);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_FlipHorizontal(const Bmp: TBitmap): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  try
    GPBitmap := Bmp.ToGPBitmap;
    GPBitmap.RotateFlip(RotateNoneFlipX);
    Bmp.FromGPBitmap(GPBitmap);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Rotate90(const Bmp: TBitmap): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  try
    GPBitmap := Bmp.ToGPBitmap;
    GPBitmap.RotateFlip(Rotate90FlipNone);
    Bmp.FromGPBitmap(GPBitmap);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Rotate180(const Bmp: TBitmap): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  try
    GPBitmap := Bmp.ToGPBitmap;
    GPBitmap.RotateFlip(Rotate180FlipNone);
    Bmp.FromGPBitmap(GPBitmap);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_Rotate270(const Bmp: TBitmap): DWORD;
var
  GPBitmap: IGPBitmap;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  try
    GPBitmap := Bmp.ToGPBitmap;
    GPBitmap.RotateFlip(Rotate270FlipNone);
    Bmp.FromGPBitmap(GPBitmap);
  finally
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;




{$ENDIF IM_USE_GDI_PLUS}



{$IFDEF IM_USE_VAMPYRE_IMAGING}
function IM_PerformManip_VI_FlipVertical(const Bmp: TBitmap): DWORD;
var
  si: TSingleImage;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  si := TSingleImage.Create;
  try
    ConvertBitmapToImage(Bmp, si);
    si.Flip;
    ConvertImageToBitmap(si, Bmp);
  finally
    si.Free;
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;

function IM_PerformManip_VI_FlipHorizontal(const Bmp: TBitmap): DWORD;
var
  si: TSingleImage;
  tmStart: Int64;
begin
  tmStart := GetTickCountHP;
  si := TSingleImage.Create;
  try
    ConvertBitmapToImage(Bmp, si);
    si.Mirror;
    ConvertImageToBitmap(si, Bmp);
  finally
    si.Free;
    Result := DWORD(GetTickCountHP - tmStart);
  end;
end;
{$ENDIF IM_USE_VAMPYRE_IMAGING}


end.



