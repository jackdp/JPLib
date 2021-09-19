unit JPL.ImageManip;

{
  /PL.ImageManip uses GDI+ 1.1 Extensions which are available on Windows Vista and later,
  or on computers with a certain version of Office installed (like Office 2007).

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

  TImageManipulation

  How to use:

  1. Create instance of TImageManipulation. You must provide the bitmap (Vcl.Graphics.TBitmap) or Png in the constructor.
     If you want to keep the alpha channel, use Png! Most bitmap operations performed by GDI+ clears the alpha channel.

  2. Add some manipulations to perform (AddManipulation_xxx methods).
     All operations to be performed are recorded in the List (TIMManipulationList).
     For more information about operations see JPL.ImageManip.Base.pas

  3. Call the ProcessImage function.
     If the function returns FALSE (ValidImage = False), then something has gone wrong (invalid input image format...).
     Operations (manipulations) are performed in the order in which they are added to the list.
     You can interrupt the execution of subsequent operations from the list by calling the Abort procedure.
     Then property Aborted = TRUE.
}


{$I .\..\jp.inc}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}


interface


uses
  Windows, SysUtils, Classes, Graphics,
  Generics.Collections,

  // Vampyre Imaging
  Imaging, ImagingClasses, ImagingTypes, ImagingComponents, ImagingCanvases, ImagingTiff, //ImagingExtras,

  // Delphi GDI+
  GdiPlus, GdiPlusHelpers,

  // JPLib
  JPL.Strings, JPL.TStr, JPL.Conversion, JPL.Math, JPL.TimeLogger, JPL.Win.Dialogs,

  JPL.ImageManip.Base, JPL.ImageManip.Classes;


type

  TIMManipulationList = TObjectList<TIMBaseManipulation>;

  TImageManipulation = class
  private
    FSingleImage: TSingleImage;
    FBitmap: TBitmap;
    FValidImage: Boolean;
    FList: TIMManipulationList;
    FElapsedTime: DWORD;
    FTimeLogger: TClassTimeLogger;
    FElapsedTimeStr: string;
    FAborted: Boolean;
  protected
  public
    constructor Create(const Bitmap: TBitmap);
    destructor Destroy; override;

    procedure AddManipulation_BrightnessContrast(const Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_Blur(const Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_Sharpen(const Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_HueSaturationLightness(const Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_ColorBalance(const CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_Tint(const Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
    procedure AddManipulation_Levels(const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Exposure(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Density(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Contrast(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Highlight(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Shadow(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_Midtone(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_WhiteSaturation(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorCurve_BlackSaturation(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
      Count: Word = 1; RectPtr: PRect = nil);

    procedure AddManipulation_ColorMatrix(const ColorMatrix: TGPColorMatrix; Count: Word = 1; RectPtr: PRect = nil);


    function ProcessImage: Boolean;
    procedure Abort;
    procedure GetOutputBitmap(var Bmp: TBitmap);

    property ValidImage: Boolean read FValidImage;
    property ElapsedTime: DWORD read FElapsedTime;
    property ElapsedTimeStr: string read FElapsedTimeStr;
    property Aborted: Boolean read FAborted;
    property List: TIMManipulationList read FList;
  end;


implementation




{ TImageManipulation }


constructor TImageManipulation.Create(const Bitmap: TBitmap);
begin
  inherited Create;

  FValidImage := False;
  FElapsedTime := 0;
  FElapsedTimeStr := '';
  FTimeLogger := TClassTimeLogger.Create;

  FSingleImage := TSingleImage.Create;
  FBitmap := Bitmap;

  FList := TIMManipulationList.Create(True);
end;

destructor TImageManipulation.Destroy;
begin
  FList.Free;
  FTimeLogger.Free;
  if Assigned(FSingleImage) then FSingleImage.Free;
  inherited;
end;


procedure TImageManipulation.AddManipulation_BrightnessContrast(const Brightness, Contrast: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMBrightnessContrastManipulation;
begin
  M := TIMBrightnessContrastManipulation.Create(Brightness, Contrast, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorBalance(const CyanRed, MagentaGreen, YellowBlue: ShortInt; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorBalanceManipulation;
begin
  M := TIMColorBalanceManipulation.Create(CyanRed, MagentaGreen, YellowBlue, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Contrast(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Contrast;
begin
  M := TIMColorCurveManipulation_Contrast.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Density(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Density;
begin
  M := TIMColorCurveManipulation_Density.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Exposure(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Exposure;
begin
  M := TIMColorCurveManipulation_Exposure.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Highlight(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Highlight;
begin
  M := TIMColorCurveManipulation_Highlight.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Shadow(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Shadow;
begin
  M := TIMColorCurveManipulation_Shadow.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_Midtone(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_Midtone;
begin
  M := TIMColorCurveManipulation_Midtone.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_WhiteSaturation(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_WhiteSaturation;
begin
  M := TIMColorCurveManipulation_WhiteSaturation.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorCurve_BlackSaturation(const AdjustValue: SmallInt; CurveChannel: TGPCurveChannel = CurveChannelAll;
  Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMColorCurveManipulation_BlackSaturation;
begin
  M := TIMColorCurveManipulation_BlackSaturation.Create(AdjustValue, CurveChannel, Count, RectPtr);
  List.Add(M);
end;

procedure TImageManipulation.AddManipulation_HueSaturationLightness(const Hue, Saturation, Lightness: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMHueSaturationLightnessManipulation;
begin
  M := TIMHueSaturationLightnessManipulation.Create(Hue, Saturation, Lightness, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_Levels(const Highlight, Midtone, Shadow: ShortInt; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMLevelsManipulation;
begin
  M := TIMLevelsManipulation.Create(Highlight, Midtone, Shadow, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_Sharpen(const Radius, Amount: Single; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMSharpenManipulation;
begin
  M := TIMSharpenManipulation.Create(Radius, Amount, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_Tint(const Hue, Amount: SmallInt; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMTintManipulation;
begin
  M := TIMTintManipulation.Create(Hue, Amount, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_Blur(const Radius: Single; ExpandEdge: Boolean = False; Count: Word = 1; RectPtr: PRect = nil);
var
  M: TIMBlurManipulation;
begin
  M := TIMBlurManipulation.Create(Radius, ExpandEdge, Count, RectPtr);
  FList.Add(M);
end;

procedure TImageManipulation.AddManipulation_ColorMatrix(const ColorMatrix: TGPColorMatrix; Count: Word; RectPtr: PRect);
var
  M: TIMColorMatrixManipulation;
begin
  M := TIMColorMatrixManipulation.Create(ColorMatrix, Count, RectPtr);
  FList.Add(M);
end;



procedure TImageManipulation.Abort;
begin
  FAborted := True;
end;

function TImageManipulation.ProcessImage: Boolean;
var
  i: integer;
  BaseManip: TIMBaseManipulation;
  M_BC: TIMBrightnessContrastManipulation;
  M_Blur: TIMBlurManipulation;
  M_Sharpen: TIMSharpenManipulation;
  M_HSL: TIMHueSaturationLightnessManipulation;
  M_ColorBalance: TIMColorBalanceManipulation;
  M_Tint: TIMTintManipulation;
  M_Levels: TIMLevelsManipulation;
  M_CC_Exposure: TIMColorCurveManipulation_Exposure;
  M_CC_Density: TIMColorCurveManipulation_Density;
  M_CC_Contrast: TIMColorCurveManipulation_Contrast;
  M_CC_Highlight: TIMColorCurveManipulation_Highlight;
  M_CC_Shadow: TIMColorCurveManipulation_Shadow;
  M_CC_Midtone: TIMColorCurveManipulation_Midtone;
  M_CC_WhiteSat: TIMColorCurveManipulation_WhiteSaturation;
  M_CC_BlackSat: TIMColorCurveManipulation_BlackSaturation;
  M_ColorMatrix: TIMColorMatrixManipulation;
begin
  FTimeLogger.StartLog;
  FAborted := False;
  FElapsedTime := 0;
  FElapsedTimeStr := '';
  Result := False;
  try

    FValidImage := False;
    if not Assigned(FBitmap) then Exit;
    ConvertBitmapToImage(FBitmap, FSingleImage);
    if not FSingleImage.Valid then Exit;

    FValidImage := True;


    for i := 0 to FList.Count - 1 do
    begin

      if FAborted then Break;

      BaseManip := FList[i];

      if BaseManip is TIMBrightnessContrastManipulation then
      begin
        M_BC := BaseManip as TIMBrightnessContrastManipulation;
        IM_PerformManip_BrightnessContrast(FBitmap, M_BC.Brightness, M_BC.Contrast, M_BC.Count, M_BC.RectPtr);
      end

      else if BaseManip is TIMBlurManipulation then
      begin
        M_Blur := BaseManip as TIMBlurManipulation;
        IM_PerformManip_Blur(FBitmap, M_Blur.Radius, M_Blur.ExpandEdge, M_Blur.Count, M_Blur.RectPtr);
      end

      else if BaseManip is TIMSharpenManipulation then
      begin
        M_Sharpen := BaseManip as TIMSharpenManipulation;
        IM_PerformManip_Sharpen(FBitmap, M_Sharpen.Radius, M_Sharpen.Amount, M_Sharpen.Count, M_Sharpen.RectPtr);
      end

      else if BaseManip is TIMHueSaturationLightnessManipulation then
      begin
        M_HSL := BaseManip as TIMHueSaturationLightnessManipulation;
        IM_PerformManip_HueSaturationLightness(FBitmap, M_HSL.Hue, M_HSL.Saturation, M_HSL.Lightness, M_HSL.Count, M_HSL.RectPtr);
      end

      else if BaseManip is TIMColorBalanceManipulation then
      begin
        M_ColorBalance := BaseManip as TIMColorBalanceManipulation;
        IM_PerformManip_ColorBalance(FBitmap, M_ColorBalance.CyanRed, M_ColorBalance.MagentaGreen, M_ColorBalance.YellowBlue, M_ColorBalance.Count, M_ColorBalance.RectPtr);
      end

      else if BaseManip is TIMTintManipulation then
      begin
        M_Tint := BaseManip as TIMTintManipulation;
        IM_PerformManip_Tint(FBitmap, M_Tint.Hue, M_Tint.Amount, M_Tint.Count, M_Tint.RectPtr)
      end

      else if BaseManip is TIMLevelsManipulation then
      begin
        M_Levels := BaseManip as TIMLevelsManipulation;
        IM_PerformManip_Levels(FBitmap, M_Levels.Highlight, M_Levels.Midtone, M_Levels.Shadow, M_Levels.Count, M_Levels.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Exposure then
      begin
        M_CC_Exposure := BaseManip as TIMColorCurveManipulation_Exposure;
        IM_PerformManip_ColorCurve_Exposure(FBitmap, M_CC_Exposure.AdjustValue, M_CC_Exposure.CurveChannel, M_CC_Exposure.Count, M_CC_Exposure.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Density then
      begin
        M_CC_Density := BaseManip as TIMColorCurveManipulation_Density;
        IM_PerformManip_ColorCurve_Density(FBitmap, M_CC_Density.AdjustValue, M_CC_Density.CurveChannel, M_CC_Density.Count, M_CC_Density.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Contrast then
      begin
        M_CC_Contrast := BaseManip as TIMColorCurveManipulation_Contrast;
        IM_PerformManip_ColorCurve_Contrast(FBitmap, M_CC_Contrast.AdjustValue, M_CC_Contrast.CurveChannel, M_CC_Contrast.Count, M_CC_Contrast.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Highlight then
      begin
        M_CC_Highlight := BaseManip as TIMColorCurveManipulation_Highlight;
        IM_PerformManip_ColorCurve_Highlight(FBitmap, M_CC_Highlight.AdjustValue, M_CC_Highlight.CurveChannel, M_CC_Highlight.Count, M_CC_Highlight.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Shadow then
      begin
        M_CC_Shadow := BaseManip as TIMColorCurveManipulation_Shadow;
        IM_PerformManip_ColorCurve_Shadow(FBitmap, M_CC_Shadow.AdjustValue, M_CC_Shadow.CurveChannel, M_CC_Shadow.Count, M_CC_Shadow.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_Midtone then
      begin
        M_CC_Midtone := BaseManip as TIMColorCurveManipulation_Midtone;
        IM_PerformManip_ColorCurve_Midtone(FBitmap, M_CC_Midtone.AdjustValue, M_CC_Midtone.CurveChannel, M_CC_Midtone.Count, M_CC_Midtone.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_WhiteSaturation then
      begin
        M_CC_WhiteSat := BaseManip as TIMColorCurveManipulation_WhiteSaturation;
        IM_PerformManip_ColorCurve_WhiteSaturation(FBitmap, M_CC_WhiteSat.AdjustValue, M_CC_WhiteSat.CurveChannel, M_CC_WhiteSat.Count, M_CC_WhiteSat.RectPtr);
      end

      else if BaseManip is TIMColorCurveManipulation_BlackSaturation then
      begin
        M_CC_BlackSat := BaseManip as TIMColorCurveManipulation_BlackSaturation;
        IM_PerformManip_ColorCurve_BlackSaturation(FBitmap, M_CC_BlackSat.AdjustValue, M_CC_BlackSat.CurveChannel, M_CC_BlackSat.Count, M_CC_BlackSat.RectPtr);
      end

      else if BaseManip is TIMColorMatrixManipulation then
      begin
        M_ColorMatrix := BaseManip as TIMColorMatrixManipulation;
        IM_PerformManip_ColorMatrix(FBitmap, M_ColorMatrix.ColorMatrix, M_ColorMatrix.Count, M_ColorMatrix.RectPtr);
      end


    end; // for i



    //ConvertImageToBitmap(FSingleImage, FBitmap);


  finally
    Result := FValidImage;

    FTimeLogger.EndLog;
    FElapsedTime := DWORD(FTimeLogger.ElapsedTimeMs);
    FElapsedTimeStr := FTimeLogger.ElapsedTimeStr;
  end;
end;

procedure TImageManipulation.GetOutputBitmap(var Bmp: TBitmap);
begin
  if FValidImage then
  begin
    Bmp.SetSize(FBitmap.Width, FBitmap.Height);
    Bmp.PixelFormat := FBitmap.PixelFormat;
    Bmp.Assign(FBitmap);
  end;
end;



end.
