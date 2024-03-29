unit JPL.Hash.HL_SHA2_256;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  // HashLib4Pascal - https://github.com/Xor-el/HashLib4Pascal
  HlpHash, HlpIHashResult, HlpSHA2_256;


const
  HASH_BUFFER_SIZE = HASH_DEF_BUF_SIZE; // 1024 * 64; // 64 KB


function HlGetFileHash_SHA2_256(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function HlGetStreamHash_SHA2_256(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation


function HlGetStreamHash_SHA2_256(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  Hash: TSHA2_256;
  hr: IHashResult;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE - 1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  Hash := TSHA2_256.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htSha0;

    Hash.Initialize;
    xTotalRead := StartPos;
    xSize := AStream.Size - StartPos;
    HashResult.StreamSize := xSize;
    BufNo := 0;

    AStream.Position := StartPos;

    while AStream.Position < AStream.Size do
    begin
      Inc(BufNo);
      xRead := AStream.Read(Buffer, SizeOf(Buffer));
      xTotalRead := xTotalRead + xRead;
      xPercent := Round(PercentValue(xTotalRead, xSize));
      if xRead > 0 then
      begin
        Hash.TransformUntyped(Buffer, xRead);
        if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;
      end
      else Break;
    end; // while

    hr := Hash.TransformFinal;
    HashResult.StrValueUpper := UpperCase(hr.ToString);
    HashResult.StrValueLower := LowerCase(HashResult.StrValueUpper);
    Logger.EndLog;
    HashResult.ElapsedTimeMs := Logger.ElapsedTimeMs;
    HashResult.SpeedMBperSec := GetSpeedValue_MB_per_sec(HashResult.StreamSize, HashResult.ElapsedTimeMs);
    HashResult.ValidHash := True;

    Result := True;
  finally
    Logger.Free;
    Hash.Free;
  end;

end;

function HlGetFileHash_SHA2_256(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyWrite);
  try
    Result := HlGetStreamHash_SHA2_256(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;


end.
