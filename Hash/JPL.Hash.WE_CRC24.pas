unit JPL.Hash.WE_CRC24;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE: https://github.com/jackdp/www.wolfgang-ehrhardt.de
  Hash, Mem_util, CRC24
  ;

const
  HASH_BUFFER_SIZE_CRC24 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_Crc24(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_Crc24(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation


function WeGetStreamHash_Crc24(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  Crc: integer;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_CRC24-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htCrc24;
    CRC24Init(Crc);

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

      if xRead > 0 then CRC24Update(Crc, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while

    CRC24Final(Crc);

    HashResult.StrValueUpper := IntToHex(Crc, HASH_LEN_CRC24);
    HashResult.StrValueLower := LowerCase(HashResult.StrValueUpper);
    HashResult.IntValue := Crc;
    HashResult.Int64Value := Crc;
    Logger.EndLog;
    HashResult.ElapsedTimeMs := Logger.ElapsedTimeMs;
    HashResult.SpeedMBperSec := GetSpeedValue_MB_per_sec(HashResult.StreamSize, HashResult.ElapsedTimeMs);
    HashResult.ValidHash := True;

    Result := True;
  finally
    Logger.Free;
  end;

end;

function WeGetFileHash_Crc24(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_Crc24(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
