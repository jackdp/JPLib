unit JPL.Hash.WE_CRC64;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE: https://github.com/jackdp/www.wolfgang-ehrhardt.de
  Hash, Mem_util, CRC64
  ;

const
  HASH_BUFFER_SIZE_CRC32 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_Crc64(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_Crc64(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation


function WeGetStreamHash_Crc64(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  Crc: TCRC64;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_CRC32-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htCrc64;
    CRC64Init(Crc);

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

      if xRead > 0 then CRC64Update(Crc, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while

    CRC64Final(Crc);

    HashResult.StrValueUpper := UpperCase(string(HexStr(@CRC, DIGEST_LEN_CRC64))); // !!! musi byæ DIGEST_LEN_CRC64 zamiast HASH_LEN_CRC64
    HashResult.StrValueLower := LowerCase(HashResult.StrValueUpper);
    Logger.EndLog;
    HashResult.ElapsedTimeMs := Logger.ElapsedTimeMs;
    HashResult.SpeedMBperSec := GetSpeedValue_MB_per_sec(HashResult.StreamSize, HashResult.ElapsedTimeMs);
    HashResult.ValidHash := True;

    Result := True;
  finally
    Logger.Free;
  end;

end;

function WeGetFileHash_Crc64(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_Crc64(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
