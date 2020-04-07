unit JPL.Hash.WE_Adler32;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE: https://github.com/jackdp/www.wolfgang-ehrhardt.de
  Hash, Mem_util, Adler32
  ;

const
  HASH_BUFFER_SIZE_ADLER32 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_Adler32(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_Adler32(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation


function WeGetStreamHash_Adler32(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  Crc: integer;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_ADLER32-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htAdler32;
    Adler32Init(Crc);

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
      if xRead > 0 then Adler32Update(Crc, @Buffer, xRead);
      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;
    end; // while

    Adler32Final(Crc);

    HashResult.StrValueUpper := IntToHex(Crc, HASH_LEN_ADLER32);
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

function WeGetFileHash_Adler32(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_Adler32(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
