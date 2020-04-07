unit JPL.Hash.WE_Shake_128;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE: https://github.com/jackdp/www.wolfgang-ehrhardt.de
  Hash, Mem_util, sha3
  ;

const
  HASH_BUFFER_SIZE = 1024*63; // 1024 * 100; // F000 = 61 440 bytes

type
  TShake128Digest = packed array[0..31] of byte;


function WeGetFileHash_Shake128(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_Shake128(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation

function HexString(const x: array of byte): AnsiString; {-HEX string from memory}
begin
  Result := HexStr(@x, sizeof(x));
end;


function WeGetStreamHash_Shake128(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  Digest: TShake128Digest;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE-1] of Byte;
  Logger: TClassTimeLogger;
  State:  TSHA3State;
begin

  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htShake128;
    SHA3_Init(State,__SHAKE_128);

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

      if xRead > 0 then SHA3_Update(State, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while


    SHA3_FinalBit_LSB(State, Digest[0], SizeOf(Digest), @Digest[0], SizeOf(Digest) * 8); // <- OK !!!

    HashResult.StrValueUpper := UpperCase(string(HexString(Digest)));
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

function WeGetFileHash_Shake128(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_Shake128(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
