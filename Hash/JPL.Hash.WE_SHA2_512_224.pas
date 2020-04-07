unit JPL.Hash.WE_SHA2_512_224;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE: https://github.com/jackdp/www.wolfgang-ehrhardt.de
  Hash, Mem_util, sha5_224
  ;

const
  HASH_BUFFER_SIZE_SHA512_224 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_SHA2_512_224(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_SHA2_512_224(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation

function HexString(const x: array of byte): AnsiString; {-HEX string from memory}
begin
  Result := HexStr(@x, sizeof(x));
end;


function WeGetStreamHash_SHA2_512_224(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  //Crc: integer;
  Context: THashContext;
  Digest: TSHA5_224Digest;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_SHA512_224-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    HashResult.HashType := htSha2_512_224;
    SHA5_224Init(Context);

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

      if xRead > 0 then SHA5_224Update(Context, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while

    SHA5_224Final(Context, Digest);

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

function WeGetFileHash_SHA2_512_224(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_SHA2_512_224(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
