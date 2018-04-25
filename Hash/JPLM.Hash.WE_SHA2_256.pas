unit JPLM.Hash.WE_SHA2_256;

interface

uses
  SysUtils,
  MFPC.Classes.Streams,
  JPL.Math, JPL.TimeLogger,
  JPLM.Hash.Common,

  //WE
  Hash, Mem_util, sha256
  ;

const
  HASH_BUFFER_SIZE_SHA256 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_SHA2_256(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_SHA2_256(AStream: TM_Stream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation

function HexString(const x: array of byte): AnsiString; {-HEX string from memory}
begin
  Result := HexStr(@x, sizeof(x));
end;


function WeGetStreamHash_SHA2_256(AStream: TM_Stream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  //Crc: integer;
  Context: THashContext;
  Digest: TSHA256Digest;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_SHA256-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    SHA256Init(Context);

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

      if xRead > 0 then SHA256Update(Context, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while

    SHA256Final(Context, Digest);

    HashResult.StrValueUpper := UpperCase(string(HexString(Digest)));
    HashResult.StrValueLower := LowerCase(HashResult.StrValueUpper);
    Logger.EndLog;
    HashResult.ElapsedTimeMs := Logger.ElapsedTimeMs;
    HashResult.SpeedMBperSec := GetSpeedValue_MB_per_sec(HashResult.StreamSize, HashResult.ElapsedTimeMs);

    Result := True;
  finally
    Logger.Free;
  end;

end;

function WeGetFileHash_SHA2_256(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TM_FileStream;
begin
  fs := TM_FileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_SHA2_256(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
