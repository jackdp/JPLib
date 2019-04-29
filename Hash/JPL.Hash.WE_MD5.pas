unit JPL.Hash.WE_MD5;

interface

uses
  Classes, SysUtils,
  JPL.Math, JPL.TimeLogger,
  JPL.Hash.Common,

  //WE
  Hash, Mem_util, MD5
  ;

const
  HASH_BUFFER_SIZE_MD5 = 1024*63; // 1024 * 100; // F000 = 61 440 bytes


function WeGetFileHash_Md5(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
function WeGetStreamHash_Md5(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;


implementation

function HexString(const x: array of byte): AnsiString; {-HEX string from memory}
begin
  Result := HexStr(@x, sizeof(x));
end;


function WeGetStreamHash_Md5(AStream: TStream; var HashResult: THashResultRec; StartPos: Int64 = 0; HashEnumProc: THashEnumProc = nil): Boolean;
var
  //Crc: integer;
  Context: THashContext;
  Digest: TMD5Digest;
  xRead, xPercent: integer;
  xSize, xTotalRead: Int64;
  BufNo: integer;
  Buffer: array[0..HASH_BUFFER_SIZE_MD5-1] of Byte;
  Logger: TClassTimeLogger;
begin
  Result := False;
  Logger := TClassTimeLogger.Create;
  try
    Logger.StartLog;
    ClearHashResultRec(HashResult);
    MD5Init(Context);

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

      if xRead > 0 then MD5Update(Context, @Buffer, xRead);

      if Assigned(HashEnumProc) then if not HashEnumProc(xPercent, BufNo) then Exit;

    end; // while

    MD5Final(Context, Digest);

    //HashResult.StrValueUpper := IntToHex(Crc, HASH_LEN_CRC24);
    HashResult.StrValueUpper := UpperCase(string(HexString(Digest)));
    HashResult.StrValueLower := LowerCase(HashResult.StrValueUpper);
//    HashResult.IntValue := Crc;
//    HashResult.Int64Value := Crc;
    Logger.EndLog;
    HashResult.ElapsedTimeMs := Logger.ElapsedTimeMs;
    HashResult.SpeedMBperSec := GetSpeedValue_MB_per_sec(HashResult.StreamSize, HashResult.ElapsedTimeMs);

    Result := True;
  finally
    Logger.Free;
  end;

end;

function WeGetFileHash_Md5(fName: string; var HashResult: THashResultRec; HashEnumProc: THashEnumProc = nil): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fName, fmOpenRead or fmShareDenyNone);
  try
    Result := WeGetStreamHash_Md5(fs, HashResult, 0, HashEnumProc);
  finally
    fs.Free;
  end;
end;

end.
