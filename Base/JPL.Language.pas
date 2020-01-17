unit JPL.Language;

{
  Jacek Pazera
  http://www.pazera-software.com


  Przydatne linki:
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx - Code Page Identifiers
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms647463(v=vs.85).aspx - VerLanguageName

 }

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses 
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils;

type
  TJPCodePageNameRec = record
    DotNETName: string;
    FullName: string;
  end;


{$IFDEF MSWINDOWS}
function LanguageIDToStr(const LangID: WORD): string;
{$ENDIF}
procedure GetCodePageNames(const CP: WORD; out cpnr: TJPCodePageNameRec);
function GetCodePageDesc(const CP: WORD): string;
function GetCodePageDotNetName(const CP: WORD): string;
  
implementation

{$IFDEF MSWINDOWS}
function LanguageIDToStr(const LangID: WORD): string;
var
  Buffer: array[0..255] of Char;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  if VerLanguageName(LangID, Buffer, {size in characters-->}Length(Buffer)) > 0 then Result := Buffer
  else Result := '';
end;
{$ENDIF}

function GetCodePageDesc(const CP: WORD): string;
var
  cpnr: TJPCodePageNameRec;
begin
  GetCodePageNames(CP, cpnr);
  Result := cpnr.FullName;
end;

function GetCodePageDotNetName(const CP: WORD): string;
var
  cpnr: TJPCodePageNameRec;
begin
  GetCodePageNames(CP, cpnr);
  Result := cpnr.DotNETName;
end;

procedure GetCodePageNames(const CP: WORD; out cpnr: TJPCodePageNameRec);

  procedure Save(const DN, FN: string);
  begin
    cpnr.DotNETName := DN;
    cpnr.FullName := FN;
  end;

begin
  case CP of
    037: Save('IBM037', 'IBM EBCDIC US-Canada');
    437: Save('IBM437', 'OEM United States');
    500: Save('IBM500', 'IBM EBCDIC International');
    708: Save('ASMO-708', 'Arabic (ASMO 708)');
    709: Save('', 'Arabic (ASMO-449+, BCON V4)');
    710: Save('', 'Arabic - Transparent Arabic');
    720: Save('DOS-720', 'Arabic (Transparent ASMO); Arabic (DOS)');
    737: Save('ibm737', 'OEM Greek (formerly 437G); Greek (DOS)');
    775: Save('ibm775', 'OEM Baltic; Baltic (DOS)');
    850: Save('ibm850', 'OEM Multilingual Latin 1; Western European (DOS)');
    852: Save('ibm852', 'OEM Latin 2; Central European (DOS)');
    855: Save('IBM855', 'OEM Cyrillic (primarily Russian)');
    857: Save('ibm857', 'OEM Turkish; Turkish (DOS)');
    858: Save('IBM00858', 'OEM Multilingual Latin 1 + Euro symbol');
    860: Save('IBM860', 'OEM Portuguese; Portuguese (DOS)');
    861: Save('ibm861', 'OEM Icelandic; Icelandic (DOS)');
    862: Save('DOS-862', 'OEM Hebrew; Hebrew (DOS)');
    863: Save('IBM863', 'OEM French Canadian; French Canadian (DOS)');
    864: Save('IBM864', 'OEM Arabic; Arabic (864)');
    865: Save('IBM865', 'OEM Nordic; Nordic (DOS)');
    866: Save('cp866', 'OEM Russian; Cyrillic (DOS)');
    869: Save('ibm869', 'OEM Modern Greek; Greek, Modern (DOS)');
    870: Save('IBM870', 'IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2');
    874: Save('windows-874', 'ANSI/OEM Thai (ISO 8859-11); Thai (Windows)');
    875: Save('cp875', 'IBM EBCDIC Greek Modern');
    932: Save('shift_jis', 'ANSI/OEM Japanese; Japanese (Shift-JIS)');
    936: Save('gb2312', 'ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)');
    949: Save('ks_c_5601-1987', 'ANSI/OEM Korean (Unified Hangul Code)');
    950: Save('big5', 'ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)');
    1026: Save('IBM1026', 'IBM EBCDIC Turkish (Latin 5)');
    1047: Save('IBM01047', 'IBM EBCDIC Latin 1/Open System');
    1140: Save('IBM01140', 'IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)');
    1141: Save('IBM01141', 'IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)');
    1142: Save('IBM01142', 'IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)');
    1143: Save('IBM01143', 'IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)');
    1144: Save('IBM01144', 'IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)');
    1145: Save('IBM01145', 'IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)');
    1146: Save('IBM01146', 'IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)');
    1147: Save('IBM01147', 'IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)');
    1148: Save('IBM01148', 'IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)');
    1149: Save('IBM01149', 'IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)');
    1200: Save('utf-16', 'Unicode UTF-16, little endian byte order (BMP of ISO 10646)'); //; available only to managed applications');
    1201: Save('unicodeFFFE', 'Unicode UTF-16, big endian byte order'); //; available only to managed applications');
    1250: Save('windows-1250', 'ANSI Central European; Central European (Windows)');
    1251: Save('windows-1251', 'ANSI Cyrillic; Cyrillic (Windows)');
    1252: Save('windows-1252', 'ANSI Latin 1; Western European (Windows)');
    1253: Save('windows-1253', 'ANSI Greek; Greek (Windows)');
    1254: Save('windows-1254', 'ANSI Turkish; Turkish (Windows)');
    1255: Save('windows-1255', 'ANSI Hebrew; Hebrew (Windows)');
    1256: Save('windows-1256', 'ANSI Arabic; Arabic (Windows)');
    1257: Save('windows-1257', 'ANSI Baltic; Baltic (Windows)');
    1258: Save('windows-1258', 'ANSI/OEM Vietnamese; Vietnamese (Windows)');
    1361: Save('Johab', 'Korean (Johab)');
    10000: Save('macintosh', 'MAC Roman; Western European (Mac)');
    10001: Save('x-mac-japanese', 'Japanese (Mac)');
    10002: Save('x-mac-chinesetrad', 'MAC Traditional Chinese (Big5); Chinese Traditional (Mac)');
    10003: Save('x-mac-korean', 'Korean (Mac)');
    10004: Save('x-mac-arabic', 'Arabic (Mac)');
    10005: Save('x-mac-hebrew', 'Hebrew (Mac)');
    10006: Save('x-mac-greek', 'Greek (Mac)');
    10007: Save('x-mac-cyrillic', 'Cyrillic (Mac)');
    10008: Save('x-mac-chinesesimp', 'MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)');
    10010: Save('x-mac-romanian', 'Romanian (Mac)');
    10017: Save('x-mac-ukrainian', 'Ukrainian (Mac)');
    10021: Save('x-mac-thai', 'Thai (Mac)');
    10029: Save('x-mac-ce', 'MAC Latin 2; Central European (Mac)');
    10079: Save('x-mac-icelandic', 'Icelandic (Mac)');
    10081: Save('x-mac-turkish', 'Turkish (Mac)');
    10082: Save('x-mac-croatian', 'Croatian (Mac)');
    12000: Save('utf-32', 'Unicode UTF-32, little endian byte order'); //; available only to managed applications');
    12001: Save('utf-32BE', 'Unicode UTF-32, big endian byte order'); //; available only to managed applications');
    20000: Save('x-Chinese_CNS', 'CNS Taiwan; Chinese Traditional (CNS)');
    20001: Save('x-cp20001', 'TCA Taiwan');
    20002: Save('x_Chinese-Eten', 'Eten Taiwan; Chinese Traditional (Eten)');
    20003: Save('x-cp20003', 'IBM5550 Taiwan');
    20004: Save('x-cp20004', 'TeleText Taiwan');
    20005: Save('x-cp20005', 'Wang Taiwan');
    20105: Save('x-IA5', 'IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)');
    20106: Save('x-IA5-German', 'IA5 German (7-bit)');
    20107: Save('x-IA5-Swedish', 'IA5 Swedish (7-bit)');
    20108: Save('x-IA5-Norwegian', 'IA5 Norwegian (7-bit)');
    20127: Save('us-ascii', 'US-ASCII (7-bit)');
    20261: Save('x-cp20261', 'T.61');
    20269: Save('x-cp20269', 'ISO 6937 Non-Spacing Accent');
    20273: Save('IBM273', 'IBM EBCDIC Germany');
    20277: Save('IBM277', 'IBM EBCDIC Denmark-Norway');
    20278: Save('IBM278', 'IBM EBCDIC Finland-Sweden');
    20280: Save('IBM280', 'IBM EBCDIC Italy');
    20284: Save('IBM284', 'IBM EBCDIC Latin America-Spain');
    20285: Save('IBM285', 'IBM EBCDIC United Kingdom');
    20290: Save('IBM290', 'IBM EBCDIC Japanese Katakana Extended');
    20297: Save('IBM297', 'IBM EBCDIC France');
    20420: Save('IBM420', 'IBM EBCDIC Arabic');
    20423: Save('IBM423', 'IBM EBCDIC Greek');
    20424: Save('IBM424', 'IBM EBCDIC Hebrew');
    20833: Save('x-EBCDIC-KoreanExtended', 'IBM EBCDIC Korean Extended');
    20838: Save('IBM-Thai', 'IBM EBCDIC Thai');
    20866: Save('koi8-r', 'Russian (KOI8-R); Cyrillic (KOI8-R)');
    20871: Save('IBM871', 'IBM EBCDIC Icelandic');
    20880: Save('IBM880', 'IBM EBCDIC Cyrillic Russian');
    20905: Save('IBM905', 'IBM EBCDIC Turkish');
    20924: Save('IBM00924', 'IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)');
    20932: Save('EUC-JP', 'Japanese (JIS 0208-1990 and 0212-1990)');
    20936: Save('x-cp20936', 'Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)');
    20949: Save('x-cp20949', 'Korean Wansung');
    21025: Save('cp1025', 'IBM EBCDIC Cyrillic Serbian-Bulgarian');
    21027: Save('', '(deprecated)');
    21866: Save('koi8-u', 'Ukrainian (KOI8-U); Cyrillic (KOI8-U)');
    28591: Save('iso-8859-1', 'ISO 8859-1 Latin 1; Western European (ISO)');
    28592: Save('iso-8859-2', 'ISO 8859-2 Central European; Central European (ISO)');
    28593: Save('iso-8859-3', 'ISO 8859-3 Latin 3');
    28594: Save('iso-8859-4', 'ISO 8859-4 Baltic');
    28595: Save('iso-8859-5', 'ISO 8859-5 Cyrillic');
    28596: Save('iso-8859-6', 'ISO 8859-6 Arabic');
    28597: Save('iso-8859-7', 'ISO 8859-7 Greek');
    28598: Save('iso-8859-8', 'ISO 8859-8 Hebrew; Hebrew (ISO-Visual)');
    28599: Save('iso-8859-9', 'ISO 8859-9 Turkish');
    28603: Save('iso-8859-13', 'ISO 8859-13 Estonian');
    28605: Save('iso-8859-15', 'ISO 8859-15 Latin 9');
    29001: Save('x-Europa', 'Europa 3');
    38598: Save('iso-8859-8-i', 'ISO 8859-8 Hebrew; Hebrew (ISO-Logical)');
    50220: Save('iso-2022-jp', 'ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)');
    50221: Save('csISO2022JP', 'ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)');
    50222: Save('iso-2022-jp', 'ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)');
    50225: Save('iso-2022-kr', 'ISO 2022 Korean');
    50227: Save('x-cp50227', 'ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)');
    50229: Save('', 'ISO 2022 Traditional Chinese');
    50930: Save('', 'EBCDIC Japanese (Katakana) Extended');
    50931: Save('', 'EBCDIC US-Canada and Japanese');
    50933: Save('', 'EBCDIC Korean Extended and Korean');
    50935: Save('', 'EBCDIC Simplified Chinese Extended and Simplified Chinese');
    50936: Save('', 'EBCDIC Simplified Chinese');
    50937: Save('', 'EBCDIC US-Canada and Traditional Chinese');
    50939: Save('', 'EBCDIC Japanese (Latin) Extended and Japanese');
    51932: Save('euc-jp', 'EUC Japanese');
    51936: Save('EUC-CN', 'EUC Simplified Chinese; Chinese Simplified (EUC)');
    51949: Save('euc-kr', 'EUC Korean');
    51950: Save('', 'EUC Traditional Chinese');
    52936: Save('hz-gb-2312', 'HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)');
    54936: Save('GB18030', '');
    57002: Save('x-iscii-de', 'ISCII Devanagari');
    57003: Save('x-iscii-be', 'ISCII Bangla');
    57004: Save('x-iscii-ta', 'ISCII Tamil');
    57005: Save('x-iscii-te', 'ISCII Telugu');
    57006: Save('x-iscii-as', 'ISCII Assamese');
    57007: Save('x-iscii-or', 'ISCII Odia');
    57008: Save('x-iscii-ka', 'ISCII Kannada');
    57009: Save('x-iscii-ma', 'ISCII Malayalam');
    57010: Save('x-iscii-gu', 'ISCII Gujarati');
    57011: Save('x-iscii-pa', 'ISCII Punjabi');
    65000: Save('utf-7', 'Unicode (UTF-7)');
    65001: Save('utf-8', 'Unicode (UTF-8)');

  else
    Save('', '');
  end;

end;



end.
