unit JPL.Language;

{
  Jacek Pazera
  http://www.pazera-software.com


  Przydatne linki:
  https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx - Code Page Identifiers
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms647463(v=vs.85).aspx - VerLanguageName

 }

{$I .\..\jp.inc}
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses 
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils;

type

  TJPCodePageNameRec = record
    DotNETName: string;
    FullName: string;
  end;

  TCodePageRec = record
    CodePage: Word;
    DotNETName: string;
    FullName: string;
  end;


const

  ArrCodePages: array[0..151] of TCodePageRec = (
    (CodePage:    37; DotNETName: 'IBM037'; FullName: 'IBM EBCDIC US-Canada'),
    (CodePage:   437; DotNETName: 'IBM437'; FullName: 'OEM United States'),
    (CodePage:   500; DotNETName: 'IBM500'; FullName: 'IBM EBCDIC International'),
    (CodePage:   708; DotNETName: 'ASMO-708'; FullName: 'Arabic (ASMO 708)'),
    (CodePage:   709; DotNETName: ''; FullName: 'Arabic (ASMO-449+, BCON V4)'),
    (CodePage:   710; DotNETName: ''; FullName: 'Arabic - Transparent Arabic'),
    (CodePage:   720; DotNETName: 'DOS-720'; FullName: 'Arabic (Transparent ASMO); Arabic (DOS)'),
    (CodePage:   737; DotNETName: 'ibm737'; FullName: 'OEM Greek (formerly 437G); Greek (DOS)'),
    (CodePage:   775; DotNETName: 'ibm775'; FullName: 'OEM Baltic; Baltic (DOS)'),
    (CodePage:   850; DotNETName: 'ibm850'; FullName: 'OEM Multilingual Latin 1; Western European (DOS)'),
    (CodePage:   852; DotNETName: 'ibm852'; FullName: 'OEM Latin 2; Central European (DOS)'),
    (CodePage:   855; DotNETName: 'IBM855'; FullName: 'OEM Cyrillic (primarily Russian)'),
    (CodePage:   857; DotNETName: 'ibm857'; FullName: 'OEM Turkish; Turkish (DOS)'),
    (CodePage:   858; DotNETName: 'IBM00858'; FullName: 'OEM Multilingual Latin 1 + Euro symbol'),
    (CodePage:   860; DotNETName: 'IBM860'; FullName: 'OEM Portuguese; Portuguese (DOS)'),
    (CodePage:   861; DotNETName: 'ibm861'; FullName: 'OEM Icelandic; Icelandic (DOS)'),
    (CodePage:   862; DotNETName: 'DOS-862'; FullName: 'OEM Hebrew; Hebrew (DOS)'),
    (CodePage:   863; DotNETName: 'IBM863'; FullName: 'OEM French Canadian; French Canadian (DOS)'),
    (CodePage:   864; DotNETName: 'IBM864'; FullName: 'OEM Arabic; Arabic (864)'),
    (CodePage:   865; DotNETName: 'IBM865'; FullName: 'OEM Nordic; Nordic (DOS)'),
    (CodePage:   866; DotNETName: 'cp866'; FullName: 'OEM Russian; Cyrillic (DOS)'),
    (CodePage:   869; DotNETName: 'ibm869'; FullName: 'OEM Modern Greek; Greek, Modern (DOS)'),
    (CodePage:   870; DotNETName: 'IBM870'; FullName: 'IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2'),
    (CodePage:   874; DotNETName: 'windows-874'; FullName: 'ANSI/OEM Thai (ISO 8859-11); Thai (Windows)'),
    (CodePage:   875; DotNETName: 'cp875'; FullName: 'IBM EBCDIC Greek Modern'),
    (CodePage:   932; DotNETName: 'shift_jis'; FullName: 'ANSI/OEM Japanese; Japanese (Shift-JIS)'),
    (CodePage:   936; DotNETName: 'gb2312'; FullName: 'ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)'),
    (CodePage:   949; DotNETName: 'ks_c_5601-1987'; FullName: 'ANSI/OEM Korean (Unified Hangul Code)'),
    (CodePage:   950; DotNETName: 'big5'; FullName: 'ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)'),
    (CodePage:  1026; DotNETName: 'IBM1026'; FullName: 'IBM EBCDIC Turkish (Latin 5)'),
    (CodePage:  1047; DotNETName: 'IBM01047'; FullName: 'IBM EBCDIC Latin 1/Open System'),
    (CodePage:  1140; DotNETName: 'IBM01140'; FullName: 'IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)'),
    (CodePage:  1141; DotNETName: 'IBM01141'; FullName: 'IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)'),
    (CodePage:  1142; DotNETName: 'IBM01142'; FullName: 'IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)'),
    (CodePage:  1143; DotNETName: 'IBM01143'; FullName: 'IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)'),
    (CodePage:  1144; DotNETName: 'IBM01144'; FullName: 'IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)'),
    (CodePage:  1145; DotNETName: 'IBM01145'; FullName: 'IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)'),
    (CodePage:  1146; DotNETName: 'IBM01146'; FullName: 'IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)'),
    (CodePage:  1147; DotNETName: 'IBM01147'; FullName: 'IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)'),
    (CodePage:  1148; DotNETName: 'IBM01148'; FullName: 'IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)'),
    (CodePage:  1149; DotNETName: 'IBM01149'; FullName: 'IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)'),
    (CodePage:  1200; DotNETName: 'utf-16'; FullName: 'Unicode UTF-16, little endian byte order (BMP of ISO 10646)'),
    (CodePage:  1201; DotNETName: 'unicodeFFFE'; FullName: 'Unicode UTF-16, big endian byte order'),
    (CodePage:  1250; DotNETName: 'windows-1250'; FullName: 'ANSI Central European; Central European (Windows)'),
    (CodePage:  1251; DotNETName: 'windows-1251'; FullName: 'ANSI Cyrillic; Cyrillic (Windows)'),
    (CodePage:  1252; DotNETName: 'windows-1252'; FullName: 'ANSI Latin 1; Western European (Windows)'),
    (CodePage:  1253; DotNETName: 'windows-1253'; FullName: 'ANSI Greek; Greek (Windows)'),
    (CodePage:  1254; DotNETName: 'windows-1254'; FullName: 'ANSI Turkish; Turkish (Windows)'),
    (CodePage:  1255; DotNETName: 'windows-1255'; FullName: 'ANSI Hebrew; Hebrew (Windows)'),
    (CodePage:  1256; DotNETName: 'windows-1256'; FullName: 'ANSI Arabic; Arabic (Windows)'),
    (CodePage:  1257; DotNETName: 'windows-1257'; FullName: 'ANSI Baltic; Baltic (Windows)'),
    (CodePage:  1258; DotNETName: 'windows-1258'; FullName: 'ANSI/OEM Vietnamese; Vietnamese (Windows)'),
    (CodePage:  1361; DotNETName: 'Johab'; FullName: 'Korean (Johab)'),
    (CodePage: 10000; DotNETName: 'macintosh'; FullName: 'MAC Roman; Western European (Mac)'),
    (CodePage: 10001; DotNETName: 'x-mac-japanese'; FullName: 'Japanese (Mac)'),
    (CodePage: 10002; DotNETName: 'x-mac-chinesetrad'; FullName: 'MAC Traditional Chinese (Big5); Chinese Traditional (Mac)'),
    (CodePage: 10003; DotNETName: 'x-mac-korean'; FullName: 'Korean (Mac)'),
    (CodePage: 10004; DotNETName: 'x-mac-arabic'; FullName: 'Arabic (Mac)'),
    (CodePage: 10005; DotNETName: 'x-mac-hebrew'; FullName: 'Hebrew (Mac)'),
    (CodePage: 10006; DotNETName: 'x-mac-greek'; FullName: 'Greek (Mac)'),
    (CodePage: 10007; DotNETName: 'x-mac-cyrillic'; FullName: 'Cyrillic (Mac)'),
    (CodePage: 10008; DotNETName: 'x-mac-chinesesimp'; FullName: 'MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)'),
    (CodePage: 10010; DotNETName: 'x-mac-romanian'; FullName: 'Romanian (Mac)'),
    (CodePage: 10017; DotNETName: 'x-mac-ukrainian'; FullName: 'Ukrainian (Mac)'),
    (CodePage: 10021; DotNETName: 'x-mac-thai'; FullName: 'Thai (Mac)'),
    (CodePage: 10029; DotNETName: 'x-mac-ce'; FullName: 'MAC Latin 2; Central European (Mac)'),
    (CodePage: 10079; DotNETName: 'x-mac-icelandic'; FullName: 'Icelandic (Mac)'),
    (CodePage: 10081; DotNETName: 'x-mac-turkish'; FullName: 'Turkish (Mac)'),
    (CodePage: 10082; DotNETName: 'x-mac-croatian'; FullName: 'Croatian (Mac)'),
    (CodePage: 12000; DotNETName: 'utf-32'; FullName: 'Unicode UTF-32, little endian byte order'),
    (CodePage: 12001; DotNETName: 'utf-32BE'; FullName: 'Unicode UTF-32, big endian byte order'),
    (CodePage: 20000; DotNETName: 'x-Chinese_CNS'; FullName: 'CNS Taiwan; Chinese Traditional (CNS)'),
    (CodePage: 20001; DotNETName: 'x-cp20001'; FullName: 'TCA Taiwan'),
    (CodePage: 20002; DotNETName: 'x_Chinese-Eten'; FullName: 'Eten Taiwan; Chinese Traditional (Eten)'),
    (CodePage: 20003; DotNETName: 'x-cp20003'; FullName: 'IBM5550 Taiwan'),
    (CodePage: 20004; DotNETName: 'x-cp20004'; FullName: 'TeleText Taiwan'),
    (CodePage: 20005; DotNETName: 'x-cp20005'; FullName: 'Wang Taiwan'),
    (CodePage: 20105; DotNETName: 'x-IA5'; FullName: 'IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)'),
    (CodePage: 20106; DotNETName: 'x-IA5-German'; FullName: 'IA5 German (7-bit)'),
    (CodePage: 20107; DotNETName: 'x-IA5-Swedish'; FullName: 'IA5 Swedish (7-bit)'),
    (CodePage: 20108; DotNETName: 'x-IA5-Norwegian'; FullName: 'IA5 Norwegian (7-bit)'),
    (CodePage: 20127; DotNETName: 'us-ascii'; FullName: 'US-ASCII (7-bit)'),
    (CodePage: 20261; DotNETName: 'x-cp20261'; FullName: 'T.61'),
    (CodePage: 20269; DotNETName: 'x-cp20269'; FullName: 'ISO 6937 Non-Spacing Accent'),
    (CodePage: 20273; DotNETName: 'IBM273'; FullName: 'IBM EBCDIC Germany'),
    (CodePage: 20277; DotNETName: 'IBM277'; FullName: 'IBM EBCDIC Denmark-Norway'),
    (CodePage: 20278; DotNETName: 'IBM278'; FullName: 'IBM EBCDIC Finland-Sweden'),
    (CodePage: 20280; DotNETName: 'IBM280'; FullName: 'IBM EBCDIC Italy'),
    (CodePage: 20284; DotNETName: 'IBM284'; FullName: 'IBM EBCDIC Latin America-Spain'),
    (CodePage: 20285; DotNETName: 'IBM285'; FullName: 'IBM EBCDIC United Kingdom'),
    (CodePage: 20290; DotNETName: 'IBM290'; FullName: 'IBM EBCDIC Japanese Katakana Extended'),
    (CodePage: 20297; DotNETName: 'IBM297'; FullName: 'IBM EBCDIC France'),
    (CodePage: 20420; DotNETName: 'IBM420'; FullName: 'IBM EBCDIC Arabic'),
    (CodePage: 20423; DotNETName: 'IBM423'; FullName: 'IBM EBCDIC Greek'),
    (CodePage: 20424; DotNETName: 'IBM424'; FullName: 'IBM EBCDIC Hebrew'),
    (CodePage: 20833; DotNETName: 'x-EBCDIC-KoreanExtended'; FullName: 'IBM EBCDIC Korean Extended'),
    (CodePage: 20838; DotNETName: 'IBM-Thai'; FullName: 'IBM EBCDIC Thai'),
    (CodePage: 20866; DotNETName: 'koi8-r'; FullName: 'Russian (KOI8-R); Cyrillic (KOI8-R)'),
    (CodePage: 20871; DotNETName: 'IBM871'; FullName: 'IBM EBCDIC Icelandic'),
    (CodePage: 20880; DotNETName: 'IBM880'; FullName: 'IBM EBCDIC Cyrillic Russian'),
    (CodePage: 20905; DotNETName: 'IBM905'; FullName: 'IBM EBCDIC Turkish'),
    (CodePage: 20924; DotNETName: 'IBM00924'; FullName: 'IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)'),
    (CodePage: 20932; DotNETName: 'EUC-JP'; FullName: 'Japanese (JIS 0208-1990 and 0212-1990)'),
    (CodePage: 20936; DotNETName: 'x-cp20936'; FullName: 'Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)'),
    (CodePage: 20949; DotNETName: 'x-cp20949'; FullName: 'Korean Wansung'),
    (CodePage: 21025; DotNETName: 'cp1025'; FullName: 'IBM EBCDIC Cyrillic Serbian-Bulgarian'),
    (CodePage: 21027; DotNETName: ''; FullName: ''), // (deprecated)
    (CodePage: 21866; DotNETName: 'koi8-u'; FullName: 'Ukrainian (KOI8-U); Cyrillic (KOI8-U)'),
    (CodePage: 28591; DotNETName: 'iso-8859-1'; FullName: 'ISO 8859-1 Latin 1; Western European (ISO)'),
    (CodePage: 28592; DotNETName: 'iso-8859-2'; FullName: 'ISO 8859-2 Central European; Central European (ISO)'),
    (CodePage: 28593; DotNETName: 'iso-8859-3'; FullName: 'ISO 8859-3 Latin 3'),
    (CodePage: 28594; DotNETName: 'iso-8859-4'; FullName: 'ISO 8859-4 Baltic'),
    (CodePage: 28595; DotNETName: 'iso-8859-5'; FullName: 'ISO 8859-5 Cyrillic'),
    (CodePage: 28596; DotNETName: 'iso-8859-6'; FullName: 'ISO 8859-6 Arabic'),
    (CodePage: 28597; DotNETName: 'iso-8859-7'; FullName: 'ISO 8859-7 Greek'),
    (CodePage: 28598; DotNETName: 'iso-8859-8'; FullName: 'ISO 8859-8 Hebrew; Hebrew (ISO-Visual)'),
    (CodePage: 28599; DotNETName: 'iso-8859-9'; FullName: 'ISO 8859-9 Turkish'),
    (CodePage: 28603; DotNETName: 'iso-8859-13'; FullName: 'ISO 8859-13 Estonian'),
    (CodePage: 28605; DotNETName: 'iso-8859-15'; FullName: 'ISO 8859-15 Latin 9'),
    (CodePage: 29001; DotNETName: 'x-Europa'; FullName: 'Europa 3'),
    (CodePage: 38598; DotNETName: 'iso-8859-8-i'; FullName: 'ISO 8859-8 Hebrew; Hebrew (ISO-Logical)'),
    (CodePage: 50220; DotNETName: 'iso-2022-jp'; FullName: 'ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)'),
    (CodePage: 50221; DotNETName: 'csISO2022JP'; FullName: 'ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)'),
    (CodePage: 50222; DotNETName: 'iso-2022-jp'; FullName: 'ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)'),
    (CodePage: 50225; DotNETName: 'iso-2022-kr'; FullName: 'ISO 2022 Korean'),
    (CodePage: 50227; DotNETName: 'x-cp50227'; FullName: 'ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)'),
    (CodePage: 50229; DotNETName: ''; FullName: 'ISO 2022 Traditional Chinese'),
    (CodePage: 50930; DotNETName: ''; FullName: 'EBCDIC Japanese (Katakana) Extended'),
    (CodePage: 50931; DotNETName: ''; FullName: 'EBCDIC US-Canada and Japanese'),
    (CodePage: 50933; DotNETName: ''; FullName: 'EBCDIC Korean Extended and Korean'),
    (CodePage: 50935; DotNETName: ''; FullName: 'EBCDIC Simplified Chinese Extended and Simplified Chinese'),
    (CodePage: 50936; DotNETName: ''; FullName: 'EBCDIC Simplified Chinese'),
    (CodePage: 50937; DotNETName: ''; FullName: 'EBCDIC US-Canada and Traditional Chinese'),
    (CodePage: 50939; DotNETName: ''; FullName: 'EBCDIC Japanese (Latin) Extended and Japanese'),
    (CodePage: 51932; DotNETName: 'euc-jp'; FullName: 'EUC Japanese'),
    (CodePage: 51936; DotNETName: 'EUC-CN'; FullName: 'EUC Simplified Chinese; Chinese Simplified (EUC)'),
    (CodePage: 51949; DotNETName: 'euc-kr'; FullName: 'EUC Korean'),
    (CodePage: 51950; DotNETName: ''; FullName: 'EUC Traditional Chinese'),
    (CodePage: 52936; DotNETName: 'hz-gb-2312'; FullName: 'HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)'),
    (CodePage: 54936; DotNETName: 'GB18030'; FullName: ''),
    (CodePage: 57002; DotNETName: 'x-iscii-de'; FullName: 'ISCII Devanagari'),
    (CodePage: 57003; DotNETName: 'x-iscii-be'; FullName: 'ISCII Bangla'),
    (CodePage: 57004; DotNETName: 'x-iscii-ta'; FullName: 'ISCII Tamil'),
    (CodePage: 57005; DotNETName: 'x-iscii-te'; FullName: 'ISCII Telugu'),
    (CodePage: 57006; DotNETName: 'x-iscii-as'; FullName: 'ISCII Assamese'),
    (CodePage: 57007; DotNETName: 'x-iscii-or'; FullName: 'ISCII Odia'),
    (CodePage: 57008; DotNETName: 'x-iscii-ka'; FullName: 'ISCII Kannada'),
    (CodePage: 57009; DotNETName: 'x-iscii-ma'; FullName: 'ISCII Malayalam'),
    (CodePage: 57010; DotNETName: 'x-iscii-gu'; FullName: 'ISCII Gujarati'),
    (CodePage: 57011; DotNETName: 'x-iscii-pa'; FullName: 'ISCII Punjabi'),
    (CodePage: 65000; DotNETName: 'utf-7'; FullName: 'Unicode (UTF-7)'),
    (CodePage: 65001; DotNETName: 'utf-8'; FullName: 'Unicode (UTF-8)')
  );


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
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
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
var
  i: integer;
begin
  cpnr.DotNETName := '';
  cpnr.FullName := '';

  for i := 0 to Length(ArrCodePages) - 1 do
    if ArrCodePages[i].CodePage = CP then
    begin
      cpnr.DotNETName := ArrCodePages[i].DotNETName;
      cpnr.FullName := ArrCodePages[i].FullName;
      Break;
    end;
end;



end.
