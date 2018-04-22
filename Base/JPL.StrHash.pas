unit JPL.StrHash;

{
  FPHash - Functions from "fpcsrc\packages\fcl-base\src\contnrs.pp" (Free Pascal 3.1.1).
  xxHash32, Adler32 from "SynCommons.pas" (mORMot project http://synopse.info).
}


{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
  {$MACRO ON}
  {$COPERATORS ON}
{$ENDIF}
{$POINTERMATH ON}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}


{$region ' --- Defines from Synopse.inc --- '}

  {$ifdef FPC}


    {$INLINE ON}
    {$MINENUMSIZE 1}
    {$PACKSET 1}
    {$PACKENUM 1}
    {$CODEPAGE UTF8} // otherwise unexpected behavior occurs in most cases

    {$define HASINLINENOTX86}

    {$ifdef CPU64}
      {$define PUREPASCAL} // e.g. x64, AARCH64
      {$ifdef CPUX64}
        {$define CPUINTEL}
        {$ASMMODE INTEL} // as Delphi expects
      {$endif CPUX64}
    {$else}
      {$ifdef CPUARM}
        {$define PUREPASCAL} // ARM32
      {$endif CPUARM}
      {$ifdef CPUX86}
        {$define CPUINTEL}
        {$ASMMODE INTEL} // as Delphi expects
      {$endif CPUX86}
    {$endif CPU64}

  {$ELSE} // Delphi

    {$ifndef PUREPASCAL}
      {$define CPUINTEL} // Delphi only for Intel by now
    {$endif}
    {$ifdef CPUX64}
      {$define CPU64} // Delphi compiler for 64 bit CPU
      {$define CPU64DELPHI}
      {$undef CPU32}
      {$define PUREPASCAL}   // no x86 32 bit asm to be used
    {$else CPUX64}
      {$define CPU32} // Delphi compiler for 32 bit CPU
      {$define CPU32DELPHI}
      {$undef CPU64}
      {$define CPUX86} // for compatibility with older versions of Delphi
    {$endif CPUX64}

    {$ifdef NEWRTTINOTUSED}
      {$WEAKLINKRTTI ON}
      {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$endif NEWRTTINOTUSED}

  {$ENDIF}


  {$ifdef PUREPASCAL}
    {$define NODELPHIASM}
    {$define FPC_OR_PUREPASCAL}
  {$else}
  {$endif PUREPASCAL}

  {$R-} // disable Range checking in our code
  {$S-} // disable Stack checking in our code
  {$X+} // expect extended syntax
  {$W-} // disable stack frame generation
  {$Q-} // disable overflow checking in our code
  {$B-} // expect short circuit boolean
  {$V-} // disable Var-String Checking
  {$T-} // Typed @ operator
  {$Z1} // enumerators stored as byte by default
  {$IFNDEF FPC}
    {$P+} // Open string params
  {$ENDIF FPC}


{$endregion}

{$IFDEF FPC}{$WARN 7104 off : Use of -offset(%ebp) is not recommended for local variable access}{$ENDIF}

interface


Function FPHash(P: PChar; Len: Integer): LongWord; overload;
Function FPHash(const s: ShortString): LongWord; overload;

//function xxHash32(crc: cardinal; P: PAnsiChar; len: integer): cardinal;
function xxHash32(crc: cardinal; P: Pointer; len: integer): cardinal; // PAnsiChar -> Pointer
function Adler32(AKey: Pointer; ALength: integer): UInt32;


implementation


// from "fpcsrc\packages\fcl-base\src\contnrs.pp", line 1175
Function FPHash(P: PChar; Len: Integer): LongWord;
var
  pmax : PChar;
begin
{$IFDEF FPC}{$push}{$ENDIF}
{$Q-}
  Result := 0;
  pmax := P + Len;
  while P < pmax do
  begin
    Result := LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
    Inc(P);
  end;
{$IFDEF FPC}{$pop}{$ENDIF}
end;

Function FPHash(const s: ShortString):LongWord;
var
  p, pmax : PChar;
begin
{$IFDEF FPC}{$push}{$ENDIF}
{$Q-}
  Result := 0;
  p := @s[1];
  pmax := @s[length(s) + 1];
  while p < pmax do
    begin
      Result := LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
      Inc(p);
    end;
{$IFDEF FPC}{$pop}{$ENDIF}
end;





function Adler32(AKey: Pointer; ALength: integer): UInt32;
const
  MOD_ADLER = 65521;
var
  ABuffer: PByte {PUInt8} absolute AKey;
  a: UInt32; // = 1;
  b: UInt32; // = 0;
  n: Integer;
begin
  a := 1;
  b := 0;
  for n := 0 to ALength -1 do
  begin
    a := (a + ABuffer[n]) mod MOD_ADLER;
    b := (b + a) mod MOD_ADLER;
  end;
  Result := (b shl 16) or a;
end;




{$ifdef CPUINTEL} // use optimized x86/x64 asm versions for xxHash32

{$ifdef CPUX86}
//function xxHash32(crc: cardinal; P: PAnsiChar; len: integer): cardinal;
function xxHash32(crc: cardinal; P: Pointer; len: integer): cardinal;
asm
        xchg    edx, ecx
        push    ebp
        push    edi
        lea     ebp, [ecx+edx]
        push    esi
        push    ebx
        sub     esp, 8
        cmp     edx, 15
        mov     ebx, eax
        mov     dword ptr [esp], edx
        lea     eax, [ebx+165667B1H]
        jbe     @2
        lea     eax, [ebp-10H]
        lea     edi, [ebx+24234428H]
        lea     esi, [ebx-7A143589H]
        mov     dword ptr [esp+4H], ebp
        mov     edx, eax
        lea     eax, [ebx+61C8864FH]
        mov     ebp, edx
@1:     mov     edx, dword ptr [ecx]
        imul    edx, edx, -2048144777
        add     edi, edx
        rol     edi, 13
        imul    edi, edi, -1640531535
        mov     edx, dword ptr [ecx+4]
        imul    edx, edx, -2048144777
        add     esi, edx
        rol     esi, 13
        imul    esi, esi, -1640531535
        mov     edx, dword ptr [ecx+8]
        imul    edx, edx, -2048144777
        add     ebx, edx
        rol     ebx, 13
        imul    ebx, ebx, -1640531535
        mov     edx, dword ptr [ecx+12]
        lea     ecx, [ecx+16]
        imul    edx, edx, -2048144777
        add     eax, edx
        rol     eax, 13
        imul    eax, eax, -1640531535
        cmp     ebp, ecx
        jnc     @1
        rol     edi, 1
        rol     esi, 7
        rol     ebx, 12
        add     esi, edi
        mov     ebp, dword ptr [esp+4H]
        ror     eax, 14
        add     ebx, esi
        add     eax, ebx
@2:     lea     esi, [ecx+4H]
        add     eax, dword ptr [esp]
        cmp     ebp, esi
        jc      @4
        mov     ebx, esi
        nop
@3:     imul    edx, dword ptr [ebx-4H], -1028477379
        add     ebx, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, eax, 668265263
        cmp     ebp, ebx
        jnc     @3
        lea     edx, [ebp-4H]
        sub     edx, ecx
        mov     ecx, edx
        and     ecx, 0FFFFFFFCH
        add     ecx, esi
@4:     cmp     ebp, ecx
        jbe     @6
@5:     movzx   edx, byte ptr [ecx]
        add     ecx, 1
        imul    edx, edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, eax, -1640531535
        cmp     ebp, ecx
        jnz     @5
        nop
@6:     mov     edx, eax
        add     esp, 8
        shr     edx, 15
        xor     eax, edx
        imul    eax, eax, -2048144777
        pop     ebx
        pop     esi
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, eax, -1028477379
        pop     edi
        pop     ebp
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
end;
{$endif CPUX86}

{$ifdef CPUX64}
//function xxHash32(crc: cardinal; P: PAnsiChar; len: integer): cardinal;
function xxHash32(crc: cardinal; P: Pointer; len: integer): cardinal;
asm
        {$ifdef LINUX} // crc=rdi P=rsi len=rdx
        mov     r8, rdi
        mov     rcx, rsi
        {$else} // crc=r8 P=rcx len=rdx
        mov     r10, r8
        mov     r8, rcx
        mov     rcx, rdx
        mov     rdx, r10
        push    rsi   // Win64 expects those registers to be preserved
        push    rdi
        {$endif}
        // P=r8 len=rcx crc=rdx
        push    rbx
        lea     r10, [rcx+rdx]
        cmp     rdx, 15
        lea     eax, [r8+165667B1H]
        jbe     @2
        lea     rsi, [r10-10H]
        lea     ebx, [r8+24234428H]
        lea     edi, [r8-7A143589H]
        lea     eax, [r8+61C8864FH]
@1:     imul    r9d, dword ptr [rcx], -2048144777
        add     rcx, 16
        imul    r11d, dword ptr [rcx-0CH], -2048144777
        add     ebx, r9d
        lea     r9d, [r11+rdi]
        rol     ebx, 13
        rol     r9d, 13
        imul    ebx, ebx, -1640531535
        imul    edi, r9d, -1640531535
        imul    r9d, dword ptr [rcx-8H], -2048144777
        add     r8d, r9d
        imul    r9d, dword ptr [rcx-4H], -2048144777
        rol     r8d, 13
        imul    r8d, r8d, -1640531535
        add     eax, r9d
        rol     eax, 13
        imul    eax, eax, -1640531535
        cmp     rsi, rcx
        jnc     @1
        rol     edi, 7
        rol     ebx, 1
        rol     r8d, 12
        mov     r9d, edi
        ror     eax, 14
        add     r9d, ebx
        add     r8d, r9d
        add     eax, r8d
@2:     lea     r9, [rcx+4H]
        add     eax, edx
        cmp     r10, r9
        jc      @4
        mov     r8, r9
@3:     imul    edx, dword ptr [r8-4H], -1028477379
        add     r8, 4
        add     eax, edx
        ror     eax, 15
        imul    eax, eax, 668265263
        cmp     r10, r8
        jnc     @3
        lea     rdx, [r10-4H]
        sub     rdx, rcx
        mov     rcx, rdx
        and     rcx, 0FFFFFFFFFFFFFFFCH
        add     rcx, r9
@4:     cmp     r10, rcx
        jbe     @6
@5:     movzx   edx, byte ptr [rcx]
        add     rcx, 1
        imul    edx, edx, 374761393
        add     eax, edx
        rol     eax, 11
        imul    eax, eax, -1640531535
        cmp     r10, rcx
        jnz     @5
@6:     mov     edx, eax
        shr     edx, 15
        xor     eax, edx
        imul    eax, eax, -2048144777
        mov     edx, eax
        shr     edx, 13
        xor     eax, edx
        imul    eax, eax, -1028477379
        mov     edx, eax
        shr     edx, 16
        xor     eax, edx
        pop     rbx
        {$ifndef LINUX}
        pop     rdi
        pop     rsi
        {$endif}
end;
{$endif CPUX64}

{$else not CPUINTEL}

const
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;

{$ifdef FPC} // RolDWord is an intrinsic function under FPC :)
function Rol13(value: cardinal): cardinal; inline;
begin
  result := RolDWord(value, 13);
end;
{$else}
{$ifdef HASINLINENOTX86}
function RolDWord(value: cardinal; count: integer): cardinal; inline;
begin
  result := (value shl count) or (value shr (32-count));
end;

function Rol13(value: cardinal): cardinal;
begin
  result := (value shl 13) or (value shr 19);
end;
{$else}
function RolDWord(value: cardinal; count: integer): cardinal;
asm
      mov     cl, dl
      rol     eax, cl
end;

function Rol13(value: cardinal): cardinal;
asm
      rol     eax, 13
end;
{$endif HASINLINENOTX86}
{$endif FPC}

//function xxHash32(crc: cardinal; P: PAnsiChar; len: integer): cardinal;
function xxHash32(crc: cardinal; P: Pointer; len: integer): cardinal;
var c1, c2, c3, c4: cardinal;
    PLimit, PEnd: PAnsiChar;
begin
  PEnd := P + len;
  if len >= 16 then
    begin
      PLimit := PEnd - 16;
      c3 := crc;
      c2 := c3 + PRIME32_2;
      c1 := c2 + PRIME32_1;
      c4 := c3 - PRIME32_1;
      repeat
        c1 := PRIME32_1 * Rol13(c1 + PRIME32_2 * PCardinal(P)^);
        c2 := PRIME32_1 * Rol13(c2 + PRIME32_2 * PCardinal(P+4)^);
        c3 := PRIME32_1 * Rol13(c3 + PRIME32_2 * PCardinal(P+8)^);
        c4 := PRIME32_1 * Rol13(c4 + PRIME32_2 * PCardinal(P+12)^);
        inc(P, 16);
      until not (P <= PLimit);
      result := RolDWord(c1, 1) + RolDWord(c2, 7) + RolDWord(c3, 12) + RolDWord(c4, 18);
    end else
    result := crc + PRIME32_5;
  inc(result, len);
  while P <= PEnd - 4 do begin
    inc(result, PCardinal(P)^ * PRIME32_3);
    result := RolDWord(result, 17) * PRIME32_4;
    inc(P, 4);
  end;
  while P < PEnd do begin
    inc(result, PByte(P)^ * PRIME32_5);
    result := RolDWord(result, 11) * PRIME32_1;
    inc(P);
  end;
  result := result xor (result shr 15);
  result := result * PRIME32_2;
  result := result xor (result shr 13);
  result := result * PRIME32_3;
  result := result xor (result shr 16);
end;

{$endif CPUINTEL}








end.
