unit CompilerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, gvector;

type
  // Terminal and non terminal symbols for DTA, Sizeof(Pointer)*8-1 Bit
  // First bit to determine if its TS(1) or NTS(0)
  PPushDownAlpha = ^TPushDownAlpha;
  TPushDownAlpha = IntPtr;

{$ifdef CPU64}    
  TNonTerminal = 0..$7FFFFFFFFFFFFFFF;
  TTerminal = IntPtr($8000000000000000)..IntPtr($FFFFFFFFFFFFFFFF);
{$EndIf}
{$ifdef CPU32}
  TNonTerminal = 0..$7FFFFFFF;
  TTerminal = IntPtr($80000000)..IntPtr($FFFFFFFF);
{$EndIf}

    {TODO: Add NonTerminal Constants}

  TToken = (
    // Keword Types
    tkKeyType, tkKeySyntax, tkKeyImport, tkKeyWeak,
    tkKeyPublic, tkKeyPackage, tkKeyOption, tkKeyRepeated, tkKeyOneOf, tkKeyMap,
    tkKeyReserved, tkKeyEnum, tkKeyMessage, tkKeyService, tkKeyRPC, tkKeyStream,
    tkKeyReturns,
    // Constant Types
    tkStringConst, tkBoolConst, tkIntConst, tkFloatConst,
    // Symbol Types
    tkAssign, tkAngleBracket, tkBracket, tkBrace, tkParenthesis, tkComma, tkSemicolon,
    // Other
    tkIdent, tkBlank, tkEOF);
  TTypeInfo = (tiDouble, tiFloat, tiInt32, tiInt64, tiUInt32, tiUint64,
    tiSInt32, tiSInt64, tiFixed32, tiFixed64, tiSFixed32, tiSFixed64,
    tiBool, tiString, tiBytes);

  TLexTok = record
    Token: TToken;
    Start: PAnsiChar;
    Len,
    Attr: IntPtr;
  end;

  TLexOut = specialize TVector<TLexTok>;

function GetTerminal(Tok: TToken): TTerminal; inline;

{$ifdef CPU64}
function isTerminal(val: TPushDownAlpha): QWordBool; inline; 
{$EndIf}
{$ifdef CPU32}
function isTerminal(val: TPushDownAlpha): LongBool; inline;
{$EndIf} 
function GetToken(val: TTerminal): TToken;

const
  Epsilon: TPushDownAlpha = TPushDownAlpha(-1);
implementation

function GetTerminal(Tok: TToken): TTerminal;
begin
{$ifdef CPU64}
  Result:=ord(Tok) or IntPtr($8000000000000000);  
{$EndIf}
{$ifdef CPU32}
  Result:=ord(Tok) or IntPtr($80000000);
{$EndIf}
end;

function GetToken(val: TTerminal): TToken;
begin
{$ifdef CPU64}
  Result:=TToken(val and not $8000000000000000);
{$EndIf}
{$ifdef CPU32}
  Result:=TToken(val and not $80000000);
{$EndIf}
end;

{$ifdef CPU64}
function isTerminal(val: TPushDownAlpha): QWordBool; inline;   
{$EndIf}
{$ifdef CPU32}
function isTerminal(val: TPushDownAlpha): LongBool; inline;
{$EndIf}
begin
{$ifdef CPU64}
  Result:=QWordBool(val and $8000000000000000);    
{$EndIf}
{$ifdef CPU32}
  Result:=QWordBool(val and $80000000);
{$EndIf}
end;

end.
