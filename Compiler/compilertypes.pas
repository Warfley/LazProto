unit CompilerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, gvector;

type
  // Terminal and non terminal symbols for DTA, 32768 each
  TNonTerminal = 0..32767;
  TTerminal = 32768..65535;

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
function isTerminal(val: Word): WordBool; inline;

const
  Epsilon: Word = Word(-1);
implementation

function GetTerminal(Tok: TToken): TTerminal;
begin
  Result:=ord(Tok) or -32768;
end;

function isTerminal(val: Word): WordBool;
begin
  Result:=WordBool(val and -32768);
end;

end.
