unit CompilerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, gvector;

type
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
    Token: IntPtr;
    Start: PAnsiChar;
    Len,
    Attr: IntPtr;
  end;

  TLexOut = specialize TVector<TLexTok>;

implementation

end.
