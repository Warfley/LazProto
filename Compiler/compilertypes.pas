unit CompilerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, gvector;

type
  TToken = (tkType, tkStringConst, tkBoolConst, tkIntConst, tkFloatConst,
  tkAssign, tkAngleBracket, tkBrace, tkParenthesis, tkEmpty, tkIdent, tkBlank);
  TLexTok = record
    Token: TToken;
    Start: PAnsiChar;
    Len,
    Attr: IntPtr;
  end;

  TLexOut = specialize TVector<TLexTok>;

implementation

end.

