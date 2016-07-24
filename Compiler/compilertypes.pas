unit CompilerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, gvector;

type
  TToken = (tkKeyword, tkConst, tkSymbol, tkIdent);
  TConstType = (ctInt, ctFloat, ctBool, ctString);
  TSymbolType = (stAsg, stBracket, stEmpty);
  TLexTok = record
    Token: TToken;
    Start: PAnsiChar;
    Len: IntPtr;
    Tag: IntPtr;
  end;

  TLexOut = specialize TVector<TLexTok>;

implementation

end.

