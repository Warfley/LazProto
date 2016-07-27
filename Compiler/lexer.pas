unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DFA, CompilerTypes, contnrs;

type
  ELexException = Exception;
  TTokenFoundEvent = procedure(Sender: TObject; Tok: TLexTok) of object;
  { Lexer Class, implements a backtracking DFA to generate
    a lexical LFM-Analysis }
  TLexer = class
  private
    FAutomatas: TObjectList;
    FLastTok: TLexTok;
    FHead: PansiChar;
    FBackTrackHead: PAnsiChar;

    FOnTokenFound: TTokenFoundEvent;

    procedure SetHead(s: PAnsiChar);
    procedure BackTrack;
    function PerformStep: boolean;
  public
    procedure Run;
    constructor Create;
    destructor Destroy; override;

    property Head: PAnsiChar read FHead write SetHead;
    property OnTokenFound: TTokenFoundEvent read FOnTokenFound write FOnTokenFound;
  end;

implementation

procedure TLexer.SetHead(s: PAnsiChar);
var
  o: Pointer;
begin
  for o in FAutomatas do
    TDFA(o).Reset(s);
  FHead := s;
  FBackTrackHead := nil;
end;

procedure TLexer.BackTrack;
begin
  if Assigned(FOnTokenFound) then
    FOnTokenFound(Self, FLastTok);
  SetHead(FBackTrackHead);
end;

function TLexer.PerformStep: boolean;
var
  s: TDFAState;
  o: Pointer;
begin
  Result := False;
  for o in FAutomatas do
    with TDFA(o) do
    begin
      s := PerformStep;
      Result := Result or (s <> dsSink);
      if (s = dsFinal) and (FBackTrackHead <> Position) then
      begin
        FBackTrackHead := Position;
        FLastTok := Token;
      end;
    end;
end;

procedure TLexer.Run;
var
  t: TLexTok;
begin
  while FHead^ <> #00 do
    if PerformStep then
      Inc(FHead)
    else if Assigned(FBackTrackHead) then
      BackTrack
    else
      raise ELexException.Create('Token not recognized');
  if FHead = FBackTrackHead then
    BackTrack
  else
    raise ELexException.Create('Unexpected EOF');
  t.Token := Ord(tkEOF);
  t.Len := 0;
  t.Start := nil;
  t.Attr := 0;
  if Assigned(FOnTokenFound) then
    FOnTokenFound(self, t);
end;

constructor TLexer.Create;
function CreateKA(key: String; tok: TToken; attr:IntPtr): TKeywordDFA;
begin
  Result:=TKeywordDFA.Create(nil);
  Result.SetKeyword(key, tok, attr);
end;
begin
  FAutomatas := TObjectList.Create(True);
  // Types
  FAutomatas.Add(CreateKA('double', tkKeyType, ord(tiDouble)));
  FAutomatas.Add(CreateKA('float', tkKeyType, ord(tiFloat)));   
  FAutomatas.Add(CreateKA('int32', tkKeyType, ord(tiInt32)));
  FAutomatas.Add(CreateKA('int64', tkKeyType, ord(tiInt64)));
  FAutomatas.Add(CreateKA('uint32', tkKeyType, ord(tiUInt32)));
  FAutomatas.Add(CreateKA('uint64', tkKeyType, ord(tiUint64)));
  FAutomatas.Add(CreateKA('sint32', tkKeyType, ord(tiSInt32)));
  FAutomatas.Add(CreateKA('sint64', tkKeyType, ord(tiSInt64)));
  FAutomatas.Add(CreateKA('fixed32', tkKeyType, ord(tiFixed32)));
  FAutomatas.Add(CreateKA('fixed64', tkKeyType, ord(tiFixed64)));
  FAutomatas.Add(CreateKA('sfixed32', tkKeyType, ord(tiSFixed32)));
  FAutomatas.Add(CreateKA('sfixed64', tkKeyType, ord(tiSFixed64)));
  FAutomatas.Add(CreateKA('bool', tkKeyType, ord(tiBool)));
  FAutomatas.Add(CreateKA('string', tkKeyType, ord(tiString)));
  FAutomatas.Add(CreateKA('bytes', tkKeyType, ord(tiBytes)));
  // Keywords
  FAutomatas.Add(CreateKA('syntax', tkKeySyntax, 0));
  FAutomatas.Add(CreateKA('import', tkKeyImport, 0));
  FAutomatas.Add(CreateKA('weak', tkKeyWeak, 0));
  FAutomatas.Add(CreateKA('public', tkKeyPublic, 0));
  FAutomatas.Add(CreateKA('package', tkKeyPackage, 0));
  FAutomatas.Add(CreateKA('option', tkKeyOption, 0));
  FAutomatas.Add(CreateKA('repeated', tkKeyRepeated, 0));
  FAutomatas.Add(CreateKA('oneof', tkKeyOneOf, 0));
  FAutomatas.Add(CreateKA('map', tkKeyMap, 0));
  FAutomatas.Add(CreateKA('reserved', tkKeyReserved, 0));
  FAutomatas.Add(CreateKA('enum', tkKeyEnum, 0));
  FAutomatas.Add(CreateKA('message', tkKeyMessage, 0));
  FAutomatas.Add(CreateKA('service', tkKeySyntax, 0));
  FAutomatas.Add(CreateKA('rpc', tkKeyRPC, 0));
  FAutomatas.Add(CreateKA('stream', tkKeyStream, 0));
  FAutomatas.Add(CreateKA('returns', tkKeyReturns, 0));
  FAutomatas.Add(TSymbolDFA.Create(nil));
  FAutomatas.Add(TStringDFA.Create(nil));
  FAutomatas.Add(TBoolDFA.Create(nil));
  FAutomatas.Add(TIntDFA.Create(nil));
  FAutomatas.Add(TFloatDFA.Create(nil));
  FAutomatas.Add(TIdentDFA.Create(nil));
  FAutomatas.Add(TBlankDFA.Create(nil));
  FBackTrackHead := nil;
end;

destructor TLexer.Destroy;
begin
  FAutomatas.Free;
end;

end.
