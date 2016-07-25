unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DFA, CompilerTypes, contnrs;

type
  ELexException = Exception;
  TTokenFoundEvent = procedure (Sender: TObject; Tok: TLexTok) of object;
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
    function PerformStep: Boolean;
  public
    procedure Run;
    constructor Create;
    destructor Destroy; override;

    property Head: PAnsiChar read FHead write SetHead;
    property OnTokenFound: TTokenFoundEvent read FOnTokenFound write FOnTokenFound;
  end;

implementation

    procedure TLexer.SetHead(s: PAnsiChar);
    var o: Pointer;
    begin
      for o in FAutomatas do
        TDFA(o).Reset(s);
      FHead:=s;
      FBackTrackHead:=Nil;
		end;

    procedure TLexer.BackTrack;
    begin
      if Assigned(FOnTokenFound) then
        FOnTokenFound(Self, FLastTok);
      SetHead(FBackTrackHead);
		end;

    function TLexer.PerformStep: Boolean;
    var s: TDFAState;
      o: Pointer;
    begin
      Result:=False;
      for o in FAutomatas do
        with TDFA(o) do
        begin
          s:=PerformStep;
          Result:=Result or (s<>dsSink);
          if (s=dsFinal) and (FBackTrackHead<>Position) then
          begin
            FBackTrackHead:=Position;
            FLastTok:=Token;
					end;
				end;
		end;

    procedure TLexer.Run;
    begin
      while FHead^<>#00 do
        if PerformStep then
          inc(FHead)
        else if Assigned(FBackTrackHead) then
          BackTrack
				else
          raise ELexException.Create('Token not recognized');
      if FHead=FBackTrackHead then BackTrack
      else raise ELexException.Create('Unexpected EOF');
		end;

    constructor TLexer.Create;
    begin
      FAutomatas:=TObjectList.Create(True);
      FAutomatas.Add(TSymbolDFA.Create(nil));
      FAutomatas.Add(TStringDFA.Create(nil));
      FAutomatas.Add(TBoolDFA.Create(nil));
      FAutomatas.Add(TIntDFA.Create(nil));
      FAutomatas.Add(TFloatDFA.Create(nil));
      FAutomatas.Add(TIdentDFA.Create(nil));
      FAutomatas.Add(TBlankDFA.Create(nil));
      FBackTrackHead:=nil;
		end;

    destructor TLexer.Destroy;
    begin
      FAutomatas.Free;
		end;

end.
