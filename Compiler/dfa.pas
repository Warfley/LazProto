unit DFA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CompilerTypes, Math, AVL_Tree;

type
  EEOFException = Exception;

  TTransition = array of array[0..255] of IntPtr;

  {
    Hint: StateCount is the amount of productive states
    State numbers have to be positive, except for the sink state (-1 by default)

    Hint: CaseSense sets the case sensetivity, if it is set to true, the
    transitions set to the uppercase chars wont get called (they will not be deleted)
    While CaseSense is True no changes can be made to uppercase chars
  }

  TTransitionManager = class
  private
    FTransitions: TTransition;
    FSinkState: IntPtr;
    FCaseSense: boolean;
    function PerformTransition(StartState: IntPtr; Input: AnsiChar): IntPtr;
    procedure AddTrans(StartState: IntPtr; Input: AnsiChar; NewState: IntPtr);
    procedure SetSinkState(s: IntPtr);
    function GetStateCount: IntPtr;
    procedure SetStateCount(c: IntPtr);
  public
    procedure AddTransition(StartState, EndState: IntPtr; Input: array of AnsiChar);
    constructor Create(StateCount, SinkState: IntPtr);
    property Transition[StartState: IntPtr;
      Input: AnsiChar]: IntPtr read PerformTransition write AddTrans;
    property SinkState: IntPtr read FSinkState write SetSinkState;
    property StateCount: IntPtr read GetStateCount write setStateCount;
    property CaseSense: boolean read FCaseSense write FCaseSense;
  end;

  TDFAState = (dsProductive, dsFinal, dsSink);

  TDFA = class
  private
    function GetCaseSense: boolean;
    procedure SetCaseSense(c: boolean);
  protected
    FTransitions: TTransitionManager;
    FFinalStates: TAVLTree;
    FCurrentState: IntPtr;
    FPosition: PAnsiChar;
    FLength: IntPtr;
    FInitialState: IntPtr;
    procedure AddToFinalState(State: IntPtr);
    function GetToken: TLexTok; virtual;
  public
    function PerformStep: TDFAState; virtual;
    procedure Reset(InputPos: PAnsiChar); virtual;
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True); virtual;
    destructor Destroy; override;

    property Token: TLexTok read GetToken;
    property Position: PAnsiChar read FPosition write FPosition;
    property CaseSense: boolean read GetCaseSense write SetCaseSense;
  end;

  { Implemented DFA's for lexical analysis of .proto files }

  { Keyword DFA: Accepts keywords set after construction }
  TKeywordDFA = class(TDFA)
  private
    FTok: TToken;
    FAttr: IntPtr;
  protected
    function GetToken: TLexTok; override;
  public
    procedure SetKeyword(Keyword: ansistring; Tok: TToken; Attr: IntPtr);
  end;

  { Symbol DFA: Accepts =, <. >, (, ), {, }, ; }
  TSymbolDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True); override;
  end;

  { String DFA: Accepts ".*".
  To support unicode Strings this isn't a DFA and works different }
  TStringDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    function PerformStep: TDFAState; override;
  end;

  { Bool DFA: Matches "true" and "false" }
  TBoolDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
      override;
  end;

  { Int DFA: Accepts Hex (0xNum) Oct (0Num) and Dec Numbers
       (also directly reads their Value into Attr) }
  TIntDFA = class(TDFA)
  private
    FOct: boolean;
  protected
    function GetToken: TLexTok; override;
  public
    function PerformStep: TDFAState; override;
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
      override;
  end;

  { Float DFA: Accepts (+|-)Num.Num(e|E)(+|-)Num }
  TFloatDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
      override;
  end;

  { Ident DFA: Matches identefiers}
  TIdentDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
      override;
  end;

  { Blank DFA: Matches all Whitespace Characters }
  TBlankDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
      override;
  end;

implementation

{ TTransitionManager }

function TTransitionManager.PerformTransition(StartState: IntPtr;
  Input: AnsiChar): IntPtr;
begin
  if (not CaseSense) and (Input in ['A'..'Z']) then
    Input := chr(Ord(Input) - Ord('A') + Ord('a'));
  if StartState = FSinkState then
    Result := StartState
  else
    Result := FTransitions[StartState, Ord(Input)];
end;

procedure TTransitionManager.AddTrans(StartState: IntPtr; Input: AnsiChar;
  NewState: IntPtr);
var
  m: IntPtr;
begin
  if StartState < 0 then
    exit;
  if not CaseSense and (Input in ['A'..'Z']) then
    Input := chr(Ord(Input) - Ord('A') + Ord('a'));
  m := Max(StartState, NewState);
  if m >= Length(FTransitions) then
    SetStateCount(m + 1);
  FTransitions[StartState, Ord(Input)] := NewState;
end;

procedure TTransitionManager.SetSinkState(s: IntPtr);
var
  i, k: IntPtr;
begin
  for i := 0 to Length(FTransitions) do
    for k := 0 to 255 do
      if FTransitions[i, k] = FSinkState then
        FTransitions[i, k] := s;
  FSinkState := s;
end;

function TTransitionManager.GetStateCount: IntPtr;
begin
  Result := Length(FTransitions);
end;

procedure TTransitionManager.SetStateCount(c: IntPtr);
var
  oldLen, k: IntPtr;
begin
  oldLen := Length(FTransitions);
  SetLength(FTransitions, c);
  while oldLen < c do
  begin
    for k := 0 to 255 do
      FTransitions[oldLen, k] := FSinkState;
    Inc(oldLen);
  end;
end;

procedure TTransitionManager.AddTransition(StartState, EndState: IntPtr;
  Input: array of AnsiChar);
var
  c: AnsiChar;
  m: IntPtr;
begin
  if StartState < 0 then
    exit;
  m := Max(StartState, EndState);
  if m >= Length(FTransitions) then
    SetStateCount(m + 1);
  for c in Input do
    if not CaseSense and (c in ['A'..'Z']) then
      FTransitions[StartState, Ord(c) - Ord('A') + Ord('a')] := EndState
    else
      FTransitions[StartState, Ord(c)] := EndState;
end;

constructor TTransitionManager.Create(StateCount, SinkState: IntPtr);
begin
  FSinkState := SinkState;
  SetStateCount(StateCount);
end;

{ TDFA }

function TDFA.GetCaseSense: boolean;
begin
  Result := FTransitions.CaseSense;
end;

procedure TDFA.SetCaseSense(c: boolean);
begin
  FTransitions.CaseSense := c;
end;

function TDFA.GetToken: TLexTok;
begin
  Result.Attr := 0;
  Result.Token := tkIdent;
  Result.Len := FLength;
  Result.Start := FPosition - FLength;
end;

procedure TDFA.AddToFinalState(State: IntPtr);
begin
  FFinalStates.Add(Pointer(State));
end;

function TDFA.PerformStep: TDFAState;
begin
  if FPosition^ = #00 then
    raise EEOFException.Create('End of file reached');
  FCurrentState := FTransitions.Transition[FCurrentState, FPosition^];
  Inc(FPosition);
  Inc(FLength);
  if Assigned(FFinalStates.Find(Pointer(FCurrentState))) then
    Result := dsFinal
  else if FCurrentState = FTransitions.SinkState then
    Result := dsSink
  else
    Result := dsProductive;
end;

procedure TDFA.Reset(InputPos: PAnsiChar);
begin
  FPosition := InputPos;
  FLength := 0;
  FCurrentState := FInitialState;
end;

constructor TDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
begin
  FFinalStates := TAVLTree.Create;
  FInitialState := 0;
  FTransitions := TTransitionManager.Create(1, -1);
  FTransitions.CaseSense := UseCaseSense;
  FPosition := InputString;
  FLength := 0;
end;

destructor TDFA.Destroy;
begin
  FFinalStates.Free;
  FTransitions.Free;
end;

{ TKeywordDFA }

function TKeywordDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := FTok;
  Result.Attr := FAttr;
end;

procedure TKeywordDFA.SetKeyword(Keyword: ansistring; Tok: TToken; Attr: IntPtr);
var
  i: integer;
begin
  FTransitions.StateCount := Length(Keyword) + 1;
  FTok := Tok;
  FAttr := Attr;
  for i := 1 to Length(Keyword) do
    FTransitions.Transition[i - 1, Keyword[i]] := i;
  AddToFinalState(Length(Keyword));
end;

{ TSymbolDFA }

function TSymbolDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  case Result.Start^ of
    '<':
    begin
      Result.Token := tkAngleBracket;
      Result.Attr := 0; // opend
    end;
    '>':
    begin
      Result.Token := tkAngleBracket;
      Result.Attr := 1; // closed
    end;
    '(':
    begin
      Result.Token := tkParenthesis;
      Result.Attr := 0; // opend
    end;
    ')':
    begin
      Result.Token := tkParenthesis;
      Result.Attr := 1; // closed
    end;   
    '[':
    begin
      Result.Token := tkBracket;
      Result.Attr := 0; // opend
    end;
    ']':
    begin
      Result.Token := tkBracket;
      Result.Attr := 1; // closed
    end;
    '{':
    begin
      Result.Token := tkBrace;
      Result.Attr := 0; // opend
    end;
    '}':
    begin
      Result.Token := tkBrace;
      Result.Attr := 1; // closed
    end;
    '=': Result.Token := tkAssign;
    ';': Result.Token := tkSemicolon;
    ',': Result.Token:=tkComma;
  end;
end;

constructor TSymbolDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
begin
  inherited Create(InputString, UseCaseSense);
  FTransitions.SetStateCount(2);
  FTransitions.AddTransition(0, 1, ['<', '>', '(', ')', '{', '}', ';', '=', ',']);
  AddToFinalState(1);
end;

{ TStringDFA }

function TStringDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkStringConst;
end;

function TStringDFA.PerformStep: TDFAState;
begin
  Result := dsProductive;
  if FLength = 0 then
  begin
    if FPosition^ <> '"' then
      FCurrentState := -1;
  end
  else if (FPosition^ = #10) or (FCurrentState = -1) then
  begin
    Result := dsSink;
    FCurrentState := -1;
  end
  else if FPosition^ = '"' then
  begin
    Result := dsFinal;
    FCurrentState:=-1;
	end;
	Inc(FLength);
  Inc(FPosition);
end;

{ TBoolDFA }

function TBoolDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkBoolConst;
end;

constructor TBoolDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
var
  c: char;
  i: integer;
begin
  inherited Create(InputString, UseCaseSense);
  FTransitions.SetStateCount(10);
  i := 1;
  for c in 'true' do
  begin
    FTransitions.Transition[i - 1, c] := i;
    Inc(i);
  end;
  FTransitions.Transition[0, 'f'] := i;
  Inc(i);
  for c in 'alse' do
  begin
    FTransitions.Transition[i - 1, c] := i;
    Inc(i);
  end;
  AddToFinalState(4);
  AddToFinalState(9);
end;

{ TIntDFA }

function TIntDFA.GetToken: TLexTok;
var
  s: string;
begin
  Result := inherited GetToken;
  Result.Token := tkIntConst;
  if FOct then
  begin
    if Result.Start^ = '0' then
      s := '&' + Copy(Result.Start, 2, FLength - 1)
    else
      s := Result.Start^ + '&' + Copy(Result.Start, 3, FLength - 2);
  end
  else
    s := Copy(Result.Start, 1, FLength);
  Result.Attr := StrToInt(s);
end;

function TIntDFA.PerformStep: TDFAState;
begin
  Result := inherited PerformStep;
  FOct := FCurrentState = 3;
end;

constructor TIntDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
begin
  inherited Create(InputString, False);
  FOct := False;
  FTransitions.StateCount := 7;
  // +/-
  FTransitions.AddTransition(0, 6, ['+', '-']);
  // Dec Number
  FTransitions.AddTransition(0, 1, ['1', '2', '3', '4', '5', '6', '7', '8', '9']);
  // +/-
  FTransitions.AddTransition(6, 1, ['1', '2', '3', '4', '5', '6', '7', '8', '9']);
  FTransitions.AddTransition(1, 1, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  AddToFinalState(1);
  // Oct & Hex
  FTransitions.Transition[0, '0'] := 2;
  // +/-
  FTransitions.Transition[6, '0'] := 2;
  AddToFinalState(2);
  // Oct
  FTransitions.AddTransition(2, 3, ['0', '1', '2', '3', '4', '5', '6', '7']);
  FTransitions.AddTransition(3, 3, ['0', '1', '2', '3', '4', '5', '6', '7']);
  AddToFinalState(3);
  // Hex
  FTransitions.Transition[2, 'x'] := 4;
  FTransitions.AddTransition(4, 5,
    ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']);
  FTransitions.AddTransition(5, 5,
    ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']);
  AddToFinalState(5);
end;

{ TFloatDFA }

function TFloatDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkFloatConst;
end;

constructor TFloatDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
begin
  inherited Create(InputString, False);
  FTransitions.SetStateCount(14);
  // +/-
  FTransitions.AddTransition(0, 1, ['+', '-']);
  // dec.[dec][EDec] and decExpDec
  FTransitions.AddTransition(0, 2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  FTransitions.AddTransition(1, 2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  // Any dec before .
  FTransitions.AddTransition(2, 2, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  FTransitions.Transition[2, '.'] := 3;
  // .[dec][EDec]
  FTransitions.Transition[0, '.'] := 4;
  FTransitions.Transition[1, '.'] := 4;
  FTransitions.AddTransition(4, 3, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  // Stay in State 3 for any new digit
  FTransitions.AddTransition(3, 3, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  // State 3 is final state
  AddToFinalState(3);

  //Exp State
  FTransitions.Transition[3, 'e'] := 5;
  FTransitions.Transition[2, 'e'] := 5;
  FTransitions.AddTransition(5, 6, ['+', '-']);
  FTransitions.AddTransition(5, 7, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  FTransitions.AddTransition(8, 7, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  FTransitions.AddTransition(7, 7, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
  AddToFinalState(7);

  // nan
  FTransitions.Transition[0, 'n'] := 8;
  FTransitions.Transition[1, 'n'] := 8;
  FTransitions.Transition[8, 'a'] := 9;
  FTransitions.Transition[9, 'n'] := 10;
  AddToFinalState(10);
  // inf
  FTransitions.Transition[0, 'i'] := 11;
  FTransitions.Transition[1, 'i'] := 11;
  FTransitions.Transition[11, 'n'] := 12;
  FTransitions.Transition[12, 'f'] := 13;
  AddToFinalState(13);
end;

{ TIdentDFA }

function TIdentDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkIdent;
  Result.Attr := ifthen(Pos('.', Copy(Result.Start, 1, Result.Len)) > 0, 1, 0);
end;

constructor TIdentDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
var
  c: TAnsiChar;
begin
  inherited Create(InputString, UseCaseSense);
  FTransitions.SetStateCount(3);
  for c in ['A'..'Z', 'a'..'z'] do
  begin
    FTransitions.Transition[0, c] := 1;
    FTransitions.Transition[1, c] := 1;
    FTransitions.Transition[2, c] := 1;
  end;
  FTransitions.Transition[0, '.'] := 1;
  FTransitions.AddTransition(1, 1, ['0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', '_']);
  AddToFinalState(1);
  FTransitions.Transition[1, '.'] := 2;
end;

{ TBlankDFA }

function TBlankDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkBlank;
end;

constructor TBlankDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
var
  c: TAnsiChar;
begin
  inherited Create(InputString, UseCaseSense);
  FTransitions.SetStateCount(2);
  for c in [#1..#32] do
  begin
    FTransitions.Transition[0, c] := 1;
    FTransitions.Transition[1, c] := 1;
  end;
  AddToFinalState(1);
end;

end.
