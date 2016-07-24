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
    property CaseSense: boolean read GetCaseSense write SetCaseSense;
  end;

  { Implemented DFA's for lexical analysis of .proto files }

  { Keyword DFA: Accepts keywords set after construction }
  TKeywordDFA = class(TDFA)
  protected
    function GetToken: TLexTok; override;
  public
    procedure SetKeyword(Keyword: ansistring);
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
    constructor Create(InputString: PAnsiChar; UseCaseSense: boolean=True);
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
  m := Max(StartState, SinkState);
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
    SetStateCount(m);
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
  Result.Tag := 0;
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
  FLength:=0;
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
  Result.Token := tkKeyword;
end;

procedure TKeywordDFA.SetKeyword(Keyword: ansistring);
var
  i: integer;
begin
  FTransitions.StateCount := Length(Keyword) + 1;
  for i := 1 to Length(Keyword) do
    FTransitions.Transition[i - 1, Keyword[i]] := i;
  AddToFinalState(Length(Keyword));
end;

{ TSymbolDFA }

function TSymbolDFA.GetToken: TLexTok;
begin
  Result := inherited GetToken;
  Result.Token := tkSymbol;
  case FPosition^ of
  '<', '>', '(',')','{','}': Result.Tag:=Ord(stBracket);
  '=': Result.Tag:=Ord(stAsg);
  ';': Result.Tag:=ord(stEmpty);
  end;
end;

constructor TSymbolDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean = True);
begin
  inherited Create(InputString, UseCaseSense);
  FTransitions.AddTransition(0, 1, ['<', '>', '(', ')', '{', '}', ';', '=']);
  AddToFinalState(1);
end;

{ TStringDFA }

    function TStringDFA.GetToken: TLexTok;
    begin
  Result := inherited GetToken;
  Result.Token := tkConst;
  Result.Tag:=Ord(ctString);
    end;

    function TStringDFA.PerformStep: TDFAState;
    begin
      Result:=dsProductive;
      if FLength=0 then
      begin
        if FPosition^ <> '"' then
          FCurrentState:=-1;
      end
      else if (FPosition^=#10) or (FCurrentState=-1) then
      begin
        Result:=dsSink;  
          FCurrentState:=-1;
      end
      else if FPosition^ = '"' then
        Result:=dsFinal;
      inc(FLength);
      Inc(FPosition);
    end;

{ TBoolDFA }

        function TBoolDFA.GetToken: TLexTok;
        begin
          Result:=inherited GetToken;
          Result.Token:=tkSymbol;
          Result.Tag:=ord(ctBool);
        end;

        constructor TBoolDFA.Create(InputString: PAnsiChar; UseCaseSense: boolean=True);
        var c: Char;
          i: Integer;
        begin
          inherited Create(InputString, UseCaseSense);
          FTransitions.SetStateCount(10);
          i:=1;
          for c in 'true' do
          begin
            FTransitions.Transition[i-1, c]:=i;
            inc(i);
          end;  
            FTransitions.Transition[0, 'f']:=i;
            inc(i);
          for c in 'alse' do
          begin
            FTransitions.Transition[i-1, c]:=i;
            inc(i);
          end;
          AddToFinalState(4);
          AddToFinalState(9);
        end;

end.
