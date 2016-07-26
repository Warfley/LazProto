{
  Unit implementing context free grammas as a Set of Rules.
}
unit CFG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CompilerTypes, Math, gvector;

type
  ECFGException = Exception;

  // Rules are Encoded as array, for A -> B | CD it is
  // Rules[A] := [B, C, D], Length of each rule is stored seperately
  TRuleData = array of TPushDownAlpha;
  TRuleDataArray = array of TRuleData;

  TRulePos = record
    NTM: TNonTerminal;
    Index: IntPtr;
  end;

  TRuleArray = array of TRulePos;

  TRuleInfo = record
    Position, Size: IntPtr;
  end;

  { TRule data is Readonly, and only set on Create }
  TRule = class
  private
    FCount: IntPtr;
    FRulePos: array of TRuleInfo;
    FRules: TRuleData;

    function GetRule(i: integer): TRuleData;
    procedure DoCreate(InitRules: array of TRuleData);
  public
    constructor Create(InitRules: TRuleDataArray); overload;
    constructor Create(InitRules: array of TRuleData); overload;

    property Count: IntPtr read FCount;
    property Rule[index: integer]: TRuleData read GetRule; default;
  end;

  TCFG = class
  private
    FRules: array of TRule;
    function GetRules(a: TNonTerminal): TRule;
    procedure DoAddRule(LeftSide: TNonTerminal; RightSide: array of TRuleData);
    function DoFindRule(RightSide: array of TRuleData): TRuleArray;
  public
    destructor Destroy; override;
    procedure AddRules(LeftSide: TNonTerminal; RightSide: array of TRuleData); overload;
    procedure AddRules(LeftSide: TNonTerminal; RightSide: TRuleDataArray); overload;
    function CheckRule(a: TNonTerminal; r: TRuleData): TRulePos;
    function FindRule(RightSide: array of TRuleData): TRuleArray;
      overload;
    function FindRule(RightSide: TRuleDataArray): TRuleArray;
      overload;
    {TODO: Implement fi/fo/la Sets}
    {TODO: Implement Checkrule}
    {TODO: Implement GetAllRules}
    {TODO: Implement CheckLL1}
    {TODO: Implement CheckSLR0}
    {TODO: Implement SLR0 Sets}

    property Rules[Leftside: TNonTerminal]: TRule read GetRules; default;
  end;

function RulePos(NTM: TNonTerminal; Pos: IntPtr): TRulePos;

function CompareRules(a, b: TRuleData): boolean; inline;

implementation

function CompareRules(a, b: TRuleData): boolean; inline;
begin
  Result := CompareMem(Pointer(a), Pointer(b), Max(Length(a), Length(b)));
end;

function RulePos(NTM: TNonTerminal; Pos: IntPtr): TRulePos;
begin
  Result.Index := Pos;
  Result.NTM := NTM;
end;

{ TRule }

procedure TRule.DoCreate(InitRules: array of TRuleData);
var
  i: IntPtr;
  c, offset: IntPtr;
begin
  FCount := Length(InitRules);
  c := 0;
  // Counting elements
  for i := 0 to Count - 1 do
    Inc(c, Length(InitRules[i]));
  // Fill array
  SetLength(FRules, c);
  SetLength(FRulePos, Count);
  offset := 0;
  for i := 0 to Count - 1 do
  begin
    FRulePos[i].Position := offset;
    FRulePos[i].Size := Length(InitRules[i]);
    for c := 0 to Length(InitRules[i]) - 1 do
      FRules[offset + c] := InitRules[i, c];
    Inc(offset, Length(InitRules[i]));
  end;
end;

constructor TRule.Create(InitRules: array of TRuleData); overload;
begin
  DoCreate(InitRules);
end;

constructor TRule.Create(InitRules: TRuleDataArray); overload;
begin
  DoCreate(InitRules);
end;

function TRule.GetRule(i: integer): TRuleData;
begin
  SetLength(Result, FRulePos[i].Size);
  // to prevent rangecheck errors on size of 0 (gdb sucks)
  Move(PPushDownAlpha(PPushDownAlpha(FRules) + FRulePos[i].Position)^,
    PPushDownAlpha(Result),
    FRulePos[i].Size * SizeOf(TPushDownAlpha));
end;

{ TCFG }

destructor TCFG.Destroy;
var
  i: IntPtr;
begin
  for i := 0 to Length(FRules) - 1 do
    FRules[i].Free;
end;

procedure TCFG.DoAddRule(LeftSide: TNonTerminal; RightSide: array of TRuleData);
begin
  if isTerminal(LeftSide) then
    raise ECFGException.Create(Format('%d is not a non terminal symbol',
      [LeftSide]));
  if LeftSide >= Length(FRules) then
    SetLength(FRules, LeftSide + 1);
  FRules[LeftSide] := TRule.Create(RightSide);
end;

procedure TCFG.AddRules(LeftSide: TNonTerminal; RightSide: array of TRuleData);
begin
  DoAddRule(LeftSide, RightSide);
end;

procedure TCFG.AddRules(LeftSide: TNonTerminal; RightSide: TRuleDataArray);
begin
  DoAddRule(LeftSide, RightSide);
end;

function TCFG.GetRules(a: TNonTerminal): TRule;
begin
  if isTerminal(a) then
    raise ECFGException.Create(Format('%d is not a non terminal symbol', [a]))
  else if a >= Length(FRules) then
    Result := nil
  else
    Result := FRules[a];
end;

function TCFG.CheckRule(a: TNonTerminal; r: TRuleData): TRulePos;
var
  i: IntPtr;
begin
  if isTerminal(a) then
    raise ECFGException.Create(Format('%d is not a non terminal symbol', [a]));
  Result.Index := -1;
  if a >= Length(FRules) then
    Exit;
  for i := 0 to FRules[a].Count - 1 do
    if CompareRules(r, FRules[a].Rule[i]) then
    begin
      Result.NTM := a;
      Result.Index := i;
      Break;
    end;
end;


function TCFG.DoFindRule(RightSide: array of TRuleData): TRuleArray;
type
  TRList = specialize TVector<TRulePos>;
var
  i: TNonTerminal;
  r: TRuleData;
  p: TRulePos;
  l: TRList;
begin
  l := TRList.Create;
  try
    for i := 0 to Length(FRules) - 1 do
      for r in RightSide do
      begin
        p := CheckRule(i, r);
        if p.Index >= 0 then
          l.PushBack(p);
      end;
    SetLength(Result, l.Size);
    for i := 0 to l.Size - 1 do
      Result[i] := l[i];
  finally
    l.Free;
  end;
end;

function TCFG.FindRule(RightSide: array of TRuleData): TRuleArray;
begin
  Result := DoFindRule(RightSide);
end;

function TCFG.FindRule(RightSide: TRuleDataArray): TRuleArray;
begin
  Result := DoFindRule(RightSide);
end;

end.
