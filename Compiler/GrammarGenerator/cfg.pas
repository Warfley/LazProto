{
  Unit implementing context free grammas as a Set of Rules.
}
unit CFG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fgl, gvector, DOM, XMLRead, XMLWrite;

type
  ECFGException = Exception;  
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
  public
    constructor Create(InitRules: array of TRuleData);

    property Count: IntPtr read FCount;
    property Rule[index: integer]: TRuleData read GetRule; default;
  end;

  TTerminalSet = specialize TFPGList<TPushDownAlpha>;

  TLookAheadSet = array of array of TTerminalSet;

  TCFG = class
  private
    FRules: array of TRule;
    FStart: TNonTerminal;
    function GetRules(a: TNonTerminal): TRule;
    function AddToSet(i: TPushDownAlpha; s: TTerminalSet): boolean;
    procedure FillFi(inp: array of TPushDownAlpha; s: TTerminalSet;
      Checked: TTerminalSet);
    procedure FillFo(inp: TNonTerminal; s: TTerminalSet; Checked: TTerminalSet);
    procedure SetNTSCount(c: IntPtr);
    function GetNTSCount: IntPtr;
  public
    FLookAheadSet: TLookAheadSet;
    destructor Destroy; override;
    procedure AddRules(LeftSide: TNonTerminal; RightSide: array of TRuleData); overload;
    function CheckRule(a: TNonTerminal; r: TRuleData): TRulePos;
    function FindRule(RightSide: array of TRuleData): TRuleArray; overload;
    procedure Fi(inp: array of TPushDownAlpha; s: TTerminalSet);
    procedure Fo(inp: TNonTerminal; s: TTerminalSet);
    procedure ComputeLASets;
    procedure ClearLASets;
    procedure SaveToFile(f: TFilename);
    procedure LoadFromFile(f: TFilename);
    {Done: Implement fi/fo/la Sets}
    {TODO: Implement CheckLL1}
    {TODO: Implement CheckSLR0}
    {TODO: Implement SLR0 Sets}
    {DONE: Save/Load Grammar}
    {TODO: Exporter}
    {TODO: Create Recursive Descendence Parser}

    property Count: IntPtr read GetNTSCount write SetNTSCount;
    property Rules[Leftside: TNonTerminal]: TRule read GetRules; default;
    property Start: TNonTerminal read FStart write FStart;
  end;

function RulePos(NTM: TNonTerminal; Pos: IntPtr): TRulePos;

function CompareRules(a, b: TRuleData): boolean; inline;
function Concatinate(a, b: TRuleData): TRuleData;

function GetTerminal(val: IntPtr): TTerminal; inline;

{$ifdef CPU64}
function isTerminal(val: TPushDownAlpha): QWordBool; inline;
{$EndIf}
{$ifdef CPU32}
function isTerminal(val: TPushDownAlpha): LongBool; inline;
{$EndIf}
function TerminalToInt(val: TTerminal): IntPtr; inline;

const
  Epsilon: TPushDownAlpha = TPushDownAlpha(-1);

implementation 

function GetTerminal(val: IntPtr): TTerminal; inline;
begin
{$ifdef CPU64}
  Result:=val or IntPtr($8000000000000000);
{$EndIf}
{$ifdef CPU32}
  Result:=val or IntPtr($80000000);
{$EndIf}
end;

function TerminalToInt(val: TTerminal): IntPtr; inline;
begin
{$ifdef CPU64}
  Result:=val and not $8000000000000000;
{$EndIf}
{$ifdef CPU32}
  Result:=val and not $80000000;
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

function Concatinate(a, b: TRuleData): TRuleData;
begin
  SetLength(Result, Length(a) + Length(b));
  if Length(a) > 0 then
    Move(a[0], Result[0], Length(a) * SizeOf(TPushDownAlpha));
  if Length(b) > 0 then
    Move(b[0], Result[Length(a)], Length(b) * SizeOf(TPushDownAlpha));
end;

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

constructor TRule.Create(InitRules: array of TRuleData);
var
  i, c: IntPtr;
  offset: IntPtr;
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
    for c := 0 to FRulePos[i].Size - 1 do
      FRules[offset + c] := InitRules[i, c];
    Inc(offset, Length(InitRules[i]));
  end;
end;

function TRule.GetRule(i: integer): TRuleData;
begin
  SetLength(Result, FRulePos[i].Size);
  if FRulePos[i].Size > 0 then
    Move(FRules[FRulePos[i].Position], Result[0], FRulePos[i].Size *
      SizeOf(TPushDownAlpha));
end;

{ TCFG }

procedure TCFG.SaveToFile(f: TFilename);
var
  doc: TXMLDocument;
  NTMNode, ruleNode, elemNode: TDOMNode;
  i,c: Integer;
  v: TPushDownAlpha;
begin
  doc:=TXMLDocument.Create;
  try
    doc.AppendChild(doc.CreateElement('CFG'));
    TDOMElement(doc.DocumentElement).SetAttribute('Count', IntToStr(Length(FRules)));  
    TDOMElement(doc.DocumentElement).SetAttribute('Start', IntToStr(FStart));
    for i:=0 to Length(FRules)-1 do
    begin
      NTMNode:=doc.CreateElement('Rules');
      doc.DocumentElement.AppendChild(NTMNode);
      TDOMElement(NTMNode).SetAttribute('NTM', IntToStr(i));
      for c:=0 to FRules[i].Count-1 do
      begin
        ruleNode:=doc.CreateElement('Rule');
        NTMNode.AppendChild(ruleNode);
        for v in FRules[i].Rule[c] do
        begin
          elemNode:=doc.CreateElement('Item');
          ruleNode.AppendChild(elemNode);
				  TDOMElement(elemNode).SetAttribute('Terminal', BoolToStr(isTerminal(v), True));
				  TDOMElement(elemNode).SetAttribute('Value', IntToStr(ifthen(isTerminal(v), TerminalToInt(v), v)));
				end;
			end;
		end;
    WriteXML(doc, f);
	finally
    doc.Free;
	end;
end;

procedure TCFG.LoadFromFile(f: TFilename);
var
  newRules: TRuleDataArray;
  doc: TXMLDocument;
  NTMNode, ruleNode, elemNode: TDOMNode;
  i, c, k: Integer;
  NTM, tmp: TPushDownAlpha;
begin
  ReadXMLFile(doc, f);
  try
    Count:=StrToInt(doc.DocumentElement.GetAttribute('Count'));
    FStart:=StrToInt(doc.DocumentElement.GetAttribute('Start'));
    for i:=0 to doc.DocumentElement.ChildNodes.Count-1 do
    begin
      NTMNode:=doc.DocumentElement.ChildNodes[i];
      NTM:=StrToInt(TDOMElement(NTMNode).GetAttribute('NTM'));
      SetLength(newRules, NTMNode.ChildNodes.Count);
      for c:=0 to Length(newRules)-1 do
      begin
        ruleNode:=NTMNode.ChildNodes[c];
        SetLength(newRules[c], ruleNode.ChildNodes.Count);
        for k:=0 to Length(newRules[c])-1 do
        begin
          elemNode:=ruleNode.ChildNodes[k];
          tmp:=StrToInt(TDOMElement(elemNode).GetAttribute('Value'));
          if TDOMElement(elemNode).GetAttribute('Terminal') =  'True' then
            tmp:=GetTerminal(tmp);
          newRules[c][k]:=tmp;
				end;
			end;
      AddRules(NTM, newRules);
		end;
	finally
    doc.Free;
  end;
end;

procedure TCFG.ClearLASets;
var
		c, i: IntPtr;
begin
  for i := 0 to Length(FLookAheadSet) - 1 do
    for c := 0 to Length(FLookAheadSet[i]) - 1 do
      FLookAheadSet[i, c].Free;
end;

procedure TCFG.SetNTSCount(c: IntPtr);
var
  i: IntPtr;
begin
  if c < Length(FRules) then
    for i := c to Length(FRules) - 1 do
      FRules[i].Free;
  SetLength(FRules, c);
end;

function TCFG.GetNTSCount: IntPtr;
begin
  Result := Length(FRules);
end;

function TCFG.AddToSet(i: TPushDownAlpha; s: TTerminalSet): boolean;
begin
  Result := s.IndexOf(i) < 0;
  if Result then
    s.Add(i);
end;

procedure TCFG.FillFi(inp: array of TPushDownAlpha; s: TTerminalSet;
  Checked: TTerminalSet);
var
  r: TRule;
  i, c: IntPtr;
  l: TTerminalSet;
  a: array of TPushDownAlpha;
begin
  if Length(inp) = 0 then
  begin
    AddToSet(Epsilon, s);
    Exit;
  end;

  if isTerminal(inp[0]) then
  begin
    if inp[0] = Epsilon then
    begin
      SetLength(a, Length(inp) - 1);
      for i := 1 to Length(inp) - 1 do
        a[i - 1] := inp[i];
      FillFi(a, s, Checked);
      Exit;
    end
    else
    begin
      AddToSet(inp[0], s);
      Exit;
    end;
  end;

  if AddToSet(inp[0], Checked) then
  begin
    r := Rules[inp[0]];
    l := TTerminalSet.Create;
    try
      for i := 0 to r.Count - 1 do
      begin
        l.Clear;
        FillFi(r.Rule[i], l, Checked);
        for c := 0 to l.Count - 1 do
          if l[c] <> Epsilon then
            AddToSet(l[c], s);
        if l.IndexOf(Epsilon) >= 0 then
        begin
          SetLength(a, Length(inp) - 1);
          for c := 1 to Length(inp) - 1 do
            a[c - 1] := inp[c];
          FillFi(a, s, Checked);
        end;
      end;
    finally
      l.Free;
    end;
  end;
end;

procedure TCFG.FillFo(inp: TNonTerminal; s: TTerminalSet; Checked: TTerminalSet);

  function CheckFor(r: TRuleData; b: TNonTerminal; out beta: TRuleData): boolean;
  var
    i: IntPtr;
  begin
    Result := False;
    for i := 0 to Length(r) - 1 do
      if r[i] = b then
      begin
        SetLength(beta, Length(r) - (i + 1));
        if Length(beta) > 0 then
          Move(r[i + 1], beta[0], Length(beta) * SizeOf(TPushDownAlpha));
        Result := True;
        Break;
      end;
  end;

var
  i, c, d: IntPtr;
  r: TRule;
  b: TRuleData;
  fiSet: TTerminalSet;
begin
  if inp = FStart then
    s.Add(Epsilon);
  for i := 0 to Length(FRules) - 1 do
  begin
    r := FRules[i];
    for c := 0 to r.Count - 1 do
    begin
      if CheckFor(r.Rule[c], inp, b) then
      begin
        fiSet := TTerminalSet.Create;
        try
          Fi(b, fiSet);
          for d in fiSet do
            if (d = Epsilon) and AddToSet(i, Checked) then
              FillFo(i, s, Checked)
            else if (d <> Epsilon) then
              AddToSet(d, s);
        finally
          fiSet.Free;
        end;
      end;
    end;
  end;
end;

procedure TCFG.Fi(inp: array of TPushDownAlpha; s: TTerminalSet);
var
  l: TTerminalSet;
begin
  l := TTerminalSet.Create;
  try
    FillFi(inp, s, l);
  finally
    l.Free;
  end;
end;

procedure TCFG.Fo(inp: TNonTerminal; s: TTerminalSet);
var
  l: TTerminalSet;
begin
  l := TTerminalSet.Create;
  try
    FillFo(inp, s, l);
  finally
    l.Free;
  end;
end;

procedure TCFG.ComputeLaSets;
var
  i, c, x, y: IntPtr;
  fiSet, foSet: TTerminalSet;
begin
  fiSet := TTerminalSet.Create;
  foSet := TTerminalSet.Create;
  try
    SetLength(FLookAheadSet, Length(FRules));
    for i := 0 to Length(FRules) - 1 do
    begin
      foSet.Clear;
      Fo(i, foSet);
      SetLength(FLookAheadSet[i], FRules[i].Count);
      for c := 0 to FRules[i].Count - 1 do
      begin
        FLookAheadSet[i, c] := TTerminalSet.Create;
        fiSet.Clear;
        Fi(FRules[i].Rule[c], fiSet);
        for x in fiSet do
          for y in foSet do
            Fi([x, y], FLookAheadSet[i, c]);
      end;
    end;
  finally
    fiSet.Free;
    foSet.Free;
  end;
end;

destructor TCFG.Destroy;
var
  i: IntPtr;
begin
  for i := 0 to Length(FRules) - 1 do
    FRules[i].Free;
  ClearLASets;
end;

procedure TCFG.AddRules(LeftSide: TNonTerminal; RightSide: array of TRuleData);
begin
  if isTerminal(LeftSide) then
    raise ECFGException.Create(Format('%d is not a non terminal symbol',
      [LeftSide]));
  if LeftSide >= Length(FRules) then
    SetLength(FRules, LeftSide + 1);
  if Assigned(FRules[LeftSide]) then
    FRules[LeftSide].Free;
  FRules[LeftSide] := TRule.Create(RightSide);
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


function TCFG.FindRule(RightSide: array of TRuleData): TRuleArray;
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

end.
