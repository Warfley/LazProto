unit ExampleGrammarConst;

{$MODE OBJFPC}{$H+}

interface
  uses CFG;

type 
  TToken = (tkPlus, tkMul, tkParenthOpen, tkParenthClose, tkA, tkB);
  TNTM = (ntE, ntE_, ntT, ntT_, ntF); 
	
procedure WriteRule(k: integer; r: TRule);  

implementation

procedure WriteRule(k: integer; r: TRule);  
var
  i, x: integer;
begin
  for i := 0 to r.Count - 1 do
  begin
    Write(TNTM(k), ' -> ');
    if Length(r.Rule[i])=0 then
    Write('(epsilon)');
    for x := 0 to Length(r.Rule[i]) - 1 do
      if isTerminal(r.Rule[i][x]) then
      begin
        if r.Rule[i][x]<>Epsilon then
          Write('(', TToken(TerminalToInt(r.Rule[i][x])), ')')
        else
          Write('(epsilon)');
			end
			else
        Write('(', TNTM(r.Rule[i][x]), ')');
    WriteLn();
  end;

end;  

end.