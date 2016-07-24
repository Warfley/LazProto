program lazprotoc;

uses CompilerTypes, Lexer, DFA;

var d: TBoolDFA;
  i: Integer;
begin
  d:=TBoolDFA.Create('truee', False);
  for i:=1 to Length('truee') do
    WriteLn(d.PerformStep);
  Write('(');
  Write(d.Token.Token);
  WriteLn(', '+Copy(d.Token.Start,1, d.Token.Len)+')') ;
  d.Free;
end.

