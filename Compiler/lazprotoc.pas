program lazprotoc;

uses CompilerTypes, Lexer, DFA, sysutils;

var d: TFullIdentDFA;
  i: Integer;
begin
  d:=TFullIdentDFA.Create('a.');
  for i:=1 to Length('a.') do
    WriteLn(d.PerformStep);
  WriteLn(Copy(d.Token.Start, 1, d.Token.Len));
  d.Reset('a.b_');
  for i:=1 to Length('a.b:') do
    WriteLn(d.PerformStep);
  WriteLn(Copy(d.Token.Start, 1, d.Token.Len));   
  d.Reset('a._a');
  for i:=1 to Length('a.ba') do
    WriteLn(d.PerformStep);
  WriteLn(Copy(d.Token.Start, 1, d.Token.Len));
  d.Free;
end.

