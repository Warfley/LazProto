{
  Unit implementing context free grammas as a Set of Rules.
}
unit CFG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CompilerTypes;

type
  // Rules are Encoded as array, for A -> B | CD it is
  // Rules[A] := [B, C, D], Length of each rule is stored seperately
  TRuleData = array of word;

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
    property Rule[index: integer]: TRuleData read GetRule;
  end;

  TCFG = class
  private
    FRules: array of TRule;
  public
    {TODO: Implement AddRule}
    {TODO: Implement fi/fo/la Sets} 
    {TODO: Implement Checkrule} 
    {TODO: Implement GetAllRules}
    {TODO: Implement CheckLL1} 
    {TODO: Implement CheckSLR0}
    {TODO: Implement SLR0 Sets}
  end;

implementation

{ TRule }

constructor TRule.Create(InitRules: array of TRuleData);
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

function TRule.GetRule(i: integer): TRuleData;
begin
  SetLength(Result, FRulePos[i].Size);
  // to prevent rangecheck errors on size of 0 (gdb sucks)
  Move(PWord(PWord(FRules) + FRulePos[i].Position)^, PWord(Result),
       FRulePos[i].Size*SizeOf(Word));
end;

end.
