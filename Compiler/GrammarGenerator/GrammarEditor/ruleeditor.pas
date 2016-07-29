unit RuleEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, contnrs, Math, CFG, StartSelector;

type

  { TRuleEditForm }

  TRuleEditForm = class(TForm)
    CreateGrammarButton: TButton;
    ElementSelector: TComboBox;
				OpenDialog1: TOpenDialog;
    RuleBox: TComboBox;
    DeleteRuleButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NTMBox: TListBox;
    RuleListBox: TListBox;
    AddToRuleButton: TButton;
    DeleteFromRuleButton: TButton;
    procedure AddToRuleButtonClick(Sender: TObject);
    procedure CreateGrammarButtonClick(Sender: TObject);
    procedure DeleteFromRuleButtonClick(Sender: TObject);
    procedure DeleteRuleButtonClick(Sender: TObject);
    procedure ElementSelectorKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NTMBoxClick(Sender: TObject);
    procedure RuleBoxChange(Sender: TObject);
  private
    FTerminals: TStrings;
    FNonTerminals: TStrings;
    FRules: TObjectList;
    FLastIndex, FLastRule: integer;
    procedure SetNonTerminals(AValue: TStrings);
    procedure SetTerminals(AValue: TStrings);
    { private declarations }
  public
    property Terminals: TStrings read FTerminals write SetTerminals;
    property NonTerminals: TStrings read FNonTerminals write SetNonTerminals;
    { public declarations }
  end;

var
  RuleEditForm: TRuleEditForm;

implementation

{$R *.lfm}

{ TRuleEditForm }

procedure TRuleEditForm.AddToRuleButtonClick(Sender: TObject);
begin
  if ElementSelector.ItemIndex >= 0 then
    RuleListBox.ItemIndex := RuleListBox.Items.Add(
      ElementSelector.Items[ElementSelector.ItemIndex]);
  ElementSelector.ItemIndex := -1;
  ElementSelector.SetFocus;
end;

procedure TRuleEditForm.CreateGrammarButtonClick(Sender: TObject);
function GetSymbol(str: String): TPushDownAlpha;
begin
  if str[1] = 'n' then
    Result:=FNonTerminals.IndexOf(str)
  else
    Result:=GetTerminal(FTerminals.IndexOf(str));
end;

var
  i, c, k: integer;
  r: TRuleDataArray;
  sl:TStringList;
begin
  StartSelector.StartSelectForm.StartSelector.Items.Assign(FNonTerminals);
  while StartSelector.StartSelectForm.StartSelector.ItemIndex < 0 do
    if StartSelector.StartSelectForm.ShowModal <> mrOk then
      Exit;
  with TCFG.Create do
    try
      Count:=FRules.Count;
      for i:=0 to FRules.Count-1 do
      begin
        SetLength(r, (FRules[i] as TObjectList).Count);
        for c:=0 to Length(r)-1 do
        begin
          SetLength(r[c], ((FRules[i] as TObjectList)[c] as TStringList).Count);
          for k:=0 to Length(r[c])-1 do
            r[c,k]:=GetSymbol(((FRules[i] as TObjectList)[c] as TStringList)[k]);
								end;
        AddRules(i, r);
						end;
      Start:=StartSelector.StartSelectForm.StartSelector.ItemIndex;
      if OpenDialog1.Execute then
      begin
        SaveToFile(OpenDialog1.FileName);
        sl:=TStringList.Create;
        try
          sl.Add(Format('unit %s;', [ExtractFileName(ChangeFileExt(OpenDialog1.FileName,'Constants'))]));
          sl.Add('');
          sl.Add('{$MODE OBJFP}{$H+}');
          sl.Add('');
          sl.Add('interface'); 
          sl.Add('');
          sl.Add('uses CFG;');  
          sl.Add('');
          sl.Add('type');
          sl.Add('  TToken = (');
          for i:=0 to FTerminals.Count-2 do
            sl.Add('    '+FTerminals[i]+',');
          if FTerminals.Count>1 then
            sl.Add('    '+FTerminals[FTerminals.Count-1]);
          sl.Add('  );');  
          sl.Add('  TNTTokens = (');
          for i:=0 to FNonTerminals.Count-2 do
            sl.Add('    '+FNonTerminals[i]+',');
          if FNonTerminals.Count>1 then
            sl.Add('    '+FNonTerminals[FNonTerminals.Count-1]);
          sl.Add('  );');
          sl.Add('');
          sl.Add('function TokenToTerminal(tok: TToken): TTerminal; inline;');
          sl.Add('function NTTokenToNonTerminal(tok: TNTToken): TTerminal; inline;');
          sl.Add('');
          sl.Add('implementation');
          sl.Add('');
          sl.Add('function TokenToTerminal(tok: TToken): TTerminal; inline;');
          sl.Add('begin');
          sl.Add('  Result := GetTerminal(ord(tok));');
          sl.Add('end;');
          sl.Add('');
          sl.Add('function NTTokenToNonTerminal(tok: TNTToken): TTerminal; inline;');  
          sl.Add('begin');
          sl.Add('  Result := ord(tok);');
          sl.Add('end;');
          sl.Add('');
          sl.Add('end.');
          sl.SaveToFile(ChangeFileExt(OpenDialog1.FileName, 'Constants.pas'));
								finally
          sl.Free;
        end;
						end;
    finally
      Free;
    end;
  Close;
end;

procedure TRuleEditForm.DeleteFromRuleButtonClick(Sender: TObject);
var
  i: integer;
begin
  i := RuleListBox.ItemIndex;
  if i >= 0 then
  begin
    RuleListBox.Items.Delete(i);
    RuleListBox.ItemIndex := Max(i, RuleListBox.Items.Count - 1);
  end;
end;

procedure TRuleEditForm.DeleteRuleButtonClick(Sender: TObject);
var
  i: integer;
begin
  (FRules[NTMBox.ItemIndex] as TObjectList).Delete(RuleBox.ItemIndex);
  FLastIndex := -1;
  FLastRule := -1;
  RuleBox.Items.Delete(RuleBox.ItemIndex);
  RuleBox.ItemIndex := -1;
  RuleBoxChange(RuleBox);
  for i := 0 to RuleBox.Items.Count - 2 do
    RuleBox.Items[i] := IntToStr(i);
end;

procedure TRuleEditForm.ElementSelectorKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = 13 then
    AddToRuleButtonClick(nil);
end;

procedure TRuleEditForm.FormCreate(Sender: TObject);
begin
  FRules := TObjectList.Create(True);
  FTerminals := TStringList.Create;
  FNonTerminals := TStringList.Create;
end;

procedure TRuleEditForm.FormDestroy(Sender: TObject);
begin
  FRules.Free;
  FTerminals.Free;
  FNonTerminals.Free;
end;

procedure TRuleEditForm.NTMBoxClick(Sender: TObject);
var
  i: integer;
begin
  RuleBox.ItemIndex := -1;
  RuleBoxChange(nil);
  RuleBox.Items.Clear;
  for i := 0 to (FRules[NTMBox.ItemIndex] as TObjectList).Count - 1 do
    RuleBox.Items.Add(IntToStr(i));
  RuleBox.Items.Add('(new)');
  FLastIndex := NTMBox.ItemIndex;
end;

procedure TRuleEditForm.RuleBoxChange(Sender: TObject);
var
  i: integer;
begin
  i := RuleBox.ItemIndex;
  if i < 0 then
  begin
    RuleListBox.Enabled := False;
    ElementSelector.Enabled := False;
    AddToRuleButton.Enabled := False;
    DeleteFromRuleButton.Enabled := False;
    DeleteRuleButton.Enabled := False;
  end
  else
  begin
    RuleListBox.Enabled := True;
    ElementSelector.Enabled := True;
    AddToRuleButton.Enabled := True;
    DeleteFromRuleButton.Enabled := True;
    DeleteRuleButton.Enabled := True;
    if RuleBox.Items[i] = '(new)' then
    begin
      RuleBox.Items.Insert(i, IntToStr(i));
      (FRules[NTMBox.ItemIndex] as TObjectList).Add(TStringList.Create);
      RuleBox.ItemIndex := i;
      RuleBoxChange(RuleBox);
      Exit;
    end;
  end;
  if FLastRule >= 0 then
    ((FRules[FLastIndex] as TObjectList)[FLastRule] as
      TStringList).Assign(RuleListBox.Items);
  FLastRule := i;
  if i >= 0 then
    RuleListBox.Items.Assign((FRules[NTMBox.ItemIndex] as TObjectList)[FLastRule] as
      TStringList)
  else
    RuleListBox.Clear;
end;

procedure TRuleEditForm.SetTerminals(AValue: TStrings);
begin
  FTerminals.Assign(AValue);
  ElementSelector.Items.Clear;
  ElementSelector.Items.AddStrings(FNonTerminals);
  ElementSelector.Items.AddStrings(FTerminals);
end;

procedure TRuleEditForm.SetNonTerminals(AValue: TStrings);
var
  i: integer;
begin
  FNonTerminals.Assign(AValue);
  NTMBox.Items.Assign(AValue);
  ElementSelector.Items.Clear;
  ElementSelector.Items.AddStrings(FNonTerminals);
  ElementSelector.Items.AddStrings(FTerminals);
  if AValue.Count > FRules.Count then
    for i := FRules.Count to AValue.Count - 1 do
      FRules.Add(TObjectList.Create(True))
  else
    for i := FRules.Count - 1 downto AValue.Count do
      FRules.Delete(i);
  NTMBox.ItemIndex := 0;
  FLastIndex := 0;
  FLastRule := -1;
end;

end.
