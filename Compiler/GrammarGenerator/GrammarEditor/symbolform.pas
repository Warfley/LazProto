unit SymbolForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, math, Types, RuleEditor;

type

  { TMainform }

  TMainform = class(TForm)
    Button1: TButton;
    TerminalAdd: TButton;
    TerminalDelete: TButton;
    NonTerminalAdd: TButton;
    NonTerminalDelete: TButton;
    TerminalEdit: TEdit;
    NonTerminalEdit: TEdit;
    TerminalGroup: TGroupBox;
    NonTerminalGroup: TGroupBox;
    TerminalBox: TListBox;
    NonTerminalBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
				procedure Button1Click(Sender: TObject);
				procedure FormShow(Sender: TObject);
				procedure NonTerminalEditKeyDown(Sender: TObject; var Key: Word;
							Shift: TShiftState);
				procedure NonTerminalEditKeyUp(Sender: TObject; var Key: Word;
							Shift: TShiftState);
    procedure TerminalAddClick(Sender: TObject);
    procedure TerminalDeleteClick(Sender: TObject);
    procedure NonTerminalAddClick(Sender: TObject);
    procedure NonTerminalDeleteClick(Sender: TObject);
    procedure TerminalEditEnter(Sender: TObject);
				procedure TerminalEditKeyDown(Sender: TObject; var Key: Word;
							Shift: TShiftState);
				procedure TerminalEditKeyUp(Sender: TObject; var Key: Word;
							Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Mainform: TMainform;

implementation
function isValid(s: String): Boolean;
  var c:Char;
begin
  Result:=Length(s)>0;
  for c in s do
    if not (c in ['a'..'z','A'..'Z','_','0'..'9']) then
      exit(False);
end;

{$R *.lfm}

{ TMainform }

procedure TMainform.TerminalDeleteClick(Sender: TObject);
var i: Integer;
begin
  i:=TerminalBox.ItemIndex;
  if TerminalBox.ItemIndex>=0 then
    TerminalBox.Items.Delete(TerminalBox.ItemIndex);
  TerminalBox.ItemIndex:=Min(TerminalBox.Items.Count-1, i);
end;

procedure TMainform.NonTerminalAddClick(Sender: TObject);
begin
  if IsValid(NonTerminalEdit.Text) and (NonTerminalBox.Items.IndexOf('nt'+NonTerminalEdit.Text)<0) then
  begin
    NonTerminalBox.ItemIndex:=NonTerminalBox.Items.Add('nt'+NonTerminalEdit.Text);
    NonTerminalEdit.SelectAll;
    NonTerminalEdit.SetFocus;
  end;
end;

procedure TMainform.NonTerminalDeleteClick(Sender: TObject);
var i: Integer;
begin
  i:=NonTerminalBox.ItemIndex;
  if NonTerminalBox.ItemIndex>=0 then
    NonTerminalBox.Items.Delete(NonTerminalBox.ItemIndex);
  NonTerminalBox.ItemIndex:=Min(NonTerminalBox.Items.Count-1, i);
end;

procedure TMainform.TerminalEditEnter(Sender: TObject);
begin
  (Sender as TEdit).SelectAll;
end;

procedure TMainform.TerminalEditKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
   if Key=9 then
     NonTerminalEdit.SetFocus;
end;

procedure TMainform.TerminalEditKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
   if Key=13 then
     TerminalAddClick(nil);
end;

procedure TMainform.TerminalAddClick(Sender: TObject);
begin
  if IsValid(TerminalEdit.Text) and (TerminalBox.Items.IndexOf('tk'+TerminalEdit.Text)<0) then
  begin
    TerminalBox.ItemIndex:=TerminalBox.Items.Add('tk'+TerminalEdit.Text);
    TerminalEdit.SelectAll;
    TerminalEdit.SetFocus;
  end;
end;

procedure TMainform.Button1Click(Sender: TObject);
begin
  if (NonTerminalBox.Items.Count=0) or (TerminalBox.Items.Count = 0) then exit;
  RuleEditForm.NonTerminals:=NonTerminalBox.Items;
  RuleEditForm.Terminals:=TerminalBox.Items;
  RuleEditForm.Width:=Width;
  RuleEditForm.Height:=Height;       
  RuleEditForm.Left:=Left;
  RuleEditForm.Top:=Top;
  Hide;
  RuleEditForm.ShowModal;
  Close;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
   TerminalEdit.SetFocus;
end;

procedure TMainform.NonTerminalEditKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
   if Key=9 then
     TerminalEdit.SetFocus;
end;

procedure TMainform.NonTerminalEditKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
begin
   if Key=13 then
     NonTerminalAddClick(nil);
end;

end.

