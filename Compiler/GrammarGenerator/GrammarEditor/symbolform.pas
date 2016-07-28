unit SymbolForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, math, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure TerminalAddClick(Sender: TObject);
    procedure TerminalDeleteClick(Sender: TObject);
    procedure NonTerminalAddClick(Sender: TObject);
    procedure NonTerminalDeleteClick(Sender: TObject);
    procedure TerminalEditEnter(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

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

{ TForm1 }

procedure TForm1.TerminalDeleteClick(Sender: TObject);
var i: Integer;
begin
  i:=TerminalBox.ItemIndex;
  if TerminalBox.ItemIndex>=0 then
    TerminalBox.Items.Delete(TerminalBox.ItemIndex);
  TerminalBox.ItemIndex:=Min(TerminalBox.Items.Count-1, i);
end;

procedure TForm1.NonTerminalAddClick(Sender: TObject);
begin
  if IsValid(NonTerminalEdit.Text) and (TerminalBox.Items.IndexOf('nt'+NonTerminalEdit.Text)<0) then
  begin
    NonTerminalBox.ItemIndex:=NonTerminalBox.Items.Add('nt'+NonTerminalEdit.Text);
    NonTerminalEdit.SelectAll;
    NonTerminalEdit.SetFocus;
  end;
end;

procedure TForm1.NonTerminalDeleteClick(Sender: TObject);
var i: Integer;
begin
  i:=NonTerminalBox.ItemIndex;
  if NonTerminalBox.ItemIndex>=0 then
    NonTerminalBox.Items.Delete(NonTerminalBox.ItemIndex);
  NonTerminalBox.ItemIndex:=Min(NonTerminalBox.Items.Count-1, i);
end;

procedure TForm1.TerminalEditEnter(Sender: TObject);
begin
  (Sender as TEdit).SelectAll;
end;

procedure TForm1.TerminalAddClick(Sender: TObject);
begin
  if IsValid(TerminalEdit.Text) and (TerminalBox.Items.IndexOf('tk'+TerminalEdit.Text)<0) then
  begin
    TerminalBox.ItemIndex:=TerminalBox.Items.Add('tk'+TerminalEdit.Text);
    TerminalEdit.SelectAll;
    TerminalEdit.SetFocus;
  end;
end;

end.

