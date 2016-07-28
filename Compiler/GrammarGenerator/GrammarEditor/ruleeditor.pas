unit RuleEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    ComboBox1: TComboBox;
    RuleBox: TComboBox;
    DeleteRule: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    AddToRule: TButton;
    DeleteFromRule: TButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

end.

