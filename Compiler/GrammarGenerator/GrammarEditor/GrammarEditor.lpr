program GrammarEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SymbolForm, RuleEditor, StartSelector
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
			Application.CreateForm(TMainform, Mainform);
			Application.CreateForm(TRuleEditForm, RuleEditForm);
			Application.CreateForm(TStartSelectForm, StartSelectForm);
  Application.Run;
end.

