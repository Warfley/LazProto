program GrammarCreator;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  cfg,
  CompUtils;

type

  { TGrammarCreator }

  TGrammarCreator = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure WriteHeader(vd: TVersionData);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

  { TGrammarCreator }

  procedure TGrammarCreator.WriteHeader(vd: TVersionData);
  begin
    WriteLn(vd.ProductName, ' ', vd.ProductVersion, ' (Fileversion: ',
      vd.FileVersion, ') [',
{$I %Date%}
      , ']');
    WriteLn(vd.LegalCopyright);
    WriteLn(vd.FileDescription);
    WriteLn;
  end;

  procedure TGrammarCreator.DoRun;
  var
    ErrorMsg: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hi:o:lr', ['help', 'input', 'output', 'LL1', 'LR1']);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') or (ParamCount = 0) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    // stop program loop
    Terminate;
  end;

  constructor TGrammarCreator.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  procedure TGrammarCreator.WriteHelp;
  begin
    WriteLn('Option                     --  Info');
    WriteLn('-i <File> --input=<File>   --  The input Grammar');
    WriteLn('-o <Path> --output=<Path>  --  The output path for the LL/LR Parser');
    WriteLn('-m Parser --Mode=Parser    --  Selects the Parser to create (LL/SLR/LALR/LR)');
    WriteLn('-h --help                  --  Prints this page');
  end;

var
  Application: TGrammarCreator;

{$R *.res}

begin
  Application := TGrammarCreator.Create(nil);
  Application.Title := 'Grammar Creator';
  Application.Run;
  Application.Free;
end.
