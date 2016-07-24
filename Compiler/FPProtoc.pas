program FPProtoc;

uses
  Classes,
  SysUtils,
  CustApp,
  CompilerTypes,
  Lexer,
  DFA,
  CompUtils,
  Crt;

type

  { TProtocApp }

  TProtocApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure WriteHeader(vd: TVersionData);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TProtocApp }


    procedure TProtocApp.WriteHeader(vd: TVersionData);
    begin
      WriteLn(vd.ProductName, ' ', vd.ProductVersion, ' (Fileversion: ', vd.FileVersion, ') [',{$I %Date%},']');
      WriteLn(vd.LegalCopyright);
      WriteLn(vd.FileDescription);
      WriteLn;
    end;

  procedure TProtocApp.DoRun;
  var
    ErrorMsg: string;
  begin        
      CaseSensitiveOptions:=False;
    WriteHeader(GetVersionData(ExeName));
    // quick check parameters
    ErrorMsg := CheckOptions('i:o:l:xpg:h', ['input:', 'output:', 'library:','lex','parse','log:','help']);
    if ErrorMsg <> '' then
    begin
      TextColor(Red);
      Write('Error: ');
      TextColor(LightGray);
      WriteLn(ErrorMsg);
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') or (ParamCount=0) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    //if not HasOption('i', 'input');


    // stop program loop
    Terminate;
  end;

  constructor TProtocApp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TProtocApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TProtocApp.WriteHelp;
  begin
    WriteLn('Option                              --  Info');
    WriteLn('-i <File> --input=<File>            --  The input file to be compiled');
    WriteLn('-o <Path> --output=<Path>           --  The output path for the pascal units');
    WriteLn('-l <Path> --library=<Path>          --  The searchpath for other .proto files');
    WriteLn('-x --lex                            --  Only execute the lexer (on proto file)');
    WriteLn('-p --parse                          --  Only execute the parser (on .lex file)');
    WriteLn('-g <Path> --log=<Path>              --  Writes the log files to path');
    WriteLn('-h --help                           --  Prints this page');
  end;

var
  Application: TProtocApp;

{$R *.res}

begin
  Application := TProtocApp.Create(nil);
  Application.Title := 'Protobuf Compiler';
  Application.Run;
  Application.Free;
end.
