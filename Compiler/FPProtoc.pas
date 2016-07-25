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
  ECompilerException = Exception;

  TProtocApp = class(TCustomApplication)
  protected
    FInputString: AnsiString;
    procedure DoRun; override;
    procedure WriteHeader(vd: TVersionData);
    procedure LexerFoundTok(Sender: TObject; Tok: TLexTok);
    procedure StartLexer(inFile: String);
  public
    procedure ExceptionFound (Sender : TObject; E : Exception);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TProtocApp }

    procedure TProtocApp.ExceptionFound (Sender : TObject; E : Exception);
    begin
      TextColor(Red);
      Write('Error: ');
      TextColor(LightGray);
      WriteLn(E.Message);
		end;

    procedure TProtocApp.LexerFoundTok(Sender: TObject; Tok: TLexTok);
    begin
      WriteLn('Token found (', tok.Token,', ', Copy(Tok.Start, 1, Tok.Len),')');
		end;

    procedure TProtocApp.WriteHeader(vd: TVersionData);
    begin
      WriteLn(vd.ProductName, ' ', vd.ProductVersion, ' (Fileversion: ', vd.FileVersion, ') [',{$I %Date%},']');
      WriteLn(vd.LegalCopyright);
      WriteLn(vd.FileDescription);
      WriteLn;
    end;

    procedure TProtocApp.StartLexer(inFile: String);
    var lex: TLexer;
      sl: TStringList;
    begin
      WriteLn('Initializing Lexer, building DFA');
      lex:=TLexer.Create;
      try
        if not FileExists(inFile) then
          raise EFilerError.Create('Input File not found');
        WriteLn('Loading File: ', inFile);
        sl:=TStringList.Create;
        try
          sl.LoadFromFile(inFile);
          FInputString:=sl.Text;
				finally
          sl.Free;
				end;
				lex.Head:=PAnsiChar(FInputString);
        lex.OnTokenFound:=@LexerFoundTok;
        WriteLn('Starting lexical analysis');
        Lex.Run;
			finally
        lex.Free;
			end;
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
      raise ECompilerException.Create(ErrorMsg);
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') and (ParamCount>0) then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    //if not HasOption('i', 'input');
    if not (HasOption('i', 'input') and HasOption('o', 'output')) then
      raise ECompilerException.Create('Parameter --input (-i) and --output (-o)'
            +' are missing.'+LineEnding+'Check --help for more information');

    if HasOption('x', 'lex') or not HasOption('p', 'parse');
       StartLexer(GetOptionValue('i', 'input'));

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
  Application.OnException:=@Application.ExceptionFound;
  Application.Run;
  Application.Free;
end.
