unit SoccerTets.SoccerParserTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Parser,
  Soccer.Domain.Abstract,
  Soccer.Domain.Factory;

type

  TTestDomain = class(TInterfacedObject, ISoccerDomain)
  private
    FOutput: TList<System.AnsiString>;
  public
    constructor Create;
    procedure AddToInput(ALine: string);
    function AmIStarted(AWhatIsStarted: string): Boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: System.Generics.Collections.TList<System.AnsiString>;
    function SupportsCommand(ACommand: string): Boolean;
    destructor Destroy; override;
  end;

  TTestDomainAction = class(TInterfacedObject, ISoccerAction)
  private
    FParent: TTestDomain;
  public
    constructor Create(AParent: TTestDomain);
    procedure WorkOnCommand(ACommand: string);
    destructor Destroy; override;
  end;

  [TestFixture]
  TSoccerParserTests = class(TObject)
  public
    [Test]
    procedure FullTest;
  end;

implementation

{ TSoccerParserTests }

procedure TSoccerParserTests.FullTest;
var
  LParser: TSoccerParser;
  LFactory: TSoccerDomainFactory;
  LDomain: TTestDomain;
  LOutput: TList<AnsiString>;
begin
  LFactory := TSoccerDomainFactory.Create;
  LDomain := TTestDomain.Create;
  LFactory.Domains.Add(LDomain);
  LParser := TSoccerParser.Create(LFactory);
  LOutput := LParser.ParseExecuteScript('START[test]   TEST');
  Assert.IsTrue(LOutput[0] = 'THIS IS');
  Assert.IsTrue(LOutput[1] = 'SPARTA!!!');
  FreeAndNil(LFactory);
  FreeAndNil(LParser);
  FreeAndNil(LOutput);
end;

{ TTestDomain }

procedure TTestDomain.AddToInput(ALine: string);
begin
  FOutput.Add(ALine);
end;

function TTestDomain.AmIStarted(AWhatIsStarted: string): Boolean;
begin
  Result := AWhatIsStarted = 'test';
end;

constructor TTestDomain.Create;
begin
  FOutput := TList<AnsiString>.Create;
end;

destructor TTestDomain.Destroy;
begin
  FreeAndNil(FOutput);
  inherited;
end;

function TTestDomain.GetActionForCommand(ACommand: string): ISoccerAction;
begin
  if ACommand = 'TEST' then
    Result := TTestDomainAction.Create(Self);
end;

function TTestDomain.GetOutput: System.Generics.Collections.
  TList<System.AnsiString>;
begin
  Result := FOutput;
  FOutput := TList<AnsiString>.Create;
end;

function TTestDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := ACommand = 'TEST';
end;

{ TTestDomainAction }

constructor TTestDomainAction.Create(AParent: TTestDomain);
begin
  FParent := AParent;
end;

destructor TTestDomainAction.Destroy;
begin
  FParent := nil;
  inherited;
end;

procedure TTestDomainAction.WorkOnCommand(ACommand: string);
begin
  FParent.AddToInput('THIS IS');
  FParent.AddToInput('SPARTA!!!');
end;

initialization

TDUnitX.RegisterTestFixture(TSoccerParserTests);

end.
