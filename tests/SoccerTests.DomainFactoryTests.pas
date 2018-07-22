unit SoccerTests.DomainFactoryTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Domain.Abstract,
  Soccer.Domain.Factory;

type

  TFakeDomain = class(TInterfacedObject, ISoccerDomain)
  public
    function AmIStarted(AStartCommand: string): Boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: System.Generics.Collections.
      TList<System.AnsiString>;
    function SupportsCommand(ACommand: string): Boolean;
  end;

  [TestFixture]
  TSoccerDomainFactoryTests = class(TObject)
  public
    [Test]
    procedure FactoryTest;
    [Test]
    procedure GlobalFactoryTest;
  end;

implementation

{ TSoccerDomainFactoryTests }

procedure TSoccerDomainFactoryTests.FactoryTest;
var
  LFactory: TSoccerDomainFactory;
begin
  LFactory := TSoccerDomainFactory.Create;
  LFactory.Domains.Add(TFakeDomain.Create);
  Assert.IsTrue(LFactory.Domains.Count = 1);
  FreeAndNil(LFactory);
end;

procedure TSoccerDomainFactoryTests.GlobalFactoryTest;
begin
  Assert.IsTrue(GlobalDomainFactory.Domains.Count = 0);
end;

{ TFakeDomain }

function TFakeDomain.AmIStarted(AStartCommand: string): Boolean;
begin
  Result := true;
end;

function TFakeDomain.GetActionForCommand(ACommand: string): ISoccerAction;
begin
  Result := nil;
end;

function TFakeDomain.GetOutput: System.Generics.Collections.
  TList<System.AnsiString>;
begin
  Result := nil;
end;

function TFakeDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := false;
end;

initialization

TDUnitX.RegisterTestFixture(TSoccerDomainFactoryTests);

end.
