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
      TList<string>;
    function SupportsCommand(ACommand: string): Boolean;
    procedure DeInitialize;
    procedure Initialize;
  end;

  [TestFixture]
  TSoccerDomainFactoryTests = class(TObject)
  public
    [SetupFixture]
    procedure InitializeEncodings;
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
  Assert.IsTrue(GlobalDomainFactory.Domains.Count = 1);
end;

procedure TSoccerDomainFactoryTests.InitializeEncodings;
begin
  TEncoding.Unicode;
  TEncoding.BigEndianUnicode;
end;

{ TFakeDomain }

function TFakeDomain.AmIStarted(AStartCommand: string): Boolean;
begin
  Result := true;
end;

procedure TFakeDomain.DeInitialize;
begin

end;

function TFakeDomain.GetActionForCommand(ACommand: string): ISoccerAction;
begin
  Result := nil;
end;

function TFakeDomain.GetOutput: System.Generics.Collections.TList<string>;
begin
  Result := nil;
end;

procedure TFakeDomain.Initialize;
begin

end;

function TFakeDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := false;
end;

initialization

TDUnitX.RegisterTestFixture(TSoccerDomainFactoryTests);

end.
