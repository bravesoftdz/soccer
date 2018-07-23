unit SoccerTests.MainTests;

interface
uses
  System.SysUtils,

  DUnitX.TestFramework,

  Soccer.Main;

type

  [TestFixture]
  TMainTests = class(TObject) 
  public
  [Test]
  procedure FullTest;
  end;

implementation


{ TMainTests }

procedure TMainTests.FullTest;
var LSoccer: TSoccer;
begin
  LSoccer := TSoccer.Create;
  FreeAndNil(LSoccer);
end;

initialization
  TDUnitX.RegisterTestFixture(TMainTests);
end.
