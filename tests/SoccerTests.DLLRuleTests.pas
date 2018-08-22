unit SoccerTests.DLLRuleTests;

interface

uses
  System.SysUtils,

  DUnitX.TestFramework,

  Soccer.Voting.AbstractRule,
  Soccer.VotingRules.DLLRule;

type

  [TestFixture]
  TSoccerDLLRuleTests = class(TObject)
  public
    [Test]
    procedure FullTest;
  end;

implementation

{ TSoccerDLLRuleTests }

procedure TSoccerDLLRuleTests.FullTest;
var
  LRule: ISoccerVotingRule;
  LPath: string;
begin
  LRule := TSoccerDLLVotingRule.Create
    ('..\..\testdata\dllrules\basic\basicrule.dll');
  Assert.IsTrue(LRule.GetName = 'nowinnerrule');
end;

initialization

TDUnitX.RegisterTestFixture(TSoccerDLLRuleTests);

end.
