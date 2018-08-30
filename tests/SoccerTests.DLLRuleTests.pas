unit SoccerTests.DLLRuleTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Preferences,

  Soccer.VotingRules.DLLRule;

type

  [TestFixture]
  TSoccerDLLRuleTests = class(TObject)
  public
    [Test]
    procedure FullTest;
    [Test]
    procedure NoExecuteOnTest;
    [Test]
    procedure NoGetNameTest;
    [Test]
    procedure SystemExceptionTest;
  end;

implementation

{ TSoccerDLLRuleTests }

procedure TSoccerDLLRuleTests.FullTest;
var
  LRule: ISoccerVotingRule;
  LPath: string;
  LProfile: TSoccerVotingVotersPreferences;
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LWinners: TList<AnsiString>;
begin
  LRule := TSoccerDLLVotingRule.Create
    ('..\..\testdata\dllrules\basic\basicrule.dll');
  Assert.IsTrue(LRule.GetName = 'nowinnerrule');
  LProfile := TSoccerVotingVotersPreferences.Create;

  LVoter := TSoccerVotingIndividualPreferenceProfile.Create;
  LVoter.Add('a');
  LVoter.Add('b');
  LVoter.Add('c');
  LProfile.Profile.Add(LVoter);

  Assert.IsTrue(LRule.ExecuteOn(LProfile,LWinners));

  Assert.IsTrue(Assigned(LWinners));
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'a');

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
end;

procedure TSoccerDLLRuleTests.NoExecuteOnTest;
var
  LRule: ISoccerVotingRule;
begin
  Assert.WillRaise(
    procedure
    begin
      LRule := TSoccerDLLVotingRule.Create
        ('..\..\testdata\dllrules\noexecuteon\noexecuteon.dll');
    end, ESoccerParserException);
end;

procedure TSoccerDLLRuleTests.NoGetNameTest;
var
  LRule: ISoccerVotingRule;
begin
  Assert.WillRaise(
    procedure
    begin
      LRule := TSoccerDLLVotingRule.Create
        ('..\..\testdata\dllrules\nogetname\nogetname.dll');
    end, ESoccerParserException);
end;

procedure TSoccerDLLRuleTests.SystemExceptionTest;
var
  LRule: ISoccerVotingRule;
begin
  Assert.WillRaise(
    procedure
    begin
      LRule := TSoccerDLLVotingRule.Create
        ('..\..\testdata\dllrules\basic64\basicrule64.dll');
    end, ESoccerParserException);
end;

initialization

TDUnitX.RegisterTestFixture(TSoccerDLLRuleTests);

end.
