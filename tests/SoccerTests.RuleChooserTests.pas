unit SoccerTests.RuleChooserTests;

interface

uses
  System.SysUtils,

  DUnitX.TestFramework,

  Soccer.Exceptions,

  Soccer.Voting.Preferences,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.RuleChooser;

type

  [TestFixture]
  TRuleChooserTests = class(TObject)
  public
    [Test]
    procedure NoRuleImportedTest;
  end;

implementation

{ TRuleChooserTests }

procedure TRuleChooserTests.NoRuleImportedTest;
var
  LRuleChooser: TSoccerRuleChooser;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TSoccerVotingRulePreferenceList;
begin
  LRuleChooser := TSoccerRuleChooser.Create;
  LList := TSoccerVotingRulePreferenceList.Create
    ('..\..\testdata\empty.soccfg');
  LProfile := TSoccerVotingVotersPreferences.Create;
  Assert.WillRaise(
    procedure
    begin
      LRuleChooser.ChooseRule(LProfile, LList);
    end, ESoccerParserException, 'No rule was imported');
  FreeAndNil(LList);
  FreeAndNil(LProfile);
  FreeAndNil(LRuleChooser);
end;

initialization

TDUnitX.RegisterTestFixture(TRuleChooserTests);

end.
