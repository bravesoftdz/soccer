unit SoccerTests.VotingActionTests;

interface

uses
  System.SysUtils,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Actions,
  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences,

  DUnitX.TestFramework;

type

  [TestFixture]
  TVotingImportActionTests = class(TObject)
  public
    [Test]
    procedure FullTest;
    [Test]
    procedure ImportNonExistingRule;
  end;

  TFakeVotingRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
  end;

  [TestFixture]
  TVoteActionTests = class(TObject)
    [Test]
    procedure FullTest;
  end;

implementation

{ TVotingImportActionTests }

procedure TVotingImportActionTests.FullTest;
var
  LAction: TSoccerVotingImportAction;
  LDict: TSoccerVotingRulesDict;
  LList: TSoccerVotingRulePreferenceList;
begin
  LDict := TSoccerVotingRulesDict.Create;
  LDict.Rules.Add('first', TFakeVotingRule.Create);
  LList := TSoccerVotingRulePreferenceList.Create
    ('.\testdata\votingimportactiontests.cfg');
  LAction := TSoccerVotingImportAction.Create(LDict, LList);
  LAction.WorkOnCommand('IMPORT[first]');
  Assert.IsTrue(LList.Items[0].GetName = 'first');
  FreeAndNil(LAction);
  FreeAndNil(LList);
  FreeAndNil(LDict);
end;

procedure TVotingImportActionTests.ImportNonExistingRule;
var
  LAction: TSoccerVotingImportAction;
  LDict: TSoccerVotingRulesDict;
  LList: TSoccerVotingRulePreferenceList;
begin
  LDict := TSoccerVotingRulesDict.Create;
  LList := TSoccerVotingRulePreferenceList.Create
    ('.\testdata\votingimportactiontests.cfg');
  LAction := TSoccerVotingImportAction.Create(LDict, LList);
  Assert.WillRaise(
    procedure
    begin
      LAction.WorkOnCommand('IMPORT[first]');
    end, ESoccerParserException, 'No rule with a name "first" found');
  FreeAndNil(LAction);
  FreeAndNil(LList);
  FreeAndNil(LDict);
end;

{ TFakeVotingRule }

function TFakeVotingRule.GetName: string;
begin
  Result := 'first';
end;

{ TVoteActionTests }

procedure TVoteActionTests.FullTest;
var
  LAction: TSoccerVoteAction;
  LPref: TSoccerVotingVotersPreferences;
begin
  LPref := TSoccerVotingVotersPreferences.Create;
  LAction := TSoccerVoteAction.Create(LPref);
  LAction.WorkOnCommand('VOTE(a->b->lol)');
  Assert.IsTrue(LPref.Profile[0][0] = 'a');
  Assert.IsTrue(LPref.Profile[0][1] = 'b');
  Assert.IsTrue(LPref.Profile[0][2] = 'lol');
  FreeAndNil(LPref);
  FreeAndNil(LAction);
end;

initialization

TDUnitX.RegisterTestFixture(TVotingImportActionTests);
TDUnitX.RegisterTestFixture(TVoteActionTests);

end.
