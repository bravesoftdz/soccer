unit SoccerTests.VotingRulePreferenceListTests;

interface

uses
  System.SysUtils,

  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.AbstractRule,

  DUnitX.TestFramework;

type

  [TestFixture]
  TVotingRulePreferenceListTests = class(TObject)
  public
    [Test]
    procedure InsertTwoRules;
    [Test]
    procedure InsertTwoRules2;
  end;

  TFirstRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
  end;

  TSecondRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
  end;

implementation

{ TVotingRulePreferenceListTests }

procedure TVotingRulePreferenceListTests.InsertTwoRules;
var
  LList: TSoccerVotingRulePreferenceList;
  LRule1, LRule2: ISoccerVotingRule;
begin
  LList := TSoccerVotingRulePreferenceList.Create
    ('.\testdata\rulepreferencelisttests.cfg');
  LRule1 := TFirstRule.Create;
  LRule2 := TSecondRule.Create;
  LList.Add(LRule2);
  LList.Add(LRule1);
  Assert.IsTrue(LList.Count = 2);
  Assert.IsTrue(LList.Items[0].GetName = 'first');
  Assert.IsTrue(LList.Items[1].GetName = 'second');
  FreeAndNil(LList);
end;

procedure TVotingRulePreferenceListTests.InsertTwoRules2;
var
  LList: TSoccerVotingRulePreferenceList;
  LRule1, LRule2: ISoccerVotingRule;
begin
  LList := TSoccerVotingRulePreferenceList.Create
    ('.\testdata\rulepreferencelisttests.cfg');
  LRule1 := TFirstRule.Create;
  LRule2 := TSecondRule.Create;
  LList.Add(LRule1);
  LList.Add(LRule2);
  Assert.IsTrue(LList.Count = 2);
  Assert.IsTrue(LList.Items[0].GetName = 'first');
  Assert.IsTrue(LList.Items[1].GetName = 'second');
  FreeAndNil(LList);
end;

{ TSecondRule }

function TSecondRule.GetName: string;
begin
  Result := 'second';
end;

{ TFirstRule }

function TFirstRule.GetName: string;
begin
  Result := 'first';
end;

initialization

TDUnitX.RegisterTestFixture(TVotingRulePreferenceListTests);

end.
