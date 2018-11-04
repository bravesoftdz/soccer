unit SoccerTests.RuleChooserTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Preferences,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.RuleChooser;

type

  [TestFixture]
  TRuleChooserTests = class(TObject)
  public
    [Test]
    procedure NoRuleImportedTest;
    [Test]
    procedure NoRuleFoundTest;
  end;

  TUnusefulRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.
      TList<string>): Boolean;
    function GetName: string;
  end;

implementation

{ TRuleChooserTests }

procedure TRuleChooserTests.NoRuleFoundTest;
var
  LRuleChooser: TSoccerRuleChooser;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TSoccerVotingRulePreferenceList;
begin
  LRuleChooser := TSoccerRuleChooser.Create;
  LList := TSoccerVotingRulePreferenceList.Create
    ('..\..\testdata\empty.soccfg');
  LList.Add(TUnusefulRule.Create);
  LProfile := TSoccerVotingVotersPreferences.Create;
  Assert.WillRaise(
    procedure
    begin
      LRuleChooser.ChooseRuleFindWinners(LProfile, LList);
    end, ESoccerParserException);
  FreeAndNil(LProfile);
  FreeAndNil(LList);
  FreeAndNil(LRuleChooser);
end;

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
      LRuleChooser.ChooseRuleFindWinners(LProfile, LList);
    end, ESoccerParserException, 'No rule was imported');
  FreeAndNil(LList);
  FreeAndNil(LProfile);
  FreeAndNil(LRuleChooser);
end;

{ TUnusefulRule }

function TUnusefulRule.ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
out Winners: System.Generics.Collections.TList<string>): Boolean;
begin
  Winners := TList<string>.Create;
  Winners.Add('LOL');
  Result := false;
end;

function TUnusefulRule.GetName: string;
begin
  Result := 'unuseful';
end;

initialization

TDUnitX.RegisterTestFixture(TRuleChooserTests);

end.
