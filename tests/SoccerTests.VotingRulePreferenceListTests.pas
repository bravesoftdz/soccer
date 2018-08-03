unit SoccerTests.VotingRulePreferenceListTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Voting.Preferences,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.AbstractRule,

  DUnitX.TestFramework;

type

  [TestFixture]
  TVotingRulePreferenceListTests = class(TObject)
  public
    [SetupFixture]
    procedure InitilaizeEncodings;
    [Test]
    procedure InsertTwoRules;
    [Test]
    procedure InsertTwoRules2;
  end;

  TFirstRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences): System.Generics.Collections.TList<System.AnsiString>;
  end;

  TSecondRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences): System.Generics.Collections.TList<System.AnsiString>;
  end;

implementation

{ TVotingRulePreferenceListTests }

procedure TVotingRulePreferenceListTests.InitilaizeEncodings;
begin
  TEncoding.Unicode;
  TEncoding.BigEndianUnicode;
end;

procedure TVotingRulePreferenceListTests.InsertTwoRules;
var
  LList: TSoccerVotingRulePreferenceList;
  LRule1, LRule2: ISoccerVotingRule;
begin
  LList := TSoccerVotingRulePreferenceList.Create
    ('..\..\testdata\rulepreferencelisttests.soccfg');
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
    ('..\..\testdata\rulepreferencelisttests.soccfg');
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

function TSecondRule.ExecuteOn(
  AProfile: TSoccerVotingVotersPreferences): System.Generics.Collections.TList<System.AnsiString>;
begin
  Result := nil;
end;

function TSecondRule.GetName: string;
begin
  Result := 'second';
end;

{ TFirstRule }

function TFirstRule.ExecuteOn(
  AProfile: TSoccerVotingVotersPreferences): System.Generics.Collections.TList<System.AnsiString>;
begin
  Result := nil;
end;

function TFirstRule.GetName: string;
begin
  Result := 'first';
end;

initialization

TDUnitX.RegisterTestFixture(TVotingRulePreferenceListTests);

end.
