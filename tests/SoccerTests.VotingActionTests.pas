unit SoccerTests.VotingActionTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Exceptions,

  Soccer.Domain.Abstract,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Actions,
  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences,
  Soccer.Voting.RuleChooser,
  Soccer.Voting.Domain,

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
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.
      TList<System.AnsiString>): Boolean;
  end;

  [TestFixture]
  TVoteActionTests = class(TObject)
    [Test]
    procedure FullTest;
  end;

  [TestFixture]
  TDecideActionTests = class(TObject)
    [Test]
    procedure ComanndNotDecideTest;
    [Test]
    procedure WrongDomain;
  end;

  TFakeDomain = class(TInterfacedObject, ISoccerDomain)

  public
    function AmIStarted(AWhatIsStarted: string): Boolean;
    procedure DeInitialize;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: System.Generics.Collections.TList<System.AnsiString>;
    procedure Initialize;
    function SupportsCommand(ACommand: string): Boolean;
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

function TFakeVotingRule.ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
out Winners: System.Generics.Collections.TList<System.AnsiString>): Boolean;
begin
  Result := false;
end;

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

{ TDecideActionTests }

procedure TDecideActionTests.ComanndNotDecideTest;
var
  LAction: TSoccerDecideAction;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TSoccerVotingRulePreferenceList;
  LResult: TList<AnsiString>;
  LRuleChooser: ISoccerVotingRuleChooser;
  LDomain: TSoccerVotingDomain;
begin
  LDomain := TSoccerVotingDomain.Create;
  LList := TSoccerVotingRulePreferenceList.Create
    ('..\..\testdata\empty.soccfg');
  LProfile := TSoccerVotingVotersPreferences.Create;
  LAction := TSoccerDecideAction.Create(LProfile, LList, LDomain, LRuleChooser);
  LRuleChooser := TSoccerRuleChooser.Create;
  Assert.WillRaise(
    procedure
    begin
      LAction.WorkOnCommand('NOTDECIDE!');
    end, ESoccerParserException, 'Command is not "DECIDE!"');
  LDomain := nil;
  FreeAndNil(LProfile);
  FreeAndNil(LAction);
  FreeAndNil(LList);
end;

procedure TDecideActionTests.WrongDomain;
var
  LDomain: TFakeDomain;
  LAction: TSoccerDecideAction;
begin
  LDomain := TFakeDomain.Create;
  Assert.WillRaise(
    procedure
    begin
      LAction := TSoccerDecideAction.Create(nil, nil, LDomain, nil);
    end, ESoccerParserException,
    'Internal error: soccer decide action is appliable only for voting domain');
  LDomain := nil;
end;

{ TFakeDomain }

function TFakeDomain.AmIStarted(AWhatIsStarted: string): Boolean;
begin
  Result := false;
end;

procedure TFakeDomain.DeInitialize;
begin
  Sleep(0);
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

procedure TFakeDomain.Initialize;
begin
  Sleep(0);
end;

function TFakeDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := false;
end;

initialization

TDUnitX.RegisterTestFixture(TVotingImportActionTests);
TDUnitX.RegisterTestFixture(TVoteActionTests);
TDUnitX.RegisterTestFixture(TDecideActionTests);

end.
