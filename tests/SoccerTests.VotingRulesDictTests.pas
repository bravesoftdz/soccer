unit SoccerTests.VotingRulesDictTests;

interface

uses
  System.SysUtils,
  System.Classes,

  Soccer.Voting.RulesDict,
  Soccer.Voting.AbstractRule,

  DUnitX.TestFramework;

type

  [TestFixture]
  TVotingRulesDictTest = class(TObject)
  public
    [SetupFixture]
    procedure CallGlobalDict;
    [Test]
    procedure VotingRulesDictTest;
    [Test]
    procedure GlobalDictTest;
  end;

  TFakeVotingRule = class(TInterfacedObject, ISoccerVotingRule)
  public
    function GetName: string;
  end;

implementation

{ TVotingRulesDictTest }

procedure TVotingRulesDictTest.CallGlobalDict;
begin
  GlobalVotingRulesDict;
end;

procedure TVotingRulesDictTest.GlobalDictTest;
begin
  Assert.IsTrue(GlobalVotingRulesDict.Rules.Count = 0);
end;

procedure TVotingRulesDictTest.VotingRulesDictTest;
var
  LDict: TSoccerVotingRulesDict;
  LRule: ISoccerVotingRule;
begin
  LDict := TSoccerVotingRulesDict.Create;
  LRule := TFakeVotingRule.Create;
  LDict.Rules.Add(LRule.GetName,LRule);
  Assert.IsTrue(LDict.Rules.Count = 1);
  FreeAndNil(LDict);
end;

{ TFakeVotingRule }

function TFakeVotingRule.GetName: string;
begin
  Result := 'lollingrulefake';
end;

initialization

TDUnitX.RegisterTestFixture(TVotingRulesDictTest);

end.
