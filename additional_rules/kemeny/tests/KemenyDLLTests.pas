unit KemenyDLLTests;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Preferences,

  Soccer.VotingRules.DLLRule;

type

  [TestFixture]
  TKemenyDLLTests = class(TObject)
  public
    [Test]
    procedure GetNameTest;
    [Test]
    procedure CalcKemeny;
  end;

const
  KEMENY_DLL = '..\..\..\out\kemenydll.dll';

implementation

{ TKemenyDLLTests }

procedure TKemenyDLLTests.CalcKemeny;
var
  LKemeny: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LWinners: TList<string>;
begin
  LKemeny := TSoccerDLLVotingRule.Create(KEMENY_DLL);
  LProfile := TSoccerVotingVotersPreferences.Create;
  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[0].Add('alt2');
  LProfile.Profile[0].Add('alt1');
  LProfile.Profile[0].Add('alt3');
  LKemeny.ExecuteOn(LProfile, LWinners);
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'alt2');
end;

procedure TKemenyDLLTests.GetNameTest;
var
  LKemeny: ISoccerVotingRule;
  LName: string;
begin
  LKemeny := TSoccerDLLVotingRule.Create(KEMENY_DLL);
  LName := LKemeny.GetName;
  Assert.IsTrue(LName = 'kemeny');
end;

initialization

TDUnitX.RegisterTestFixture(TKemenyDLLTests);

end.
