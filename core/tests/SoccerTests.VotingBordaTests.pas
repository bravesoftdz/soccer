unit SoccerTests.VotingBordaTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.Preferences,
  Soccer.VotingRules.Borda;

type

  [TestFixture]
  TBordaTests = class(TObject)
  public
    [Test]
    procedure FullTest;
  end;

implementation

{ TBordaTests }

procedure TBordaTests.FullTest;
var
  LRule: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LWinners: TList<string>;
begin
  LRule := TSoccerBordaVotingScoreRule.Create;
  LProfile := TSoccerVotingVotersPreferences.Create;

  LVoter := TSoccerVotingIndividualPreferenceProfile.Create;
  LVoter.Add('a');
  LVoter.Add('b');
  LVoter.Add('c');
  LProfile.Profile.Add(LVoter);

  LVoter := TSoccerVotingIndividualPreferenceProfile.Create;
  LVoter.Add('c');
  LVoter.Add('b');
  LVoter.Add('a');
  LProfile.Profile.Add(LVoter);

  LVoter := TSoccerVotingIndividualPreferenceProfile.Create;
  LVoter.Add('b');
  LVoter.Add('c');
  LVoter.Add('a');
  LProfile.Profile.Add(LVoter);

  Assert.IsTrue(LRule.ExecuteOn(LProfile, LWinners));
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'b');

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LRule := nil;
end;

initialization

TDUnitX.RegisterTestFixture(TBordaTests);

end.
