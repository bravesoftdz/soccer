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
  private
    FKemeny: ISoccerVotingRule;
  public
    [SetupFixture]
    procedure InitilizeKemeny;
    [Test]
    procedure GetNameTest;
    [Test]
    procedure CalcKemeny;
    [Test]
    procedure Bigtest1;
    [TearDownFixture]
    procedure FreeKemeny;
  end;

const
  KEMENY_DLL = '..\..\..\out\kemenydll.dll';

implementation

{ TKemenyDLLTests }

procedure TKemenyDLLTests.Bigtest1;
var
  LProfile: TSoccerVotingVotersPreferences;
  LWinners: TList<string>;
begin
  LProfile := TSoccerVotingVotersPreferences.Create;

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[0].Add('Murmansk');
  LProfile.Profile[0].Add('Saint Petersburg');
  LProfile.Profile[0].Add('Moscow');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[1].Add('Murmansk');
  LProfile.Profile[1].Add('Saint Petersburg');
  LProfile.Profile[1].Add('Moscow');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[2].Add('Murmansk');
  LProfile.Profile[2].Add('Saint Petersburg');
  LProfile.Profile[2].Add('Moscow');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[3].Add('Saint Petersburg');
  LProfile.Profile[3].Add('Murmansk');
  LProfile.Profile[3].Add('Moscow');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[4].Add('Saint Petersburg');
  LProfile.Profile[4].Add('Murmansk');
  LProfile.Profile[4].Add('Moscow');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[5].Add('Murmansk');
  LProfile.Profile[5].Add('Moscow');
  LProfile.Profile[5].Add('Saint Petersburg');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[6].Add('Moscow');
  LProfile.Profile[6].Add('Saint Petersburg');
  LProfile.Profile[6].Add('Murmansk');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[7].Add('Moscow');
  LProfile.Profile[7].Add('Murmansk');
  LProfile.Profile[7].Add('Saint Petersburg');

  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[8].Add('Saint Petersburg');
  LProfile.Profile[8].Add('Moscow');
  LProfile.Profile[8].Add('Murmansk');

  FKemeny.ExecuteOn(LProfile, LWinners);
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'Murmansk');
  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
end;

procedure TKemenyDLLTests.CalcKemeny;
var
  LProfile: TSoccerVotingVotersPreferences;
  LWinners: TList<string>;
begin
  LProfile := TSoccerVotingVotersPreferences.Create;
  LProfile.Profile.Add(TSoccerVotingIndividualPreferenceProfile.Create);
  LProfile.Profile[0].Add('alt2');
  LProfile.Profile[0].Add('alt1');
  LProfile.Profile[0].Add('alt3');
  FKemeny.ExecuteOn(LProfile, LWinners);
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'alt2');
  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
end;

procedure TKemenyDLLTests.FreeKemeny;
begin
  FKemeny := nil;
end;

procedure TKemenyDLLTests.GetNameTest;
var
  LName: string;
begin
  LName := FKemeny.GetName;
  Assert.IsTrue(LName = 'kemeny');
end;

procedure TKemenyDLLTests.InitilizeKemeny;
begin
  { It's dangerous to create Kemeny in every test:
    it causes access violation sometimes }
  FKemeny := TSoccerDLLVotingRule.Create(KEMENY_DLL);
end;

initialization

TDUnitX.RegisterTestFixture(TKemenyDLLTests);

end.
