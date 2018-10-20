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
  end;

const
  KEMENY_DLL = '..\..\..\out\kemenydll.dll';

implementation

{ TKemenyDLLTests }

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
  {It's dangerous to create Kemeny in every test:
   it causes access violation sometimes }
  FKemeny := TSoccerDLLVotingRule.Create(KEMENY_DLL);
end;

initialization

TDUnitX.RegisterTestFixture(TKemenyDLLTests);

end.
