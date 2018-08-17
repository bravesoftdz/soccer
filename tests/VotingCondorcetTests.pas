unit VotingCondorcetTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule,
  Soccer.VotingRules.Condorcet;

type

  [TestFixture]
  TVotingCondorcetTests = class(TObject)
  public
    [Test]
    procedure FullTest;
    [Test]
    procedure CondorcetParadox;
    [Test]
    procedure EqualAlternatives;
    [Test]
    procedure IncompleteProfileTest;
    [Test]
    procedure DisableEmpty;
  end;

implementation

{ TVotingCondorcetTests }

procedure TVotingCondorcetTests.CondorcetParadox;
var
  LCondorcet: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TList<string>;
  LWinners: TList<AnsiString>;
begin
  LCondorcet := TSoccerCondorcetRule.Create(true);
  LProfile := TSoccerVotingVotersPreferences.Create;
  // Voter 1
  LList := TList<string>.Create;
  LList.Add('Andrei_noob');
  LList.Add('Nikto_ne_noob');
  LList.Add('Alexey_noob');
  LProfile.Profile.Add(LList);
  // Voter 2
  LList := TList<string>.Create;
  LList.Add('Alexey_noob');
  LList.Add('Andrei_noob');
  LList.Add('Nikto_ne_noob');
  LProfile.Profile.Add(LList);
  // Voter 3
  LList := TList<string>.Create;
  LList.Add('Nikto_ne_noob');
  LList.Add('Alexey_noob');
  LList.Add('Andrei_noob');
  LProfile.Profile.Add(LList);

  Assert.IsTrue(LCondorcet.ExecuteOn(LProfile, LWinners));
  Assert.IsTrue(LWinners.Count = 0);

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LCondorcet := nil;
end;

procedure TVotingCondorcetTests.DisableEmpty;
var
  LCondorcet: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TList<string>;
  LWinners: TList<AnsiString>;
begin
  LCondorcet := TSoccerCondorcetRule.Create(false);
  LProfile := TSoccerVotingVotersPreferences.Create;

  Assert.IsFalse(LCondorcet.ExecuteOn(LProfile, LWinners));

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LCondorcet := nil;
end;

procedure TVotingCondorcetTests.EqualAlternatives;
var
  LCondorcet: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TList<string>;
  LWinners: TList<AnsiString>;
begin
  LCondorcet := TSoccerCondorcetRule.Create(true);
  LProfile := TSoccerVotingVotersPreferences.Create;
  // Voter 1
  LList := TList<string>.Create;
  LList.Add('lol');
  LList.Add('kek');
  LProfile.Profile.Add(LList);
  // Voter 2
  LList := TList<string>.Create;
  LList.Add('kek');
  LList.Add('lol');
  LProfile.Profile.Add(LList);

  Assert.IsTrue(LCondorcet.ExecuteOn(LProfile, LWinners));
  Assert.IsTrue(LWinners.Count = 2);
  Assert.IsTrue(LWinners[0] = 'lol');
  Assert.IsTrue(LWinners[1] = 'kek');

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LCondorcet := nil;
end;

procedure TVotingCondorcetTests.FullTest;
var
  LCondorcet: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TList<string>;
  LWinners: TList<AnsiString>;
begin
  LCondorcet := TSoccerCondorcetRule.Create(true);
  LProfile := TSoccerVotingVotersPreferences.Create;
  // Voter 1
  LList := TList<string>.Create;
  LList.Add('a');
  LList.Add('c');
  LList.Add('b');
  LProfile.Profile.Add(LList);
  // Voter 2
  LList := TList<string>.Create;
  LList.Add('a');
  LList.Add('b');
  LList.Add('c');
  LProfile.Profile.Add(LList);
  // Voter 3
  LList := TList<string>.Create;
  LList.Add('c');
  LList.Add('b');
  LList.Add('a');
  LProfile.Profile.Add(LList);

  Assert.IsTrue(LCondorcet.ExecuteOn(LProfile, LWinners));
  Assert.IsTrue(LWinners.Count = 1);
  Assert.IsTrue(LWinners[0] = 'a');

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LCondorcet := nil;
end;

procedure TVotingCondorcetTests.IncompleteProfileTest;
var
  LCondorcet: ISoccerVotingRule;
  LProfile: TSoccerVotingVotersPreferences;
  LList: TList<string>;
  LWinners: TList<AnsiString>;
begin
  LCondorcet := TSoccerCondorcetRule.Create(true);
  LProfile := TSoccerVotingVotersPreferences.Create;
  // Voter 1
  LList := TList<string>.Create;
  LList.Add('mem');
  LList.Add('nen');
  LProfile.Profile.Add(LList);
  // Voter 2
  LList := TList<string>.Create;
  LList.Add('nen');
  LProfile.Profile.Add(LList);

  Assert.IsTrue(LCondorcet.ExecuteOn(LProfile, LWinners));

  FreeAndNil(LProfile);
  FreeAndNil(LWinners);
  LCondorcet := nil;
end;

initialization

TDUnitX.RegisterTestFixture(TVotingCondorcetTests);

end.
