unit SoccerTests.PreferencesTests;

interface
uses
  System.SysUtils,

  DUnitX.TestFramework,

  Soccer.Voting.Preferences;

type

  [TestFixture]
  TSoccerPreferencesTests = class(TObject) 
  public
  [Test]
  procedure CalculatePropertiesTest;
  end;

implementation


{ TSoccerPreferencesTests }

procedure TSoccerPreferencesTests.CalculatePropertiesTest;
var LProfile: TSoccerVotingVotersPreferences;
begin
  LProfile := TSoccerVotingVotersPreferences.Create;
  LProfile.Properties;
  FreeAndNil(LProfile);
end;

initialization
  TDUnitX.RegisterTestFixture(TSoccerPreferencesTests);
end.
