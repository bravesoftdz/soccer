unit Soccer.Voting.Preferences;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TSoccerVotingIndividualPreferenceProfile = TList<string>;

  TSoccerVotingCollectivePreferenceProfile =
    TObjectList<TSoccerVotingIndividualPreferenceProfile>;

  TSoccerVotersPreferencesProperties = record

  end;

  TSoccerVotingVotersPreferences = class
  private
    FProfile: TSoccerVotingCollectivePreferenceProfile;
    FProperties: TSoccerVotersPreferencesProperties;
    FPropertiesCalculated: boolean;
    function GetProperties: TSoccerVotersPreferencesProperties;
    procedure CalculateProperties;
  public
    constructor Create;
    property Profile: TSoccerVotingCollectivePreferenceProfile read FProfile;
    property Properties: TSoccerVotersPreferencesProperties read GetProperties;
    destructor Destroy;
    override;
  end;

implementation

{ TSoccerVotingVoterPreferences }

procedure TSoccerVotingVotersPreferences.CalculateProperties;
begin

end;

constructor TSoccerVotingVotersPreferences.Create;
begin
  FProfile := TSoccerVotingCollectivePreferenceProfile.Create(true);
  FPropertiesCalculated := false;
end;

destructor TSoccerVotingVotersPreferences.Destroy;
begin
  FreeAndNil(FProfile);
end;

function TSoccerVotingVotersPreferences.GetProperties: TSoccerVotersPreferencesProperties;
begin
  if not FPropertiesCalculated then
    CalculateProperties;
  Result := FProperties;
end;

end.
