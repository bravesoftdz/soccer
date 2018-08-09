unit Soccer.Voting.Preferences;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TSoccerVotingIndividualPreferenceProfile = TList<string>;

  TSoccerVotingCollectivePreferenceProfile =
    TObjectList<TSoccerVotingIndividualPreferenceProfile>;

  TSoccerVotersPreferencesProperties = record
    AlternativesCount: integer;
    VotersCount: integer;
    Complete: boolean;
  end;

  TSoccerVotingVotersPreferences = class
  private
    FProfile: TSoccerVotingCollectivePreferenceProfile;
    FProperties: TSoccerVotersPreferencesProperties;
    FPropertiesCalculated: boolean;
    function GetProperties: TSoccerVotersPreferencesProperties;
    procedure CalculateProperties;
    function GetProfile: TSoccerVotingCollectivePreferenceProfile;
  public
    constructor Create;
    property Profile: TSoccerVotingCollectivePreferenceProfile read GetProfile;
    property Properties: TSoccerVotersPreferencesProperties read GetProperties;
    destructor Destroy; override;
  end;

implementation

{ TSoccerVotingVoterPreferences }

procedure TSoccerVotingVotersPreferences.CalculateProperties;
var
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LAlternatives: TStringList;
  LAlternativesCopy: TStringList;
  LAlternative: string;
begin
  FProperties.VotersCount := FProfile.Count;

  for LVoter in FProfile do
  begin
    for LAlternative in LVoter do
      if LAlternatives.IndexOf(LAlternative) = -1 then
        LAlternatives.Add(LAlternative);
  end;
  FProperties.AlternativesCount := LAlternatives.Count;

  FProperties.Complete := true;
  for LVoter in FProfile do
  begin
    for LAlternative in LAlternatives do
    begin
      if LVoter.IndexOf(LAlternative) = -1 then
        FProperties.Complete := false;
    end;
  end;
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

function TSoccerVotingVotersPreferences.GetProfile
  : TSoccerVotingCollectivePreferenceProfile;
begin
  Result := FProfile;
  FPropertiesCalculated := false;
end;

function TSoccerVotingVotersPreferences.GetProperties
  : TSoccerVotersPreferencesProperties;
begin
  if not FPropertiesCalculated then
    CalculateProperties;
  Result := FProperties;
end;

end.
