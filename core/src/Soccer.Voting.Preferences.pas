unit Soccer.Voting.Preferences;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TSoccerVotingIndividualPreferenceProfile = TList<string>;

  TSoccerVotingCollectivePreferenceProfile =
    TObjectList<TSoccerVotingIndividualPreferenceProfile>;

  TSoccerVotingVotersPreferences = class
  private
    FProfile: TSoccerVotingCollectivePreferenceProfile;
  public
    constructor Create;
    property Profile: TSoccerVotingCollectivePreferenceProfile read FProfile;
    destructor Destroy; override;
  end;

implementation

{ TSoccerVotingVoterPreferences }

constructor TSoccerVotingVotersPreferences.Create;
begin
  FProfile := TSoccerVotingCollectivePreferenceProfile.Create(true);
end;

destructor TSoccerVotingVotersPreferences.Destroy;
begin
  FreeAndNil(FProfile);
end;

end.
