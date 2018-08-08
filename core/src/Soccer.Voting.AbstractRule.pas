unit Soccer.Voting.AbstractRule;

interface

uses
  System.Generics.Collections,

  Soccer.Voting.Preferences;

type
  ISoccerVotingRule = interface
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences): TList<AnsiString>;
    function IsAppliable(AProfile: TSoccerVotingVotersPreferences): boolean;
  end;

implementation

end.
