unit Soccer.Voting.AbstractRule;

interface

uses
  System.Generics.Collections,

  Soccer.Voting.Preferences;

type
  ISoccerVotingRule = interface
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: TList<string>): boolean;
  end;

implementation

end.
