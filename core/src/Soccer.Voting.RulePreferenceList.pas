unit Soccer.Voting.RulePreferenceList;

interface

uses
  Soccer.Voting.AbstractRule;

type
  TSoccerVotingRulePreferenceList = class
    procedure Add(ARule: ISoccerVotingRule);
  end;

function GlobalRulePreferenceList: TSoccerVotingRulePreferenceList;

implementation

var
  GRulePreferenceList: TSoccerVotingRulePreferenceList;

function GlobalRulePreferenceList: TSoccerVotingRulePreferenceList;
begin

end;

{ TSoccerVotingRulePreferenceList }

procedure TSoccerVotingRulePreferenceList.Add(ARule: ISoccerVotingRule);
begin

end;

end.
