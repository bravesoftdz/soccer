unit Soccer.Voting.RuleChooser;

interface

uses
  System.SysUtils,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences;

type
  ISoccerVotingRuleChooser = interface
    function ChooseRule(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
  end;

  TSoccerRuleChooser = class(TInterfacedObject, ISoccerVotingRuleChooser)
  public
    function ChooseRule(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
  end;

function GetDefaultRuleChooser: ISoccerVotingRuleChooser;

implementation

function GetDefaultRuleChooser: ISoccerVotingRuleChooser;
begin
  Result := TSoccerRuleChooser.Create;
end;

{ TRuleChooser }

function TSoccerRuleChooser.ChooseRule(AProfile: TSoccerVotingVotersPreferences;
  ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
begin
  if ARules.Count = 0 then
    raise ESoccerParserException.Create('No rule was imported');
  Result := ARules.Items[0];
end;

end.
