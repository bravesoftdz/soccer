unit Soccer.Voting.RuleChooser;

interface

uses
  System.SysUtils,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences;

type
  IRuleChooser = interface
    function ChooseRule(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
  end;

  TRuleChooser = class(TInterfacedObject, IRuleChooser)
  public
    function ChooseRule(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
  end;

function GetDefaultRuleChooser: IRuleChooser;

implementation

function GetDefaultRuleChooser: IRuleChooser;
begin
  Result := TRuleChooser.Create;
end;

{ TRuleChooser }

function TRuleChooser.ChooseRule(AProfile: TSoccerVotingVotersPreferences;
  ARules: TSoccerVotingRulePreferenceList): ISoccerVotingRule;
begin
  if ARules.Count = 0 then
    raise ESoccerParserException.Create('No rule was imported');
  Result := ARules.Items[0];
end;

end.
