unit Soccer.Voting.RuleChooser;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Exceptions,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences;

type
  ISoccerVotingRuleChooser = interface
    function ChooseRuleFindWinners(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList): TList<AnsiString>;
  end;

  TSoccerRuleChooser = class(TInterfacedObject, ISoccerVotingRuleChooser)
  public
    function ChooseRuleFindWinners(AProfile: TSoccerVotingVotersPreferences;
      ARules: TSoccerVotingRulePreferenceList)
      : System.Generics.Collections.TList<System.AnsiString>;
  end;

function GetDefaultRuleChooser: ISoccerVotingRuleChooser;

implementation

function GetDefaultRuleChooser: ISoccerVotingRuleChooser;
begin
  Result := TSoccerRuleChooser.Create;
end;

{ TRuleChooser }

function TSoccerRuleChooser.ChooseRuleFindWinners
  (AProfile: TSoccerVotingVotersPreferences;
  ARules: TSoccerVotingRulePreferenceList)
  : System.Generics.Collections.TList<System.AnsiString>;
var
  i: integer;
  LRule: ISoccerVotingRule;
  LResult: TList<AnsiString>;
begin
  if not AProfile.Properties.Complete then
    raise Exception.Create('Incompete profiles are for now not supported');
  LResult := TList<AnsiString>.Create;
  if ARules.Count = 0 then
    raise ESoccerParserException.Create('No rule was imported');
  for i := 0 to ARules.Count - 1 do
  begin
    LResult.Clear;
    LRule := ARules.Items[0];
    if LRule.ExecuteOn(AProfile, LResult) then
      break;
  end;
  Result := TList<AnsiString>.Create;
  Result.Add(LRule.GetName);
  Result.AddRange(LResult.ToArray);
  FreeAndNil(LResult);
end;

end.
