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
      out ARules: TSoccerVotingRulePreferenceList): TList<string>;
  end;

  TSoccerRuleChooser = class(TInterfacedObject, ISoccerVotingRuleChooser)
  public
    function ChooseRuleFindWinners(AProfile: TSoccerVotingVotersPreferences;
      out ARules: TSoccerVotingRulePreferenceList)
      : System.Generics.Collections.TList<string>;
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
  out ARules: TSoccerVotingRulePreferenceList)
  : System.Generics.Collections.TList<System.string>;
var
  i: integer;
  LRule: ISoccerVotingRule;
  LResult: TList<string>;
  LRuleFound: Boolean;
begin
  LRuleFound := false;
  LResult := nil;
  if not AProfile.Properties.Complete then
    raise ESoccerParserException.Create
      ('Incompete profiles are not supported for now');
  if ARules.Count = 0 then
    raise ESoccerParserException.Create('No rule was imported');
  for i := 0 to ARules.Count - 1 do
  begin
    LRule := ARules.Items[i];
    if LRule.ExecuteOn(AProfile, LResult) then
    begin
      LRuleFound := true;
      break;
    end;
    if Assigned(LResult) then
      FreeAndNil(LResult);
  end;
  if not LRuleFound then
    raise ESoccerParserException.Create('No rule was found for your purposes');
  Result := TList<string>.Create;
  Result.Add(LRule.GetName);
  Result.AddRange(LResult.ToArray);
  FreeAndNil(LResult);
end;

end.
