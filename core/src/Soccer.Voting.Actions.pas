unit Soccer.Voting.Actions;

interface

uses
  System.RegularExpressions,

  Soccer.Exceptions,

  Soccer.Domain.Abstract,

  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.AbstractRule;

type
  TSoccerVotingImportAction = class(TInterfacedObject, ISoccerAction)
  private
    FVotingRulesDict: TSoccerVotingRulesDict;
    FRulePreferenceList: TSoccerVotingRulePreferenceList;
  public
    constructor Create(AVotingRulesDict: TSoccerVotingRulesDict;
      ARulePreferenceList: TSoccerVotingRulePreferenceList);
    procedure WorkOnCommand(ACommand: string);
  end;

implementation

{ TSoccerVotingImportAction }

constructor TSoccerVotingImportAction.Create(AVotingRulesDict
  : TSoccerVotingRulesDict;
  ARulePreferenceList: TSoccerVotingRulePreferenceList);
begin
  FVotingRulesDict := AVotingRulesDict;
  FRulePreferenceList := ARulePreferenceList;
end;

procedure TSoccerVotingImportAction.WorkOnCommand(ACommand: string);
var
  RegEx: TRegEx;
  LMatch: TMatch;
  LRuleName: string;
  LRule: ISoccerVotingRule;
begin
  RegEx := TRegEx.Create('IMPORT\[(.*)\]');
  LMatch := RegEx.Match(ACommand);
  LRuleName := LMatch.Groups[1].Value;
  if not FVotingRulesDict.Rules.ContainsKey(LRuleName) then
    raise ESoccerParserException.Create('No rule with a name ' + LRuleName +
      ' found');
  LRule := FVotingRulesDict.Rules[LRuleName];

end;

end.
