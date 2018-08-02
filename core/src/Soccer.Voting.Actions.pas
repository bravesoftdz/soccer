unit Soccer.Voting.Actions;

interface

uses
  System.SysUtils,
  System.RegularExpressions,
  System.Generics.Collections,

  Soccer.Exceptions,

  Soccer.Domain.Abstract,

  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.AbstractRule,
  Soccer.Voting.Preferences,
  Soccer.Voting.RuleChooser;

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

  TSoccerVoteAction = class(TInterfacedObject, ISoccerAction)
  private
    FPreferenceStorage: TSoccerVotingVotersPreferences;
    function ExtractProfile(APreferences: string)
      : TSoccerVotingIndividualPreferenceProfile;
  public
    constructor Create(AVotePreferences: TSoccerVotingVotersPreferences);
    procedure WorkOnCommand(ACommand: string);
  end;

  TSoccerDecideAction = class(TInterfacedObject, ISoccerAction)
  private
    FPreferenceProfile: TSoccerVotingVotersPreferences;
    FRulesList: TSoccerVotingRulePreferenceList;
    FResult: TList<AnsiString>;
    FRuleChooser: IRuleChooser;
  public
    constructor Create(APreferenceProfile
  : TSoccerVotingVotersPreferences; ARulesList: TSoccerVotingRulePreferenceList;
  var AResult: TList<AnsiString>; ARuleChooser: IRuleChooser);
    procedure WorkOnCommand(ACommand: string);
    destructor Destroy; override;
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
    raise ESoccerParserException.Create('No rule with a name "' + LRuleName +
      '" found');
  LRule := FVotingRulesDict.Rules[LRuleName];
  FRulePreferenceList.Add(LRule);
end;

{ TSoccerVoteAction }

constructor TSoccerVoteAction.Create(AVotePreferences
  : TSoccerVotingVotersPreferences);
begin
  FPreferenceStorage := AVotePreferences;
end;

function TSoccerVoteAction.ExtractProfile(APreferences: string)
  : TSoccerVotingIndividualPreferenceProfile;
var
  ch: char;
  LAlternativeName: string;
begin
  Result := TSoccerVotingIndividualPreferenceProfile.Create;
  for ch in APreferences do
  begin
    if ch = '-' then
    begin
      Result.Add(LAlternativeName.Trim);
      LAlternativeName := '';
      continue;
    end;
    if ch = '>' then
      continue;
    LAlternativeName := LAlternativeName + ch;
  end;
  if LAlternativeName.Trim <> '' then
    Result.Add(LAlternativeName);
end;

procedure TSoccerVoteAction.WorkOnCommand(ACommand: string);
var
  RegEx: TRegEx;
  LPreference: string;
  LNewProfile: TSoccerVotingIndividualPreferenceProfile;
begin
  RegEx := TRegEx.Create('VOTE\((.*)\)');
  LPreference := RegEx.Match(ACommand).Groups[1].Value;
  LNewProfile := ExtractProfile(LPreference);
  FPreferenceStorage.Profile.Add(LNewProfile);
end;

{ TSoccerDecideAction }

constructor TSoccerDecideAction.Create(APreferenceProfile
  : TSoccerVotingVotersPreferences; ARulesList: TSoccerVotingRulePreferenceList;
  var AResult: TList<AnsiString>; ARuleChooser: IRuleChooser);
begin
  FRulesList := ARulesList;
  FPreferenceProfile := APreferenceProfile;
  FResult := AResult;
  FRuleChooser := ARuleChooser;
end;

destructor TSoccerDecideAction.Destroy;
begin
  FRulesList := nil;
  FPreferenceProfile := nil;
  FRuleChooser := nil;
  inherited;
end;

procedure TSoccerDecideAction.WorkOnCommand(ACommand: string);
var
  LRule: ISoccerVotingRule;
begin
  if not (ACommand = 'DECIDE!') then
    raise ESoccerParserException.Create('Command is not "DECIDE!"');
  LRule := FRuleChooser.ChooseRule(FPreferenceProfile, FRulesList);
  FResult := LRule.ExecuteOn(FPreferenceProfile);
end;

end.
