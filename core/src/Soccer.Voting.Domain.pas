unit Soccer.Voting.Domain;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,

  Soccer.Exceptions,

  Soccer.Domain.Abstract,
  Soccer.Domain.Factory,

  Soccer.Voting.Actions,
  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList,
  Soccer.Voting.Preferences,
  Soccer.Voting.RuleChooser;

type
  TSoccerVotingDomain = class(TInterfacedObject, ISoccerDomain)
  private
    FRulePreferenceList: TSoccerVotingRulePreferenceList;
    FVotersPreferenceProfile: TSoccerVotingVotersPreferences;
    FOutput: TList<AnsiString>;
  public
    constructor Create;
    function AmIStarted(AWhatIsStarted: string): Boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: System.Generics.Collections.TList<System.AnsiString>;
    function SupportsCommand(ACommand: string): Boolean;
    destructor Destroy; override;
  end;

implementation

{ TVotingDomain }

function TSoccerVotingDomain.AmIStarted(AWhatIsStarted: string): Boolean;
begin
  Result := AWhatIsStarted = 'voting';
end;

constructor TSoccerVotingDomain.Create;
begin
  FRulePreferenceList := TSoccerVotingRulePreferenceList.Create
    (GetPreferenceFilePath);
  FVotersPreferenceProfile := TSoccerVotingVotersPreferences.Create;
  FOutput := nil;
end;

destructor TSoccerVotingDomain.Destroy;
begin
  FreeAndNil(FRulePreferenceList);
  FreeAndNil(FVotersPreferenceProfile);
  inherited;
end;

function TSoccerVotingDomain.GetActionForCommand(ACommand: string)
  : ISoccerAction;
begin
  Result := nil;
  if TRegEx.IsMatch(ACommand, 'IMPORT\[(.*)\]') then
    Result := TSoccerVotingImportAction.Create(GlobalVotingRulesDict,
      FRulePreferenceList);
  if TRegEx.IsMatch(ACommand, 'VOTE\((.*)\)') then
    Result := TSoccerVoteAction.Create(FVotersPreferenceProfile);
  if ACommand = 'DECIDE!' then
    Result := TSoccerDecideAction.Create(FVotersPreferenceProfile,
      FRulePreferenceList, FOutput, GetDefaultRuleChooser);
  if not Assigned(Result) then
    raise ESoccerParserException.Create('Unknown command: ' + ACommand);
end;

function TSoccerVotingDomain.GetOutput
  : System.Generics.Collections.TList<System.AnsiString>;
begin
  if not Assigned(FOutput) then
    raise ESoccerParserException.Create('No "DECIDE!" command found');
  Result := FOutput;
end;

function TSoccerVotingDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := TRegEx.IsMatch(ACommand, 'IMPORT\[(.*)\]') or
    TRegEx.IsMatch(ACommand, 'VOTE\((.*)\)');
end;

initialization

GlobalDomainFactory.Domains.Add(TSoccerVotingDomain.Create);

end.
