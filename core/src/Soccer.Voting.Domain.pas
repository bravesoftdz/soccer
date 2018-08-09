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
    function GetOutput: System.Generics.Collections.TList<System.AnsiString>;
    procedure SetOutput(AValue: TList<AnsiString>);
  public
    procedure Initialize;
    function AmIStarted(AWhatIsStarted: string): Boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function SupportsCommand(ACommand: string): Boolean;
    property Output: TList<AnsiString> read GetOutput write SetOutput;
    procedure DeInitialize;
  end;

implementation

{ TVotingDomain }

function TSoccerVotingDomain.AmIStarted(AWhatIsStarted: string): Boolean;
begin
  Result := AWhatIsStarted = 'voting';
end;

procedure TSoccerVotingDomain.DeInitialize;
begin
  if Assigned(FRulePreferenceList) then
    FreeAndNil(FRulePreferenceList);
  if Assigned(FVotersPreferenceProfile) then
    FreeAndNil(FVotersPreferenceProfile);
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
  begin
    Result := TSoccerDecideAction.Create(FVotersPreferenceProfile,
      FRulePreferenceList, Self, GetDefaultRuleChooser);
  end;
end;

function TSoccerVotingDomain.GetOutput
  : System.Generics.Collections.TList<System.AnsiString>;
begin
  if not Assigned(FOutput) then
    raise ESoccerParserException.Create('No "DECIDE!" command found');
  Result := FOutput;
end;

procedure TSoccerVotingDomain.Initialize;
begin
  FRulePreferenceList := TSoccerVotingRulePreferenceList.Create
    (GetPreferenceFilePath);
  FVotersPreferenceProfile := TSoccerVotingVotersPreferences.Create;
  FOutput := nil;
end;

procedure TSoccerVotingDomain.SetOutput(AValue: TList<AnsiString>);
begin
  FOutput := AValue;
end;

function TSoccerVotingDomain.SupportsCommand(ACommand: string): Boolean;
begin
  Result := TRegEx.IsMatch(ACommand, 'IMPORT\[(.*)\]') or
    TRegEx.IsMatch(ACommand, 'VOTE\((.*)\)') or (ACommand = 'DECIDE!');
end;

initialization

GlobalDomainFactory.Domains.Add(TSoccerVotingDomain.Create);

end.
