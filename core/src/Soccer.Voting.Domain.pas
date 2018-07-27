unit Soccer.Voting.Domain;

interface

uses
  System.Generics.Collections,
  System.RegularExpressions,

  Soccer.Domain.Abstract,
  Soccer.Domain.Factory,

  Soccer.Voting.Actions,
  Soccer.Voting.RulesDict,
  Soccer.Voting.RulePreferenceList;

type
  TSoccerVotingDomain = class(TInterfacedObject, ISoccerDomain)
  public
    function AmIStarted(AWhatIsStarted: string): Boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: System.Generics.Collections.TList<System.AnsiString>;
    function SupportsCommand(ACommand: string): Boolean;
  end;

implementation

{ TVotingDomain }

function TSoccerVotingDomain.AmIStarted(AWhatIsStarted: string): Boolean;
begin
  Result := AWhatIsStarted = 'voting';
end;

function TSoccerVotingDomain.GetActionForCommand(ACommand: string)
  : ISoccerAction;
begin
  if TRegEx.IsMatch(ACommand, 'IMPORT\[(.*)\]') then
    Result := TSoccerVotingImportAction.Create(GlobalVotingRulesDict, GlobalRulePreferenceList);
end;

function TSoccerVotingDomain.GetOutput
  : System.Generics.Collections.TList<System.AnsiString>;
begin

end;

function TSoccerVotingDomain.SupportsCommand(ACommand: string): Boolean;
begin

end;

initialization

GlobalDomainFactory.Domains.Add(TSoccerVotingDomain.Create);

end.
