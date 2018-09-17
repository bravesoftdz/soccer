unit Soccer.VotingRules.Borda;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Voting.RulesDict,
  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule;

type
  TSoccerBordaVotingScoreRule = class(TInterfacedObject, ISoccerVotingRule)
  private
    function FindWinners(LScores: TDictionary<string, integer>)
      : TList<string>;
  public
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.
      TList<string>): Boolean;
    function GetName: string;
  end;

implementation

{ TBordaScore }

function TSoccerBordaVotingScoreRule.ExecuteOn
  (AProfile: TSoccerVotingVotersPreferences;
  out Winners: System.Generics.Collections.TList<string>): Boolean;
var
  LScores: TDictionary<string, integer>;
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  i: integer;
  LAlternative: string;
begin
  Result := false;
  if not AProfile.Properties.Complete then
    exit;
  LScores := TDictionary<string, integer>.Create;
  for LAlternative in AProfile.Profile[0] do
    LScores.Add(LAlternative, 0);
  for LVoter in AProfile.Profile do
  begin
    for i := LVoter.Count - 1 downto 0 do
    begin
      LScores[LVoter[i]] := LScores[LVoter[i]] + (LVoter.Count - i);
    end;
  end;
  Winners := FindWinners(LScores);
  Result := true;
  FreeAndNil(LScores);
end;

function TSoccerBordaVotingScoreRule.FindWinners
  (LScores: TDictionary<string, integer>): TList<String>;
var
  LAlternative: string;
  LMaxScore: integer;
begin
  LMaxScore := 0;
  for LAlternative in LScores.Keys do
  begin
    if LScores[LAlternative] > LMaxScore then
      LMaxScore := LScores[LAlternative];
  end;
  Result := TList<String>.Create;
  for LAlternative in LScores.Keys do
  begin
    if LScores[LAlternative] = LMaxScore then
      Result.Add(LAlternative);
  end;
end;

function TSoccerBordaVotingScoreRule.GetName: string;
begin
  Result := 'borda';
end;

var
  LRule: ISoccerVotingRule;

initialization

LRule := TSoccerBordaVotingScoreRule.Create;
GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);

end.
