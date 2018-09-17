unit Soccer.VotingRules.Plurality;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Voting.RulesDict,
  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule;

type
  TSoccerPluralityVotingRule = class(TInterfacedObject, ISoccerVotingRule)
  private
    FMoreThenTwoAlternativesAllowed: boolean;
    procedure CalculateScores(LCandidates
      : System.Generics.Collections.TList<string>;
      LScores: System.Generics.Collections.TList<Integer>;
      AProfile: TSoccerVotingVotersPreferences);
    function FindMaximalScore(AScoresList: TList<Integer>): Integer;
    function FindBestCandidates(ACandidatesList: TList<string>;
      AScoresList: TList<Integer>; AMaxScore: Integer): TList<string>;
    function IsAppliable(AProfile: TSoccerVotingVotersPreferences): boolean;
  public
    constructor Create(AMoreThenTwoAlternativesAllowed: boolean);
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: TList<string>): boolean;
  end;

implementation

{ TSoccerPluralityVotingRule }

constructor TSoccerPluralityVotingRule.Create(AMoreThenTwoAlternativesAllowed
  : boolean);
begin
  FMoreThenTwoAlternativesAllowed := AMoreThenTwoAlternativesAllowed;
end;

function TSoccerPluralityVotingRule.ExecuteOn
  (AProfile: TSoccerVotingVotersPreferences;
      out Winners: TList<string>): boolean;
var
  LCandidates: TList<string>;
  LScores: TList<Integer>;
  LMax: Integer;
begin
  Result := IsAppliable(AProfile);
  if not Result then
    exit;
  LCandidates := TList<string>.Create;
  LScores := TList<Integer>.Create;
  CalculateScores(LCandidates, LScores, AProfile);
  LMax := FindMaximalScore(LScores);
  Winners := FindBestCandidates(LCandidates, LScores, LMax);
  FreeAndNil(LCandidates);
  FreeAndNil(LScores);
end;

function TSoccerPluralityVotingRule.FindBestCandidates(ACandidatesList: TList<string>;
      AScoresList: TList<Integer>; AMaxScore: Integer): TList<string>;
var
  i: Integer;
begin
  Result := TList<string>.Create;
  for i := 0 to ACandidatesList.Count - 1 do
  begin
    if AScoresList[i] = AMaxScore then
      Result.Add(ACandidatesList[i]);
  end;
end;

function TSoccerPluralityVotingRule.FindMaximalScore
  (AScoresList: TList<Integer>): Integer;
var
  LScore: Integer;
begin
  Result := 0;
  for LScore in AScoresList do
  begin
    if LScore > Result then
      Result := LScore;
  end;
end;

procedure TSoccerPluralityVotingRule.CalculateScores
  (LCandidates
      : System.Generics.Collections.TList<string>;
      LScores: System.Generics.Collections.TList<Integer>;
      AProfile: TSoccerVotingVotersPreferences);
var
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LFirstAlternative: string;
  LIndex: Integer;
begin
  { Calculate scores }
  for LVoter in AProfile.Profile do
  begin
    LFirstAlternative := LVoter[0];
    if not LCandidates.Contains(LFirstAlternative) then
    begin
      LCandidates.Add(LFirstAlternative);
      LScores.Add(1);
    end
    else
    begin
      LIndex := LCandidates.IndexOf(LFirstAlternative);
      LScores[LIndex] := LScores[LIndex] + 1;
    end;
  end;
end;

function TSoccerPluralityVotingRule.GetName: string;
begin
  if FMoreThenTwoAlternativesAllowed then
    Result := 'plurality'
  else
    Result := 'plurality2';
end;

function TSoccerPluralityVotingRule.IsAppliable
  (AProfile: TSoccerVotingVotersPreferences): boolean;
begin
  Result := FMoreThenTwoAlternativesAllowed or
    (AProfile.Properties.AlternativesCount <= 2);
end;

var
  LRule: ISoccerVotingRule;

initialization

LRule := TSoccerPluralityVotingRule.Create(false);
GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);
LRule := TSoccerPluralityVotingRule.Create(true);
GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);

end.
