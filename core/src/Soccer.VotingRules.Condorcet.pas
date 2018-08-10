unit Soccer.VotingRules.Condorcet;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule,
  Soccer.Voting.RulesDict;

type
  TCollectivePreferenceRelation = TList<TPair<string, string>>;

  TSoccerCondorcetRule = class(TInterfacedObject, ISoccerVotingRule)
  private
    function FindAlternatives(AProfile: TSoccerVotingVotersPreferences)
      : TStringList;
    function DownProfile(AAlt1, AAlt2: string;
      AProfile: TSoccerVotingVotersPreferences): TSoccerVotingVotersPreferences;
    function IsFirstDominated(AFirstAlt, ASecondAlt: string;
      ADownProfile: TSoccerVotingVotersPreferences): integer;
    function FindWinners(APreferences: TCollectivePreferenceRelation;
      AAlternatives: TStringList): TList<AnsiString>;
  public
    function GetName: string;
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.
      TList<System.AnsiString>): Boolean;
  end;

implementation

{ TSoccerCondorcetRule }

function TSoccerCondorcetRule.DownProfile(AAlt1, AAlt2: string;
  AProfile: TSoccerVotingVotersPreferences): TSoccerVotingVotersPreferences;
var
  LVoter: TList<string>;
  LAlternative: string;
begin
  Result := TSoccerVotingVotersPreferences.Create;
  for LVoter in AProfile.Profile do
  begin
    Result.Profile.Add(TList<string>.Create);
    for LAlternative in LVoter do
    begin
      if LAlternative = AAlt1 then
        Result.Profile[Result.Profile.Count - 1].Add(AAlt1);
      if LAlternative = AAlt2 then
        Result.Profile[Result.Profile.Count - 1].Add(AAlt2);
    end;
  end;
end;

function TSoccerCondorcetRule.ExecuteOn
  (AProfile: TSoccerVotingVotersPreferences;
  out Winners: System.Generics.Collections.TList<System.AnsiString>): Boolean;
var
  LAlternatives: TStringList;
  LAlternative1, LAlternative2: string;
  LDownProfile: TSoccerVotingVotersPreferences;
  LIsDominatedResult: integer;
  LDominations: TCollectivePreferenceRelation;
begin
  LAlternatives := FindAlternatives(AProfile);
  LDominations := TCollectivePreferenceRelation.Create;
  for LAlternative1 in LAlternatives do
  begin
    for LAlternative2 in LAlternatives do
    begin
      if not(LAlternative1 = LAlternative2) then
      begin
        LDownProfile := DownProfile(LAlternative1, LAlternative2, AProfile);
        LIsDominatedResult := IsFirstDominated(LAlternative1, LAlternative2,
          LDownProfile);
        if LIsDominatedResult > 0 then
          LDominations.Add(TPair<string, string>.Create(LAlternative1,
            LAlternative2))
        else if LIsDominatedResult = 0 then
        begin
          LDominations.Add(TPair<string, string>.Create(LAlternative1,
            LAlternative2));
          LDominations.Add(TPair<string, string>.Create(LAlternative2,
            LAlternative1));
        end
        else if LIsDominatedResult < 0 then
          LDominations.Add(TPair<string, string>.Create(LAlternative2,
            LAlternative1));
        FreeAndNil(LDownProfile);
      end;
    end;
  end;
  Winners := FindWinners(LDominations, LAlternatives);
  Result := Winners.Count > 0;
end;

function TSoccerCondorcetRule.FindAlternatives
  (AProfile: TSoccerVotingVotersPreferences): TStringList;
var
  LAlternative: string;
  LVoter: TList<string>;
begin
  Result := TStringList.Create;
  if AProfile.Properties.Complete then
  begin
    if AProfile.Profile.Count > 0 then
      for LAlternative in AProfile.Profile[0] do
        Result.Add(LAlternative)
  end
  else
    for LVoter in AProfile.Profile do
    begin
      for LAlternative in LVoter do
      begin
        if Result.IndexOf(LAlternative) = -1 then
          Result.Add(LAlternative);
      end;
    end;
end;

function TSoccerCondorcetRule.FindWinners(APreferences
  : TCollectivePreferenceRelation; AAlternatives: TStringList)
  : TList<AnsiString>;
var
  LAlternative1, LAlternative2: string;
  LIsWinner: Boolean;
begin
  Result := TList<AnsiString>.Create;
  for LAlternative1 in AAlternatives do
  begin
    LIsWinner := true;
    for LAlternative2 in AAlternatives do
    begin
      if (APreferences.IndexOf(TPair<string, string>.Create(LAlternative2,
        LAlternative1)) >= 0) and
        (APreferences.IndexOf(TPair<string, string>.Create(LAlternative1,
        LAlternative2)) = -1) then
        LIsWinner := false;
    end;
    if LIsWinner then
      Result.Add(AnsiString(LAlternative1));
  end;
end;

function TSoccerCondorcetRule.GetName: string;
begin
  Result := 'condorcet';
end;

function TSoccerCondorcetRule.IsFirstDominated(AFirstAlt, ASecondAlt: string;
  ADownProfile: TSoccerVotingVotersPreferences): integer;
var
  LVoter: TList<string>;
  LFirstAlternative: string;
  LScore1, LScore2: integer;
begin
  LScore1 := 0;
  LScore2 := 0;
  for LVoter in ADownProfile.Profile do
  begin
    LFirstAlternative := LVoter[0];
    if LFirstAlternative = AFirstAlt then
      Inc(LScore1);
    if LFirstAlternative = ASecondAlt then
      Inc(LScore2);
  end;
  Result := LScore1 - LScore2;
end;

var
  LRule: TSoccerCondorcetRule;

initialization

LRule := TSoccerCondorcetRule.Create;
GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);

end.
