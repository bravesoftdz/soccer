unit Soccer.Voting.RulesDict;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Voting.AbstractRule;

type
  TSoccerVotingRulesDict = class
  private
    FRules: TDictionary<string, ISoccerVotingRule>;
  public
    constructor Create;
    property Rules: TDictionary<string, ISoccerVotingRule> read FRules;
    destructor Destroy; override;
  end;

function GlobalVotingRulesDict: TSoccerVotingRulesDict;

implementation

var
  GDict: TSoccerVotingRulesDict;

function GlobalVotingRulesDict: TSoccerVotingRulesDict;
begin
  if not Assigned(GDict) then
    GDict := TSoccerVotingRulesDict.Create;
  Result := GDict;
end;

{ TSoccerVotingRulesDict }

constructor TSoccerVotingRulesDict.Create;
begin
  FRules := TDictionary<string, ISoccerVotingRule>.Create;
end;

destructor TSoccerVotingRulesDict.Destroy;
begin
  FreeAndNil(FRules);
  inherited;
end;

end.
