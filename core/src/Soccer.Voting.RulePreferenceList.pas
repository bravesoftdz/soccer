unit Soccer.Voting.RulePreferenceList;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Voting.AbstractRule;

type
  TSoccerVotingRulePreferenceList = class
  private
    FList: TList<ISoccerVotingRule>;
    FPreferenceFile: string;
    function IsBiggerThan(ARule: ISoccerVotingRule;
      ALessPreferred: ISoccerVotingRule): boolean;
    function GetItems(i: integer): ISoccerVotingRule;
    function GetCount: integer;
  public
    constructor Create(APreferenceFileName: string);
    procedure Add(ARule: ISoccerVotingRule);
    property Items[i: integer]: ISoccerVotingRule read GetItems;
    property Count: integer read GetCount;
    destructor Destroy; override;
  end;

function GetPreferenceFilePath: string;

implementation

function GetPreferenceFilePath: string;
begin
{$IFDEF DEBUG}
  Result := '..\..\..\..\deploy\data\rules.cfg';
{$ELSE}
  Result := 'rules.cfg';
{$ENDIF}
end;

{ TSoccerVotingRulePreferenceList }

procedure TSoccerVotingRulePreferenceList.Add(ARule: ISoccerVotingRule);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if IsBiggerThan(ARule, FList[i]) then
    begin
      FList.Insert(i, ARule);
      exit;
    end;
  end;
  FList.Add(ARule);
end;

constructor TSoccerVotingRulePreferenceList.Create(APreferenceFileName: string);
begin
  FList := TList<ISoccerVotingRule>.Create;
  FPreferenceFile := APreferenceFileName;
end;

destructor TSoccerVotingRulePreferenceList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TSoccerVotingRulePreferenceList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TSoccerVotingRulePreferenceList.GetItems(i: integer)
  : ISoccerVotingRule;
begin
  Result := FList[i];
end;

function TSoccerVotingRulePreferenceList.IsBiggerThan(ARule, ALessPreferred
  : ISoccerVotingRule): boolean;
var
  LStr: string;
  LStringList: TStringList;
begin
  Result := false;
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(FPreferenceFile);
    for LStr in LStringList do
    begin
      if ARule.GetName = LStr.Trim then
      begin
        Result := true;
        exit;
      end;
      if ALessPreferred.GetName = LStr.Trim then
        exit;
    end;
  finally
    FreeAndNil(LStringList);
  end;
end;

end.
