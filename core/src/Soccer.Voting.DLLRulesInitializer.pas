unit Soccer.Voting.DLLRulesInitializer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,

  Soccer.Voting.AbstractRule,
  Soccer.Voting.RulesDict,

  Soccer.VotingRules.DLLRule;

implementation

function GetDLLRulesPath: string;
begin
  Result := '.\dllrules\';
{$IFDEF DEBUG}
  Result := '..\..\..\dllrules';
{$ENDIF}
end;

var
  LRule: ISoccerVotingRule;
  LPath: string;

initialization

if TDirectory.Exists(GetDLLRulesPath) then
  for LPath in TDirectory.GetFiles(GetDLLRulesPath) do
    if TPath.GetExtension(LPath) = '.dll' then
    begin
      LRule := TSoccerDLLVotingRule.Create(LPath);
      GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);
    end;

end.
