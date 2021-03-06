library libsoccer;

uses
  System.SysUtils,
  System.Classes,
  LibSoccer.Export in 'src\LibSoccer.Export.pas',
  Soccer.Main in '..\core\src\Soccer.Main.pas',
  Soccer.Parser in '..\core\src\Soccer.Parser.pas',
  Soccer.Exceptions in '..\core\src\Soccer.Exceptions.pas',
  Soccer.Domain.Factory in '..\core\src\Soccer.Domain.Factory.pas',
  Soccer.Domain.Abstract in '..\core\src\Soccer.Domain.Abstract.pas',
  Soccer.Voting.Domain in '..\core\src\Soccer.Voting.Domain.pas',
  Soccer.Voting.Actions in '..\core\src\Soccer.Voting.Actions.pas',
  Soccer.Voting.RulesDict in '..\core\src\Soccer.Voting.RulesDict.pas',
  Soccer.Voting.AbstractRule in '..\core\src\Soccer.Voting.AbstractRule.pas',
  Soccer.Voting.RulePreferenceList in '..\core\src\Soccer.Voting.RulePreferenceList.pas',
  Soccer.Voting.Preferences in '..\core\src\Soccer.Voting.Preferences.pas',
  Soccer.Voting.RuleChooser in '..\core\src\Soccer.Voting.RuleChooser.pas',
  Soccer.VotingRules.Plurality in '..\core\src\Soccer.VotingRules.Plurality.pas',
  Soccer.VotingRules.Condorcet in '..\core\src\Soccer.VotingRules.Condorcet.pas',
  Soccer.VotingRules.Borda in '..\core\src\Soccer.VotingRules.Borda.pas',
  Soccer.VotingRules.DLLRule in '..\core\src\Soccer.VotingRules.DLLRule.pas',
  Soccer.Voting.DLLRulesInitializer in '..\core\src\Soccer.Voting.DLLRulesInitializer.pas';

{$R *.res}

exports
  LibSoccer.Export.ExecScript,
  LibSoccer.Export.FreeSoccerPtr;

begin

end.
