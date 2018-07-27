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
  Soccer.Voting.RulePreferenceList in '..\core\src\Soccer.Voting.RulePreferenceList.pas';

{$R *.res}

exports
  LibSoccer.Export.ExecScript;

begin

end.
