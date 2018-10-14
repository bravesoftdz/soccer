program kemenydlltestsproject;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  KemenyDLLTests in 'KemenyDLLTests.pas',
  Soccer.Parser in '..\..\..\core\src\Soccer.Parser.pas',
  Soccer.Voting.AbstractRule in '..\..\..\core\src\Soccer.Voting.AbstractRule.pas',
  Soccer.Voting.Actions in '..\..\..\core\src\Soccer.Voting.Actions.pas',
  Soccer.Voting.Domain in '..\..\..\core\src\Soccer.Voting.Domain.pas',
  Soccer.Voting.Preferences in '..\..\..\core\src\Soccer.Voting.Preferences.pas',
  Soccer.Voting.RuleChooser in '..\..\..\core\src\Soccer.Voting.RuleChooser.pas',
  Soccer.Voting.RulePreferenceList in '..\..\..\core\src\Soccer.Voting.RulePreferenceList.pas',
  Soccer.Voting.RulesDict in '..\..\..\core\src\Soccer.Voting.RulesDict.pas',
  Soccer.VotingRules.Borda in '..\..\..\core\src\Soccer.VotingRules.Borda.pas',
  Soccer.VotingRules.Condorcet in '..\..\..\core\src\Soccer.VotingRules.Condorcet.pas',
  Soccer.VotingRules.DLLRule in '..\..\..\core\src\Soccer.VotingRules.DLLRule.pas',
  Soccer.VotingRules.Plurality in '..\..\..\core\src\Soccer.VotingRules.Plurality.pas',
  Soccer.Domain.Abstract in '..\..\..\core\src\Soccer.Domain.Abstract.pas',
  Soccer.Domain.Factory in '..\..\..\core\src\Soccer.Domain.Factory.pas',
  Soccer.Exceptions in '..\..\..\core\src\Soccer.Exceptions.pas',
  Soccer.Main in '..\..\..\core\src\Soccer.Main.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
