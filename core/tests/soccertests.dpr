program soccertests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  LeakCheck,
  LeakCheck.Utils,
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  DUnitX.MemoryLeakMonitor.LeakCheck,
  Soccer.Exceptions in '..\src\Soccer.Exceptions.pas',
  Soccer.Main in '..\src\Soccer.Main.pas',
  Soccer.Parser in '..\src\Soccer.Parser.pas',
  Soccer.Domain.Abstract in '..\src\Soccer.Domain.Abstract.pas',
  Soccer.Domain.Factory in '..\src\Soccer.Domain.Factory.pas',
  SoccerTests.DomainFactoryTests in 'SoccerTests.DomainFactoryTests.pas',
  SoccerTests.SoccerParserTests in 'SoccerTests.SoccerParserTests.pas',
  SoccerTests.MainTests in 'SoccerTests.MainTests.pas',
  Soccer.Voting.AbstractRule in '..\src\Soccer.Voting.AbstractRule.pas',
  Soccer.Voting.Actions in '..\src\Soccer.Voting.Actions.pas',
  Soccer.Voting.Domain in '..\src\Soccer.Voting.Domain.pas',
  Soccer.Voting.Preferences in '..\src\Soccer.Voting.Preferences.pas',
  Soccer.Voting.RulePreferenceList in '..\src\Soccer.Voting.RulePreferenceList.pas',
  Soccer.Voting.RulesDict in '..\src\Soccer.Voting.RulesDict.pas',
  SoccerTests.VotingRulesDictTests in 'SoccerTests.VotingRulesDictTests.pas',
  SoccerTests.VotingRulePreferenceListTests in 'SoccerTests.VotingRulePreferenceListTests.pas',
  SoccerTests.VotingActionTests in 'SoccerTests.VotingActionTests.pas',
  Soccer.Voting.RuleChooser in '..\src\Soccer.Voting.RuleChooser.pas',
  Soccer.VotingRules.Plurality in '..\src\Soccer.VotingRules.Plurality.pas',
  SoccerTests.RuleChooserTests in 'SoccerTests.RuleChooserTests.pas',
  SoccerTests.PreferencesTests in 'SoccerTests.PreferencesTests.pas',
  Soccer.VotingRules.Condorcet in '..\src\Soccer.VotingRules.Condorcet.pas',
  SoccerTests.VotingCondorcetTests in 'SoccerTests.VotingCondorcetTests.pas',
  Soccer.VotingRules.Borda in '..\src\Soccer.VotingRules.Borda.pas',
  SoccerTests.VotingBordaTests in 'SoccerTests.VotingBordaTests.pas',
  Soccer.VotingRules.DLLRule in '..\src\Soccer.VotingRules.DLLRule.pas',
  SoccerTests.DLLRuleTests in 'SoccerTests.DLLRuleTests.pas';

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);
    // Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create
      (TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False;
    // When true, Assertions must be made during tests;

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
    // We don't want this happening when running under CI.
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
