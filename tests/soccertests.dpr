program soccertests;

{ ATTENTON
  To compile tests you need to set two environment variables:
  1)DUNITX - path to DUnitX
  2)LEAKCHECK - path to Delphi LeakCheck
}

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
  Soccer.Exceptions in '..\core\src\Soccer.Exceptions.pas',
  Soccer.Main in '..\core\src\Soccer.Main.pas',
  Soccer.Parser in '..\core\src\Soccer.Parser.pas',
  Soccer.Domain.Abstract in '..\core\src\Soccer.Domain.Abstract.pas',
  Soccer.Domain.Factory in '..\core\src\Soccer.Domain.Factory.pas',
  SoccerTests.DomainFactoryTests in 'SoccerTests.DomainFactoryTests.pas',
  SoccerTests.SoccerParserTests in 'SoccerTests.SoccerParserTests.pas',
  SoccerTests.MainTests in 'SoccerTests.MainTests.pas';

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
