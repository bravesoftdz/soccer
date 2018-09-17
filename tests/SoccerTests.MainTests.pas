unit SoccerTests.MainTests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  DUnitX.TestFramework,

  Soccer.Main,
  Soccer.Exceptions;

type

  [TestFixture]
  TMainTests = class(TObject)
  public
    [Test]
    procedure FullTest;
    [Test]
    procedure NoDecideCommand;
    [Test]
    procedure IncompletePreferencesTest;
  end;

implementation

{ TMainTests }

procedure TMainTests.FullTest;
var
  LSoccer: TSoccer;
  LOutput: TList<string>;
begin
  LSoccer := TSoccer.Create;
  LOutput := LSoccer.ExecScript('START[voting] ' + 'IMPORT[plurality] ' +
    'VOTE(a->b->c) ' + 'VOTE(c->b->a) ' + 'VOTE(b->a->c) ' + 'VOTE(a->b->c) ' +
    'DECIDE! ');
  Assert.IsTrue(LOutput.Count = 2);
  Assert.IsTrue(LOutput[0] = 'plurality');
  Assert.IsTrue(LOutput[1] = 'a');
  FreeAndNil(LOutput);
  FreeAndNil(LSoccer);
end;

procedure TMainTests.IncompletePreferencesTest;
var
  LSoccer: TSoccer;
begin
  LSoccer := TSoccer.Create;
  Assert.WillRaise(
    procedure
    begin
      LSoccer.ExecScript('START[voting] ' + 'IMPORT[plurality] ' + 'VOTE(a->b) '
        + 'VOTE(a->b->c) ' + 'DECIDE!')
    end, ESoccerParserException,
    'Incompete profiles are for now not supported');
  FreeAndNil(LSoccer);
end;

procedure TMainTests.NoDecideCommand;
var
  LSoccer: TSoccer;
  LOutput: TList<string>;
begin
  LSoccer := TSoccer.Create;
  Assert.WillRaise(
    procedure
    begin
      LOutput := LSoccer.ExecScript('START[voting] ' + 'IMPORT[plurality] ' +
        'VOTE(a->b->c) ' + 'VOTE(c->b->a) ' + 'VOTE(b->a->c) ' +
        'VOTE(a->b->c) ');
    end, ESoccerParserException, 'No "DECIDE!" command found');
  FreeAndNil(LOutput);
  FreeAndNil(LSoccer);
end;

initialization

TDUnitX.RegisterTestFixture(TMainTests);

end.
