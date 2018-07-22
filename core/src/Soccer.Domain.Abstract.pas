unit Soccer.Domain.Abstract;

interface

uses
  System.Generics.Collections;

type
  ISoccerAction = interface
    procedure WorkOnCommand(ACommand: string);
  end;

  ISoccerDomain = interface
    function AmIStarted(AWhatIsStarted: string): boolean;
    function SupportsCommand(ACommand: string): boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: TList<AnsiString>;
  end;

implementation

end.
