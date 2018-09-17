unit Soccer.Domain.Abstract;

interface

uses
  System.Generics.Collections;

type
  ISoccerAction = interface
    procedure WorkOnCommand(ACommand: string);
  end;

  ISoccerDomain = interface
    procedure Initialize;
    function AmIStarted(AWhatIsStarted: string): boolean;
    function SupportsCommand(ACommand: string): boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
    function GetOutput: TList<string>;
    procedure DeInitialize;
  end;

implementation

end.
