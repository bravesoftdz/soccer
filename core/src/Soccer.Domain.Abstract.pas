unit Soccer.Domain.Abstract;

interface

type
  ISoccerAction = interface
    procedure WorkOnCommand(ACommand: string);
  end;

  ISoccerDomain = interface
    function AmIStarted(AStartCommand: string): boolean;
    function GetActionForCommand(ACommand: string): ISoccerAction;
  end;

implementation

end.
