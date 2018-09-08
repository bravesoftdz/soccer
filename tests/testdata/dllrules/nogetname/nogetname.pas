{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library nogetname;

{$mode objfpc}

type
    TVoter = array of PAnsiChar;
    TProfile = array of TVoter;
    TWinners = array of PAnsiChar;

    TSoccerVotersPreferencesProperties = record
        AlternativesCount: integer;
        VotersCount: integer;
        Complete: boolean;
    end;

function executeOn(AProfile: PAnsiChar;
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: PAnsiChar;
  var WinnersLength: integer): integer; stdcall;
begin
  Result := 0;
  WinnersLength := 0;
end;

exports 
    executeOn;

begin
  
end.
