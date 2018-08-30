{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule64;

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

function getName: PAnsiChar; stdcall;
begin
  Result := PAnsiChar('nowinnerrule');
end;

function executeOn(AProfile: TProfile;
    AProperties: TSoccerVotersPreferencesProperties;
    var OutWinners: TWinners; var WinnersLength: integer): integer; stdcall;
begin
  Result := 0;
  WinnersLength := 0;
  SetLength(OutWinners, 0);
end;

exports 
    executeOn,
    getName;

begin
  
end.