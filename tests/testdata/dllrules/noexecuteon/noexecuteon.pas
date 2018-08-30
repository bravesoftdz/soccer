{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library noexecuteon;

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

exports
    getName;

begin
  
end.
