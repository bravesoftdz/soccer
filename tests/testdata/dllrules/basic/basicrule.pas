{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule;

{$mode objfpc}

uses
    SysUtils;

type

  TSoccerVotersPreferencesProperties = record
    AlternativesCount: integer;
    VotersCount: integer;
    Complete: boolean;
  end;

  function getName: PAnsiChar; stdcall;
  begin
    Result := PAnsiChar('nowinnerrule');
  end;

  function executeOn(AProfile: PAnsiChar;
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: PAnsiChar;
    var WinnersLength: integer): integer; stdcall;
  begin
    WinnersLength:= 1;
    OutWinners:= 'a';
  end;

exports
  executeOn,
  getName;

begin

end.
