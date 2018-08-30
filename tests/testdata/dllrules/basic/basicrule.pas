{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule;

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
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: TWinners;
  var WinnersLength: integer): integer; stdcall;
  begin
    if AProperties.VotersCount = 1 then
    begin
      Result := 1;
      WinnersLength := 1;
      SetLength(OutWinners, 1);
      OutWinners[0] := AProfile[0][0];
    end
    else
      Result := 0;
  end;

exports
  executeOn,
  getName;

begin

end.
