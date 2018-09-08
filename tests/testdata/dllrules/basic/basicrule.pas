{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule;

{$mode objfpc}

uses
  SysUtils;

type
  TProfile = array of array of ansistring;

  TSoccerVotersPreferencesProperties = record
    AlternativesCount: integer;
    VotersCount: integer;
    Complete: boolean;
  end;

  function getName: PAnsiChar; stdcall;
  begin
    Result := PAnsiChar('nowinnerrule');
  end;

  function ConvertProfile(AProfile: PAnsiChar): TProfile;
  var
    LStrProfile: ansistring;
    LVoters: TStringArray;
    LVoter: TStringArray;
    i, j: integer;
  begin
    LStrProfile := ansistring(AProfile);
    LVoters := LStrProfile.Split(['>']);
    SetLength(Result, Length(LVoters));
    for i := 0 to Length(LVoters)-1 do
    begin
      LVoter := LVoters[i].Split(['-']);
      SetLength(Result[i], Length(LVoter));
      for j := 0 to Length(LVoter)-1 do
        Result[i][j] := LVoter[j];
    end;
  end;

  function executeOn(AProfile: PAnsiChar;
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: PAnsiChar;
  var WinnersLength: integer): integer; stdcall;
  var
    LProfile: TProfile;
    LWinner: ansistring;
  begin
    if AProperties.VotersCount = 2 then
    begin
      LProfile := ConvertProfile(AProfile);
      WinnersLength := 1;
      LWinner := LProfile[1][0];
      OutWinners := PAnsiChar(LWinner);
    end
    else
      Result := 0;
  end;

exports
  executeOn,
  getName;

begin

end.
