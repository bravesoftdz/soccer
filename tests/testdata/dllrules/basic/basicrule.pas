{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule;

{$mode delphi}

uses
  SysUtils;

type
  TProfile = array of array of string;

  TSoccerVotersPreferencesProperties = record
    AlternativesCount: integer;
    VotersCount: integer;
    Complete: boolean;
  end;

  function getName: PWideChar; stdcall;
  begin
    Result := PWideChar('nowinnerrule');
  end;

  function ConvertProfile(AProfile: PWideChar): TProfile;
  var
    LStrProfile: string;
    LVoters: TStringArray;
    LVoter: TStringArray;
    i, j: integer;
  begin
    LStrProfile := string(AProfile);
    LVoters := LStrProfile.Split(['>']);
    SetLength(Result, Length(LVoters));
    for i := 0 to Length(LVoters) - 1 do
    begin
      LVoter := LVoters[i].Split(['-']);
      SetLength(Result[i], Length(LVoter));
      for j := 0 to Length(LVoter) - 1 do
        Result[i][j] := LVoter[j];
    end;
  end;

  function executeOn(AProfile: PWideChar; AProperties: TSoccerVotersPreferencesProperties;
  var OutWinners: PWideChar; var WinnersLength: integer): integer; stdcall;
  var
    LProfile: TProfile;
    LWinner: string;
  begin
    if AProperties.VotersCount = 2 then
    begin
      LProfile := ConvertProfile(AProfile);
      WinnersLength := 1;
      LWinner := LProfile[1][0];
      OutWinners := PWideChar(LWinner);
    end
    else
      Result := 0;
  end;

exports
  executeOn,
  getName;

begin

end.
