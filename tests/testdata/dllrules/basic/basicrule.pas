{
 This is a DLL rule for tests. You need to compile it into Win32 DLL.
}
library basicrule;

{$mode delphi}

uses
  SysUtils,
  fgl;

type
  TIndividualProfile = TFPGList<UnicodeString>;
  TProfile = TFPGList<TIndividualProfile>;

  PPPWideChar = ^PPWideChar;

  TSoccerVotersPreferencesProperties = record
    AlternativesCount: integer;
    VotersCount: integer;
    Complete: boolean;
  end;

  function getName: PWideChar; stdcall;
  begin
    Result := PWideChar('nowinnerrule');
  end;

  {$POINTERMATH ON}

  function ConvertProfile(AProfile: PPPWideChar;
    AProperties: TSoccerVotersPreferencesProperties): TProfile;
  var
    LVoter: PPWideChar;
    LAlternative: PWideChar;
    LAlternativeCopy: PWideChar;
    i, j: integer;
  begin
    Result := TProfile.Create;
    for i := 0 to AProperties.VotersCount - 1 do
    begin
      LVoter := (AProfile + i)^;
      Result.Add(TIndividualProfile.Create);
      for j := 0 to AProperties.AlternativesCount - 1 do
      begin
        LAlternative := (LVoter + j)^;
        GetMem(LAlternativeCopy, (StrLen(LAlternative) + 1) * SizeOf(widechar));
        StrCopy(LAlternativeCopy, LAlternative);
        Result[i].Add(UnicodeString(LAlternativeCopy));
      end;
    end;
  end;

  {$POINTERMATH OFF}

  function executeOn(AProfile: PPPWideChar;
    AProperties: TSoccerVotersPreferencesProperties; OutWinners: PPWideChar;
  var WinnersLength: integer): integer; stdcall;
  var
    LProfile: TProfile;
    LWinner: UnicodeString;
  begin
    if AProperties.VotersCount = 2 then
    begin
      LProfile := ConvertProfile(AProfile, AProperties);
      WinnersLength := 1;
      LWinner := LProfile[1][0];
      OutWinners^ := PWideChar(LWinner);
    end
    else
      Result := 0;
  end;

exports
  executeOn,
  getName;

begin

end.
