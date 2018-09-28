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

exports
  getName;

begin

end.

