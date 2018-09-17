unit Soccer.VotingRules.DLLRule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Types,
  System.StrUtils,

{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  Soccer.Exceptions,

  Soccer.Voting.RulesDict,
  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule;

type
  TSoccerGetNameProc = function(): PChar; stdcall;
  TSoccerExecuteOnProc = function(AProfile: PChar;
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: PChar;
    var WinnersLength: integer): integer; stdcall;

  TSoccerDLLVotingRule = class(TInterfacedObject, ISoccerVotingRule)
  private
    FHandle: HMODULE;
    FExecuteOn: TSoccerExecuteOnProc;
    FGetName: TSoccerGetNameProc;
  public
    constructor Create(ADLLPath: string);
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.TList<System.string>): Boolean;
    function GetName: string;
    destructor Destroy; override;
  end;

implementation

{ TDLLRule }

constructor TSoccerDLLVotingRule.Create(ADLLPath: string);
var
  WCharPath: PWideChar;
  Card: cardinal;
  Msg: string;
begin
  @FExecuteOn := nil;
  @FGetName := nil;
  WCharPath := PWideChar(ADLLPath);
  FHandle := LoadLibrary(WCharPath);
  if FHandle <> 0 then
  begin
    @FExecuteOn := GetProcAddress(FHandle, 'executeOn');
    if @FExecuteOn = nil then
      raise ESoccerParserException.Create('Library "' + ADLLPath +
        '" has no "executeOn" function');
    @FGetName := GetProcAddress(FHandle, 'getName');
    if @FGetName = nil then
      raise ESoccerParserException.Create('Library "' + ADLLPath +
        '" has no "getName" function');
  end
  else
  begin
    Card := GetLastError;
    Msg := SysErrorMessage(Card);
    raise ESoccerParserException.Create(StringReplace(Msg, '%1', ADLLPath, []));
  end;
end;

destructor TSoccerDLLVotingRule.Destroy;
begin
  FreeLibrary(FHandle);
  inherited;
end;

function TSoccerDLLVotingRule.ExecuteOn
  (AProfile: TSoccerVotingVotersPreferences;
  out Winners: System.Generics.Collections.TList<System.string>): Boolean;
var
  LProfile: string;
  LPProfile: PChar;
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LPWinners: PChar;
  LWinner: string;
  LWinners: string;
  LAlternative: string;
  i, j, LWinnersLength: integer;
begin
  LProfile := '';
  for i := 0 to AProfile.Profile.Count - 1 do
  begin
    LVoter := AProfile.Profile[i];
    for j := 0 to LVoter.Count - 1 do
    begin
      LAlternative := LVoter[j];
      LProfile := LProfile + LAlternative;
      if j <> LVoter.Count - 1 then
        LProfile := LProfile + '-';
    end;
    if i <> AProfile.Profile.Count - 1 then
      LProfile := LProfile + '>';
  end;
  LPProfile := PChar(LProfile);
  Result := FExecuteOn(LPProfile, AProfile.Properties, LPWinners,
    LWinnersLength) > 0;
  Winners := TList<string>.Create;
  LWinners := LPWinners;
  for LWinner in SplitString(LWinners, '-') do
  begin
    Winners.Add(LWinner);
  end;
end;

function TSoccerDLLVotingRule.GetName: string;
begin
  Result := string(FGetName());
end;

end.
