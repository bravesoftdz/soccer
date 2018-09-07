unit Soccer.VotingRules.DLLRule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,

{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  Soccer.Exceptions,

  Soccer.Voting.RulesDict,
  Soccer.Voting.Preferences,
  Soccer.Voting.AbstractRule;

type

  TSoccerGetNameProc = function(): PAnsiChar; stdcall;
  TSoccerExecuteOnProc = function(AProfile: PAnsiChar;
    AProperties: TSoccerVotersPreferencesProperties; var OutWinners: PAnsiChar;
    var WinnersLength: integer): integer; stdcall;

  TSoccerDLLVotingRule = class(TInterfacedObject, ISoccerVotingRule)
  private
    FHandle: HMODULE;
    FExecuteOn: TSoccerExecuteOnProc;
    FGetName: TSoccerGetNameProc;
  public
    constructor Create(ADLLPath: string);
    function ExecuteOn(AProfile: TSoccerVotingVotersPreferences;
      out Winners: System.Generics.Collections.
      TList<System.AnsiString>): Boolean;
    function GetName: string;
    destructor Destroy; override;
  end;

implementation

{ TDLLRule }

function GetDLLRulesPath: string;
begin
  Result := '.\dllrules\';
{$IFDEF DEBUG}
  Result := '..\..\..\dllrules';
{$ENDIF}
end;

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
  out Winners: System.Generics.Collections.TList<System.AnsiString>): Boolean;
var
  LPStrProfile: PAnsiChar;
  LStrProfile: AnsiString;
  LVoter: TSoccerVotingIndividualPreferenceProfile;
  LWinners: PAnsiChar;
  LWinner: PAnsiChar;
  LWinnerStr: AnsiString;
  LAlternative: AnsiString;
  i, j, LWinnersLength: integer;
begin
  LPStrProfile := '';
  for i := 0 to AProfile.Profile.Count - 1 do
  begin
    LVoter := AProfile.Profile[i];
    for j := 0 to LVoter.Count - 1 do
    begin
      LAlternative := AnsiString(LVoter[j]);
      LStrProfile := LStrProfile + LAlternative + ',';
    end;
  end;
  LPStrProfile := PAnsiChar(LStrProfile);
  Result := FExecuteOn(LPStrProfile, AProfile.Properties, LWinners,
    LWinnersLength) > 0;
  Winners := TList<AnsiString>.Create;
  for i := 0 to LWinnersLength - 1 do
  begin
    Winners.Add(LWinners);
  end;
end;

function TSoccerDLLVotingRule.GetName: string;
begin
  Result := string(AnsiString(FGetName()));
end;

var
  LRule: ISoccerVotingRule;
  LPath: string;

initialization

if TDirectory.Exists(GetDLLRulesPath) then
  for LPath in TDirectory.GetFiles(GetDLLRulesPath) do
    if TPath.GetExtension(LPath) = '.dll' then
    begin
      LRule := TSoccerDLLVotingRule.Create(LPath);
      GlobalVotingRulesDict.Rules.Add(LRule.GetName, LRule);
    end;

end.
