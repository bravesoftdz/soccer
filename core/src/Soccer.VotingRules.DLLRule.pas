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
  PPPChar = ^PPChar;

  TSoccerGetNameProc = function(): PChar; stdcall;
  TSoccerExecuteOnProc = function(AProfile: PPPChar;
    AProperties: TSoccerVotersPreferencesProperties; OutWinners: PPChar;
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

{$POINTERMATH ON}

function StrListToPtr(const AArray: TList < TList < String >> ): PPPChar;
{ Thanks to Dennis Göhlert for this function }
var
  X: integer;
  Y: integer;
begin
  GetMem(Result, SizeOf(PPChar) * AArray.Count);
  for X := 0 to AArray.Count - 1 do
  begin
    GetMem((Result + X)^, SizeOf(PChar) * AArray[X].Count);
    for Y := 0 to AArray[X].Count - 1 do
    begin
      ((Result + X)^ + Y)^ := StrNew(PChar(AArray[X][Y]));
    end;
  end;
end;

procedure FreeArrPtr(const APtr: PPPChar;
  AOriginalList: TList < TList < string >> );
{ Thanks to Dennis Göhlert for this function }
var
  X: integer;
  Y: integer;
begin
  for X := 0 to AOriginalList.Count - 1 do
  begin
    for Y := 0 to AOriginalList[X].Count - 1 do
    begin
      StrDispose(((APtr + X)^ + Y)^);
    end;
    FreeMem((APtr + X)^, SizeOf(PChar) * AOriginalList[X].Count);
  end;
  FreeMem(APtr, SizeOf(PPChar) * AOriginalList.Count);
end;

{$POINTERMATH OFF}
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
  LProfile: PPPChar;
  LPPWinners: PPChar;
  LPWinners: PChar;
  LWinner: string;
  LWinners: string;
  LWinnersLength: integer;
begin
  LProfile := StrListToPtr(AProfile.Profile);
  New(LPPWinners);
  Result := FExecuteOn(LProfile, AProfile.Properties, LPPWinners,
    LWinnersLength) > 0;
  LPWinners := LPPWinners^;
  Winners := TList<string>.Create;
  LWinners := LPWinners;
  for LWinner in SplitString(LWinners, '-') do
  begin
    Winners.Add(LWinner);
  end;
  FreeArrPtr(LProfile, AProfile.Profile);
  Dispose(LPPWinners);
end;

function TSoccerDLLVotingRule.GetName: string;
begin
  Result := string(FGetName());
end;

end.
