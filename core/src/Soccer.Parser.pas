unit Soccer.Parser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,

  Soccer.Exceptions,
  Soccer.Domain.Abstract,
  Soccer.Domain.Factory;

type
  TSoccerParser = class
  private
    FDomainFactory: TSoccerDomainFactory;
    FDomain: ISoccerDomain;
    function FindStartingToken(AScript: string): integer;
    function IsCommandSeparator(AChar: char): boolean;
    function IsCommandEmpty(ACommand: string): boolean;
  public
    constructor Create(ADomainFactory: TSoccerDomainFactory);
    function ParseExecuteScript(AScript: string): TList<AnsiString>;
  end;

implementation

{ TSoccerParser }

constructor TSoccerParser.Create(ADomainFactory: TSoccerDomainFactory);
begin
  FDomainFactory := ADomainFactory;
end;

function TSoccerParser.FindStartingToken(AScript: string): integer;
var
  i: integer;
  ch: char;
  LCommand: string;
  LMatch: TMatch;
  LWhatIsStarted: string;
  LDomain: ISoccerDomain;
begin
  for i := 0 to AScript.Length - 1 do
  begin
    ch := AScript.Chars[i];
    if IsCommandSeparator(ch) then
    begin
      if IsCommandEmpty(LCommand) then
      begin
        LCommand := '';
        continue;
      end;
      { Is command of form START[something] }
      LMatch := TRegEx.Match(LCommand, 'START\[(.*)\]');
      if not LMatch.Success then
        raise ESoccerParserException.Create('No "START" command found');
      LWhatIsStarted := LMatch.Groups[1].Value;
      { Find an appropriate domain for solution }
      for LDomain in FDomainFactory.Domains do
        if LDomain.AmIStarted(LWhatIsStarted) then
        begin
          FDomain := LDomain;
          exit(i + 1)
        end;
    end;
  end;
  raise EParserError.Create('No domain found');
end;

function TSoccerParser.IsCommandEmpty(ACommand: string): boolean;
begin
  Result := ACommand.Trim = '';
end;

function TSoccerParser.IsCommandSeparator(AChar: char): boolean;
begin
  Result := Trim(AChar) = '';
end;

function TSoccerParser.ParseExecuteScript(AScript: string): TList<AnsiString>;
var
  ch: char;
  LCommand: string;
  LStartingPoint: integer;
  i: integer;
begin
  LStartingPoint := FindStartingToken(AScript);
  for i := LStartingPoint to AScript.Length - 1 do
  begin
    ch := AScript.Chars[i];
    // Parse
  end;
end;

end.
