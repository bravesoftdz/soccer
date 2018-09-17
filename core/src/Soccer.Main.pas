unit Soccer.Main;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Parser,
  Soccer.Domain.Factory;

type
  TSoccer = class
  private
    FParser: TSoccerParser;
  public
    constructor Create;
    function ExecScript(AScript: string): TList<string>;
    destructor Destroy; override;
  end;

implementation

{ TSoccer }

constructor TSoccer.Create;
begin
  FParser := TSoccerParser.Create(GlobalDomainFactory);
end;

destructor TSoccer.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

function TSoccer.ExecScript(AScript: string): TList<string>;
begin
  Result := FParser.ParseExecuteScript(string(AScript));
end;

end.
