unit Soccer.Domain.Factory;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  Soccer.Domain.Abstract;

type
  TSoccerDomainFactory = class
  private
    FDomains: TList<ISoccerDomain>;
  public
    constructor Create;
    property Domains: TList<ISoccerDomain> read FDomains;
    destructor Destroy; override;
  end;

function GlobalDomainFactory: TSoccerDomainFactory;

implementation

var
  GDomainFactory: TSoccerDomainFactory;

function GlobalDomainFactory: TSoccerDomainFactory;
begin
  if not Assigned(GDomainFactory) then
    GDomainFactory := TSoccerDomainFactory.Create;
  Result := GDomainFactory;
end;

{ TSoccerDomainFactory }

constructor TSoccerDomainFactory.Create;
begin
  FDomains := TList<ISoccerDomain>.Create;
end;

destructor TSoccerDomainFactory.Destroy;
begin
  FreeAndNil(FDomains);
  inherited;
end;

end.
