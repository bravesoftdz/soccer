unit Soccer.Domain.Factory;

interface

uses
  System.Generics.Collections,

  Soccer.Domain.Abstract;

type
  TSoccerDomainFactory = class
  private
    FDomains: TList<ISoccerDomain>;
  public
    property Domains: TList<ISoccerDomain> read FDomains;
  end;

  function GlobalDomainFactory: TSoccerDomainFactory;

implementation

var
  GDomainFactory: TSoccerDomainFactory;

function GlobalDomainFactory: TSoccerDomainFactory;
begin
  if not Assigned(GDomainFactory) then
    GDomainFactory := TSoccerDomainFactory.Create;
  Result := GlobalDomainFactory;
end;

end.
