library libsoccer;

uses
  System.SysUtils,
  System.Classes,
  LibSoccer.Export in 'src\LibSoccer.Export.pas',
  Soccer.Main in '..\core\src\Soccer.Main.pas',
  Soccer.Parser in '..\core\src\Soccer.Parser.pas',
  Soccer.Exceptions in '..\core\src\Soccer.Exceptions.pas',
  Soccer.Domain.Factory in '..\core\src\Soccer.Domain.Factory.pas',
  Soccer.Domain.Abstract in '..\core\src\Soccer.Domain.Abstract.pas';

{$R *.res}

exports
  LibSoccer.Export.ExecScript;

begin

end.
