unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

procedure ExecScript(AScript: PAnsiChar; var OutString: PAnsiChar;
  var OutLength: Int32); stdcall;

implementation

procedure ExecScript(AScript: PAnsiChar; var OutString: PAnsiChar;
  var OutLength: Int32);
var
  LSoccer: TSoccer;
  LStrList: TList<AnsiString>;
  LStr: AnsiString;
  i: integer;
  LPChar: PAnsiChar;
  LOutString: AnsiString;
begin
  LSoccer := TSoccer.Create;
  LOutString := '';
  try
    try
      LStrList := LSoccer.ExecScript(AScript);
      for i := 0 to LStrList.Count - 1 do
      begin
        LStr := LStrList[i];
        if i = 0 then
          LOutString := LStr
        else
          LOutString := '<>' + LStr;
      end;
    except
      on E: Exception do
      begin
        LOutString := 'error<>' + E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    FreeAndNil(LSoccer);
    OutString := PAnsiChar(LOutString);
    OutLength := Length(OutString);
  end;
end;

end.
