unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

function ExecScript(AScript: PAnsiChar; var OutLength: Int32)
  : PAnsiChar; stdcall;

procedure FreeSoccerString(var AStr: PAnsiChar); stdcall;

implementation

function ExecScript(AScript: PAnsiChar; var OutLength: Int32)
  : PAnsiChar; stdcall;
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
          LOutString := LOutString + '<>' + LStr;
      end;
    except
      on E: Exception do
      begin
        LOutString := 'error<>' + E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    FreeAndNil(LSoccer);
    Result := PAnsiChar(LOutString);
    OutLength := Length(Result);
  end;
end;

procedure FreeSoccerString(var AStr: PAnsiChar);
begin
  AStr := nil;
end;

end.
