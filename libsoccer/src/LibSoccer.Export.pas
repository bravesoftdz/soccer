unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

function ExecScript(AScript: PChar; var OutLength: Int32): PChar; stdcall;

procedure FreeSoccerString(var AStr: PChar); stdcall;

implementation

function ExecScript(AScript: PChar; var OutLength: Int32): PChar; stdcall;
var
  LSoccer: TSoccer;
  LStrList: TList<string>;
  LStr: string;
  i: integer;
  LOutString: string;
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
    Result := PChar(LOutString);
    OutLength := Length(Result);
  end;
end;

procedure FreeSoccerString(var AStr: PChar);
begin
  AStr := nil;
end;

end.
