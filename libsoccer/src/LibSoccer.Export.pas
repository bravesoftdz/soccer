unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

procedure ExecScript(AScript: PAnsiChar; var OutArray: TArray<PAnsiChar>;
  var OutLength: Int32); stdcall;

implementation

procedure ExecScript(AScript: PAnsiChar; var OutArray: TArray<PAnsiChar>;
  var OutLength: Int32);
var
  LSoccer: TSoccer;
  LStrList: TList<AnsiString>;
  LStr: AnsiString;
  i: integer;
begin
  LSoccer := TSoccer.Create;
  try
    try
      LStrList := LSoccer.ExecScript(AScript);
      SetLength(OutArray, LStrList.Count);
      OutLength := LStrList.Count;
      for i := 0 to LStrList.Count - 1 do
      begin
        LStr := LStrList[i];
        OutArray[i] := PAnsiChar(LStr);
      end;
    except
      on E: Exception do
      begin
        SetLength(OutArray, 2);
        OutArray[0] := PAnsiChar(AnsiString('error'));
        OutArray[1] := PAnsiChar(AnsiString(E.ClassName + ' : ' + E.Message));
      end;
    end;
  finally
    FreeAndNil(LSoccer);
  end;
end;

end.
