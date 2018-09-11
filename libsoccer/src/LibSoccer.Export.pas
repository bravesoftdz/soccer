unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

type

  TOutArray = array [0 .. 20] of PAnsiChar;
  POutArray = ^TOutArray;

procedure ExecScript(AScript: PAnsiChar; var OutArray: POutArray;
  var OutLength: Int32); stdcall;

implementation

procedure ExecScript(AScript: PAnsiChar; var OutArray: POutArray;
  var OutLength: Int32);
var
  LSoccer: TSoccer;
  LStrList: TList<AnsiString>;
  LStr: AnsiString;
  i: integer;
  LBorder: integer;
begin
  LSoccer := TSoccer.Create;
  try
    try
      LStrList := LSoccer.ExecScript(AScript);
      OutLength := LStrList.Count;
      if LStrList.Count - 1 > 20 then
        LBorder := 20
      else
        LBorder := LStrList.Count;
      for i := 0 to LStrList.Count - 1 do
      begin
        LStr := LStrList[i];
        OutArray[i] := PAnsiChar(LStr);
      end;
    except
      on E: Exception do
      begin
        OutArray[0] := PAnsiChar(AnsiString('error'));
        OutArray[1] := PAnsiChar(AnsiString(E.ClassName + ' : ' + E.Message));
      end;
    end;
  finally
    FreeAndNil(LSoccer);
  end;
end;

end.
