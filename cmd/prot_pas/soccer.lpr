program soccer;

{$mode objfpc}

uses
  SysUtils,
  Classes;

  procedure ExecScript(AScript: PAnsiChar; var OutArray: array of PAnsiChar;
  var OutLength: Int32); stdcall; external 'libsoccer.dll';

var
  LFileName: string;
  LStringList: TStringList;
  LScript: PAnsiChar;
  LOutArray: array of PAnsiChar;
  LOutLength: Int32;
  i: integer;

begin
  if ParamCount > 0 then
    LFileName := ParamStr(1)
  else
  begin
    WriteLn('No file specified');
    exit;
  end;
  LStringList := TStringList.Create;
  try
    try
      LStringList.LoadFromFile(LFileName);
      LScript := PAnsiChar(LStringList.Text);
      ExecScript(LScript, LOutArray, LOutLength);
      for i := 0 to LOutLength - 1 do
        Writeln(LOutArray[0] + ',');
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeAndNil(LStringList);
  end;
  ReadLn();
end.
