program soccer;

{$mode objfpc}

uses
  SysUtils,
  Classes;

  procedure ExecScript(AScript: PAnsiChar; var OutString: PAnsiChar;
  var OutLength: Int32); stdcall;
  external 'libsoccer.dll';

var
  LFileName: string;
  LStringList: TStringList;
  LScript: PAnsiChar;
  LOutStr: PAnsiChar;
  LOutLength: Int32;
  LTest: ansistring;

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
      LTest := ansistring(LStringList.Text);
      LScript := PAnsiChar(LTest);
      ExecScript(LScript, LOutStr, LOutLength);
      WriteLn(LOutStr);
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeAndNil(LStringList);
  end;
  ReadLn();
end.
