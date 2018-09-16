program soccer;

{$mode objfpc}

uses
  SysUtils,
  Classes;

  function ExecScript(AScript: PAnsiChar; var OutLength: Int32): PAnsiChar;
  stdcall; external 'libsoccer.dll';

  procedure FreeSoccerString(var AStr: PAnsiChar); stdcall; external 'libsoccer.dll';

  function ParseSoccerOutputToArray(ASoccerOut: PAnsiChar): TStringArray;
  var
    LStr: ansistring;
  begin
    LStr := ansistring(ASoccerOut);
    while Pos('<>', LStr) <> 0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := LStr.Substring(0, Pos('<>', LStr) + 1);
      LStr := LStr.Substring(Pos('<>', LStr) + 1);
      Result[Length(Result) - 1] :=
        Result[Length(Result) - 1].Substring(0, Length(Result[Length(Result) - 1]) - 2);
    end;
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := LStr;
  end;

var
  LFileName: string;
  LStringList: TStringList;
  LScript: PAnsiChar;
  LOutStr: PAnsiChar;
  LOutLength: Int32;
  LTest: ansistring;
  LOutArr: TStringArray;
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
      LTest := ansistring(LStringList.Text);
      LScript := PAnsiChar(LTest);
      LOutStr := ExecScript(LScript, LOutLength);
      LOutArr := ParseSoccerOutputToArray(LOutStr);
      if LOutArr[0] = 'error' then
        Writeln(LOutArr[0] + ': ' + LOutArr[1])
      else
      begin
        Writeln('Selected with ' + LOutArr[0]);
        Write('Winners: ');
        for i := 1 to Length(LOutArr)-1 do
          Write(LOutArr[i] + ' ');
        Writeln();
      end;
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeAndNil(LStringList);
    FreeSoccerString(LOutStr);
  end;
end.
