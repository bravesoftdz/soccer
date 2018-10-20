program soccer;

{$mode delphi}

uses
  SysUtils,
  Classes;

  function ExecScript(AScript: PWideChar; var OutLength: Int32): PPWideChar;
  stdcall; external 'libsoccer.dll';

  procedure FreeSoccerPtr(var APtr: PPWideChar; ALength: Int32); stdcall; external 'libsoccer.dll';

  function ParseSoccerOutputToArray(ASoccerOut: PPWideChar;
    AWinnersLength: integer): TStringArray;
  var
    i: integer;
    LWinner: PWideChar;
    LWinnerStr : ansistring;
  begin
    for i := 0 to AWinnersLength - 1 do
    begin
      LWinner := (ASoccerOut + i)^;
      LWinnerStr := ansistring(LWinner);
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := LWinnerStr;
    end;
  end;

var
  LFileName: string;
  LStringList: TStringList;
  LScript: PWideChar;
  LOutPointer: PPWideChar;
  LOutLength: Int32;
  LTest: UnicodeString;
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
  LOutLength := 0;
  try
    try
      LStringList.LoadFromFile(LFileName);
      LTest := UnicodeString(Copy(LStringList.Text, 1, Length(LStringList.Text)));
      LScript := PWideChar(LTest);
      LOutPointer := ExecScript(LScript, LOutLength);
      LOutArr := ParseSoccerOutputToArray(LOutPointer, LOutLength);
      if LOutArr[0] = 'error' then
        Writeln(LOutArr[0] + ': ' + LOutArr[1])
      else
      begin
        Writeln('Selected with ' + LOutArr[0]);
        Write('Winners: ');
        for i := 1 to Length(LOutArr) - 1 do
          Write(LOutArr[i] + ' ');
        Writeln();
      end;
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeAndNil(LStringList);
    FreeSoccerPtr(LOutPointer, LOutLength);
  end;
end.
