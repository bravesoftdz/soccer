program soccer;

{$mode delphi}

uses
  SysUtils,
  Classes;

  function ExecScript(AScript: PWideChar; var OutLength: Int32): PPWideChar;
  stdcall; external 'libsoccer.dll';

  procedure FreeSoccerPtr(var APtr: PPWideChar; ALength: Int32);
  stdcall; external 'libsoccer.dll';

  function ParseSoccerOutputToArray(ASoccerOut: PPWideChar;
    AWinnersLength: integer): TStringArray;
  var
    i: integer;
    LWinner: PWideChar;
    LWinnerStr: ansistring;
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
  LReadLnFlag: boolean;

begin
  if ParamCount > 0 then
  begin
    LFileName := ParamStr(1);
    if ParamCount > 1 then
      if ParamStr(1) = '-r' then
        LReadLnFlag := True;
  end
  else
  begin
    WriteLn('No file specified');
    exit;
  end;
  LStringList := TStringList.Create;
  LOutLength := 0;
  try
    try
      Writeln('Load from file');
      LStringList.LoadFromFile(LFileName);
      LTest := UnicodeString(Copy(LStringList.Text, 1, Length(LStringList.Text)));
      LScript := PWideChar(LTest);
      Writeln('Calling core');
      LOutPointer := ExecScript(LScript, LOutLength);
      Writeln('Parse output');
      Writeln('Information count: ', LOutLength);
      LOutArr := ParseSoccerOutputToArray(LOutPointer, LOutLength);
      if Length(LOutArr) > 0 then
        if LOutArr[0] = 'error' then
          if Length(LOutArr) > 1 then
            Writeln(LOutArr[0] + ': ' + LOutArr[1])
          else
            Writeln('error: no information about the failure')
        else
        begin
          Writeln('Selected with ' + LOutArr[0]);
          Write('Winners: ');
          for i := 1 to Length(LOutArr) - 1 do
            Write(LOutArr[i] + ' ');
          Writeln();
        end
      else
        Writeln('Unknown error: no output from core');
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeAndNil(LStringList);
    FreeSoccerPtr(LOutPointer, LOutLength);
  end;
  if LReadLnFlag then
    ReadLn;
end.
