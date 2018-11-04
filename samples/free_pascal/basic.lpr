program basic;

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
  LScript: PWideChar;
  LOutPointer: PPWideChar;
  LOutLength: Int32;
  LOutArr: TStringArray;
  i: integer;

begin
  LOutLength := 0;
  try
    try
      LScript := PWideChar('START[voting] IMPORT[plurality] VOTE(a->b->c) DECIDE!');
      LOutPointer := ExecScript(LScript, LOutLength);
      LOutArr := ParseSoccerOutputToArray(LOutPointer, LOutLength);
      for i := 0 to Length(LOutArr) - 1 do
        Writeln(LOutArr[i]);
    except
      on E: Exception do
        Writeln(E.Message);
    end;
  finally
    FreeSoccerPtr(LOutPointer, LOutLength);
    Readln;
  end;
end.

