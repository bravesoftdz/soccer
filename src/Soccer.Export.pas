unit Soccer.Export;

interface

uses
  System.SysUtils,
  System.Classes;

procedure ExecScript(AScript: PAnsiChar; var OutArray: TArray<PAnsiChar>;
  var OutLength: Int32);

implementation

procedure ExecScript(AScript: PAnsiChar; var OutArray: TArray<PAnsiChar>;
  var OutLength: Int32);
var
  s: AnsiChar;
  i: integer;
  LStr1, LStr2: AnsiString;
begin
  LStr1 := '';
  LStr2 := '';
  SetLength(OutArray, 2);
  OutLength := 2;
  for i := 0 to Length(AScript) - 1 do
  begin
    s := AScript[i];
    if Odd(i) then
      LStr1 := LStr1 + s
    else
      LStr2 := LStr2 + s;
  end;
  SetString(LStr1,OutArray[0],Length(LStr1));
end;

end.
