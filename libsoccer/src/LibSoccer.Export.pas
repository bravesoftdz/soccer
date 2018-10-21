unit LIbSoccer.Export;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Soccer.Main;

function ExecScript(AScript: PChar; var OutLength: Int32): PPChar; stdcall;

procedure FreeSoccerPtr(var APtr: PPChar; ALength: Int32); stdcall;

implementation

{$POINTERMATH ON}

function ExecScript(AScript: PChar; var OutLength: Int32): PPChar;
var
  LSoccer: TSoccer;
  LStrList: TList<string>;
  i: integer;
  LOutString: string;
begin
  LSoccer := TSoccer.Create;
  LOutString := '';
  try
    try
      LStrList := LSoccer.ExecScript(AScript);
      GetMem(Result, SizeOf(PWideChar) * LStrList.Count);
      for i := 0 to LStrList.Count - 1 do
      begin
        (Result + i)^ := StrNew(PChar(LStrList[i]));
      end;
      OutLength := LStrList.Count;
    except
      on E: Exception do
      begin
        GetMem(Result, SizeOf(PWideChar) * 2);
        Result^ := PChar('error');
        (Result + 1)^ := StrNew(PChar(E.ClassName + ' :' + E.Message));
        OutLength := 2;
      end;
    end;
  finally
    FreeAndNil(LSoccer);
  end;
end;

procedure FreeSoccerPtr(var APtr: PPChar; ALength: Int32);
var
  i: integer;
begin
  for i := 0 to ALength - 1 do
  begin
    StrDispose((APtr + i)^);
  end;
  Dispose(APtr);
end;

{$POINTERMATH OFF}

end.
