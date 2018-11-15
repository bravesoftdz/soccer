{***************************************************************************}
{                                                                           }
{           LeakCheck for Delphi                                            }
{                                                                           }
{           Copyright (c) 2015 Honza Rames                                  }
{                                                                           }
{           https://bitbucket.org/shadow_cs/delphi-leakcheck                }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit LeakCheck.Cycle;

{$I LeakCheck.inc}

interface

uses
  LeakCheck,
  SysUtils,
  TypInfo,
  Classes,
  Generics.Defaults,
  Generics.Collections,
  LeakCheck.Collections,
  Rtti;

{$IF CompilerVersion >= 25} // >= XE4
  {$LEGACYIFEND ON}
{$IFEND}
{$IF CompilerVersion >= 24} // >= XE3
  {$DEFINE XE3_UP}
{$IFEND}
{$SCOPEDENUMS ON}

type
{$IFNDEF XE3_UP}
  TSymbolName = ShortString;
{$ENDIF}
  PSymbolName = ^TSymbolName;

  /// <summary>
  ///   Specifies the output format of <c>TCycle.ToString</c>.
  /// </summary>
  TCycleFormat = (
    /// <summary>
    ///   Generate Graphviz compatible format
    /// </summary>
    Graphviz,
    /// <summary>
    ///   Append addresses of to the output (useful to distinguish different
    ///   instances with the same type name, recommended if Graphviz is
    ///   enabled).
    /// </summary>
    WithAddress,
    /// <summary>
    ///   Append field name if available (extended RTTI use must be enabled) to
    ///   record and class fields.
    /// </summary>
    WithField,
    /// <summary>
    ///   Find root instances of the graph.
    /// </summary>
    FindRoots);

  TCycle = record
  public type
    TItem = record
      TypeInfo: PTypeInfo;
      Address: Pointer;
      FieldName: PSymbolName;
    end;

    /// <seealso cref="">
    ///   <see cref="LeakCheck.Cycle|TCycleFormat" />
    /// </seealso>
    TCycleFormats = set of TCycleFormat;
  private
    FData: TArray<TItem>;
    FIsCycle: Boolean;
    function GetLength: Integer; inline;
    function GetItem(Index: Integer): TItem; inline;
    class function ItemToStr(const Item: TCycle.TItem;
      Format: TCycleFormats): string; inline; static;
  public
    /// <summary>
    ///   Converts cycle to textual representation. See <see cref="LeakCheck.Cycle|TCycleFormat" />
    ///   .
    /// </summary>
    function ToString(Format: TCycleFormats = []; const LineBreak: string = sLineBreak): string;

    property Items[Index: Integer]: TItem read GetItem; default;
    property Length: Integer read GetLength;
    property IsCycle: Boolean read FIsCycle;
  end;
  TCycles = TArray<TCycle>;

  TCyclesFormatter = record
  private type
    TNode = record
      /// <summary>
      ///   Seen node has True value if it has any incoming edges or False
      ///   otherwise (in this case the edge is a sub-graph root).
      /// </summary>
      HasIncomming: Boolean;
      TypeInfo: PTypeInfo;
    end;
  private
    FData: string;
    FFormat: TCycle.TCycleFormats;
    FLineBreak: string;
    /// <summary>
    ///   Contains all seen nodes.
    /// </summary>
    FSeenNodes: IDictionary<Pointer, TNode>;
  public const
    CompleteGraph = [TCycleFormat.Graphviz, TCycleFormat.WithField,
      TCycleFormat.WithAddress, TCycleFormat.FindRoots];
  public
    constructor Create(Format: TCycle.TCycleFormats);
    procedure Append(const Cycles: TCycles);
    function ToString: string; inline;
    function GetRoots: TArray<TCycle.TItem>;

    class function CyclesToStr(const Cycles: TCycles;
      Format: TCycle.TCycleFormats = []): string; static;
  end;

  TScanFlag = (
    /// <summary>
    ///   <see cref="LeakCheck.Cycle|TScanner.UseExtendedRtti" />
    /// </summary>
    UseExtendedRtti);
  TScanFlags = set of TScanFlag;

  TScanner = class;
  TScannerClass = class of TScanner;

  TScanner = class
  strict protected type
    TSeenInstancesSet = TDictionary<Pointer, Boolean>;
    TCurrentPathStack = TStack<TCycle.TItem>;
    {$INCLUDE LeakCheck.Types.inc}
  public type
    TIsInstanceIgnored = function(const Instance: TObject; ClassType: TClass): Boolean;
  strict private
    FOnInstanceIgnored: TIsInstanceIgnored;
  strict protected
    // Binary scanner
    class procedure PeekData(var P: PByte; var Data; Len: Integer); inline;
    class procedure ReadData(var P: PByte; var Data; Len: Integer); inline;
    class function ReadI16(var P: PByte): Smallint; inline;
    class function ReadI32(var P: PByte): Integer; inline;
    class function ReadI8(var P: PByte): Shortint; inline;
    class function ReadPointer(var P: PByte): Pointer; inline;
    class function ReadU16(var P: PByte): Word; inline;
    class function ReadU32(var P: PByte): Cardinal; inline;
    class function ReadU8(var P: PByte): Byte; inline;
  strict protected
    FCurrentPath: TCurrentPathStack;
    FInstance: Pointer;
    FResult: TCycles;
    FSeenInstances: TSeenInstancesSet;
    FUseExtendedRtti: Boolean;

    procedure AddResult(const AResult: TCycle);
    function IsInstanceIgnored(const Instance: TObject): Boolean; inline;
    procedure ScanArray(P: Pointer; TypeInfo: PTypeInfo; ElemCount: NativeUInt;
      FieldName: PSymbolName);
    procedure ScanClass(const Instance: TObject); virtual;
    procedure ScanClassInternal(const Instance: TObject); virtual;
    procedure ScanDynArray(var A: Pointer; TypeInfo: PTypeInfo);
    procedure ScanInterface(const Instance: IInterface);
    procedure ScanRecord(P: Pointer; TypeInfo: PTypeInfo;
      AUseExtendedRtti: Boolean);
    procedure ScanTCollection(const Collection: TCollection);
    procedure ScanTValue(const Value: PValue);
    procedure TypeEnd; inline;
    procedure TypeStart(Address: Pointer; TypeInfo: PTypeInfo;
      FieldName: PSymbolName); virtual;

    procedure CycleFound;
  protected
    function Scan(const AInstance: TObject): TCycles; overload;
    class function Scan(const Instance: TObject; ScannerClass: TScannerClass;
      Flags: TScanFlags;
      InstanceIgnoreProc: TIsInstanceIgnored = nil): TCycles; overload; static;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Use extended RTTI if enabled for given class type (or fallback to
    ///   classic mechanism, ie. closures), this will allow detection of pure
    ///   object references even on non-ARC.
    /// </summary>
    property UseExtendedRtti: Boolean read FUseExtendedRtti write FUseExtendedRtti;

    /// <summary>
    ///   If assigned all instances are passed to this callback, if it returns <c>
    ///   True</c> then the instance is skipped.
    /// </summary>
    property OnInstanceIgnored: TIsInstanceIgnored read FOnInstanceIgnored write FOnInstanceIgnored;
  end;

  TCycleScanner = class(TScanner)
  strict protected
    procedure ScanClass(const Instance: TObject); override;
  public
    function Scan(const AInstance: TObject): TCycles; inline;
  end;

  TGraphScanner = class(TScanner)
  strict protected
    procedure TypeStart(AAddress: Pointer; ATypeInfo: PTypeInfo;
      AFieldName: PSymbolName); override;
    procedure ScanClass(const Instance: TObject); override;
  public
    function Scan(const AInstance: TObject): TCycles; inline;
  end;

/// <summary>
///   Scans for reference cycles in managed fields. It can ONLY scan inside
///   managed fields so it can scan for interface cycles on any platform but
///   can only find object cycles on NextGen generated code. On non-NextGen it
///   cannot find cycles produced by referencing interface from owned object.
///   Main goal of this function is to detect cycles on NextGen in places where
///   you might have forgot to put <c>Weak</c> attribute.
/// </summary>
function ScanForCycles(const Instance: TObject; Flags: TScanFlags = [];
  InstanceIgnoreProc: TScanner.TIsInstanceIgnored = nil): TCycles;

/// <summary>
///   Scans object structure and generates an object graph (instance
///   relations).
/// </summary>
function ScanGraph(const Entrypoint: TObject; Flags: TScanFlags = [];
  InstanceIgnoreProc: TScanner.TIsInstanceIgnored = nil): TCycles;

implementation

{$I LeakCheck.Configuration.inc}

function ScanForCycles(const Instance: TObject; Flags: TScanFlags = [];
  InstanceIgnoreProc: TScanner.TIsInstanceIgnored = nil): TCycles;
begin
  Result := TScanner.Scan(Instance, TCycleScanner, Flags, InstanceIgnoreProc);
end;

function ScanGraph(const Entrypoint: TObject; Flags: TScanFlags = [];
  InstanceIgnoreProc: TScanner.TIsInstanceIgnored = nil): TCycles;
begin
  Result := TScanner.Scan(Entrypoint, TGraphScanner, Flags, InstanceIgnoreProc);
end;

{$REGION 'TScanner'}

procedure TScanner.AddResult(const AResult: TCycle);
var
  Len: Integer;
begin
  Len := Length(FResult);
  SetLength(FResult, Len + 1);
  FResult[Len] := AResult;
end;

constructor TScanner.Create;
begin
  inherited Create;
  FCurrentPath := TCurrentPathStack.Create;
  FSeenInstances := TSeenInstancesSet.Create;
end;

procedure TScanner.CycleFound;
var
  LResult: TCycle;
begin
  LResult.FData := FCurrentPath.ToArray;
  LResult.FIsCycle := True;
  AddResult(LResult);
end;

destructor TScanner.Destroy;
begin
  FSeenInstances.Free;
  FCurrentPath.Free;
  inherited;
end;

function TScanner.IsInstanceIgnored(const Instance: TObject): Boolean;
begin
  if Assigned(OnInstanceIgnored) then
    Result := OnInstanceIgnored(Instance, Instance.ClassType)
  else
    Result := False;
end;

class procedure TScanner.PeekData(var P: PByte; var Data; Len: Integer);
begin
  Move(P^, Data, Len);
end;

class procedure TScanner.ReadData(var P: PByte; var Data; Len: Integer);
begin
  PeekData(P, Data, Len);
  Inc(P, Len);
end;

class function TScanner.ReadI16(var P: PByte): Smallint;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadI32(var P: PByte): Integer;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadI8(var P: PByte): Shortint;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadPointer(var P: PByte): Pointer;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadU16(var P: PByte): Word;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadU32(var P: PByte): Cardinal;
begin
  ReadData(P, Result, SizeOf(Result));
end;

class function TScanner.ReadU8(var P: PByte): Byte;
begin
  ReadData(P, Result, SizeOf(Result));
end;

function TScanner.Scan(const AInstance: TObject): TCycles;
begin
  FInstance := AInstance;
  try
    ScanClassInternal(FInstance);
    Result := FResult;
  finally
    FResult := Default(TCycles);
    FSeenInstances.Clear;
    // FCurrentPath.Clear;
    Assert(FCurrentPath.Count = 0);
  end;
end;

class function TScanner.Scan(const Instance: TObject;
  ScannerClass: TScannerClass; Flags: TScanFlags;
  InstanceIgnoreProc: TIsInstanceIgnored): TCycles;
var
  Scanner: TScanner;
begin
  Scanner := ScannerClass.Create;
  try
    Scanner.UseExtendedRtti := TScanFlag.UseExtendedRtti in Flags;
    Scanner.OnInstanceIgnored := InstanceIgnoreProc;
    Result := Scanner.Scan(Instance);
  finally
    Scanner.Free;
  end;
end;

procedure TScanner.ScanArray(P: Pointer; TypeInfo: PTypeInfo;
  ElemCount: NativeUInt; FieldName: PSymbolName);
var
  FT: PFieldTable;
begin
  TypeStart(P, TypeInfo, FieldName);
  if ElemCount > 0 then
  begin
    case TypeInfo^.Kind of
      // TODO: Variants
      tkClass:
        while ElemCount > 0 do
        begin
          ScanClass(TObject(P^));
          Inc(PByte(P), SizeOf(Pointer));
          Dec(ElemCount);
        end;
      tkInterface:
        while ElemCount > 0 do
        begin
          ScanInterface(IInterface(P^));
          Inc(PByte(P), SizeOf(Pointer));
          Dec(ElemCount);
        end;
      tkDynArray:
        while ElemCount > 0 do
        begin
          // See System._FinalizeArray for why we call it like that
          ScanDynArray(PPointer(P)^, typeInfo);
          Inc(PByte(P), SizeOf(Pointer));
          Dec(ElemCount);
        end;
      tkArray:
        begin
          FT := PFieldTable(PByte(typeInfo) + Byte(PTypeInfo(typeInfo).Name{$IFNDEF NEXTGEN}[0]{$ENDIF}));
          while ElemCount > 0 do
          begin
            ScanArray(P, FT.Fields[0].TypeInfo^, FT.Count, nil);
            Inc(PByte(P), FT.Size);
            Dec(ElemCount);
          end;
        end;
      tkRecord:
        begin
          FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name{$IFNDEF NEXTGEN}[0]{$ENDIF}));
          while ElemCount > 0 do
          begin
            if TypeInfo = System.TypeInfo(TValue) then
              ScanTValue(PValue(P))
            else
              ScanRecord(P, TypeInfo, UseExtendedRtti);
            Inc(PByte(P), FT.Size);
            Dec(ElemCount);
          end;
        end;
    end;
  end;
  TypeEnd;
end;

procedure TScanner.ScanClass(const Instance: TObject);
begin
  if not Assigned(Instance) then
    // NOP
  else if not FSeenInstances.ContainsKey(Instance) then
    ScanClassInternal(Instance);
end;

procedure TScanner.ScanClassInternal(const Instance: TObject);

  procedure ScanClassic(Inst: Pointer; ClassType: TClass);
  var
    InitTable: PTypeInfo;
  begin
    InitTable := PPointer(PByte(ClassType) + vmtInitTable)^;
    if Assigned(InitTable) then
    begin
      // Do not use extedned RTTI here since we already know there is none
      // available (ScanExtended returned false) or it is not enabled globally.
      // This may occur in cases where $RTTI directive is used.
      ScanRecord(Instance, InitTable, False);
    end;
  end;

  function ScanExtended(Inst: Pointer; ClassType: TClass): Boolean;
  var
    P: PByte;
    FieldTable: PVmtFieldTable;
    ClassTab: PVmtFieldClassTab;
    I: Integer;
    Total, Found: Integer;
    Count: Integer;
    LastFieldOffset: Integer;
    Classic: TArray<Cardinal>;
    Comparer: IComparer<Cardinal>;
  begin
    P := PPointer(PByte(ClassType) + vmtFieldTable)^;
    if not Assigned(P) then
      Exit(False);

    // Classic published fields
    FieldTable := Pointer(P);
    ClassTab := FieldTable^.ClassTab;
    Inc(P, SizeOf(FieldTable^));
    Total := FieldTable^.Count;
    LastFieldOffset := -1;
    for I := 0 to Total - 1 do
    begin
      with PVmtFieldEntry(P)^ do
      begin
        Assert(LastFieldOffset < Integer(FieldOffset)); // Assert we can use binary search
        ScanArray(Pointer(PByte(Inst) + NativeInt(FieldOffset)),
          ClassTab^.ClassRef[TypeIndex]^.ClassInfo, 1, @Name);
        Count := Length(Classic);
        SetLength(Classic, Count + 1);
        Classic[Count] := FieldOffset;
        LastFieldOffset := FieldOffset;
        // AttrData - skip the name
        P := PByte(@Name);
        Inc(P, P^ + 1);
      end;
      // No attr data
    end;

    //Extended fields (may or may not include some of the classic fields as well)
    I := ReadU16(P);
    Inc(Total, I);
    if I > 0 then
    begin
      Comparer := TComparer<Cardinal>.Default;
      for I := 0 to I - 1 do
      begin
        with PFieldExEntry(P)^ do
        begin
          if Assigned(TypeRef) and not TArray.BinarySearch<Cardinal>(
            Classic, Offset, Found, Comparer) then
          begin
            ScanArray(Pointer(PByte(Inst) + NativeInt(Offset)), TypeRef^, 1, @Name);
          end;
          // AttrData - skip the name
          P := PByte(@Name);
          Inc(P, P^ + 1);
        end;
        // LazyLoadAttributes
        Count := PWord(P)^; // Size in bytes including length itself (no attributes = 2)
        Inc(P, Count);
      end;
    end;

    Result := Total > 0;
  end;

var
  LClassType: TClass;
begin
{$IF ScannerEnableObjectPointerSanitation <> TScannerSanitationType.Complex}
  if PNativeUInt(Instance)^ < -vmtSelfPtr then
    Exit;
  if Instance.ClassInfo = nil then
    Exit;
  LClassType := Instance.ClassType;
  if PPointer(PByte(LClassType) + vmtSelfPtr)^ <> LClassType then
    Exit;
{$IFEND}
{$IF ScannerEnableObjectPointerSanitation = TScannerSanitationType.FreedObject}
  if Instance.ClassParent = TLeakCheck.TFreedObject then
    Exit;
{$ELSEIF ScannerEnableObjectPointerSanitation = TScannerSanitationType.Complex}
  if TLeakCheck.GetObjectClass(Instance) = nil then
    Exit;
{$IFEND}

  // Do after the class is valdiated. All calls to ScanClassInternal should add
  // the instance to FSeenInstances.
  FSeenInstances.Add(Instance, True);

  if IsInstanceIgnored(Instance) then
    Exit;

  TypeStart(Instance, Instance.ClassInfo, nil);

  repeat
    if UseExtendedRtti then
    begin
      if not ScanExtended(Instance, LClassType) then
        ScanClassic(Instance, LClassType);
    end
    else
      ScanClassic(Instance, LClassType);

    LClassType := LClassType.ClassParent;
  until LClassType = nil;

  // Scanned as TList<TCollectionItem>.arrayofT after RTL changed it from simple TList
  if {$IFDEF XE3_UP}(not UseExtendedRtti) and {$ENDIF}(Instance is TCollection) then
    ScanTCollection(TCollection(Instance));

  TypeEnd;
end;

procedure TScanner.ScanDynArray(var A: Pointer; TypeInfo: PTypeInfo);
var
  P: Pointer;
  Rec: PDynArrayRec;
  TypeData: PTypeData;
begin
  // Do not push another type, we already did in previous call

  P := A;
  if P <> nil then
  begin
    Rec := PDynArrayRec(PByte(P) - SizeOf(TDynArrayRec));

    // If refcount is negative the array is released
    if (Rec^.RefCnt > 0) and (Rec^.Length <> 0) then
    begin
      // Fetch the type descriptor of the elements
      TypeData := GetTypeData(TypeInfo);
      if TypeData^.elType2 <> nil then
        ScanArray(P, TypeData^.elType2^, Rec^.Length, nil);
    end;
  end;
end;

procedure TScanner.ScanInterface(const Instance: IInterface);
var
  inst: Pointer;
begin
  // Do not push another type, we cannot be sure of the type information
  // Cast should return nil not raise an exception if interface is not class
  try
    inst := TObject(Instance);
  except
    // If there are dangling references that were previsouly released they may
    // not be accessible
    // TODO: We could ask the memory manager whether the Instance address is readable (ie. is allocated/leaks)
    on EAccessViolation do
      Exit;
    else raise;
  end;
  ScanClass(inst);
end;

procedure TScanner.ScanRecord(P: Pointer; TypeInfo: PTypeInfo;
  AUseExtendedRtti: Boolean);

  procedure ScanClassic(P: Pointer; TypeInfo: PTypeInfo);
  var
    I: Cardinal;
    FT: PFieldTable;
  begin
    // Do not push another type, ScanArray will do it later
    FT := PFieldTable(PByte(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name{$IFNDEF NEXTGEN}[0]{$ENDIF}));
    if FT.Count > 0 then
    begin
      for I := 0 to FT.Count - 1 do
      begin
  {$IFDEF WEAKREF}
        if FT.Fields[I].TypeInfo = nil then
          Exit; // Weakref separator
  {$ENDIF}
        ScanArray(Pointer(PByte(P) + NativeInt(FT.Fields[I].Offset)),
          FT.Fields[I].TypeInfo^, 1, nil);
      end;
    end;
  end;

  function ScanExtended(Inst: Pointer; TypeInfo: PTypeInfo): Boolean;
  var
    TypeData: PTypeData;
    Count: Integer;
    P: PByte;
    I: Integer;
  begin
    TypeData := GetTypeData(TypeInfo);
    P := @TypeData^.ManagedFldCount;
    Count := ReadI32(P); // Managed fields
    Inc(P, SizeOf(TManagedField) * Count);
    Count := ReadU8(P); // Ops
    Inc(P, SizeOf(Pointer) * Count);
    Count := ReadI32(P);
    if Count = 0 then
      Exit(False);
    for I := 0 to Count - 1 do
    begin
      with PRecordTypeField(P)^ do
      begin
        if Assigned(Field.TypeRef) then
        begin
          ScanArray(Pointer(PByte(Inst) + NativeInt(Field.FldOffset)),
            Field.TypeRef^, 1, @Name);
        end;
        // AttrData - skip the name
        P := PByte(@Name);
        Inc(P, P^ + 1);
      end;
      // LazyLoadAttributes
      Count := PWord(P)^; // Size in bytes including length itself (no attributes = 2)
      Inc(P, Count);
    end;
    Result := True;
  end;

begin
  if AUseExtendedRtti then
  begin
    if not ScanExtended(P, TypeInfo) then
      ScanClassic(P, TypeInfo);
  end
  else
    ScanClassic(P, TypeInfo);
end;

procedure TScanner.ScanTCollection(const Collection: TCollection);
var
  Item: TCollectionItem;
begin
  for Item in Collection do
    ScanClassInternal(Item);
end;

procedure TScanner.ScanTValue(const Value: PValue);
var
  ValueData: PValueData absolute Value;
begin
  // Do not push another type, ScanArray already did
  if (not Value^.IsEmpty) and Assigned(ValueData^.FValueData) then
  begin
    // Performance optimization, keep only supported types here to avoid adding
    // strings
    case Value^.Kind of
      // TODO: Variants
      tkClass,
      tkInterface,
      tkDynArray,
      tkArray,
      tkLString,
      tkUString,
      tkWString,
      tkRecord:
        // If TValue contains the instance directly it will duplicate it
        // but it is totally OK, otherwise some other type holding the instance
        // might get hidden. The type is the actual type TValue holds.
        ScanArray(Value^.GetReferenceToRawData, Value.TypeInfo, 1, nil);
    end;
  end;
end;

procedure TScanner.TypeEnd;
begin
  FCurrentPath.Pop;
end;

procedure TScanner.TypeStart(Address: Pointer; TypeInfo: PTypeInfo;
  FieldName: PSymbolName);
var
  Item: TCycle.TItem;
begin
  Item.Address := Address;
  Item.TypeInfo := TypeInfo;
  Item.FieldName := FieldName;
  FCurrentPath.Push(Item);
end;

{$ENDREGION}

{$REGION 'TCycleScanner'}

function TCycleScanner.Scan(const AInstance: TObject): TCycles;
begin
  Result := inherited;
end;

procedure TCycleScanner.ScanClass(const Instance: TObject);
begin
  if Instance = FInstance then
    CycleFound
  else inherited;
end;

{$ENDREGION}

{$REGION 'TGraphScanner'}

function TGraphScanner.Scan(const AInstance: TObject): TCycles;
begin
  Result := inherited;
end;

procedure TGraphScanner.ScanClass(const Instance: TObject);
begin
  if not Assigned(Instance) then
    // NOP
  else if not FSeenInstances.ContainsKey(Instance) then
    ScanClassInternal(Instance)
  else
  begin
    // Add to result but do NOT scan AGAIN
    TypeStart(Instance, Instance.ClassInfo, nil);
    TypeEnd;
  end;
  // Generate cycle AFTER scanning the class to allow the graph formatter to
  // override edge color.
  if Instance = FInstance then
    CycleFound;
end;

procedure TGraphScanner.TypeStart(AAddress: Pointer; ATypeInfo: PTypeInfo;
  AFieldName: PSymbolName);
var
  LResult: TCycle;
begin
  if (ATypeInfo^.Kind in [tkClass, tkInterface, tkRecord, tkArray, tkDynArray]) and
    (FCurrentPath.Count > 0) then
  begin
    SetLength(LResult.FData, 2);
    LResult.FData[0] := FCurrentPath.Peek;
    LResult.FIsCycle := False;
    with LResult.FData[1] do
    begin
      Address := AAddress;
      TypeInfo := ATypeInfo;
      FieldName := AFieldName;
    end;
    AddResult(LResult);
  end;
  inherited;
end;

{$ENDREGION}

{$REGION 'TCycle'}

function TCycle.GetItem(Index: Integer): TCycle.TItem;
begin
  Result := FData[Index];
end;

function TCycle.GetLength: Integer;
begin
  Result := System.Length(FData);
end;

class function TCycle.ItemToStr(const Item: TCycle.TItem;
  Format: TCycleFormats): string;
begin
{$IFDEF XE3_UP}
  Result := Item.TypeInfo^.NameFld.ToString;
{$ELSE}
  Result := string(Item.TypeInfo^.Name);
{$ENDIF}
  if TCycleFormat.WithAddress in Format then
    Result := Result + SysUtils.Format(' (%p)', [Item.Address]);

  if TCycleFormat.Graphviz in Format then
    Result := '"' + Result + '"';
end;

function TCycle.ToString(Format: TCycleFormats = [];
  const LineBreak: string = sLineBreak): string;

  function SymbolToStr(Name: PSymbolName): string; inline;
{$IFDEF NEXTGEN}
  var
    Dest: array[0..511] of Char;
{$ENDIF}
  begin
{$IFNDEF NEXTGEN}
    Result := UTF8ToString(Name^);
{$ELSE}
    if Name^ = 0 then
      Result := ''
    else
      SetString(Result, Dest, UTF8ToUnicode(Dest, System.Length(Dest),
        MarshaledAString(PByte(Name) + 1), Name^) - 1);
{$ENDIF}
  end;

  function EdgeToStr(const Item: TCycle.TItem; IsCycle: Boolean;
    Format: TCycleFormats): string; inline;
  begin
    if (TCycleFormat.WithField in Format) and Assigned(Item.FieldName) then
    begin
      if TCycleFormat.Graphviz in Format then
      begin
        Result := ' [label="' + SymbolToStr(Item.FieldName) + '"';
        if IsCycle then
          Result := Result + ', color=red';
        Result := Result + ']';
      end
      else
        Result := ' [' + SymbolToStr(Item.FieldName) + ']';
    end
    else
      Result := '';
  end;

const
  Separator = ' -> ';
var
  Item: TCycle.TItem;
  OneByOne: Boolean;
  Last, s: string;
begin
  Result := '';
  if Length = 0 then
    Exit;

  // Only one edge per line?
  OneByOne := [TCycleFormat.Graphviz, TCycleFormat.WithField] <= Format;

  Last := '';
  for Item in FData do
  begin
    if Byte(Item.TypeInfo^.Name{$IFNDEF NEXTGEN}[0]{$ENDIF}) > 0 then
    begin
      if OneByOne then
      begin
        if Last = '' then
          Last := ItemToStr(Item, Format)
        else
        begin
          if Result <> '' then
            Result := Result + ';' + LineBreak;
          s := ItemToStr(Item, Format);
          Result := Result + Last + Separator + s + EdgeToStr(Item, IsCycle, Format);
          Last := s;
        end;
      end
      else
      begin
        if Result <> '' then
          Result := Result + Separator;
        Result := Result + ItemToStr(Item, Format) + EdgeToStr(Item, IsCycle, Format);
      end;
    end;
  end;
  // Complete the circle (if cycle)
  if IsCycle then
  begin
    if OneByOne then
    begin
      if Last <> '' then
      begin
        if Result <> '' then
          Result := Result + ';' + LineBreak;
        Result := Result + Last + Separator + ItemToStr(FData[0], Format);
      end;
    end
    else
      Result := Result + Separator + ItemToStr(FData[0], Format);
    if TCycleFormat.Graphviz in Format then
      Result := Result + '[color=red]';
  end;
  if TCycleFormat.Graphviz in Format then
    Result := Result + ';';
end;

{$ENDREGION}

{$REGION 'TCyclesFormatter'}

procedure TCyclesFormatter.Append(const Cycles: TCycles);

  procedure AddSeenNode(const Item: TCycle.TItem; Incoming: Boolean);
  var
    Node: TNode;
  begin
    // We're interrested if incoming nodes were found, so this is basically or
    // operation.
    if Incoming then
    begin
      Node.TypeInfo := Item.TypeInfo;
      Node.HasIncomming := True;
      FSeenNodes.AddOrSetValue(Item.Address, Node)
    end
    else
    begin
      // If there is previous node do nothing (it has HasIncomming already
      // either True in which case we don't want to overwrite it and if False
      // there is also nothing to do we would set the same value).
      if FSeenNodes.TryGetValue(Item.Address, Node) then
        // NOP
      else
      begin
        // TryGetValue will reset the Node if not found
        Node.TypeInfo := Item.TypeInfo;
        Node.HasIncomming := False;
        FSeenNodes.AddOrSetValue(Item.Address, Node);
      end;
    end;
  end;

var
  Cycle: TCycle;
  Item: TCycle.TItem;
  Incoming: Boolean;
begin
  for Cycle in Cycles do
  begin
    FData := FData + FLineBreak + Cycle.ToString(FFormat, FLineBreak);
    if TCycleFormat.FindRoots in FFormat then
    begin
      // First node may not have any Incoming edge
      Incoming := False;
      for Item in Cycle.FData do
      begin
        AddSeenNode(Item, Incoming);
        // All following nodes have at leas one Incoming edge
        Incoming := true;
      end;
    end;
  end;
end;

constructor TCyclesFormatter.Create(Format: TCycle.TCycleFormats);
begin
  FFormat := Format;
  // strict maintains only one edge if multiple same edges are found
  FLineBreak := sLineBreak;
  if TCycleFormat.Graphviz in FFormat then
  begin
    FData := 'strict digraph L {';
    FLineBreak := FLineBreak + '  ';
  end;
  if TCycleFormat.FindRoots in Format then
    FSeenNodes := TDictionary<Pointer, TNode>.Create;
end;

class function TCyclesFormatter.CyclesToStr(const Cycles: TCycles;
  Format: TCycle.TCycleFormats): string;
var
  Formatter: TCyclesFormatter;
begin
  Formatter := TCyclesFormatter.Create(Format);
  Formatter.Append(Cycles);
  Result := Formatter.ToString;
end;

function TCyclesFormatter.GetRoots: TArray<TCycle.TItem>;
var
  Pair: TPair<Pointer, TNode>;
  i: Integer;
begin
  Result := nil;
  i := 0;
  for Pair in FSeenNodes do
    if not Pair.Value.HasIncomming then // No incoming edge
  begin
    SetLength(Result, i + 1);
    with Result[i] do
    begin
      Address := Pair.Key;
      TypeInfo := Pair.Value.TypeInfo;
      FieldName := nil;
    end;
    Inc(i);
  end;
end;

function TCyclesFormatter.ToString: string;
var
  Root: TCycle.TItem;
begin
  Result := FData;
  if TCycleFormat.Graphviz in FFormat then
  begin
    if TCycleFormat.FindRoots in FFormat then
    begin
      for Root in GetRoots do
      begin
        Result := Result + sLineBreak + TCycle.ItemToStr(Root, FFormat) +
          ' [color=red];';
      end;
    end;
    Result := Result + sLineBreak + '}';
  end;
end;

{$ENDREGION}

end.
