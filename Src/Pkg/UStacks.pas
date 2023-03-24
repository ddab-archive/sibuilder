{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UStacks.pas
  @COMMENTS                 Unit the provides several stack implementations for
                            various basic data types.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Moved error message from string literal to resource
                            string.
    )
  )
}


{
 * ***** BEGIN LICENSE BLOCK *****
 * 
 * Version: MPL 1.1
 * 
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * The Original Code is UStacks.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UStacks;


interface


uses
  // Delphi
  SysUtils, Classes;

     
type

  {
  EStackError:
    Class of exception raised when stack objects report an error.
  }
  EStackError = class(Exception);


  {
  TStack:
    Implements a stack of pointers.
  }
  TStack = class(TObject)
  private // properties
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetCount: Integer;
  private
    fStack: TList;
      {List object used to store stack items}
  public
    constructor Create;
      {Class constructor: creates owned list object}
    destructor Destroy; override;
      {Class destructor: frees owned object}
    procedure Expand;
      {Create more space for adding new items to the stack. Expand does nothing
      if the list is not already filled to Capacity}
    procedure Clear;
      {Clears the stack}
    function IsEmpty: Boolean;
      {Returns true if the stack is empty}
    function Top: Pointer;
      {Returns pointer at top of stack without removing it}
    function Pop: Pointer;
      {Removes item at top of stack and returns the pointer}
    procedure Push(const P: Pointer);
      {Pushes given pointer onto stack}
    property Capacity: Integer read GetCapacity write SetCapacity;
      {Specifies the allocated size of the array of pointers maintained by the
      stack}
    property Count: Integer read GetCount;
      {Number of items in the stack}
  end;


  {
  TIntegerStack:
    Implements a stack of integers.
  }
  TIntegerStack = class(TStack)
  public
    function Top: Integer;
      {Returns integer at top of stack without removing it}
    function Pop: Integer;
      {Removes integer at top of stack and returns it}
    procedure Push(const I: Integer);
      {Pushes given integer onto stack}
  end;


  {
  TObjectStack:
    Implements a stack of objects.
  }
  TObjectStack = class(TStack)
  public
    function Top: TObject;
      {Returns object at top of stack without removing it}
    function Pop: TObject;
      {Removes object at top of stack and returns it}
    procedure Push(const Obj: TObject);
      {Pushes given object onto stack}
  end;


  {
  TStringStack:
    Implements a stack of strings and associated objects.
  }
  TStringStack = class(TObject)
  private // properties
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetCapacity(const Value: Integer);
  private
    fStack: TStringList;
      {String list object used to store strings and objects on stack}
  public
    constructor Create;
      {Class constructor: creates owned object}
    destructor Destroy; override;
      {Class destructor: frees owned object}
    procedure Clear;
      {Clears the stack}
    function IsEmpty: Boolean;
      {Returns true if the stack is empty}
    function Top: string; overload;
      {Returns string at top of stack without removing it}
    function Top(out Obj: TObject): string; overload;
      {Returns string at top of stack and passes associated object back thru the
      Obj parameter}
    function Pop: string; overload;
      {Removes string at top of stack and returns it. Any associated object is
      lost}
    function Pop(out Obj: TObject): string; overload;
      {Removes string at top of stack and returns it. Also passes associated
      object back in Obj parameter}
    procedure Push(const S: string); overload;
      {Pushes string onto stack}
    procedure Push(const S: string; const Obj: TObject); overload;
      {Pushes string and associated object onto stack}
    property Capacity: Integer read GetCapacity write SetCapacity;
      {Specifies the allocated size of the array of pointers maintained by the
      stack}
    property Count: Integer read GetCount;
      {Number of items in the stack}
  end;


implementation


resourcestring
  // Error message
  sEmptyStack = 'Stack is empty';


{ TStack }

procedure TStack.Clear;
  {Clears the stack}
begin
  fStack.Clear;
end;

constructor TStack.Create;
  {Class constructor: creates owned list object}
begin
  inherited;
  fStack := TList.Create;
end;

destructor TStack.Destroy;
  {Class destructor: frees owned object}
begin
  fStack.Free;
  inherited;
end;

procedure TStack.Expand;
  {Create more space for adding new items to the stack. Expand does nothing if
  the list is not already filled to Capacity}
begin
  fStack.Expand;
end;

function TStack.GetCapacity: Integer;
  {Read access method for Capacity property}
begin
  Result := fStack.Capacity;
end;

function TStack.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fStack.Count;
end;

function TStack.IsEmpty: Boolean;
  {Returns true if the stack is empty}
begin
  Result := fStack.Count = 0;
end;

function TStack.Pop: Pointer;
  {Removes item at top of stack and returns the pointer}
begin
  Result := Top;
  fStack.Delete(Pred(fStack.Count));
end;

procedure TStack.Push(const P: Pointer);
  {Pushes given pointer onto stack}
begin
  fStack.Add(P);
end;

procedure TStack.SetCapacity(const Value: Integer);
  {Write access method for Capacity property}
begin
  fStack.Capacity := Value;
end;

function TStack.Top: Pointer;
  {Returns pointer at top of stack without removing it}
begin
  if IsEmpty then
    raise EStackError.Create(sEmptyStack);
  Result := fStack.Last;
end;


{ TIntegerStack }

function TIntegerStack.Pop: Integer;
  {Removes integer at top of stack and returns it}
begin
  Result := Integer(inherited Pop);
end;

procedure TIntegerStack.Push(const I: Integer);
  {Pushes given integer onto stack}
begin
  inherited Push(Pointer(I));
end;

function TIntegerStack.Top: Integer;
  {Returns integer at top of stack without removing it}
begin
  Result := Integer(inherited Top);
end;


{ TObjectStack }

function TObjectStack.Pop: TObject;
  {Removes object at top of stack and returns it}
begin
  Result := TObject(inherited Pop);
end;

procedure TObjectStack.Push(const Obj: TObject);
  {Pushes given object onto stack}
begin
  inherited Push(Obj);
end;

function TObjectStack.Top: TObject;
  {Returns object at top of stack without removing it}
begin
  Result := TObject(inherited Top);
end;


{ TStringStack }

procedure TStringStack.Clear;
  {Clears the stack}
begin
  fStack.Clear;
end;

constructor TStringStack.Create;
  {Class constructor: creates owned object}
begin
  inherited;
  fStack := TStringList.Create;
end;

destructor TStringStack.Destroy;
  {Class destructor: frees owned object}
begin
  fStack.Free;
  inherited;
end;

function TStringStack.GetCapacity: Integer;
  {Read access method for Capacity property}
begin
  Result := fStack.Capacity;
end;

function TStringStack.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fStack.Count;
end;

function TStringStack.IsEmpty: Boolean;
  {Returns true if the stack is empty}
begin
  Result := Count = 0;
end;

function TStringStack.Pop: string;
  {Removes string at top of stack and returns it. Any associated object is lost}
var
  Dummy: TObject; // stores unused object associated with string
begin
  Result := Pop(Dummy);
end;

function TStringStack.Pop(out Obj: TObject): string;
  {Removes string at top of stack and returns it. Also passes associated object
  back in Obj parameter}
begin
  Result := Top(Obj);
  fStack.Delete(Pred(Count));
end;

procedure TStringStack.Push(const S: string);
  {Pushes string onto stack}
begin
  fStack.Add(S);
end;

procedure TStringStack.Push(const S: string; const Obj: TObject);
  {Pushes string and associated object onto stack}
begin
  fStack.AddObject(S, Obj);
end;

procedure TStringStack.SetCapacity(const Value: Integer);
  {Write access method for Capacity property}
begin
  fStack.Capacity := Value;
end;

function TStringStack.Top: string;
  {Returns string at top of stack without removing it}
begin
  if IsEmpty then
    raise EStackError.Create(sEmptyStack);
  Result := fStack[Pred(Count)];
end;

function TStringStack.Top(out Obj: TObject): string;
  {Returns string at top of stack and passes associated object back thru the Obj
  parameter}
begin
  Result := Top;
  Obj := fStack.Objects[Pred(Count)];
end;


end.
