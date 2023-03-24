{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     ULists.pas
  @COMMENTS                 Implements a list class that can store and
                            manipulate lists of ojects. The list can optionally
                            own the objects added to it (and hence free the
                            objects when the list is freed).
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
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
 * The Original Code is ULists.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit ULists;


interface


uses
  // Delphi
  Classes;


type

  {
  TBasicObjList:
    This class provides a basic class that can store and manipulate lists of
    ojects. The list can optionally own the objects added to it (and hence free
    the objects when the list is freed). The default behaviour is not to own the
    objects and for the user to be responsible for managing their lifetime.
  }
  TBasicObjList = class(TObject)
  private // properties
    fItems: TList;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  protected // properties
    function GetCount: Integer;
    function GetItem(Idx: Integer): TObject;
  private
    fOwnObjects: Boolean;
      {Flag true if list owns objects and frees them on deletion, clearing
      and destruction}
  public
    constructor Create(const OwnObjects: Boolean = False);
      {Class constructor: creates owned list. If OwnObjects is true then any
      objects added to the list are "owned" by the list and will be freed when
      deleted, either explicitly or when the list is cleared or freed}
    destructor Destroy; override;
      {Class destructor: frees owned list and frees objects if owned}
    function Add(const Obj: TObject): Integer;
      {Adds the given object to the list}
    procedure Delete(Idx: Integer);
      {Delete the object reference from the list and frees object if owned}
    function IndexOf(const Obj: TObject): Integer;
      {Return index of given object or -1 if object not found}
    function Last: TObject;
      {Return last object in list. If list is empty, nil is returned}
    procedure Clear;
      {Clears the list, freeing objects if owned}
    function IsEmpty: Boolean;
      {Returns true if the list is empty or false if it has entries}
    procedure Expand;
      {Create more space for adding new items to the list. Expand does nothing
      if the list is not already filled to Capacity}
    property Items[Idx: Integer]: TObject read GetItem; default;
      {List of objects}
    property Count: Integer read GetCount;
      {Number of objects in list}
    property Capacity: Integer read GetCapacity write SetCapacity;
      {Specifies the allocated size of the array of pointers maintained by the
      list}
  end;


implementation


{ TBasicObjList }

function TBasicObjList.Add(const Obj: TObject): Integer;
  {Adds the given object to the list}
begin
  Result := fItems.Add(Obj);
end;

procedure TBasicObjList.Clear;
  {Clears the list, freeing objects if owned}
var
  Idx: Integer; // loops thru items in list
begin
  if fOwnObjects then
  begin
    // We own objects, so free them
    for Idx := Pred(Count) downto 0 do
      Delete(Idx);
  end
  else
    // We don't own objects so just clear list
    fItems.Clear;
end;

constructor TBasicObjList.Create(const OwnObjects: Boolean);
  {Class constructor: creates owned list. If OwnObjects is true then any objects
  added to the list are "owned" by the list and will be freed when deleted,
  either explicitly or when the list is cleared or freed}
begin
  inherited Create;
  fOwnObjects := OwnObjects;
  fItems := TList.Create;
end;

procedure TBasicObjList.Delete(Idx: Integer);
  {Delete the object reference from the list and frees object if owned}
var
  Obj: TObject; // object to be deleted (and possibly freed)
begin
  Obj := GetItem(Idx);
  fItems.Delete(Idx);
  if fOwnObjects then
    // We own object, so free it
    Obj.Free;
end;

destructor TBasicObjList.Destroy;
  {Class destructor: frees owned list and frees objects if owned}
begin
  Clear;
  fItems.Free;
  inherited;
end;

procedure TBasicObjList.Expand;
  {Create more space for adding new items to the list. Expand does nothing if
  the list is not already filled to Capacity}
begin
  fItems.Expand;
end;

function TBasicObjList.GetCapacity: Integer;
  {Read access method for Capacity method}
begin
  Result := fItems.Capacity;
end;

function TBasicObjList.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fItems.Count;
end;

function TBasicObjList.GetItem(Idx: Integer): TObject;
  {Read access methid for Items property}
begin
  Result := TObject(fItems[Idx]);
end;

function TBasicObjList.IndexOf(const Obj: TObject): Integer;
  {Return index of given object or -1 if object not found}
begin
  Result := fItems.IndexOf(Obj);
end;

function TBasicObjList.IsEmpty: Boolean;
  {Returns true if the list is empty or false if it has entries}
begin
  Result := Count = 0;
end;

function TBasicObjList.Last: TObject;
  {Return last object in list}
begin
  if Count > 0 then
    Result := TObject(fItems.Last)
  else
    Result := nil;
end;

procedure TBasicObjList.SetCapacity(const Value: Integer);
  {Write access method for Capacity property}
begin
  fItems.Capacity := Value;
end;

end.
