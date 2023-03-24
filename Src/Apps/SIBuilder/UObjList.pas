{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UObjList.pas
  @COMMENTS                 This unit includes two related classes - TObjList
                            and TObjListItem. The latter manages an list of
                            classes derived from the former. TObjListItem
                            objects can belong to at most one list.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 04/09/2000
      @COMMENTS             Added new TObjList.Exchange method and removed
                            redundant TObjList.IsInList method.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literals with resource strings.
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
 * The Original Code is UObjList.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UObjList;


interface


uses
  // Delphi
  SysUtils, Classes;


type
  {
  EObjList:
    Class of exception raised by TObjList.
  }
  EObjList = class(Exception);

  TObjListItem = class;

  TObjListItemClass = class of TObjListItem;

  {
  TObjListChangeEvent:
    Type of method used to provide event handler for TObjList's OnChange event.
  }
  TObjListChangeEvent = procedure(var AllowChange: Boolean) of object;

  {
  TObjListFinder:
    Type of function that tests the given object and returns true if it matches
    a required criterion, and false if not. These functions are used by
    TObjList.Find to find an object that has required properties.
  }
  TObjListFinder = function (const AnObject: TObjListItem; const Ptr: Pointer):
    Boolean of object;

  {
  TObjList:
    Class implements and manages list of objects. Provides methods to insert,
    remove, free and search for objects. Stores a reference to any object that
    owns the. Objects inserted in the list must have TObjListItem as a base
    class. An event is triggered each time the list changes.
  }
  TObjList = class(TObject)
  private // properties
    fOwner: TObject;
    fItemClass: TObjListItemClass;
    fOnChange: TObjListChangeEvent;
    function GetItem(I: Integer): TObjListItem;
    function GetCount: Integer;
  private
    fItems: TList;
      {Maintains list of objects}
    procedure CheckOKToChange;
      {Triggers OnChange event and returns if change is allowed, otherwise
      raises exception}
  protected
    function GetObject(AnIndex: Integer): TObjListItem;
      {Return a reference to the object at the given index in the list}
    property ItemClass: TObjListItemClass read fItemClass;
      {The class that list items belong to}
  public
    constructor Create(Owner: TObject); virtual;
      {Constructor - creates owned list object with items of class
      TObjectListItem}
    constructor CreateForClass(Owner: TObject; AClass: TObjListItemClass);
      virtual;
      {Constructor - creates owned list object that contains items of given
      class}
    destructor Destroy; override;
      {Destructor - deletes and frees all objects from list and frees owned
      TList object}
    function Add(AnObject: TObjListItem) : Integer;
      {Add the given object to end of list and return it's index in list. An
      exception is raised if the given object is already in a list. NOTE: this
      is the only method used to add something to the list and is where
      permission to add items is sought}
    procedure Delete(Index: Integer);
      {Remove the object at the given index from the list and free the object}
    procedure DeleteObj(AnObject: TObjListItem);
      {Remove the given object from the list and free the object. Raise EObjList
      exception if object not in list}
    procedure Remove(Index: Integer);
      {Remove the object at the given index from the list without freeing the
      object.
      NOTE: this method is called by all other methods that remove an
      object from list - this is where permission to remove is sought}
    procedure RemoveObj(AnObject: TObjListItem);
      {Remove the given object from the list without freeing the object. Raise
      EObjList exception if object not in list}
    procedure Clear;
      {Clear all objects from the list, freeing each object}
    procedure Exchange(Obj1, Obj2: TObjListItem);
      {Exchange position in list of the two given objects. Raise exception if
      either of the items do not exist}
    function IndexOf(AnObject: TObjListItem): Integer;
      {Return the index in the list of the given object, or -1 if not in list}
    function FindIndex(Finder: TObjListFinder; Ptr: Pointer): Integer;
      {Return the index of the first object in the list which has properties in
      accordance with Ptr that causes the Finder function to return True}
    function FindObject(Finder: TObjListFinder; Ptr: Pointer): TObjListItem;
      {Return the first object in the list which has properties in accordance
      with Ptr that causes the Finder function to return True}
    property Items[I: Integer]: TObjListItem read GetItem; default;
      {The items in the list}
    property Count: Integer read GetCount;
      {The number of objects in the list}
    property Owner: TObject read fOwner;
      {The object that owns this list}
    property OnChange: TObjListChangeEvent read fOnChange write fOnChange;
      {Event triggered whenever there's an attempt to change the list - allows
      user to prevent the change by toggling flag to false, triggering
      exception}
  end;

  {
  TObjListItem:
    Base class for objects that can be inserted in a TObjList object list.
    Instances of this class can be added to just one list and they store a
    reference to any list they a included in which is used to find other
    instances in the same list.
  }
  TObjListItem = class(TObject)
  private // properties
    fList: TObjList;
  protected
    procedure SetList(AList: TObjList);
      {Sets owning list - is actually accessed by TObjList - like a friend
      method}
  public
    destructor Destroy; override;
      {Class destructor - ensures that object is detached from any list it
      belongs to before destroying itself}
    function FindSibling(Finder: TObjListFinder; Ptr: Pointer): TObjListItem;
      {Return the reference to the first sibling object found in same list as
      this item which has properties in accordance with Ptr that causes the
      Finder function to return True. Returns nil if no such sibling exists}
    property List: TObjList read fList;
      {The list to which this item belongs}
  end;


implementation


resourcestring
  // Error messages
  sAlreadyInList =
    'Attempting to add a TObjListItem object to more than one list';
  sChangeDenied = 'Attempt to change object list denied';
  sCantExchange = 'Can''t exchange objects that are not in list';
  sCantRemove = 'Can''t remove object - it is not in the list';

{ TObjList }

function TObjList.Add(AnObject: TObjListItem): Integer;
  {Add the given object to end of list and return it's index in list. An
  exception is raised if the given object is already in a list. NOTE: this is
  the only method used to add something to the list and is where permission to
  add items is sought}
begin
  // Check it's OK to add items to the list - exception raised if not
  CheckOKToChange;
  // Check if the object is in another list and raise exception if so
  if AnObject.List <> nil then
    raise EObjList.Create(sAlreadyInList);
  // If we get here we can add the object to the list
  Result := fItems.Add(Pointer(AnObject));
  // .. and tell the object it's in this list
  AnObject.SetList(Self);
end;

procedure TObjList.CheckOKToChange;
  {Triggers OnChange event and returns if change is allowed, otherwise raises
  exception}
var
  OK: Boolean;    // flag that records whether it's OK to change
begin
  // Set flag to default "OK to change" value
  OK := True;
  // If event handler is assigned then call it - this may change OK flag
  if Assigned(fOnChange) then
    fOnChange(OK);
  // Raise exception if it's not OK to change the object list
  if not OK then
    raise EObjList.Create(sChangeDenied);
end;

procedure TObjList.Clear;
  {Clear all objects from the list, freeing each object}
begin
  while Count > 0 do
    Delete(Count - 1);
end;

constructor TObjList.Create(Owner: TObject);
  {Constructor - creates owned list object with items of class TObjectListItem}
begin
  CreateForClass(Owner, TObjListItem);
end;

constructor TObjList.CreateForClass(Owner: TObject; AClass: TObjListItemClass);
  {Constructor - creates owned list object that contains items of given class}
begin
  inherited Create;
  fItems := TList.Create;
  fItemClass := AClass;
  fOwner := Owner;
end;

procedure TObjList.Delete(Index: Integer);
  {Remove the object at the given index from the list and free the object}
var
  Obj: TObjListItem;  // the object to be freed
begin
  // Record object to free (can't free it here since it's needed by Remove
  Obj := GetObject(Index);
  Remove(Index);                  // check that OK to alter list done via Remove
  // It's now safe to free the object
  Obj.Free;
end;

procedure TObjList.DeleteObj(AnObject: TObjListItem);
  {Remove the given object from the list and free the object. Raise EObjList
  exception if object not in list}
begin
  // Remove the given object from list - raise EObjList if not it list
  RemoveObj(AnObject);         // check that OK to alter list done via RemoveObj
  // Free the removed object
  AnObject.Free;
end;

destructor TObjList.Destroy;
  {Destructor - deletes and frees all objects from list and frees owned TList
  object}
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TObjList.Exchange(Obj1, Obj2: TObjListItem);
  {Exchange position in list of the two given objects. Raise exception if either
  of the items do not exist}
var
  Idx1, Idx2: Integer;    // index of items in list
begin
  // Find index of each item and raise exception if not in list
  Idx1 := IndexOf(Obj1);
  Idx2 := IndexOf(Obj2);
  if (Idx1 = -1) or (Idx2 = -1) then
    raise EObjList.Create(sCantExchange);
  // Now swap if idnexes are different
  if Idx1 <> Idx2 then
    fItems.Exchange(Idx1, Idx2);
end;

function TObjList.FindIndex(Finder: TObjListFinder; Ptr: Pointer): Integer;
  {Return the index of the first object in the list which has properties in
  accordance with Ptr that causes the Finder function to return True}
var
  I : Integer;    // loops thru list
begin
  // Set "not found" result
  Result := -1;
  // Loop thru list
  for I := 0 to Count - 1 do
    // Check if current list item has required properties
    if Finder(GetObject(I), Ptr) then
    begin
      // It does - record its index and quit
      Result := I;
      Break;
    end;
end;

function TObjList.FindObject(Finder: TObjListFinder;
  Ptr: Pointer): TObjListItem;
  {Return the first object in the list which has properties in accordance with
  Ptr that causes the Finder function to return True}
var
  Index: Integer;   // the index of in list of the given object
begin
  // Find index in list of given object and return object if found or nil if not
  Index := FindIndex(Finder, Ptr);
  if Index = -1 then
    Result := nil
  else
    Result := GetObject(Index);
end;

function TObjList.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fItems.Count;
end;

function TObjList.GetItem(I: Integer): TObjListItem;
  {Read access method for Items property}
begin
  Result := GetObject(I);
end;

function TObjList.GetObject(AnIndex: Integer): TObjListItem;
  {Return a reference to the object at the given index in the list}
begin
  Result := TObjListItem(fItems[AnIndex]);
end;

function TObjList.IndexOf(AnObject: TObjListItem): Integer;
  {Return the index in the list of the given object, or -1 if not in list}
begin
  Result := fItems.IndexOf(Pointer(AnObject))
end;

procedure TObjList.Remove(Index: Integer);
  {Remove the object at the given index from the list without freeing the
  object. NOTE: this method is called by all other methods that remove an object
  from list - this is where permission to remove is sought}
begin
  // Check it's OK to change list - raise exception if not
  CheckOKToChange;
  // If we get here we can go ahead - tell object it's no longer in list
  GetObject(Index).SetList(nil);
  // .. and delete its entry in our list
  fItems.Delete(Index);
end;

procedure TObjList.RemoveObj(AnObject: TObjListItem);
  {Remove the given object from the list without freeing the object. Raise
  EObjList exception if object not in list}
var
  Index: Integer;   // index of object in list
begin
  // Find object's index in list
  Index := IndexOf(AnObject);
  // Raise exception if it's not there
  if Index = -1 then
    raise EObjList.Create(sCantRemove);
  // Remove it from list
  Remove(Index);                  // check that OK to alter list done via Remove
end;

{ TObjListItem }

destructor TObjListItem.Destroy;
  {Class destructor - ensures that object is detached from any list it belongs
  to before destroying itself}
begin
  // Detach self from any list - fList references owning list
  if fList <> nil then
    fList.RemoveObj(Self);
  inherited Destroy;
end;

function TObjListItem.FindSibling(Finder: TObjListFinder;
  Ptr: Pointer): TObjListItem;
  {Return the reference to the first sibling object found in same last as this
  item which has properties in accordance with Ptr that causes the Finder
  function to return True. Returns nil if no such sibling exists}
var
  I: Integer;       // loops through list that this object belongs to
  Index: Integer;   // index of required sibling object in list
begin
  // Set default "not found" result
  Result := nil;
  // If this object is not part of a list then exit with "not found" result
  if fList = nil then
    Exit;
  // Scan parent list to find index of any matching sibling
  // set index to "not found" result
  Index := -1;
  // scan thru parent list
  for I := 0 to fList.Count - 1 do
    // check if Finder returns true for current item, providing it is not self
    if (fList[I] <> Self) and Finder(fList[I], Ptr) then
    begin
      // found a match - exit loop
      Index := I;
      Break;
    end;
  // If we've found a match, return reference to it
  if Index <> -1 then
    Result := fList[Index];
end;

procedure TObjListItem.SetList(AList: TObjList);
  {Sets owning list - is actually accessed by TObjList - like a friend method}
begin
  fList := AList;
end;

end.

