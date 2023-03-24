{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegObjects.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TRegObject and TRegObjList classes.
                            TRegObject provides an abstract ancestor class for
                            all objects that record the operations the install &
                            uninstall programs need to apply to the registry. It
                            provides common properties for the registry obejcts.
                            TRegObjList manages a list of TRegObject objects and
                            gives the ability to search the list for TRegObjects
                            by name.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 11/08/2000
      @COMMENTS             New version of this object now works with XML
                            objects instead of binary streams to achieve
                            persistency. Particular changes were:
                            + Changed derivation of classes from
                              TStreamableListItem to TXMLObjectItem and from
                              TStreamableObjList to TXMLObjectList.
                            + Removed SaveToStream and LoadFromStream methods
                            + Added abstract method to save install info to XML
                              for a particular registry entity.
                            + Changed binary streaming unit references to XML
                              related unit references.\
                            In addition, redundant code was removed as follows:
                            + Path property and its abstract read method.
                            + RootKey property its abstract read method.
                            + SetName property access method - property now
                              simply accesses field.
                            + GetParent method.
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
 * The Original Code is URegObjects.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegObjects;

interface

uses
  // VCL
  Classes, Windows, SysUtils,
  // Project
  UXMLTag, UObjList, UXMLObjects;

type
  ERegObject = class(Exception);
    {Class of exception raised by TRegObject}

  TRegObject = class(TXMLObjectItem)
    {Abstract class that provides an anscestor class for all objects that are to
    operate on registry (TRegKey, TRegRootKey and TRegValue). It provides common
    properties for the registry objects}
  private // properties
    FName: string;
  protected
    function CheckForName(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Returns true if RegObject AnObject has a name that matches the name
      passed as a pointer in Ptr)}
  public
    constructor Create(const AName: string); virtual;
      {Constructor - creates an instance of object with given name}
    procedure SaveInstallXML(const ParentTag: TXMLTag); virtual; abstract;
      {Abstract method which is overridden in descendant classes to save
      information required to install/uninstall the descendant item as a
      sub-tag of given XML tag}
    function SiblingHasName(const AName: string): Boolean;
      {Returns true if any sibling in list (other than this object) has given
      Name property value}
    property Name: string read FName write FName;
      {The object's name}
  end;

  TRegObjList = class(TXMLObjectList)
    {Class implements a list of TRegObject objects}
  protected // properties
    function GetItem(AnIndex: Integer): TRegObject;
  protected
    function CheckForName(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Returns true if RegObject AnObject has a name that matches the name
      passed as a pointer in Ptr)}
  public
    constructor Create(Owner: TObject); override;
      {Class constructor - creates a list of TRegObject objects}
    function FindByName(const AName: string): TRegObject;
      {Return RegObject with given name, or nil if there is no such object}
    procedure SaveInstallXML(const ParentTag: TXMLTag);
      {Saves information required to install/uninstall all owned registry
      objects in XML format as subtags of the given tag. Calls
      TRegObject.SaveInstallXML abstract method}
    property Items[AnIndex: Integer]: TRegObject read GetItem;
      {The RegKey objects in the list as an array}
  end;

implementation

{ TRegObject }

function TRegObject.CheckForName(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
  {Returns true if RegObject AnObject has a name that matches the name passed as
  a pointer in Ptr)}
begin
  Result := CompareText((AnObject as TRegObject).Name, StrPas(PChar(Ptr))) = 0;
end;

constructor TRegObject.Create(const AName: string);
  {Constructor - creates an instance of object with given name}
begin
  inherited Create;
  FName := AName;
end;

function TRegObject.SiblingHasName(const AName: string): Boolean;
  {Returns true if any sibling in list (other than this object) has given Name
  property value}
begin
  Result := (FindSibling(CheckForName, PChar(AName)) <> nil);
end;

{ TRegObjList }

function TRegObjList.CheckForName(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
    {Returns true if RegObject AnObject has a name that matches the name passed
    as a pointer in Ptr)}
begin
  Result := CompareText((AnObject as TRegObject).Name, StrPas(PChar(Ptr))) = 0;
end;

constructor TRegObjList.Create(Owner: TObject);
  {Class constructor - creates a list of TRegObject objects}
begin
  inherited CreateForClass(Owner, TRegObject);
end;

function TRegObjList.FindByName(const AName: string): TRegObject;
  {Return RegObject with given name, or nil if there is no such object}
begin
  Result := FindObject(CheckForName, PChar(AName)) as TRegObject;
end;

function TRegObjList.GetItem(AnIndex: Integer): TRegObject;
  {Read access method for the Items property}
begin
  Result := GetObject(AnIndex) as TRegObject;
end;

procedure TRegObjList.SaveInstallXML(const ParentTag: TXMLTag);
  {Saves information required to install/uninstall all owned registry objects in
  XML format as subtags of the given tag. Calls TRegObject.SaveInstallXML
  abstract method}
var
  I: Integer;   // loops thru items in list
begin
  // Loop through all items in list getting them to save their installation info
  // as sub tags of the given XML tag
  for I := 0 to Pred(Count) do
    (GetObject(I) as TRegObject).SaveInstallXML(ParentTag);
end;

end.
