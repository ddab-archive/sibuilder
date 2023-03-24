{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UXMLObjects.pas
  @COMMENTS                 This unit contains defintions of two related clases
                            - TXMLObjectList and TXMLObjectItem that extend the
                            TObjList and TObjListItem classes by adding the
                            ability to stream list and object contents to and
                            from XML files.
  @OTHER_NAMES              + Original unit name was UStreamableObjList.
                            + Changed to UXMLObjects at v2.0.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 10/08/2000
      @COMMENTS             Unit renamed from UStreamableObjList. The purpose of
                            the unit remains to be the root class of all the
                            SIBuilder objects that exhibit persistency. However
                            the method has been changed from loading and saving
                            from and to binary data streams to loading and
                            saving form and to XML format files. \
                            Key changes are:
                            + Added methods to save and load objects to and from
                              an XML object in place of binary stream saving an
                              loading methods.
                            + Replaced CreateFromStream constructor with a
                              CreateFromXML constructor.
                            + Created method to add new objects to the object
                              list by reading their properties from an XML
                              object.
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
 * The Original Code is UXMLObjects.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UXMLObjects;

interface

uses
  // Delphi
  Classes,
  // Project
  UObjList, UXMLTag;

type

  TXMLObjectItem = class;

  TXMLObjectItemClass = class of TXMLObjectItem;

  TXMLObjectList = class(TObjList)
    {Object list that can be streamed to and from an XML object}
  public
    constructor Create(Owner: TObject); override;
      {Creates list object (owned by given owner) that stores
      TXMLObjectItem objects}
    procedure SaveProjectXML(const ParentTag: TXMLTag);
      {Saves each item in list to an XML project file}
    procedure AddFromXML(const XMLTag: TXMLTag);
      {Creates new list item and adds to list, reading properties from given XML
      object}
  end;

  TXMLObjectItem = class(TObjListItem)
    {Abstract base class for a list item that can be streamed to and from a
    XML object}
  protected
    procedure CreateOwnedObjects; virtual;
      {Method called by constructors to create any objects owned by this object
      - in TXMLObjectItem this method does nothing - it should be
      overridden by classes that need to create owned objects}
  public
    constructor Create;
      {Class constructor - creates a new default object}
    constructor CreateFromXML(const XMLTag: TXMLTag); virtual;
      {Class constructor - creates owned object and reads in property values
      from given XML object}
    procedure SaveProjectXML(const ParentTag: TXMLTag); virtual; abstract;
      {Abstract method for saving objects properties to given XML object}
    procedure LoadProjectXML(const XMLTag: TXMLTag); virtual; abstract;
      {Abstract methor for reading object's properties from given XML object}
  end;

implementation

uses
  // VCL
  SysUtils;

{ TXMLObjectList }

procedure TXMLObjectList.AddFromXML(const XMLTag: TXMLTag);
  {Creates new list item and adds to list, reading properties from given XML
  object}
begin
  inherited Add(TXMLObjectItemClass(ItemClass).CreateFromXML(XMLTag));
end;

constructor TXMLObjectList.Create(Owner: TObject);
  {Creates list object (owned by given owner) that stores TXMLObjectItem
  objects}
begin
  inherited CreateForClass(Owner, TXMLObjectItem);
end;

procedure TXMLObjectList.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves each item in list to an XML project file}
var
  I: Integer;   // loops thru items in list
begin
  // Get each item in list to save itself to XML object
  for I := 0 to Pred(Count) do
    (GetObject(I) as TXMLObjectItem).SaveProjectXML(ParentTag);
end;

{ TXMLObjectItem }

constructor TXMLObjectItem.Create;
  {Class constructor - creates a new default object}
begin
  inherited Create;
  // Create any objects that are owned by this object
  CreateOwnedObjects;
end;

constructor TXMLObjectItem.CreateFromXML(const XMLTag: TXMLTag);
  {Class constructor - creates owned object and reads in property values from
  given XML object}
begin
  inherited Create;
  // Create any objects that are owned by this object
  CreateOwnedObjects;
  // Load object's properties from stream - calls abstract method in this class
  LoadProjectXML(XMLTag);
end;

procedure TXMLObjectItem.CreateOwnedObjects;
  {Method called by constructors to create any objects owned by this object -
  in TXMLObjectItem this method does nothing - it should be overridden by
  classes that need to create owned objects}
begin
  // Do nothing
end;

end.
