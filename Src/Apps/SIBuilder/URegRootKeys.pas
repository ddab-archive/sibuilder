{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegRootKeys.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TRegRootKey and TRegRootKeyList
                            classes. The latter class manages a list of root
                            registry key objects while TRegRootKey encapsulates
                            the information needed about the root key, including
                            the list of subkeys that need to be installed.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 10/08/2000
      @COMMENTS             This unit's classes now read/write their properties
                            from/to XML objects rather than direct to binary
                            streams. The project file saving/loading behaviour
                            has been separated from the installation data file
                            creation process. Because of this methods enabling
                            the installation information required by the install
                            library to be written to an XML file have been
                            added. Since the classes are no longer used by the
                            install/uninstall programs all registry update and
                            querying functionality has been removed from the
                            classes.\
                            In addition, the following redundant items were
                            removed:
                            + FRootKey field and overridden GetRootKey method
                            + SetName property access method override
                            + GetPath property access method override
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 29/12/2002
      @COMMENTS             + Modified Deletable property access method to
                              support new TRegKeyDeletionActions type, rather
                              than boolean.
                            + Replaced lookup table and local helper function
                              for reg root keys with calls into URegUtils unit.
                            + Added new TRegRootKeyList.AddKeyFromPath method to
                              add keys objects from complete key path.
                            + Instead of raising exception on attempts to set
                              Deletable property, the attempt is now simply
                              quietly ignore: Deletable is always rdNone
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Moved a unit reference from interface
                            to implementation.
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
 * The Original Code is URegRootKeys.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegRootKeys;


interface


uses
  // Delphi
  Windows,
  // Project
  URegKeys, UXMLTag, UCommonTypes;

  
type

  {
  ERegRootKey:
    Class of exception raised by TRegRootKey.

    Inheritance: ERegRootKey -> ERegKey -> [Exception]
  }
  ERegRootKey = class(ERegKey);

  {
  ERegRootKeyList:
    Class of exception raised by TRegRootKeyList.

    Inheritance: ERegRootKeyList -> ERegKeyList -> [Exception]
  }
  ERegRootKeyList = class(ERegKeyList);

  {
  TRegRootKey:
    Class that encapsulates information about registry root keys that is needed
    for installation / uninstallation. Descends from TRegKey and adds/alters
    functionality.

    Inheritance: TRegRootKey -> TRegKey -> TRegObject -> TXMLObjectItem
      -> TObjListItem -> [TObject]
  }
  TRegRootKey = class(TRegKey)
  private
    procedure DisallowChanges(var AllowChange: Boolean);
      {Event handler for DataItems list - disallows changes to that list. This
      is because we are forbidding data items to be assigned to the root key}
  protected // properties
    procedure SetDeletable(const Value: TRegKeyDeletionActions); override;
      {Override of write access method for Deletable property: ignores setting
      and simply sets to rdNone}
  protected
    procedure CreateOwnedObjects; override;
      {Creates owned objects and assigns OnChange handler to DataItems owned
      object}
  public
    constructor Create(const AName: string); override;
      {Class constructor: sets default values}
    procedure Validate;
      {Validate this root key and all it's sub-keys}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves project information as sub tag of given XML tag}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Sets properties according to information in given XML tag and also loads
      any sub-keys from sub-tags}
    procedure SaveInstallXML(const ParentTag: TXMLTag); override;
      {Saves information required to install/uninstall this registry root key as
      a sub-tag of given tag}
  end;

  {
  TRegRootKeyList:
    Class maintains a list of registry root keys and causes the subkeys of the
    root keys to be validated and installed / unistalled.

    Inheritance: TRegRootKeyList -> TRegKeyList -> TRegObjList -> TXMLObjectList
      -> TObjList -> [TObject]
  }
  TRegRootKeyList = class(TRegKeyList)
  private // properties
    function GetItem(AnIndex: Integer): TRegRootKey;
  public
    constructor Create(Owner: TObject); override;
      {Creates list of TRegRootKey objects}
    procedure Validate;
      {Validates all root keys in list, raising ERegRootKeyList exception if
      there is an error}
    function AddKeyFromPath(const KeyPath: string): TRegKey;
      {Given a fully qualified key path name (from root key) creates a sequence
      of key objects to represent the path. KeyPath contains a sequence of sub-
      paths of this key to create. The method checks to see if the first key in
      the path is a valid root key and creates it if necessary. The remainder of
      the path is then created by calling a similar method of TRegKey. A
      reference to the terminal key object in the path is returned}
    property Items[AnIndex: Integer]: TRegRootKey read GetItem;
      {The RegRootKey objects in the list as an array}

  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UObjList, URegUtils, UStringProcs;


resourcestring
  // Error messages
  sInvalidRootKey = 'Invalid root key: %s';
  sRootKeyHasNoValues =
    'There are data items in the root key %s when there should be none';


{ Helper routine }

function NameToKey(const AName: string): HKEY;
  {Return required key value for a given name, raise exception if not found}
begin
  Result := URegUtils.RegRootKeyStrToKey(AName);
  if Result = 0 then
    raise ERegRootKey.CreateFmt(sInvalidRootKey, [AName]);
end;


{ TRegRootKey }

constructor TRegRootKey.Create(const AName: string);
  {Class constructor: sets default values}
begin
  inherited;
  // Make registry key default to never delete
  Deletable := rdNone;
end;

procedure TRegRootKey.CreateOwnedObjects;
  {Creates owned objects and assigns OnChange handler to DataItems owned object}
begin
  inherited CreateOwnedObjects;
  DataItems.OnChange := DisallowChanges;
end;

procedure TRegRootKey.DisallowChanges(var AllowChange: Boolean);
  {Event handler for DataItems list - disallows changes to that list. This is
  becuase we are forbidding data items to be assigned to the root key}
begin
  AllowChange := False;
end;

procedure TRegRootKey.LoadProjectXML(const XMLTag: TXMLTag);
  {Sets properties according to information in given XML tag and also loads any
  sub-keys from sub-tags}
var
  I: Integer;       // loops thru all subtags
  SubTag: TXMLTag;  // refers to sub-tags
begin
  // Record name of root key
  Name := XMLTag.Params['name'];
  // Loop thru all sub-tags adding to list of subkeys, ignoring unknown tags
  for I := 0 to Pred(XMLTag.SubTagCount) do
  begin
    SubTag := XMLTag.SubTags[I];
    if SubTag.Tag = 'regkey' then
      SubKeys.AddFromXML(SubTag);
  end;
end;

procedure TRegRootKey.SaveInstallXML(const ParentTag: TXMLTag);
  {Saves information required to install/uninstall this registry root key as a
  sub-tag of given tag}
var
  Tag: TXMLTag; // tag to hold root key information
begin
  // Only save root key if it has subkeys
  if SubKeys.Count > 0 then
  begin
    // Create the required root key tag
    Tag := ParentTag.AddSubTag('regrootkey');
    // Save hkey value of root key
    Tag.ParamAsInt['hkey'] := NameToKey(Name);
    // Save all sub keys as sub-tags of this one
    SubKeys.SaveInstallXML(Tag);
  end;
end;

procedure TRegRootKey.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves project information as sub tag of given XML tag}
var
  Tag: TXMLTag; // tag that stores this object's project information
begin
  // Create new tag to hold this roo key's information
  Tag := ParentTag.AddSubTag('regrootkey');
  // Record name of root key
  Tag.Params['name'] := Name;
  // Save any subkeys
  SubKeys.SaveProjectXML(Tag);
  // Don't save deletable property - it is always false
  // Don't save data items, root keys have none
end;

procedure TRegRootKey.SetDeletable(const Value: TRegKeyDeletionActions);
  {Override of write access method for Deletable property: ignores setting and
  simply sets to rdNone}
begin
  // We can't set the value of the Deletable to anything except rdNone
  inherited SetDeletable(rdNone);
end;

procedure TRegRootKey.Validate;
  {Validate this root key and all it's sub-keys}
begin
  // Check there are no data items - raise exception if so
  if DataItems.Count > 0 then
    raise ERegRootKey.CreateFmt(sRootKeyHasNoValues, [Name]);
  // Check owned sub-keys by getting them to validate themselves
  try
    SubKeys.Validate;
  except
    // A subkey was not valid - raise required exception
    on E: ERegKeyList do
      raise ERegRootKey.CreateFmt('%s\%s', [Name, E.Message]);
  end;
end;

{ TRegRootKeyList }

function TRegRootKeyList.AddKeyFromPath(const KeyPath: string): TRegKey;
  {Given a fully qualified key path name (from root key) creates a sequence of
  key objects to represent the path. KeyPath contains a sequence of sub-paths of
  this key to create. The method checks to see if the first key in the path is a
  valid root key and creates it if necessary. The remainder of the path is then
  created by calling a similar method of TRegKey. A reference to the terminal
  key object in the path is returned}
var
  RootKeyStr: string;       // name of root key at head of key path
  RootKeyObj: TRegRootKey;  // reference to root key object at head of path
  RemainingPath: string;    // remainder of key path after removing current key
begin
  // Split given path into root name and remainder of path
  UStringProcs.SplitStr(KeyPath, '\', RootKeyStr, RemainingPath);
  // Check whole key path validity
  if RemainingPath <> '' then
    // we have key including more than just root key: validate whole path
    URegUtils.RegUtilCheck(URegUtils.ValidateRegKeyPath(KeyPath))
  else
    // we have only a root key: validate root key
    URegUtils.RegUtilCheck(URegUtils.ValidateRegRootKeyStr(RootKeyStr));
  // Find root key with required name and create it if it doesn't exist
  RootKeyObj := FindByName(RootKeyStr) as TRegRootKey;
  if not Assigned(RootKeyObj) then
  begin
    RootKeyObj := TRegRootKey.Create(RootKeyStr);
    Self.Add(RootKeyObj);
  end;
  if RemainingPath = '' then
    // We're at end of key path: return reference to terminal subkey
    Result := RootKeyObj
  else
    Result := RootKeyObj.AddSubKeyFromPath(RemainingPath);
end;

constructor TRegRootKeyList.Create(Owner: TObject);
  {Creates list of TRegRootKey objects}
begin
  inherited CreateForClass(Owner, TRegRootKey);
end;

function TRegRootKeyList.GetItem(AnIndex: Integer): TRegRootKey;
  {Read access method for Items property}
begin
  Result := inherited GetItem(AnIndex) as TRegRootKey;
end;

procedure TRegRootKeyList.Validate;
  {Validates all root keys in list, raising ERegRootKeyList exception if there
  is an error}
var
  I: Integer; // loops thru root keys in list
begin
  try
    // Loop thru list getting each TRegRootKey object to validate itelf
    for I := 0 to Count - 1 do
      GetItem(I).Validate;
  except
    on E: ERegRootKey do
      // A root key wasn't valid - raise required exception
      raise ERegRootKeyList.Create(E.Message);
  end;
end;

end.
