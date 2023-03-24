{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegKeys.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TRegKey and TRegKeyList classes.
                            The latter class manages a list of user-defined
                            registry subkeys while TRegKey encapsulates the data
                            required for such a key, including a list of any
                            data items and further subkeys.
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
                            creation process. Because of this methods have been
                            added to enable the installation information
                            required by the install library to be written to an
                            XML file. Since the classes are no longer used by
                            the install/uninstall programs all registry update
                            functionality has been removed from the classes.\
                            In addition, the following usnused methods were
                            removed:
                            + TRegKeyList.FindByName
                            + TRegKey.GetPath property access override
                            + TRegKey.GetRootKey property access override
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 29/12/2002
      @COMMENTS             + Modified Deletable property access method to
                              support new TRegKeyDeletionActions type, rather
                              than boolean. Modified load and save XML code
                              accordingly. New keys now default to this new
                              values.
                            + Made protected TRegKey.HasSubKeys method public.
                            + Added new TRegKey.HasDataItems method to return if
                              key has data items.
                            + Added new TRegKey.PathName method to return full
                              key path name.
                            + Added new TRegKey.AddSubKeyFromPath method to
                              add a sub key from the full path name. Returns
                              reference to terminal key object.
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
 * The Original Code is URegKeys.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegKeys;


interface


uses
  // Delphi
  Windows, SysUtils,
  // Project
  URegObjects, URegData, UXMLTag, UCommonTypes;

type

  {
  ERegKey:
    Class of exception raised by TRegKey.

    Inheritance: ERegKey -> [Exception]
  }
  ERegKey = class(Exception);

  {
  ERegKeyList:
    Class of exception raised by TRegKeyList.

    Inheritance: ERegKeyList -> [Exception]
  }
  ERegKeyList = class(Exception);


  TRegKeyList = class;

  {
  TRegKey:
    Class that encapsulates information about a registry key that is needed to
    install or uninstall it in registry.

    Inheritance: TRegKey -> TRegObject -> TXMLObjectItem -> TObjListItem
      -> [TObject]
  }
  TRegKey = class(TRegObject)
  private // properties
    fDeletable: TRegKeyDeletionActions;
    fDataItems: TRegDataList;
    fSubKeys: TRegKeyList;
  protected // properties
    procedure SetDeletable(const Value: TRegKeyDeletionActions); virtual;
      {Overrideable write access method for Deletable property}
  protected
    procedure CreateOwnedObjects; override;
      {Create lists of registry data items and sub keys}
  public
    constructor Create(const AName: string); override;
      {Class constructor: sets default values}
    destructor Destroy; override;
      {Destructor - frees owned objects}
    procedure Validate;
      {Checks that all owned data items have valid values and that all subkeys
      are also valid. Returns normally if everything is OK and raises ERegKey
      exception if an error is found}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves project information as sub tag of given XML tag}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Sets properties according to information in given XML tag and also loads
      any sub-keys and data items from sub-tags}
    procedure SaveInstallXML(const ParentTag: TXMLTag); override;
      {Saves information required to install/uninstall this registry sub-key as
      a sub-tag of given tag}
    function AddSubKeyFromPath(const KeyPath: string): TRegKey;
      {Given a fully qualified key path name creates a sequence of key objects
      to represent the path. KeyPath contains a sequence of sub-paths of this
      key to create. The method checks to see if the first key in the path
      exists as a sub key and creates it if not. The remainder of the path is
      then created by recursively calling this method. A reference to the
      terminal key object in the path is returned}
    function HasSubKeys: Boolean;
      {Returns true if this key has subkeys and false otherwise}
    function HasDataItems: Boolean;
      {Returns true if this key has data items}
    function PathName: string;
      {Returns fully qualified path name of key}
    property Deletable: TRegKeyDeletionActions
      read fDeletable write SetDeletable;
      {Flag set true if this registry key is to be deleted by un-install
      program}
    property SubKeys: TRegKeyList
      read fSubKeys;
      {List of sub-keys}
    property DataItems: TRegDataList
      read fDataItems;
      {List of registry data items associated with this key}
  end;

  {
  TRegKeyList:
    Class that maintains a list of TRegKey objects which can be validated or
    installed/unintalled.

    Inheritance: TRegKeyList -> TRegObjList -> TXMLObjectList -> TObjList
      -> [TObject]
  }
  TRegKeyList = class(TRegObjList)
  private // properties
    function GetItem(AnIndex: Integer): TRegKey;
  public
    constructor Create(Owner: TObject); override;
      {Constructor creates a list of TRegKey objects}
    procedure Validate;
      {Checks that all subkeys in list are valid. Returns normally if everything
      is OK and raises ERegKeyList exception if an error is found}
    property Items[AnIndex: Integer]: TRegKey read GetItem;
      {The RegKey objects in the list as an array}
  end;


implementation


uses
  // Project
  UObjList, URegUtils, UStringProcs;


{ TRegKey }

function TRegKey.AddSubKeyFromPath(const KeyPath: string): TRegKey;
  {Given a fully qualified key path name creates a sequence of key objects to
  represent the path. KeyPath contains a sequence of sub-paths of this key to
  create. The method checks to see if the first key in the path exists as a
  sub key and creates it if not. The remainder of the path is then created by
  recursively calling this method. A reference to the terminal key object in the
  path is returned}
var
  KeyStr: string;         // name of sub key at head of key path
  KeyObj: TRegKey;        // reference to key relating to KeyStr
  RemainingPath: string;  // remainder of key path after removing current key
begin
  // Split given path into leading name and remainder of path
  UStringProcs.SplitStr(KeyPath, '\', KeyStr, RemainingPath);
  // Check leading key name for validity
  URegUtils.RegUtilCheck(URegUtils.ValidateRegKeyName(KeyStr));
  // Find sub key with required name and create it if it doesn't exist
  KeyObj := SubKeys.FindByName(KeyStr) as TRegKey;
  if not Assigned(KeyObj) then
  begin
    KeyObj := TRegKey.Create(KeyStr);
    SubKeys.Add(KeyObj);
  end;
  if RemainingPath = '' then
    // We're at end of key path: return reference to terminal subkey
    Result := KeyObj
  else
    // There's more sub keys to create in path: do it recursively
    Result := KeyObj.AddSubKeyFromPath(RemainingPath);
end;

constructor TRegKey.Create(const AName: string);
  {Class constructor: sets default values}
begin
  inherited;
  // Make registry key default to deletion if no sub keys
  Deletable := rdIfNoSubKeys;
end;

procedure TRegKey.CreateOwnedObjects;
  {Create lists of registry data items and sub keys}
begin
  inherited CreateOwnedObjects;
  fDataItems := TRegDataList.Create(Self);
  fSubKeys := TRegKeyList.Create(Self);
end;

destructor TRegKey.Destroy;
  {Destructor - frees owned objects}
begin
  fSubKeys.Free;
  fDataItems.Free;
  inherited Destroy;
end;

function TRegKey.HasDataItems: Boolean;
  {Returns true if this key has data items}
begin
  Result := (DataItems.Count > 0);
end;

function TRegKey.HasSubKeys: Boolean;
  {Returns true if this key has subkeys and false otherwise}
begin
  Result := (SubKeys.Count > 0);
end;

procedure TRegKey.LoadProjectXML(const XMLTag: TXMLTag);
  {Sets properties according to information in given XML tag and also loads any
  sub-keys and data items from sub-tags}
var
  I: Integer;         // loops thru this tag's sub-tags
  SubTag: TXMLTag;    // references each sub-tag
begin
  // Update properties from tag's parameters
  Name := XMLTag.Params['name'];
  fDeletable := TRegKeyDeletionActions(XMLTag.ParamAsInt['deletable']);
  // Load sub-keys and data items from sub-tag, ignoring unrecognised sub-tags
  for I := 0 to Pred(XMLTag.SubTagCount) do
  begin
    SubTag := XMLTag.SubTags[I];
    if SubTag.Tag = 'regkey' then
      fSubKeys.AddFromXML(SubTag)
    else if SubTag.Tag = 'regdata' then
      fDataItems.AddFromXML(SubTag);
  end;
end;

function TRegKey.PathName: string;
  {Returns fully qualified path name of key}
begin
  if Assigned(List) and Assigned(List.Owner) and (List.Owner is TRegKey) then
    // This is not the root key: result is path name of parent + this name
    Result := (List.Owner as TRegKey).PathName + '\' + Name
  else
    // This is root key in path: return just this key's name
    Result := Name;
end;

procedure TRegKey.SaveInstallXML(const ParentTag: TXMLTag);
  {Saves information required to install/uninstall this registry sub-key as a
  sub-tag of given tag}
var
  Tag: TXMLTag;       // the tag which will hold info about this sub-key
begin
  // Create a tag to hold install info for this sub key
  Tag := ParentTag.AddSubTag('regkey');
  // Record relevant properties
  Tag.Params['name'] := Name;
  if fDeletable <> rdNone then
    Tag.ParamAsInt['deletable'] := Ord(fDeletable);   // only save if not "none"
  // Save install info for any data items and subkeys
  fDataItems.SaveInstallXML(Tag);
  fSubKeys.SaveInstallXML(Tag);
end;

procedure TRegKey.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves project information as sub tag of given XML tag}
var
  Tag: TXMLTag;   // the subtag where info about this sub-key is stored
begin
  // Create a sub-tag for this sub-key
  Tag := ParentTag.AddSubTag('regkey');
  // Record relevant properties in the tag's parameters
  Tag.Params['name'] := Name;
  Tag.ParamAsInt['deletable'] := Ord(fDeletable);
  // Save the data items and sub keys as sub-tags of this one
  fDataItems.SaveProjectXML(Tag);
  fSubKeys.SaveProjectXML(Tag);
end;

procedure TRegKey.SetDeletable(const Value: TRegKeyDeletionActions);
  {Overrideable write access method for Deletable property}
begin
  // Simply record value of property
  fDeletable := Value;
end;

procedure TRegKey.Validate;
  {Checks that all owned registry data items have valid values and that all
  subkeys are also valid. Returns normally if everything is OK and raises
  ERegKey exception if an error is found}
begin
  // First check all data items
  try
    fDataItems.Validate;
  except
    on E: ERegDataList do
      raise ERegKey.CreateFmt('%s\%s', [Name, E.Message]);
  end;
  // Now check owned sub-keys
  try
    fSubKeys.Validate;
  except
    on E: ERegKeyList do
      raise ERegKey.CreateFmt('%s\%s', [Name, E.Message]);
  end;
end;


{ TRegKeyList }

constructor TRegKeyList.Create(Owner: TObject);
  {Constructor creates a list of TRegKey objects}
begin
  inherited CreateForClass(Owner, TRegKey);
end;

function TRegKeyList.GetItem(AnIndex: Integer): TRegKey;
  {Read access method for Items property}
begin
  Result := inherited GetItem(AnIndex) as TRegKey;
end;

procedure TRegKeyList.Validate;
  {Checks that all subkeys in list are valid. Returns normally if everything is
  OK and raises ERegKeyList exception if an error is found}
var
  I: Integer;   // loops thru list
begin
  // Get each item in the list to validates itself
  try
    for I := 0 to Count - 1 do
      GetItem(I).Validate;
  except
    on E: ERegKey do
      // One of list items was invalid - raise required exception
      raise ERegKeyList.Create(E.Message);
  end;
end;

end.
