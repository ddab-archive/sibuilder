{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UGroups.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TGroup and TGroupList classes.
                            TGroup encapsulates an "installation group" - a
                            group of files that will be installed in a
                            particular folder. This contains information about
                            the installation folder for the files and how/if the
                            files and the folder will be deleted on
                            uninstallation. TGroupList manages a list of TGroup
                            items.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 28/08/2000
      @COMMENTS             This unit's classes now read/write their properties
                            from/to XML objects rather than direct to binary
                            streams. The project file saving/loading behaviour
                            has been separated from the installation data file
                            creation process. Because of this, methods enabling
                            the classes to "build" the relevant part of an
                            installation project have been added. These store
                            information about the group in the installation
                            information XML file and cause the groups owned list
                            of files objects to copy the files into the install
                            data file. Since the classes are no longer used by
                            the install/uninstal programs all the
                            install/uninstall related functionality has been
                            removed. The unused TGroup.Hidden property was
                            removed. Some enumerated types have been moved to a
                            separate unit to be shared with other SITools
                            projects.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 28/12/2000
      @COMMENTS             Added code to return string list containing list of
                            all install files in group list, including any
                            non-expanded path macros.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 21/01/2001
      @COMMENTS             Modified how list of install files is created by
                            TGroupList.GetInstallFileNames as follows:
                            + The file names are now appended to given string
                              list rather than replacing any existing contents.
                            + An optional second param to method forces
                              inclusion of install folder name file in addition
                              to files in folder\
                            Also moved error string literal to a resource
                            string.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             + Added support for file item's shared DLL
                              property by ensuring those properties are set
                              false when TGroup's file deletion property is not
                              set to "delete only files that were installed"
                              options.
                            + Made set TGroup's file deletion property to "All
                              file in folder" when directory deletion property
                              is set to "delete folder and all contents". This
                              enables correct functioning of file item's
                              Shared DLLs property. These checks are applied
                              when properties set and when loading project XML
                              since older file version won't have these
                              dependencies enforced and may break the rules.
                            + Added new OSOptions property to TGroup to
                              determine whether group installs on a given OS.
                            + Modified GetInstallFileNames methods of group and
                              group list to enable files to be filtered by type
                              and to store reference to any associated file info
                              objects in returned string list of file names.
                              When returning a folder then the group is
                              referenced in the Objects[] property.
                            + Adapted Build methods of TGroupList and TGroup to
                              use a TDataWriter object to write out files to the
                              install data file rather than writing directly to
                              stream.
                            + Adapted Validate methods to pass the
                              UninstallPrevious parameter and modified exception
                              handlers and raisers to pass along any help
                              context associated with exceptions.
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
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
 * The Original Code is UGroups.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UGroups;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UCommonTypes, UFiles, UObjList, UReportingObjects, UXMLTag, UDataWriter;

type

  {
  EGroup:
    Class of exception raised by TGroup.

    Inheritance: EGroup -> [Exception]
  }
  EGroup = class(Exception);

  {
  EGroupList:
    Class of exception raised by TGroupList.

    Inheritance: EGroupList -> [Exception]
  }
  EGroupList = class(Exception);

  {
  TGroup:
    This class encapsulates an "installation group" - a group of files that will
    be installed in a particular folder. The group has:
      + a name (used to display to user);
      + a path to the installation folder (which may include macro names);
      + a list of files to be installed;
      + information about how files and folders are to be un-installed.

    Inheritance: TGroup -> TReportingListItem -> TXMLObjectItem -> TObjListItem
      -> [TObject]
  }
  TGroup = class(TReportingListItem)
  private // properties
    fName: string;
    fPath: string;
    fFiles: TFileInfoList;
    fDirDeletion: TDirDeletionActions;      // defined in .inc file
    fFileDeletion: TFileDeletionActions;
    fOSOptions: LongWord;    // defined in .inc file
    procedure SetFileDeletion(const Value: TFileDeletionActions);
    procedure SetDirDeletion(const Value: TDirDeletionActions);
  protected
    procedure CreateOwnedObjects; override;
      {Creates Files object - a list of files in the group}
  public
    constructor Create(const Name, Path: string);
      {Records group name and installation path, sets property defaults and
      creates owned objects}
    destructor Destroy; override;
      {Frees owned objects}
    procedure Validate(UninstallPrevious: Boolean);
      {Checks that the files in this group actually exist. Returns normally if
      all files exist and raise EGroup exception with group and file name as
      message when a problem is encountered. UninstallPrevious is required in
      validation and is true if installer should uninstall previous
      installations in install folder}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves project information as sub tag of given XML tag and files as sub-
      tags}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Sets properties according to information in given XML tag and reads in
      files from sub-tags}
    procedure Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
      {Stores group's files in the install data file by using the given data
      writer object and writes group install info as a sub tag of given XML tag}
    procedure GetInstallFileNames(const NameList: TStrings;
      const FileKinds: TFileKinds; const IncFolder: Boolean = False);
      {Appends fully specified names of install files (which may contain path
      macros) to given strings object. If FileKinds is not [] then only files
      with one of the given kinds are included in list. If IncFolder is true
      then the group's folder name is included in the list. The string list's
      Objects[] property references the relevant file info object for files or
      group object for folders}
    property Name: string
      read fName write fName;
      {The name of the group}
    property Path: string
      read fPath write fPath;
      {The path where the group's files will be installed - can include macros}
    property Files: TFileInfoList
      read fFiles;
      {The list of files in the group}
    property FileDeletion: TFileDeletionActions
      read fFileDeletion write SetFileDeletion;
      {Contains information about which, if any, files in this group's folder
      are to be deleted}
    property DirDeletion: TDirDeletionActions
      read fDirDeletion write SetDirDeletion;
      {Contains information about whether this group's folder is to be deleted}
    property OSOptions: LongWord
      read fOSOptions write fOSOptions;
      {Determines on which operating system(s) the group is to be installed}
  end;

  {
  TGroupList:
    Class that maintains a list of TGroup objects which can be validated,
    stored, installed and uninstalled.

    Inheritance: TGroupList -> TReportingObjList -> TXMLObjectList -> TObjList
      -> [TObject]
  }
  TGroupList = class(TReportingObjList)
  private // properties
    function GetItem(AnIndex: Integer): TGroup;
  private
    function CheckForName(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Returns true if group AnObject has a name that matches the name passed as
      a pointer in Ptr)}
  public
    constructor Create(Owner: TObject); override;
      {Creates list object (owned by given owner) that stores TGroup objects}
    function FindByName(const AName: string): TGroup;
      {Return group object with given name, or nil if there is no such object}
    procedure GetInstallFileNames(const NameList: TStrings;
      const FileKinds: TFileKinds;
      const IncFolder: Boolean = False);
      {Appends fully specified names of install files in all groups in (which
      may contain path macros) in given strings object. If FileKinds is not []
      then only files with one of the given kinds are included in list. If
      IncFolder is true then each group's folder name is included in the list.
      The string list's Objects[] property references the relevant file info
      object for files or group object for folders}
    procedure Validate(UninstallPrevious: Boolean);
      {Checks that all groups in list are valid. Returns normally if so, but
      raises EGroupList exception with problem file name as message if there
      is a problem. UninstallPrevious is required in validation and is true if
      installer should uninstall previous installations in install folder}
    procedure Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
      {Gets each group to build itself, writing install info as subtags of given
      XML tag and copying all its files to the install data file using the given
      date writer object}
    property Items[AnIndex: Integer]: TGroup read GetItem;
      {The group objects in the list as an array}
  end;


implementation


uses
  // Project
  UFileProcs;


{ TGroup }

procedure TGroup.Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
  {Stores group's files in the install data file by using the given data writer
  object and writes group install info as a sub tag of given XML tag}
var
  Tag: TXMLTag; // XML tag to store group install info
begin
  // Create sub tag for group
  Tag := ParentTag.AddSubTag('group');
  // Record required properties as paramaters of tag
  Tag.Params['name'] := fName;
  Tag.Params['path'] := fPath;
  Tag.ParamAsInt['filedeletion'] := Ord(fFileDeletion);
  Tag.ParamAsInt['dirdeletion'] := Ord(fDirDeletion);
  Tag.ParamAsInt['os'] := Ord(fOSOptions);
  // Store files in data stream and write sub-tags for each file
  fFiles.Build(Tag, DataWriter);
end;

constructor TGroup.Create(const Name, Path: string);
  {Records group name and installation path, sets property defaults and creates
  owned objects}
begin
  inherited Create;       // this calls CreateOwnedObjects
  // Record name of group and installation path
  fName := Name;
  fPath := Path;
  fFileDeletion := fdInstalled;
  fDirDeletion := ddIfEmpty;
end;

procedure TGroup.CreateOwnedObjects;
  {Creates Files object - a list of files in the group}
begin
  // Create list of files, owned by this object
  fFiles := TFileInfoList.Create(Self);
end;

destructor TGroup.Destroy;
  {Frees owned objects}
begin
  // Free list of files
  fFiles.Free;
  inherited Destroy;
end;

procedure TGroup.GetInstallFileNames(const NameList: TStrings;
  const FileKinds: TFileKinds; const IncFolder: Boolean = False);
  {Appends fully specified names of install files (which may contain path
  macros) to given strings object. If FileKinds is not [] then only files with
  one of the given kinds are included in list. If IncFolder is true then the
  group's folder name is included in the list. The string list's Objects[]
  property references the relevant file info object for files or group object
  for folders}
var
  Index: Integer; // loops thru all files in group
begin
  if IncFolder then
    // Include the folder in list
    NameList.AddObject(UFileProcs.MakePathName(fPath), Self);
  for Index := 0 to Pred(fFiles.Count) do
    // Check if required kind and and add to list if so
    if (FileKinds = []) or (fFiles.Items[Index].FileKind in FileKinds) then
      NameList.AddObject(
        UFileProcs.MakePathName(fPath) + fFiles.Items[Index].FileName,
        fFiles.Items[Index]
      );
end;

procedure TGroup.LoadProjectXML(const XMLTag: TXMLTag);
  {Sets properties according to information in given XML tag and reads in files
  from sub-tags}
var
  I: Integer;       // loops thru subtags
  SubTag: TXMLTag;  // references sub tags
begin
  // Read in properties from tag's params
  fName := XMLTag.Params['name'];
  fPath := XMLTag.Params['path'];
  fOSOptions := XMLTag.ParamAsInt['os'];
  // file and dir deletion are interdependent: use accessor method to adjust
  SetFileDeletion(TFileDeletionActions(XMLTag.ParamAsInt['filedeletion']));
  SetDirDeletion(TDirDeletionActions(XMLTag.ParamAsInt['dirdeletion']));
  // Read sub tags, ignoring unrecognised tags
  for I := 0 to Pred(XMLTag.SubTagCount) do
  begin
    SubTag := XMLTag.SubTags[I];
    if SubTag.Tag = 'file' then
      fFiles.AddFromXML(SubTag);
  end;
end;

procedure TGroup.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves project information as sub tag of given XML tag and files as sub-tags}
var
  Tag: TXMLTag; // tag where group object is stored
begin
  // Create required tag
  Tag := ParentTag.AddSubTag('group');
  // Store properties as parameters of tag
  Tag.Params['name'] := fName;
  Tag.Params['path'] := fPath;
  Tag.ParamAsInt['filedeletion'] := Ord(fFileDeletion);
  Tag.ParamAsInt['dirdeletion'] := Ord(fDirDeletion);
  Tag.ParamAsInt['os'] := Ord(fOSOptions);
  // Save the group's files as sub-tags
  fFiles.SaveProjectXML(Tag);
end;

procedure TGroup.SetDirDeletion(const Value: TDirDeletionActions);
  {Write access method for DirDeletion property: sets FileDeletion property to
  fdAll if DirDeletion is being set to ddAll}
begin
  fDirDeletion := Value;
  if Value = ddAll then
    SetFileDeletion(fdAll);
end;

procedure TGroup.SetFileDeletion(const Value: TFileDeletionActions);
  {Write access method for FileDeletion property: records value and ensures that
  all owned file items have SharedDLL properties set according to file deletion
  type}
var
  Idx: Integer;   // loops thru all owned file items
  FI: TFileInfo;  // refers to a file item
begin
  // Record new value: don't allow anything but fdAll if DirDeletion is ddAll
  if fDirDeletion <> ddAll then
    fFileDeletion := Value
  else
    fFileDeletion := fdAll;
  // It is not valid for a file to be registered as shared DLL if it is either
  // not going to be deleted or if all files are to be deleted
  if Value <> fdInstalled then
  begin
    for Idx := 0 to Pred(fFiles.Count) do
    begin
      FI := fFiles.Items[Idx];
      FI.RegisterSharedDLL := False;
    end;
  end;
end;

procedure TGroup.Validate(UninstallPrevious: Boolean);
  {Checks that the files in this group actually exist. Returns normally if all
  files exist and raise EGroup exception with group and file name as message
  when a problem is encountered. UninstallPrevious is required in validation and
  is true if installer should uninstall previous installations in install
  folder}
begin
  // All we have to validate is the list of files
  try
    // Get list of files to validate itself
    fFiles.Validate(UninstallPrevious);
  except
    on E: EFileInfoList do
      // An error has occured in file list - pass this on
      raise EGroup.CreateHelp(E.Message, E.HelpContext);
  end;
end;


{ TGroupList }

procedure TGroupList.Build(const ParentTag: TXMLTag;
  const DataWriter: TDataWriter);
  {Gets each group to build itself, writing install info as subtags of given XML
  tag and copying all its files to the install data file using the given date
  writer object}
var
  I: Integer; // loops thru groups
begin
  // Get each group in list to store itself
  for I := 0 to Pred(Count) do
    GetItem(I).Build(ParentTag, DataWriter);
end;

function TGroupList.CheckForName(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
  {Returns true if group AnObject has a name that matches the name passed as a
  pointer in Ptr)}
begin
  Result := CompareText((AnObject as TGroup).Name, StrPas(PChar(Ptr))) = 0;
end;

constructor TGroupList.Create(Owner: TObject);
  {Creates list object (owned by given owner) that stores TGroup objects}
begin
  inherited CreateForClass(Owner, TGroup);
end;

function TGroupList.FindByName(const AName: string): TGroup;
  {Return group object with given name, or nil if there is no such object}
begin
  Result := FindObject(CheckForName, PChar(AName)) as TGroup;
end;

procedure TGroupList.GetInstallFileNames(const NameList: TStrings;
  const FileKinds: TFileKinds; const IncFolder: Boolean = False);
  {Appends fully specified names of install files in all groups in (which may
  contain path macros) in given strings object. If FileKinds is not [] then
  only files with one of the given kinds are included in list. If IncFolder is
  true then each group's folder name is included in the list. The string list's
  Objects[] property references the relevant file info object for files or group
  object for folders}
var
  Index: Integer; // loops thru all groups in list
begin
  for Index := 0 to Pred(Count) do
    GetItem(Index).GetInstallFileNames(NameList, FileKinds, IncFolder);
end;

function TGroupList.GetItem(AnIndex: Integer): TGroup;
  {Read access method for Items property}
begin
  Result := GetObject(AnIndex) as TGroup;
end;

procedure TGroupList.Validate(UninstallPrevious: Boolean);
  {Checks that all groups in list are valid. Returns normally if so, but raises
  EGroupList exception with problem file name as message if there is a problem.
  UninstallPrevious is required in validation and is true if installer should
  uninstall previous installations in install folder}
var
  I: Integer; // scans thru list
begin
  // Loop thru groups and get each one to validate itself, converting any
  // exceptions raised to EGroupList
  for I := 0 to Count - 1 do
    try
      GetItem(I).Validate(UninstallPrevious);
    except
      on E: EGroup do
        raise EGroupList.CreateHelp(E.Message, E.HelpContext);
    end;
end;

end.
