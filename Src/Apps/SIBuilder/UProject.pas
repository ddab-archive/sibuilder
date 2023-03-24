{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UBaseProject.pas
  @COMMENTS                 This unit defines the TProject class that sits at
                            the top of the heirachy of classes used to model the
                            project - it encapsulates the whole installation
                            project.
  @OTHER_NAMES              + Original unit name was UBaseProject.
                            + Changed to UProject at v2.0.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 28/08/2000
      @COMMENTS             The unit was renamed from TBaseProject. The relevant
                            methods of the now defunct TProjectBuilder class
                            (which descended from TBaseProject) were added to
                            this class. \
                            The TProject class no longer has any role in
                            actually performing installation & uninstallation so
                            all relevant code has been removed. The class's role
                            is to manage information about an installation
                            project and to output the installation files. A new
                            "Build" method was added to perform the latter
                            function, while the class was modified to permit
                            project and installation information files to be
                            output in XML format rather than the previous
                            all-purpose binary project file. \
                            Other changes were:
                            + The uninstall program (where required) is now
                              merged into the main install data file in a
                              special reserved group. The handling of the
                              WantUninstall property was changed so that it can
                              handle the creation / deletion of this group as
                              required.
                            + Added method to check if a given group name is
                              reserved.
                            + The GenerateWinApp property was removed since it
                              is not used.
                            + The CanEditInstallPath property was replaced by a
                              new InstallPathEdit property that allows three
                              options - the two exsiting "no editing" and "edit
                              via command line" options and the new "ask user"
                              option.
                            + Added new "RunProgs" property to maintain a list
                              of programs that will be run at the end of the
                              installation.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 28/12/2000
      @COMMENTS             + Added code to support in-process COM server
                              registration on installation and uninstallation.
                            + Because above modification causes an additional
                              file to be optionally distributed in hidden SYSTEM
                              group, reworked handling of hidden group.
                            + Changed handling of programs to be executed to
                              permit running of programs on uninstallation as
                              well as installation.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 21/01/2001
      @COMMENTS             + Added new LicenseInfo property and support for
                              licenses.
                            + Updated handling of reserved groups by modifying
                              IsReservedGroupName method to allow for new
                              reserved groups used by license info object.
                            + Changed handling of system group: it is no longer
                              treated as a field of the onject but created as
                              required depending on other properties.
                            + Added method to list all install files in project.
                            + General tidying up: (a) used constant for
                              RegCOMSvrs.dll file name rather than string
                              literal and (b) moved all string literal error
                              messages to resource strings.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             Major update with changes in way COM server
                            registration is handled and changes to build
                            process:
                            + Added support for speciying which OS installer
                              will run on.
                            + Added support for identifying type of compression
                              used by project.
                            + Adapted Build method to use a given TDataWriter
                              object to store project files in install data file
                              created by Builder object rather than explicitly
                              opening a stream onto data file and writing
                              directly to stream. The method now updates a given
                              XML root node rather than creating the XML file
                              object and writing it out to file itself.
                            + Modified to use updated TGroup.GetInstallFileNames
                              method.
                            + Removed installation of COM server registration
                              DLL: this functionality is now built into install
                              lib DLL.
                            + Deleted COM servers section of code: 'com' tag no
                              longer supported. Removed reference to the old
                              UCOMServers unit. COM server registration now part
                              of 'files' tag
                            + Fixed bug in GetInstallFileNames routine that was
                              returning reference to freed object.
                            + Added new [EXTERNAL] group to provide info about
                              files that are part of installation but not part
                              of project data file.
                            + Changed implementation of WantUninstall property
                              to update the system and external groups objects
                              with required file objects that depend on the
                              property.
                            + Added UninstallPrevious property and supporting
                              code.
                            + Added validation of run programs and license files
                              and ammended validation of groups take note of
                              new UninstallPrevious property.
                            + Added support for exception help contexts to be
                              passed from project validation: topics should be
                              in main help file and should be be standard, not
                              popup, topics.
                            + Enabled a '' value to be assigned to ID property.
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 23/02/2003
      @COMMENTS             Added new UninstallPrevNamed property to allow
                            uninstallation of existing project(s) before
                            the installation proceeds.
    )
    @REVISION(
      @VERSION              3.2
      @DATE                 19/02/2008
      @COMMENTS             Replaced usage of FileNames.inc include file with
                            UFileNames unit.
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
 * The Original Code is UProject.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UProject;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UCommonTypes, UFiles, UGroups, URegRootKeys, URunProgs, ULicenseInfo,
  UXMLTag, UDataWriter, UUninstPrevProj;

const
  // Default compressor if none specified (backwards compatible)
  cDefaultCompressorID = 'ZLib';

type

  {
  EProject:
    This class is used for all exceptions raised by TProject.

    Inheritance: EProject -> [Exception]
  }
  EProject = class(Exception);

  {
  TProjectProgressEvent:
    The type for OnProgress event handlers.
  }
  TProjectProgressEvent = procedure(const Msg: string) of object;

  {
  TProject:
    This class encapsulates a whole installation project.

    Inheritance: TProject -> [TObject]
  }
  TProject = class(TObject)
  private // properties
    fID: string;
    fName: string;
    fInstallPath: string;
    fInstallProgName: string;
    fInstallPathEditing: TInstallPathEditKinds;
    fInstallGroups: TGroupList;
    fRegRootKeys: TRegRootKeyList;
    fOnProgress: TProjectProgressEvent;
    fRunProgs: TRunProgList;
    fLicenseInfo: TLicenseInfo;
    fSupportedOSs: LongWord;
    fCompressorID: string;
    fUninstallPrevious: Boolean;
    fUninstallPrevNamed: TUninstallPrevProjects;
    procedure SetID(const Value: string);
    function GetWantUnInstall: Boolean;
    procedure SetWantUnInstall(const Value: Boolean);
  private
    fSysGroup: TGroup;
      {The [SYSTEM] group: this group is used to include files in install
      project that are required by system: UnInstall.exe is included in this
      group when WantUninstall is true}
    fExternalGroup: TGroup;
      {The [EXTERNAL] group: this group is used for information purposes only -
      it is not included in project build, but is used to provide inforamtion
      about files included in project directly by installer. This group may
      include Uninstall.xml and InstallLib.dll when WantUninstall is true:
      Installer deletes these files from install folder if WantUninstall is
      false}
    fUninstProg: TFileInfo;
      {File info item for uninstall program: used to include in [SYSTEM] group
      for installation when WantUninstall is true}
    fUninstInfo: TFileInfo;
      {File info item for uninstall info xml file: used to include in [EXTERNAl]
      group when WantUninstall is true}
    fInstLibInfo: TFileInfo;
      {File info item for install DLL file: used to include in [EXTERNAl] group
      when WantUninstall is true}
    function GetSourceFilePath: string;
      {Returns the path where system source files are stored. System install
      files are those provided by SIBuilder and are used by the installer}
    procedure Clear;
      {Clears all of project to default state}
    procedure EnsureRootKeys;
      {Ensures a new project contains all required registry root keys}
    procedure LoadProjectXML(const XMLTag: TXMLTag);
      {Loads project from given XML object}
    procedure SaveProjectXML(const XMLTag: TXMLTag);
      {Saves project into given XML object}
    procedure ReportProgress(const Msg: string);
      {Event handler for InstallGroups object OnReport event - simply triggers
      this object's own OnProgress event and passes message on. Can also be used
      by derived classes to send messages by OnProgess event}
    procedure SaveProjectToStream(const Stream: TStream);
      {Saves project in XML to given stream}
    procedure LoadProjectFromStream(const Stream: TStream);
      {Loads project from XML in given stream}
  public
    constructor Create;
      {Class constructor: creates owned objects and intialises fields to default
      values}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure Reset;
      {Resets project - sets properties to their creation condition}
    procedure SaveProjectToFile(const FileName: string);
      {Saves project (as XML) to given file}
    procedure LoadProjectFromFile(const FileName: string);
      {Loads project from given (XML) file}
    procedure Validate;
      {Validates project integrity and raises exception if a problem is found.
      Returns with no exception if project is OK}
    procedure Build(const InfoXML: TXMLTag; const DataWriter: TDataWriter);
      {Builds the project ready for inclusion in an install program. Information
      describing the project is output in an XML tree with its root at the given
      XML tag. The project's files are passed to the given data writer object
      which in turn writes them to the install data file}
    procedure GetInstallFileNames(const NameList: TStrings;
      const IncFolder: Boolean = False);
      {Appends fully specified names of all files installed by this project to
      the given string list. Path name may contain path macros. If IncFolder is
      true then each group's folder name is included in the list. The string
      list's Objects[] property references the relevant file info object for
      files or group object for folders}
    class function IsReservedGroupName(const GroupName: string): Boolean;
      {Returns true if the given group name is a reserved for system use}
    property ID: string read fID write SetID;
      {Brief name of project}
    property Name: string
      read fName write fName;
      {Descriptive name of project}
    property InstallPath: string
      read fInstallPath write fInstallPath;
      {Default installation path = [Install]}
    property InstallProgName: string
      read fInstallProgName write fInstallProgName;
      {The name of the installation program}
    property InstallPathEditing: TInstallPathEditKinds
      read fInstallPathEditing write fInstallPathEditing;
      {Kind of editing that user can apply to install path}
    property WantUnInstall: Boolean
      read GetWantUnInstall write SetWantUnInstall;
      {Whether an UnInstall program is to be generated}
    property UninstallPrevious: Boolean
      read fUninstallPrevious write fUninstallPrevious;
      {Whether the installer uninstalls any previous installation in same folder
      before performing current installation}
    property UninstallPrevNamed: TUninstallPrevProjects
      read fUninstallPrevNamed;
      {List of previously installed projects that must be removed before
      installating this project: installer looks each project up in windows
      registry and uninstalls them if present}
    property InstallGroups: TGroupList
      read fInstallGroups;
      {List of "Groups" (ie install folders) in project}
    property RegRootKeys: TRegRootKeyList
      read fRegRootKeys;
      {List of registry keys that are to be processed}
    property RunProgs: TRunProgList
      read fRunProgs;
      {List of programs to be run after installation}
    property LicenseInfo: TLicenseInfo
      read fLicenseInfo;
      {Object providing information about the license to be displayed by the
      installer}
    property OnProgress: TProjectProgressEvent
      read fOnProgress write fOnProgress;
      {OnProgress event - triggered with messages about progress}
    property SupportedOSs: LongWord
      read fSupportedOSs write fSupportedOSs;
      {Bitmask describing the operating systems that this project can be
      installed on. If installer detects an operating system not included in
      this mask it will refuse to go ahead with installation}
    property CompressorID: string
      read fCompressorID write fCompressorID;
      {Identifier for preferred compressor to be used to build the project (if
      available)}
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UPathMacros, UFileProcs, UXMLFile, UFileNames;


const
  cSystemGroup = '[SYSTEM]';
    {Name of reserved system group UnInstall.exe is stored here if needed and it
    is included in project build}
  cExternalGroup = '[EXTERNAL]';
    {Name of reserved external group. UnInstall.xml, InstallLib.dll and
    are stored here only if we're we're provding an uninstaller. This group is
    never included in build: it's here for information purposes only}

resourcestring
  // Error messages
  sBadFileVer = 'File version %d not supported';
  sBadID = 'ID "%s" is not a valid identifier';
  sMissingRootKey = 'Registry root key "%s" is missing from project';
  sBadRegValue = 'Registry entry "%s" has bad value';


{ TProject }

procedure TProject.Build(const InfoXML: TXMLTag; const DataWriter: TDataWriter);
  {Builds the project ready for inclusion in an install program. Information
  describing the project is output in an XML tree with its root at the given XML
  tag. The project's files are passed to the given data writer object which in
  turn writes them to the install data file}
begin
  // Record install info file version
  InfoXML.ParamAsInt['version'] := cCurrentInstVersion;
  // Record relevant project properties as params of root tag
  InfoXML.Params['id'] := ID;
  InfoXML.Params['name'] := Name;
  InfoXML.Params['instpath'] := InstallPath;
  InfoXML.ParamAsInt['instpathedit'] := Ord(InstallPathEditing);
  InfoXML.ParamAsInt['uninstall'] := Ord(WantUnInstall);
  InfoXML.ParamAsInt['uninstprev'] := Ord(UninstallPrevious);
  InfoXML.ParamAsInt['ossupport'] := fSupportedOSs;
  // Save info about which projects to uninstall first
  fUninstallPrevNamed.SaveInstallXML(InfoXML);
  // Save un-installation info that has to come before files are deleted
  fRunProgs.SaveInstallXML(InfoXML, True);
  // Build the system group only if it has files in it
  if fSysGroup.Files.Count > 0 then
    fSysGroup.Build(InfoXML, DataWriter);
  // Write license files to install data file
  fLicenseInfo.Build(InfoXML, DataWriter);
  // Add info about install groups and copy their files to install data file
  fInstallGroups.Build(InfoXML, DataWriter);
  // Save installation info about registry
  fRegRootKeys.SaveInstallXML(InfoXML);
  // Save installation info that has to come after files installed
  fRunProgs.SaveInstallXML(InfoXML, False);
end;

procedure TProject.Clear;
  {Clears all of project to default state}
begin
  // Clear all fields to default values
  fID := '';
  fName := '';
  fInstallPath := '';
  fInstallProgName := CDefInstallPrg;
  fInstallPathEditing := ipeNone;
  WantUninstall := True;
  UninstallPrevious := True;
  fSupportedOSs := 0;
  fCompressorID := cDefaultCompressorID;
  // Clear owned objects
  fUninstallPrevNamed.Clear;
  fInstallGroups.Clear;
  fRegRootKeys.Clear;
  fRunProgs.Clear;
  fLicenseInfo.Reset;
end;

constructor TProject.Create;
  {Class constructor: creates owned objects and intialises fields to default
  values}
begin
  inherited Create;
  // Create owned ObjLists: all at root of their heirachies => owner is nil
  fInstallGroups := TGroupList.Create(nil);
  fRegRootKeys := TRegRootKeyList.Create(nil);
  fRunProgs := TRunProgList.Create(nil);
  // Create owned license info object
  fLicenseInfo := TLicenseInfo.Create;
  // Create owned uninstall named projects string list
  fUninstallPrevNamed := TUninstallPrevProjects.Create;
  // Assign event handler to InstallGroups object
  fInstallGroups.OnReport := ReportProgress;
  // Create private groups
  // group for special files added to project
  fSysGroup := TGroup.Create(cSystemGroup, PathMacros.Names[pnInstall]);
  fSysGroup.FileDeletion := fdInstalled; // delete only installed file
  fSysGroup.DirDeletion := ddNone;       // don't delete folder;
  // group for files installed that are not part of project: but are placed in
  // install folder
  fExternalGroup := TGroup.Create(cExternalGroup, PathMacros.Names[pnInstall]);
  // Reset to default condition
  Reset;
end;

destructor TProject.Destroy;
  {Class destructor: frees owned objects}
begin
  // Free owned objects
  fLicenseInfo.Free;
  fRunProgs.Free;
  fRegRootKeys.Free;
  fInstallGroups.Free;
  fUninstallPrevNamed.Free;
  inherited Destroy;
end;

procedure TProject.EnsureRootKeys;
  {Ensures a new project contains all required registry root keys}
begin
  // Ensure that all registry root keys are included in project
  if fRegRootKeys.FindByName('HKEY_CLASSES_ROOT') = nil then
    fRegRootKeys.Add(TRegRootKey.Create('HKEY_CLASSES_ROOT'));
  if fRegRootKeys.FindByName('HKEY_CURRENT_USER') = nil then
    fRegRootKeys.Add(TRegRootKey.Create('HKEY_CURRENT_USER'));
  if fRegRootKeys.FindByName('HKEY_LOCAL_MACHINE') = nil then
    fRegRootKeys.Add(TRegRootKey.Create('HKEY_LOCAL_MACHINE'));
  if fRegRootKeys.FindByName('HKEY_USERS') = nil then
    fRegRootKeys.Add(TRegRootKey.Create('HKEY_USERS'));
end;

procedure TProject.GetInstallFileNames(const NameList: TStrings;
  const IncFolder: Boolean = False);
  {Appends fully specified names of all files installed by this project to the
  given string list. Path name may contain path macros. If IncFolder is true
  then each group's folder name is included in the list. The string list's
  Objects[] property references the relevant file info object for files or group
  object for folders}
begin
  // System group files: only include if there are files
  if fSysGroup.Files.Count > 0 then
    fSysGroup.GetInstallFileNames(NameList, [], IncFolder);
  // Files installed directly from installer:
  // only include if there are files that will persist after installer finished
  if fExternalGroup.Files.Count > 0 then
    fExternalGroup.GetInstallFileNames(NameList, [], IncFolder);
  // License info files
  fLicenseInfo.GetInstallFileNames(NameList, IncFolder);
  // Now installation files
  fInstallGroups.GetInstallFileNames(NameList, [], IncFolder);
end;

function TProject.GetSourceFilePath: string;
  {Returns the path where system source files are stored. System install files
  are those provided by SIBuilder and are used by the installer}
begin
  Result := UFileProcs.MakePathName(
    ExtractFilePath(ParamStr(0)) + cInstallFolder
  );
end;

function TProject.GetWantUnInstall: Boolean;
  {Read access method for WantUninstall property: if this property is true then
  there will be a file info item for Uninstall program, so we test for this}
begin
  Result := Assigned(fUninstProg);
end;

class function TProject.IsReservedGroupName(const GroupName: string): Boolean;
  {Returns true if the given group name is a reserved for system use}
begin
  // There is one reserved group in project + some in license info property
  Result := (AnsiCompareText(GroupName, cSystemGroup) = 0)
    or (AnsiCompareText(GroupName, cExternalGroup) = 0)
    or TLicenseInfo.IsReservedGroupName(GroupName);
end;

procedure TProject.LoadProjectFromFile(const FileName: string);
  {Loads project from given (XML) file}
var
  FileStream: TFileStream;    // stream attached to file
begin
  // Open the file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // Load the project from the stream
    LoadProjectFromStream(FileStream);
  finally
    // Close the file
    FileStream.Free;
  end;
end;

procedure TProject.LoadProjectFromStream(const Stream: TStream);
  {Loads project from XML in given stream}
var
  XMLFile: TXMLFile;      // Object encapsulating XML file
begin
  // Create XML "file" object
  XMLFile := TXMLFile.Create(CPrjRootTag);
  try
    // Load XML file from given stream
    XMLFile.LoadFromStream(Stream);
    // Load project from XML root tag
    LoadProjectXML(XMLFile.RootTag);
  finally
    // Free "file" object
    XMLFile.Free;
  end;
  // Make sure all root keys are present in project
  EnsureRootKeys;
end;

procedure TProject.LoadProjectXML(const XMLTag: TXMLTag);
  {Loads project from given XML object}
var
  Version: Integer;     // version of XML file
  I: Integer;           // loops thru sub tags
  SubTag: TXMLTag;      // references a sub tag on given tag
begin
  // Check if we support project file version
  Version := XMLTag.ParamAsInt['version'];
  if (Version < cEarliestPrjVersion) or (Version > cCurrentPrjVersion) then
    raise EProject.CreateFmt(sBadFileVer, [Version]);
  // Clear the existing project
  Clear;
  // Read params
  ID := XMLTag.Params['id'];
  Name := XMLTag.Params['name'];
  InstallPath := XMLTag.Params['instpath'];
  InstallProgName := XMLTag.Params['instprog'];
  InstallPathediting := TInstallPathEditKinds(
    XMLTag.ParamAsInt['instpathedit']);
  WantUnInstall := Boolean(XMLTag.ParamAsInt['uninstall']);
  UninstallPrevious := Boolean(XMLTag.ParamAsInt['uninstprev']);
  fSupportedOSs := XMLTag.ParamAsInt['ossupport'];
  fCompressorID := XMLTag.Params['compressor'];
  // set default compression if not specified
  if fCompressorID = '' then
    fCompressorID := cDefaultCompressorID;
  // Read sub-tags, ignoring unrecognised ones
  for I := 0 to Pred(XMLTag.SubTagCount) do
  begin
    SubTag := XMLTag.SubTags[I];
    if SubTag.Tag = 'group' then
      fInstallGroups.AddFromXML(SubTag)
    else if SubTag.Tag = 'regrootkey' then
      fRegRootKeys.AddFromXML(SubTag)
    else if SubTag.Tag = 'run' then
      fRunProgs.AddFromXML(SubTag)
    else if SubTag.Tag = 'license' then
      fLicenseInfo.LoadProjectXML(SubTag)
    else if SubTag.Tag = 'uninstprev' then
      fUninstallPrevNamed.LoadProjectXML(SubTag);
  end;
end;

procedure TProject.ReportProgress(const Msg: string);
  {Event handler for InstallGroups object OnReport event - simply triggers this
  object's own OnProgress event and passes message on. Can also be used by
  derived classes to send messages by OnProgess event}
begin
  if Assigned(fOnProgress) then
    fOnProgress(Msg);
end;

procedure TProject.Reset;
  {Resets project - sets properties to their creation condition}
begin
  // Reset project to default values
  Clear;
  // Ensure that registry root keys are present in project
  EnsureRootKeys;
end;

procedure TProject.SaveProjectToFile(const FileName: string);
  {Saves project (as XML) to given file}
var
  FileStream: TFileStream;      // stream attached to file
begin
  // Create new file
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    // Save project to file
    SaveProjectToStream(FileStream);
  finally
    // Close file
    FileStream.Free;
  end;
end;

procedure TProject.SaveProjectToStream(const Stream: TStream);
  {Saves project in XML to given stream}
var
  XMLFile: TXMLFile;      // object encapsulating an XML file
begin
  // Create XML "file" object
  XMLFile := TXMLFile.Create(CPrjRootTag);
  try
    // Save XML tags
    SaveProjectXML(XMLFile.RootTag);
    // Save file to given stream
    XMLFile.SaveToStream(Stream);
  finally
    // Free XML file object
    XMLFile.Free;
  end;
end;

procedure TProject.SaveProjectXML(const XMLTag: TXMLTag);
  {Saves project into given XML object}
begin
  // Record version of project file
  XMLTag.ParamAsInt['version'] := CCurrentPrjVersion;
  // Store project properties
  XMLTag.Params['id'] := fID;
  XMLTag.Params['name'] := fName;
  XMLTag.Params['instpath'] := fInstallPath;
  XMLTag.Params['instprog'] := fInstallProgName;
  XMLTag.ParamAsInt['instpathedit'] := Ord(fInstallPathEditing);
  XMLTag.ParamAsInt['uninstall'] := Ord(WantUnInstall);
  XMLTag.ParamAsInt['uninstprev'] := Ord(UninstallPrevious);
  XMLTag.ParamAsInt['ossupport'] := fSupportedOSs;
  XMLTag.Params['compressor'] := fCompressorID;
  // Write sub-tags
  fUninstallPrevNamed.SaveProjectXML(XMLTag);
  fInstallGroups.SaveProjectXML(XMLTag);
  fRegRootKeys.SaveProjectXML(XMLTag);
  fRunProgs.SaveProjectXML(XMLTag);
  fLicenseInfo.SaveProjectXML(XMLTag);
end;

procedure TProject.SetID(const Value: string);
  {Write access method for ID property - checks that supplied value is a valid
  Pascal identifier}
begin
  // Check new value for validity and raise exception if not a Pascal identifier
  // or not ''
  if not IsValidIdent(Value) and (Value <> '') then
    raise EProject.CreateFmt(sBadID, [Value]);
  // Record new value
  fID := Value;
end;

procedure TProject.SetWantUnInstall(const Value: Boolean);
  {Write access method for WantUninstall property. If the value is true then
  this creates the required file info objects ands updates the appropriate
  groups. If the value is false then the file info objects are freed and removed
  from the appropriate groups}
begin
  if Value <> WantUninstall then
  begin
    if Value then
    begin
      // Create required info objects
      // uninstall program: this is included in project data
      fUninstProg := TFileInfo.Create(GetSourceFilePath + cUninstallPrg);
      fSysGroup.Files.Add(fUninstProg);
      // uninstall info file and install lib: included externally but only stay
      // on system after installation if uninstall program is present
      fUninstInfo := TFileInfo.Create(GetSourceFilePath + cUninstallInfo);
      fExternalGroup.Files.Add(fUninstInfo);
      fInstLibInfo := TFileInfo.Create(GetSourceFilePath + cInstallLib);
      fExternalGroup.Files.Add(fInstLibInfo);
    end
    else
    begin
      // Delete the info objects
      // NOTE: we could clear each group's files, but this make break future
      //   code that stores other files here that don't depend on WantUnInstall
      // from external group
      fExternalGroup.Files.DeleteObj(fUninstInfo);    // frees fUninstInfo
      fUninstInfo := nil;
      fExternalGroup.Files.DeleteObj(fInstLibInfo);   // frees fInstLibInfo
      fInstLibInfo := nil;
      // from system group
      fSysGroup.Files.DeleteObj(fUninstProg);         // frees fUninstProg
      fUninstProg := nil;
    end;
  end;
end;

procedure TProject.Validate;
  {Validates project integrity and raises exception if a problem is found.
  Returns with no exception if project is OK}

  // ---------------------------------------------------------------------------
  procedure TestRegKey(AKey: string);
    {Checks if given registry root key is present and raises exception if not}
  begin
    if fRegRootKeys.FindByName(AKey) = nil then
      // a registry root key is missing
      raise EProject.CreateFmtHelp(sMissingRootKey, [AKey], 0);
  end;

  procedure CreateTempFileList(out TempFiles: TStringList);
    {Create a string list containing references to all temporary files in
    installer: Objects[] property reference to associated file info object.
    Warning: the string list created by this routine must be freed by caller}
  var
    Idx: Integer; // loops thru all groups in project
  begin
    // Create the string list
    TempFiles := TStringList.Create;
    // Copy files from any temporary group into list
    for Idx := 0 to Pred(fInstallGroups.Count) do
    begin
      if InstallGroups.Items[Idx].FileDeletion = fdTemporary then
        InstallGroups.Items[Idx].GetInstallFileNames(TempFiles, []);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  TempFiles: TStringList; // list of temporary install files
begin
  // Check all registry root keys are present
  TestRegKey('HKEY_CLASSES_ROOT');
  TestRegKey('HKEY_CURRENT_USER');
  TestRegKey('HKEY_LOCAL_MACHINE');
  TestRegKey('HKEY_USERS');
  try
    // Validate the various owned objects
    // registry keys and data
    fRegRootKeys.Validate;
    // installed files and groups
    fInstallGroups.Validate(UninstallPrevious);
    // license information
    fLicenseInfo.Validate(UninstallPrevious);
    // run programs: need list of temp files to do this validation
    CreateTempFileList(TempFiles);
    try
      // do validation
      RunProgs.Validate(TempFiles, WantUnInstall);
    finally
      TempFiles.Free;
    end;
  except
    // Handle known exceptions from validator by raising EProject exceptions
    // based on exceptions from validators
    on E: ERegRootKeyList do
      // there's been an exception in registry root keys - re-raise
      raise EProject.CreateFmtHelp(sBadRegValue, [E.Message], E.HelpContext);
    on E: EGroupList do
      // There was a file missing - re-raise
      raise EProject.CreateHelp(E.Message, E.HelpContext);
    on E: ELicenseInfo do
      // There was an error: re-raise with same message
      raise EProject.CreateHelp(E.Message, E.HelpContext);
    on E: ERunProgList do
      // There was an error: re-raise with same message
      raise EProject.CreateHelp(E.Message, E.HelpContext);
  end;
end;

end.
