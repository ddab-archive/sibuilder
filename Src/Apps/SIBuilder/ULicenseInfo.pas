{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     ULicenseInfo.pas
  @COMMENTS                 Defines a class that maintains information about how
                            license file is displayed and installed, reads and
                            writes from project file and assists with building
                            installation project.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Major revision.
                            + Rewrote way in which groups and file objects are
                              used to provide list of installed files and to
                              build project.
                            + Fixed bug in GetInstallFileNames that was
                              returning reference to freed object.
                            + No longer provides ability to keep license file
                              and dialog box for retention by user. The DLL and
                              file are now installed to temp folder and deleted
                              after use. Therefore the DLLInstallOptions,
                              KeepFile and InstallPath properties were removed.
                            + No loger registers license DLL as COM server.
                            + Modified Build method to use a TDataWriter object
                              to write files into install data file.
                            + Fixed bug that was not reseting Active property to
                              false when license was reset.
                            + Source file property is now implemented via the
                              SourceFileSpec property of license file info
                              object.
                            + Instead of creating two install groups: one for
                              the file and one for the dll, now only one private
                              group is required.
                            + Added a Validation method that raises exceptions
                              if license info not valid
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 24/01/2003
      @COMMENTS             Fixed bug introduced in v2.0 that was validating
                            licenses even when license not required and raising
                            un-necessary exception when license file missing!
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused units and
                            moved some units from interface to implementation.
    )
    @REVISION(
      @VERSION              2.4
      @DATE                 15/01/2006
      @COMMENTS             Changed to install license DLL and data file to new
                            [ScratchPad] folder rather than [Temp].
    )
    @REVISION(
      @VERSION              2.5
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of FileNames.inc include file with
                              UFileNames unit.
                            + Replaced string literals with resource strings.
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
 * The Original Code is ULicenseInfo.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit ULicenseInfo;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UXMLObjects, UXMLTag, UFiles, UGroups, UDataWriter;

type

  {
  TLicenseDlgSize:
    Supported dialog box sizes.
  }
  TLicenseDlgSize = (ldsSmall, ldsMedium, ldsLarge);

  {
  TLicenseInfo:
    Class that maintains information about how license file is displayed and
    installed, reads and writes from project file and assists with building
    installation project.

    Inheritance: TLicenseInfo -> TXMLObjectItem -> TObjListItem -> [TObject]
  }
  TLicenseInfo = class(TXMLObjectItem)
  private // properties
    fMustAccept: Boolean;
    fActive: Boolean;
    fDlgSize: TLicenseDlgSize;
    function GetSourceFile: string;
    procedure SetSourceFile(const Value: string);
  private
    fLicGroup: TGroup;
      {Installation group used to add required files to installation data and
      script}
    fLicFileInfo: TFileInfo;
      {Reference to license file info object that is stored in installation
      group: used to record source file name for license file and to get install
      path to license file}
    fDLLFileInfo: TFileInfo;
      {Reference to license DLL file info object that is stored in installation
      group: used to get install path to license DLL}
    function GetLicenseInstallPath: string;
      {Returns the installation for the install file and DLL: the Windows temp
      folder}
    function GetDLLSourceFileSpec: string;
      {Returns the source file spec for the license DLL: this is in Install sub-
      folder of the SIBuilder installation}
  public
    constructor Create;
      {Class constructor: creates install file group and file objects and sets
      default values}
    destructor Destroy; override;
      {Class destructor: owned objects}
    procedure Reset;
      {Reset values to defaults}
    procedure Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
      {Stores required files in given data stream using the given data writer
      object and writes install info as a sub tag of given XML tag}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves object properties to given XML object}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Loads object's properties from given XML object}
    procedure GetInstallFileNames(const NameList: TStrings;
      const IncFolder: Boolean = False);
      {Appends fully specified names of files installed to support license info
      (which may contain path macros) to given strings object. The string list's
      Objects[] property references the relevant file info object for files or
      group object for folders}
    procedure Validate(UninstallPrevious: Boolean);
      {Validates license info and raises an exception if there is a problem.
      UninstallPrevious is required in validation and is true if installer should
      uninstall previous installations in install folder}
    class function IsReservedGroupName(const GroupName: string): Boolean;
      {Returns true if the given group name is a reserved for use by this class}
    property Active: Boolean
      read fActive write fActive;
      {Property is true if installer is to display license, and false if not}
    property SourceFile: string
      read GetSourceFile write SetSourceFile;
      {The source path to the file containing the license}
    property MustAccept: Boolean
      read fMustAccept write fMustAccept;
      {When this flag is true the license dlg displays both Accept and Decline
      buttons and user must accept to continue with install. When false only a
      Close button is displayed and no check is made for acceptance}
    property DlgSize: TLicenseDlgSize
      read fDlgSize write fDlgSize;
      {The size of the license dlg: small, medium or large}
    class function GetDlgHeight(const Size: TLicenseDlgSize): Integer;
      {Returns the height in pixels for a dlg box of given size code}
    class function GetDlgWidth(const Size: TLicenseDlgSize): Integer;
      {Returns the width in pixels for a dlg box of given size code}
  end;

  {
  ELicenseInfo:
    Class of exception raised by TLicenseInfo object.

    Inheritance: ELicenseInfo -> [Exception]
  }
  ELicenseInfo = class(Exception);


implementation


uses
  // Project
  UCommonTypes, UPathMacros, UFileProcs, UFileNames;


const
  // Special system groups names for installing license COM server and files
  cGroupName = '[LICENSE]';


resourcestring
  // Error messages
  sErrNoLicenseFile = 'No license file specified';
  sErrMissingLicenseFile = 'License file "%s" does not exist';


{ TLicenseInfo }

procedure TLicenseInfo.Build(const ParentTag: TXMLTag;
  const DataWriter: TDataWriter);
  {Stores required files in given data stream using the given data writer object
  and writes install info as a sub tag of given XML tag}
var
  SubTag: TXMLTag;        // sub tag where install info is recorded
begin
  // Do nothing if we're not to display a license
  if not fActive then
    Exit;
  // Build the license group: this must preceed license info in info file
  fLicGroup.Build(ParentTag, DataWriter);
  // Write out the required code to the project info XML file
  SubTag := ParentTag.AddSubTag('license');
  SubTag.PlainText := fLicFileInfo.InstallPath;
  SubTag.Params['dll'] := fDLLFileInfo.InstallPath;
  SubTag.ParamAsInt['accept'] := Ord(fMustAccept);
  SubTag.ParamAsInt['width'] := GetDlgWidth(fDlgSize);
  SubTag.ParamAsInt['height'] := GetDlgHeight(fDlgSize);
end;

constructor TLicenseInfo.Create;
  {Class constructor: creates install file group and file objects and sets
  default values}
begin
  inherited Create;
  // Create license install group
  fLicGroup := TGroup.Create(cGroupName, GetLicenseInstallPath);
  fLicGroup.FileDeletion := fdTemporary;
  fLicGroup.DirDeletion := ddNone;
  fDLLFileInfo := TFileInfo.Create(GetDLLSourceFileSpec);
  fLicGroup.Files.Add(fDLLFileInfo);
  fLicFileInfo := nil;
  Reset;
end;

destructor TLicenseInfo.Destroy;
  {Class destructor: owned objects}
begin
  fLicGroup.Free; // this frees the license and DLL file info objects
  inherited;
end;

class function TLicenseInfo.GetDlgHeight(const Size: TLicenseDlgSize): Integer;
  {Returns the height in pixels for a dlg box of given size code}
const
  cHeights: array[TLicenseDlgSize] of Integer = (240, 320, 380);
    {actual heights that relate to size codes}
begin
  Result := cHeights[Size];
end;

class function TLicenseInfo.GetDlgWidth(const Size: TLicenseDlgSize): Integer;
  {Returns the width in pixels for a dlg box of given size code}
const
  cWidths: array[TLicenseDlgSize] of Integer = (300, 420, 540);
    {actual widths that relate to size codes}
begin
  Result := cWidths[Size];
end;

function TLicenseInfo.GetDLLSourceFileSpec: string;
  {Returns the source file spec for the license DLL: this is in Install sub-
  folder of the SIBuilder installation}
begin
  Result := MakePathName(ExtractFilePath(ParamStr(0)) + cInstallFolder)
    + cLicenseDlgLib;
end;

procedure TLicenseInfo.GetInstallFileNames(const NameList: TStrings;
  const IncFolder: Boolean = False);
  {Appends fully specified names of files installed to support license info
  (which may contain path macros) to given strings object. The string list's
  Objects[] property references the relevant file info object for files or group
  object for folders}
begin
  // If installer is not to display license, there are no files
  if not fActive then
    Exit;
  // We need to add the installed files to the list
  fLicGroup.GetInstallFileNames(NameList, [], IncFolder);
end;

function TLicenseInfo.GetLicenseInstallPath: string;
  {Returns the installation for the install file and DLL: the Windows temp
  folder}
begin
  Result := PathMacros.Names[pnScratchPad];
end;

function TLicenseInfo.GetSourceFile: string;
  {Read access method for SourceFile property: returns SourceFileSpec property
  of license file's info object, or '' if there is no info object}
begin
  if Assigned(fLicFileInfo) then
    Result := fLicFileInfo.SourceFileSpec
  else
    Result := '';
end;

class function TLicenseInfo.IsReservedGroupName(
  const GroupName: string): Boolean;
  {Returns true if the given group name is a reserved for use by this class}
begin
  Result := (CompareText(GroupName, cGroupName) = 0)
end;

procedure TLicenseInfo.LoadProjectXML(const XMLTag: TXMLTag);
  {Loads object's properties from given XML object}
begin
  SetSourceFile(XMLTag.PlainText);
  fMustAccept := Boolean(XMLTag.ParamAsInt['accept']);
  fActive := Boolean(XMLTag.ParamAsInt['active']);
  fDlgSize := TLicenseDlgSize(XMLTag.ParamAsInt['size']);
end;

procedure TLicenseInfo.Reset;
  {Reset values to defaults}
begin
  SetSourceFile('');
  fMustAccept := True;
  fDlgSize := ldsMedium;
  fActive := False;
end;

procedure TLicenseInfo.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves object properties to given XML object}
var
  SubTag: TXMLTag;  // sub tag where license info is stored
begin
  SubTag := ParentTag.AddSubTag('license');
  SubTag.PlainText := GetSourceFile;
  SubTag.ParamAsInt['active'] := Ord(fActive);
  SubTag.ParamAsInt['accept'] := Ord(fMustAccept);
  SubTag.ParamAsInt['size'] := Ord(fDlgSize);
end;

procedure TLicenseInfo.SetSourceFile(const Value: string);
  {Write access method for SourceFile property: stores value in license file
  info object's SourceFileSpec property unless Value is '' in which case the
  info object is freed}
begin
  if Assigned(fLicFileInfo) then
  begin
    // We have an existing info file object: free it and remove from group
    fLicGroup.Files.DeleteObj(fLicFileInfo);    // frees fLicFileInfo
    fLicFileInfo := nil;
  end;
  if Value <> '' then
  begin
    // We have actual value: create info object for it and add to list
    // NOTE: the file info SourceFileSpec property is read only, so we have to
    // create a new object with the new source file spec
    fLicFileInfo := TFileInfo.Create(Value);
    fLicGroup.Files.Add(fLicFileInfo);
  end;
end;

procedure TLicenseInfo.Validate(UninstallPrevious: Boolean);
  {Validates license info and raises an exception if there is a problem.
  UninstallPrevious is required in validation and is true if installer should
  uninstall previous installations in install folder}
begin
  // License is always valid if not active!
  if Active then
  begin
    // Check if a license file has been specified
    if GetSourceFile = '' then
      raise ELicenseInfo.Create(sErrNoLicenseFile);
    // Validate license file, returning error if there's a problem
    try
      fLicFileInfo.Validate(UninstallPrevious);
    except
      on E: EFileInfo do
        raise ELicenseInfo.CreateFmt(sErrMissingLicenseFile, [GetSourceFile]);
    end;
    // Validate DLL file, returning error if there's a problem
    try
      fDLLFileInfo.Validate(UninstallPrevious);
    except
      on E: EFileInfo do
        raise ELicenseInfo.CreateHelp(E.Message, E.HelpContext);
    end;
  end;
end;

end.
