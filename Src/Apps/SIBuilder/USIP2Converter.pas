{ ##                 
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     USIP2Converter.pas
  @COMMENTS                 Implements a class that reads a v2 project file and
                            converts it into v3 project file, with new file
                            adjusted as necessary to account for any obsolete
                            features from the v2 file. Notes the changes made
                            and reports to caller.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 20/02/2008
      @COMMENTS             Replaced usage of FileNames.inc and Help.inc include
                            files with UFileNames and UHelpContexts units.
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
 * The Original Code is USIP2Converter.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USIP2Converter;


interface


uses
  // Delphi
  Classes,
  // Project
  UXMLFile, UXMLTag, UProject;

type

  {
  TSIP2Converter:
    Class that reads a v2 project file and converts it into a v3 project file
    with new file adjusted as necessary to account for any obsolete features
    from the v2 file. Notes of changes made and associated help contexts are
    reported to caller via a string list object.

    TSIP2Converter -> [TObject]
  }
  TSIP2Converter = class(TObject)
  private
    fOldFile: string;
      {Name of v2 file being read}
    fNewFile: string;
      {Name of v3 file being created}
    fComments: TStrings;
      {List stores comments about adjustments that have been made to project to
      allow for compatibility differences between the two file formats. Any
      help contexts for topics that provide additional info are stored in
      Objects[] property, cast from integer to pointer}
    fXMLFile2: TXMLFile;
      {Stores a representation of the XML from the v2 file: used to identifiy
      any obsolete tags and attributes that need to be adjusted in v3 project}
    fProject3: TProject;
      {v3 SIBuilder project object: used to read initial values and to
      manipulate to make adjustments}
    fInstallFiles: TStringList;
      {List of user supplied files to be installed by project: used to check
      presence of certain files as part of manipulation}
    fTempProgGroupID: Integer;
      {ID of next temporary group created for programs that are to be deleted.
      Different groups will be created for each path a deletable program is
      installed onto}
    procedure ReadXMLFile;
      {Reads v2 XML file into XML file object}
    procedure ReadProjectFile;
      {Reads v3 project from v2 file and records list files installed by
      project}
    procedure ProcessTags;
      {Scans all tags in v2 XML file and processes those which are absolete or
      have obsolete attributes, updating v3 project where possible}
    procedure ProcessCOMTag(Tag: TXMLTag);
      {Processes the obsolete v2 COM servers tag and updates v3 project files
      object with supported registration details and discards unsupport external
      servers registration}
    procedure ProcessLicenseTag(Tag: TXMLTag);
      {Processes v2 license tag. This has been greatly simplified in v3, so
      additional group and file objects are created to replicate functions from
      v2 file as far as possible}
    procedure ProcessRunTag(Tag: TXMLTag);
      {Processes v2 run tag. The deletion attribute has been removed in v3 and
      ability for installer to delete files has been added to file groups. So,
      each flagged program with a deletion attribute that is included in project
      is moved from its existing group to a newly created group with the
      deletion attribute set. Deletion attributes for external programs are
      simply ignored}
    procedure SaveProjectFile;
      {Saves processed v3 project to disk}
  public
    constructor Create(const OldFile, NewFile: string;
      const Comments: TStrings);
      {Class constructor: records reference to object passed as parameters and
      creates owned objects. OldFile is the name of the file containing the
      project to be covertered and NewFile is the name to be used for the
      converted file. Comments about modifications made to the project are
      stored in the Comments list, along with help contexts relating to changes
      (in Objects[] property}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    function Convert: Boolean;
      {Performs conversion of file passed to constructor, writes new output
      file, and records comments and related help contexts in Comments list
      passed to constructor. Returns true if conversion completes successfully,
      false if not}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UFileProcs, UHelpContexts, UCommonTypes, UGroups, UFiles, UPathMacros,
  UFileNames;


resourcestring
  // Comments
  sIgnoredCOMRegOnly    = 'COM server "register only" property ignored for %s.';
  sIgnoredExternalCOM   = 'Registration of COM server "%s" ignored. The file '
                          + 'is not part of the installation project.';
  sLicFileAdded         = 'License file "%0:s" was added to the newly created '
                          + '"%1:s" group.';
  sLicDLLAdded          = 'License dialog DLL "%0:s" has been added to the '
                          + 'newly created "%1:s" group.';
  sLicDLLRemoved        = 'License dialog DLL "%0:s" will be deleted by the '
                          + 'installer after displaying the licence file.';
  sLicDLLRetained       = 'License dialog DLL "%0:s" has been added to the '
                          + 'newly created "%1:s" group. The DLL will be '
                          + 'installed into [Windows]\PJSoft and will not be '
                          + 'removed by the uninstaller. Other, more '
                          + 'preferable, alternatives are available using '
                          + 'SIBuilder 3.';
  sLicIsCOMSvr          = 'License dialog DLL "%0:s" in the newly created '
                          + '"%1:s" group is to be registered as a COM server '
                          + 'by the installer.';
  sRunProgAltered       = 'Program "%0:s" that is to be run and then deleted '
                          + 'by the installer has been moved from the "%1:s" '
                          + 'group to the newly created "%2:s" group which has '
                          + 'been set to be deleted by the installer.';
  sRunProgDeleteIgnored = 'External program "%s" that is to be run by the '
                          + 'installer has had its "Deletion" property '
                          + 'ignored.';

const
  // Names of groups that may be added to project by this converter
  cCustGroupLeader = 'SIP2 CONVERTER: ';
  cLicDLLGroup = cCustGroupLeader + 'License DLL';
  cLicFileGroup = cCustGroupLeader + 'License File';
  cTempProgGroup = cCustGroupLeader + 'Temp Prog';


{ TSIP2Converter }

function TSIP2Converter.Convert: Boolean;
  {Performs conversion of file passed to constructor, writes new output file,
  and records comments and related help contexts in Comments list passed to
  constructor. Returns true if conversion completes successfully, false if not}
begin
  // Assume success
  Result := True;
  try
    // Read the XML from v2 file and also read new style project from it
    // (note v3 projects are syntax compatible with v2 file: just some of
    // semantics change and some attributes of v2 files are ignored by XML
    // reading code in v3 project - it's these items we need to abjust for
    ReadXMLFile;
    ReadProjectFile;
    // Process the tags in the v2 file, adjusting for unsupported attributes
    ProcessTags;
    // Save version 3 project file
    SaveProjectFile;
  except
    // We have error: report it in comments and return false
    on E: Exception do
    begin
      Result := False;
      fComments.AddObject(E.Message, Pointer(E.HelpContext));
    end;
  end;
end;

constructor TSIP2Converter.Create(const OldFile, NewFile: string;
  const Comments: TStrings);
  {Class constructor: records reference to object passed as parameters and
  creates owned objects. OldFile is the name of the file containing the project
  to be covertered and NewFile is the name to be used for the converted file.
  Comments about modifications made to the project are stored in the Comments
  list, along with help contexts relating to changes (in Objects[] property}
begin
  inherited Create;
  // Record reference to parameters
  fOldFile := OldFile;
  fNewFile := NewFile;
  fComments := Comments;
  // Create owned objects
  fProject3 := TProject.Create;               // new project
  fXMLFile2 := TXMLFile.Create(cPrjRootTag);  // XML in original file
  fInstallFiles := TStringList.Create;        // list of files in project
  // Set ID of next temporary group created for programs that are to be deleted
  fTempProgGroupID := 1;
end;

destructor TSIP2Converter.Destroy;
  {Class destructor: frees owned objects}
begin
  fInstallFiles.Free;
  fXMLFile2.Free;
  fProject3.Free;
  inherited;
end;

procedure TSIP2Converter.ProcessCOMTag(Tag: TXMLTag);
  {Processes the obsolete v2 COM servers tag and updates v3 project files object
  with supported registration details and discards unsupport external servers
  registration}
var
  COMInstPath: string;      // install path for COM server
  COMFileIdx: Integer;      // index of COM server in install file list
  COMFileInfo: TFileInfo;   // file info object relating to COM server
begin
  // Check if referenced COM server is in the project
  // (SIBuilder 3 does not support registration of "external" COM servers)
  COMInstPath := Tag.PlainText;
  COMFileIdx := fInstallFiles.IndexOf(COMInstPath);
  if COMFileIdx >= 0 then
  begin
    // Server is part on installation
    // get hold of file info object and set its RegisterCOMDLL property
    COMFileInfo := fInstallFiles.Objects[COMFileIdx] as TFileInfo;
    COMFileInfo.RegisterCOMDLL := True;
    if Boolean(Tag.ParamAsInt['regonly']) then
      // in v2 this file was not to be unregistered on deletion:
      // we don't support this feature now!
      fComments.AddObject(
        Format(sIgnoredCOMRegOnly, [COMFileInfo.InstallPath]),
        Pointer(IDH_SIP2_IGNOREREGONLY)
      );
  end
  else
    // COM server not in this project so we ignore it
    fComments.AddObject(
      Format(sIgnoredExternalCOM, [COMInstPath]),
      Pointer(IDH_SIP2_IGNOREINPROCSVR)
    );
end;

procedure TSIP2Converter.ProcessLicenseTag(Tag: TXMLTag);
  {Processes v2 license tag. This has been greatly simplified in v3, so
  additional group and file objects are created to replicate functions from v2
  file as far as possible}
var
  LicFileGroup: TGroup;   // group used to install license file
  LicFileObj: TFileInfo;  // license file object associated with above group
  LicFileIdx: Integer;    // index of license file in installation
  LicInstDir: string;     // license file installation directory
  LicSrcFile: string;     // license file source file spec
  LicInstFile: string;    // license file installation file spec
  DLLGroup: TGroup;       // group used to install license DLL
  DLLFile: TFileInfo;     // DLL file object associated with above group
begin
  // Check if license is active: nothing to do if not
  if Boolean(Tag.ParamAsInt['active']) then
  begin
    // Check 'keepfile' parameter: unused in v3
    {
      when 'keepfile' param was 0 the license file was deleted after being
      shown by installer => same as new license behaviour in v3
      when 'keepfile' parm was 1 the license file was retained and deleted
      by uninstaller => we need a group for required install path to install
      license file and delete it with installer
    }
    if Boolean(Tag.ParamAsInt['keepfile']) then
    begin
      // We need to install license file flagged for deletion by installer
      // gather location information
      LicSrcFile := Tag.PlainText;
      LicInstDir := Tag.Params['instpath'];
      LicInstFile := MakePathName(LicInstDir) + ExtractFileName(LicSrcFile);
      // check if license file already in install project: nothing to do if so
      LicFileIdx := fInstallFiles.IndexOf(LicInstFile);
      if LicFileIdx = -1 then
      begin
        // license file not in installation: create a group to install it in
        LicFileGroup := TGroup.Create(cLicFileGroup, LicInstDir);
        LicFileGroup.FileDeletion := fdInstalled;  // delete only installed file
        LicFileGroup.DirDeletion := ddIfEmpty;     // delete folder if empty
        // create a file object for file and add to group
        LicFileObj := TFileInfo.Create(LicSrcFile);
        LicFileGroup.Files.Add(LicFileObj);
        // add new group to project
        fProject3.InstallGroups.Add(LicFileGroup);
        // report what we've done
        fComments.AddObject(
          Format(sLicFileAdded, [LicSrcFile, cLicFileGroup]),
          Pointer(IDH_SIP2_LICFILEADDED)
        );
      end;
    end;
    // Check 'dllopts' parameter
    {
      'dllopts' had three possible values:
        0:  license dialog was installed in [Install Path] and deleted by
            uninstaller - we do exactly this in this case if the license file
            was retained, but don't install the DLL if the license file is not
            retained.
        1:  license dialog was installed in [Windows]\PJSoft and never deleted
            by uninstaller - we do exactly this but comment that new location
            should be [Common Files]\delphiDabbler and DLL should be flagged as
            shared
        2:  license dialog was installed in [Windows]\PJSoft, registered as COM
            server and never deleted by uninstaller - we do exactly this but
            comment that new location should be [Common Files]\delphiDabbler
            and DLL should be shared - DLL is flagged as requiring registration
            as COM server in file info object
      Note from above that v2 installer always keeps license dialog until at
      least the uninstaller has run - so we need to emulate this.
    }
    // Assume DLL not installed
    DLLGroup := nil;
    case Tag.ParamAsInt['dllopts'] of
      0:
      begin
        // DLL was deleted by uninstaller: we install DLL, but only if user kept
        // license file, since this is a reasonable indication of if DLL is used
        // by program being installed. In this case DLL was never registered as
        // a COM server
        if Boolean(Tag.ParamAsInt['keepfile']) then
        begin
          // we install DLL: create a group for it that's deleted by uninstaller
          DLLGroup := TGroup.Create(
            cLicDLLGroup, PathMacros.Names[pnInstall]
          );
          DLLGroup.FileDeletion := fdInstalled;
          DLLGroup.DirDeletion := ddIfEmpty;
          // say what we've done
          fComments.AddObject(
            Format(sLicDLLAdded, [cLicenseDlgLib, cLicDLLGroup]),
            Pointer(IDH_SIP2_LICDLLADDED)
          );
        end
        else
          // user is not keeping license file, so we don't install DLL: say so
          fComments.AddObject(
            Format(sLicDLLRemoved, [cLicenseDlgLib]),
            Pointer(IDH_SIP2_LICDLLREMOVED)
          );
      end;
      else
      begin
        // DLL was not deleted by unsinstaller: we definately need to install it
        // create group for DLL that is not deleted by uninstaller - install in
        // [Windows]/PJSoft
        DLLGroup := TGroup.Create(
          cLicDLLGroup,
          MakePathName(PathMacros.Names[pnWindows]) + 'PJSoft'
        );
        DLLGroup.FileDeletion := fdNone;
        DLLGroup.DirDeletion := ddNone;
        // say what we've done
        fComments.AddObject(
          Format(sLicDLLRetained, [cLicenseDlgLib, cLicDLLGroup]),
          Pointer(IDH_SIP2_LICDLLRETAINED)
        );
      end;
    end;
    // If we are installing DLL then DLL group has been create
    if Assigned(DLLGroup) then
    begin
      // we have a DLL group, so create a file object used to install DLL
      DLLFile := TFileInfo.Create(
        MakePathName(ExtractFilePath(ParamStr(0)) + cInstallFolder)
          + cLicenseDlgLib
      );
      DLLFile.OverwriteAction := foaIfOlder;
      // determine if DLL needs to be registered as a COM server: we now use
      // file object to do this
      DLLFile.RegisterCOMDLL := Tag.ParamAsInt['dllopts'] = 2;
      if DLLFile.RegisterCOMDLL then
        // add extra comment if this is happening
        fComments.AddObject(
          Format(sLicIsCOMSvr, [cLicenseDlgLib, cLicDLLGroup]),
          Pointer(IDH_SIP2_LICDLLREGASCOM)
        );
      // add file object to group and group to project
      DLLGroup.Files.Add(DLLFile);
      fProject3.InstallGroups.Add(DLLGroup);
    end;
  end;
end;

procedure TSIP2Converter.ProcessRunTag(Tag: TXMLTag);
  {Processes v2 run tag. The deletion attribute has been removed in v3 and
  ability for installer to delete files has been added to file groups. So,
  each flagged program with a deletion attribute that is included in project
  is moved from its existing group to a newly created group with the
  deletion attribute set. Deletion attributes for external programs are
  simply ignored}

  // ---------------------------------------------------------------------------
  function TempGroupFromPath(const Path: string; OSOptions: LongWord): TGroup;
    {Searches project for a temporary group that will install into given path
    with given OSOptions. If no such group is found one is created and added to
    project. A reference to the group is returned}
  var
    Idx: Integer;       // loops through groups in project
    Group: TGroup;      // reference to a group
    GroupName: string;  // name of a new group
  begin
    // Assume group doesn't already exsist
    Result := nil;
    // Loop thru all groups, searching for a match
    for Idx := 0 to Pred(fProject3.InstallGroups.Count) do
    begin
      // Check if group is deletable, and has same path and OS
      Group := fProject3.InstallGroups.Items[Idx];
      if (Group.FileDeletion = fdTemporary) and
        (AnsiCompareText(Group.Path, Path) = 0) and
        (Group.OSOptions = OSOptions) then
      begin
        // found the group: look no further
        Result := Group;
        Break;
      end;
    end;
    // If we've not found group we create it
    if not Assigned(Result) then
    begin
      // create the group with unique ID and required OSOptions
      GroupName := cTempProgGroup;
      if fTempProgGroupID > 1 then
        GroupName := Format('%s (%d)', [GroupName, fTempProgGroupID]);
      Result := TGroup.Create(GroupName, Path);
      Result.OSOptions := OSOptions;
      // make group temporary
      Result.FileDeletion := fdTemporary;
      Result.DirDeletion := ddIfEmpty;
      // add it to project
      fProject3.InstallGroups.Add(Result);
      // increment ID to ensure next group is unique
      Inc(fTempProgGroupID);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  InstPath: string;   // install path for program
  InstProg: string;   // fully specified install file name for program
  InstIdx: Integer;   // index of program in install files (-1=>not in project)
  RunFile: TFileInfo; // reference to program's install file object
  RunGroup: TGroup;   // reference to program's install group
  TempGroup: TGroup;  // reference to temp group for program
begin
  // Check if the program is run on installation and has delete flag set
  if Boolean(Tag.ParamAsInt['delete']) and (Tag.ParamAsInt['whenrun'] = 0) then
  begin
    // Program has delete flag: we need to handle this
    // get install path and install file spec of program
    InstPath := Tag.Params['path'];
    InstProg := UFileProcs.MakePathName(InstPath) + Trim(Tag.PlainText);
    // Check if program is part of installation by looking up in file manifest
    InstIdx := fInstallFiles.IndexOf(InstProg);
    if InstIdx >= 0 then
    begin
      // Program is in install path: create a special deletion group
      // get program's file object and group it currently belongs to
      RunFile := fInstallFiles.Objects[InstIdx] as TFileInfo;
      RunGroup := (RunFile.GetGroup) as TGroup;
      // get temporary group to place it in
      TempGroup := TempGroupFromPath(InstPath, RunGroup.OSOptions);
      // don't register anything: the file is temporary
      RunFile.RegisterAppPath := False;
      RunFile.RegisterSharedDLL := False;
      RunFile.RegisterCOMDLL := False;
      // move program file from its original group to temporary one
      RunGroup.Files.RemoveObj(RunFile);
      TempGroup.Files.Add(RunFile);
      // Say what we've done
      fComments.AddObject(
        Format(sRunProgAltered, [InstProg, RunGroup.Name, TempGroup.Name]),
        Pointer(IDH_SIP2_RUNPROGMOVED)
      );
    end
    else
      // Program is not in install path: delete not permitted so say so
      fComments.AddObject(
        Format(sRunProgDeleteIgnored, [InstProg]),
        Pointer(IDH_SIP2_RUNPROGDELIGNORED)
      );
  end;
end;

procedure TSIP2Converter.ProcessTags;
  {Scans all tags in v2 XML file and processes those which are absolete or
  have obsolete attributes, updating v3 project where possible}
var
  Idx: Integer;   // loops thru all main subtags in project
  Tag: TXMLTag;   // refers to a particular tag object
begin
  // Examine each top level sub tag
  for Idx := 0 to Pred(fXMLFile2.RootTag.SubTagCount) do
  begin
    Tag := fXMLFile2.RootTag.SubTags[Idx];
    if AnsiCompareText(Tag.Tag, 'comsvr') = 0 then
      // obsolete COM server tag: COM registration info in files tag in v3
      ProcessCOMTag(Tag)
    else if AnsiCompareText(Tag.Tag, 'license') = 0 then
      // license tag: this has less attributes in v3
      ProcessLicenseTag(Tag)
    else if AnsiCompareText(Tag.Tag, 'run') = 0 then
      // run programs tag: delete attribute not supported in v3
      ProcessRunTag(Tag);
  end;
end;

procedure TSIP2Converter.ReadProjectFile;
  {Reads v3 project from v2 file and records list files installed by project}
begin
  // V3 project objects can read v2 file: obsolete constructs are ignored
  fProject3.LoadProjectFromFile(fOldFile);
  // List of files installed are just those sepcified by user:
  // files that will be included by SIBuilder are ignored here
  fProject3.InstallGroups.GetInstallFileNames(fInstallFiles, []);
end;

procedure TSIP2Converter.ReadXMLFile;
  {Reads v2 XML file into XML file object}
var
  InStm: TStream; // read only stream onto XML file
begin
  InStm := TFileStream.Create(fOldFile, fmOpenRead or fmShareDenyNone);
  try
    fXMLFile2.LoadFromStream(InStm);
  finally
    InStm.Free;
  end;
end;

procedure TSIP2Converter.SaveProjectFile;
  {Saves processed v3 project to disk}
begin
  fProject3.SaveProjectToFile(fNewFile);
end;

end.
