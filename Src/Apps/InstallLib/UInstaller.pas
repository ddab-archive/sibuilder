{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstaller.pas
  @COMMENTS                 This unit is unique to the InstallLib.dll
                            sub-project. It contains classes that perform
                            installation and uninstallation using relevant data
                            and information files.
  @DEPENDENCIES             For some installation options the following SITools
                            support library is required:
                            + SITLicenseDlg.dll 1.0 or later
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 04/09/2000
      @COMMENTS             The class has been totally rewritten as a direct
                            TObject descedant containing all the required
                            install and uninstall logic.\
                            TInstaller now provides for both installation and
                            uninstallation and uses physical files to control
                            behaviour rather than resource streams.\
                            The following additional functionality is provided
                            by this revision:
                            + ability to run external programs
                            + calling application can now be interactively asked
                              to provide an install path.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 20/12/2000
      @COMMENTS             Modified call to UFileProc.ExecAndWait procedure to
                            account for change in parameter list and return
                            value.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 24/12/2000
      @COMMENTS             + Modifed registry data installation code to
                              optionally expand path macros in string data.
                            + Replaced all string literals used for output with
                              resource strings.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 28/12/2000
      @COMMENTS             + Added support for registration/unregistration of
                              in-process COM server dlls.
                            + Added facility to run selected programs on
                              installation and others on uninstallation.
    )
    @REVISION(
      @VERSION              2.4
      @DATE                 06/01/2001
      @COMMENTS             Changed method used to register/unregister COM
                            server DLLsto use v2 of RegCOMSvrs.dll.
    )
    @REVISION(
      @VERSION              2.5
      @DATE                 21/01/2001
      @COMMENTS             + Added ability to display license in dialog box as
                              part of installation and to optionally abort
                              installation if user fails to accept license.
                            + Also fixed a bug that meant that RegCOMSvr.dll
                              could not be found when registering/unregistering
                              in-process COM servers.
    )
    @REVISION(
      @VERSION              2.6
      @DATE                 28/06/2001
      @COMMENTS             Made TInstaller.Install and InstallProject methods
                            update install path parameter with install path
                            actually used.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             + Split out Install and UnInstall code into
                              different classes with common base class rather
                              than have install and uninstall code in same class
                              as before.
                            + Added code to register/unregister files as
                              applications with Windows.
                            + Added support for registering/unregistering (i.e.
                              usage counting) and deleting shared DLLs.
                            + Added extra progress output procedures to use
                              various different parameter lists to simplify
                              calling.
                            + Added code to support file version info as means
                              of deciding if to overwrite files.
                            + Added support for deleting files at end of
                              install process for new temporary file groups.
                            + Added code to check underlying OSs for whole
                              installation and to selectively install groups.
                            + Removed support for comsvr tag and replaced this
                              with support for registering COM servers within
                              file tag. Registration functionality added to
                              installer/uninstaller classes - external DLL no
                              longer required for this.
                            + Replaced ZLib compression stream library with
                              calls into an Inflator DLL - this permits various
                              different kinds of compression to be used as plug
                              ins. Details of compression DLLs are now passed to
                              install class's main method.
                            + Added ability to uninstall registry keys if empty
                              as well as forcing deletion or no deletion
                              options.
                            + Improved output when registry being updated: names
                              of root keys being altered are displayed and names
                              of keys being deleted are now displayed.
                            + Now uses UBinStrings unit to convert binary
                              strings to binary data and can now support zero-
                              length binary data values in registry and
                              URegUtils is now used to convert HKEY values into
                              the string representation of root keys.
                            + Added ability to execute uninstall program for any
                              previous SITools program in same folder before
                              running installer.
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 23/02/2003
      @COMMENTS             + Extracted output methods from TInstallerBase and
                              moved to new base class.
                            + Added facility to run named uninstallers in
                              addition to existing uninstaller in current path
                              by creating new class to run uninstallers -
                              existing uninstaller run code moved to new class.
    )
    @REVISION(
      @VERSION              3.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
    )
    @REVISION(
      @VERSION              3.3
      @DATE                 16/01/2006
      @COMMENTS             + Changed to find actual location of InstallLib.dll
                              rather than assuming Temp folder when copying DLL
                              to installation path.
                            + Added event handler for PathMacros.OnGetScratchPad
                              that sets [ScratchPad] path macro to folder where
                              files were extracted.
                            + Ensured that license files are deleted if license
                              refused.
                            + Changed to pass a TInstallerFiles object to
                              TInstaller.Install rather than list of files.
    )
    @REVISION(
      @VERSION              3.4
      @DATE                 23/02/2008
      @COMMENTS             Replaced usage of Registry.inc and FileNames.inc
                            include file with URegistry and UFileNames units.
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
 * The Original Code is UInstaller.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInstaller;


interface


uses
  // Delphi
  Windows, Classes, Registry,
  // Project
  UXMLFile, UXMLTag, UInflater, UInstallerFiles;

type

  {
  TInstallerMsgCallback:
    Callback procedure called to output progress messages. NOTE: this is a
    simple procedure, not a method.
  }
  TInstallerMsgCallback = procedure(const Msg: string);

  {
  TInstallerQueryCallback:
    Callback procedure called to get a string. QueryStr is the string to be
    returned. It also contains the default string. NOTE: this is a simple
    procedure, not a method.
  }
  TInstallerQueryCallback = procedure(var QueryStr: string);


  {
  TInstallOutputter:
    Base class that provides ability to output messages to owning object via
    OnMessage callback. This class is used as a base for all install classes
    requiring output.
  }
  TInstallOutputter = class(TObject)
  private // properties
    fOnMessage: TInstallerMsgCallback;
  protected
    procedure OutputFmt(const FmtStr: string; const Args: array of const);
      {Triggers the OnMessage event with formatted message}
    procedure Output(const Msg: string);
      {Triggers the OnMessage event with given message}
    procedure OutputStr(const FmtStr, ArgStr: string);
      {Triggers the OnMessage event with a string made up of given format string
      that includes the given argument string. The format string must have
      exactly one string format identifier embedded in it}
  public
    property OnMessage: TInstallerMsgCallback read fOnMessage write fOnMessage;
      {An event handler for passing text messages back to the calling program
      code}
  end;

  {
  TUninstallRunner:
    Helper class used by installer to run uninstall programs.
  }
  TUninstallRunner = class(TInstallOutputter)
  private
    procedure VersionInfo(const FileName: string; out MajorVer: Word;
      out Company, OrigFileName: string);
      {Returns some version information about the given program (FileName). The
      major version, company and original file name are passed back through out
      parameters. This info is required to determine kind of install program}
    procedure DoRunUninstaller(const Prog: string);
      {Run given uninstall program. Way uninstaller is run depends on type of
      program as follows:
        SIBuilder v3 and later: run program in quiet mode
        SIBuilder v2 and earlier: run program via batch file to prevent pause
          for user input
        Any other program is simply run}
  public
    procedure RunUninstaller(const InstPath: string);
      {Checks if a valid uninstaller is present on given installation path, gets
      permission from user, and runs uninstaller if permission given. If
      uninstaller is earlier than version 3 then the uninstaller is run via a
      dynamically created batch file that prevents uninstaller from prompting
      user. If uninstaller is v3 then a command line switch is used to prevent
      prompting or sign-on messages from being displayed}
    procedure RunNamedUninstallers(const Tag: TXMLTag);
      {Runs the uninstallers specified as sub tags of the given tag}
  end;

  {
  TInstallerBase:
    Base class for installer and uninstaller classes. Provides functionality
    common to both classes.

    Inheritance: TInstallerBase -> [TObject]
  }
  TInstallerBase = class(TInstallOutputter)
  private // properties
    fXMLFile: TXMLFile;
    fOS: LongWord;
  protected
    procedure ReadInstallXMLFile(const XMLFileName: string);
      {Read given install XML like file into XML file object}
    procedure RunProgs(const Tag: TXMLTag; const UnInstall: Boolean);
      {Runs the program specified by the Tag with the required parameters. Only
      runs programs where Tag's uninstall parameter has same state as
      UnInstall flag}
    procedure UnInstallGroup(const Tag: TXMLTag);
      {Uninstall the file group specified by the contents of the given tag}
    procedure UnInstallFile(const Tag: TXMLTag; const Path: string);
      {Uninstall the file specified by the given tag from the given path}
    function UnregisterSharedDLL(const FileName: string): Boolean;
      {Unregisters given DLL with Delphi. If DLL has entry in registry the usage
      count is decreased. If usage count hits 0 or DLL not registered then True
      is returned indicating that DLL is to be deleted, otherwise False is
      returned}
    procedure UnregisterAppPath(const FileName: string);
      {Unregisters application's path with Windows}
    property XMLFile: TXMLFile read fXMLFile;
      {Exposes properties of XML installation/uninstallation info file}
    property OS: LongWord read fOS;
      {The OS on which installer is running}
  public
    constructor Create;
      {Class constructor: creates owned XML file object and records underlying
      OS}
    destructor Destroy; override;
      {Class destructor: frees owned object}
  end;

  {
  TInstaller:
    Class that reads a project installation (XML) script and performs project
    installation according to the script using a data file that contains all
    files to be installed.

    Inheritance: TInstaller -> TInstallerBase -> [TObject]
  }
  TInstaller = class(TInstallerBase)
  private // properties
    fOnGetInstallPath: TInstallerQueryCallback;
  private
    fInstallFiles: TStream;
      {Stream associated with the data file that stores the files to be
      installed}
    fInflater: TInflater;
      {Object used to inflate compressed files}
    fInstallerFiles: TInstallerFiles;
      {Object that stores details about file used by installer}
    procedure GetScratchPad(var Dir: string);
      {Event handler for PathMacros.OnGetScratchPad event. Sets value of
      [ScratchPad] path macro to name of folder where files are extracted}
    procedure RunUninstaller(const InstPath: string);
      {Checks if a valid uninstaller is present on given installation path, gets
      permission from user, and runs uninstaller if permission given. If
      uninstaller is earlier than version 3 then the uninstaller is run via a
      dynamically created batch file that prevents uninstaller from prompting
      user. If uninstaller is v3 then a command line switch is used to prevent
      prompting or sign-on messages from being displayed}
    procedure RunNamedUninstallers(const Tag: TXMLTag);
      {Runs the uninstallers specified as sub tags of the given tag}
    procedure InstallProject(const Tag: TXMLTag; var InstPath: string);
      {Install the project specified by the given XML tag onto the given
      installation path (if specified or allowed). InstPath is updated to refer
      to installation actually used}
    procedure WriteUnInstallFile(const FileName: string);
      {Writes the current XML file contents to the given file name, for use by
      the uninstall program}
    procedure RegisterUninstaller;
      {Register uninstall program with Windows}
    procedure InstallGroup(const Tag: TXMLTag);
      {Install the file group specified by the contents of the given tag}
    procedure InstallFile(const Tag: TXMLTag; const Path: string);
      {Install the file specified by the given tag onto the given path}
    procedure CopyFile(const FileName: string; const StoredSize,
      Offset: LongInt; const Compressed: Boolean);
      {Install file FileName stored in StoredSize bytes at given offset in data
      stream, decompressing as necessary}
    procedure RegisterAppPath(const FileName: string);
      {Registers given application's path with Windows}
    procedure RegisterSharedDLL(const FileName: string);
      {Registers the given shared DLL with Windows. If DLL is not already
      registered a registry entry is created for it with usage count of 1. If
      DLL is already registered then its usage count is incremented}
    procedure RegisterCOMDLL(const FileName: string);
      {Registers the given COM server DLL with Windows by called DLL's own
      exported registration function}
    procedure InstallRegRootKey(const Tag: TXMLTag);
      {Install keys and data under given registry root key as specified by given
      tag}
    procedure DisplayLicense(const Tag: TXMLTag);
      {Display license and act on user input}
    procedure InstallRegKey(const Tag: TXMLTag; const Reg: TRegistry;
      Path: string);
      {Install keys and data under given registry sub-key as specified by given
      tag and path}
    procedure InstallRegData(const Tag: TXMLTag; const Reg: TRegistry;
      Path: string);
      {Install registry data item as specified by given XML tag and path}
    procedure BinaryStringToRegData(const Reg: TRegistry; const Name: string;
      const Value: string);
      {Store binary data item with given name and string-encoded value in
      registry on current key}
  public
    procedure Install(const InstFiles: TInstallerFiles; var InstPath: string);
      {Install the project using information in the installer and inflator files
      specified by InstFiles. Use the given path InstPath if it is specified and
      permitted. Pass the actual path used back to caller in InstPath}
    property OnGetInstallPath: TInstallerQueryCallback
      read fOnGetInstallPath write fOnGetInstallPath;
      {An event handler for getting the install path back from the install
      programs user}
  end;

  {
  TUninstaller:
    Class that reads a project installation (XML) script and performs project
    installation according to the script using a data file that contains all
    files to be installed.

    Inheritance: TUninstaller -> TInstallerBase -> [TObject]
  }
  TUninstaller = class(TInstallerBase)
  private
    procedure UnInstallProject(const Tag: TXMLTag);
      {UnInstall the project as specified by the given XML tag}
    procedure UnRegisterUninstaller;
      {Un-register uninstall program with Windows}
    procedure UnInstallRegRootKey(const Tag: TXMLTag);
      {Uninstall keys and data under given registry root key as specified by
      given tag}
    procedure UnInstallRegKey(const Tag: TXMLTAg; const Reg: TRegistry;
      Path: string; const Force: Boolean);
      {Uninstall the registry key on the given path whose key value is stored in
      the given tag. If Force is true then the key is to be deleted. If Force is
      false then key is deleted only if the tag's 'deletable' parameter is true}
  public
    procedure UnInstall(const InstFile: string);
      {Uninstall program using given uninstall information (XML) file}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCommonTypes, UFileProcs, UPathMacros, UOS, UBinStrings,
  URegUtils, UCOMSvrRegProcs, URegistry, UFileNames, IntfSITLicenseDlg;


resourcestring
  // Output messages
  sOpInstalling           = 'Installing: %s';
  sOpSkipFile             = 'Skipping: %s';
  sOpGroup                = 'Group: %0:s on %1:s';
  sOpSkipGroup            = 'Group: %s being skipped';
  sOpProjName             = 'Project name: %s';
  sOpInstPath             = 'Installation path: %s';
  sOpRootKey              = 'Installing registry keys under %s';
  sOpRegKey               = '  Subkey: %s';
  sCantExec               = 'WARNING: Can''t execute'#13#10'%s';
  sExec                   = 'Executing: %s';
  sRegSharedDLL           = 'Registering shared DLL %s';
  sUnregSharedDLL         = 'Unregistering shared DLL %s';
  sDeletingRootKey        = 'Uninstalling registry keys under %s';
  sDeletingRegKey         = '  Deleting subkey: %s';
  sRegAppPath             = 'Registering app path for %s';
  sUnregAppPath           = 'Unregistering app path for %s';
  sDeletingFile           = 'Deleting: %s';
  sDeletingInstFiles      = 'Deleting files installed on %s';
  sDeletingAllFiles       = 'Deleting all files from %s';
  sRemovingEmptyFolder    = 'Removing empty folder %s';
  sRemovingFolder         = 'Removing folder %s and all contents';
  sUninstalling           = 'Uninstalling Project: %s';
  sUninstallingFrom       = '  from: %s';
  sRegisteredCOMDLL       = 'Registered COM Server DLL "%s"';
  sUnRegisteredCOMDLL     = 'Un-registered COM Server DLL "%s"';
  sCantRegisterCOMDLL     = 'WARNING: Couldn''t register COM Server DLL %s';
  sCantUnRegisterCOMDLL   = 'WARNING: Couldn''t un-register COM Server DLL %s';
  sLicenseAccepted        = 'License accepted';
  sMessageBoxTitle        = 'SITools Installer';
  sExecUninstBatMsg       = 'EXECUTING UNINSTALLER';
  sUninstExitingQuery     = 'An existing program already exists in this folder.'
                            + ' It will need to be uninstalled before this'
                            + ' installation can continue.'#13#10
                            + #13#10'  Click OK to uninstall the program.'
                            + #13#10'  Click Cancel to abort the installation.';
  sUninstNamedQuery       = 'Currently installed program "%s" must be'
                            + ' uninstalled before this installation can'
                            + ' continue.'#13#10
                            + #13#10'  Click OK to uninstall the program.'
                            + #13#10'  Click Cancel to abort the installation.';
  sUninstPrevOK           = 'Uninstallation program completed successfully.';
  // Error messages
  sErrRegBadBinString     = 'Invalid binary data definition string';
  sErrBadOverwriteInfo    = 'Error in install file: invalid overwrite info';
  sErrUnsupportedSubTag   = 'Unsupported subtag <%0:s> found in <%1:s>';
  sErrBadFileVersion      = 'Invalid install file version';
  sErrCantEditInstPath    = 'You can''t edit the path for this installation';
  sErrRegCantOpenKey      = 'Can''t open registry key %s';
  sErrRegCantCreateKey    = 'Can''t create registry key %s';
  sErrRegUnknownType      = 'Encountered unknown registry data type';
  sErrExecFailed          = 'Execution failed';
  sErrDLLLoadFailed       = 'Can''t find DLL %s';
  sErrDLLFnMissing        = 'Can''t find function %s in DLL %s';
  sErrDLLBadInterface     = 'Requested interface is not supported by %s';
  sErrLicenseRefused      = 'User refused license';
  sErrCantDisplayLicense  = 'Can''t display license';
  sErrBadOS               = 'This program cannot be installed on %s';
  sErrCantInflate         = 'Can''t inflate compressed file %s';
  sErrUninstPrev          = 'Uninstallation reported an error';
  sErrUserCancelled       = 'User cancelled';


{ Helper routines }

procedure LoadIntfFromDLL(DLLName: string; out DLLHandle: THandle;
  IID: TGUID; out Obj);
  {Loads the gien DLL, and passes it's handle back in DLLHandle. Attempts to
  call CreateIntfObject in the DLL to get an instance of the given interface
  which is passed back in Obj. If the DLL can't be loaded, the DLL doesn't
  support CreateIntfObject or the given interface is not supported then an
  exception is raised. The caller is responsible for freeing the object and the
  DLL}
const
  cFnName = 'CreateIntfObject'; // name of imported object creator function
var
  CreatorFn: function(IID: TGUID; out Obj): HResult;  // the CreateIntfObject fn
begin
  // Load the library
  DLLHandle := LoadLibrary(PChar(DLLName));
  if DLLHandle = 0 then
    raise Exception.CreateFmt(sErrDLLLoadFailed, [DLLName]);
  // Get address of CreateIntfObject function
  CreatorFn := GetProcAddress(DLLHandle, cFnName);
  if @CreatorFn = nil then
    raise Exception.CreateFmt(sErrDLLFnMissing, [cFnName, DLLName]);
  // Call CreateIntfObject to create required interface
  if not Succeeded(CreatorFn(IID, Obj)) then
    raise Exception.CreateFmt(sErrDLLBadInterface, [DLLName]);
end;


{ TInstallOutputter }

procedure TInstallOutputter.Output(const Msg: string);
  {Triggers the OnMessage event with given message}
begin
  if Assigned(fOnMessage) then fOnMessage(Msg);
end;

procedure TInstallOutputter.OutputFmt(const FmtStr: string;
  const Args: array of const);
  {Triggers the OnMessage event with formatted message}
begin
  Output(Format(FmtStr, Args));
end;

procedure TInstallOutputter.OutputStr(const FmtStr, ArgStr: string);
  {Triggers the OnMessage event with a string made up of given format string
  that includes the given argument string. The format string must have exactly
  one string format identifier embedded in it}
begin
  OutputFmt(FmtStr, [ArgStr]);
end;


{ TInstallerBase }

constructor TInstallerBase.Create;
  {Class constructor: creates owned XML file object and records underlying OS}
begin
  inherited Create;
  fXMLFile := TXMLFile.Create(cInstRootTag);
  fOS := UOS.EncodeSystemOS;
end;

destructor TInstallerBase.Destroy;
  {Class destructor: frees owned object}
begin
  fXMLFile.Free;
  inherited Destroy;
end;

procedure TInstallerBase.ReadInstallXMLFile(const XMLFileName: string);
  {Read given install XML like file into XML file object}
var
  FileStream: TFileStream;  // stream attached to information file
begin
  FileStream := TFileStream.Create(XMLFileName, fmOpenRead);
  try
    XMLFile.LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TInstallerBase.RunProgs(const Tag: TXMLTag;
  const UnInstall: Boolean);
  {Runs the program specified by the Tag with the required parameters. Only runs
  programs where Tag's uninstall parameter has same state as UnInstall flag}
var
  CmdTag: TXMLTag;  // references tags used to provide any cmd line params
  CmdLine: string;  // the total command line, inc program name
  Prog: string;     // the fully specified path of the program to run
  I: Integer;       // loops thru all params

  // ---------------------------------------------------------------------------
  function AddQuotes(const Str: string): string;
    {Tests to see if a command or parameter contains spaces and delimits by
    quotes if so}
  begin
    if Pos(' ', Str) > 0 then
      Result := '"' + Str + '"'
    else
      Result := Str;
  end;
  // ---------------------------------------------------------------------------

begin
  // Check if we need to handle this tag
  if Boolean(Tag.ParamAsInt['uninstall']) <> UnInstall then
    Exit;
  // Find the program name, quoting if necessary
  Prog := MakePathName(PathMacros.ExpandMacroPath(Tag.Params['path']))
    + Trim(Tag.PlainText);
  // Build the whole command line from the prog name and params (in <cmd> tags)
  CmdLine := AddQuotes(Prog);
  for I := 0 to Pred(Tag.SubTagCount) do
  begin
    CmdTag := Tag.SubTags[I];
    if CmdTag.Tag <> 'cmd' then
      OutputStr(sCantExec, CmdLine);
    CmdLine := CmdLine + ' ' + AddQuotes(PathMacros.ExpandMacroPath(
      CmdTag.PlainText));
  end;
  // Run the program and wait for it to finish
  OutputStr(sExec, CmdLine);
  if ExecAndWait(CmdLine, '', SW_SHOWNORMAL) <> 0 then
    raise Exception.Create(sErrExecFailed);
  // Delete the program if required
  if Boolean(Tag.ParamAsInt['delete']) then
    DeleteFile(Prog);
end;


procedure TInstallerBase.UnInstallFile(const Tag: TXMLTag;
  const Path: string);
  {Uninstall the file specified by the given tag from the given path}

  // ---------------------------------------------------------------------------
  procedure DoDeleteFile(const FileName: string);
    {Informs user that given file is being deleted and then deletes it}
  begin
    // If file is a COM server check if it was registered by installer and if so
    // try to unregister it
    if (TFileKind(Tag.ParamAsInt['kind']) = fkCOMDLL)
      and Boolean(Tag.ParamAsInt['regcomdll']) then
      if Succeeded(UCOMSvrRegProcs.UnRegInProcSvr(PChar(FileName))) then
        OutputStr(sUnRegisteredCOMDLL, FileName)
      else
        OutputStr(sCantUnRegisterCOMDLL, FileName);
    // Delete file and say so
    OutputStr(sDeletingFile, FileName);
    SysUtils.DeleteFile(FileName);
  end;
  // ---------------------------------------------------------------------------

var
  FileName: string; // full path to file to delete
begin
  // Store name of file to be deleted
  FileName := MakePathName(Path) + Tag.PlainText;
  if Boolean(Tag.ParamAsInt['regshareddll']) then
  begin
    // We have a shared DLL: only delete if ref count is zero
    if UnregisterSharedDLL(FileName) then
      DoDeleteFile(FileName);
  end
  else
    // Not a shared DLL: delete it
    DoDeleteFile(FileName);
  // Unregister file from app paths if necessary
  if Boolean(Tag.ParamAsInt['regapppath']) then
    UnregisterAppPath(FileName);
end;

procedure TInstallerBase.UnInstallGroup(const Tag: TXMLTag);
  {Uninstall the file group specified by the contents of the given tag}

  // ---------------------------------------------------------------------------
  procedure DeleteInstalledFiles(const Tag: TXMLTag;
    const ExpandedPath: string);
    {Deletes all files that form part of group given by Tag. In effect this
    deletes just those files that were installed}
  var
    SubTag: TXMLTag;  // sub tags of this tag
    I: Integer;       // loops thru sub tags
  begin
    // Loop tru all sub tags of group tag
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      // check that tag is file: delete if so, error if not
      if SubTag.Tag = 'file' then
        UnInstallFile(SubTag, ExpandedPath)
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag,
          [SubTag.Tag, Tag.Tag]);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  ExpandedPath: string;               // where group's files were installed
  FileDeletion: TFileDeletionActions; // how files to be deleted
  DirDeletion: TDirDeletionActions;   // how folder to be deleted
begin
  // Check if this group applies to this OS
  // if not group is not uninstalled since it was not installed
  if UOS.IsMatchingOS(OS, Tag.ParamAsInt['os']) then
  begin
    // Record fully expanded installation path for this group
    ExpandedPath := PathMacros.ExpandMacroPath(Tag.Params['path']);
    // Record file and dir deletion actions
    FileDeletion := TFileDeletionActions(Tag.ParamAsInt['filedeletion']);
    DirDeletion := TDirDeletionActions(Tag.ParamAsInt['dirdeletion']);
    // Delete files per FileDeletion "property"
    case FileDeletion of
      fdNone:
        // do nothing - we're not deleting files
        ;
      fdInstalled, fdTemporary:
      begin
        // delete only those files installed
        OutputStr(sDeletingInstFiles, ExpandedPath);
        DeleteInstalledFiles(Tag, ExpandedPath);
      end;
      fdAll:
      begin
        // delete all files in folder
        OutputStr(sDeletingAllFiles, ExpandedPath);
        // delete installed ones first: makes sure any unregistering gets done
        DeleteInstalledFiles(Tag, ExpandedPath);
        // delete rest of files: no special processing
        DeleteFiles(ExpandedPath);
      end;
    end;
    // Delete folders per DirDeletion "property"
    case DirDeletion of
      ddNone:
        // do nothing - we're not deleting the folder
        ;
      ddIfEmpty:
        // delete folder only if empty
        if IsFolderEmpty(ExpandedPath) then
        begin
          OutputStr(sRemovingEmptyFolder, ExpandedPath);
          RemoveDir(ExpandedPath);
        end;
      ddAll:
      begin
        // delete folder and all its contents
        OutputStr(sRemovingFolder, ExpandedPath);
        ForceRemoveFolder(ExpandedPath);
      end;
    end;
  end;
end;

procedure TInstallerBase.UnregisterAppPath(const FileName: string);
  {Unregisters application's path with Windows}
var
  Reg: TRegistry; // object used to access registry
begin
  // Tell user we're unregistering app
  OutputStr(sUnregAppPath, FileName);
  // Remove app's entry from registry
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.DeleteKey(cWdwAppPath + '\' + ExtractFileName(FileName));
  finally
    Reg.Free;
  end;
end;

function TInstallerBase.UnregisterSharedDLL(
  const FileName: string): Boolean;
  {Unregisters given DLL with Delphi. If DLL has entry in registry the usage
  count is decreased. If usage count hits 0 or DLL not registered then True is
  returned indicating that DLL is to be deleted, otherwise False is returned}
var
  Reg: TRegistry; // object that accesses registry
  Count: Integer; // usage count for DLL
begin
  // Inform user that we are unregistering DLL
  OutputStr(sUnregSharedDLL, FileName);
  // Assume DLL is to be deleted
  Result := True;
  // Open registry key where shared DLL info is stored
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(cWdwSharedDLL, True) then
    begin
      if Reg.ValueExists(FileName) then
      begin
        // DLL is registered: reduce usage count
        Count := Reg.ReadInteger(FileName);
        Dec(Count);
        if Count > 0 then
        begin
          // new usage count is non-zero: write new value back to registry
          Reg.WriteInteger(FileName, Count);
          Result := False;
        end
        else
          // new usage count is 0: delete registry entry
          Reg.DeleteValue(FileName);
      end;
    end;
  finally
    Reg.Free;
  end;
end;


{ TInstaller }

procedure TInstaller.BinaryStringToRegData(const Reg: TRegistry;
  const Name, Value: string);
  {Store binary data item with given name and string-encoded value in registry
  on current key}
var
  Buffer: PByte;   // data buffer
  BufLen: Integer; // length of data buffer
begin
  try
    // Convert the string of hex value to binary
    // NOTE: SpaceBinStrToBinary allocates a buffer we must free
    UBinStrings.SpaceBinStrToBinary(Value, Buffer, BufLen);
    try
      // Store in registry
      Reg.WriteBinaryData(Name, Buffer^, BufLen);
    finally
      // Release buffer that was created by SpaceBinStrToBinary
      FreeMem(Buffer, BufLen);
    end;
  except
    // Convert exception raised by SpaceBinStrToBinary to Exception class
    on E: EBinStrings do
      raise Exception.Create(sErrRegBadBinString);
  end;
end;

procedure TInstaller.CopyFile(const FileName: string; const StoredSize,
  Offset: Integer; const Compressed: Boolean);
  {Install file FileName stored in StoredSize bytes at given offset in data
  stream, decompressing as necessary}
var
  OutFileStream: TFileStream; // output stream to file
  CompBuf: Pointer;           // buffer for compressed file data
begin
  // Move to position in input stream where file is located
  fInstallFiles.Position := Offset;
  // Decide if file needs decompressing
  if Compressed then
  begin
    // File is compressed: decompress it
    // create buffer to store compressed file data and read it in
    GetMem(CompBuf, StoredSize);
    try
      fInstallFiles.ReadBuffer(CompBuf^, StoredSize);
      // inflate data to file using call into inflator DLL
      if not fInflater.InflateToFile(FileName, CompBuf, StoredSize) then
        raise Exception.CreateFmt(sErrCantInflate, [FileName]);
    finally
      FreeMem(CompBuf);
    end;
  end
  else
  begin
    // File is not compressed: just copy it out
    // open output file stream
    OutFileStream := TFileStream.Create(FileName, fmCreate);
    try
      // copy required number of bytes to file
      OutFileStream.CopyFrom(fInstallFiles, StoredSize);
    finally
      OutFileStream.Free;
    end;
  end;
end;

procedure TInstaller.DisplayLicense(const Tag: TXMLTag);
  {Display license and act on user input}
var
  DLLFile: string;        // fully specified name of license dll
  DLLHandle: THandle;     // handle to license dll
  Obj: ISITLicenseDlg;    // license dlg interface instance
  Keep: Boolean;          // whether to keep or delete license file after use
  MustAccept: Boolean;    // whether user must accept license or not
  LicenseFile: string;    // fully specified name of license file
  Style: DWORD;           // style of license dlg box
  Left, Top: Integer;     // left and top of dlg box
  Width, Height: Integer; // width and height of dlg box
  Response: HRESULT;      // how user responded to dlg box (what button clicked)
begin
  // Get file names
  DLLFile := PathMacros.ExpandMacroPath(Tag.Params['dll']);
  LicenseFile := PathMacros.ExpandMacroPath(Tag.PlainText);
  // Decide whether to keep or delete license file
  Keep := Boolean(Tag.ParamAsInt['keepfile']);
  // Decide whether used must accept license
  MustAccept := Boolean(Tag.ParamAsInt['accept']);
  // Load the DLL and create license dlg object
  DLLHandle := 0;
  try
    LoadIntfFromDLL(DLLFile, DLLHandle, ISITLicenseDlg, Obj);
    // Set style of dlg box
    Style := SITLicDlg_MONOFONT;
    if MustAccept then
      Style := Style or SITLicDlg_DECLINEBTN;
    Obj.SetStyle(Style);
    // Centre dlg on screen
    Width := Tag.ParamAsInt['width'];
    Height := Tag.ParamAsInt['height'];
    Left := (GetSystemMetrics(SM_CXSCREEN) - Width) div 2;
    Top := (GetSystemMetrics(SM_CYSCREEN) - Height) div 2;
    Obj.SetSize(Width, Height);
    // Display license dlg and act on user's response
    Response := Obj.DisplayLicenseFile(
      0, Left, Top, 'License', PChar(LicenseFile));
    case Response of
      S_FALSE:
        if MustAccept then
          raise Exception.Create(sErrLicenseRefused)
        else
          Output(sLicenseAccepted);
      S_OK:
        Output(sLicenseAccepted);
      else
        raise Exception.Create(sErrCantDisplayLicense);
    end;
  finally
    // Tidy up
    Obj := nil;
    if DLLHandle <> 0 then
      FreeLibrary(DLLHandle);
    // Delete license file if we're not keeping it
    if not Keep then
      DeleteFile(LicenseFile);
  end;
end;

procedure TInstaller.GetScratchPad(var Dir: string);
  {Event handler for PathMacros.OnGetScratchPad event. Sets value of
  [ScratchPad] path macro to name of folder where files are extracted}
begin
  // We use folder where files were extracted for scratch pad
  Dir := fInstallerFiles.ExtractPath;
end;

procedure TInstaller.Install(const InstFiles: TInstallerFiles;
  var InstPath: string);
  {Install the project using information in the install information file
  InstXMLFile using the files stored in InstDataFile. Use the InflateDLL, where
  provided, to inflate any compressed file data. Use the given path InstPath if
  it is specified and permitted. Pass the actual path used back to caller in
  InstPath}
var
  InstallLib: array[0..MAX_PATH] of Char; // path and name of InstallLib.dll
begin
  // Store reference to object containing names of installer files etc
  fInstallerFiles := InstFiles;
  // Enable PathMacros to get dir of scratch pad
  PathMacros.OnGetScratchPad := GetScratchPad;
  // Read in the install file
  ReadInstallXMLFile(fInstallerFiles.InstallInfoFile);
  // Open file install files stream
  fInstallFiles := TFileStream.Create(
    fInstallerFiles.InstallDataFile, fmOpenRead
  );
  try
    // Create object used to inflate compressed file data using DLL passed as
    // parameter to method
    if FileExists(fInstallerFiles.InflaterDLLFile) then
      fInflater := TInflater.Create(fInstallerFiles.InflaterDLLFile);
    // Do the installation
    InstallProject(XMLFile.RootTag, InstPath);
    // Check if we need to extract the un-install program
    if Boolean(XMLFile.RootTag.ParamAsInt['uninstall']) then
    begin
      // Write out project file
      WriteUnInstallFile(
        MakePathName(
          PathMacros.ExpandMacroPath(PathMacros.InstallPath)
        ) + cUninstallInfo
      );
      // Write out copy of this file
      GetModuleFileName(HInstance, InstallLib, MAX_PATH);
      Windows.CopyFile(
        InstallLib,
        PChar(
          MakePathName(
            PathMacros.ExpandMacroPath(PathMacros.InstallPath)
          ) + cInstallLib
        ),
        False
      );
      // Register uninstaller with control panel
      RegisterUninstaller;
    end;
  finally
    // Free install file stream
    fInstallFiles.Free;
    fInstallFiles := nil;
    // Free the inflater object
    fInflater.Free;
    fInflater := nil;
  end;
end;

procedure TInstaller.InstallFile(const Tag: TXMLTag; const Path: string);
  {Install the file specified by the given tag onto the given path}
var
  FileName: string;                       // full path to file
  ExistingFileDate: Integer;              // date of any existing file
  ExistingFileVerMS,
  ExistingFileVerLS: LongWord;            // file version of any existing file
  NewFileDate: Integer;                   // date of new install file
  NewFileVerMS,
  NewFileVerLS: LongWord;                 // file version of new file
  OverwriteAction: TFileOverwriteActions; // action to take if file exists
  WriteFile: Boolean;                     // flag true if file to be written
begin
  // Calculate full path to file
  FileName := MakePathName(Path) + Tag.PlainText;
  // Decide on overwrite actions - depends on any existing file's date
  OverwriteAction := TFileOverwriteActions(Tag.ParamAsInt['overwrite']);
  // Get date and version of any existing file
  ExistingFileDate := GetFileDate(FileName);    // -1 => file doesn't exist
  GetFileVer(FileName, ExistingFileVerLS, ExistingFileVerMS);
  // Get date and version of new file
  NewFileDate := Tag.ParamAsInt['date'];
  NewFileVerMS := Tag.ParamAsInt['verms'];
  NewFileVerLS := Tag.ParamAsInt['verls'];
  case OverwriteAction of
    foaAlways:
      // Always write file
      WriteFile := True;
    foaNever:
      // Only write file if it doesn't exist already
      WriteFile := ExistingFileDate = -1;
    foaIfOlder:
      // Only write file if (a) file doesn't exist or (b) is older than new one
      WriteFile := ExistingFileDate < NewFileDate;
    foaIfLaterVer:
    begin
      // Only write file if (a) file doens't exist of (b) has lower file version
      WriteFile := (NewFileVerMS > ExistingFileVerMS)
        or ((NewFileVerMS = ExistingFileVerMS)
          and (NewFileVerLS > ExistingFileVerLS));
    end
    else
      // None of the above - this is an error!
      raise Exception.Create(sErrBadOverwriteInfo);
  end;
  // Copy the file from the data file if OK to do so
  if WriteFile then
  begin
    OutputStr(sOpInstalling, FileName);
    CopyFile(FileName, Tag.ParamAsInt['storedsize'],
      Tag.ParamAsInt['pos'], Boolean(Tag.ParamAsInt['compressed']));
  end
  else
    OutputStr(sOpSkipFile, FileName);
  // Set file's data and attributes to those of original file
  SetFileDate(FileName, NewFileDate);
  FileSetAttr(FileName, Tag.ParamAsInt['attr']);
  // Register file if required
  if Boolean(Tag.ParamAsInt['regapppath']) then
    RegisterAppPath(FileName);
  if Boolean(Tag.ParamAsInt['regshareddll']) then
    RegisterSharedDLL(FileName);
  if Boolean(Tag.ParamAsInt['regcomdll']) then
    RegisterCOMDLL(FileName);
end;

procedure TInstaller.InstallGroup(const Tag: TXMLTag);
  {Install the file group specified by the contents of the given tag}
var
  Path: string;     // path to install files in this group onto
  SubTag: TXMLTag;  // sub tags specifying files to install
  I: Integer;       // loops thru subtags
begin
  // Check if we need to install this group on this OS
  if UOS.IsMatchingOS(OS, Tag.ParamAsInt['os']) then
  begin
    // Group is valid for OS
    // calculate the installation path for this group of files
    Path := PathMacros.ExpandMacroPath(Tag.Params['path']);
    OutputFmt(sOpGroup, [Tag.Params['name'], Path]);
    // make sure installation path exists
    EnsureFolders(Path);
    // install each file represented by a sub-tag
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'file' then
        InstallFile(SubTag, Path)
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
  end
  else
    // Group is not valid for this OS
    OutputStr(sOpSkipGroup, Tag.Params['name']);
end;

procedure TInstaller.InstallProject(const Tag: TXMLTag;
  var InstPath: string);
  {Install the project specified by the given XML tag onto the given
  installation path (if specified or allowed). InstPath is updated to refer to
  installation actually used}
var
  PathEditing: TInstallPathEditKinds; // editing allowed for install path
  I: Integer;                         // loops through project's sub tags
  SubTag: TXMLTag;                    // refers to sub-tag of the project tag
  PermittedOSs: LongWord;             // bit set of valid OS for this program
begin
  try
    // Check this is correct version
    if (Tag.ParamAsInt['version'] < cEarliestInstVersion)
      or (Tag.ParamAsInt['version'] > cCurrentInstVersion) then
      raise Exception.Create(sErrBadFileVersion);
    OutputStr(sOpProjName, Tag.Params['name']);
    // Check if this is a valid OS for this program
    PermittedOSs := Tag.ParamAsInt['ossupport'];
    if not UOS.IsMatchingOS(OS, PermittedOSs) then
      raise Exception.CreateFmt(sErrBadOS, [UOS.OSCodeToStr(OS)]);
    // Check if we can edit install path
    PathEditing := TInstallPathEditKinds(Tag.ParamAsInt['instpathedit']);
    // Get installation path and store it back in tag for later saving
    case PathEditing of
      ipeNone:
        // editing of path not allowed - complain if there's a command line
        begin
          if InstPath <> '' then
            raise Exception.Create(sErrCantEditInstPath);
          // use default path
          InstPath := PathMacros.ExpandMacroPath(Tag.Params['instpath']);
        end;
      ipeCommandLine:
        // we can edit via command line - use default if not present
        if InstPath = '' then
          InstPath := PathMacros.ExpandMacroPath(Tag.Params['instpath']);
      ipeQueryUser:
        // we have to ask user for path if not passed on command line
        if InstPath = '' then
        begin
          // no command line - ask user
          InstPath := PathMacros.ExpandMacroPath(Tag.Params['instpath']);
          if Assigned(fOnGetInstallPath) then
            fOnGetInstallPath(InstPath);
        end;
    end;
    Tag.Params['instpath'] := InstPath;
    // Update path macros with install path we're using and display it
    PathMacros.InstallPath := InstPath;
    OutputStr(sOpInstPath, InstPath);
    // Uninstall any existing installation if required
    if Boolean(Tag.ParamAsInt['uninstprev']) then
      RunUninstaller(InstPath);
    // Install sub tags
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'group' then
        InstallGroup(SubTag)                             // this is a file group
      else if SubTag.Tag = 'regrootkey' then
        InstallRegRootKey(SubTag)                 // this is a registry root key
      else if SubTag.Tag = 'run' then
        RunProgs(SubTag, False)               // this may be a program to be run
      else if SubTag.Tag = 'license' then
        DisplayLicense(SubTag)                    // we are to display a license
      else if SubTag.Tag = 'uninstprev' then
        RunNamedUninstallers(SubTag)
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
  finally
    // Finalise: delete temporary files and remove unneeded groups from project
    for I := Pred(Tag.SubTagCount) downto 0 do
    begin
      SubTag := Tag.SubTags[I];
      if (SubTag.Tag = 'group') then
      begin
        // We have group tag: tidy it up
        case TFileDeletionActions(SubTag.ParamAsInt['filedeletion']) of
          fdTemporary:
          begin
            // temporary file group:
            // delete the files
            UninstallGroup(SubTag);
            // clear the group: not required by uninstaller since files deleted
            SubTag.Clear;
          end;
          fdNone:
            // clear the group: not required by uninstaller since no deletion
            SubTag.Clear;
        end;
      end;
    end;
  end;
end;

procedure TInstaller.InstallRegData(const Tag: TXMLTag;
  const Reg: TRegistry; Path: string);
  {Install registry data item as specified by given XML tag and path}
begin
  // Attempt to open key where data to be stored, reporting any error
  if not Reg.OpenKey(Path, True) then
    raise Exception.CreateFmt(sErrRegCantOpenKey, [Path]);
  // Write data item to registry according to type
  case TRegDataType(Tag.ParamAsInt['type']) of
    rdUnknown:      // unknown data item
      raise Exception.Create(sErrRegUnknownType);
    rdString:       // string data
      if Boolean(Tag.ParamAsInt['expmacros']) then
        Reg.WriteString(                        // we need to expand path macros
          Tag.Params['name'],
          PathMacros.ExpandMacroPath(Tag.PlainText)
        )
      else
        Reg.WriteString(                               // ignore any path macros
          Tag.Params['name'],
          Tag.PlainText
        );
    rdExpandString: // expanded string data
      if Boolean(Tag.ParamAsInt['expmacros']) then
        Reg.WriteExpandString(                  // we need to expand path macros
          Tag.Params['name'],
          PathMacros.ExpandMacroPath(Tag.PlainText)
        )
      else
        Reg.WriteExpandString(                         // ignore any path macros
          Tag.Params['name'],
          Tag.PlainText
        );
    rdInteger:      // integer (DWORD) data
      Reg.WriteInteger(Tag.Params['name'], StrToInt(Tag.PlainText));
    rdBinary:       // raw binary data
      BinaryStringToRegData(Reg, Tag.Params['name'], Tag.PlainText);
  end;
end;

procedure TInstaller.InstallRegKey(const Tag: TXMLTag;
  const Reg: TRegistry; Path: string);
  {Install keys and data under given registry sub-key as specified by given tag
  and path}
var
  SubTag: TXMLTag;  // refers to sub tags of given tag
  I: Integer;       // loops thru all subtags
begin
  // Calculate path to this registry sub key
  Path := Path + '\' + Tag.Params['name'];
  OutputStr(sOpRegKey, Path);
  // Open the key, reporting error if can't be opened
  if not Reg.OpenKey(Path, True) then
    raise Exception.CreateFmt(sErrRegCantCreateKey, [Path]);
  try
    // Scan through all subtags installing subkeys and data items as required
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'regkey' then
        InstallRegKey(SubTag, Reg, Path)
      else if SubTag.Tag = 'regdata' then
        InstallRegData(SubTag, Reg, Path)
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
  finally
    Reg.CloseKey;
  end;
end;

procedure TInstaller.InstallRegRootKey(const Tag: TXMLTag);
  {Install keys and data under given registry root key as specified by given
  tag}
var
  Key: HKEY;        // root key value
  Reg: TRegistry;   // registry object
  SubTag: TXMLTag;  // reference to subtag containing details of sub-keys
  I: Integer;       // loops thru all subtags
begin
  // Record key value for this root key
  Key := Tag.ParamAsInt['hkey'];
  // Open registry
  Reg := TRegistry.Create;
  try
    // Move to required root key
    Reg.RootKey := Key;
    OutputStr(sOpRootKey, URegUtils.RegRootKeyToStr(Key));
    // Scan thru all sub-tags creating required sub keys per subtags
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'regkey' then
        InstallRegKey(SubTag, Reg, '')
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
  finally
    // Close registry
    Reg.Free;
  end;
end;

procedure TInstaller.RegisterAppPath(const FileName: string);
  {Registers given application's path with Windows}
var
  Reg: TRegistry; // registry access object
begin
  // Inform user we're registering app
  OutputStr(sRegAppPath, FileName);
  // Update registry with details of application
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(cWdwAppPath + '\' + ExtractFileName(FileName), True) then
      Reg.WriteString('', FileName);
  finally
    Reg.Free;
  end;
end;

procedure TInstaller.RegisterCOMDLL(const FileName: string);
  {Registers the given COM server DLL with Windows by called DLL's own exported
  registration function}
begin
  if Succeeded(UCOMSvrRegProcs.RegInProcSvr(PChar(FileName))) then
    OutputStr(sRegisteredCOMDLL, FileName)
  else
    OutputStr(sCantRegisterCOMDLL, FileName);
end;

procedure TInstaller.RegisterSharedDLL(const FileName: string);
  {Registers the given shared DLL with Windows. If DLL is not already registered
  a registry entry is created for it with usage count of 1. If DLL is already
  registered then its usage count is incremented}
var
  Reg: TRegistry; // object used to access registry
  Count: Integer; // usage count for DLL
begin
  // Inform user we're registering shared DLL
  OutputStr(sRegSharedDLL, FileName);
  // Open registry key where shared DLLs are registered.
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(cWdwSharedDLL, True) then
    begin
      // Get current user count for DLL
      if Reg.ValueExists(FileName) then
        Count := Reg.ReadInteger(FileName)
      else
        Count := 0;
      // Increment count and write new value to registry
      Inc(Count);
      Reg.WriteInteger(FileName, Count);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TInstaller.RegisterUninstaller;
  {Register uninstall program with Windows}
var
  Reg: TRegistry; // instance of registry object
begin
  // Add uninstall program to Add/Remove programs in registry
  Reg := TRegistry.Create;
  try
    // Add/Remove programs keeps its info in:
    //   HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Uninstall
    // so we create the required key using project ID as subkey
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey(cWdwUnInstall + '\' + XMLFile.RootTag.Params['id'], True);
    // .. and the required data using project's descriptive name
    Reg.WriteString(cWdwUnInstallDisplayName, XMLFile.RootTag.Params['name']);
    // .. and the path to the uninstall program
    Reg.WriteString(cWdwUnInstallUninstallString,
      MakePathName(PathMacros.ExpandMacroPath(
        PathMacros.InstallPath)) + cUninstallPrg);
  finally
    Reg.Free;
  end;
end;

procedure TInstaller.RunNamedUninstallers(const Tag: TXMLTag);
  {Runs the uninstallers specified as sub tags of the given tag}
begin
  with TUninstallRunner.Create do
    try
      RunNamedUninstallers(Tag);
    finally
      Free;
    end;
end;

procedure TInstaller.RunUninstaller(const InstPath: string);
  {Checks if a valid uninstaller is present on given installation path, gets
  permission from user, and runs uninstaller if permission given. If uninstaller
  is earlier than version 3 then the uninstaller is run via a dynamically
  created batch file that prevents uninstaller from prompting user. If
  uninstaller is v3 then a command line switch is used to prevent prompting or
  sign-on messages from being displayed}
begin
  with TUninstallRunner.Create do
    try
      RunUninstaller(InstPath);
    finally
      Free;
    end;
end;

procedure TInstaller.WriteUnInstallFile(const FileName: string);
  {Writes the current XML file contents to the given file name, for use by
  the uninstall program}
var
  FileStream: TFileStream;  // stream to uninstall info file
begin
  // Create the uninstall info file and save the XML to it
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    XMLFile.SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;


{ TUninstaller }

procedure TUninstaller.UnInstall(const InstFile: string);
  {Uninstall program using given uninstall information (XML) file}
begin
  ReadInstallXMLFile(InstFile);
  UnInstallProject(XMLFile.RootTag);
  UnRegisterUninstaller;
  SysUtils.DeleteFile(InstFile);
end;

procedure TUninstaller.UnInstallProject(const Tag: TXMLTag);
  {UnInstall the project as specified by the given XML tag}
var
  I: Integer;       // loops thru project tag's sub-tags
  SubTag: TXMLTag;  // references a sub tag of the project tag
  InstPath: string; // the path where the project was installed
begin
  // Check this is correct version of install info file
  if (Tag.ParamAsInt['version'] < cEarliestInstVersion)
    or (Tag.ParamAsInt['version'] > cCurrentInstVersion) then
    raise Exception.Create(sErrBadFileVersion);
  OutputStr(sUninstalling, Tag.Params['name']);
  // Get installation path
  InstPath := PathMacros.ExpandMacroPath(Tag.Params['instpath']);
  PathMacros.InstallPath := InstPath;     // store install path in PathMacros
  OutputStr(sUninstallingFrom, InstPath);
  // Uninstall sub tags
  for I := 0 to Pred(Tag.SubTagCount) do
  begin
    SubTag := Tag.SubTags[I];
    if SubTag.Tag = 'group' then
      UnInstallGroup(SubTag)                          // remove files from group
    else if SubTag.Tag = 'regrootkey' then
      UnInstallRegRootKey(SubTag)        // uninstall registry root key sub-keys
    else if SubTag.Tag = 'run' then
      RunProgs(SubTag, True)                  // this may be a program to be run
    else if SubTag.Tag = 'license' then
      {Do nothing}                                             // ignore license
    else if SubTag.Tag = 'uninstprev' then
      {Do nothing}                                             // ignore license
    else
      raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
  end;
end;

procedure TUninstaller.UnInstallRegKey(const Tag: TXMLTAg;
  const Reg: TRegistry; Path: string; const Force: Boolean);
  {Uninstall the registry key on the given path whose key value is stored in the
  given tag. If Force is true then the key is to be deleted. If Force is false
  then key is deleted only if the tag's 'deletable' is either "rdAlways" or
  "rdIfNoSubKeys" and key has no subkeys}

  // ---------------------------------------------------------------------------
  function HasSubKeys(Reg: TRegistry; const Path: string): Boolean;
    {Returns true if given path in registry has sub keys and false otherwise}
  begin
    if Reg.OpenKeyReadOnly(Path) then
    begin
      // key exists: return whether it has sub keys
      Result := Reg.HasSubKeys;
      Reg.CloseKey;
    end
    else
      // key doesn't exist: return false => no sub keys!
      Result := False;
  end;
  // ---------------------------------------------------------------------------

var
  SubTag: TXMLTag;                    // references sub tags of this tag
  I: Integer;                         // loops thru this tag's subtags
  Deleting: Boolean;                  // flag true if key is to be deleted
  DeleteCode: TRegKeyDeletionActions; // key's deletion code
begin
  // Calculate the full path to the key
  Path := Path + '\' + Tag.Params['name'];
  // Record key's deletion code
  DeleteCode := TRegKeyDeletionActions(Tag.ParamAsInt['deletable']);
  // Determine if tag is to be deleted
  Deleting := Force or (DeleteCode = rdAlways);
  try
    // Scan through sub tags, deleting the referenced paths as required
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'regkey' then
        UnInstallRegKey(SubTag, Reg, Path, Deleting)
      else if SubTag.Tag = 'regdata' then
        // Do nothing - data is deleted when key deleted
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
    // Check if we need to delete this key: either we know already, or key is
    // now empty and we want to delete empty keys
    Deleting := Deleting or
      ((DeleteCode = rdIfNoSubKeys) and not HasSubKeys(Reg, Path));
    // Delete this key if required
    if Deleting then
    begin
      OutputStr(sDeletingRegKey, Path);
      Reg.DeleteKey(Path);
    end;
  finally
    // Close the key
    Reg.CloseKey;
  end;
end;

procedure TUninstaller.UnInstallRegRootKey(const Tag: TXMLTag);
  {Uninstall keys and data under given registry root key as specified by given
  tag}
var
  Key: HKEY;        // this registry root key
  Reg: TRegistry;   // registry object
  SubTag: TXMLTag;  // a subtag of this tag
  I: Integer;       // loops through subtags
begin
  // Record key
  Key := Tag.ParamAsInt['hkey'];
  // Open registry
  Reg := TRegistry.Create;
  try
    // Move to required root key in registry
    Reg.RootKey := Key;
    OutputStr(sDeletingRootKey, RegRootKeyToStr(Key));
    // Scan through all sub tags uninstalling keys they represent as required
    for I := 0 to Pred(Tag.SubTagCount) do
    begin
      SubTag := Tag.SubTags[I];
      if SubTag.Tag = 'regkey' then
        UnInstallRegKey(SubTag, Reg, '', False)
      else
        raise Exception.CreateFmt(sErrUnsupportedSubTag, [SubTag.Tag, Tag.Tag]);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TUninstaller.UnRegisterUninstaller;
  {Un-register uninstall program with Windows}
var
  Reg: TRegistry; // instance of registry object
begin
  // remove program from Add/Remove programs in registry
  Reg := TRegistry.Create;
  try
    // Add/Remove programs keeps its info in:
    //   HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Uninstall
    // so we delete the appropriate key (which has project ID as sub-key)
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.DeleteKey(cWdwUnInstall + '\' + XMLFile.RootTag.Params['id']);
  finally
    Reg.Free;
  end;
end;


{ TUninstallRunner }

procedure TUninstallRunner.DoRunUninstaller(const Prog: string);
  {Run given uninstall program. Way uninstaller is run depends on type of
  program as follows:
    SIBuilder v3 and later: run program in quiet mode
    SIBuilder v2 and earlier: run program via batch file to prevent pause for
      user input
    Any other program is simply run}

  // ---------------------------------------------------------------------------
  procedure CreateUninstallBatFile(const FileName: string);
    {Creates a new batch file with given name (and path) with contents that will
    run a v2 or earlier uninstaller, passing the required key presses to prevent
    the uninstaller from pausing the installer waiting for a prompt}
  var
    FS: TFileStream;  // stream onto file we're creating
    Contents: string; // contents of batch file
  begin
    // Create the file
    FS := TFileStream.Create(FileName, fmCreate);
    try
      // Create a string containing batch file
      // batch calls uninstaller (uninstaller file is passed as %1 parameter)
      Contents :=
        // switch off echoing of commands
        '@echo off'#13#10 +
        // send a carriage return into the uninstaller to provide the required
        // key press that closes uninstaller
        'echo. | %1'#13#10 +
        // make sure next output is on line after uninstaller's exit prompt
        'echo.'#13#10 +
        // exit the batch file with error code returned from installer
        'exit %errorlevel%'#13#10;
      FS.Write(PChar(Contents)^, Length(Contents));
    finally
      FS.Free;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  UseBatFile: Boolean;  // flag true if we used batch file
  MajorVer: Word;       // major version of program file
  Company: string;      // company name from English trans in program ver info
  OrigFile: string;     // orig file name from English trans in program ver info
  BatFile: string;      // batch file used to run v2 uninstallers
  Command: string;      // command used to uninstall earlier install
begin
  // Read version info from uninstall program
  VersionInfo(Prog, MajorVer, Company, OrigFile);
  // Check if uninstall program is a SIBuilder uninstall program
  if (CompareText(OrigFile, cUninstallPrg) = 0)
    and (
      (CompareText(Company, 'PJSoft') = 0)
      or (CompareText(Company, 'DelphiDabbler') = 0)
    )
    and (MajorVer > 0) then
  begin
    UseBatFile := MajorVer in [1, 2];
    // This is a SIBuilder uninstall file
    if UseBatFile then
    begin
      // v2 or ealier uninstaller: we call it via a dynamically created batch
      // file that sends EOL char sequence to respond to prompt, which
      // prevents uninstaller from halting installer with completion prompt
      BatFile := MakePathName(TempFolder) + cUninstExecBat; // batch file
      CreateUninstallBatFile(BatFile);
      Command := LongToShortFileName(BatFile) + ' '
        + LongToShortFileName(Prog);
   end
    else
    begin
      // current version uninstaller: we call it with switch that prevents it
      // from prompting use or displaying success or failure messages
      Command := Prog + ' ' + cNoUninstPrompt;
    end;
  end
  else
  begin
    // This is a 3rd party uninstaller: just call it - no batch file
    Command := Prog;
    UseBatFile := False;
  end;
  // Run the uninstall program
  if UFileProcs.ExecAndWait(Command, '', SW_SHOWNORMAL) = 0 then
    Output(sUninstPrevOK)
  else
    Output(sErrUninstPrev);
  // Delete batch file if we used one
  if UseBatFile then
    SysUtils.DeleteFile(BatFile);
end;

procedure TUninstallRunner.RunNamedUninstallers(const Tag: TXMLTag);
  {Runs the uninstallers specified as sub tags of the given tag}

  // ---------------------------------------------------------------------------
  function QueryContinue(const Desc: string): Boolean;
    {Queries whether user wishes to uninstall app with given description
    (returns true) or to abort this installation (returns false)}
  begin
    Result := Windows.MessageBox(
      0,
      PChar(Format(sUninstNamedQuery, [Desc])),
      PChar(sMessageBoxTitle),
      MB_OKCANCEL or MB_ICONQUESTION or MB_SETFOREGROUND or MB_TASKMODAL
    ) = IDOK;
  end;

  function GetUninstallInfo(const ID: string;
    out UninstFile, Desc: string): Boolean;
    {Gets information about the uninstaller with the given id from registry.
    Returns uninstall file and description thru out parameters. Returns true if
    we have valid, existing uninstall program and false otherwise}
  var
    Reg: TRegistry; // registry access object
  begin
    // Assume failure: reset strings to nul value
    UninstFile := '';
    Desc := '';
    // Open uninstall key for given ID
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(cWdwUnInstall + '\' + ID) then
      begin
        // Read uninstall program path and description from registry
        UninstFile := Reg.ReadString(cWdwUnInstallUninstallString);
        Desc := Reg.ReadString(cWdwUnInstallDisplayName);
        // Check we have a valid uninstall file
        Result := (UninstFile <> '') and FileExists(UninstFile);
      end
      else
        // Registry key doesn't exist
        Result := False;
    finally
      Reg.Free;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Idx: Integer;           // loops thru uninstall app tags
  SubTag: TXMLTag;        // Sub tag containing info about app to be uninstalled
  UninstallFile: string;  // Name of uninstall file for app to be uninstalled
  UninstallDesc: string;  // Description of app to be uninstalled
begin
  // Loop thru all sub tags that describe apps to be uninstalled
  for Idx := 0 to Pred(Tag.SubTagCount) do
  begin
    SubTag := Tag.SubTags[Idx];
    // Check if tag is valid and if there's anything to uninstall
    if (SubTag.Tag = 'project')                 // valid tags are <project> tags
      and GetUninstallInfo(     // uninst if there's info in reg and file exists
        Trim(SubTag.PlainText), UninstallFile, UninstallDesc
      ) then
    begin
      // There is something to uninstall: ask user if OK
      if not QueryContinue(UninstallDesc) then
        // use declined: bail out
        raise Exception.Create(sErrUserCancelled);
      // User OKd: execute the uninstaller and display success or error message
      DoRunUninstaller(UninstallFile);
    end;
  end;
end;

procedure TUninstallRunner.RunUninstaller(const InstPath: string);
  {Checks if a valid uninstaller is present on given installation path, gets
  permission from user, and runs uninstaller if permission given. If uninstaller
  is earlier than version 3 then the uninstaller is run via a dynamically
  created batch file that prevents uninstaller from prompting user. If
  uninstaller is v3 then a command line switch is used to prevent prompting or
  sign-on messages from being displayed}

  // ---------------------------------------------------------------------------
  function QueryContinue: Boolean;
    {Queries whether user wishes to uninstall existing installation (returns
    true) or to abort this installation (returns false)}
  begin
    Result := Windows.MessageBox(
      0,
      PChar(sUninstExitingQuery),
      PChar(sMessageBoxTitle),
      MB_OKCANCEL or MB_ICONQUESTION or MB_SETFOREGROUND or MB_TASKMODAL
    ) = IDOK;
  end;
  // ---------------------------------------------------------------------------

var
  UninstProg: string; // uninstall program path
  UninstInfo: string; // uninstall info file path
  UninstLib: string;  // uninstall library file path
  BatFile: string;    // batch file used to run v2 uninstallers
begin
  // Build the required file names and paths
  UninstProg  := MakePathName(InstPath) + cUninstallPrg;    // uninstall program
  UninstInfo  := MakePathName(InstPath) + cUninstallInfo;   // uninstall info
  UninstLib   := MakePathName(InstPath) + cInstallLib;      // install DLL
  BatFile     := MakePathName(TempFolder) + cUninstExecBat; // batch file
  // Check if uninstall program, uninstall info and (un)install DLL file present
  if FileExists(UninstProg)
    and FileExists(UninstInfo)
    and FileExists(UninstLib) then
  begin
    // There's a valid installation: ask use if OK to uninstall
    if not QueryContinue() then
      // use declined: bail out
      raise Exception.Create(sErrUserCancelled);
    DoRunUninstaller(UninstProg);
  end;
end;

procedure TUninstallRunner.VersionInfo(const FileName: string;
  out MajorVer: Word; out Company, OrigFileName: string);
  {Returns some version information about the given program (FileName). The
  major version, company and original file name are passed back through out
  parameters. This info is required to determine kind of install program}
var
  VerInfoBuf: Pointer;  // points to memory storing version info
  VerInfoSize: Integer; // size of version info memory
  Dummy: THandle;       // unused parameter required by API function
  PBuf: Pointer;        // points to data buffer
  BufSize: LongWord;    // size of data buffer
const
  cStrInfo = '\StringFileInfo\080904E4\';
    // string table where needed values are in all DelphiDabbler/PJSoft apps
begin
  // Assume failure: sets zero result
  MajorVer := 0;
  Company := '';
  OrigFileName := '';
  // Get size of version info: there is none if this is zero
  VerInfoSize := Windows.GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize > 0 then
  begin
    // Allocate memory to store ver info
    GetMem(VerInfoBuf, VerInfoSize);
    try
      // Get the version info, filling buffer
      if Windows.GetFileVersionInfo(
        PChar(FileName), Dummy, VerInfoSize, VerInfoBuf
      ) then
      begin
        // Get fixed file info
        if VerQueryValue(VerInfoBuf, '\', PBuf, BufSize) then
          MajorVer := HiWord(PVSFixedFileInfo(PBuf)^.dwFileVersionMS);
        // Get required string values
        if VerQueryValue(
          VerInfoBuf, cStrInfo + 'CompanyName', PBuf, BufSize
        ) then
          Company := PChar(PBuf);
        if VerQueryValue(
          VerInfoBuf, cStrInfo + 'OriginalFileName', PBuf, BufSize
        ) then
          OrigFileName := PChar(PBuf);
      end;
    finally
      // Dispose of ver info storage
      FreeMem(VerInfoBuf, VerInfoSize);
    end;
  end;
end;

end.
