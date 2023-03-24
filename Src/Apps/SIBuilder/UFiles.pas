{ ##                 
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UFiles.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TFileInfo and TFileInfoList
                            classes. TFileInfo encapsulates the information and
                            methods needed to store and install a file while
                            TFileInfoList maintains a list of TFileInfo objects.
  @DEPENDENCIES             None.
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
                            installation project have been added. These copy the
                            files into the installation data file and update the
                            XML install info file. Since the classes are no
                            longer used by the install/uninstal programs all the
                            install/uninstall related functionality has been
                            removed. Also added ability to record action to take
                            when destination files already exist.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             + Added new RegisterAppPath and RegisterSharedDLL
                              properties.
                            + Added new 'verms' and 'verls' paramters to file
                              info tag in install info file. This stores file
                              version info for files where this is used to
                              determine whether to install.
                            + Rewrote code that stores files in install data
                              file. The new DataWriter object passed to the
                              Build methods is now used to perform the actual
                              writing to the data file: the TFileInfo object
                              simply calls into this object to get it to store
                              its file.
                            + Now stores compressed file size for compressed
                              files rather than original file size: this done to
                              meet needs of new inflator DLLs used by install
                              library.
                            + Added FileKind property to give kind of file: e.g.
                              document, 16 bit application, 32 bit DLL.
                            + TFileInfo.Validate does not now try to expand path
                              macros: file info objects don't include path
                              macros. Also removed use of UPathMacros unit.
                            + Removed unused properties / access methods from
                              TFileInfo i.e: FileAttributes & GetFileAttributes,
                              FileDate & GetFileDate, SourcePath and
                              GetSourcePath.
                            + Added new TFileInfo.RegisterCOMDLL property to
                              flag that file should be registered as a COM
                              server.
                            + Added new TFileInfo.InstallPath property to
                              give fully qualified install name of file: this
                              refers to owning group object.
                            + Added new TFileInfo.GetGroup method to get
                              reference to owning group.
                            + Modified Validate methods to take a
                              UninstallPrevious parameter and TFileInfo method
                              now uses a validator object to do validation.
                              Modified exception handling/raising code to pass
                              along any HelpContext related to errors.
    )
    @REVISION(
      @VERSION              3.1
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
 * The Original Code is UFiles.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UFiles;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UCommonTypes, UObjList, UReportingObjects, UXMLTag, UDataWriter;

type

  {
  EFileInfo:
    Class of exception raised by TFileInfo.

    Inheritance: EFileInfo -> [Exception]
  }
  EFileInfo = class(Exception);

  {
  EFileInfoList:
    Class of exception raised by TFileInfoList.

    Inheritance: EFileInfoList -> [Exception]
  }
  EFileInfoList = class(Exception);

  {
  TFileInfo:
    Encapsulates information and methods needed to store, install and uninstall
    a file.

    Inheritance: TFileInfo -> TReportingListItem -> TXMLObjectItem
      -> TObjListItem -> [TObject]
  }
  TFileInfo = class(TReportingListItem)
  private // properties
    fSourceFileSpec: string;
    fOverwriteAction: TFileOverwriteActions;
    fRegisterSharedDLL: Boolean;
    fRegisterAppPath: Boolean;
    fFileKind: TFileKind;
    fRegisterCOMDLL: Boolean;
    function GetFileName: string;
    function GetFileSize: LongInt;
    function GetFileKind: TFileKind;
    function GetInstallPath: string;
    function GetRegisterCOMDLL: Boolean;
    procedure SetRegisterCOMDLL(const Value: Boolean);
  public
    constructor Create(const SourceFileSpec: string);
      {Class constructor that records file spec and notes that file not saved to
      stream and that size of file and date stamp are not yet known}
    procedure Validate(UninstallPrevious: Boolean);
      {Checks that source file exists. Returns normally if so, but raises
      EFileInfo exception with source file spec as message if there is a
      problem. UninstallPrevious is required in validation and is true if
      installer should uninstall previous installations in install folder}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves project information as sub tag of given XML tag}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Sets properties according to information in given XML tag}
    procedure Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
      {Called when project is being built. Uses the DataWriter object to store
      file contents in the install data file and records installation
      information as a sub-tag of given XML tag}
    function GetGroup: TObject;
      {Returns reference to owning group if there is one or nil if not. Returns
      a TObject since returning TGroup would require Groups unit in interface
      which would causes a circular unit reference}
    property FileName: string read GetFileName;
      {Name of file, without path}
    property FileSize: LongInt read GetFileSize;
      {Actual size of file}
    property SourceFileSpec: string read fSourceFileSpec;
      {Fully qualified file name for source file}
    property OverwriteAction: TFileOverwriteActions
      read fOverwriteAction write fOverwriteAction;
      {Method to be used to decide if to overwrite pre-existing files}
    property RegisterAppPath: Boolean
      read fRegisterAppPath write fRegisterAppPath;
      {True if this file is to be registered as application with Windows on
      installation and unregistered on uninstallation}
    property RegisterSharedDLL: Boolean
      read fRegisterSharedDLL write fRegisterSharedDLL;
      {True if this file is to be registered / counted as a shared DLL with
      Windows and unregistered on uninstallation}
    property RegisterCOMDLL: Boolean
      read GetRegisterCOMDLL write SetRegisterCOMDLL;
      {True if this file is a COM Server and is flagged for registration as such
      with Windows, false otherwise}
    property FileKind: TFileKind
      read GetFileKind;
      {The kind of file this is: used to determine purposes file can be used for
      by SIBuilder and to determine icon displayed}
    property InstallPath: string read GetInstallPath;
      {The installation path of this file: returns file name prepended by the
      install path of the parent group. If there is no parent group the file
      name only is returned}
  end;

  {
  TFileInfoList:
    Maintains a list of TFileInfo objects which can be validated, copied,
    installed and uninstalled.

    Inheritance: TFileInfoList -> TReportingObjList -> TXMLObjectList
      -> TObjList -> [TObject]
  }
  TFileInfoList = class(TReportingObjList)
  private // properties
    function GetItem(AnIndex: Integer): TFileInfo;
  protected
    function CheckForSpec(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Returns true if file info object passed in AnObject has source file spec
      that is equal to the string pointed to by Ptr}
  public
    constructor Create(Owner: TObject); override;
      {Creates list object (owned by given owner) that stores TFileInfo objects}
    function FindByFileSpec(const ASpec: string): TFileInfo;
      {Return FileInfo object with given file spec, or nil if there is no such
      object. Uses CheckForSpec method to perform comparison}
    procedure Validate(UninstallPrevious: Boolean);
      {Checks that all files in list are valid. Returns normally if so, but
      raises EFileInfoList exception with problem file name as message if there
      is a problem. UninstallPrevious is required in validation and is true if
      installer should uninstall previous installations in install folder}
    procedure Build(const ParentTag: TXMLTag; const DataWriter: TDataWriter);
      {Called when project is being built. Stores each file in list in install
      data file using the DataWriter object and records required install
      information about each file}
    property Items[AnIndex: Integer]: TFileInfo read GetItem;
      {The group objects in the list as an array}
  end;


implementation


uses
  // Delphi
  ActiveX, Windows,
  // PJSoft library
  PJIStreams,
  // Project
  UFileProcs, UGroups, UFileValidator, UFileNames;

resourcestring
  // Error messages
  sFileError = 'File "%0:s" in group "%1:s" has the following problem: %2:s';


{ TFileInfo }

procedure TFileInfo.Build(const ParentTag: TXMLTag;
  const DataWriter: TDataWriter);
  {Called when project is being built. Uses the DataWriter object to store file
  contents in the install data file and records installation information as a
  sub-tag of given XML tag}
var
  Tag: TXMLTag;           // XML tag where we store file's install info
  FileVerLS,
  FileVerMS: LongWord;    // file version value from version info
  StoredSize: Integer;    // size of file as stored in data stream
  StreamPos: Integer;     // position of file in stream
  IsCompressed: Boolean;  // whther file was compressed
begin
  // Report that we're processing the file
  ReportMessage(FileName);
  // Create required XML sub tag
  Tag := ParentTag.AddSubTag('file');
  // Store file name as tag's text
  Tag.PlainText := GetFileName;
  // Store required properties as parameters to tag
  Tag.ParamAsInt['date'] := UFileProcs.GetFileDate(fSourceFileSpec);
  Tag.ParamAsInt['attr'] := FileGetAttr(fSourceFileSpec);
  Tag.ParamAsInt['overwrite'] := Ord(fOverwriteAction);
  Tag.ParamAsInt['kind'] := Ord(GetFileKind);
  // get any file version from file and store it only if present and we will
  // be testing it: this is to save space in project file
  UFileProcs.GetFileVer(fSourceFileSpec, FileVerLS, FileVerMS);
  if (fOverwriteAction = foaIfLaterVer)
    and ((FileVerLS <> 0) or (FileVerMS <> 0)) then
  begin
    Tag.ParamAsInt['verms'] := FileVerMS;
    Tag.ParamAsInt['verls'] := FileVerLS;
  end;
  // store registration details only if true to save space
  if fRegisterAppPath then
    Tag.ParamAsInt['regapppath'] := Ord(True);
  if fRegisterSharedDLL then
    Tag.ParamAsInt['regshareddll'] := Ord(True);
  if GetRegisterCOMDLL then
    Tag.ParamAsInt['regcomdll'] := Ord(True);
  // Store the file using the data writer object
  DataWriter.Store(fSourceFileSpec, StreamPos, StoredSize, IsCompressed);
  Tag.ParamAsInt['compressed'] := Ord(IsCompressed);
  Tag.ParamAsInt['storedsize'] := StoredSize;
  Tag.ParamAsInt['pos'] := StreamPos;
end;

constructor TFileInfo.Create(const SourceFileSpec: string);
  {Class constructor that records file spec and notes that file not saved to
  stream and that size of file and date stamp are not yet known}
begin
  inherited Create;
  fSourceFileSpec := SourceFileSpec;
  fOverwriteAction := foaAlways;
  fFileKind := fkUnknown;
end;

function TFileInfo.GetFileKind: TFileKind;
  {Read access method for FileKind property: if file kind is currently unknown
  the file itself is examined and its type determined. If file kind has already
  been chcked then the previously recorded value is used}
begin
  if fFileKind = fkUnknown then
    fFileKind := ExeType(fSourceFileSpec);
  Result := fFileKind;
end;

function TFileInfo.GetFileName: string;
  {Read access method for FileName property - extract the file name component
  from the fully specified file spec}
begin
  Result := ExtractFileName(fSourceFileSpec);
end;

function TFileInfo.GetFileSize: LongInt;
  {Read access method for the FileSize property - return size of file or -1 if
  file not found}
begin
  Result := UFileProcs.SizeOfFile(fSourceFileSpec);
end;

function TFileInfo.GetGroup: TObject;
  {Returns reference to owning group if there is one or nil if not. Returns a
  TObject since returning TGroup would require Groups unit in interface which
  would causes a circular unit reference}
begin
  if Assigned(List) then
    Result := List.Owner
  else
    Result := nil;
end;

function TFileInfo.GetInstallPath: string;
  {Read access method for InstallPath property: gets install path from owning
  group (if any) and returns that prepended to file name. If there is no group
  the file name is returned}
var
  Group: TGroup;  // parent group
begin
  Group := GetGroup as TGroup;
  if Assigned(Group) then
    Result := UFileProcs.MakePathName(Group.Path) + FileName
  else
    Result := FileName;
end;

function TFileInfo.GetRegisterCOMDLL: Boolean;
  {Read access method for RegisterCOMDLL property: returns state of property per
  field if file is a COM DLL, otherwise always returns False}
begin
  if GetFileKind <> fkCOMDLL then
    fRegisterCOMDLL := False;
  Result := fRegisterCOMDLL;
end;

procedure TFileInfo.LoadProjectXML(const XMLTag: TXMLTag);
  {Sets properties according to information in given XML tag}
begin
  // Store fully specified path to file per plain text of tag
  fSourceFileSpec := XMLTag.PlainText;
  // Record overwrite action
  fOverwriteAction := TFileOverwriteActions(XMLTag.ParamAsInt['overwrite']);
  fRegisterAppPath := Boolean(XMLTag.ParamAsInt['regapppath']);
  fRegisterSharedDLL := Boolean(XMLTag.ParamAsInt['regshareddll']);
  SetRegisterCOMDLL(Boolean(XMLTag.ParamAsInt['regcomdll']));
end;

procedure TFileInfo.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves project information as sub tag of given XML tag}
var
  Tag: TXMLTag;     // tag where file info object is stored
begin
  // Create required tag
  Tag := ParentTag.AddSubTag('file');
  // Record full path to file
  Tag.PlainText := fSourceFileSpec;
  // Record overwrite and registration actions
  Tag.ParamAsInt['overwrite'] := Ord(fOverwriteAction);
  Tag.ParamAsInt['regapppath'] := Ord(fRegisterAppPath);
  Tag.ParamAsInt['regshareddll'] := Ord(fRegisterSharedDLL);
  Tag.ParamAsInt['regcomdll'] := Ord(GetRegisterCOMDLL);
end;

procedure TFileInfo.SetRegisterCOMDLL(const Value: Boolean);
  {Write access method for RegisterCOMDLL property: sets given value if file is
  a COM server, otherwise sets property to false}
begin
  if GetFileKind = fkCOMDLL then
    fRegisterCOMDLL := Value
  else
    fRegisterCOMDLL := False;
end;

procedure TFileInfo.Validate(UninstallPrevious: Boolean);
  {Checks that source file exists. Returns normally if so, but raises EFileInfo
  exception with source file spec as message if there is a problem.
  UninstallPrevious is required in validation and is true if installer should
  uninstall previous installations in install folder}
var
  Validator: TFileValidator;  // object used to validate file
  ErrReason: string;          // reason for error
  ErrHelpCtx: Integer;        // help context associated with error
begin
  // Create validator object
  Validator := TFileValidator.Create;
  try
    // Validate file
    if not Validator.ValidateFile(
      Self, UninstallPrevious, ErrReason, ErrHelpCtx
    ) then
      // we have error: report it
      raise EFileInfo.CreateFmtHelp(
        sFileError,
        [FileName, (GetGroup as TGroup).Name, ErrReason],
        ErrHelpCtx
    );
  finally
    Validator.Free;
  end;
end;

{ TFileInfoList }

procedure TFileInfoList.Build(const ParentTag: TXMLTag;
  const DataWriter: TDataWriter);
  {Called when project is being built. Stores each file in list in install data
  file using the DataWriter object and records required install information
  about each file}
var
  I: Integer;   // loops thru all files in list
begin
  // Get each file to store its own information
  for I := 0 to Pred(Count) do
    GetItem(I).Build(ParentTag, DataWriter);
end;

function TFileInfoList.CheckForSpec(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
  {Returns true if file info object passed in AnObject has source file spec that
  is equal to the string pointed to by Ptr}
begin
  Result := CompareText((AnObject as TFileInfo).SourceFileSpec,
    StrPas(PChar(Ptr))) = 0;
end;

constructor TFileInfoList.Create(Owner: TObject);
  {Creates list object (owned by given owner) that stores TFileInfo objects}
begin
  inherited CreateForClass(Owner, TFileInfo);
end;

function TFileInfoList.FindByFileSpec(const ASpec: string): TFileInfo;
  {Return FileInfo object with given file spec, or nil if there is no such
  object. Uses CheckForSpec method to perform comparison}
begin
  Result := FindObject(CheckForSpec, PChar(ASpec)) as TFileInfo;
end;

function TFileInfoList.GetItem(AnIndex: Integer): TFileInfo;
  {Read access method for Items property}
begin
  Result := GetObject(AnIndex) as TFileInfo;
end;

procedure TFileInfoList.Validate(UninstallPrevious: Boolean);
  {Checks that all files in list are valid. Returns normally if so, but raises
  EFileInfoList exception with problem file name as message if there is a
  problem. UninstallPrevious is required in validation and is true if installer
  should uninstall previous installations in install folder}
var
  I: Integer; // iterates thru file list
begin
  // Get each file info object in list to validate itself and convert any
  // exceptions raise by file info objects into exceptions of this list
  for I := 0 to Pred(Count) do
    try
      GetItem(I).Validate(UninstallPrevious);
    except
      on E: EFileInfo do
        raise EFileInfoList.CreateHelp(E.Message, E.HelpContext);
    end;
end;

end.

