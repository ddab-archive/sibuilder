{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URunProgs.pas
  @COMMENTS                 This is one of the units that together define
                            classes that model an installation project. This
                            unit defines the TRunProg and TRunProgList classes.
                            TRunProg encapsulates the information needed to run
                            a program after installation while TRunProgList
                            maintains a list of TRunProg objects.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 04/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 28/12/2000
      @COMMENTS             Added support for whether program run on
                            install/uninstall or both and modified way that
                            install project is written to permit different
                            entries to be written to different parts of the
                            project file.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 29/12/2002
      @COMMENTS             + Removed TRunProg.Delete property: deleted of files
                              is now handled via file groups.
                            + Added inherited call to CreateOwnedObjects in case
                              new code is added to ancestor method (now empty).
                            + Added Validation method to TRunProgList which
                              raises exception on error along with help context.
                            + Added new ERunProgList exception type.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 20/02/2008
      @COMMENTS             Replaced usage of Help.inc include file with
                            UHelpContexts unit.
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
 * The Original Code is URunProgs.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit URunProgs;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UXMLTag, UXMLObjects, UCommonTypes;

type

  {
  TRunProg
    Encapsulates information about programs that need to be run by the
    installation program.

    Inheritance: TRunProg -> TXMLObjectItem -> TObjListItem -> [TObject]
  }
  TRunProg = class(TXMLObjectItem)
  private // properties
    fExecPath: string;
    fParams: TStringList;
    fProgName: string;
    fWhenRun: TRunSchedule;
  protected
    procedure CreateOwnedObjects; override;
      {Create owned string list object}
  public
    destructor Destroy; override;
      {Class destructor - frees owned string list}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves project information as sub tag of given XML tag}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Sets properties according to information in given XML tag}
    procedure SaveInstallXML(const ParentTag: TXMLTag;
      const UnInstall: Boolean);
      {Saves information required by installer or uninstaller program to run
      program. If UnInstall is true save only those programs to be run by
      uninstaller and if false save only those to be run by installer}
    property ProgName: string read fProgName write fProgName;
      {The name of the program to be run}
    property ExecPath: string read fExecPath write fExecPath;
      {The path where the program is to be run from - can contain path macros}
    property Params: TStringList read fParams;
      {The parameters to be passed to the program - can contain path macros}
    property WhenRun: TRunSchedule read fWhenRun write fWhenRun;
      {Whether program is run only on install, only on uninstall, or both}
  end;

  {
  TRunProgList:
    Maintains a list of TRunProg objects which can be stored or read from XML
    objects.

    Inheritance: TRunProgList -> TXMLObjectList -> TObjList -> [TObject]
  }
  TRunProgList = class(TXMLObjectList)
  protected // properties
    function GetItem(AnIndex: Integer): TRunProg;
  public
    constructor Create(Owner: TObject); override;
      {Class constructor: creates a list of TRunProg objects}
    procedure SaveInstallXML(const ParentTag: TXMLTag;
      const UnInstall: Boolean);
      {Saves information required to run all programs in list in XML format as
      subtags of the given tag. If Uninstall is true then save only files which
      are to be run on uninstall, otherwise save only those which are to be run
      on install}
    procedure Validate(const TempFiles: TStrings; HasUninstaller: Boolean);
      {Validates all programs in list, checking if (a) any program slated to be
      run by ununinstaller is a temporary file or (b) any program is slated to
      be run by uninstaller when we have no uninstaller}
    property Items[AnIndex: Integer]: TRunProg read GetItem; default;
      {The programs to be run as objects in the list as an array}
  end;

  {
  ERunProgList:
    Class of exception raised by TRunProgList.
  }
  ERunProgList = class(Exception);


implementation


uses
  // Project
  UHelpContexts, URunProgValidator;


resourcestring
  // Error messages
  sProgValidationError = 'There is a problem with program "%s" which is '
    + 'scheduled to be run by the installer and/or the uninstaller.';


{ TRunProgList }

constructor TRunProgList.Create(Owner: TObject);
  {Class constructor: creates a list of TRunProg objects}
begin
  inherited CreateForClass(Owner, TRunProg);
end;

function TRunProgList.GetItem(AnIndex: Integer): TRunProg;
  {Read access method for Items property}
begin
  Result := inherited GetObject(AnIndex) as TRunProg;
end;

procedure TRunProgList.SaveInstallXML(const ParentTag: TXMLTag;
  const UnInstall: Boolean);
  {Saves information required to run all programs in list in XML format as
  subtags of the given tag. If Uninstall is true then save only files which are
  to be run on uninstall, otherwise save only those which are to be run on
  install}
var
  I: Integer; // loops thru items in list
begin
  // Loop through all items in list getting them to save their installation info
  // as sub tags of the given XML tag
  for I := 0 to Pred(Count) do
    (GetObject(I) as TRunProg).SaveInstallXML(ParentTag, UnInstall);
end;

procedure TRunProgList.Validate(const TempFiles: TStrings;
  HasUninstaller: Boolean);
  {Validates all programs in list, checking if (a) any program slated to be run
  by ununinstaller is a temporary file or (b) any program is slated to be run by
  uninstaller when we have no uninstaller}
var
  Idx: Integer;                 // loops thru all programs
  Validator: TRunProgValidator; // object used to perform validation
  RunProg: TRunProg;            // reference to a program
begin
  // Create object used to validate project
  Validator := TRunProgValidator.Create(TempFiles);
  try
    // Loop thru all programs in project
    for Idx := 0 to Pred(Count) do
    begin
      // Check validity of program
      RunProg := GetItem(Idx);
      if not Validator.ValidateProg(RunProg, HasUninstaller) then
        // there's an error: raise exception
        raise ERunProgList.CreateFmtHelp(
          sProgValidationError, [RunProg.ProgName], IDH_ERR_RUNPROGS
        );
    end;
  finally
    Validator.Free;
  end;
end;


{ TRunProg }

procedure TRunProg.CreateOwnedObjects;
  {Create owned string list object}
begin
  inherited;
  fParams := TStringList.Create;
end;

destructor TRunProg.Destroy;
  {Class destructor - frees owned string list}
begin
  fParams.Free;
  inherited Destroy;
end;

procedure TRunProg.LoadProjectXML(const XMLTag: TXMLTag);
  {Sets properties according to information in given XML tag}
var
  I: Integer;         // scans thru sub-tags
  SubTag: TXMLTag;    // references sub-tags
begin
  // Read in properties from tag's params
  fProgName := Trim(XMLTag.PlainText);
  fExecPath := XMLTag.Params['path'];
  fWhenRun := TRunSchedule(XMLTag.ParamAsInt['whenrun']);
  // Read in command line(s) from sub tags
  for I := 0 to Pred(XMLTag.SubTagCount) do
  begin
    SubTag := XMLTag.SubTags[I];
    if SubTag.Tag = 'cmd' then
      fParams.Add(SubTag.PlainText);
  end;
end;

procedure TRunProg.SaveInstallXML(const ParentTag: TXMLTag;
  const UnInstall: Boolean);
  {Saves information required by installer or uninstaller program to run
  program. If UnInstall is true save only those programs to be run by
  uninstaller and if false save only those to be run by installer}
var
  Tag: TXMLTag;     // tag where run progs object is stored
  SubTag: TXMLTag;  // <cmd> sub tags
  I: Integer;       // loops thru all command line params
begin
  if (fWhenRun = rsdBoth)
    or ((fWhenRun = rsdOnInstall) and not UnInstall)
    or ((fWhenRun = rsdOnUnInstall) and UnInstall) then
  begin
    // Create required tag
    Tag := ParentTag.AddSubTag('run');
    // Store properties as parameters of tag
    Tag.PlainText := fProgName;
    Tag.Params['path'] := fExecPath;
    Tag.ParamAsInt['uninstall'] := Ord(UnInstall);
    for I := 0 to Pred(fParams.Count) do
    begin
      SubTag := Tag.AddSubTag('cmd');
      SubTag.PlainText := fParams[I];
    end;
  end;
end;

procedure TRunProg.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves project information as sub tag of given XML tag}
var
  Tag: TXMLTag;     // tag where run progs object is stored
  SubTag: TXMLTag;  // <cmd> sub tags
  I: Integer;       // loops thru all command line params
begin
  // Create required tag
  Tag := ParentTag.AddSubTag('run');
  // Store properties as parameters of tag
  Tag.PlainText := fProgName;
  Tag.Params['path'] := fExecPath;
  Tag.ParamAsInt['whenrun'] := Ord(fWhenRun);
  // Store parameters in <cmd> tag
  for I := 0 to Pred(fParams.Count) do
  begin
    SubTag := Tag.AddSubTag('cmd');
    SubTag.PlainText := fParams[I];
  end;
end;

end.
