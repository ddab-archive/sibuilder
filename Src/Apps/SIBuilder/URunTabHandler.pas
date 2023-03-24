{
 * URunTabHandler.pas
 *
 * SIBuilder TTabSheetHandler descendant that provides the required additional
 * functionality for the "run programs" tab sheet.
 *
 * v1.0 of 04 Sep 2000  - Original version.
 * v1.1 of 29 Sep 2000  - Added new event handlers for buttons on Run Programs
 *                        page that relate to COM servers.
 *                      - Changed handling of run programs dialogue box so that
 *                        a list of install files is obtained and passed to the
 *                        dialog box.
 *                      - Changed display procedure for run programs so that
 *                        command line switches are no longer quoted.
 *                      - Added support for hotlink on related info pane.
 * v2.0 of 29 Dec 2002  - Deleted Enter method: now Enter method in base class
 *                        calls UpdateSheet which is what we used to do here.
 *                      - Changed to use a tree view to list programs rather
 *                        than list box. Programs are listed under one or both
 *                        of the main Install and Uninstall nodes, depending on
 *                        when run. Programs have an icon that shows their type,
 *                        and those that are to be deleted after being run are
 *                        have icons marked with Xs.
 *                      - Removed support for old run program object delete
 *                        property, now removed.
 *                      - Added support for new temporary file: UpdateSheet
 *                        method now detects any temporary files that are
 *                        flagged for running by installer and warns user:
 *                        programs are either deleted or are modified to run
 *                        only under installer - user is given option.
 *                      - Removed hot link setting: hot links are now defined in
 *                        tagged rtf file and automatically created when loaded.
 *                      - Removed InProc COM servers button: there is now a
 *                        separate tab sheet for registering in process COM
 *                        servers.
 *                      - Non-executable install files are no longer presented
 *                        in combo boxes.
 *                      - Made sure all event handlers are nilled when class is
 *                        destroyed.
 *                      - Added pop-up menu for tree view to duplicate buttons
 *                        available on tab sheet.
 *                      - Now uses UImageList unit rather than ImageList.inc
 *                        include file.
 *                      - Replaced calls the MessageDlg with calls to methods of
 *                        the TMessageBox object.
 *                      - Added facility to validate the run programs and made
 *                        display a warning message / icons when there are
 *                        problems. A popup help item can be displayed to
 *                        describe causes of errors and solutions.
 * v2.1 of 19 Nov 2003  - Changed creation of error handling "warner" object to
 *                        use new hot text control insted of rich text control.
 * v2.2 of 28 Nov 2003  - Refactoring: Deleted reference to unused unit.
 * v2.3 of 20 Feb 2008  - Replaced usage of ResIds.inc and Help.inc include
 *                        files with UResources and UHelpContexts units.
 * v2.4 of 01 Mar 2008  - Adapted to work with renamed help context const.
 *
 *
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
 * The Original Code is URunTabHandler.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit URunTabHandler;


interface


uses
  // Delphi
  ComCtrls, Controls, Classes,
  // Project
  UTabSheetHandler, URunProgs, UFiles, URunProgValidator, UWarner;


type

  {
  TRunTabHandler:
    Encapsulates the "Programs" tab sheet and provides the relevant specialised
    processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TRunTabHandler -> TTabSheetHandler -> TObjListItem -> [TObject]
  }
  TRunTabHandler = class(TTabSheetHandler)
  private
    fValidator: TRunProgValidator;
      {Object used to check validity of program and parameter entries}
    fWarner: TWarner;
      {Object used to record whether any program or parameters are invalid and
      to warn the user of the case, using a hot text component to display
      warnings with a clickable link that leads to a popup help topic}
    fInstallNode: TTreeNode;
      {Tree node under which the programs to be run on installation are lised}
    fUninstallNode: TTreeNode;
      {Tree node under which the programs to be run on uninstallation are lised}
    f32BitProgs: TStringList;
      {List of 32 bit programs included in installation: these are the programs
      available for selection when selecting Delphi COM server applications to
      register/unregister}
    fRunFiles: TStringList;
      {List of all excutable files included in installation: these are the
      programs available for selection when selecting programs to be run}
    fTempFiles: TStringList;
      {List of all temporary files included in installaion: any program or
      parameter among these files can't be used by uninstaller}
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
      {OnChange event handler for treeview component on tab sheet: updates
      buttons to reflect current selection in tree view control}
    procedure TreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      {OnMouseDown event handler for tree view: ensures that the relevant
      tree node is selected when right mouse button is clicked}
    procedure TreeDblClick(Sender: TObject);
      {OnDblClick event handler for tree view: triggers pop-up menu's
      default menu item if there is one, else does nothing}
    procedure TreeProgramsDeletion(Sender: TObject; Node: TTreeNode);
      {OnDeletion event handler for tree view: ensures all nodes are de-
      registered with warning object}
    procedure AddProg(Sender: TObject);
      {OnClick event handler for Add Program button: allows user to specify a
      new program to be run by installer}
    procedure DeleteProg(Sender: TObject);
      {OnClick event handler for Delete Program button: deletes selected
      program}
    procedure EditProg(Sender: TObject);
      {OnClick event handler for Edit Program button: allows user to edit
      selected program's details}
    procedure DelphiServerClick(Sender: TObject);
      {OnClick event handler for Delphi COM Apps button: allows user to specify
      a Delphi COM server application to be registered/unregistered by the
      installer}
    procedure UpdateButtons;
      {Updates state of buttons and popup menu items on tab sheet according to
      what is curently selected in tree view}
    procedure UpdateNode(Node: TTreeNode; RunProg: TRunProg;
      FileInfo: TFileInfo);
      {Updates given node to display the information about the given program to
      be run, using information from the associated file information object. If
      the program is not included in installation then FileInfo will be nil}
    function AddProgNode(Parent: TTreeNode; RunProg: TRunProg;
      FileInfo: TFileInfo): TTreeNode;
      {Adds a new node as a child of the given parent node for the given program
      to be run, using information from the associated file info object (which
      will be nil if the program is not part of the installation. Returns
      reference to new node}
    procedure AddParams(Parent: TTreeNode; RunProg: TRunProg);
      {Add the parameter nodes as children of the given parent (program) node
      for the parameters of the given program to be run}
    function FindChild(Parent: TTreeNode; RunProg: TRunProg): TTreeNode;
      {Finds the first child node of the given parent node that is associated
      with the given program and returned a reference to the found node. Returns
      nil if there is no such node}
    procedure SetProgNodeImage(Node: TTreeNode; RunProg: TRunProg;
      FileInfo: TFileInfo);
      {Sets the image to be displayed at the given node for the given program to
      be run and its assciated file information object. If the program to be run
      is not included in installation then FileInfo will be nil. If the program
      is in error then a warning icon will be displayed}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: links event handlers in this class to controls in tab
      sheet's owning form and creates the lists used to store installed
      programs}
    destructor Destroy; override;
      {Class destructor: nil all the form's event handlers that were handled by
      this class and frees owned objects}
    procedure UpdateSheet; override;
      {Updates controls on page to reflect current state of project. Validates
      programs}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Forms, Consts, StdCtrls,
  // Project
  UGUIHelper, UFileProcs, UCommonTypes, UImageList, UGroups,
  UMessageBox, UResources, UHelpContexts,
  FmMain, FmProgEdit, FmInstallFileName;


const
  // Kinds of files that can appear in run program dialog box
  cRunFileTypes: TFileKinds = [fkExe32, fkExe16, fkDOS, fkBatch];

resourcestring
  // Dialog box queries
  sQueryDeleteUninstall = '%s is also to be run on uninstallation. Do you also '
    + 'wish to delete this entry?';
  sQueryDeleteInstall = '%s is also to be run on installation. Do you also '
    + 'wish to delete this entry?';
  // Dialog box titles
  sNewDelphiSvrTitle = 'New Delphi COM Server Application';
  // Titles (captions) of install and uninstall root nodes
  sInstallNodeTitle = 'Install programs';
  sUninstallNodeTitle = 'Uninstall programs';


{ Helper routines }

function FileInfoFromList(InstFiles: TStrings; R: TRunProg): TFileInfo;
  {Looks up the install file associated with the given run program in the given
  list of install files and returns the file info object associated with the
  program. Returns nil if the given program is not part of the installation}
var
  FileIdx: Integer; // index of required file in string list
begin
  // Search for file specified by run program and get its index in list
  FileIdx := InstFiles.IndexOf(MakePathName(R.ExecPath) + R.ProgName);
  if FileIdx <> -1 then
    // Found file: return reference to associated file info object
    Result := InstFiles.Objects[FileIdx] as TFileInfo
  else
    // Couldn't find: return nil
    Result := nil;
end;


{ TRunTabHandler }

procedure TRunTabHandler.AddParams(Parent: TTreeNode; RunProg: TRunProg);
  {Add the parameter nodes as children of the given parent (program) node for
  the parameters of the given program to be run}
var
  Idx: Integer;         // loops thru all parameters of program
  ParamNode: TTreeNode; // tree node for parameter
begin
  // Loop thru all params of program, adding node for each one
  for Idx := 0 to Pred(RunProg.Params.Count) do
  begin
    // Create a node for parameter
    ParamNode := MainForm.tvPrograms.Items.AddChild(
      Parent, RunProg.Params[Idx]
    );
    // Record reference to program whose parameter this is
    ParamNode.Data := RunProg;
    // Set parameter image for node: depends of if parameter is valid
    if fValidator.ValidateParam(RunProg, Idx) then
    begin
      // param OK: ensure it's not in the warning list
      fWarner.UnregisterWarning(ParamNode);
      ParamNode.ImageIndex := cParamNodeImageIndex;
      ParamNode.SelectedIndex := cParamNodeImageIndex;
    end
    else
    begin
      // param in error: add reference to tree node to warning list
      fWarner.RegisterWarning(ParamNode);
      ParamNode.ImageIndex := cBadParamNodeImageIndex;
      ParamNode.SelectedIndex := cBadParamNodeImageIndex;
    end;
  end;
  // Ensure we can see the parameters
  Parent.Expand(True);
end;

procedure TRunTabHandler.AddProg(Sender: TObject);
  {OnClick event handler for Add Program button: allows user to specify a new
  program to be run by installer}
var
  RunProg: TRunProg;          // run program object instance
  NewNode: TTreeNode;         // newly added node
  ParentNode: TTreeNode;      // top level parent of selected node
  FileInfo: TFileInfo;        // file info object associated with run program
begin
  // Create a new run program object instance
  RunProg := TRunProg.Create;
  // Find top-level parent node of selected node (if any)
  ParentNode := MainForm.tvPrograms.Selected;
  while Assigned(ParentNode) and (ParentNode.Level <> 0) do
    ParentNode := ParentNode.Parent;
  // Guess at its "when run" attribute from which parent node selected
  if ParentNode = fUninstallNode then
    RunProg.WhenRun := rsdOnUnInstall
  else
    RunProg.WhenRun := rsdOnInstall;
  // Get info about new program from user
  if TEditProgDlg.EditRunProg(
    MainForm, RunProg, fRunFiles, fTempFiles, GetProject.WantUnInstall
  ) then
  begin
    // User has OK'd creation of new run program object
    // add run program object to project
    GetProject.RunProgs.Add(RunProg);
    // Preset new node (to keep compiler happy)
    NewNode := nil;
    // get file info object for run program (if program being installed)
    FileInfo := FileInfoFromList(fRunFiles, RunProg);
    // Add tree nodes for new program
    // under install and/or uninstall node as required
    if RunProg.WhenRun in [rsdOnInstall, rsdBoth] then
      NewNode := AddProgNode(fInstallNode, RunProg, FileInfo);
    if RunProg.WhenRun in [rsdOnUninstall, rsdBoth] then
      NewNode := AddProgNode(fUninstallNode, RunProg, FileInfo);
    // Make new node current
    MainForm.tvPrograms.Selected := NewNode;
    // Update buttons according to new selection
    UpdateButtons;
  end
  else
    // User cancelled - destroy new run program object
    RunProg.Free;
end;

function TRunTabHandler.AddProgNode(Parent: TTreeNode;
  RunProg: TRunProg; FileInfo: TFileInfo): TTreeNode;
  {Adds a new node as a child of the given parent node for the given program to
  be run, using information from the associated file info object (which will be
  nil if the program is not part of the installation. Returns reference to new
  node}
begin
  // Create new node for program, and store reference to run program object
  Result := MainForm.tvPrograms.Items.AddChild(
    Parent, MakePathName(RunProg.ExecPath) + RunProg.ProgName
  );
  Result.Data := RunProg;
  // Set the program node's image according to file type
  SetProgNodeImage(Result, RunProg, FileInfo);
  // Add parameters
  AddParams(Result, RunProg);
  // Ensure node is visible
  Parent.Expand(True);
end;

constructor TRunTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: links event handlers in this class to controls in tab
  sheet's owning form and creates the lists used to store installed programs}
begin
  inherited Create(TabSheet);
  with MainForm do
  begin
    tvPrograms.OnChange := TreeChange;
    tvPrograms.OnMouseDown := TreeMouseDown;
    tvPrograms.OnDblClick := TreeDblClick;
    tvPrograms.OnDeletion := TreeProgramsDeletion;
    btnAddProg.OnClick := AddProg;
    miProgAdd.OnClick := AddProg;
    btnEditProg.OnClick := EditProg;
    miProgEdit.OnClick := EditProg;
    btnDeleteProg.OnClick := DeleteProg;
    miProgDelete.OnClick := DeleteProg;
    btnDelphiSvr.OnClick := DelphiServerClick;
  end;
  // Create lists to store installed programs
  f32BitProgs := TStringList.Create;  // stores 32 bit Windows executables
  fRunFiles := TStringList.Create;    // stores all executables
  fTempFiles := TStringList.Create;   // stores all temporary files
  // Create validator object
  fValidator := TRunProgValidator.Create(fTempFiles);
  // Create warner object
  fWarner := TWarner.Create(
    MainForm.htProgWarning,  cRunTabProgErrResID
  );
end;

procedure TRunTabHandler.DeleteProg(Sender: TObject);
  {OnClick event handler for Delete Program button: deletes selected program}
var
  RunProg: TRunProg;        // object representing program entry to be deleted
  SelNode,                  // currently selected tree node
  Parent,                   // parent of selected tree node
  Node1, Node2: TTreeNode;  // the nodes to be deleted from tree view
begin
  // Record selected node: this may be a program or a parameter node
  SelNode := MainForm.tvPrograms.Selected;
  Assert(Assigned(SelNode));

  // Record reference to program object associated with node
  RunProg := SelNode.Data;
  Assert(Assigned(RunProg));

  // Record reference to top level parents
  Parent := SelNode.Parent;
  while Parent.Level > 0 do
    Parent := Parent.Parent;
  Assert(Assigned(Parent));

  // Find program node associated with program under this parent
  Node1 := FindChild(Parent, RunProg);
  Assert(Assigned(Node1));

  // Delete the program node
  Node1.Delete;
  if RunProg.WhenRun = rsdBoth then
  begin
    // Program object is referenced for installation and uninstallation:
    // we need to check if we're deleting both referenced nodes
    if Parent = fInstallNode then
    begin
      // We have deleted node from install sub-tree, so check uninstall sub-tree
      // find program's node in uninstall sub tree
      Node2 := FindChild(fUninstallNode, RunProg);
      Assert(Assigned(Node2));
      // ask user if its OK to delete
      if TMessageBox.ConfirmFmt(
        MainForm, sQueryDeleteUninstall, [RunProg.ProgName]
      ) then
      begin
        // it is OK to delete uninstall node, so do it
        Node2.Delete;
        // we can now delete program object since both nodes have gone
        GetProject.RunProgs.DeleteObj(RunProg);
      end
      else
        // not OK to delete uninstall node, so we can't free program object,
        // just change its WhenRun property: now deleted on uninstall only
        RunProg.WhenRun := rsdOnUninstall;
    end
    else
    begin
      // We deleted node from uninstall sub-tree, so check uninstall sub-tree
      // find program's node in install sub tree
      Node2 := FindChild(fInstallNode, RunProg);
      Assert(Assigned(Node2));
      // ask user if its OK to delete
      if TMessageBox.ConfirmFmt(
        MainForm, sQueryDeleteInstall, [RunProg.ProgName]
      ) then
      begin
        // it is OK to delete install node, so do it
        Node2.Delete;
        // we can now delete program object since both nodes have gone
        GetProject.RunProgs.DeleteObj(RunProg);
      end
      else
        // not OK to delete install node, so we can't free program object,
        // just change its WhenRun property: now deleted on install only
        RunProg.WhenRun := rsdOnInstall;
    end
  end
  else
  begin
    // Deleted node was only one for this program, so delete program object also
    GetProject.RunProgs.DeleteObj(RunProg);
  end;

  // Deselect all nodes (selected one was deleted)
  MainForm.tvPrograms.Selected := nil;
  // Update buttons to reflect change
  UpdateButtons;
end;

procedure TRunTabHandler.DelphiServerClick(Sender: TObject);
  {OnClick event handler for Delphi COM Apps button: allows user to specify a
  Delphi COM server application to be registered/unregistered by the installer}
var
  InstProg: TRunProg;         // run program object for installing svr
  UnInstProg: TRunProg;       // run program object for uninstalling svr
  FileName: string;           // Delphi app server file name
begin
  // Initialise file name
  FileName := '';
  // Get info about new program from user
  if TInstallFileNameDlg.EditInstallFileName(
    MainForm,
    sNewDelphiSvrTitle,
    IDH_DLG_DELPHICOMSVRAPP,
    FileName,
    f32BitProgs
  ) then
  begin
    // User has OK'd creation of new run program objects
    // add new program run to install server
    // .. create and set up object
    InstProg := TRunProg.Create;
    InstProg.ProgName := ExtractFileName(FileName);
    InstProg.ExecPath := ExtractFileDir(FileName);
    InstProg.Params.Add('/regserver');
    InstProg.WhenRun := rsdOnInstall;
    // .. add new object to project and add to install node in tree
    GetProject.RunProgs.Add(InstProg);
    AddProgNode(
      fInstallNode, InstProg, FileInfoFromList(f32BitProgs, InstProg)
    );
    // add new program run to uninstall server
    // .. create and set up object
    UnInstProg := TRunProg.Create;
    UnInstProg.ProgName := ExtractFileName(FileName);
    UnInstProg.ExecPath := ExtractFileDir(FileName);
    UnInstProg.Params.Add('/unregserver');
    UnInstProg.WhenRun := rsdOnUninstall;
    // .. add new object to project and add to uninstall node in tree
    GetProject.RunProgs.Add(UnInstProg);
    AddProgNode(
      fUninstallNode, UnInstProg, FileInfoFromList(f32BitProgs, UnInstProg)
    );
    // update buttons according to new selected
    UpdateButtons;
  end;
end;

destructor TRunTabHandler.Destroy;
  {Class destructor: nil all the form's event handlers that were handled by this
  class and frees owned objects}
begin
  with MainForm do
  begin
    tvPrograms.OnChange := nil;
    tvPrograms.OnMouseDown := nil;
    tvPrograms.OnDblClick := nil;
    tvPrograms.OnDeletion := nil;
    btnAddProg.OnClick := nil;
    miProgAdd.OnClick := nil;
    btnEditProg.OnClick := nil;
    miProgEdit.OnClick := nil;
    btnDeleteProg.OnClick := nil;
    miProgDelete.OnClick := nil;
    btnDelphiSvr.OnClick := nil;
  end;
  fWarner.Free;
  fValidator.Free;
  fTempFiles.Free;
  fRunFiles.Free;
  f32BitProgs.Free;
  inherited;
end;

procedure TRunTabHandler.EditProg(Sender: TObject);
  {OnClick event handler for Edit Program button: allows user to edit selected
  program's details}
var
  RunProg: TRunProg;          // object representing item being edited
  SelNode: TTreeNode;         // currently selected node
  InstProgNode: TTreeNode;    // install node associated with selected program
  UninstProgNode: TTreeNode;  // uninstall node associated with selected program
  FileInfo: TFileInfo;        // program's file info object in installation
begin
  // Record currently selected node
  SelNode := MainForm.tvPrograms.Selected;
  Assert(Assigned(SelNode));

  // Record program object associated with node
  RunProg := SelNode.Data;
  Assert(Assigned(RunProg));

  // Display editor dlg box and update name of displayed program if OK'd
  if TEditProgDlg.EditRunProg(
    MainForm, RunProg, fRunFiles, fTempFiles, GetProject.WantUnInstall
  ) then
  begin
    // Get reference to file info object associated with program (if any)
    FileInfo := FileInfoFromList(fRunFiles, RunProg);
    // Find tree node for the program in each sub-tree (one may be nil)
    InstProgNode := FindChild(fInstallNode, RunProg);
    UninstProgNode := FindChild(fUninstallNode, RunProg);
    // Update program's entries according to how it is run
    case RunProg.WhenRun of
      rsdOnInstall:
      begin
        // Program now is install only
        SelNode := InstProgNode;
        if Assigned(InstProgNode) then
          // install node exists: update the node
          UpdateNode(InstProgNode, RunProg, FileInfo)
        else
          // install node doesn't exist: create it
          SelNode := AddProgNode(fInstallNode, RunProg, FileInfo);
        if Assigned(UninstProgNode) then
          // uninstall node exists but shouldn't: delete it
          UninstProgNode.Delete;
      end;
      rsdOnUninstall:
      begin
        // Program is now uninstall only
        SelNode := UnInstProgNode;
        if Assigned(UninstProgNode) then
          // uninstall node exists: update it
          UpdateNode(UninstProgNode, RunProg, FileInfo)
        else
          // uninstall node doesn't exist: create it
          SelNode := AddProgNode(fUninstallNode, RunProg, FileInfo);
        if Assigned(InstProgNode) then
          // install node exists but shouldn't: delete it
          InstProgNode.Delete;
      end;
      rsdBoth:
      begin
        // Program is now both uninstall and install
        if Assigned(InstProgNode) then
          // install node exists: update it
          UpdateNode(InstProgNode, RunProg, FileInfo)
        else
          // install node doesn't exist: create it
          AddProgNode(fInstallNode, RunProg, FileInfo);
        if Assigned(UninstProgNode) then
          // uninstall node exists: update it
          UpdateNode(UninstProgNode, RunProg, FileInfo)
        else
          // uninstall node doesn't exist: create it
          AddProgNode(fUninstallNode, RunProg, FileInfo);
      end;
    end;
  end;
  // re-select (one of) the edited program's node(s)
  MainForm.tvPrograms.Selected := SelNode;
  // Update buttons following edit
  UpdateButtons;
end;

function TRunTabHandler.FindChild(Parent: TTreeNode;
  RunProg: TRunProg): TTreeNode;
  {Finds the first child node of the given parent node that is associated with
  the given program and returned a reference to the found node. Returns nil if
  there is no such node}
begin
  Result := Parent.GetFirstChild;
  while Assigned(Result) and (Result.Data <> RunProg) do
    Result := Result.GetNextSibling;
end;

function TRunTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cRunProgsInfoResId;
end;

procedure TRunTabHandler.SetProgNodeImage(Node: TTreeNode;
  RunProg: TRunProg; FileInfo: TFileInfo);
  {Sets the image to be displayed at the given node for the given program to be
  run and its assciated file information object. If the program to be run is not
  included in installation then FileInfo will be nil. If the program is in error
  then a warning icon will be displayed}
begin
  // Set icon assuming program is OK
  if Assigned(FileInfo) then
    // program is part of installation: use the recorded file kind to set image
    Node.ImageIndex := UImageList.FileKindImageIndex(
      ExeType(FileInfo.SourceFileSpec)
    )
  else
    // program is not part of installation: use a unknown file kind image
    Node.ImageIndex := UImageList.FileKindImageIndex(fkError);
  // Check validity of program, updating node's icon if necessary
  if fValidator.ValidateProgOnly(RunProg, GetProject.WantUnInstall) then
    // valid program: ensure it's not registered
    fWarner.UnregisterWarning(Node)
  else
  begin
    // invalid program: register warning and change icon to error icon
    fWarner.RegisterWarning(Node);
    Node.ImageIndex := cBadProgNodeImageIndex;
  end;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TRunTabHandler.TreeChange(Sender: TObject; Node: TTreeNode);
  {OnChange event handler for treeview component on tab sheet: updates buttons
  to reflect current selection in tree view control}
begin
  UpdateButtons;
end;

procedure TRunTabHandler.TreeDblClick(Sender: TObject);
  {OnDblClick event handler for tree view: triggers pop-up menu's default menu
  item if there is one, else does nothing}
begin
  if MainForm.miProgEdit.Default then
    MainForm.miProgEdit.Click
end;

procedure TRunTabHandler.TreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {OnMouseDown event handler for tree view: ensures that the relevant tree node
  is selected when right mouse button is clicked}
var
  ClickedNode: TTreeNode; // reference to node mouse is over
begin
  if Button = mbRight then
  begin
    // Right mouse button was clicked: select node under mouse
    ClickedNode := MainForm.tvPrograms.GetNodeAt(X, Y);
    if Assigned(ClickedNode) then
      MainForm.tvPrograms.Selected := ClickedNode;
  end;
  inherited;
end;

procedure TRunTabHandler.TreeProgramsDeletion(Sender: TObject;
  Node: TTreeNode);
  {OnDeletion event handler for tree view: ensures all nodes are de-registered
  with warning object}
begin
  fWarner.UnregisterWarning(Node);
end;

procedure TRunTabHandler.UpdateButtons;
  {Updates state of buttons and popup menu items on tab sheet according to what
  is curently selected in tree view}
var
  Node: TTreeNode;  // references selected tree node
  Level: Integer;   // the level of the selected node
begin
  with MainForm do
  begin
    // Record selected node
    Node := tvPrograms.Selected;
    // Record the level of selected node (-1 if no selected node)
    if Assigned(Node) then
      Level := Node.Level
    else
      Level := -1;
    case Level of
      -1, 0:  // either no node or root level node selected: add only
      begin
        EnableCtrls([btnAddProg], True);
        EnableCtrls([btnEditProg, btnDeleteProg], False);
        miProgEdit.Default := False;
      end;
      1:      // a program is selected: add, edit delete + possible default
      begin
        EnableCtrls([btnAddProg, btnEditProg, btnDeleteProg], True);
        miProgEdit.Default := Node.Count = 0;
      end;
      2:      // a param is selected: add, edit, delete + default edit
      begin
        EnableCtrls([btnAddProg, btnEditProg, btnDeleteProg], True);
        miProgEdit.Default := True;
      end;
    end;
    // Delphi server registration button enabled only if we have 32 bit programs
    // in installation
    btnDelphiSvr.Enabled := f32BitProgs.Count > 0;
    // Update popup menu items enabled state
    miProgAdd.Enabled := btnAddProg.Enabled;
    miProgEdit.Enabled := btnEditProg.Enabled;
    miProgDelete.Enabled := btnDeleteProg.Enabled;
  end;
end;

procedure TRunTabHandler.UpdateNode(Node: TTreeNode; RunProg: TRunProg;
  FileInfo: TFileInfo);
  {Updates given node to display the information about the given program to be
  run, using information from the associated file information object. If the
  program is not included in installation then FileInfo will be nil}
var
  ChildNode: TTreeNode; // reference to child nodes of given node
begin
  // Update text of node and imahe
  Node.Text := MakePathName(RunProg.ExecPath) + RunProg.ProgName;
  SetProgNodeImage(Node, RunProg, FileInfo);
  // Delete existing parameters
  ChildNode := Node.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    ChildNode.Delete;
    ChildNode := Node.GetFirstChild;
  end;
  // Add new parameters
  Self.AddParams(Node, RunProg);
end;

procedure TRunTabHandler.UpdateSheet;
  {Updates controls on page to reflect current state of project. Validates
  programs}

  //----------------------------------------------------------------------------
  function IsTempFile(FileInfo: TFileInfo): Boolean;
    {Returns true if the given file info item represents a temporary file. False
    is returned if file is not temporary or if file info is nil}
  var
    Group: TGroup;  // parent group of file info object
  begin
    // Assume not a temp file
    Result := False;
    if Assigned(FileInfo) then
    begin
      // File is temp if part of a temporary group
      Group := FileInfo.GetGroup as TGroup;
      if Assigned(Group) then
        Result := Group.FileDeletion = fdTemporary;
    end;
  end;
  //----------------------------------------------------------------------------

var
  I: Integer;             // loops thru various lists
  R: TRunProg;            // reference to a program object
  FileInfo: TFileInfo;    // reference to file info relating to program object
  AllFiles: TStringList;  // list of all installed files
begin
  // Clear any warnings
  fWarner.Clear;
  // Update various file lists with current installed files
  // clear the lists
  f32BitProgs.Clear;
  fRunFiles.Clear;
  fTempFiles.Clear;
  // read all installed files into temporary files
  AllFiles := TStringList.Create;
  try
    GetProject.InstallGroups.GetInstallFileNames(AllFiles, []);
    // check each file and decide which list to store it in
    for I := 0 to Pred(AllFiles.Count) do
    begin
      FileInfo := AllFiles.Objects[I] as TFileInfo;
      if FileInfo.FileKind in cRunFileTypes then
        // this is executable file: always add these
        fRunFiles.AddObject(AllFiles[I], FileInfo);
      if IsTempFile(FileInfo) then
        // this is a temp file: add to temp files list
        fTempFiles.AddObject(AllFiles[I], FileInfo)
      else if FileInfo.FileKind = fkExe32 then
        // this is a non-temp 32 bit exec file
        f32BitProgs.AddObject(AllFiles[I], FileInfo);
    end;
  finally
    AllFiles.Free;
  end;
  // Build the tree view from list of programs in project
  with MainForm do
  begin
    // Freeze the tree view while updating
    tvPrograms.Items.BeginUpdate;
    try
      // Clear any existing entries
      tvPrograms.Items.Clear;
      // Create install node and record reference
      fInstallNode := tvPrograms.Items.AddChild(nil, sInstallNodeTitle);
      fInstallNode.ImageIndex := cInstallNodeImageIndex;
      fInstallNode.SelectedIndex := cInstallNodeImageIndex;
      // Create uninstall node and record reference
      fUninstallNode := tvPrograms.Items.AddChild(nil, sUninstallNodeTitle);
      fUninstallNode.ImageIndex := cUninstallNodeImageIndex;
      fUninstallNode.SelectedIndex := cUninstallNodeImageIndex;
      // Add all run programs in project
      // loop thru all programs in project
      I := 0;
      while I < GetProject.RunProgs.Count do
      begin
        // reference current project and get its file info if in installation
        R := GetProject.RunProgs[I];
        FileInfo := FileInfoFromList(fRunFiles, R);
        // add node(s) for program according to when it is run
        if R.WhenRun in [rsdOnInstall, rsdBoth] then
          AddProgNode(fInstallNode, R, FileInfo);
        if R.WhenRun in [rsdOnUninstall, rsdBoth] then
          AddProgNode(fUninstallNode, R, FileInfo);
        // next program
        Inc(I);
      end;
      // ensure we can see all treeview
      tvPrograms.FullExpand;
    finally
      tvPrograms.Items.EndUpdate;
    end;
  end;
  // Update buttons according to state of list box
  UpdateButtons;
end;

end.

