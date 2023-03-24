{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UGroupsTabHandler.pas
  @COMMENTS                 Implements a TTabSheetHandler descendant that
                            provides the required additional functionality for
                            the "file groups" tab sheet. Also implements two
                            information popup window classes to provide
                            information about selected groups and files.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added new context menu to treeview that replicates
                              buttons.
                            + Added default ability to display relevant edit
                              dialog when a node is double clicked (providing
                              node has no children).
                            + Removed hot link setting: hot links are now
                              defined in tagged rtf file and automatically
                              created when loaded.
                            + Moved literal strings to resource strings
                            + Nilled all event handlers when tab handler object
                              freed.
                            + Now distinguishes between different types of
                              executables and files: we use different icons for
                              each.
                            + Added optional pop-up windows to display
                              information about a file or a group when a file or
                              group node is clioked. New classes were added to
                              unit to implement these windows.
                            + Now uses UImageList unit rather than ImageList.inc
                              include file.
                            + Added ability to add files to groups using drag
                              and drop.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
                            + Added ability to validate files and to display
                              warning if errors detected.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/11/2003
      @COMMENTS             Changed creation of error handling "warner" object
                            to use new hot text control insted of rich text
                            control.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of Registry.inc and ResIds.inc
                              include file with URegistry and UResources units.
                            + Replaced a string literal with resource string.
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
 * The Original Code is UGroupsTabHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UGroupsTabHandler;


interface


uses
  // Delphi
  ComCtrls, SysUtils, Controls, Classes,
  // Project
  UTabSheetHandler, UGroups, UFiles, UInfoWdw, UFileValidator, UWarner;

type

  {
  TGroupInfoWdw:
    Implements an info window that displays the information from a group object.

    Inheritance: TGroupInfoWdw -> TInfoWdw -> [TCustomControl]
  }
  TGroupInfoWdw = class(TInfoWdw)
  public
    procedure SetGroup(const Group: TGroup);
      {Sets the window content to describe the attributes of the given group}
  end;

  {
  TFileInfoWdw:
    Implements an info window that displays the information from a file info
    object.

    Inheritance: TFileInfoWdw -> TInfoWdw -> [TCustomControl]
  }
  TFileInfoWdw = class(TInfoWdw)
  private
    fUninstallPrevious: Boolean;
      {Flag which indicates if project can uninstall previous installations
      (required by validator of files)}
  public
    constructor Create(AOwner: TComponent; UninstallPrevious: Boolean);
      reintroduce;
      {Class constructor: creates window instance and records flag showing if
      project can uninstall previous installations (required by validator)}
    procedure SetFileInfo(const FileInfo: TFileInfo);
      {Sets the window content to describe the attributes of the given file
      object}
  end;

  {
  EGroupsTabHandler:
    Class of Exception raised by TGroupsTabHandler.

    Inheritance: EGroupsTabHandler -> [Exception]
  }
  EGroupsTabHandler = class(Exception);

  {
  TGroupsTabHandler:
    Encapsulates the "file groups" tab sheet and provides the relevant
    specialised processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TGroupsTabHandler -> TTabSheetHandler -> TObjListItem
      -> [TObject]
  }
  TGroupsTabHandler = class(TTabSheetHandler)
  private
    fGroupInfoWdw: TGroupInfoWdw;
      {Popup info window object used to display information about a group}
    fFileInfoWdw: TFileInfoWdw;
      {Popup info window object used to display information about a file}
    fValidator: TFileValidator;
      {Object used to check validity of file entries}
    fWarner: TWarner;
      {Object used to record whether any program or parameters are invalid and
      to warn the user of the case, using a hot text component to display
      warnings with a clickable link that leads to a popup help topic}
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
      {OnChange event handler for group treeview component on tab sheet: updates
      state of buttons}
    procedure AddGroup(Sender: TObject);
      {OnClick event handler for add group button: adds a new group using user-
      specified information}
    procedure DeleteGroup(Sender: TObject);
      {OnClick event handler for delete group button: deletes selected group}
    procedure EditGroup(Sender: TObject);
      {OnClick event handler for edit group button: puts up dialog box to edit
      selected group}
    procedure AddFiles(Sender: TObject);
      {OnClick event handler for add files button: permits user to add files to
      selected group using standard file open dialog box}
    procedure EditFile(Sender: TObject);
      {OnClick event handler for edit file button: permits user to edit selected
      file's installation details using dialog box}
    procedure RemoveFile(Sender: TObject);
      {OnClick event handler for remove file button: deletes select file from
      group}
    procedure FileProperties(Sender: TObject);
      {OnClick event handler for file properties button: puts up dialog that
      gives file properties and versio information (where present)}
    procedure CreateGroup(const Group: TGroup);
      {Adds given group to treeview and to project}
    procedure TreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      {OnMouseDown event handler for group tree view: ensures that the relevant
      tree node is selected when right mouse button is clicked or that any
      required popup info window is displayed if left button is clicked}
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
      {OnMouseMove event handler for group tree view: determines if we are to
      display the node under cursor (if any) underlined to denote that a popup
      info window is available if the node is left clicked}
    procedure TreeDblClick(Sender: TObject);
      {OnDblClick event handler for group tree view: triggers pop-up menu's
      default menu item if there is one, else does nothing}
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
      {OnDeletion event handler for rgoup tree view: unregisters any outstanding
      warnings on all nodes before deleting them}
    procedure FilesDropFiles(Sender: TObject);
      {Drop files event handler for drop files window behind files tree view}
    procedure SetFileNodeImage(Node: TTreeNode; const FileInfo: TFileInfo);
      {Sets the image displayed next to given file node that is associated with
      the kind of file represented by the FileInfo object}
    procedure UpdateButtons;
      {Updates state of buttons on tab sheet and related popup menu items
      according to what is selected in tree view control}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: links event handlers in this class to controls in tab
      sheet's owning form and validator and warning objects}
    destructor Destroy; override;
      {Class destructor: nil all the form's event handlers that were handled by
      this class and free owned objects}
    procedure UpdateSheet; override;
      {Updates content of controls on tab sheet to reflect relavant project
      property values}
  end;


implementation


uses
  // Delphi
  Windows, 
  // Project
  UOS, UCommonTypes, UImageList, UMessageBox, URegistry, UResources,
  FmMain, FmGroupEdit, FmFileEdit, FmFileProperties;


{ TGroupsTabHandler }

resourcestring
  // Error messages
  sGroupExists =
    'Can''t create a group named "%s". Such a group already exists.';
  sReservedGroupName =
    'Can''t create a group name "%s". This is a reserved group name.';
  sDupGroupName =
    'Can''t change group name to "%s". A group with this name already exists.';
  sReservedGroupRename =
    'Can''t change group name to "%s". This is a reserved group name.';
  sNoDropGroup =
    'Files were not dropped over a group. The drop has been ignored.';
  // Prompts
  sConfirmGroupDelete = 'OK to delete "%s" group?';

procedure TGroupsTabHandler.AddFiles(Sender: TObject);
  {OnClick event handler for add files button: permits user to add files to
  selected group using standard file open dialog box}
var
  Node: TTreeNode;    // the currently selected tree node
  NewNode: TTreeNode; // the newwly added node
  Group: TGroup;      // group object associated with node
  I: Integer;         // loops thru files being added
  FileName: string;   // name of each file to be added
  FileIdx: Integer;   // index of new file info object in group
begin
  // Get hold of current node and exit if none selected
  Node := MainForm.tvFiles.Selected;
  if Node = nil then
    Exit;
  // If node is a child node (child nodes have level = 1), find parent
  if Node.Level = 1 then
    Node := Node.Parent;
  // Get reference to group associated with this node
  Group := TGroup(Node.Data);
  Assert(Group <> nil);
  // Now get files to add from user (use standard file open dlg)
  if MainForm.dlgAddFiles.Execute then
  begin
    // Got them - add to group Files property and treeview
    for I := 0 to MainForm.dlgAddFiles.Files.Count - 1 do
    begin
      // record file name
      FileName := MainForm.dlgAddFiles.Files[I];
      // add to group
      FileIdx := Group.Files.Add(TFileInfo.Create(FileName));
      // add a new node to treeview (with page image)
      NewNode := MainForm.tvFiles.Items.AddChild(Node, FileName);
      SetFileNodeImage(NewNode, Group.Files.Items[FileIdx]);
    end;
    // Expand the node to display files
    Node.Expanded := True;
  end;
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

procedure TGroupsTabHandler.AddGroup(Sender: TObject);
  {OnClick event handler for add group button: adds a new group using user-
  specified information}
var
  Dlg: TGroupEditDlg; // instance of dialogue box that gets group info
  NewGroup: TGroup;   // instance of the new group
begin
  // Create dialogue box to get group name and path
  Dlg := TGroupEditDlg.Create(MainForm);
  with Dlg do
  begin
    try
      // Create a new group object and assign in to Dialogue's Group property
      NewGroup := TGroup.Create('', '');
      Group := NewGroup;
      // Display dialogue and act on result
      if ShowModal = mrOK then
      begin
        // User OK'd - check that group name is unique - using Dlg's EditedName
        // property - Dlg didn't set Group's Name property to allow these tests
        if GetProject.InstallGroups.FindByName(EditedName) <> nil then
        begin
          // group name already used - free new group object & raise exception
          NewGroup.Free;
          raise EGroupsTabHandler.CreateFmt(sGroupExists, [EditedName]);
        end;
        // Now check if name is reserved
        if GetProject.IsReservedGroupName(EditedName) then
        begin
          // group name is reserved - free new group object & raise exception
          NewGroup.Free;
          raise EGroupsTabHandler.CreateFmt(sReservedGroupName, [EditedName]);
        end;
        // name is OK - assign it to the Group object
        NewGroup.Name := EditedName;
        // store new group object in tree view
        CreateGroup(NewGroup);
      end
      else
        // We cancelled - free the group that was created
        NewGroup.Free;
    finally
      Free;
    end;
  end;
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

constructor TGroupsTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: links event handlers in this class to controls in tab
  sheet's owning form and validator and warning objects}
begin
  inherited Create(TabSheet);
  // Link event handlers
  with MainForm do
  begin
    tvFiles.OnChange := TreeChange;
    tvFiles.OnMouseDown := TreeMouseDown;
    tvFiles.OnMouseMove := TreeMouseMove;
    tvFiles.OnDblClick := TreeDblClick;
    tvFiles.OnDeletion := TreeDeletion;
    btnAddGroup.OnClick := AddGroup;
    miGroupAddGroup.OnClick := AddGroup;
    btnDeleteGroup.OnClick := DeleteGroup;
    miGroupDeleteGroup.OnClick := DeleteGroup;
    btnEditGroup.OnClick := EditGroup;
    miGroupEditGroup.OnClick := EditGroup;
    btnAddFiles.OnClick := AddFiles;
    miGroupAddFiles.OnClick := AddFiles;
    btnEditFile.OnClick := EditFile;
    miGroupEditFile.OnClick := EditFile;
    btnRemoveFile.OnClick := RemoveFile;
    miGroupRemoveFile.OnClick := RemoveFile;
    btnFileProperties.OnClick := FileProperties;
    miGroupFileProperties.OnClick := FileProperties;
    dfFiles.OnDropFiles := FilesDropFiles;
  end;
  // Create file validator and warner objects
  fValidator := TFileValidator.Create;
  fWarner := TWarner.Create(MainForm.htGroupsWarning, cGroupsTabErrResID);
end;

procedure TGroupsTabHandler.CreateGroup(const Group: TGroup);
  {Adds given group to treeview and to project}
var
  Node: TTreeNode;        // the tree node for the group
  GroupTree: TTreeView;   // the group tree view control
begin
  // Get reference to tree view control
  GroupTree := MainForm.tvFiles;
  // Add the group to the project
  GetProject.InstallGroups.Add(Group);
  // Add the group to the tree view as 0 level entry and select it
  Node := GroupTree.Items.Add(nil, Group.Name);
  Node.Data := Group;   // ref to group object stored in node's Data prop
  Node.ImageIndex := cGroupNodeImageIndex;
  Node.SelectedIndex := cGroupNodeImageIndex;
  GroupTree.Selected := Node;
end;

procedure TGroupsTabHandler.DeleteGroup(Sender: TObject);
  {OnClick event handler for delete group button: deletes selected group}
var
  Node: TTreeNode;    // the currently selected tree node
  Group: TGroup;      // group object associated with node
begin
  // Get hold of current node
  Node := MainForm.tvFiles.Selected;
  // Exit if node is not a group node (group nodes are at level 0)
  if (Node = nil) or (Node.Level <> 0) then
    Exit;
  // Record required group from Data property of node which must exist
  Group := TGroup(Node.Data);
  Assert(Group <> nil);
  // Get user to confirm deletion and exit if says no
  if not TMessageBox.ConfirmFmt(
    MainForm, sConfirmGroupDelete, [Group.Name]
  ) then
    Exit;
  // Delete node from tree - this also deletes all child nodes
  MainForm.tvFiles.Items.Delete(Node);
  // Delete group from project - this also deletes all files in group also
  GetProject.InstallGroups.DeleteObj(Group);
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

destructor TGroupsTabHandler.Destroy;
  {Class destructor: nil all the form's event handlers that were handled by this
  class and free owned objects}
begin
  // Free the owned object
  fWarner.Free;
  fValidator.Free;
  // Nil the event handlers
  with MainForm do
  begin
    tvFiles.OnChange := nil;
    tvFiles.OnMouseDown := nil;
    tvFiles.OnMouseMove := nil;
    tvFiles.OnDblClick := nil;
    tvFiles.OnDeletion := nil;
    btnAddGroup.OnClick := nil;
    miGroupAddGroup.OnClick := nil;
    btnDeleteGroup.OnClick := nil;
    miGroupDeleteGroup.OnClick := nil;
    btnEditGroup.OnClick := nil;
    miGroupEditGroup.OnClick := nil;
    btnAddFiles.OnClick := nil;
    miGroupAddFiles.OnClick := nil;
    btnEditFile.OnClick := nil;
    miGroupEditFile.OnClick := nil;
    btnRemoveFile.OnClick := nil;
    miGroupRemoveFile.OnClick := nil;
    btnFileProperties.OnClick := nil;
    miGroupFileProperties.OnClick := nil;
    dfFiles.OnDropFiles := nil;
  end;
  inherited;
end;

procedure TGroupsTabHandler.EditFile(Sender: TObject);
  {OnClick event handler for edit file button: permits user to edit selected
  file's installation details using dialog box}
var
  Node: TTreeNode;      // the node containing the file name
  Group: TGroup;        // the group containing the file
  FileObj: TFileInfo;   // the file info object to be deleted
begin
  // Get hold of current node and exit if none selected or not a file node
  Node := MainForm.tvFiles.Selected;
  if (Node = nil) or (Node.Level <> 1) then   // file nodes are at level 1
    Exit;
  // Edit file install info
  // find group from node, ensuring it's not nil
  Group := Node.Parent.Data;
  Assert(Group <> nil);
  // find file info object in group's Files property
  FileObj := Group.Files.FindByFileSpec(Node.Text);
  Assert(FileObj <> nil);
  // Edit file install overwrite info - dlg updates file object
  if TFileEditDlg.EditFileProperities(MainForm, FileObj, Group) then
  begin
    // reset file node in case validity has changed and reset selection
    SetFileNodeImage(Node, FileObj);
    MainForm.tvFiles.Refresh; // ensures node redisplays properly
//    MainForm.tvFiles.Selected := Node;
  end;
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

procedure TGroupsTabHandler.EditGroup(Sender: TObject);
  {OnClick event handler for edit group button: puts up dialog box to edit
  selected group}
var
  Node: TTreeNode;              // the currently selected tree node
  CurrentGroup: TGroup;         // group object associated with node
  Dlg: TGroupEditDlg;           // instance of dialogue box
begin
  // Get hold of current node and exit if none selected
  Node := MainForm.tvFiles.Selected;
  // Exit if node is not a group node (group nodes are at level 0)
  if (Node = nil) or (Node.Level <> 0) then
    Exit;
  // Store reference to associated group object which must exist
  CurrentGroup := TGroup(Node.Data);
  Assert(CurrentGroup <> nil);
  // Create dialogue box to get group properties
  Dlg := TGroupEditDlg.Create(MainForm);
  with Dlg do
  begin
    try
      // Initialise properties to current group
      Group := CurrentGroup;
      if ShowModal = mrOK then
      begin
        // User OK'd - if group's name has changed, check if a new name OK
        // Dlg stores new name in EditedName, not in Group.Name
        if (CompareText(EditedName, Group.Name) <> 0) then
        begin
          // check if already exists
          if GetProject.InstallGroups.FindByName(EditedName) <> nil then
            // new name already exists - don't use it
            TMessageBox.WarningFmt(MainForm, sDupGroupName, [EditedName])
          // check if reserved
          else if GetProject.IsReservedGroupName(EditedName) then
            TMessageBox.WarningFmt(MainForm, sReservedGroupRename, [EditedName])
          else
          begin
            // name is unique - record new name in group and update tree view
            Group.Name := EditedName;
            Node.Text := Group.Name;
          end;
        end;
      end;
    finally
      Free;
    end;
  end;
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

procedure TGroupsTabHandler.FileProperties(Sender: TObject);
  {OnClick event handler for file properties button: puts up dialog that gives
  file properties and versio information (where present)}
var
  Node: TTreeNode;          // the node containing the file name
  Group: TGroup;            // the group containing the file
  FileObj: TFileInfo;       // the file info object to be displayed
begin
  // Get hold of current node and exit if none selected
  Node := MainForm.tvFiles.Selected;
  if (Node = nil) or (Node.Level <> 1) then   // file nodes are at level 1
    Exit;
  // Find required file object
  // find group from node, ensuring it exists
  Group := Node.Parent.Data;
  Assert(Group <> nil);
  // find file's object in group's Files property
  FileObj := Group.Files.FindByFileSpec(Node.Text);
  Assert(FileObj <> nil);
  // Display file info in dlg box
  TFilePropertiesDlg.DisplayFileInfo(MainForm, FileObj.SourceFileSpec);
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

procedure TGroupsTabHandler.FilesDropFiles(Sender: TObject);
  {Drop files event handler for drop files window behind files tree view}
var
  Node: TTreeNode;    // node of group node to own dropped files
  NewNode: TTreeNode; // newly added file node
  Group: TGroup;      // group object associated with group node
  I: Integer;         // loops thru files being added
  FileName: string;   // name of each file to be added
  FileIdx: Integer;   // index of new file info object in group
  Pt: TPoint;         // point where files are dropped
begin
  // Get tree node under drop point
  Pt := MainForm.dfFiles.DropPoint;
  Node := MainForm.tvFiles.GetNodeAt(Pt.X, Pt.Y);
  // Bail out with message if we didn't drop over a node
  if Node = nil then
  begin
    TMessageBox.Information(MainForm, sNoDropGroup);
    Exit;
  end;
  // If node is a child node (child nodes have level = 1), find parent
  if Node.Level = 1 then
    Node := Node.Parent;
  // Select group node under which files are to be dropped & get the group
  MainForm.tvFiles.Selected := Node;
  Group := TGroup(Node.Data);
  Assert(Group <> nil);
  // Loop thru dropped files
  for I := 0 to Pred(MainForm.dfFiles.Count) do
  begin
    // record file name
    FileName := MainForm.dfFiles.Files[I];
    // add to group
    FileIdx := Group.Files.Add(TFileInfo.Create(FileName));
    // add a new node to treeview (with page image)
    NewNode := MainForm.tvFiles.Items.AddChild(Node, FileName);
    SetFileNodeImage(NewNode, Group.Files.Items[FileIdx]);
  end;
  // Expand the node to ensure files are displayed
  Node.Expanded := True;
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

function TGroupsTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cGroupsInfoResId;
end;

procedure TGroupsTabHandler.RemoveFile(Sender: TObject);
  {OnClick event handler for remove file button: deletes select file from group}
var
  Node: TTreeNode;      // the node containing the file name
  Group: TGroup;        // the group containing the file
  FileObj: TFileInfo;   // the file info object to be deleted
begin
  // Get hold of current node and exit if none selected or not a file node
  Node := MainForm.tvFiles.Selected;
  if (Node = nil) or (Node.Level <> 1) then   // file nodes are at level 1
    Exit;
  // Delete file from project
  // find group from node, ensuring it's not nil
  Group := Node.Parent.Data;
  Assert(Group <> nil);
  // find file info object in group's Files property
  FileObj := Group.Files.FindByFileSpec(Node.Text);
  Assert(FileObj <> nil);
  // delete the file info object
  Group.Files.DeleteObj(FileObj);
  // Delete node from tree
  MainForm.tvFiles.Items.Delete(Node);
  // Return focus to group tree
  MainForm.tvFiles.SetFocus;
end;

procedure TGroupsTabHandler.SetFileNodeImage(Node: TTreeNode;
  const FileInfo: TFileInfo);
  {Sets the image displayed next to given file node that is associated with the
  kind of file represented by the FileInfo object}
var
  ImgImage: Integer;  // index of image in image list
begin
  // Get index of image required
  // we first validate the node to check if it's OK
  if fValidator.ValidateFile(FileInfo, GetProject.UninstallPrevious) then
  begin
    // node is OK: unregister any warning and get appropriate image
    fWarner.UnregisterWarning(Node);
    ImgImage := UImageList.FileKindImageIndex(FileInfo.FileKind)
  end
  else
  begin
    // node is in error: register warning and provide error node
    fWarner.RegisterWarning(Node);
    ImgImage := UImageList.cErrFileNodeImageIndex;
  end;
  // Make sure the image gets displayed
  Node.ImageIndex := ImgImage;
  Node.SelectedIndex := ImgImage;
end;

procedure TGroupsTabHandler.TreeChange(Sender: TObject; Node: TTreeNode);
  {OnChange event handler for group treeview component on tab sheet: updates
  state of buttons}
begin
  UpdateButtons;
end;

procedure TGroupsTabHandler.TreeDblClick(Sender: TObject);
  {OnDblClick event handler for group tree view: triggers pop-up menu's default
  menu item if there is one, else does nothing}
begin
  with MainForm do
  begin
    if miGroupEditGroup.Default then
      miGroupEditGroup.Click
    else if miGroupEditFile.Default then
      miGroupEditFile.Click;
  end;
end;

procedure TGroupsTabHandler.TreeDeletion(Sender: TObject; Node: TTreeNode);
  {OnDeletion event handler for rgoup tree view: unregisters any outstanding
  warnings on all nodes before deleting them}
begin
  // We unregister any warnings that are outstanding for the node
  fWarner.UnregisterWarning(Node);
end;

procedure TGroupsTabHandler.TreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {OnMouseDown event handler for group tree view: ensures that the relevant tree
  node is selected when right mouse button is clicked or that any required popup
  info window is displayed if left button is clicked}
var
  ClickedNode: TTreeNode; // reference to node mouse is over
  ItemRect: TRect;        // display rect for clicked node
  ItemTopLeft: TPoint;    // top left of above rect in screen coords
  Group: TGroup;          // group clicked or parent of clicked node
  FileInfo: TFileInfo;    // file object related to clicked file node
begin
  // Get reference to node that was clicked (nil if no node clicked)
  ClickedNode := MainForm.tvFiles.GetNodeAt(X, Y);
  if Button = mbRight then
  begin
    // Right mouse button was clicked: select node under mouse
    if Assigned(ClickedNode) then
      MainForm.tvFiles.Selected := ClickedNode;
  end
  else if Button = mbLeft then
  begin
    // Left mouse button was clicked: optionally display popup info window
    if Assigned(ClickedNode)
      and (htOnLabel in MainForm.tvFiles.GetHitTestInfoAt(X, Y)) then
    begin
      // We clicked a node on its label area: check if we need to display window
      if Assigned(fGroupInfoWdw) and (ClickedNode.Level = 0) then
      begin
        // We clicked a group node and we are displaying popup info for groups
        // get reference to associated group from node's data pointer
        Group := ClickedNode.Data;
        Assert(Assigned(Group));
        // update popup window with this group's information
        fGroupInfoWdw.SetGroup(Group);
        // display window alongside the clicked node
        ItemRect := ClickedNode.DisplayRect(True);
        ItemTopLeft := MainForm.tvFiles.ClientToScreen(ItemRect.TopLeft);
        Inc(ItemTopLeft.X, ItemRect.Right - MainForm.tvFiles.Indent * 2 + 8);
        fGroupInfoWdw.DisplayWdw(ItemTopLeft.X, ItemTopLeft.Y);
      end
      else if Assigned(fFileInfoWdw) and (ClickedNode.Level = 1) then
      begin
        // We clicked a file node and we are displaying popup info for files
        // get reference to file's parent group node
        Group := ClickedNode.Parent.Data;
        Assert(Group <> nil);
        // find file info object in group's Files property
        FileInfo := Group.Files.FindByFileSpec(ClickedNode.Text);
        Assert(Assigned(FileInfo));
        // update file popup window with this file's information
        fFileInfoWdw.SetFileInfo(FileInfo);
        // display window alongside the clicked node
        ItemRect := ClickedNode.DisplayRect(True);
        ItemTopLeft := MainForm.tvFiles.ClientToScreen(ItemRect.TopLeft);
        Inc(ItemTopLeft.X, ItemRect.Right - MainForm.tvFiles.Indent * 3 + 8);
        fFileInfoWdw.DisplayWdw(ItemTopLeft.X, ItemTopLeft.Y);
      end;
    end;
  end;
  inherited;
end;

procedure TGroupsTabHandler.TreeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  {OnMouseMove event handler for group tree view: determines if we are to
  display the node under cursor (if any) underlined to denote that a popup info
  window is available if the node is left clicked}
var
  Node: TTreeNode;  //  node under cursor
begin
  // Find node under cursor
  Node := MainForm.tvFiles.GetNodeAt(X, Y);
  // We display hottrack underline for node if cursor is over we are over a
  // node's label and we're displaying info windows for the type of node under
  // cursor (i.e. group node or file node)
  MainForm.tvFiles.HotTrack := Assigned(Node)
    and (
      ((Node.Level = 0) and Assigned(fGroupInfoWdw))
      or ((Node.Level = 1) and Assigned(fFileInfoWdw))
    )
    and (htOnLabel in MainForm.tvFiles.GetHitTestInfoAt(X, Y));
end;

procedure TGroupsTabHandler.UpdateButtons;
  {Updates state of buttons on tab sheet and related popup menu items according
  to what is selected in tree view control}
var
  State: Integer;     // a state code indicating what node is selected in tree
  Node: TTreeNode;    // the currently selected tree node
begin
  // Record currently selected node and set required state code
  Node := MainForm.tvFiles.Selected;
  if Node = nil then
    State := -1                   // there is no node - record state to indicate
  else
    State := Node.Level;                 // there is a node - state is its level
  // Enable/disable buttons according to state
  with MainForm do
  begin
    case State of
      -1 :  // no node is selected - we can only add groups
      begin
        btnAddGroup.Enabled := True;
        btnDeleteGroup.Enabled := False;
        btnEditGroup.Enabled := False;
        btnAddFiles.Enabled := False;
        btnEditFile.Enabled := False;
        btnRemoveFile.Enabled := False;
        btnFileProperties.Enabled := False;
      end;
      0 :   // a group node is selected - edit or add group & possibly add files
      begin
        btnAddGroup.Enabled := True;
        btnDeleteGroup.Enabled := True;
        btnEditGroup.Enabled := True;
        btnAddFiles.Enabled := tvFiles.Items.Count > 0;
        btnEditFile.Enabled := False;
        btnRemoveFile.Enabled := False;
        btnFileProperties.Enabled := False;
      end;
      1 :   // a file node is selected - we can add groups or process files
      begin
        btnAddGroup.Enabled := True;
        btnDeleteGroup.Enabled := False;
        btnEditGroup.Enabled := False;
        btnAddFiles.Enabled := tvFiles.Items.Count > 0;
        btnEditFile.Enabled := True;
        btnRemoveFile.Enabled := True;
        btnFileProperties.Enabled := Node.ImageIndex <> cErrFileNodeImageIndex;
      end;
    end;
    // Update pop-up menu items according to state of buttons
    miGroupAddGroup.Enabled := btnAddGroup.Enabled;
    miGroupDeleteGroup.Enabled := btnDeleteGroup.Enabled;
    miGroupEditGroup.Enabled := btnEditGroup.Enabled;
    miGroupAddFiles.Enabled := btnAddFiles.Enabled;
    miGroupEditFile.Enabled := btnEditFile.Enabled;
    miGroupRemoveFile.Enabled := btnRemoveFile.Enabled;
    miGroupFileProperties.Enabled := btnFileProperties.Enabled;
    // Set default menu item (this acts when user double clicks node)
    Assert(not(miGroupEditGroup.Enabled and miGroupEditFile.Enabled));
    // default is edit group if edit group btn enabled and node has no children
    // (when node has children double clicking expands/collapses node)
    miGroupEditGroup.Default := miGroupEditGroup.Enabled
      and not Node.HasChildren;
    // default is edit file if edit file btn enabled
    // (this button is never enabled when edit group is enabled)
    miGroupEditFile.Default := miGroupEditFile.Enabled;
  end;
end;

procedure TGroupsTabHandler.UpdateSheet;
  {Updates content of controls on tab sheet to reflect relevant project property
  values}
var
  GroupCounter: Integer;    // loops thru groups in project
  FileCounter: Integer;     // loops thru files in a group
  Group: TGroup;            // reference to a group
  GroupNode: TTreeNode;     // reference to a treeview node related to a group
  ChildNode: TTreeNode;     // reference to child of a group node in treeview
  InfoWdws: LongWord;       // mask that determines which info windows displayed
  FileInfo: TFileInfo;      // reference to a file object
begin
  // Freeze the tree view while updating
  MainForm.tvFiles.Items.BeginUpdate;
  try
    // Clear the tree view
    MainForm.tvFiles.Items.Clear;
    // Add each group in project to tree view
    for GroupCounter := 0 to GetProject.InstallGroups.Count - 1 do
    begin
      // Add group to tree
      Group := GetProject.InstallGroups.Items[GroupCounter];
      GroupNode := MainForm.tvFiles.Items.Add(nil, Group.Name);
      GroupNode.Data := Group;  // put ref to group object in node's Data prop
      // Set group node's image
      GroupNode.ImageIndex := cGroupNodeImageIndex;
      GroupNode.SelectedIndex := cGroupNodeImageIndex;
      // Add each file as child node of groups
      for FileCounter := 0 to Group.Files.Count - 1 do
      begin
        // get reference to file info object
        FileInfo := Group.Files.Items[FileCounter];
        // add to treeview
        ChildNode := MainForm.tvFiles.Items.AddChild(
          GroupNode, FileInfo.SourceFileSpec
        );
        // set file node's image
        SetFileNodeImage(ChildNode, FileInfo);
      end;
    end;
    // Expand all the nodes
    MainForm.tvFiles.FullExpand;
    // Ensure no node is selected in tree
    MainForm.tvFiles.Selected := nil;
  finally
    MainForm.tvFiles.Items.EndUpdate;
  end;
  // Determine if info windows are to be displayed
  InfoWdws := MainForm.Settings.InfoWindows;
  if cSIBFileInfoWdw and InfoWdws <> 0 then
  begin
    // we are displaying file info window: create window object
    fFileInfoWdw := TFileInfoWdw.Create(MainForm, GetProject.UninstallPrevious);
    fFileInfoWdw.Width := 250;
  end
  else
  begin
    // we are not displaying file info window: free & nil window object
    fFileInfoWdw.Free;
    fFileInfoWdw := nil;
  end;
  if cSIBGroupInfoWdw and InfoWdws <> 0 then
  begin
    // we are displaying group info window: create window object
    fGroupInfoWdw := TGroupInfoWdw.Create(MainForm);
    fGroupInfoWdw.Width := 250;
  end
  else
  begin
    // we are not displaying group info window: free & nil window object
    fGroupInfoWdw.Free;
    fGroupInfoWdw := nil;
  end;
  // Update the tab sheet's buttons
  UpdateButtons;
end;


{ TGroupInfoWdw }

procedure TGroupInfoWdw.SetGroup(const Group: TGroup);
  {Sets the window content to describe the attributes of the given group}
resourcestring
  // File deletion option descriptions
  sNoFileDelete = 'Do not delete files';
  sDeleteInstFiles = 'Delete only installed files.';
  sDeleteAllFiles = 'Delete all files in folder.';
  sDeleteTempFile = 'Delete temporary files after installation.';
  // Folder deletion option descriptions
  sNoDirDelete = 'Do not delete directory.';
  sDeleteEmptyDirs = 'Delete directory only if empty.';
  sDeleteAllDirs = 'Forcibly delete directory and all contents.';
  // Contents strings for display in window
  sGroupContent = 'Group:=%s';
  sPathContent = 'Path:=%s';
  sRemovalContent = 'Removal:=%s';
  sOSContent = 'OS:=%s';
const
  // Descriptions of file deletion actions for group
  cFileRemoval: array[TFileDeletionActions] of string = (
    sNoFileDelete, sDeleteInstFiles, sDeleteAllFiles, sDeleteTempFile
  );
  // Descriptions of folder deletion actions for group
  cDirRemoval: array[TDirDeletionActions] of string = (
    sNoDirDelete, sDeleteEmptyDirs, sDeleteAllDirs
  );
var
  Str: string;  // temporary storage for building up display strings
begin
  // Clear any existing contents
  Contents.Clear;
  // Add group and path name
  Contents.Add(Format(sGroupContent, [Group.Name]));
  Contents.Add(Format(sPathContent, [Group.Path]));
  // Build string of deletion options to be displayed
  Str := '';
  if Group.DirDeletion <> ddAll then
    // only add file removal info if not forcibly removing folder and contents
    Str := cFileRemoval[Group.FileDeletion];
  if Str <> '' then
    Str := Str + #13;
  Str := Str + cDirRemoval[Group.DirDeletion];
  Contents.Add(Format(sRemovalContent, [Str]));
  // Build description of operating system options
  Contents.Add(Format(sOSContent, [UOS.DescribeOSs(Group.OSOptions)]));
 end;


{ TFileInfoWdw }

constructor TFileInfoWdw.Create(AOwner: TComponent; UninstallPrevious: Boolean);
  {Class constructor: creates window instance and records flag showing if
  project can uninstall previous installations (required by validator)}
begin
  inherited Create(AOwner);
  fUninstallPrevious := UninstallPrevious;
end;

procedure TFileInfoWdw.SetFileInfo(const FileInfo: TFileInfo);
  {Sets the window content to describe the attributes of the given file object}
resourcestring
  // File overwrite option descriptions
  sOverwriteAlways = 'Always.';
  sOverwriteNever = 'Never.';
  sOverwriteOlder = 'Older files only.';
  sOverwriteOlderVer = 'Older versions only.';
  // File type descriptions
  sUnknownType = '***';
  sErrorType = '?';
  sDataFile = 'Data file.';
  sBatchFile = 'Batch file.';
  sDOSexe = 'DOS executable.';
  sWin32Exe = 'Windows 32 executable.';
  sWin16Exe = 'Windows 16 executable.';
  sWin32DLL = 'Windows 32 DLL.';
  sWin16DLL = 'Windows 16 DLL.';
  sCOMDLL = 'Windows COM Server DLL.';
  // File registration option descriptions
  sRegAppPath = 'Register application with Windows.';
  sRegDLLPath = 'Register shared DLL with Windows.';
  sRegCOMDLL = 'Register in-process COM server with Windows.';
  // Contents strings for display in window
  sFileContent = 'File:=%s';
  sTypeContent = 'Type:=%s';
  sOverwriteContent = 'Overwrite:=%s';
  sSpecialContent = 'Special:=%s';
  sWarningHeader = 'Warning:';
  sNone = 'None.';
const
  // Desciption of file overwrite actions
  cFileOverwrite: array[TFileOverwriteActions] of string = (
    sOverwriteAlways, sOverwriteNever, sOverwriteOlder, sOverwriteOlderVer
  );
  // Description of file types
  cFileKind: array[TFileKind] of string = (
    sUnknownType, sErrorType, sDataFile, sBatchFile, sDOSexe, sWin32Exe,
    sWin16Exe, sWin32DLL, sWin16DLL, sCOMDLL
  );
var
  Str: string;                // used for building up display strings
  Validator: TFileValidator;  // object used to validate file info
  ErrReason: string;          // reason for any file error
  DummyCtx: Integer;          // dummy help context (never used)
begin
  // Clear any existing contents
  Contents.Clear;
  // Add file name, file kind and overwrite option to content
  Contents.Add(Format(sFileContent, [FileInfo.FileName]));
  Contents.Add(Format(sTypeContent, [cFileKind[FileInfo.FileKind]]));
  Contents.Add(
    Format(sOverwriteContent, [cFileOverwrite[FileInfo.OverwriteAction]])
  );
  // Build string of any special (registration) options and display them
  Str := '';
  if FileInfo.RegisterAppPath then
    // app is to be registered with Windows
    Str := sRegAppPath;
  if FileInfo.RegisterSharedDLL then
  begin
    // shared DLL is to be registered with Windows
    if Str <> '' then
      Str := Str + #13;
    Str := Str + sRegDLLPath;
  end;
  if FileInfo.RegisterCOMDLL then
  begin
    // COM server DLL is to be registered with Windows
    if Str <> '' then
      Str := Str + #13;
    Str := Str + sRegCOMDLL;
  end;
  if Str = '' then
    // there are no special options: say so
    Str := sNone;
  Contents.Add(Format(sSpecialContent, [Str]));
  // Check if file is valid and set warning message if not
  Validator := TFileValidator.Create;
  try
    if not Validator.ValidateFile(
      FileInfo, fUninstallPrevious, ErrReason, DummyCtx
    ) then
    begin
      // file not valid: display message
      Contents.AddObject(
        Format('%0:s=%1:s.', [sWarningHeader, ErrReason]),
        Pointer(True)
      );
    end;
  finally
    Validator.Free;
  end;
end;

end.

