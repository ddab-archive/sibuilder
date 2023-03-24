{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmUninstProjects.pas
  @COMMENTS                 Defines a class that provides a dialog box used to
                            edit the list of projects that will be uninstalled
                            by the install program before running the
                            installation.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/02/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
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
 * The Original Code is FmUninstProjects.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmUninstProjects;


interface


uses
  // Delphi
  StdCtrls, Buttons, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TUninstProjectsDlg:
    Class provides a dialog box used to edit the list of projects that will be
    uninstalled by the install program before running the installation.

    Inheritance: TUninstProjectsDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TUninstProjectsDlg = class(TGenericOKDlg)
    btnAddOrUpdate: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    edProject: TEdit;
    lblProject: TLabel;
    lblProjects: TLabel;
    lbProjects: TListBox;
    sbtnDown: TSpeedButton;
    sbtnUp: TSpeedButton;
    procedure btnAddOrUpdateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure edProjectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbProjectsClick(Sender: TObject);
    procedure lbProjectsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbtnUpClick(Sender: TObject);
    procedure sbtnDownClick(Sender: TObject);
  private
    fEditingProjIdx: Integer;
      {Index in list box of project name we're editing - -1 if we're not
      currently editing}
    procedure UpdateButtons;
      {Updates all buttons according to the state of other controls}
    procedure DeleteProject(Idx: Integer);
      {Deletes project in list box at given index and updates highlighting and
      buttons}
    procedure MoveItem(const MoveItem: Integer; MoveBy: Integer);
      {Moves the given list item in the direction given by MoveBy (+1 moves down
      the list, -1 moves up the list and any other value is an error). The
      highlight stays with the moved item and the list view is scrolled as
      required to keep the item in view}
  public
    class function EditUninstProjectList(const AOwner: TComponent;
      var AProjList: string): Boolean;
      {Display dialog box that allows list of projects to be uninstalled by
      installer to be created or edited. The list is passed as a semi-colon
      delimited string. If user OKs the updated list passed back in same
      parameter and true is returned. If user cancels project list is unchanged
      and false is returned}
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UHelpContexts, UStringProcs;


resourcestring
  // Button captions
  sUpdateBtnCaption = '&Update';
  sAddBtnCaption = '&Add';


{$R *.DFM}

procedure TUninstProjectsDlg.btnAddOrUpdateClick(Sender: TObject);
  {Add or Update button click event handler: either adds the item in the Project
  combo box to the end of the projects list (if entering a new project) or
  updates the existing project being edited}
begin
  if fEditingProjIdx > -1 then
    // We're editing an existing project: update it in list box
    lbProjects.Items[fEditingProjIdx] := edProject.Text
  else
    // We're adding a new project: add to end of list box
    lbProjects.Items.Add(edProject.Text);
  // Clear project from the combo box
  edProject.Text := '';
  // Record that we are no longer editing a project: we're back in "Add" mode
  fEditingProjIdx := -1;
  // Update buttons and return focus to combo box
  UpdateButtons;
  edProject.SetFocus;
end;

procedure TUninstProjectsDlg.btnDeleteClick(Sender: TObject);
  {Delete button click event handler: deletes the highlighted project in the
  list box}
begin
  // Cancel any editing
  fEditingProjIdx := -1;
  edProject.Text := '';
  // Delete the item
  DeleteProject(lbProjects.ItemIndex);
  // Update buttons according to new state of list box
  UpdateButtons;
end;

procedure TUninstProjectsDlg.btnEditClick(Sender: TObject);
  {Edit button click event handler: copy selected project in list box into
  combo box for editing}
begin
  // Check button is enabled: may be called from elsewhere
  if btnEdit.Enabled then
  begin
    // Record index of the project we're editing
    fEditingProjIdx := lbProjects.ItemIndex;
    // Copy project into combo box
    edProject.Text := lbProjects.Items[lbProjects.ItemIndex];
    // Update buttons to show that we're editing a project rather than adding
    UpdateButtons;
    // Give the combo box the focus
    edProject.SetFocus;
  end;
end;

procedure TUninstProjectsDlg.DeleteProject(Idx: Integer);
  {Deletes project in list box at given index and updates highlighting and
  buttons}
var
  SelItem: Integer; // selected item: to be deleted
begin
  // Record currently selected project index: which must not be -1
  SelItem := lbProjects.ItemIndex;
  Assert(SelItem > -1);
  // Delete the project
  lbProjects.Items.Delete(SelItem);
  // Highlight following (if any - if not previous) item
  if SelItem >= lbProjects.ItemIndex then
    Dec(SelItem);
  lbProjects.ItemIndex := SelItem;
  // Update buttons re new selection
  UpdateButtons;
end;

class function TUninstProjectsDlg.EditUninstProjectList(
  const AOwner: TComponent; var AProjList: string): Boolean;
  {Display dialog box that allows list of projects to be uninstalled by
  installer to be created or edited. The list is passed as a semi-colon
  delimited string. If user OKs the updated list passed back in same parameter
  and true is returned. If user cancels project list is unchanged and false is
  returned}
const
  cListSep = ';'; // list separator
var
  ProjList: string; // copy of string of projects used for modification
begin
  // Create dialog box
  with TUninstProjectsDlg.Create(AOwner) do
    try
      // Store projects, one per line in list box
      ProjList := AProjList;
      UStringProcs.SplitStr(ProjList, cListSep, lbProjects.Items, False);
      // Display dialog box and get user reponse
      Result := ShowModal = mrOK;
      if Result then
        // User OKd: built project list that is returned
        AProjList := UStringProcs.JoinStr(lbProjects.Items, cListSep, False);
    finally
      Free;
    end;
end;

procedure TUninstProjectsDlg.edProjectChange(Sender: TObject);
  {Project edit change event handler: simply update the buttons according to
  contents}
begin
  UpdateButtons;
end;

procedure TUninstProjectsDlg.FormCreate(Sender: TObject);
  {Form creation event handler: record we're not editing a project name}
begin
  inherited;
  fEditingProjIdx := -1;
  HelpContext := IDH_DLG_UNINSTPROJECTS;
  UpdateButtons;
end;

procedure TUninstProjectsDlg.lbProjectsClick(Sender: TObject);
  {Project list click event handler: check if we're editing the current project
  and clear the project combo box if not}
begin
  inherited;
  if fEditingProjIdx > -1 then
  begin
    fEditingProjIdx := -1;
    edProject.Text := '';
  end;
  UpdateButtons;
end;

procedure TUninstProjectsDlg.lbProjectsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  {OnKeyDown event handler for projects list view: promotes demotes current
  project in list if Ctrl+Up or Ctrl+Down are pressed}
begin
  if ssCtrl in Shift then
  begin
    // Ctrl was pressed: check for our keys
    case Key of
      VK_UP:    sbtnUp.Click;
      VK_DOWN:  sbtnDown.Click;
    end;
    // Don't pass key press thru to prevent usual Up+Down key actions
    Key := 0;
  end;
end;

procedure TUninstProjectsDlg.MoveItem(const MoveItem: Integer;
  MoveBy: Integer);
  {Moves the given list item in the direction given by MoveBy (+1 moves down the
  list, -1 moves up the list and any other value is an error). The highlight
  stays with the moved item and the list view is scrolled as required to keep
  the item in view}
var
  NewIdx: Integer;    // index in list where new item to be inserted
begin
  Assert((MoveBy = -1) or (MoveBy = +1));
  Assert((MoveItem > -1) and (MoveItem <= lbProjects.Items.Count));
  lbProjects.Items.BeginUpdate;
  try
    // Decide what item to swap with
    NewIdx := MoveItem + MoveBy;
    // Do the swap and make highlight track to item we moved
    lbProjects.Items.Exchange(MoveItem, NewIdx);
    lbProjects.ItemIndex := NewIdx;
  finally
    lbProjects.Items.EndUpdate;
  end;
  // Update buttons re new position of item
  UpdateButtons;
end;

procedure TUninstProjectsDlg.sbtnDownClick(Sender: TObject);
  {OnClick event handler for Down button: moves selected list item down list}
var
  SelItem: Integer; // index of selected item
begin
  inherited;
  // Check button is enabled: sometimes called from other handlers
  if sbtnDown.Enabled then
  begin
    // Get selected item which must not be top one
    SelItem := lbProjects.ItemIndex;
    Assert((SelItem > -1) and (SelItem < Pred(lbProjects.Items.Count)));
    // Move the item down the list
    MoveItem(SelItem, 1);
  end;
end;

procedure TUninstProjectsDlg.sbtnUpClick(Sender: TObject);
  {OnClick event handler for Up button: moves selected list item up list}
var
  SelItem: Integer; // index of selected item
begin
  inherited;
  // Check button is enabled: sometimes called from other handlers
  if sbtnUp.Enabled then
  begin
    // Get selected item which must not be last one
    SelItem := lbProjects.ItemIndex;
    Assert((SelItem > 0) and (SelItem <= Pred(lbProjects.Items.Count)));
    // Move the item up the list
    MoveItem(SelItem, -1);
  end;
end;

procedure TUninstProjectsDlg.UpdateButtons;
  {Updates all buttons according to the state of other controls}
var
  SelItem: Integer; // the currently selected list item
begin
  // Record selected item
  SelItem := lbProjects.ItemIndex;
  // Enable / disable buttons as required
  btnAddOrUpdate.Enabled := edProject.Text <> '';
  btnEdit.Enabled := SelItem > -1;
  btnDelete.Enabled := SelItem > -1;
  sbtnUp.Enabled := SelItem > 0;
  sbtnDown.Enabled := (SelItem > -1)
    and (SelItem < Pred(lbProjects.Items.Count));
  // Display appropriate caption on Add/Update button according to if editing
  // a project or adding a new one
  if fEditingProjIdx = -1 then
    btnAddOrUpdate.Caption := sAddBtnCaption
  else
    btnAddOrUpdate.Caption := sUpdateBtnCaption;
end;

end.
