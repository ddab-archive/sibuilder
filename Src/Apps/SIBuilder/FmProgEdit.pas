{ ##     
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmProgEdit.pas
  @COMMENTS                 Implements a dialog box that lets user specify a
                            program and a set of command line parameters to be
                            run during the installation.
  @DEPENDENCIES             Requires following components:
                            + Custom SIBuilder TNewGroupBox component v1.0
                            + Custom SIBuilder TFileComboBox component v1.0\
                            The following custom SIBuilder only components are
                            required:
                            + THotText 1.0\
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 04/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/12/2000
      @COMMENTS             + Changed dlg box to display installed files as well
                              as path macros.
                            + Also added support for new property that
                              determines if program is run on installation,
                              uninstallation or both.
                            + Now only have one combo box for program name and
                              path.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 25/06/2000
      @COMMENTS             Deleted unused reference to FmChooseFolderDlg unit.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Major update to deal with revised logic of dealing
                            with programs to be run:
                            + Removed support for flagging programs for deletion
                              after being run - any program (or other file) to
                              be deleted after installer completes is now
                              flagged on Files page. Programs combo logic now
                              prevents selection on options to run program in
                              uninstaller if program file is to be deleted by
                              installer.
                            + Replaced parameters combo box with edit box (combo
                              box's drop down list never had any entries!)
                            + Added "All files" option to previous "Program
                              Files" file type in browse dialog box.
                            + Changed program combo from standard combo to new
                              custom action combo with a "Browse..." option that
                              automatically displays file open dialog and
                              removed browse button from form.
                            + Replaced list box used to display parameters with
                              a list view that can display parameter icon or
                              error icon when there's a problem.
                            + Added means to validate program and parameters and
                              to visually inform user of problems and give
                              access to relevant pop-up help. Validation and
                              warnings are handled by special classes and a list
                              of temporary files along with a flag indicating
                              presence of uninstaller in project is now passed
                              to EditRunProg method to assist in validation.
                            + Improved handling of open dialog - now uses
                              current entry in program combo as default folder
                              and file in dialog.
                            + Removed a duplicate open dlg control from form.
                            + Fixed bug in code that displays full path hint for
                              path macros - was raising exception if any invalid
                              path macro is entered. We use new code in
                              UGUIHelper.
                            + Moved string literals to resource strings
                            + Re-ordered tabbing
                            + Replaced TGroupBox control with TNewGroupBox
                              custom control that changes appearance when
                              disabled.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
                            + Added up and down buttons to enable parameters to
                              be re-ordered.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/11/2003
      @COMMENTS             Changed creation of error handling "warner" object
                            to use new hot text controls insted of rich text
                            controls.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 20/02/2008
      @COMMENTS             Replaced usage of ResIds.inc and Help.inc include
                            files with UResources and UHelpContexts units.
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
 * The Original Code is FmProgEdit.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmProgEdit;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes, Buttons, ComCtrls, ExtCtrls, ImgList, Dialogs,
  // Project specific components
  CmpActionComboBox, CmpFileEditors, CmpGroupBox, CmpHotText,
  // Project
  FmGenericOKDlg, URunProgs, UCommonTypes, URunProgValidator, UWarner;

type

  {
  TEditProgDlg:
    Dialog box that lets user specify a program and a set of command line
    parameters to be run during the installation. The dlg box inherits OK,
    Cancel and help buttons from ancestor classes.

    Inheritance: TEditProgDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TEditProgDlg = class(TGenericOKDlg)
    dlgAddProg: TOpenDialog;
    gpProg: TNewGroupBox;
    lblProgName: TLabel;
    lblWhenExec: TLabel;
    cbWhenRun: TComboBox;
    acbProgName: TFileComboBox;
    gpParam: TNewGroupBox;
    lblParam: TLabel;
    lblParams: TLabel;
    btnAddOrUpdate: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    edParam: TEdit;
    lvParams: TListView;
    ilSmall: TImageList;
    sbtnUp: TSpeedButton;
    sbtnDown: TSpeedButton;
    htProgWarning: THotText;
    htParamWarning: THotText;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure btnAddOrUpdateClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edParamChange(Sender: TObject);
    procedure acbProgNameAfterBrowse(Sender: TObject; var FileSpec: String);
    procedure acbProgNameBeforeBrowse(Sender: TObject; var FileSpec: String);
    procedure acbProgNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvParamsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbWhenRunChange(Sender: TObject);
    procedure sbtnUpClick(Sender: TObject);
    procedure sbtnDownClick(Sender: TObject);
    procedure lvParamsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fValidator: TRunProgValidator;
      {Object used to check validity of program and parameter entries}
    fParamWarner: TWarner;
      {Object used to record whether parameters are invalid and to warn the user
      if so and provide access to help}
    fProgWarner: TWarner;
      {Object used to record whether the program being edited is invalid and to
      warn the user if so and provide access to help}
    fEditingParamItem: TListItem;
      {Reference the the list item in parameter list view that has been placed
      in the parameter edit box for editing, or nil if there is no such
      parameter or the parameter being edited is new}
    fRunProg: TRunProg;
      {Reference to the program object being edited}
    fRunFiles: TStrings;
      {List of installed programs that can be run: Objects[] property references
      related TFileInfo objects}
    fTempFiles: TStrings;
      {List of temporary files in install project: Objects[] property references
      related TFileInfo objects}
    fHaveUninstaller: Boolean;
      {Flag true if the project includes an uninstall program: this fact is used
      when validating programs}
    procedure SetParamEntries(const Params: TStrings);
      {Updates list view to display the parameters associated with program being
      edited}
    procedure MoveParamItem(const MoveItem: TListItem; MoveBy: Integer);
      {Moves the given list item in the direction given by MoveBy (+1 moves down
      the list, -1 moves up the list and any other value is an error). The
      highlight stays with the moved item and the list view is scrolled as
      required to keep the item in view}
    procedure UpdateParamList(LI: TListItem; const Param: string);
      {Updates the given item in the parameter list with given parameter}
    procedure DeleteParam(LI: TListItem);
      {Deletes the given list item and the associated parameter}
    procedure ValidateParam(LI: TListItem);
      {Validates the parameter associated with the given list view item and
      registers any errors}
    procedure ValidateParams;
      {Validates all parameters displayed in list view, registering errors as
      required}
    procedure ValidateProgramOnly;
      {Validates the program, without checking the parameters. Registers any
      errors}
    procedure UpdateButtons;
      {Updates all buttons according to the state of other controls}
    procedure UpdateProgram(const FileName: string);
      {Updates list of options of when program can be run according to if
      program is temporary (i.e. deleted after installer has run). Also updates
      program combo box hint to expand any path macros in given FileName path}
    procedure SelectWhenRunItem(const WhenRun: TRunSchedule);
      {Select drop-down list item that corresponds to given run schedule}
  public
    class function EditRunProg(const Owner: TComponent;
      const RunProg: TRunProg; const RunFiles, TempFiles: TStrings;
      HaveUninstaller: Boolean): Boolean;
      {Displays the dialog box and allows the user to edited the details of the
      program associated with the given run program object. A list of executable
      programs and temporary files in the install project are provided, along
      with a flag telling if the project includes an uninstall program, as an
      aid to validation}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Graphics,
  // Project
  UFileProcs, UPathMacros, UGUIHelper, UMessageBox, UImageList, UResources,
  UHelpContexts;


{$R *.DFM}


resourcestring
  // Button captions
  sUpdateBtnCaption = '&Update';
  sAddBtnCaption = '&Add';
  // Combo box entries
  sRunOnInstallOnly = 'Installation only';
  sRunOnUninstallOnly = 'Uninstallation only';
  sRunOnBoth = 'Installation and Uninstallation';
  // Error messages
  sErrNoProgName = 'You must provide a program name';


{ TEditProgDlg }

procedure TEditProgDlg.acbProgNameAfterBrowse(Sender: TObject;
  var FileSpec: String);
  {OnAfterBrowse event handler for program combo box: replaces any recognised
  paths in new file spec with path macros}
begin
  inherited;
  FileSpec := PathMacros.PathToMacro(FileSpec);
end;

procedure TEditProgDlg.acbProgNameBeforeBrowse(Sender: TObject;
  var FileSpec: String);
  {OnBeforeBrowse event handler for program combo box: expands any path macros
  before displaying file dialog}
begin
  inherited;
  FileSpec := PathMacros.ExpandMacroPath(FileSpec);
end;

procedure TEditProgDlg.acbProgNameChange(Sender: TObject);
  {Program name path combo box change event handler: updates validation and
  path macro expansion hints}
begin
  inherited;
  UpdateProgram(acbProgName.Text);
end;

procedure TEditProgDlg.btnAddOrUpdateClick(Sender: TObject);
  {Add or Update button click event handler - either adds the item in the
  Parameter combo box to the end of the parameter list view (if entering a new
  parameter) or updates the existing parameter being edited}
begin
  if Assigned(fEditingParamItem) then
    // We're editing an existing parameter - update it in list view
    UpdateParamList(fEditingParamItem, edParam.Text)
  else
    // We're adding a new parameter - add to end of list view
    UpdateParamList(lvParams.Items.Add, edParam.Text);
  // Clear parameter from the combo box
  edParam.Text := '';
  // Record that we are no longer editing a parameter - we're back in "Add" mode
  fEditingParamItem := nil;
  // Update buttons and return focus to combo box
  UpdateButtons;
  edParam.SetFocus;
end;

procedure TEditProgDlg.btnDeleteClick(Sender: TObject);
  {Delete button click event handler - deletes the highlighted parameter in the
  list box}
begin
  // Cancel any editing
  fEditingParamItem := nil;
  edParam.Text := '';
  // Delete the item
  DeleteParam(lvParams.Selected);
  // Update buttons according to new state of list box
  UpdateButtons;
end;

procedure TEditProgDlg.btnEditClick(Sender: TObject);
  {Edit button click event handler: copy selected parameter in list box into
  combo box for editing}
begin
  // Record index of the paraeter we're editing
  fEditingParamItem := lvParams.Selected;
  // Copy parameter into combo box
  edParam.Text := lvParams.Selected.Caption;
  // Update buttons to show that we're editing a parameter rather than adding
  UpdateButtons;
  // Give the combo box the focus
  edParam.SetFocus;
end;

procedure TEditProgDlg.cbWhenRunChange(Sender: TObject);
  {OnChange event handler for Execute on combo: Updates valiation of program
  and parameters depending on new value}
begin
  inherited;
  ValidateProgramOnly;
  ValidateParams;
end;

procedure TEditProgDlg.DeleteParam(LI: TListItem);
  {Deletes the given list item and the associated parameter}
var
  SelItem: TListItem;   // selected item: to be deleted
  NextItem: TListItem;  // item to be selected after SelItem deleted
begin
  // Record currently selected item: which must not be nil
  SelItem := lvParams.Selected;
  Assert(Assigned(SelItem));
  // Get item to highlight after selected item is deleted
  // try to get next item in list
  NextItem := lvParams.GetNextItem(SelItem, sdBelow, [isNone]);
  if not Assigned(NextItem) then
    // there is no next item, try to get previous
    NextItem := lvParams.GetNextItem(SelItem, sdAbove, [isNone]);
  // ensure not selected item again (can happen and causes GPF in next line)
  if NextItem = SelItem then
    NextItem := nil;
  // Ensure item is not registered as an error
  fParamWarner.UnregisterWarning(LI);
  // Do the deletion
  LI.Delete;
  // Select the next item (per above)
  lvParams.Selected := NextItem;
  // Update buttons re new selection
  UpdateButtons;
end;

class function TEditProgDlg.EditRunProg(const Owner: TComponent;
  const RunProg: TRunProg; const RunFiles, TempFiles: TStrings;
  HaveUninstaller: Boolean): Boolean;
  {Displays the dialog box and allows the user to edited the details of the
  program associated with the given run program object. A list of executable
  programs and temporary files in the install project are provided, along
  with a flag telling if the project includes an uninstall program, as an
  aid to validation}
var
  Dlg: TEditProgDlg;  // dlg box instance
  Idx: Integer;       // loops through params in list view
begin
  // Create the dialog box
  Dlg := TEditProgDlg.Create(Owner);
  try
    // Pass the function parameters to dialog box
    Dlg.fRunProg := RunProg;
    Dlg.fRunFiles := RunFiles;
    Dlg.fTempFiles := TempFiles;
    Dlg.fHaveUninstaller := HaveUninstaller;
    // Display dlg box and record whether user OKs
    Result := Dlg.ShowModal = mrOK;
    if Result then
    begin
      // User OKd - update program object per dlg box controls
      // program name and install path
      RunProg.ProgName := ExtractFileName(Dlg.acbProgName.Text);
      RunProg.ExecPath := ExtractFileDir(Dlg.acbProgName.Text);
      // parameters
      RunProg.Params.Clear;
      for Idx := 0 to Pred(Dlg.lvParams.Items.Count) do
        RunProg.Params.Add(Dlg.lvParams.Items[Idx].Caption);
      // run details
      RunProg.WhenRun := TRunSchedule(
        Dlg.cbWhenRun.Items.Objects[Dlg.cbWhenRun.ItemIndex]
      );
    end;
  finally
    // Get rid of dlg box
    Dlg.Free;
  end;
end;

procedure TEditProgDlg.edParamChange(Sender: TObject);
  {Parameter edit change event handler: simply update the buttons according to
  contents}
begin
  UpdateButtons;
end;

procedure TEditProgDlg.FormCreate(Sender: TObject);
  {Form creation event handler: initialise components and fields and creates
  owned validation and error reporting objects}
begin
  inherited;

  // Load image list from resources
  ilSmall.ResourceLoad(rtBitmap, cSmallImages, clFuchsia);
  Assert(ilSmall.Count > 0);

  // Create objects that manage warnings about suspicious parameters
  fParamWarner := TWarner.Create(htParamWarning, cPrgDlgParamErrResID);
  fProgWarner := TWarner.Create(htProgWarning, cPrgDlgProgErrResID);

  // Set help context
  HelpContext := IDH_DLG_PROGEDIT;
  dlgAddProg.HelpContext := IDH_DLG_PROGEDITOPEN;

  // Record that we're not editing an existing parameter
  fEditingParamItem := nil;

  // Set option for when program run
  with cbWhenRun.Items do
  begin
    AddObject(sRunOnInstallOnly, Pointer(rsdOnInstall));
    AddObject(sRunOnUninstallOnly, Pointer(rsdOnUninstall));
    AddObject(sRunOnBoth, Pointer(rsdBoth));
  end;
end;

procedure TEditProgDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler: freed owned objects}
begin
  inherited;
  fValidator.Free;
  fProgWarner.Free;
  fParamWarner.Free;
end;

procedure TEditProgDlg.FormShow(Sender: TObject);
  {Form show event handler: updates controls according to selected program}
begin
  inherited;
  // Create object that validates parameters
  fValidator := TRunProgValidator.Create(fTempFiles);
  // Set controls according to given program object
  // program name combo gets list of path macros and current install files
  // along with action text item that leads to file browse dialog box
  PathMacros.GetMacroNames(acbProgName.Items);
  acbProgName.Items.AddStrings(fRunFiles);
  acbProgName.Items.Add(acbProgName.ActionText);
  acbProgName.Text := MakePathName(fRunProg.ExecPath) + fRunProg.ProgName;
  // select required item in Execute combo box (items added on creation)
  SelectWhenRunItem(fRunProg.WhenRun);
  // set the parameters per for the program
  SetParamEntries(fRunProg.Params);
  // Update buttons
  UpdateButtons;
  // Update program options
  UpdateProgram(acbProgName.Text);
end;

procedure TEditProgDlg.lvParamsClick(Sender: TObject);
  {Parameter list view click event handler: check if we're editing the current
  paraemter and clear the parameter combo box if not}
begin
  inherited;
  if Assigned(fEditingParamItem) then
  begin
    fEditingParamItem := nil;
    edParam.Text := '';
  end;
  UpdateButtons;
end;

procedure TEditProgDlg.lvParamsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {OnKeyDown event handler for parameters list view: promotes demotes current
  topic in list if Ctrl+Up or Ctrl+Down are pressed}
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

procedure TEditProgDlg.MoveParamItem(const MoveItem: TListItem;
  MoveBy: Integer);
  {Moves the given list item in the direction given by MoveBy (+1 moves down
  the list, -1 moves up the list and any other value is an error). The highlight
  stays with the moved item and the list view is scrolled as required to keep
  the item in view}
var
  InsIdx: Integer;    // index in list where new item to be inserted
  NewItem: TListItem; // reference to new item used to move given item
begin
  Assert((MoveBy = -1) or (MoveBy = +1));
  Assert(Assigned(MoveItem));
  lvParams.Items.BeginUpdate;
  try
    // Decide where to insert
    InsIdx := MoveItem.Index + MoveBy;
    if MoveBy > 0 then
      // since insertion is before given item, add extra one for move down list
      Inc(InsIdx);
    // Create new item at required position in list and copy our item to it
    NewItem := lvParams.Items.Insert(InsIdx);
    NewItem.Assign(MoveItem);
    // Delete old item being moved
    MoveItem.Delete;
    // Kludge: set CheckBoxes property back to false: assignment sets it true!
    lvParams.CheckBoxes := False;
    // Select the item at new position and make sure we can see it
    lvParams.Selected := NewItem;
    if Assigned(lvParams.Selected) then
      lvParams.Selected.MakeVisible(False);
  finally
    lvParams.Items.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TEditProgDlg.OKBtnClick(Sender: TObject);
  {OK button click event handler: validate data entered by user and close dlg if
  all OK or display message if error}
begin
  inherited;
  ModalResult := mrNone;
  if acbProgName.Text = '' then
    TMessageBox.Error(Self, sErrNoProgName)
  else
    ModalResult := mrOK;
end;

procedure TEditProgDlg.sbtnDownClick(Sender: TObject);
  {OnClick event handler for Down button: moves selected list item down list}
var
  SelItem: TListItem; // selected item
begin
  inherited;
  // Check button is enabled: sometimes called from other handlers
  if sbtnDown.Enabled then
  begin
    // Get selected item which must not be top one
    SelItem := lvParams.Selected;
    Assert(Assigned(SelItem) and (SelItem.Index < Pred(lvParams.Items.Count)));
    // Move the item down the list
    MoveParamItem(SelItem, 1);
  end;
end;

procedure TEditProgDlg.sbtnUpClick(Sender: TObject);
  {OnClick event handler for Up button: moves selected list item up list}
var
  SelItem: TListItem; // selected item
begin
  inherited;
  // Check button is enabled: sometimes called from other handlers
  if sbtnUp.Enabled then
  begin
    // Get selected item which must not be last one
    SelItem := lvParams.Selected;
    Assert(Assigned(SelItem) and (SelItem.Index > 0));
    // Move the item up the list
    MoveParamItem(SelItem, -1);
  end;
end;

procedure TEditProgDlg.SelectWhenRunItem(const WhenRun: TRunSchedule);
  {Select drop-down list item that corresponds to given run schedule}
var
  Index: Integer; // loops thru drop-down list items
begin
  // Select no item by default
  cbWhenRun.ItemIndex := -1;
  // Find corresponding drop-down list item: run schedule stored in Objects[]
  for Index := 0 to Pred(cbWhenRun.Items.Count) do
    if TRunSchedule(cbWhenRun.Items.Objects[Index]) = WhenRun then
    begin
      cbWhenRun.ItemIndex := Index;
      Break;
    end;
  // Must have a selected item
  Assert(cbWhenRun.ItemIndex <> -1);
end;

procedure TEditProgDlg.SetParamEntries(const Params: TStrings);
  {Updates list view to display the parameters associated with program being
  edited}
var
  Idx: Integer; // loops thru all parameters
begin
  // Clear list and add entry for each parameter
  lvParams.Items.Clear;
  for Idx := 0 to Pred(Params.Count) do
    UpdateParamList(lvParams.Items.Add, Params[Idx]);
end;

procedure TEditProgDlg.UpdateButtons;
  {Updates all buttons according to the state of other controls}
var
  SelItem: TListItem; // the currently selected list item
begin
  // Record selected item
  SelItem := lvParams.Selected;
  // Enable / disable buttons as required
  btnAddOrUpdate.Enabled := edParam.Text <> '';
  btnEdit.Enabled := Assigned(SelItem);
  btnDelete.Enabled := Assigned(SelItem);
  sbtnUp.Enabled := Assigned(SelItem) and (SelItem.Index > 0);
  sbtnDown.Enabled := Assigned(SelItem)
    and (SelItem.Index < Pred(lvParams.Items.Count));
  // Display appropriate caption on Add/Update button according to if editing
  // a param or adding a new one
  if not Assigned(fEditingParamItem) then
    btnAddOrUpdate.Caption := sAddBtnCaption
  else
    btnAddOrUpdate.Caption := sUpdateBtnCaption;
end;

procedure TEditProgDlg.UpdateParamList(LI: TListItem; const Param: string);
  {Updates the given item in the parameter list with given parameter}
begin
  // Record param in list view
  LI.Caption := Param;
  // Validate the parameter
  ValidateParam(LI);
end;

procedure TEditProgDlg.UpdateProgram(const FileName: string);
  {Updates list of options of when program can be run according to if program
  is temporary (i.e. deleted after installer has run). Also updates program
  combo box hint to expand any path macros in given FileName path}
begin
  // Get hint that's an expansion of any path macros in combo box
  acbProgName.Hint := PathMacroCtrlHint(acbProgName.Text);
  // Validate program and parameter list
  ValidateProgramOnly;
  ValidateParams;
end;

procedure TEditProgDlg.ValidateParam(LI: TListItem);
  {Validates the parameter associated with the given list view item and
  registers any errors}
var
  ParamStr: string;   // name of parameter
begin
  Assert(Assigned(LI));
  // Get the parameter name from list item caption
  ParamStr := LI.Caption;
  // Do the validation and update warnings
  if fValidator.ValidateParam(
    ParamStr,
    TRunSchedule(cbWhenRun.Items.Objects[cbWhenRun.ItemIndex])
  ) then
  begin
    // parameter OK: make sure it's not registered as error give valid icon
    fParamWarner.UnregisterWarning(LI);
    LI.ImageIndex := UImageList.cParamImg;
  end
  else
  begin
    // parameter in error: register the error and give the bad icon
    fParamWarner.RegisterWarning(LI);
    LI.ImageIndex := UImageList.cBadParamNodeImageIndex;
  end;
end;

procedure TEditProgDlg.ValidateParams;
  {Validates all parameters displayed in list view, registering errors as
  required}
var
  Idx: Integer; // loops thru all items in param list view
begin
  for Idx := 0 to Pred(lvParams.Items.Count) do
    ValidateParam(lvParams.Items[Idx]);
end;

procedure TEditProgDlg.ValidateProgramOnly;
  {Validates the program, without checking the parameters. Registers any errors}
begin
  // Check program using validator and register/unregister warnings as required
  if fValidator.ValidateProgOnly(
    acbProgName.Text,
    TRunSchedule(cbWhenRun.Items.Objects[cbWhenRun.ItemIndex]),
    fHaveUninstaller
  ) then
    fProgWarner.UnregisterWarning(Self)
  else
    fProgWarner.RegisterWarning(Self);
end;

end.
