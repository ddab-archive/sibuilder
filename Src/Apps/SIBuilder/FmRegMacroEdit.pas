{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmRegMacroEdit.pas
  @COMMENTS                 Implements a dialog box used to edit the values
                            associated with registry template macros.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.2
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
 * The Original Code is FmRegMacroEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmRegMacroEdit;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, URegTemplates;

type

  {
  TRegMacroEditDlg:
    Dialog box used to edit the values associated with registry template macros.

    Inheritance: TSimpleEditDlg -> TGenericViewDlg -> TGenericDlg -> [TForm]
  }
  TRegMacroEditDlg = class(TGenericViewDlg)
    lblMacros: TLabel;
    cbMacros: TComboBox;
    lblDesc: TLabel;
    edDesc: TEdit;
    lblValue: TLabel;
    edValue: TEdit;
    lbValues: TListBox;
    btnAddUpdt: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnClear: TButton;
    procedure cbMacrosChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure btnAddUpdtClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lbValuesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fMacros: TRegMacroList;
      {List of macros}
    fCurrentMacro: TRegMacroItem;
      {Reference to macro currently being edited}
    procedure UpdateControls;
      {Updates state of controls to reflect current entries}
    procedure UpdateCurrentMacro;
      {Updates the macro that is currently displayed with new values per
      display}
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  UHelpContexts;


{$R *.DFM}

resourcestring
  // Button captions
  sAddCaption = '&Add';
  sUpdateCaption = '&Update';


{ TRegMacroEditDlg }

procedure TRegMacroEditDlg.btnAddUpdtClick(Sender: TObject);
  {OnClick event handler for add/update button: either adds new value from value
  edit control to list or updates current list item with the value, depending
  on whether acting as add or update button}
var
  NewIdx: Integer;  // index of new item in list
begin
  inherited;
  // Update list box according to button state
  if btnAddUpdt.Caption = sAddCaption then
  begin
    // Add the new value to list box: just highlight if already exists
    NewIdx := lbValues.Items.IndexOf(edValue.Text);
    if NewIdx = -1 then
      lbValues.ItemIndex := lbValues.Items.Add(edValue.Text)
    else
      lbValues.ItemIndex := NewIdx;
  end
  else
  begin
    // Update the current list item with new value
    lbValues.Items[lbValues.ItemIndex] := edValue.Text;
  end;
  // Clear edit box and refocus ready for next entry
  edValue.Text := '';
  edValue.SetFocus;
  // Update state of controls
  UpdateControls;
end;

procedure TRegMacroEditDlg.btnClearClick(Sender: TObject);
  {OnClick event handler for Clear button: clears all values from list}
begin
  inherited;
  lbValues.Clear;
  lbValues.ItemIndex := -1;
  UpdateControls;
end;

procedure TRegMacroEditDlg.btnDeleteClick(Sender: TObject);
  {OnClick event handler for Delete button: deletes current value from list}
var
  Idx: Integer; // current list index
begin
  inherited;
  // Delete current list item
  Idx := lbValues.ItemIndex;
  Assert(Idx <> -1);
  lbValues.Items.Delete(Idx);
  // Highlight another list item
  if Idx = lbValues.Items.Count then
    Dec(Idx);
  lbValues.ItemIndex := Idx;
  // Update state of controls
  UpdateControls;
end;

procedure TRegMacroEditDlg.btnEditClick(Sender: TObject);
  {OnClick event handler for edit button (also used for double clicks on list):
  copies current list item into value edit box for updating}
var
  Idx: Integer; // current list item
begin
  inherited;
  // Copy current list item to edit box and give it focus
  Idx := lbValues.ItemIndex;
  Assert(Idx <> -1);
  edValue.Text := lbValues.Items[Idx];
  edValue.SetFocus;
  // Set add/update button caption to update and update state of controls
  btnAddUpdt.Caption := sUpdateCaption;
  UpdateControls;
end;

procedure TRegMacroEditDlg.cbMacrosChange(Sender: TObject);
  {OnChange event handler for macros combo box: displays description of selected
  macro and all values associated with the macro}
var
  ItemIdx: Integer; // selected item in combo box
  Idx: Integer;     // loops thru all of macro's values
begin
  inherited;
  // Store any changes to macro currently displayed
  UpdateCurrentMacro;
  // Record reference to newly selected macro
  ItemIdx := cbMacros.ItemIndex;
  Assert(ItemIdx > -1);
  fCurrentMacro := fMacros.FindMacro(cbMacros.Items[ItemIdx]);
  Assert(Assigned(fCurrentMacro));
  // Display information about new macro
  // show description
  edDesc.Text := fCurrentMacro.Desc;
  // display all values and ensure none are selected
  lbValues.Clear;
  for Idx := 0 to Pred(fCurrentMacro.Values.Count) do
    lbValues.Items.Add(fCurrentMacro.Values[Idx]);
  lbValues.ItemIndex := -1;
  // clear value edit box
  edValue.Text := '';
  // Update state of controls
  UpdateControls;
end;

procedure TRegMacroEditDlg.edValueChange(Sender: TObject);
  {OnChange event handler for value edit box: updates control state according to
  whether control contains any text}
begin
  inherited;
  UpdateControls;
end;

procedure TRegMacroEditDlg.FormCreate(Sender: TObject);
  {Form creation event handler: create owned list of macros and displays them
  in combo box}
var
  Idx: Integer; // loops thru all macros
begin
  inherited;
  // Record help context
  HelpContext := IDH_DLG_REGMACROS;
  // Create macros list object (reads itself from registry)
  fMacros := TRegMacroList.Create;
  // Display macros in combo box
  cbMacros.Items.Clear;
  for Idx := 0 to Pred(fMacros.Count) do
    cbMacros.Items.Add(fMacros[Idx].Name);
  if cbMacros.Items.Count > 0 then
  begin
    // we have items in macros combo: select first item and display it
    cbMacros.ItemIndex := 0;
    cbMacrosChange(cbMacros);   // display by triggering OnChange handler
  end;
  // Update controls
  UpdateControls;
end;

procedure TRegMacroEditDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler: updates macro object and frees it}
begin
  // Update current macro object with any changes
  UpdateCurrentMacro;
  // Free the macro list (this writes informatin to registry)
  fMacros.Free;
  inherited;
end;

procedure TRegMacroEditDlg.lbValuesClick(Sender: TObject);
  {OnClick event handler for Values list box: updates controls to reflect state
  of list box}
begin
  inherited;
  UpdateControls;
end;

procedure TRegMacroEditDlg.UpdateControls;
  {Updates state of controls to reflect current entries}
begin
  // Add/update button enabled only if there's text in value edit
  btnAddUpdt.Enabled := edValue.Text <> '';
  if edValue.Text = '' then
    // when edit control is empty add/update button is placed in add mode
    btnAddUpdt.Caption := sAddCaption;
  // Edit & Delete buttons enabled only if a value is selected in list box
  btnEdit.Enabled := lbValues.ItemIndex <> -1;
  btnDelete.Enabled := lbValues.ItemIndex <> -1;
  // Clear button selected only if list box not empty
  btnClear.Enabled := lbValues.Items.Count > 0;
  // Value edit and list box enabled only if we have a macro selected
  // we grey out the control backgrounds when disabled
  edValue.Enabled := cbMacros.ItemIndex <> -1;
  if edValue.Enabled then
    edValue.Color := clWindow
  else
    edValue.ParentColor := True;
  lbValues.Enabled := cbMacros.ItemIndex <> -1;
  if lbValues.Enabled then
    lbValues.Color := clWindow
  else
    lbValues.ParentColor := True;
end;

procedure TRegMacroEditDlg.UpdateCurrentMacro;
  {Updates the macro that is currently displayed with new values per display}
var
  Idx: Integer;           // loops thru all values in macro
  OldCurrentIdx: Integer; // index of current value in macro
begin
  if Assigned(fCurrentMacro) then
  begin
    // Record old current value index for later restoration
    OldCurrentIdx := fCurrentMacro.CurrentIdx;
    // Clear macro's values
    fCurrentMacro.Values.Clear;
    // Update macro with values per list box
    for Idx := 0 to Pred(lbValues.Items.Count) do
      fCurrentMacro.Values.Add(lbValues.Items[Idx]);
    // Restore current value index (macro adjust if now out of range)
    fCurrentMacro.CurrentIdx := OldCurrentIdx;
    // Clear reference to current macro so we don't update again
    fCurrentMacro := nil;
  end;
end;

end.
