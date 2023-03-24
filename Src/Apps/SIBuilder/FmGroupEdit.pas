{
 * FmGroupEdit.pas
 *
 * This is a form unit unique to the SIBuilder.exe sub-project. It implements a
 * dialogue box that allows user to edit properties of an installation group.
 * This inherits OK, close and help buttons from the generic OK dialog box.
 *
 * This unit requires the following components:
 *   + DelphiDabbler Shell Folders Unit v2.2.3 or later for TPJBrowseDialog
 *     component.
 *   + Custom SIBuilder TNewGroupBox v1.0 component.
 *   + Custom SIBuilder TFolderComboBox v1.0 component.
 *
 *
 * v1.0 of 09 Mar 2000  - Original version.
 * v1.1 of 28 Aug 2000  - Removed call to AlignDlgBox inherited method. Now
 *                        removed from base class since alignment now automatic.
 *                        Also added reference to UCommonTypes unit for types of
 *                        some of the properties of the TGroup being edited.
 * v1.2 of 25 Jun 2001  - Used TPJBrowseDialog component to select folders
 *                        instead of TChooseFolderDlg form.
 * v1.3 of 01 Apr 2002  - Prevented removal of Program Files and Common Files
 *                        folders as well as Windows and System folders.
 * v2.0 of 29 Dec 2002  - Major update to dialog, with extended functionality.
 *                      - Improved enabling/disabling of file and directory
 *                        radio buttons.
 *                      - Added new section to dialog to enable selection of OSs
 *                        for different installs. Uses an additional dialog box
 *                        for editing OS details.
 *                      - Made install path combo box display new "Browse for
 *                        folder..." item that leads to browse dialog box. This
 *                        uses a custom SIBuilder component.
 *                      - Made browse for folder dialog prevent user from
 *                        entering folders that are not file system directories.
 *                      - Replaced FormActivate event handler with FormShow
 *                        handler.
 *                      - Moved string literals to resource strings.
 *                      - Fixed bug in code that displays full path hint for
 *                        path macros. It was raising exception if any invalid
 *                        path macro is entered.
 *                      - Replaced TGroupBox control with TNewGroupBox custom
 *                        control that changes appearance when disabled.
 *                      - Added radio button and supporting code for new
 *                        temporary file deletion attribute.
 *                      - Replaced calls the MessageDlg with calls to methods of
 *                        the TMessageBox object.
 * v2.1 of 28 Nov 2003  - Deleted reference to unused unit.
 * v2.2 of 25 Mar 2007  - Added OnGetFolder event handler for revised
 *                        TFolderComboBox control, deleted code that set
 *                        control's now-removed BrowseDialog property.
 * v2.3 of 20 Feb 2008  - Replaced usage of Help.inc include file with
 *                        UHelpContexts unit.
 * v2.4 of 11 Apr 2008  - Resized dialog box so that OS Options display does not
 *                        have a truncated border.
 *                      - Replaced IsEqualText routine with AnsiSameText from
 *                        StrUtils.
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
 * The Original Code is FmGroupEdit.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmGroupEdit;


interface


uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  // PJSoft components
  PJShellFolders,
  // Project specific components
  CmpActionComboBox, CmpFileEditors, CmpGroupBox,
  // Project
  FmGenericOKDlg, UCommonTypes, UGroups;

type

  {
  TGroupEditDlg:
    Dialog box that allows user to edit properties of an installation group.
    Inherits OK and close and help buttons from generic OK dialog box.
  }
  TGroupEditDlg = class(TGenericOKDlg)
    acbInstallPath: TFolderComboBox;
    dlgBrowse: TPJBrowseDialog;
    edGroupName: TEdit;
    lblGroupName: TLabel;
    lblInstallPath: TLabel;
    gpDelOptions: TNewGroupBox;
    ngpFolderRadio: TNewGroupBox;
    rbDDNone: TRadioButton;
    rbDDIfEmpty: TRadioButton;
    rbDDAll: TRadioButton;
    gpFileRadio: TNewGroupBox;
    rbFDNone: TRadioButton;
    rbFDInstalled: TRadioButton;
    rbFDAll: TRadioButton;
    rbFDTemp: TRadioButton;
    gpOSOptions: TNewGroupBox;
    bvlOSOptions: TBevel;
    lblOSOptions: TLabel;
    btnOSOptions: TButton;
    procedure acbInstallPathAfterBrowse(Sender: TObject;
      var FileSpec: String);
    procedure acbInstallPathBeforeBrowse(Sender: TObject;
      var FileSpec: String);
    procedure acbInstallPathChange(Sender: TObject);
    procedure acbInstallPathGetFolder(Sender: TObject;
      var FileSpec: String; var Cancelled: Boolean);
    procedure btnOSOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RadiosClick(Sender: TObject);
  private // properties
    fGroup: TGroup;
      {Stores value of Group property}
    fEditedName: string;
      {Stores value of EditedName property}
    procedure SetGroup(const Value: TGroup);
      {Write access method for Group property. Records group object, makes a
      temporary copy of Group's OS options property, and displays it as text.
        @param Value [in] New property value. Must not be nil.
      }
  private
    fFDRadios: array[TFileDeletionActions] of TRadioButton;
      {Array of radio buttons on form that relate to file deletion actions}
    fDDRadios: array[TDirDeletionActions] of TRadioButton;
      {Array of radio buttons on form that relate to directory deletion actions}
    fOSOptions: LongWord;
      {Records OS options and changes made to them}
    procedure CheckFDRadio(Index: TFileDeletionActions);
      {Checks the a radio button in the fFDRadios array.
        @param Index [in] Index of radio button in array.
      }
    procedure CheckDDRadio(Index: TDirDeletionActions);
      {Checks the a radio button in the fDDRadios array.
        @param Index [in] Index of radio button in array.
      }
    function CheckedFDRadio: TFileDeletionActions;
      {Finds selected deletion action from fFDRadios array.
        @return Selected action.
      }
    function CheckedDDRadio: TDirDeletionActions;
      {Finds selected deletion action from fDDRadios array.
        @return Selected action.
      }
    procedure EnableFDRadio(Index: TFileDeletionActions; Value: Boolean);
      {Enables or disables a radio button in the fFDRadios array.
        @param Index [in] Index of radio button to be updated.
        @param Value [in] Whether to enable (True) or disable (False).
      }
    procedure EnableAllFDRadio(Value: Boolean);
      {Enables or disables all radio buttons in the fFDRadios array.
        @param Value [in] Whether to enable (True) or disable (False).
      }
    procedure EnableDDRadio(Index: TDirDeletionActions; Value: Boolean);
      {Enables or disables a radio button in the fDDRadios array.
        @param Index [in] Index of radio button to be updated.
        @param Value [in] Whether to enable (True) or disable (False).
      }
    procedure EnableAllDDRadio(Value: Boolean);
      {Enables or disables all radio buttons in the fDDRadios array.
        @param Value [in] Whether to enable (True) or disable (False).
      }
    procedure UpdateControls(const FileSpec: string);
      {Updates dialog's controls according to current state of radio buttons and
      install file spec. Alters state of some radio buttons according to state
      of other buttons and controls. This is required since some combinations of
      control states and file specs are not permitted. Also updates the file
      path hints for install paths containing path macros.
        @param FileSpec [in] Full specification of file that is or will be
          displayed in install path combo box.
      }
    procedure DisplayOSOptions;
      {Displays the current OS options as text on face of dlg box.
      }
  public
    property Group: TGroup read fGroup write SetGroup;
      {The group object that is being edited}
    property EditedName: string read fEditedName;
      {The name of the group object after editing. Note this name is not
      assigned to the group object itself - this is to allow user to check if
      new name already exists in list of groups. If name of actual Group object
      was changed then new name would always appear in list!}
  end;


implementation


uses
  // Delphi
  StrUtils,
  // Project
  UPathMacros, UOS, UGUIHelper, UHelpContexts, UMessageBox, FmOSOptions;

{$R *.DFM}

resourcestring
  // Error messages
  sIncompleteEntries =
    'You must enter both a group name and an installation path.';


{ TGroupEditDlg }

procedure TGroupEditDlg.acbInstallPathAfterBrowse(Sender: TObject;
  var FileSpec: String);
  {OnAfterBrowse event handler for install path folder combo box. Replaces any
  recognised paths in new file spec with path macros.
    @param Sender [in] Not used.
    @param FileSpec [in/out] File specification. Updated with path macros.
  }
begin
  inherited;
  FileSpec := PathMacros.PathToMacro(FileSpec);
end;

procedure TGroupEditDlg.acbInstallPathBeforeBrowse(Sender: TObject;
  var FileSpec: String);
  {OnBeforeBrowse event handler for install path folder combo box. Expands any
  path macros before displaying browse for folder dialog.
    @param Sender [in] Not used.
    @param FileSpec [in/out] File specification. Updated with expanded path
      macros.
  }
begin
  inherited;
  FileSpec := PathMacros.ExpandMacroPath(FileSpec);
end;

procedure TGroupEditDlg.acbInstallPathChange(Sender: TObject);
  {OnChange event handler for install path action combo box. Updates controls to
  reflect new value.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Update radio buttons and combo box hints to reflect interdependencies
  UpdateControls(acbInstallPath.Text);
end;

procedure TGroupEditDlg.acbInstallPathGetFolder(Sender: TObject;
  var FileSpec: String; var Cancelled: Boolean);
  {Handles OnGetFolder event of install path combo box. Displays a Browse for
  Folder dialog box and records selected folder and if user cancelled.
    @param Sender [in] Not used.
    @param FileSpec [in/out] Used to iniatialise dialog box. Updated to any
      folder selected by user.
    @param Cancelled [in/out] Set to true if user cancels dialog box.
  }
begin
  dlgBrowse.FolderName := FileSpec;
  Cancelled := not dlgBrowse.Execute;
  if not Cancelled then
    FileSpec := dlgBrowse.FolderName;
end;

procedure TGroupEditDlg.btnOSOptionsClick(Sender: TObject);
  {Handle click on OS Options button. Displays OS options dlg box, passing
  current value and recording new value if user OKs dlg box.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create the dialog box
  with TOSOptionsDlg.Create(Self) do
    try
      // Pass the current OS options to dlg box and display dlg
      OSOptions := fOSOptions;
      if ShowModal = mrOK then
      begin
        // User OKd: record new OS options and display them
        fOSOptions := OSOptions;
        DisplayOSOptions;
      end;
    finally
      // Dispose of dlg box
      Free;
    end;
end;

procedure TGroupEditDlg.CheckDDRadio(Index: TDirDeletionActions);
  {Checks the a radio button in the fDDRadios array.
    @param Index [in] Index of radio button in array.
  }
begin
  fDDRadios[Index].Checked := True;
end;

function TGroupEditDlg.CheckedDDRadio: TDirDeletionActions;
  {Finds selected deletion action from fDDRadios array.
    @return Selected action.
  }
var
  I: TDirDeletionActions;   // loops thru array
begin
  Result := Low(TDirDeletionActions);   // keeps compiler warnign quiet
  // Scan thru array to find which radio button is checked, recording result
  for I := Low(TDirDeletionActions) to High(TDirDeletionActions) do
    if fDDRadios[I].Checked then
      Result := I;  // only one will be checked
end;

function TGroupEditDlg.CheckedFDRadio: TFileDeletionActions;
  {Finds selected deletion action from fFDRadios array.
    @return Selected action.
  }
var
  I: TFileDeletionActions;  // loops thru array
begin
  Result := Low(TFileDeletionActions);  // keeps compiler warning quiet
  // Scan thru array to find which radio button is checked, recording result
  for I := Low(TFileDeletionActions) to High(TFileDeletionActions) do
    if fFDRadios[I].Checked then
      Result := I;  // only one will be checked
end;

procedure TGroupEditDlg.CheckFDRadio(Index: TFileDeletionActions);
  {Checks the a radio button in the fFDRadios array.
    @param Index [in] Index of radio button in array.
  }
begin
  fFDRadios[Index].Checked := True;
end;

procedure TGroupEditDlg.DisplayOSOptions;
  {Displays the current OS options as text on face of dlg box.
  }
begin
  lblOSOptions.Caption := UOS.DescribeOSs(fOSOptions);
end;

procedure TGroupEditDlg.EnableAllDDRadio(Value: Boolean);
  {Enables or disables all radio buttons in the fDDRadios array.
    @param Value [in] Whether to enable (True) or disable (False).
  }
var
  DI: TDirDeletionActions;      // loops thru array
begin
  // Scan thru array setting Enabled property of all radio buttons
  for DI := Low(TDirDeletionActions) to High(TDirDeletionActions) do
    EnableDDRadio(DI, Value);
end;

procedure TGroupEditDlg.EnableAllFDRadio(Value: Boolean);
  {Enables or disables all radio buttons in the fFDRadios array.
    @param Value [in] Whether to enable (True) or disable (False).
  }
var
  FI: TFileDeletionActions;     // loops thru array
begin
  // Scan thru array setting Enabled property of all radio buttons
  for FI := Low(TFileDeletionActions) to High(TFileDeletionActions) do
    EnableFDRadio(FI, Value);
end;

procedure TGroupEditDlg.EnableDDRadio(Index: TDirDeletionActions;
  Value: Boolean);
  {Enables or disables a radio button in the fDDRadios array.
    @param Index [in] Index of radio button to be updated.
    @param Value [in] Whether to enable (True) or disable (False).
  }
begin
  fDDRadios[Index].Enabled := Value;
end;

procedure TGroupEditDlg.EnableFDRadio(Index: TFileDeletionActions;
  Value: Boolean);
  {Enables or disables a radio button in the fFDRadios array.
    @param Index [in] Index of radio button to be updated.
    @param Value [in] Whether to enable (True) or disable (False).
  }
begin
  fFDRadios[Index].Enabled := Value;
end;

procedure TGroupEditDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Copies folder macro names into combo box and
  records references to radio buttons relating to directory and file deletion.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create install folder combo box items
  // record names of folder macros
  PathMacros.GetMacroNames(acbInstallPath.Items);
  // add the action text that displays browse for folder dlg box to combo box
  acbInstallPath.Items.Add(acbInstallPath.ActionText);
  // Record references to appropriate radio buttons in arrays
  fFDRadios[fdNone] := rbFDNone;
  fFDRadios[fdInstalled] := rbFDInstalled;
  fFDRadios[fdAll] := rbFDAll;
  fFDRadios[fdTemporary] := rbFDTemp;
  fDDRadios[ddNone] := rbDDNone;
  fDDRadios[ddIfEmpty] := rbDDIfEmpty;
  fDDRadios[ddAll] := rbDDAll;
  // Set help contexts
  HelpContext := IDH_DLG_EDITGROUP;
  dlgBrowse.HelpContext := IDH_DLG_CHOOSEFOLDER;
end;

procedure TGroupEditDlg.FormShow(Sender: TObject);
  {Form show event. Display required properties of group object we're going to
  edit.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Display group's name and path in edit boxes
  edGroupName.Text := Group.Name;
  acbInstallPath.Text := Group.Path;
  // Check relevant radio buttons re folder and file deletion
  CheckFDRadio(Group.FileDeletion);
  CheckDDRadio(Group.DirDeletion);
  // Update controls to reflect interdependencies
  UpdateControls(acbInstallPath.Text);
end;

procedure TGroupEditDlg.OKBtnClick(Sender: TObject);
  {Handles click on OK button. Validate user's entries and updates group object
  with entries. Note that group name is written to EditedName property and is
  not stored in group object.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // We must have both a name and a path - so check
  if (edGroupName.Text <> '') and (acbInstallPath.Text <> '') then
  begin
    // Name and path entered - update group object and close
    ModalResult := mrOK;
    fEditedName := edGroupName.Text;
    Group.Path := acbInstallPath.Text;
    Group.FileDeletion := CheckedFDRadio;
    Group.DirDeletion := CheckedDDRadio;
    Group.OSOptions := fOSOptions;
  end
  else
  begin
    // There's an error - tell user and don't close dlg box
    ModalResult := mrNone;
    TMessageBox.Error(Self, sIncompleteEntries);
  end;
end;

procedure TGroupEditDlg.RadiosClick(Sender: TObject);
  {Radio button click event. Updates other radio buttons to reflect
  interdependencies.
    @param Sender [in] Not used.
  }
begin
  inherited;
  UpdateControls(acbInstallPath.Text);
end;

procedure TGroupEditDlg.SetGroup(const Value: TGroup);
  {Write access method for Group property. Records group object, makes a
  temporary copy of Group's OS options property, and displays it as text.
    @param Value [in] New property value. Must not be nil.
  }
begin
  fGroup := Value;
  fOSOptions := fGroup.OSOptions;
  DisplayOSOptions;
end;

procedure TGroupEditDlg.UpdateControls(const FileSpec: string);
  {Updates dialog's controls according to current state of radio buttons and
  install file spec. Alters state of some radio buttons according to state of
  other buttons and controls. This is required since some combinations of
  control states and file specs are not permitted. Also updates the file path
  hints for install paths containing path macros.
    @param FileSpec [in] Full specification of file that is or will be displayed
      in install path combo box.
  }
var
  DI: TDirDeletionActions;    // loops thru array of directory deletion radios
  FI: TFileDeletionActions;   // loops thru array of file deletion radios
begin
  // Check if Windows, System, Program Files or Common Files paths are specified
  if AnsiSameText(FileSpec, PathMacros.Names[pnWindows])
    or AnsiSameText(FileSpec, PathMacros.Names[pnSystem])
    or AnsiSameText(FileSpec, PathMacros.Names[pnProgramFiles])
    or AnsiSameText(FileSpec, PathMacros.Names[pnCommonFiles]) then
  begin
    // We can't allow directory to be deleted or all files to be deleted in
    // Windows or System folders or in Program Files or Common Files
    // ensure the "Do not remove folder" radio is selected
    CheckDDRadio(ddNone);
    // ensure only "Do not remove folder" dir deletion radio is enabled
    for DI := Low(TDirDeletionActions) to High(TDirDeletionActions) do
      EnableDDRadio(DI, (DI = ddNone));
    // if "Delete all files in folder" is checked check "... installed" option
    // instead
    if CheckedFDRadio = fdAll then
      CheckFDRadio(fdInstalled);
    // ensure that all file deletion radios are checked except "Delete all..."
    for FI := Low(TFileDeletionActions) to High(TFileDeletionActions) do
      EnableFDRadio(FI, FI <> fdAll);
  end
  else
  begin
    // We have a removable folder
    // enable all file deletion radios unless "Remove folder..." radio checked
    if CheckedDDRadio = ddAll then
    begin
      // remove folder is checked: disable file deletion items and ensure
      // fdAll is checked
      EnableAllFDRadio(False);
      CheckFDRadio(fdAll);
    end
    else
      // remove folder not checked: enable all file deletion radios
      EnableAllFDRadio(True);
    // enable all dir deletion radios
    EnableAllDDRadio(True);
  end;

  // Display a hint if path in combo box contains a macro - hint shows actual
  // path represented by macro (which begins with '[')
  Hint := UGUIHelper.PathMacroCtrlHint(FileSpec);
end;

end.

