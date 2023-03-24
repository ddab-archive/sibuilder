{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmFileEdit.pas
  @COMMENTS                 Dlg box that allows user to state what will happen
                            if a file that is to be installed already exists.
  @DEPENDENCIES             The following custom component is required:
                            + Custom SIBuilder TNewGroupBox component v1.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/08/2000
      @COMMENTS             Original version.
    )
  )
  @HISTORY(
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Major changes to layout and options available in
                            dialog box
                            + Added check boxes for app path, shared DLL
                              and COM server registration.
                            + Now pass parent group reference to edit method
                              to allow shared DLL check box to be disabled for
                              inappropriate file deletion properties.
                            + Replaced file overwrite check boxes with drop down
                              list.
                            + Added support for new overwrite option based on
                              file version (only appears for files containing
                              version info).
                            + Replaced TGroupBox control with TNewGroupBox
                              custom control that changes appearance when
                              disabled.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Moved unit reference from interface to
                            implementation.
    )
    @REVISION(
      @VERSION              2.2
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
 * The Original Code is FmFileEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmFileEdit;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls, ExtCtrls,
  // Custom SIBuilder components
  CmpGroupBox,
  // Project
  FmGenericOKDlg, UFiles, UGroups;

type
  {
  TFileEditDlg:
    Dlg box that allows user to state what will happen if a file that is to be
    installed already exists.

    Inheritance: TFileEditDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TFileEditDlg = class(TGenericOKDlg)
    lblFileName: TLabel;
    txtFileName: TStaticText;
    cmbOverwrite: TComboBox;
    lblOverwrite: TLabel;
    gpFileRegister: TNewGroupBox;
    chkAppPath: TCheckBox;
    chkSharedDLL: TCheckBox;
    chkCOMDLL: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  public
    class function EditFileProperities(const Owner: TComponent;
      FileInfo: TFileInfo; ParentGroup: TGroup): Boolean;
      {Display file edit dialogue box and, if user OKs, update given file info
      object with file overwriting actions provided by user. Returns true if
      user OKs and false if user cancels}
  end;


implementation


uses
  // Delphi
  Windows, 
  // Project
  UCommonTypes, UHelpContexts, UMessageBox;

{$R *.DFM}


resourcestring
  // Error messages
  sErrNoOverwriteOption = 'You must select a file overwriting option';


{ Helper function }

function HasVersionInfo(const FileName: string): Boolean;
  {Returns true if given file contains version information}
var
  Dummy: THandle; // dummy variable required by API function
begin
  // API function returns size of ver info: 0 if none
  Result := Windows.GetFileVersionInfoSize(PChar(FileName), Dummy) > 0;
end;


{ TFileEditDlg }

class function TFileEditDlg.EditFileProperities(const Owner: TComponent;
  FileInfo: TFileInfo; ParentGroup: TGroup): Boolean;
  {Display file edit dialogue box and, if user OKs, update given file info
  object with file overwriting actions provided by user. Returns true if user
  OKs and false if user cancels}
var
  Dlg: TFileEditDlg;  // instance of dlg box
begin
  // Create dlg box
  Dlg := TFileEditDlg.Create(Owner);
  try
    // Update controls
    Dlg.txtFileName.Caption := FileInfo.FileName;
    Dlg.cmbOverwrite.ItemIndex := Ord(FileInfo.OverwriteAction);
    if not HasVersionInfo(FileInfo.SourceFileSpec) then
    begin
      if Dlg.cmbOverwrite.ItemIndex = Ord(foaIfLaterVer) then
        Dlg.cmbOverwrite.ItemIndex := -1;
      Dlg.cmbOverwrite.Items.Delete(Ord(foaIfLaterVer));
    end;
    Dlg.chkAppPath.Checked := FileInfo.RegisterAppPath;
    Dlg.chkSharedDLL.Checked := FileInfo.RegisterSharedDLL;
    Dlg.chkSharedDLL.Enabled := ParentGroup.FileDeletion = fdInstalled;
    Dlg.chkCOMDLL.Checked := FileInfo.RegisterCOMDLL;
    Dlg.chkCOMDLL.Enabled := FileInfo.FileKind = fkCOMDLL;
    // Display dlg and record if user OKs
    Result := (Dlg.ShowModal = mrOK);
    if Result then
    begin
      // User OK'd - update file info object
      FileInfo.OverwriteAction :=
        TFileOverwriteActions(Dlg.cmbOverwrite.ItemIndex);
      FileInfo.RegisterAppPath := Dlg.chkAppPath.Checked;
      FileInfo.RegisterSharedDLL := Dlg.chkSharedDLL.Checked;
      FileInfo.RegisterCOMDLL := Dlg.chkCOMDLL.Checked;
    end;
  finally
    // Destroy dlg
    Dlg.Free;
  end;
end;

procedure TFileEditDlg.FormCreate(Sender: TObject);
  {Form creation event handler - assigns required help context}
begin
  inherited;
  HelpContext := IDH_DLG_EDITFILE;
end;

procedure TFileEditDlg.OKBtnClick(Sender: TObject);
  {OK button click event handler: validates entries in dialog and displays error
  message and prevents dialog from closing if there's a problem}
begin
  inherited;
  // Check that an overwriting option is selected
  if cmbOverwrite.ItemIndex = -1 then
  begin
    TMessageBox.Error(Self, sErrNoOverwriteOption);
    ModalResult := mrNone;
  end
  else
    ModalResult := mrOK;
end;

end.
