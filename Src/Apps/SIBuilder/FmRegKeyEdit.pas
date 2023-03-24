{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmRegKeyEdit.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialog box that allows
                            user to edit properties of a registry sub-key.
                            Inherits OK and close and help buttons from generic
                            OK dialog box.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 11/08/2000
      @COMMENTS             Added check box to determine if reg key is to be
                            deleted on uninstall.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 29/12/2002
      @COMMENTS             + Added support for new option to delete a sub key
                              only if it is empty: replaced deletion check box
                              with drop down list of deletion options and
                              modified code to support this feature.
                            + Also made string literal into a resource string.
                            + Checked for '\' characters in key names and
                              reported error if found.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
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
 * The Original Code is FmRegKeyEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmRegKeyEdit;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UCommonTypes;

type

  {
  TRegKeyEditDlg:
    Dialog box that allows user to edit properties of a registry sub-key.
    Inherits OK and close and help buttons from generic OK dialog box.

    Inheritance: TRegKeyEditDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TRegKeyEditDlg = class(TGenericOKDlg)
    edKeyName: TEdit;
    lblKeyName: TLabel;
    lblDeletable: TLabel;
    cbDeletable: TComboBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private // properties
    fKeyName: string;
    fDeletable: TRegKeyDeletionActions;
    procedure SetKeyName(const AName: string);
    procedure SetDeletable(const Value: TRegKeyDeletionActions);
  public
    property KeyName: string read fKeyName write SetKeyName;
      {The name of the registry sub-key}
    property Deletable: TRegKeyDeletionActions
      read fDeletable write SetDeletable;
      {Whether and how key is to be deleted on uninstallation}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelpContexts, UMessageBox;


{$R *.DFM}


resourcestring
  // Error messages
  sRegKeyNameNeeded = 'You must enter a registry key name';
  sNoBackslashes = 'Backslash (\) characters are not allowed in key names';


{ TRegKeyEditDlg }

procedure TRegKeyEditDlg.FormCreate(Sender: TObject);
  {Form creation event handler: assigns help context and initialises controls}
begin
  inherited;
  HelpContext := IDH_DLG_EDITREGKEY;
  SetDeletable(rdNone);
end;

procedure TRegKeyEditDlg.OKBtnClick(Sender: TObject);
  {OK button click event handler: validates user's entry, records new key name
  and closes dlg box}
begin
  inherited;
  // A registry key name must be entered
  if edKeyName.Text = '' then
  begin
    // no name entered - tell user and keep dlg open
    TMessageBox.Error(Self, sRegKeyNameNeeded);
    ModalResult := mrNone;
  end
  else if AnsiPos('\', edKeyName.Text) > 0 then
  begin
    // backslashes are not allowed in key names
    TMessageBox.Error(Self, sNoBackslashes);
    ModalResult := mrNone;
  end
  else
  begin
    // a name was entered - update properties and close dlg box
    ModalResult := mrOK;
    fKeyName := edKeyName.Text;
    fDeletable := TRegKeyDeletionActions(cbDeletable.ItemIndex);
  end;
end;

procedure TRegKeyEditDlg.SetDeletable(const Value: TRegKeyDeletionActions);
  {Write access method for Deletable property: records value and updates
  associated drop down list as required}
begin
  fDeletable := Value;
  cbDeletable.ItemIndex := Ord(Value);
end;

procedure TRegKeyEditDlg.SetKeyName(const AName: string);
  {Write access method for KeyName property: records name and displays it in
  edit box}
begin
  fKeyName := AName;
  edKeyName.Text := AName;
end;

end.
