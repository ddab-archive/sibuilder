{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmInstallFileName.pas
  @COMMENTS                 Implements a dialog box that presents a list of all
                            install files in project and permits user to choose
                            one of these or enter a different file.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/12/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 06/01/2001
      @COMMENTS             Modified to make file name and CreateEx constructor
                            protected and therefore available to new
                            TCOMServerDlg descendant class.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 29/12/2002
      @COMMENTS             + Made combo box into drop down list: user can now
                              only select existing files.
                            + Added error trapping to OK button event handler
                              for case where no file is selected in drop down
                              list.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
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
 * The Original Code is FmInstallFileName.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmInstallFileName;


interface


uses
  // Delphi
  SysUtils, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TInstallFileNameDlg:
    Dialog box that presents a list of available files user to choose one of
    these. Acts as a parent class for other dlgs that present the list of files.

    Inheritance: TInstallFileNameDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TInstallFileNameDlg = class(TGenericOKDlg)
    cmbServer: TComboBox;
    lblServer: TLabel;
    procedure OKBtnClick(Sender: TObject);
  private // properties
    function GetFileName: string;
  protected
    constructor CreateEx(const Owner: TComponent;
      const ACaption: string; const AHelpContext: Integer;
      const AFileName: string; const InstallFiles: TStrings);
      {Constructor that creates the dialogue box intialised with given caption,
      help context, list of installed files and name of selected file}
    property FileName: string read GetFileName;
      {Exposes file name in combo box to descended classes}
  public
    class function EditInstallFileName(const Owner: TComponent;
      const ACaption: string; const AHelpContext: Integer;
      var AFileName: string; const InstallFiles: TStrings): Boolean;
      {Displays dlg box with given caption, help context and list of available
      files and gets required file name from user}
  end;

  {
  EInstallFileNameDlg:
    Class of exception raised by the install file name dialog box.

    Inheritance: EInstallFileNameDlg -> [EInstallFileNameDlg]
  }
  EInstallFileNameDlg = class(Exception);


implementation


uses
  // Project
  UMessageBox;

{$R *.DFM}

resourcestring
  // Error messages
  sNoFileSelected = 'A file must be selected';

  
{ TInstallFileNameDlg }

constructor TInstallFileNameDlg.CreateEx(const Owner: TComponent;
  const ACaption: string; const AHelpContext: Integer;
  const AFileName: string; const InstallFiles: TStrings);
  {Constructor that creates the dialogue box intialised with given caption, help
  context, list of installed files and name of selected file}
begin
  inherited Create(Owner);
  Caption := ACaption;
  HelpContext := AHelpContext;
  cmbServer.Items := InstallFiles;
  cmbServer.ItemIndex := InstallFiles.IndexOf(AFileName);
end;

class function TInstallFileNameDlg.EditInstallFileName(
  const Owner: TComponent; const ACaption: string; const AHelpContext: Integer;
  var AFileName: string; const InstallFiles: TStrings): Boolean;
  {Displays dlg box with given caption, help context and list of available files
  and gets required file name from user}
begin
  with TInstallFileNameDlg.CreateEx(
    Owner, ACaption, AHelpContext, AFileName, InstallFiles) do
    try
      Result := ShowModal = mrOK;
      if Result then
        AFileName := GetFileName;  // user OK'd: record file name
    finally
      Free;
    end;
end;

function TInstallFileNameDlg.GetFileName: string;
  {Read access method for FileName property: returns selected item from combo
  box}
begin
  Result := cmbServer.Text;
end;

procedure TInstallFileNameDlg.OKBtnClick(Sender: TObject);
  {OK button click event handler: checks if an item has been selected, raises
  exception if not or allows dialog to close if all is OK}
begin
  inherited;
  try
    if cmbServer.ItemIndex =-1 then
      raise EInstallFileNameDlg.Create(sNoFileSelected);
    ModalResult := mrOK
  except
    on E: Exception do
    begin
      TMessageBox.Error(Self, E.Message);
      ModalResult := mrNone;
    end;
  end;
end;

end.
