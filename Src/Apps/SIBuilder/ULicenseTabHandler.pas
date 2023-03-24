{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     ULicenseTabHandler.pas
  @COMMENTS                 TTabSheetHandler descendant that provides the
                            required additional functionality for the "License
                            Info" tab sheet.
  @DEPENDENCIES             The following COM Server is required:
                            + SITLicenseDlg.dll 1.0 or later\
                            And the following components are also required:
                            + TPJBrowseDialog PJ library component from
                              PJShellFolders v1.0.
                            + Custom SIBuilder TNewGroupBox v1.0 component
                            + Custom SIBuilder TFileEdit v1.0 component
                            + Custom SIBuilder TFolderComboBox v1.0 component
  @OTHER_NAMES              + Original unit name was FmConfigLicense.pas: this
                              unit implemented a dialog box used to edit license
                              install info.
                            + Changed to ULicenseTabHandler.pas at v2.0, and
                              changed to be a tab handler for the license info
                              page of the main program.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Totally re-written as a tab sheet handler now that
                            the license editing interface has been changed from
                            a dialog box to have its own page in the main user
                            interface. Much of the functionality from the dialog
                            box's methods and event handlers was moved to
                            similar methods in the tab handler, but was updated.
                            Required tab sheet handling methods were added. Some
                            changes to the controls used in the original dialog
                            box were:
                            + Replaced separate edit box and browse button for
                              source license file editing with a TFileEdit
                              custom control.
                            + Added browse option to install path combo box (by
                              using a TFolderCombo custom control).
                            + Standard group box components were replaced by
                              custom TNewGroupBox components.
                            + Added ability to drag and drop license files onto
                              license file group box.
                            + License DLL no longer supports user defined
                              installation path and options to keep or register
                              DLL is COM object.
                            + Replaced descriptive labels in Test group box with
                              a hotlinked rtf control that can display popup
                              help giving further info.
                            + Update CanTurnPage method to return a help context
                              as well as reason when page can't be turned.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/11/2003
      @COMMENTS             + Changed handling license test hot linked message
                              to use new hot text control rather than a rich
                              edit control and the active RTF classes. The hot
                              text is now stored in the control rather than
                              being loaded from resources.
                            + Removed label and special processing to handling
                              appearance of disabled hot text since hot text
                              controls display as required when disabled.
                            + Removed reference to removed UActiveRTFWrapper
                              unit.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted references to unused units.
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
 * The Original Code is ULicenseTabHandler.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit ULicenseTabHandler;


interface


uses
  // Delphi
  Windows, ComCtrls,
  // Project
  UTabSheetHandler;

type

  {
  TLicenseTabHandler:
    TTabSheetHandler descendant that provides the required additional
    functionality for the "license info" tab sheet.

    Inheritance: TLicenseTabHandler -> TTabSheetHandler -> TObjListItem
      -> [TObject]
  }
  TLicenseTabHandler = class(TTabSheetHandler)
  private
    procedure TestButtonClick(Sender: TObject);
      {OnClick event handler for Test button: displays a sample license dlg box,
      per license info entered in dlg box}
    procedure WantLicenseClick(Sender: TObject);
      {OnClick event handler for "want license" check box: updates all controls
      on tab sheet according to check box's new value}
    procedure LicenceDropFiles(Sender: TObject);
      {OnDropFiles event handler for drop files window that contains license
      file controls}
    procedure UpdateControls;
      {Updates tab sheet controls according to current entries on sheet}
    procedure EnableControls(Flag: Boolean);
      {Enables/disables certain tab sheet groups according to state of Flag}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
    procedure UpdateProject; override;
      {Updates project to reflect values in tab sheet}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: sets event handlers for tab sheet controls and sets up
      controls}
    destructor Destroy; override;
      {Class destructor: nils all events that were handled by this object}
    procedure UpdateSheet; override;
      {Updates content of controls on tab sheet to reflect relevant project
      property values}
    function CanTurnPage(out Reason: string;
      out AHelpCtx: Integer): Boolean; override;
      {Returns true if the page can be turned (ie we can move to another page),
      false if not. Method checks validity of values of tab sheet's controls and
      returns any reason why the page can't be turned and an associated help
      context or 0 if none) via the out parameters. The page can only be turned
      if all the required license information is provided}
  end;


implementation


uses
  // Delphi
  SysUtils, ComObj, StdCtrls, Graphics,
  // Project
  FmMain, IntfSITLicenseDlg, ULicenseInfo, ULicenseDlgHelp, UFileProcs,
  UGUIHelper, UCmdDispatcher, UResources, UHelpContexts;


resourcestring
  // Error messages
  sNoLicenseFile = 'You must provide a source license file';
  sLicenseFileMissing = 'Source license file "%s" doesn''t exist';
  sNoLicenseFilePath =
    'You must provide an installation path for the license file';
  // Test license dlg box
  sLicDlgCaption = 'Test License Dialog Box';
  sNoFileMessage = '** License file not specified or doesn''t exist. **';


{ TLicenseTabHandler }

function TLicenseTabHandler.CanTurnPage(out Reason: string;
  out AHelpCtx: Integer): Boolean;
  {Returns true if the page can be turned (ie we can move to another page),
  false if not. Method checks validity of values of tab sheet's controls and
  returns any reason why the page can't be turned and an associated help context
  or 0 if none) via the out parameters. The page can only be turned if all the
  required license information is provided}
begin
  try
    // Validate controls: raise exceptions if errors found
    with MainForm do
    begin
      if chkWantLicense.Checked then
      begin
        // We want a license: file must be given and exist
        if fedLicFileSrc.Text = '' then
          raise Exception.Create(sNoLicenseFile);
        if not FileExists(fedLicFileSrc.Text) then
          raise Exception.CreateFmt(sLicenseFileMissing, [fedLicFileSrc.Text]);
      end;
    end;
    // If we get here everything's OK
    Result := True;
  except
    // Catch exceptions, return false and pass out exception messages as reason
    on E: Exception do
    begin
      Reason := E.Message;
      AHelpCtx := E.HelpContext;
      Result := False;
    end;
  end;
end;

constructor TLicenseTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: sets event handlers for tab sheet controls and sets up
  controls}
begin
  inherited;
  with MainForm do
  begin
    // Assign event handlers
    btnLicTest.OnClick := TestButtonClick;
    chkWantLicense.OnClick := WantLicenseClick;
    dfLicense.OnDropFiles := LicenceDropFiles;
    // Assign event handler for license test's hot link
    htLicTest.OnLinkClick := TCmdDispatcher.HotLinkClickHandler;
  end;
end;

destructor TLicenseTabHandler.Destroy;
  {Class destructor: nils all events that were handled by this object}
begin
  // Nil the event handlers
  with MainForm do
  begin
    btnLicTest.OnClick := nil;
    chkWantLicense.OnClick := nil;
    dfLicense.OnDropFiles := nil;
  end;
  inherited;
end;

procedure TLicenseTabHandler.EnableControls(Flag: Boolean);
  {Enables/disables certain tab sheet groups according to state of Flag}
begin
  with MainForm do
  begin
    // enable/disable all required controls (parents only)
    EnableCtrls([gpLicFile, gpLicDlg, gpLicTest], Flag, True);
  end;
end;

function TLicenseTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cLicenseInfoResID;
end;

procedure TLicenseTabHandler.LicenceDropFiles(Sender: TObject);
  {OnDropFiles event handler for drop files window that contains license file
  controls}
begin
  MainForm.fedLicFileSrc.Text := MainForm.dfLicense.FileName;
end;

procedure TLicenseTabHandler.TestButtonClick(Sender: TObject);
  {OnClick event handler for Test button: displays a sample license dlg box, per
  license info entered in dlg box}
var
  LicDlg: ISITLicenseDlg;   // instance of object that displays dlg box
  Style: DWORD;             // the style of the dlg
  Size: TLicenseDlgSize;    // the size of the dlg box (small, medium, large)
begin
  with MainForm do
  begin
    // Create instance of object that displays dlg
    LicDlg := CreateComObject(CLSID_SITLicenseDlg) as ISITLicenseDlg;
    // Determine dlg box style and set it
    Style := SITLicDlg_MONOFONT;
    if cmbLicButtons.ItemIndex = 0 then
      Style := Style or SITLicDlg_DECLINEBTN;
    LicDlg.SetStyle(Style);
    // Set size of dlg box
    case cmbLicSize.ItemIndex of
      0: Size := ldsSmall;
      1: Size := ldsMedium;
      2: Size := ldsLarge;
      else Size := ldsLarge;
    end;
    LicDlg.SetSize(TLicenseInfo.GetDlgWidth(Size),
      TLicenseInfo.GetDlgHeight(Size));
    // Register object that will provdie help
    LicDlg.RegisterHelp(TLicenseDlgHelp.Create, IDH_DLG_TESTLICENSE);
    // Now display dlg with either file or a message saying no file
    if FileExists(fedLicFileSrc.Text) then
      LicDlg.DisplayLicenseFile(Handle, Left + 40, Top + 40,
        PChar(sLicDlgCaption), PChar(fedLicFileSrc.Text)
      )
    else
      LicDlg.DisplayLicenseText(Handle, Left + 40, Top + 40,
        PChar(sLicDlgCaption), PChar(sNoFileMessage));
  end;
end;

procedure TLicenseTabHandler.UpdateControls;
  {Updates tab sheet controls according to current entries on sheet}
begin
  with MainForm do
    // Enable/disabled group boxes according to if license required
    // (the group boxes with enabled/disabled child controls)
    EnableControls(chkWantLicense.Checked);
end;

procedure TLicenseTabHandler.UpdateProject;
  {Updates project to reflect values in tab sheet}
var
  Info: TLicenseInfo; // license information object
begin
  with MainForm do
  begin
    // Get hold of license infomation object fromproject
    Info := GetProject.LicenseInfo;
    // Record whether active
    Info.Active := chkWantLicense.Checked;
    // License file stuff
    Info.SourceFile := fedLicFileSrc.Text;
    // Dialog box stuff
    Info.MustAccept := (cmbLicButtons.ItemIndex = 0);
    case cmbLicSize.ItemIndex of
      0: Info.DlgSize := ldsSmall;
      1: Info.DlgSize := ldsMedium;
      2: Info.DlgSize := ldsLarge;
    end;
  end;
end;

procedure TLicenseTabHandler.UpdateSheet;
  {Updates content of controls on tab sheet to reflect relevant project property
  values}
var
  Info: TLicenseInfo; // license info object
begin
  with MainForm do
  begin
    // Get hold of license info object from project
    Info := GetProject.LicenseInfo;
    // Update control values from license info
    // active
    chkWantLicense.Checked := Info.Active;
    // license file stuff
    fedLicFileSrc.Text := Info.SourceFile;
    // dialog box properties
    if Info.MustAccept then
      cmbLicButtons.ItemIndex := 0
    else
      cmbLicButtons.ItemIndex := 1;
    case Info.DlgSize of
      ldsSmall: cmbLicSize.ItemIndex := 0;
      ldsMedium: cmbLicSize.ItemIndex := 1;
      ldsLarge: cmbLicSize.ItemIndex := 2;
    end;
    // prepare browse dialog with default initial dir and file name
    if (Info.SourceFile <> '')
      and DirExists(ExtractFileDir(Info.SourceFile)) then
    begin
      dlgLicBrowseFile.InitialDir := ExtractFileDir(Info.SourceFile);
      if FileExists(Info.SourceFile) then
        dlgLicBrowseFile.FileName := ExtractFileName(Info.SourceFile)
      else
        dlgLicBrowseFile.FileName := '';
    end
    else
      dlgLicBrowseFile.InitialDir := GetDocumentPath;
  end;
  // Update the tab sheet's controls
  UpdateControls;
end;

procedure TLicenseTabHandler.WantLicenseClick(Sender: TObject);
  {OnClick event handler for "want license" check box: updates all controls on
  tab sheet according to check box's new value}
begin
  UpdateControls;
end;

end.
