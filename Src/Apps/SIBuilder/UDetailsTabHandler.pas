{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UDetailsTabHandler.pas
  @COMMENTS                 TTabSheetHandler descendant that provides the
                            required additional functionality for the "project
                            details" tab sheet.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 21/01/2001
      @COMMENTS             Added support for license information to page
                            handling code.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 01/04/2002
      @COMMENTS             + Fixed bug that was causing hot links spread over
                              end of lines to be ignored in some rich text
                              control versions (added phrases as phrases and as
                              separate words to hot link list).
                            + Replaced some string literals with resource
                              strings where appropriate.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Major revision. Changes are:
                            + Deleted Enter and Leave methods: now base class
                              methods call UpdateSheet and UpdateProject
                              respectively which is what we used to do here.
                            + Removed hot link setting: hot links are now
                              defined in tagged rtf file and automatically
                              created when loaded.
                            + Removed license handling code: license now on
                              separate page.
                            + Moved all prpject details tab code that was in
                              MainForm to this class.
                            + Made sure all event handlers are nilled when class
                              is destroyed.
                            + Updated CanTurnPage method to return a help
                              context as well as reason when page can't be
                              turned.
                            + Added support for project's new UninstallPrevious
                              property and matching check box on page.
                            + Added some help contexts to CanTurnPage method
                              along with reasons.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 23/02/2003
      @COMMENTS             Added support for uninstall projects edit control
                            and related project property.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 19/11/2003
      @COMMENTS             Deleted reference to removed UActiveRTFWrapper unit
                            - this unit was referenced but not actually used.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused units and
                            moved some units from interface to implementation.
    )
    @REVISION(
      @VERSION              2.4
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
 * The Original Code is UDetailsTabHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UDetailsTabHandler;


interface


uses
  // Delphi
  ComCtrls,
  // Project
  UTabSheetHandler;

type

  {
  TDetailsTabHandler:
    Encapsulates the "project details" tab sheet and provides the relevant
    specialised processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TDetailsTabHandler -> TTabSheetHandler -> TObjListItem
      -> [TObject]
  }
  TDetailsTabHandler = class(TTabSheetHandler)
  private
    procedure EditInstPathCheckClick(Sender: TObject);
      {OnClick event handler for edit install path check box - enables/disables
      "ask user" check box according to whether edit install path checkbox is
      checked}
    procedure TabSheetShow(Sender: TObject);
      {OnShow event handler for tab sheet: set focus to first control on sheet
      when sheet is displayed}
    procedure InstPathEditAfterBrowse(Sender: TObject;
      var FileSpec: string);
      {OnAfterBrowse event handler for install path browse / edit control:
      converts given file spec to include any relevant path macros (except install
      path) before displaying in edit control}
    procedure InstPathEditBeforeBrowse(Sender: TObject;
      var FileSpec: string);
      {OnBeforeBrowse event handler for install path browse / edit control:
      expands any path macros in given file spec}
    procedure UninstPrevNameBrowse(Sender: TObject);
      {OnBrowse event handler for uninstall projects edit box: displays dialog
      box used to edit semi-colon delimited list of projects to uninstall and
      enters revised string in edit box if user OKs}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
    procedure UpdateProject; override;
      {Updates project to reflect values in tab sheet}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: sets supported control's event handlers}
    destructor Destroy; override;
      {Class destructor: nil all the form's event handlers that were handled by
      this class}
    function CanTurnPage(out Reason: string;
      out AHelpCtx: Integer): Boolean; override;
      {Returns true if the page can be turned (ie we can move to another page),
      false if not. Method checks validity of values of tab sheet's controls and
      returns any reason why the page can't be turned and an associated help
      context or 0 if none) via the out parameters}
    procedure UpdateSheet; override;
      {Updates tab sheet controls to show relevant project properties}
  end;


implementation


uses
  // Delphi
  Forms, SysUtils, Controls,
  // Project
  UCommonTypes, UPathMacros, UResources, UHelpContexts,
  FmMain, FmUninstProjects;


resourcestring
  // Error messages
  sNoIDErr              = 'You must give the project an ID.';
  sBadIDErr             = 'Project ID is not a valid ID.';
  sUnNamedProjErr       = 'You must give the project a name.';
  sMissingInstPathErr   = 'You must specify an installation path.';
  sBadMacroNameErr      = 'Invalid path macro name entered for installation '
                          + 'path.';
  sUnNamedInstProgErr   = 'You must give the install program a name.';
  sCircularMacroRefErr  = 'Can''t use "%s" path macro here: circular '
                          + 'reference.';

{ TDetailsTabHandler }

function TDetailsTabHandler.CanTurnPage(out Reason: string;
  out AHelpCtx: Integer): Boolean;
  {Returns true if the page can be turned (ie we can move to another page),
  false if not. Method checks validity of values of tab sheet's controls and
  returns any reason why the page can't be turned and an associated help context
  (or 0 if none) via the out parameters}
begin
  // Set Reason parameter to empty string and no help context
  Reason := '';
  AHelpCtx := 0;
  with TabSheet.Owner as TMainForm do
  begin
    // Check control values and record any problem in Reason parameter
    if edPrjID.Text = '' then
    begin
      Reason := sNoIDErr;
      AHelpCtx := IDH_ERR_PROJECTID;
    end
    else if not IsValidIdent(edPrjID.Text) then
    begin
      Reason := sBadIDErr;
      AHelpCtx := IDH_ERR_PROJECTID;
    end
    else if edPrjName.Text = '' then
      Reason := sUnNamedProjErr
    else if fedInstPath.Text = '' then
      Reason := sMissingInstPathErr
    else if not PathMacros.ValidateMacroPath(fedInstPath.Text) then
      Reason := sBadMacroNameErr
    else if edInstPrgName.Text = '' then
      Reason := sUnNamedInstProgErr
    else if PathMacros.IncludesMacro(fedInstPath.Text, pnInstall) then
      Reason := Format(sCircularMacroRefErr, [PathMacros.Names[pnInstall]]);
  end;
  // Return true if there's no reason not to!
  Result := (Reason = '');
end;

constructor TDetailsTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: sets supported control's event handlers}
begin
  inherited;
  with MainForm do
  begin
    chkEditInstPath.OnClick := EditInstPathCheckClick;
    tsProjDetails.OnShow := TabSheetShow;
    fedInstPath.OnAfterBrowse := InstPathEditAfterBrowse;
    fedInstPath.OnBeforeBrowse := InstPathEditBeforeBrowse;
    bedUninstPrevName.OnBrowse := UninstPrevNameBrowse;
  end;
end;

destructor TDetailsTabHandler.Destroy;
  {Class destructor: nil all the form's event handlers that were handled by this
  class}
begin
  with MainForm do
  begin
    chkEditInstPath.OnClick := nil;
    tsProjDetails.OnShow := nil;
    fedInstPath.OnAfterBrowse := nil;
    fedInstPath.OnBeforeBrowse := nil;
    bedUninstPrevName.OnBrowse := nil;
  end;
  inherited;
end;

procedure TDetailsTabHandler.EditInstPathCheckClick(Sender: TObject);
  {OnClick event handler for edit install path check box - enables/disables
  "ask user" check box according to whether edit install path checkbox is
  checked}
begin
  MainForm.chkAskForInstPath.Enabled := MainForm.chkEditInstPath.Checked;
end;

procedure TDetailsTabHandler.InstPathEditAfterBrowse(Sender: TObject;
  var FileSpec: string);
  {OnAfterBrowse event handler for install path browse / edit control: converts
  given file spec to include any relevant path macros (except install path)
  before displaying in edit control}
begin
  if FileSpec <> '' then
  begin
    // Convert entered folder info to use path macros (but not install path one)
    PathMacros.InstallPath := FileSpec;
    // User provided a path, so put it in edit box via DisplayText
    FileSpec := PathMacros.InstallPath;
  end;
end;

procedure TDetailsTabHandler.InstPathEditBeforeBrowse(Sender: TObject;
  var FileSpec: string);
  {OnBeforeBrowse event handler for install path browse / edit control: expands
  any path macros in given file spec}
begin
  FileSpec := PathMacros.ExpandMacroPath(FileSpec);
end;

function TDetailsTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cDetailInfoResId;
end;

procedure TDetailsTabHandler.TabSheetShow(Sender: TObject);
  {OnShow event handler for tab sheet: set focus to first control on sheet when
  sheet is displayed}
begin
  MainForm.edPrjID.SetFocus;
end;

procedure TDetailsTabHandler.UninstPrevNameBrowse(Sender: TObject);
  {OnBrowse event handler for uninstall projects edit box: displays dialog box
  used to edit semi-colon delimited list of projects to uninstall and enters
  revised string in edit box if user OKs}
var
  UninstProjects: string; // semi-colon delimited list of uninstall projects
begin
  with MainForm do
  begin
    UninstProjects := bedUninstPrevName.Text;
    if TUninstProjectsDlg.EditUninstProjectList(MainForm, UninstProjects) then
      bedUninstPrevName.Text := UninstProjects;
  end;
end;

procedure TDetailsTabHandler.UpdateProject;
  {Updates project to reflect values in tab sheet}
begin
  with TabSheet.Owner as TMainForm do
  begin
    // store control values in appropriate project properties
    GetProject.ID := edPrjID.Text;
    GetProject.Name := edPrjName.Text;
    if chkEditInstPath.Checked then
      if chkAskForInstPath.Checked then
        GetProject.InstallPathEditing := ipeQueryUser
      else
        GetProject.InstallPathEditing := ipeCommandLine
    else
      GetProject.InstallPathEditing := ipeNone;
    GetProject.InstallProgName := ExtractFileName(edInstPrgName.Text);
    GetProject.WantUnInstall := chkUninstall.Checked;
    GetProject.UninstallPrevious := chkUninstPrev.Checked;
    GetProject.UninstallPrevNamed.AsSepText := bedUninstPrevName.Text;
    // update PathMacros object with fully expanded install path
    PathMacros.InstallPath := PathMacros.ExpandMacroPath(fedInstPath.Text);
  end;
  // now copy this fully expanded path into project object
  GetProject.InstallPath := PathMacros.InstallPath;
end;

procedure TDetailsTabHandler.UpdateSheet;
  {Updates tab sheet controls to show relevant project properties}
begin
  with TabSheet.Owner as TMainForm do
  begin
    edPrjID.Text := GetProject.ID;
    edPrjName.Text := GetProject.Name;
    fedInstPath.Text := GetProject.InstallPath;
    edInstPrgName.Text := GetProject.InstallProgName;
    case GetProject.InstallPathEditing of
      ipeNone:
      begin
        chkEditInstPath.Checked := False;
        chkAskForInstPath.Enabled := False;
        chkAskForInstPath.Checked := False;
      end;
      ipeCommandLine:
      begin
        chkEditInstPath.Checked := True;
        chkAskForInstPath.Enabled := True;
        chkAskForInstPath.Checked := False;
      end;
      ipeQueryUser:
      begin
        chkEditInstPath.Checked := True;
        chkAskForInstPath.Enabled := True;
        chkAskForInstPath.Checked := True;
      end;
    end;
    chkUninstall.Checked := GetProject.WantUnInstall;
    chkUninstPrev.Checked := GetProject.UninstallPrevious;
    bedUninstPrevName.Text := GetProject.UninstallPrevNamed.AsSepText;
  end;
end;

end.
