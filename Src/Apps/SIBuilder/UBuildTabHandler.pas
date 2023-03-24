{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UBuildTabHandler.pas
  @COMMENTS                 TTabSheetHandler descendant that provides the
                            required additional functionality for the "build
                            project" tab sheet.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 20/12/2000
      @COMMENTS             Added code to reset build success / failure "glyph"
                            when Build page is opened.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Major revision of the build tab handler - all of
                            functionality of build tab now controlled from the
                            handler. Changes are:
                            + Added support for main form's new compression type
                              combo box.
                            + Modified display update code to change caption of
                              instal program creation label to allow for new
                              packager "compiler".
                            + Deleted Enter method: now base class method calls
                              UpdateSheet which is what we used to do here.
                            + Removed hot link setting: hot links are now
                              defined in tagged rtf file and automatically
                              created when loaded.
                            + Moved all build tab code that was in MainForm to
                              this class (inc Build button functionality).
                            + Made sure all event handlers are nilled when class
                              is destroyed.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
                            + Added support for passing help contexts from
                              exceptions raised when validation of project to
                              the message box that displays error messages.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/11/2003
      @COMMENTS             + Changed reference to old info pane rich text
                              control to new hot text control when
                              enabling/disabling all controls and allowed hot
                              text to be disabled since it supports this state
                              visually.
                            + Removed reference to removed UActiveRTFWrapper
                              unit.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused units and
                            moved some units from interface to implementation.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 25/03/2007
      @COMMENTS             + Changed to use revised builder object factory
                              method.
                            + Removed reference to TSettings.Compiler property.
                            + Adjusted code to remove dependencies on type of
                              builder object - there is now only one.
                            + Removed code that sets caption of lblCreateInstall
                              label depending on installer creation method.
                            + Cleared project progress label at end of
                              compilation.
                            + Added directive to inhibit compiler warnings.
    )
    @REVISION(
      @VERSION              2.4
      @DATE                 19/02/2008
      @COMMENTS             Replaced usage of ResIds.inc include file with
                            UResources unit.
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
 * The Original Code is UBuildTabHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2007 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UBuildTabHandler;


{$WARN UNSAFE_TYPE OFF}


interface


uses
  // Delphi
  SysUtils, StdCtrls, ComCtrls,
  // Project
  UTabSheetHandler, UBuilders;


type

  {
  TBuildTabHandler:
    Encapsulates the "build project" tab sheet and provides the relevant
    specialised processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.
  }
  TBuildTabHandler = class(TTabSheetHandler)
  private
    procedure CompTypeChange(Sender: TObject);
      {OnChange event handler for compression type combo box: sets project's
      compression type to newly selected value}
    procedure ReportBtnClick(Sender: TObject);
      {Report button click event handler: generates build report using current
      report type}
    procedure ViewReportBtnClick(Sender: TObject);
      {View report button click event handler: displays report generated by
      clicking Report button}
    procedure BuildBtnClick(Sender: TObject);
      {Handler for build button click event: validates the project and create
      the install program from it}
    procedure HighlightProgressLabel(const Lbl: TLabel);
      {Highlights the given label by displaying a marker next to it}
    procedure BuildProgress(const Msg: string);
      {Event handler for Project and Builder progress events: display message}
    procedure BuildStageUpdate(const Code: TBuilderStage);
      {Event handler for OnStage event of builder object: highlights appropriate
      progress label}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: records reference to related tab sheet control and
      sets supported control's event handlers}
    destructor Destroy; override;
      {Class destructor: nil all the form's event handlers that were handled by
      this class}
    procedure UpdateSheet; override;
      {Updates controls on page to required values}
  end;

  {
  EBuildTabHandler:
    Class of exception raised by TBuildTabHandler objects.

    Inheritance: EBuildTabHandler -> [Exception]
  }
  EBuildTabHandler = class(Exception);


implementation


uses
  // Delphi
  Windows, Classes, ShellAPI, Controls, Graphics, Forms,
  // Project
  FmMain, UFileProcs, UGUIHelper, UInstallReport, UMessageBox, UResources;


const
  // Extensions that we will use for reports
  cReportExts: array[TInstallReportKinds] of string = ('.html', '.txt');


resourcestring
  // Error messages
  sReportDispErr =
    'Can''t display report named %0:s due to the following error:'#13#13'%1:s.';
  sReportNotSaved =
    'You need to have saved the project before you can produce a report';
  sReportNotCreated =
    'You need to create the report by pressing the Report button first';
  sPrjSaveErr = 'The project has not been saved.';
  sBuildFailureMsg = 'Project was not built because of the following error:'
    + #10#10'%s.';
  // Progress message
  sBuildSuccessMsg = 'Installation program successfully created:'#10#10;
  // Queries
  sProjSaveQuery = 'Your project is not saved - would you like to save it now?';


{ TBuildTabHandler }

procedure TBuildTabHandler.BuildBtnClick(Sender: TObject);
  {Handler for build button click event: validates the project and create the
  install program from it}

  // ---------------------------------------------------------------------------
  procedure DisableControls;
    {Disables form and menu and enables and raises progress panel}
  begin
    EnableForm(MainForm, False);
    with MainForm do
    begin
      EnableMainMenu(mnuMain, False);
      EnableCtrls([htInfo], False, True);
      EnableCtrls([pnlProgress], True, True);
      pnlProgress.BevelOuter := bvRaised;
    end;
   end;

  procedure EnsureProjectSaved;
    {Makes sure that project is saved}
  begin
    // Notify that we're at the saving stage of the build process
    HighlightProgressLabel(MainForm.lblSaving);
    if MainForm.ProjFileManager.FileName <> '' then
      // Project has been saved before (we have file name): save it
      MainForm.SaveProject
    else
      // Project not saved: ask user if wants to save and use save dialog if so
      if not TMessageBox.Confirm(MainForm, sProjSaveQuery)
        or not MainForm.SaveProjectAs then
        // project has not been saved: this is error
        raise EBuildTabHandler.Create(sPrjSaveErr);
  end;

  procedure VerifyProject;
    {Verifies the project}
  begin
    // Notify that we're at the verifying stage of the build process
    HighlightProgressLabel(MainForm.lblVerifying);
    GetProject.Validate;
  end;

  procedure NotifyBuildCompleted(const Success: Boolean; const Msg: string;
    const AHelpContext: Integer);
    {Reports result of a build using given message and success flag. Also passes
    help context to the messagge boxes it displays}
  const
    cSuccess = #$43;  // success symbol: WingDings thumbs up
    cFailure = #$44;  // failure symbol: WingDings thumbs down
  begin
    with MainForm do
    begin
      // Notify that we've completed the build
      HighlightProgressLabel(lblDone);
      if Success then
      begin
        // Display success symbol
        lblBuildResult.Font.Color := clGreen;
        lblBuildResult.Caption := cSuccess;
        // Display success message
        TMessageBox.Information(MainForm, Msg, AHelpContext);
      end
      else
      begin
        // Display failure symbol
        lblBuildResult.Font.Color := clRed;
        lblBuildResult.Caption := cFailure;
        // Display error message
        TMessageBox.Error(MainForm, Msg, AHelpContext);
      end;
    end;
  end;

  procedure RestoreControls;
    {Restores controls to state before build}
  begin
    with MainForm do
    begin
      // Lower the progress bevel again
      pnlProgress.BevelOuter := bvLowered;
      // Enable relevant controls
      EnableMainMenu(mnuMain, True);
      EnableForm(MainForm, True);
      EnableCtrls([btnReport, lblClickReport], ProjFileManager.FileName <> '');
      EnableCtrls([btnViewReport, lblClickViewReport, btnNext], False);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Builder: TBuilder;  // builder object used to create installer from project
begin
  try
    try
      // Disable menu and controls
      DisableControls;
      // Save project
      EnsureProjectSaved;
      // Verify project
      VerifyProject;
      // Build the installer
      GetProject.OnProgress := BuildProgress;     // build progress from project
      // create builder object
      Builder := TBuilderFactory.CreateBuilder;
      try
        // set up builder object
        Builder.Project := GetProject;
        Builder.InstallerPath := MainForm.ProjFileManager.Path;
        Builder.OnStage := BuildStageUpdate;
        Builder.OnBuild := BuildProgress;
        // do the actual build
        Builder.Execute;
      finally
        Builder.Free;
      end;
      // If we get here without exception being raised all is well: say so
      NotifyBuildCompleted(
        True,
        sBuildSuccessMsg + MakePathName(MainForm.ProjFileManager.Path)
          + GetProject.InstallProgName,
        0
      );
    finally
      // Re-enable required controls
      RestoreControls;
      // Reset progress caption
      MainForm.lblProjectProgress.Caption := '';
    end;
  except
    // Trap all exceptions that occurred while trying to build the project and
    // report them.
    on E: EBuilder do
      // errors reported from builder object: report message as is
      NotifyBuildCompleted(False, E.Message, E.HelpContext);
    on E: Exception do
    begin
      // other errors: add additional narrative
      NotifyBuildCompleted(
        False,
        Format(sBuildFailureMsg, [E.Message]),
        E.HelpContext
      );
    end;
  end;
end;

procedure TBuildTabHandler.BuildProgress(const Msg: string);
  {Event handler for Project and Builder progress events: display message}
begin
  // Display progress and let application breath
  MainForm.lblProjectProgress.Caption := Msg;
  Application.ProcessMessages;
end;

procedure TBuildTabHandler.BuildStageUpdate(const Code: TBuilderStage);
  {Event handler for OnStage event of builder object: highlights appropriate
  progress label}
begin
  with MainForm do
  begin
    case Code of
      bsBuilding: HighlightProgressLabel(lblStoringProject);
      bsCompiling: HighlightProgressLabel(lblCreateInstall);
    end;
  end;
end;

procedure TBuildTabHandler.CompTypeChange(Sender: TObject);
  {OnChange event handler for compression type combo box: sets project's
  compression type to newly selected value}
begin
  GetProject.CompressorID := MainForm.cbCompType.Text;
end;

constructor TBuildTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: sets supported control's event handlers}
begin
  inherited;
  with MainForm do
  begin
    cbCompType.OnChange := CompTypeChange;
    btnReport.OnClick := ReportBtnClick;
    btnViewReport.OnClick := ViewReportBtnClick;
    btnBuild.OnClick := BuildBtnClick;
  end;
end;

destructor TBuildTabHandler.Destroy;
  {Class destructor: nil all the form's event handlers that were handled by this
  class}
begin
  with MainForm do
  begin
    cbCompType.OnChange := nil;
    btnReport.OnClick := nil;
    btnViewReport.OnClick := nil;
    btnBuild.OnClick := nil;
  end;
  inherited;
end;

procedure TBuildTabHandler.HighlightProgressLabel(const Lbl: TLabel);
  {Highlights the given label by displaying a marker next to it}
begin
  // Move marker and ensure visible & let application breath
  MainForm.lblMarker.Top := Lbl.Top;
  MainForm.lblMarker.Visible := True;
  Application.ProcessMessages;
  // Pause long enough to allow user to see highlight move!
  Sleep(100);
end;

function TBuildTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cBuildInfoResId;
end;

procedure TBuildTabHandler.ReportBtnClick(Sender: TObject);
  {Report button click event handler: generates build report using current
  report type}
var
  Manifest: IInstallReport;     // object that writes installation report
  Stream: TStream;              // stream on which report is written
  RepKind: TInstallReportKinds; // kind of report to produce
begin
  // Check that button is enabled before preceeding. This check is needed since
  // it's possible that this handler will be called from elsewhere.
  if (Sender as TButton).Enabled then
  begin
    // Record required report kind
    RepKind := TInstallReportKinds(MainForm.Settings.ReportKind);
    // Create an HTML install report object
    Manifest := TInstallReportFactory.CreateInstallReport(GetProject, RepKind);
    // Create file where report to be written
    Stream := TFileStream.Create(
      ChangeFileExt(MainForm.ProjFileManager.FileName, cReportExts[RepKind]),
      fmCreate);
    try
      // Write the report
      Manifest.WriteToStream(Stream);
    finally
      // Close the file
      Stream.Free;
    end;
    // Enable "View Report" button
    EnableCtrls([MainForm.btnViewReport, MainForm.lblClickViewReport], True);
  end
  else
    // Button disabled (because report not saved): report problem
    TMessageBox.Error(MainForm, sReportNotSaved);
end;

procedure TBuildTabHandler.UpdateSheet;
  {Updates controls on page to required values}
var
  CompID: string; // id of a compressor
begin
  with MainForm do
  begin
    // Delete any build result marker
    lblBuildResult.Caption := '';
    // Delete any contents in project progress label
    lblProjectProgress.Caption := '';
    // Hide marker label
    lblMarker.Visible := False;
    // Disable "View Report"
    EnableCtrls([btnViewReport, lblClickViewReport], False);
    // Update compression combo box
    CompID := GetProject.CompressorID;
    cbCompType.ItemIndex := cbCompType.Items.IndexOf(CompID);
  end;
end;

procedure TBuildTabHandler.ViewReportBtnClick(Sender: TObject);
  {View report button click event handler: displays report generated by clicking
  Report button}
var
  RepKind: TInstallReportKinds; // kind of report to view
  RepFile: string;              // report file name
begin
  // Check that button is enabled before preceeding. This check is needed since
  // it's possible that this handler will be called from elsewhere.
  if (Sender as TButton).Enabled then
  begin
    // Record report file name: based on project file name with extension that
    // depends on report kind
    RepKind := TInstallReportKinds(MainForm.Settings.ReportKind);
    RepFile := ChangeFileExt(
      MainForm.ProjFileManager.FileName, cReportExts[RepKind]
    );
    // Display report using the application registered to show file
    if ShellExecute(0, nil, PChar(RepFile), nil, nil, SW_SHOW) <= 32 then
      // error: can't display report: say so
      TMessageBox.ErrorFmt(
        MainForm, sReportDispErr, [RepFile, SysErrorMessage(GetLastError)]
      );
  end
  else
    // no report available since report not generated: say so
    TMessageBox.Error(MainForm, sReportNotCreated);
end;

end.
