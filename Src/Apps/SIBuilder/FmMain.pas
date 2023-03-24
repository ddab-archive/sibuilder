{
 * FmMain.pas
 *
 * Class that provides SIBuilder's main program and user interface handling
 * code.
 *
 * Dependencies:
 *   - The following DelphiDabbler components are required:
 *     - Drop the Files Components release 5.0.1 or later for PJDropFiles.
 *     - Window State Components release 5.1 or later for TPJRegWdwState.
 *     - Shell folders unit release 2.2.3 or later for TPJBrowseDialog.
 *   - Custom components from SIBCmp.bpl
 *   - The following SITools COM server is required:
 *     - SITLicenseDlg.dll 1.0 or later
 *
 * v1.0 of 09 Mar 2000  - Original version.
 * v1.1 of 04 Apr 2000  - Added following features:
 *                        - on "build" page - can now create a file manifest
 *                          report for project in HTML format, and view it once
 *                          created;
 *                        - can now visit PJSoft website from Help menu.
 * v2.0 of 04 Sep 2000  - Moved project file and template naming / loading /
 *                        saving code to new class in separate unit and rewrote
 *                        this code to work with the new class. Both new XML
 *                        based file format and old binary format are supported.
 *                      - Changed project building code - now creates resource
 *                        for actual install executables rather than including
 *                        pre-built resources. Also now includes UnInstall.exe
 *                        in a hidden file group rather than as a separate
 *                        resource. The UNINSTALL conditional define is no
 *                        longer used to include the uninstall program. A new
 *                        XML-based file format was used for installation
 *                        instructions.
 *                      - Removed default extensions from file open and save
 *                        dialogs.
 *                      - Changed Options menu to Tools menu. Kept Options menu
 *                        item from Options menu on tools menu. Added new
 *                        Convert menu option to tools menu that displays dlg
 *                        box and allows files to be converted.
 *                      - Updated reporting code to report new uninstall files
 *                        correctly.
 *                      - Changed references to TProjectBuilder to TProject and
 *                        replaced refs to UBaseProject and UProjectBuilder
 *                        units with UProject.
 *                      - Added code to perform actions user can initiate from
 *                        new Welcome dialog box.
 *                      - Removed unused controls concerned with whether console
 *                        or windows install program is generated.
 *                      - Added checks for reserved group names when creating /
 *                        renaming groups.
 *                      - Renamed and re-captioned Group Properties as Edit
 *                        Group.
 *                      - Removed deletable check box from registry tab - now
 *                        appears in edit dlg box.
 *                      - Moved a lot of the tab sheet / page control handling
 *                        code into separate helper classes.
 *                      - Made some button placing more logical and consistent.
 *                      - Added new "run programs" page to allow user to specify
 *                        programs to be run after installation is complete.
 *                      - Changed from tabbed page display to having "wizard"
 *                        style interface using "previous" and "next" buttons.
 *                      - Added an info pane to display brief prompts re purpose
 *                        of each page. This pane can contain hot-linkable text.
 *                      - Added graphical side bar to page that accesses Welcome
 *                        dialog when clicked.
 *                      - Made main form get key previews and to access relevant
 *                        help for current page when F1 pressed (not always
 *                        happening if current page doesn't have focus).
 *                      - Added Edit File button to group page (to edit
 *                        installation options).
 *                      - Made bring app to front when file dropped on it.
 * v2.1 of 20 Dec 2000  - Changed compiler exception handling to handle compiler
 *                        error reports differently to other exceptions.
 *                      - While build in progress all main form now becomes
 *                        disabled and is restored as appropriate when build is
 *                        complete.
 *                      - The project resource file is now deleted after install
 *                        file has been compiled.
 *                      - A green tick is now displayed after a successful build
 *                        and a red cross after a failed build.
 * v2.2 of 29 Dec 2000  - Added new group and buttons to run programs page for
 *                        registering Inproc and Delphi Application COM servers.
 *                      - Added pop-up menus to info pane and side bar that
 *                        enables panes to be hidden and provides pop-up help
 *                        for panes.
 * v2.3 of 21 Jan 2001  - Added support for licenses:
 *                        - New button on project details page leads to a
 *                          dialog box where license info is defined.
 *                        - New menu item displays SIBuilder's license.
 *                      - Corrected bug in about box - wasn't aligning
 *                        correctly.
 *                      - Changed install report code by:
 *                        - Adding support for license and COM server
 *                          registration files.
 *                        - Changing to use dedicated class to write report.
 * v2.4 of 25 Jun 2001  - Replaced DrpFiles unit with PJDropFiles and updated
 *                        OnDropFiles handler to new format.
 * v2.5 of 25 Jun 2001  - Used TPJBrowseDialog component to select folders
 *                        instead of TChooseFolderDlg form.
 * v2.6 of 01 Apr 2002  - Now uses new versions of PJSoft components: about box,
 *                        version info and window state. Renamed unit references
 *                        as required to use the new components.
 *                      - Made window state component auto-save and restore
 *                        window position: registry settings now set using v4
 *                        component's event handler.
 *                      - Moved hard-coded strings to resource strings or
 *                        constants as appropriate.
 * v2.7 of 08 Aug 2002  - Upgraded TPJRegWdwState to v4.1 and set component to
 *                        ignore window size: this fixes sizing bug when window
 *                        first displayed.
 * v3.0 of 29 Dec 2002  - Major update due to redeign of program's main window
 *                        and movement of all tab sheet specific code into the
 *                        tab sheet manager classes.
 *                      - Added new Welcome tab sheet (instead of welcome
 *                        dialog).
 *                      - Added License Info tab sheet. This replaces dialog off
 *                        project details page, with some of the options
 *                        removed and layout modified to reflect new way of
 *                        handling licenses by installer. Moved license handling
 *                        code to tab handler for new license page.
 *                      - Added new Target OS tab sheet.
 *                      - Added new popup menus linked to groups, run programs
 *                        and registry pages.
 *                      - Added combo box build tab sheet to select compressor.
 *                      - Replaced side bar image with a button list that
 *                        selects tab sheets.
 *                      - Deleted about box component: we now use a custom
 *                        dialog box. Also deleted associated version info
 *                        component.
 *                      - Replaced list box on run programs page with tree view
 *                        and added error detection code and warning hot linked
 *                        text.
 *                      - Deleted buttons designed to be used to change the
 *                        running order of programs but that had been covered up
 *                        by COM server group box on run programs page!
 *                      - Improved main window resizing code to ensure a piece
 *                        of sidebar is not left on display when program is
 *                        started with sidebar switched off. Also improved
 *                        resizing when top info pane is switched off.
 *                      - Changed install path edit box to have integral browse
 *                        speed button (by using custom TFolderEdit control) and
 *                        deleted BrowseForFolder method: this is now replaced
 *                        by TFolderEdit event handlers.
 *                      - Made arrow cursor show over info pane's rich edit
 *                        control rather than I bar.
 *                      - Removed support for resizing when image panel
 *                        displayed or hidden.
 *                      - Moved compiler and project file preparation phase of
 *                        build process to new UBuilders unit.
 *                      - Changed build success / failure symbols to thumbs
 *                        up/down from tick/cross.
 *                      - Added new Delphi Dabbler logo to info pane and
 *                        included code that can recolour a (greyscale) logo
 *                        depending on background colour and that makes
 *                        elliptical logo clickable (to access website)
 *                      - Deleted hot linker object: a different version of this
 *                        now used (to read tagged rtf) and it has been moved to
 *                        tab sheet handler.
 *                      - Removed a few of the tab handler fields that are not
 *                        directly used.
 *                      - Removed "Hide" option for pane popup menu: now only
 *                        has "What's this" option.
 *                      - Moved install path edit check box click event to
 *                        details page tab handler.
 *                      - Moved all build tab code to build tab handler.
 *                      - Removed in proc COM server button from run tab - COM
 *                        server DLLs are now registered on Files page.
 *                      - Replaced all TGroupBox controls with new TNewGroupBox
 *                        controls.
 *                      - Changed website menu option to access
 *                        DelphiDabbler.com rather than PJSoft website and
 *                        changed access code to use new delphiDabbler internet
 *                        COM server instead of settings object.
 *                      - Now uses UImageList unit rather than ImageList.inc
 *                        include file.
 *                      - Improved handling of dropped files.
 *                      - Deleted Tutorial from help menu.
 *                      - Replaced calls the MessageDlg with calls to methods of
 *                        the TMessageBox object.
 *                      - Gave main window a unique custom class name so that
 *                        can be searched for by class name from external
 *                        programs.
 *                      - Added WM_COMMAND handler to process tab selection
 *                        called from help file macros and gave main form's
 *                        window class a new custom name that is used by WinHelp
 *                        to find the main window.
 *                      - Added check box to project details page used to
 *                        indicate we uninstall previous installations.
 *                      - Added rich edit controls for various warnings.
 *                      - Removed Convert Files dialog box and menu item.
 * v3.1 of 23 Feb 2003  - Added support for uninstallation of named projects by
 *                        adding a browse edit control to project details page.
 *                      - We now set client height of main form at runtime.
 *                      - Added menu option to access online tutorial. Moved
 *                        common website access code into separate routine that
 *                        is accessed for website home and tutorial pages.
 * v3.2 of 19 Nov 2003  - Replaced various rich edit controls used to process
 *                        hot links with new hot text controls as follows:
 *                        - Welcome tab: the welcome page's hot linked text is
 *                          now stored in a hot text control property rather
 *                          than loading from resources.
 *                        - License tab: the test prompt's is stored in a
 *                          property rather than loading from resources.
 *                        - Info pane.
 *                        - Error warnings on Run and Group tabs.
 * v3.3 of 28 Nov 2003  - Refactoring: Moved unit references from interface to
 *                        implementation.
 * v3.4 of 25 Nov 2005  - Now access website directly rather than via separate
 *                        DDNet.dll.
 *                      - Changed colour of border of button list to be same as
 *                        parent form.
 *                      - Changed to use THelpManager to handle help display
 *                        rather than rely on Delphi's built in processing.
 * v3.5 of 17 Jul 2006  - Fixed bug where "What's this" pop-up menu wasn't been
 *                        displayed for info pane.
 * v3.6 of 25 Mar 2007  - Gave fixed caption to lblCreateInstall label now there
 *                        is only one installation method.
 *                      - Added include statement for Registry.inc.
 *                      - Added OnGetFolder event handler for revised
 *                        TFolderEdit control. Deleted code that set control's
 *                        now-removed BrowseDialog property.
 *                      - Added directives to inhibit compiler warnings.
 * v3.7 of 10 Jun 2007  - Increased size of license dialog box.
 * v3.8 of 23 Feb 2008  - Replaced usage of ResIds.inc, Registry.inc, Help.inc
 *                        and FileNames.inc include files with UResources,
 *                        URegistry, UHelpContexts and UFileNames units.
 *                      - Made changes to use of registry constants re changes
 *                        in URegistry unit.
 *                      - Now gets license file name from UFileNames rather than
 *                        literal constant in this unit.
 * v3.9 of 01 Mar 2008  - Changed to display "what's this" help in main (HTML)
 *                        help file rather than in pop-up.
 *                      - Changed to use renamed help context consts.
 * v3.10 of 16 Sep 2008 - Changed to display correctly in task bar and flip 3D
 *                        task switcher on Vista.
 *                      - Changed copyright date range on welcome page to
 *                        include 2008.
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
 * The Original Code is FmMain.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmMain;


interface


uses
  // Delphi
  SysUtils, StdCtrls, ComCtrls, Buttons, ExtCtrls, Classes, ImgList, Menus,
  Controls, Dialogs, Forms, Windows, Graphics, Messages,
  // DelphiDabbler library code
  PJWdwState, PJShellFolders, PJDropFiles,
  // Project specific components
  CmpBrowseEdit, CmpButtonList, CmpFileEditors, CmpGroupBox, CmpHotText,
  // Project
  USettings, UProject, UProjFileManager, UTabSheetHandler, UWelcomeTabHandler,
  UDetailsTabHandler;


type

  {
  TMainForm:
    Main program window and user interface handling code. Also handles creation
    of installation program.
  }
  TMainForm = class(TForm)
    bedUninstPrevName: TBrowseEdit;
    blTabSelector: TNewButtonListBox;
    btnAddDataItem: TButton;
    btnAddFiles: TButton;
    btnAddGroup: TButton;
    btnAddKey: TButton;
    btnAddProg: TButton;
    btnAllOSs: TButton;
    btnBuild: TButton;
    btnDeleteDataItem: TButton;
    btnDeleteGroup: TButton;
    btnDeleteKey: TButton;
    btnDeleteProg: TButton;
    btnDelphiSvr: TButton;
    btnEditDataItem: TButton;
    btnEditFile: TButton;
    btnEditGroup: TButton;
    btnEditKey: TButton;
    btnEditProg: TButton;
    btnFileProperties: TButton;
    btnLicTest: TButton;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    btnRegExport: TButton;
    btnRegImport: TButton;
    btnRegTemplates: TButton;
    btnRemoveFile: TButton;
    btnReport: TButton;
    btnViewReport: TButton;
    bvlB0: TBevel;
    bvlBottom: TBevel;
    bvlBottomSpacer: TBevel;
    bvlInfoPane: TBevel;
    bvlInstPathHead: TBevel;
    bvlInstPrgHead: TBevel;
    bvlMenuSep: TBevel;
    bvlOSOptions: TBevel;
    bvlPageSelBottomSpacer: TBevel;
    bvlPageSelLeftSpacer: TBevel;
    bvlPageSelRightSpacer: TBevel;
    bvlPageSelTopSpacer: TBevel;
    bvlPrjIDHead: TBevel;
    bvlUninstPrev: TBevel;
    bvlVertical: TBevel;
    cbCompType: TComboBox;
    chkAskForInstPath: TCheckBox;
    chkEditInstPath: TCheckBox;
    chkUninstall: TCheckBox;
    chkUninstPrev: TCheckBox;
    chkWantLicense: TCheckBox;
    cmbLicButtons: TComboBox;
    cmbLicSize: TComboBox;
    dfLicense: TPJDropFiles;
    dfFiles: TPJDropFiles;
    dfMain: TPJFormDropFiles;
    dlgAddFiles: TOpenDialog;
    dlgBrowse: TPJBrowseDialog;
    dlgExportReg: TSaveDialog;
    dlgImportReg: TOpenDialog;
    dlgLicBrowseFile: TOpenDialog;
    dlgOpenProject: TOpenDialog;
    dlgSaveProject: TSaveDialog;
    edInstPrgName: TEdit;
    fedLicFileSrc: TFileEdit;
    edPrjID: TEdit;
    edPrjName: TEdit;
    fedInstPath: TFolderEdit;
    gpLicDlg: TNewGroupBox;
    gpLicFile: TNewGroupBox;
    gpLicTest: TNewGroupBox;
    htGroupsWarning: THotText;
    htInfo: THotText;
    htLicTest: THotText;
    htProgWarning: THotText;
    htWelcome: THotText;
    ilChecks: TImageList;
    ilSmall: TImageList;
    imgLogo: TImage;
    lblBuildResult: TLabel;
    lblClickBuild: TLabel;
    lblClickReport: TLabel;
    lblClickViewReport: TLabel;
    lblCompType: TLabel;
    lblCreateInstall: TLabel;
    lblDone: TLabel;
    lblInstPath: TLabel;
    lblInstPathHead: TLabel;
    lblInstPrgHead: TLabel;
    lblInstPrgName: TLabel;
    lblLicButtons: TLabel;
    lblLicFileSrc: TLabel;
    lblLicSize: TLabel;
    lblMarker: TLabel;
    lblOSOptions: TLabel;
    lblPrjID: TLabel;
    lblPrjIDHead: TLabel;
    lblPrjName: TLabel;
    lblProjectProgress: TLabel;
    lblSaving: TLabel;
    lblStoringProject: TLabel;
    lblUninstPrev: TLabel;
    lblUninstPrevName: TLabel;
    lblVerifying: TLabel;
    miAbout: TMenuItem;
    miClearDefault: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    miFileSpacer1: TMenuItem;
    miFileSpacer2: TMenuItem;
    miFileSpacer3: TMenuItem;
    miGroupAddFiles: TMenuItem;
    miGroupAddGroup: TMenuItem;
    miGroupDeleteGroup: TMenuItem;
    miGroupEditFile: TMenuItem;
    miGroupEditGroup: TMenuItem;
    miGroupFileProperties: TMenuItem;
    miGroupRemoveFile: TMenuItem;
    miGroupSpacer: TMenuItem;
    miHelp: TMenuItem;
    miHelpContents: TMenuItem;
    miHelpSpacer1: TMenuItem;
    miHelpSpacer2: TMenuItem;
    miLicense: TMenuItem;
    miMakeDefault: TMenuItem;
    miNewBlank: TMenuItem;
    miNewDefault: TMenuItem;
    miOpen: TMenuItem;
    miOptions: TMenuItem;
    miPaneWhatsThis: TMenuItem;
    miProgAdd: TMenuItem;
    miProgDelete: TMenuItem;
    miProgEdit: TMenuItem;
    miRegAddDataItem: TMenuItem;
    miRegAddKey: TMenuItem;
    miRegDeleteDataItem: TMenuItem;
    miRegDeleteKey: TMenuItem;
    miRegEditDataItem: TMenuItem;
    miRegEditKey: TMenuItem;
    miRegSpacer: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miServerAdd: TMenuItem;
    miServerDelete: TMenuItem;
    miServerEdit: TMenuItem;
    miTools: TMenuItem;
    miTutorial: TMenuItem;
    miWebsite: TMenuItem;
    mnuGroupTV: TPopupMenu;
    mnuMain: TMainMenu;
    mnuPanePopup: TPopupMenu;
    mnuProgramsTV: TPopupMenu;
    mnuRegTV: TPopupMenu;
    mnuServersTV: TPopupMenu;
    pgMain: TPageControl;
    pnlBody: TPanel;
    pnlBottom: TPanel;
    pnlLogo: TPanel;
    pnlPageSelect: TPanel;
    pnlProgress: TPanel;
    pnlTop: TPanel;
    rwsWdwState: TPJRegWdwState;
    tsBuild: TTabSheet;
    tsFiles: TTabSheet;
    tsLicense: TTabSheet;
    tsPrograms: TTabSheet;
    tsProjDetails: TTabSheet;
    tsRegistry: TTabSheet;
    tsTargetOS: TTabSheet;
    tsWelcome: TTabSheet;
    tvFiles: TTreeView;
    tvOSs: TTreeView;
    tvPrograms: TTreeView;
    tvReg: TTreeView;
    procedure dfMainDropFiles(Sender: TObject);
    procedure fedInstPathGetFolder(Sender: TObject; var FileSpec: String;
      var Cancelled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure imgLogoClick(Sender: TObject);
    procedure imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure miAboutClick(Sender: TObject);
    procedure miClearDefaultClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miHelpContentsClick(Sender: TObject);
    procedure miLicenseClick(Sender: TObject);
    procedure miMakeDefaultClick(Sender: TObject);
    procedure miNewBlankClick(Sender: TObject);
    procedure miNewDefaultClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miPaneWhatsThisClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miTutorialClick(Sender: TObject);
    procedure miWebsiteClick(Sender: TObject);
    procedure reInfoEnter(Sender: TObject);
    procedure rwsWdwStateGetRegData(var RootKey: HKEY; var SubKey: String);
  private
    fImgRegion: HRGN;
      {Ellipical region that approximately defines logo within image control:
      used to determine parts of logo image that can be clicked}
    fProject: TProject;
      {Instance of project object}
    fSettings: TSettings;
      {Object that maintains program's persistent settings}
    fProjFileManager: TProjFileManager;
      {Object that manages project files}
    fTabList: TTabSheetHandlerList;
      {Object that manages the page control's tabsheets}
    fWelcomeTabHandler: TWelcomeTabHandler;
      {Object that manages the welcome tab sheet}
    fDetailsTabHandler: TDetailsTabHandler;
      {Object that manages the project details tab sheet}
    procedure ReformatWindow;
      {Updates window contents according to current settings}
    procedure UpdateTopPanelColour(const Colour: TColor;
      const Force: Boolean = False);
      {Updates colour of top (info) panel to given colour and recolours the logo
      bitmap accordingly. The panel's colour is updated if Force is true or if
      colour has actually changed}
    procedure DisplayTopPanel(const Show: Boolean);
      {Show or hide the top (info) panel according to state of given flag}
    procedure FileNameChange(Sender: TObject);
      {Event handler for project file manager's file name change event - updates
      window caption and buttons that require file to be saved}
    procedure NewBlankProject;
      {Start a new blank project}
    procedure NewDefaultProject;
      {Start new project using default template if it exists, else create a new
      blank project}
    procedure OpenProject(const FileName: string);
      {Open the project in the given file. If project can't be loaded a new
      blank project is created. If an earlier file version is opened then the
      file is converted and opened with no name. Any comments about required
      changes in format are displayed}
    procedure RestartWizard;
      {Refeshes all controls from project properties and displays project
      details page of wizard}
    procedure DisplayWebPage(const Page: string);
      {Goes to given page on DelphiDabbler wehsite (or localhost if correct
      switch provided on command line. If page is '' then home page is
      displayed}
    procedure ExceptionHandler(Sender: TObject; E: Exception);
      {Handles all uncaught exceptions and displays then in dialog box}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Updates style of window to ensure this main window appears on task bar.
      Also gives window a uique class name: this will need to be found by
      WinHelp.
        @params Params [in/out] In: current parameters. Out: adjusted
          parameters.
      }
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
      {Message handler for WM_COMMAND messages: detects custom notification code
      $7FFF that can be sent to window by WinHelp to get program to take some
      action in reponse to user input in help system. The ItemID of the message
      specifies the action and the LParam provides additional info. Windows
      standard notification codes are passed on to inherited method}
    procedure WMSyscommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
      {Handles system command messages. Overrides default processing of
      minimizing and restoration of main window. This is required now we have
      inhibited application object's default processing of these messages.
        @param Msg [in/out] Details of system command. Result field set to 0 if
          we handle message to prevent default processing.
      }
  public
    procedure SaveProject;
      {Save current project under current name. If there is no current name then
      SaveProjectAs is called}
    function SaveProjectAs: Boolean;
      {Save project with a file name got from user, returning true if user
      provides a name and false if user cancels}
    property Settings: TSettings read fSettings;
      {Exposes settings object to classes that use FmMain}
    property ProjFileManager: TProjFileManager read fProjFileManager;
      {Exposes project file manager object to classes that use FmMain}
  end;

var
  MainForm: TMainForm;


implementation


uses
  // Delphi
  Math, ShellAPI, ComObj,
  // Project
  IntfSITLicenseDlg, ULicenseInfo, ULicenseDlgHelp, UFileProcs, UGroups,
  UPathMacros, UProjFileVerManager, UCompressionMgr, UGUIHelper, UMessageBox,
  UOSTabHandler, ULicenseTabhandler, UGroupsTabHandler, URunTabHandler,
  URegistryTabHandler, UBuildTabHandler, UHelpManager, URegistry, UFileNames,
  UResources, UHelpContexts, FmOptions, FmAbout, FmSIP2ConvMsg;


resourcestring
  // Error messages
  sConversionFailure        = 'Unable to convert "%s".'#10#10'%s.';
  sWebsiteErr               = 'Can''t display website.';
  sLicenseFileErr           = 'Can''t find license file.';
  sInvalidProjFile          = '%s is not a valid project file.';
  sBadWMCommand             = 'BUG: Unknown custom WM_COMMAND in '
                              + 'TMainForm.WMCommand: %0.4X.';
  sCantFindTab              = 'BUG: There is no tab sheet with tag %d.';
  sCantDisplayDisabledPage  = 'Can''t access %s page.';
  sCantDisplayInactivePage  = 'Can''t display %s page since the main window '
                              + 'is not active.';
  // Queries
  sOpenFileQuery            = 'OK to open file %s?';
  // Other messages
  sConversionSuccess        = 'Successfully converted "%s"'#10'to "%s.';
  sProjConvFileMsg          = '%0:s is a SIBuilder release %1:d project file.'
                              + #10#10'It has been converted and loaded as an '
                              + 'un-named file.';
  // Captions
  sUntitledFile             = '[Untitled]';         // main form: untitled files
  sLicenseTitle             = 'SIBuilder license';  // license dialog box


{$R *.DFM}


type

  {
  EMainForm:
    Exceptions raised by form.
  }
  EMainForm = class(Exception);


{ TMainForm }

procedure TMainForm.CreateParams(var Params: TCreateParams);
  {Updates style of window to ensure this main window appears on task bar. Also
  gives window a uique class name: this will need to be found by WinHelp.
    @params Params [in/out] In: current parameters. Out: adjusted parameters.
  }
begin
  inherited;
  Params.WinClassName := 'DelphiDabbler_SIBuilder3_Main';
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
end;

procedure TMainForm.dfMainDropFiles(Sender: TObject);
  {Catches any file that is dragged and dropped from explorer, and opens it as
  current project if it is valid}
var
  FileName: string; // name of dropped file
begin
  // Get file name
  FileName := dfMain.FileName;
  // Check a file name was dropped: do nothing if not
  if FileName <> '' then
  begin
    // Check this is a valid project file
    if TProjFileVerManager.FileVersion(FileName) > 0 then
    begin
      // If this is not project details page ask user if it's OK
      if (pgMain.ActivePage = tsProjDetails)
        or TMessageBox.ConfirmFmt(Self, sOpenFileQuery, [FileName]) then
        // Open first file dropped as new project
        OpenProject(FileName);
    end
    else
      // This isn't a valid project file
      TMessageBox.ErrorFmt(Self, sInvalidProjFile, [FileName]);
  end;
end;

procedure TMainForm.DisplayTopPanel(const Show: Boolean);
  {Show or hide the top (info) panel according to state of given flag}
begin
  if Show then
  begin
    // Display top panel (contains info panel)
    pnlTop.Visible := True;
    // Ensure correct colour is displayed in info pane
    UpdateTopPanelColour(fSettings.InfoPaneColour);
  end
  else
    // Hide pane
    pnlTop.Visible := False;
end;

procedure TMainForm.DisplayWebPage(const Page: string);
  {Goes to given page on DelphiDabbler wehsite (or localhost if correct switch
  provided on command line. If page is '' then home page is displayed}
const
  cHost = 'www.delphidabbler.com';
var
  URI: string;    // URI of required page
begin
  // Access required URI
  URI := Format('http://%s/%s', [cHost, Page]);
  if ShellExecute(Handle, nil, PChar(URI), nil, nil, SW_SHOW) <= 32 then
    TMessageBox.Error(Self, sWebsiteErr);
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
  {Handles all uncaught exceptions and displays then in dialog box}
begin
  // We display message box relative to active form since another form may be
  // at front when this handler is called
  TMessageBox.Error(Screen.ActiveForm, E.Message, E.HelpContext);
end;

procedure TMainForm.fedInstPathGetFolder(Sender: TObject;
  var FileSpec: string; var Cancelled: Boolean);
  {Handles OnGetFolder event of install path edit box. Displays a Browse for
  Folder dialog box and records selected folder and if user cancelled}
begin
  dlgBrowse.FolderName := FileSpec;
  Cancelled := not dlgBrowse.Execute;
  if not Cancelled then
    FileSpec := dlgBrowse.FolderName;
end;

procedure TMainForm.FileNameChange(Sender: TObject);
  {Event handler for project file manager's file name change event - updates
  window caption and buttons that require file to be saved}
begin
  // Update window caption
  if fProjFileManager.FileName = '' then
    Caption := Application.Title + ' - ' + sUntitledFile
  else
    Caption := Application.Title + ' - '
      + ExtractFileName(fProjFileManager.FileName);
  // Set state of Report buttons on build tab page
  EnableCtrls([btnReport, lblClickReport], fProjFileManager.FileName <> '');
end;

procedure TMainForm.FormCreate(Sender: TObject);
  {Start up code}
var
  CompMgr: TCompressionMgr; // manager for compression DLLs
begin
  // Remove hidden application window from task bar: this form is now use on
  // task bar. This required so task bar button conforms to Vista requirements.
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(
    Application.Handle,
    GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE)
      and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW
  );
  ShowWindow(Application.Handle, SW_SHOW);
  // Set height of main window: this enables use of a larger window at design
  // time since page tabs are visible then, reducing space for controls
  ClientHeight := 428;
  // Set application exception handler
  Application.OnException := ExceptionHandler;
  // Assign help contexts
  // for standard dlg boxes
  dlgSaveProject.HelpContext := IDH_DLG_SAVEPROJECT;
  dlgOpenProject.HelpContext := IDH_DLG_OPENPROJECT;
  dlgAddFiles.HelpContext := IDH_DLG_ADDFILES;
  dlgLicBrowseFile.HelpContext := IDH_DLG_BROWSELICENSE;
  dlgExportReg.HelpContext := IDH_DLG_REGEXPORTOPEN;
  dlgImportReg.HelpContext := IDH_DLG_REGIMPORTOPEN;
  dlgBrowse.HelpContext := IDH_DLG_CHOOSEFOLDER;
  // for tab sheets
  tsWelcome.HelpContext := IDH_WELCOME;
  tsProjDetails.HelpContext := IDH_PROJECTDETAILS;
  tsTargetOS.HelpContext := IDH_TARGETOS;
  tsLicense.HelpContext := IDH_LICENSEINFO;
  tsFiles.HelpContext := IDH_FILEGROUPS;
  tsPrograms.HelpContext := IDH_RUNPROGRAMS;
  tsRegistry.HelpContext := IDH_REGISTRY;
  tsBuild.HelpContext := IDH_BUILDPROJECT;
  // "what's this" help for panes: use Tag rather than HelpContext
  // using HelpContext can prevent page's help being displayed when F1 pressed
  htInfo.Tag := IDH_INFOPANE;
  pnlLogo.Tag := IDH_LOGO;
  blTabSelector.Tag := IDH_PAGESELECTOR;

  // Set tab sheet Tags to identify tab sheets to WinHelp: used when WinHelp
  // requests display of a page
  tsWelcome.Tag     := 1;
  tsProjDetails.Tag := 2;
  tsTargetOS.Tag    := 3;
  tsLicense.Tag     := 4;
  tsFiles.Tag       := 5;
  tsPrograms.Tag    := 6;
  tsRegistry.Tag    := 7;
  tsBuild.Tag       := 8;

  // Create Settings object to handle persistent settings
  fSettings := TSettings.Create;

  // Load images from resource file into image lists
  ilSmall.ResourceLoad(rtBitmap, cSmallImages, clFuchsia);
  Assert(ilSmall.Count > 0);
  ilChecks.ResourceLoad(rtBitmap, cCheckBoxes, clOlive);
  Assert(ilChecks.Count > 0);

  // Create project object with default values
  fProject := TProject.Create;

  // Create object that manage the tab sheets and get references to contents
  // all objects are freed when fTabList is freed
  fTabList := TTabSheetHandlerList.Create(fProject);
  fWelcomeTabHandler :=
    fTabList.AppendTabSheet(TWelcomeTabHandler, tsWelcome)
      as TWelcomeTabHandler;
  fWelcomeTabHandler.Enabled := fSettings.ShowWelcome;
  fDetailsTabHandler :=
    fTabList.AppendTabSheet(TDetailsTabHandler, tsProjDetails)
      as TDetailsTabHandler;
  fTabList.AppendTabSheet(TLicenseTabHandler, tsLicense);
  fTabList.AppendTabSheet(TOSTabHandler, tsTargetOS);
  fTabList.AppendTabSheet(TGroupsTabHandler, tsFiles);
  fTabList.AppendTabSheet(TRunTabHandler, tsPrograms);
  fTabList.AppendTabSheet(TRegistryTabHandler, tsRegistry);
  fTabList.AppendTabSheet(TBuildTabHandler, tsBuild);

  // Create project file manager
  fProjFileManager := TProjFileManager.Create(fProject);
  fProjFileManager.OnFileNameChange := FileNameChange;

  // Load compressors into combo box on build page (this includes (none) entry)
  CompMgr := TCompressionMgr.Create;
  try
    CompMgr.GetIDs(cbCompType.Items);
  finally
    FreeAndNil(CompMgr);
  end;

  // Decide how to open - depends on whether param is provided
  if ParamStr(1) = '' then
    // no command line
    // create new, default project, having set name of default project file
    NewDefaultProject
  else
    // we have command line
    // assume it's a project and try to open it
    OpenProject(ParamStr(1));

  // Move to first sheet of page control
  fTabList.FirstSheet(True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
  {Termination code}
begin
  // Release help system
  THelpManager.Quit;
  // Free windows objects
  DeleteObject(fImgRegion);
  // Free owned objects
  FreeAndNil(fProject);
  FreeAndNil(fSettings);
  FreeAndNil(fTabList);
  FreeAndNil(fProjFileManager);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Key down event for main form - check for F1 and display appropriate help if
  so}
begin
  if Key = VK_F1 then
    THelpManager.ShowTopic(fTabList.CurrentSheet.TabSheet.HelpContext);
end;

procedure TMainForm.FormShow(Sender: TObject);
  {GUI updating triggered when form is shown: that window is arranged properly
  and top panel is displayed if required}
begin
  ReformatWindow;
end;

procedure TMainForm.imgLogoClick(Sender: TObject);
  {OnClick event handler for logo image component: accesses delphiDabbler
  website if mouse is within logo's ellipse}
begin
  // Mouse is within logo's ellipse when cursor is hand pointer
  if imgLogo.Cursor = crHandPoint then
    miWebsite.Click;
end;

procedure TMainForm.imgLogoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
  {OnMouseMove event handler for logo image: sets mouse cursor and hint display
  depending on whether mouse cursor is within logo's elipse (which makes image
  "clickable") or not}
begin
  if PtInRegion(fImgRegion, X, Y) then
  begin
    // Mouse is within logo (it's clickable): use hand cursor and enable hints
    imgLogo.Cursor := crHandPoint;
    imgLogo.ShowHint := True;
  end
  else
  begin
    // Mouse is no in logo: use default cursor (not clickable) and no hints
    imgLogo.Cursor := crDefault;
    imgLogo.ShowHint := False;
  end;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
  {Display about box}
begin
  with TAboutDlg.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.miClearDefaultClick(Sender: TObject);
  {Delete default project template => File | New Default acts same as File |
  New Blank}
begin
  fProjFileManager.DeleteTemplateFiles;
end;

procedure TMainForm.miExitClick(Sender: TObject);
  {Exit the application}
begin
  Close;
end;

procedure TMainForm.miHelpContentsClick(Sender: TObject);
  {Display help contents}
begin
  THelpManager.Contents;
end;

procedure TMainForm.miLicenseClick(Sender: TObject);
  {Display license}
var
  LicenseFile: string;                  // file containing license
  LicenseDlg: ISITLicenseDlg;           // COM object that display license dlg
  DlgWidth, DlgHeight: Integer;         // width and height of license dlg
  DlgLeft, DlgTop: Integer;             // top and left of license dlg
begin
  // Store name of license file and check if exists
  LicenseFile := ExtractFilePath(ParamStr(0)) + cLicenseFile;
  if FileExists(LicenseFile) then
  begin
    // License file exists:
    // create dlg box object to show it
    LicenseDlg := CreateCOMObject(CLSID_SITLicenseDlg) as ISITLicenseDlg;
    // use a mono font
    LicenseDlg.SetStyle(SITLicDlg_MONOFONT);
    // set required size
    DlgWidth := TLicenseInfo.GetDlgWidth(ldsLarge);
    DlgHeight := TLicenseInfo.GetDlgHeight(ldsLarge);
    LicenseDlg.SetSize(DlgWidth, DlgHeight);
    // associate help class with dlg box
    LicenseDlg.RegisterHelp(TLicenseDlgHelp.Create, IDH_DLG_LICENSE);
    // align dlg box relative to window
    DlgLeft :=
      Max(0, Min(Left + (Width - DlgWidth) div 2, Screen.Width - DlgWidth));
    DlgTop :=
      Max(0, Min(Top + (Height - DlgHeight) div 3, Screen.Height - DlgHeight));
    // finally display license file
    LicenseDlg.DisplayLicenseFile(Handle, DlgLeft, DlgTop, PChar(sLicenseTitle),
      PChar(LicenseFile));
  end
  else
    // License file doesn't exist
    TMessageBox.Warning(Self, sLicenseFileErr);
end;

procedure TMainForm.miMakeDefaultClick(Sender: TObject);
  {Save current project as the default template for use by File | New Default}
begin
  fTabList.UpdateProject;
  fProjFileManager.SaveProjectTemplate;
end;

procedure TMainForm.miNewBlankClick(Sender: TObject);
  {Start new blank project with no file name}
begin
  NewBlankProject;
end;

procedure TMainForm.miNewDefaultClick(Sender: TObject);
  {Start new default project with no file name}
begin
  NewDefaultProject;
end;

procedure TMainForm.miOpenClick(Sender: TObject);
  {Open project specified by user from disk}
begin
  // Set default path in open dialogue box
  if fProjFileManager.FileName = '' then
    // current project not saved so use My Documents folder as default
    dlgOpenProject.InitialDir := GetDocumentPath
  else
    // we have a saved project - use it's folder as default
    dlgOpenProject.InitialDir := ExtractFilePath(fProjFileManager.FileName);
  // Display open dialog box and open appropriate project if user OKs
  if dlgOpenProject.Execute then
    OpenProject(dlgOpenProject.FileName);
end;

procedure TMainForm.miOptionsClick(Sender: TObject);
  {Get user options}
var
  Dlg: TOptionsDlg; // instance of dialogue box
begin
  // Create the dlg box and align it to window
  Dlg := TOptionsDlg.Create(Self);
  try
    // Record reference to persistant settings object
    Dlg.Settings := fSettings;
    // Display dlg box (it updates settings itself when OK clicked)
    if Dlg.ShowModal = mrOK then
    begin
      // Reformat display if any screen display settings changed
      if Dlg.DisplayChanged then
        ReformatWindow;
      // Update current tab sheet in case options have changed display
      if Assigned(fTabList.CurrentSheet) then
        fTabList.CurrentSheet.UpdateSheet;
      // update whether welcome page displayed and move to new first page if
      // welcome page is current and has been disabled
      fWelcomeTabHandler.Enabled := fSettings.ShowWelcome;
      if not fWelcomeTabHandler.Enabled
        and (fWelcomeTabHandler.IsCurrent) then
        fTabList.FirstSheet;
    end;
  finally
    // Get rid of dlg box
    FreeAndNil(Dlg);
  end;
end;

procedure TMainForm.miPaneWhatsThisClick(Sender: TObject);
  {Display "what's this" help for pane that was right clicked to get this menu}

  // ---------------------------------------------------------------------------
  function FindPopupMenuPane(const MenuItem: TMenuItem): TComponent;
    {Find the pane that was right-clicked to get the pop-up menu that this item
    was selected from. Assumes that this item is owned by a popup menu}
  var
    ParentMenu: TPopupMenu; // the poup menu that owns this menuitem
  begin
    // Find the menu item's parent and assume its a popup menu
    ParentMenu := (MenuItem.GetParentMenu as TPopupMenu);
    // Find the componenent that was right-clicked to display the menu
    Result := ParentMenu.PopupComponent as TComponent;
    Assert(Result <> nil);
  end;
  // ---------------------------------------------------------------------------

begin
  // We use the tag rather than HelpContext since this would cause help for
  // control to be displayed rather than the page's help if control focus when
  // F1 clicked
  THelpManager.ShowTopic(FindPopupMenuPane(Sender as TMenuItem).Tag);
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
  {Save current project with file name specified by user}
begin
  SaveProjectAs;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
  {Save current project using existing name (if any)}
begin
  SaveProject;
end;

procedure TMainForm.miTutorialClick(Sender: TObject);
  {Goto online tutorial on delphiDabbler website}
begin
  DisplayWebPage('sibuilder/tutorial');
end;

procedure TMainForm.miWebsiteClick(Sender: TObject);
  {Go to delphiDabbler Website}
begin
  DisplayWebPage('');
end;

procedure TMainForm.NewBlankProject;
  {Start a new blank project}
const
  cMainGroupName = 'Main';  // name of main group
begin
  // Reset project object, and add default Main group and default compressor
  fProjFileManager.EmptyProject;
  fProject.InstallGroups.Add(
    TGroup.Create(cMainGroupName, PathMacros.Names[pnInstall]));
  fProject.CompressorID := fSettings.DefaultCompressor;
  // Update GUI with new details and move to first page
  RestartWizard;
end;

procedure TMainForm.NewDefaultProject;
  {Start new project using default template if it exists, else create a new
  blank project}
begin
  if fProjFileManager.LoadTemplate then
    RestartWizard
  else
    NewBlankProject;
end;

procedure TMainForm.OpenProject(const FileName: string);
  {Open the project in the given file. If project can't be loaded a new blank
  project is created. If an earlier file version is opened then the file is
  converted and opened with no name. Any comments about required changes in
  format are displayed}
var
  Ver: Integer;           // project file version
  Comments: TStringList;  // receives comments about project being opened
begin
  // Create comments list: used to record comments passed back from project file
  // manager about the project that has been opened (v2->v3 coversions
  Comments := TStringList.Create;
  try
    // Open the project, storing file verison (0 = error)
    Ver := fProjFileManager.LoadProject(FileName, Comments);
    // Report any special cases
    case Ver of
      0:        // invalid file - not opened
        TMessageBox.ErrorFmt(Self, sInvalidProjFile, [FileName]);
      100:      // release 1 format file converted
        TMessageBox.InformationFmt(Self, sProjConvFileMsg, [FileName, 1]);
      200..299: // release 2 format file converted: may have comments
      begin
        if Comments.Count > 0 then
          // there are comments: display them
          TSIP2ConvMsgDlg.Display(Self, FileName, Comments)
        else
          // there are no comments: simply say conversion took place
          TMessageBox.InformationFmt(Self, sProjConvFileMsg, [FileName, 2]);
      end;
    end;
    if Ver <> 0 then
      // File successfully loaded
      RestartWizard
    else
      // There was an error - start a blank project
      NewBlankProject;
  finally
    FreeAndNil(Comments);
  end;
end;

procedure TMainForm.ReformatWindow;
  {Updates window contents according to current settings}
begin
  // Show or hide top panel as required
  DisplayTopPanel(fSettings.ShowInfoPane);
end;

procedure TMainForm.reInfoEnter(Sender: TObject);
  {OnEnter event handler for info pane - rejects focus by moving it to page
  control}
begin
  pgMain.SetFocus;
end;

procedure TMainForm.RestartWizard;
  {Refeshes all controls from project properties and displays project details
  page of wizard}
begin
  fTabList.UpdateSheets;
  fTabList.GotoSheet(fDetailsTabHandler, True);
end;

procedure TMainForm.rwsWdwStateGetRegData(var RootKey: HKEY;
  var SubKey: string);
  {Gets root and sub key useed to read/write window state data in regsitry:
  event is triggered by the window state component}
begin
  RootKey := HKEY_CURRENT_USER;
  SubKey := cSIBWindowKey;
end;

procedure TMainForm.SaveProject;
  {Save current project under current name. If there is no current name then
  SaveProjectAs is called}
begin
  // Make sure that project object reflects user's latest entries
  fTabList.UpdateProject;
  // Now save the project
  if fProjFileManager.FileName = '' then
    SaveProjectAs
  else
    fProjFileManager.SaveProject;
end;

function TMainForm.SaveProjectAs: Boolean;
  {Save project with a file name got from user, returning true if user provides
  a name and false if user cancels}
var
  FileName: string; // name of file to save
begin
  // Make sure project object reflects users latest entries
  fTabList.UpdateProject;
  // Set the save dlg's default folder
  if fProjFileManager.FileName = '' then
    dlgSaveProject.InitialDir := GetDocumentPath
  else
    dlgSaveProject.InitialDir := ExtractFilePath(fProjFileManager.FileName);
  // Get file name using open dlg and record if OK pressed
  Result := dlgSaveProject.Execute;
  if Result then
  begin
    FileName := dlgSaveProject.FileName;
    if ExtractFileExt(FileName) = '' then
      FileName := FileName + cProjExt;
    // User OK'd - record new file name and save project
    fProjFileManager.SaveProjectAs(FileName);
  end;
end;

procedure TMainForm.UpdateTopPanelColour(const Colour: TColor;
  const Force: Boolean = False);
  {Updates colour of top (info) panel to given colour and recolours the logo
  bitmap accordingly. The panel's colour is updated if Force is true or if
  colour has actually changed}
var
  GreyLogoBmp: TBitmap; // bitmap that stores greyscale version of logo
  NewLogoBmp: TBitmap;  // bitmap that stores recoloured logo
begin
  // Decide if we need to do anything
  if Force or (pnlTop.Color <> Colour) then
  begin
    // Set panel colour: this changes colour of child controls
    pnlTop.Color := Colour;
    // Create a bitmap logo with same background colour as panel
    NewLogoBmp := nil;
    // create greyscale version of bitmap from program's resources
    GreyLogoBmp := TBitmap.Create;
    try
      GreyLogoBmp.LoadFromResourceName(HInstance, cLogoBmp);
      // create new bitmap to hold new logo
      NewLogoBmp := TBitmap.Create;
      // colour the logo based on greyscale bitmap, using new background colour
      // and purple forground (text) colour
      UGUIHelper.ColouriseGreyScale(
        GreyLogoBmp,
        NewLogoBmp,
        Colour,
        clPurple
      );
      // display the new logo in image control: ensuring that image is correct
      // size and is centred in logo panel
      imgLogo.Picture.Assign(NewLogoBmp);
      imgLogo.Height := NewLogoBmp.Height;
      imgLogo.Width := NewLogoBmp.Width;
      imgLogo.Left := (pnlLogo.Width - imgLogo.Width) div 2;
      imgLogo.Top := (pnlLogo.Height - imgLogo.Height) div 2;
      // update clickable elliptical region of logo image
      if fImgRegion <> 0 then
        DeleteObject(fImgRegion);
      fImgRegion := CreateEllipticRgn(0, 0, imgLogo.Width, imgLogo.Height);
    finally
      FreeAndNil(NewLogoBmp);
      FreeAndNil(GreyLogoBmp);
    end;
  end;
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
  {Message handler for WM_COMMAND messages: detects custom notification code
  $7FFF that can be sent to window by WinHelp to get program to take some action
  in reponse to user input in help system. The ItemID of the message specifies
  the action and the LParam provides additional info. Windows standard
  notification codes are passed on to inherited method}

  //----------------------------------------------------------------------------
  function TagToTabSheet(ATag: Integer): TTabSheet;
    {Searches thru all tab sheets in page control looking for one with given Tag
    property value and returns reference to the sheet}
  var
    Idx: Integer;   // loops thru tab sheets in page control
    TS: TTabSheet;  // references a tab sheet
  begin
    // Assume can't find tab
    Result := nil;
    // Loop thru all tab sheets searching for required tag
    for Idx := 0 to Pred(pgMain.PageCount) do
    begin
      TS := pgMain.Pages[Idx];
      if TS.Tag = ATag then
      begin
        Result := TS;
        Break;
      end;
    end;
    // Check we've found tab: error if not
    if Result = nil then
      raise EMainForm.CreateFmt(sCantFindTab, [ATag]);
  end;
  //----------------------------------------------------------------------------

var
  TS: TTabSheet;      // reference to a tab sheet
  ActiveForm: TForm;  // reference to active form when this handler called
begin
  // Check to see if this is a custom command (notify code is $7FFF)
  if Msg.NotifyCode = $7FFF then
  begin
    try
      // This is custom WM_COMMAND message: ID contains command kind and
      // LParam contains action code
      case Msg.ItemID of
        $0001:
        begin
          // Goto page identified by its tag: help macro is
          // SH(delphiDabbler_SIBuilder3_Main, SIBuilder, 0x7fff0001, tabsheettag)
          // find tab's caption
          TS := TagToTabSheet(TMessage(Msg).LParam);
          // can't go to page if form not active: may hide a modal dialog!
          ActiveForm := Screen.ActiveForm;
          if ActiveForm <> Self then
          begin
            // bring previous active back to front before raising exception
            ActiveForm.BringToFront;
            raise EMainForm.CreateFmt(sCantDisplayInactivePage, [TS.Caption]);
          end;
          // goto to sheet (using caption) - if enabled
          if not TS.Enabled then
            raise EMainForm.CreateFmt(sCantDisplayDisabledPage, [TS.Caption]);
          fTabList.GotoSheet(TS.Caption);
        end;
        else
          // unrecognised custom command: *** this is a bug ***
          raise EMainForm.CreateFmt(sBadWMCommand, [Msg.ItemId]);
      end;
    finally
      // We handled message
      Msg.Result := 0;
    end;
  end
  else
    inherited;
end;

procedure TMainForm.WMSyscommand(var Msg: TWMSysCommand);
  {Handles system command messages. Overrides default processing of minimizing
  and restoration of main window. This is required now we have inhibited
  application object's default processing of these messages.
    @param Msg [in/out] Details of system command. Result field set to 0 if we
      handle message to prevent default processing.
  }
begin
  // Note: according to Win API low order four bits of Msg.CmdType are reserved
  // for use by windows. We therefore mask out those bytes before processing.
  case (Msg.CmdType and $FFF0) of
    SC_MINIMIZE:
    begin
      ShowWindow(Handle, SW_MINIMIZE);
      Msg.Result := 0;
    end;
    SC_RESTORE:
    begin
      ShowWindow(Handle, SW_RESTORE);
      Msg.Result := 0;
    end;
    else
      inherited;
  end;
end;

end.

