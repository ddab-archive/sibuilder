{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpFileEditors.pas
  @COMMENTS                 Defines a set of specialised browse edit and action
                            combo boxes that get file specifications from user,
                            using standard file open and "browse for folder"
                            dialogs, and displays the details in the controls.
                            These components are used by SIBuilder.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/03/2007
      @COMMENTS             + Removed BrowseDialog property from TFolderEdit and
                              TFolderComboxBox and replaced with OnGetFolder
                              event. Did this to decouple SIBCmp package from
                              DelphiDabbler components.
                            + Modified support code re above changes.
                            + Removed code made redundant by above changes.
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
 * The Original Code is CmpFileEditors.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2007 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpFileEditors;


interface


uses
  // Delphi
  Classes, Dialogs,
  // SIBuilder component code
  CmpBrowseEdit, CmpActionComboBox;


type
                   
  {
  TBrowseEvent:
    Method type of the OnXXXBrowse event handlers of components defined in this
    unit. The event handlers are passed a file specification which they can
    examine and change.
  }
  TBrowseEvent = procedure(Sender: TObject; var FileSpec: string) of object;

  {
  TBrowseFolderEvent:
    Event to be handled when browser for folder dialog is to be used to get a
    FileSpec. Handler must display this dialog and update file spec. Handler
    should also set Cancelled to true if user cancels dialog.
  }
  TBrowserFolderEvent = procedure(Sender: TObject; var FileSpec: string;
    var Cancelled: Boolean) of object;

  {
  TCustomFileEdit:
    Abstract base class for components that display a dialog box to get a file
    spec from user when the browse edit control's browse button is clicked. The
    contents of the edit control are considered to be file specifications that
    are used as defaults for the dialog boxes. The edit control text is updated
    with the user's entry in the dialog box. Sub classes need to provide the
    interaction with the user to get the file spec by overriding the GetFileSpec
    method.
  }
  TCustomFileEdit = class(TCustomBrowseEdit)
  private // properties
    fOnBeforeBrowse: TBrowseEvent;
    fOnAfterBrowse: TBrowseEvent;
    fBrowseCancelled: Boolean;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; virtual; abstract;
      {Gets a file spec from user. The default file spec is passed in via
      FileSpec, which is also used to return the entered value. True is returned
      if user OK's the entry and false if user cancels}
    procedure DoBrowse; override;
      {Override of inherited DoBrowse method: triggers OnBeforeBrowse event to
      enable file spec from edit control to be changed before getting user to
      edit the file spec. Then triggers OnAfterBrowse event to again enable file
      spec entered by user to be altered before displaying in edit control}
    property OnBeforeBrowse: TBrowseEvent
      read fOnBeforeBrowse write fOnBeforeBrowse;
      {Event triggered before dialog box is displayed containing default file
      spec. Enables file spec to be changed before passing to dialog box}
    property OnAfterBrowse: TBrowseEvent
      read fOnAfterBrowse write fOnAfterBrowse;
      {Event triggered after a browse dialog has been closed. Enables file spec
      entered by user to be changed before displaying in edit control}
  public
    property BrowseCancelled: Boolean
      read fBrowseCancelled write fBrowseCancelled;
      {Read only property that is set to true if user cancelled the browse
      dialog box}
  end;

  {
  TFileEdit:
    Browse edit control that is used to get file names from user when browse
    button is clicked and display them in the edit control. A standard file open
    or save dialog box is used to get file name from user. The dialog box
    control should be associated with this control via the OpenDialog property.
  }
  TFileEdit = class(TCustomFileEdit)
  private // properties
    fOpenDialog: TOpenDialog;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; override;
      {Gets a file spec from user using a standard file open/save dialog. The
      default file spec and entered file spec are passed using in FileSpec.
      True is returned if user OK's the entry and false if user cancels}
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Used to respond to removal of dialog box component referenced by our
      OpenDialog property: the property is nilled when dialog box component is
      removed}
  published
    // Published inherited properties
    property ButtonWidth;
    property ButtonGlyph;
    property ButtonText;
    property ButtonCursor;
    property ButtonHint;
    property OnBeforeBrowse;
    property OnAfterBrowse;
    // New properties
    property OpenDialog: TOpenDialog
      read fOpenDialog write fOpenDialog;
      {Reference to open/save dialog component used to get file spec from user}
  end;

  {
  TFolderEdit:
    Browse edit control that is used to get folder names from user when browse
    button is clicked and display them in the edit control. A "Browse for
    folder" dialog box is used to get file name from user. The dialog box
    control should be associated with this control via the BrowseDialog
    property.
  }
  TFolderEdit = class(TCustomFileEdit)
  private // properties
    fOnGetFolder: TBrowserFolderEvent;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; override;
      {Gets a folder name by triggering OnGetFolder event. True is returned if
      user OK's the entry and false if user cancels}
  published
    // Published inherited properties
    property ButtonWidth;
    property ButtonGlyph;
    property ButtonText;
    property ButtonCursor;
    property ButtonHint;
    property OnBeforeBrowse;
    property OnAfterBrowse;
    // New event
    property OnGetFolder: TBrowserFolderEvent
      read fOnGetFolder write fOnGetFolder;
      {Event triggered when folder required from user. Caller should display a
      suitable dialog box and set event handler's FileSpec parameter to required
      folder and set Cancelled parameter true if user cancels}
  end;

  {
  TCustomFileComboBox:
    Abstract base class for components that display a dialog box to get a file
    spec from user when the combo box's action text is clicked. The file spec is
    entered in the combo's edit box and the previous value is used as the
    default in the dialog box. Sub classes need to provide the interaction with
    the user to get the file spec by overriding the GetFileSpec method.
  }
  TCustomFileComboBox = class(TCustomActionComboBox)
  private // properties
    fOnBeforeBrowse: TBrowseEvent;
    fOnAfterBrowse: TBrowseEvent;
    fBrowseCancelled: Boolean;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; virtual; abstract;
      {Gets a file spec from user. The default file spec is passed in via
      FileSpec, which is also used to return the entered value. True is returned
      if user OK's the entry and false if user cancels}
    procedure DoAction; override;
      {Override of inherited DoAction method: triggers OnBeforeBrowse event to
      enable file spec from combo's edit box to be changed before getting user
      to edit the file spec. Then triggers OnAfterBrowse event to again enable
      file spec entered by user to be altered before displaying in combo's edit
      box. Before any event is triggered previously stored text is written back
      to combo box to replace the action text that has been placed there}
    property OnBeforeBrowse: TBrowseEvent
      read fOnBeforeBrowse write fOnBeforeBrowse;
      {Event triggered before dialog box is displayed containing default file
      spec. Enables file spec to be changed before passing to dialog box}
    property OnAfterBrowse: TBrowseEvent
      read fOnAfterBrowse write fOnAfterBrowse;
      {Event triggered after a browse dialog has been closed. Enables file spec
      entered by user to be changed before displaying in combo box.}
  public
    property BrowseCancelled: Boolean
      read fBrowseCancelled write fBrowseCancelled;
      {Read only property that is set to true if user cancelled the browse
      dialog box}
  end;

  {
  TFileComboBox:
    Action combo box that is used to get file names from user when action text
    is clicked and display them in combo box's edit box. A standard file open
    or save dialog box is used to get file name from user. The dialog box
    control should be associated with this control via the OpenDialog property.
  }
  TFileComboBox = class(TCustomFileComboBox)
  private // properties
    fOpenDialog: TOpenDialog;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; override;
      {Gets a file spec from user using a standard file open/save dialog. The
      default file spec and entered file spec are passed using in FileSpec.
      True is returned if user OK's the entry and false if user cancels}
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Used to respond to removal of dialog box component referenced by our
      OpenDialog property: the property is nilled when dialog box component is
      removed}
  published
    // Published inherited properties
    property ActionText;
    property OnBeforeBrowse;
    property OnAfterBrowse;
    // New properties
    property OpenDialog: TOpenDialog
      read fOpenDialog write fOpenDialog;
      {Reference to open/save dialog component used to get file spec from user}
  end;

  {
  TFolderComboBox:
    Action combo box that is used to get file names from user when action text
    is clicked and display them in combo box's edit box. A "Browse for folder"
    dialog box is used to get file name from user. The dialog box control should
    be associated with this control via the BrowseDialog property.
  }
  TFolderComboBox = class(TCustomFileComboBox)
  private // properties
    fOnGetFolder: TBrowserFolderEvent;
  protected
    function GetFileSpec(var FileSpec: string): Boolean; override;
      {Gets a folder name by triggering OnGetFolder event. True is returned if
      user OK's the entry and false if user cancels}
  published
    // Published inherited properties
    property ActionText;
    property OnBeforeBrowse;
    property OnAfterBrowse;
    // New event
    property OnGetFolder: TBrowserFolderEvent
      read fOnGetFolder write fOnGetFolder;
      {Event triggered when folder required from user. Caller should display a
      suitable dialog box and set event handler's FileSpec parameter to required
      folder and set Cancelled parameter true if user cancels}
  end;


procedure Register;
  {Register the components}


implementation


uses
  // Project
  UFileProcs;


procedure Register;
  {Register the components}
begin
  RegisterComponents('SIBuilder',
    [TFileEdit, TFolderEdit, TFileComboBox, TFolderComboBox]);
end;


function GetFileFromUser(const Dlg: TOpenDialog;
  var FileSpec: string): Boolean;
  {Gets a file name from user using the given standard file dialog (either open
  or save). The given file spec is used as the default to be displayed in the
  dialog box and is updated by any name entered by the user. True is returned if
  the user OKs and false if the user cancels}
var
  FileName, DirName: string;  // directory and file name from file spec
begin
  // Get the directory and file names from given file spec and set up dialog box
  if not ExtractFolderAndFileNames(FileSpec, DirName, FileName) then
    DirName := 'C:\';
  Dlg.FileName := FileName;
  Dlg.InitialDir := DirName;
  // Display dialog box and record if user OKs
  Result := Dlg.Execute;
  if Result then
    // User OK'd: update file spec with that entered by user
    FileSpec := Dlg.FileName;
end;

function GetFolderFromEvent(const Sender: TObject;
  const Event: TBrowserFolderEvent; var Folder: string): Boolean;
  {Triggers given event that caller handles to get a folder from user via some
  suitable dialog box or other method. Returns true if user OK'd, false if
  cancelled}
var
  DirName: string;      // name of directory
  Cancelled: Boolean;   // flags whether user cancelled
begin
  Assert(Assigned(Event), 'GetFolderFromEvent: Event is nil');
  Cancelled := False;
  // Normalise default directory: '' if doesn't exist
  DirName := MakeDirName(Folder);
  if not DirExists(DirName) then
    DirName := '';
  // Trigger event
  Event(Sender, DirName, Cancelled);
  // Record result and folder name if user OKd
  Result := not Cancelled;
  if Result then
    Folder := DirName;
end;

{ TCustomFileEdit }

procedure TCustomFileEdit.DoBrowse;
  {Override of inherited DoBrowse method: triggers OnBeforeBrowse event to
  enable file spec from edit control to be changed before getting user to edit
  the file spec. Then triggers OnAfterBrowse event to again enable file spec
  entered by user to be altered before displaying in edit control}
var
  FileSpec: string; // file spec
begin
  // Get current file spec stored in text
  FileSpec := Text;
  // Trigger event to update of file spec before passing to relevant dialog
  if Assigned(fOnBeforeBrowse) then
    fOnBeforeBrowse(Self, FileSpec);
  // Get new file spec from user in dialog box: cancel => no change
  fBrowseCancelled := not GetFileSpec(FileSpec);
  // Trigger event to update of file spec before setting control's text
  if Assigned(fOnAfterBrowse) then
    fOnAfterBrowse(Self, FileSpec);
  // Update text in control with result and trigger change event only if browse
  // wasn't cancelled
  if not fBrowseCancelled then
  begin
    Text := FileSpec;
    Change;
  end;
end;


{ TFileEdit }

function TFileEdit.GetFileSpec(var FileSpec: string): Boolean;
  {Gets a file spec from user using a standard file open/save dialog. The
  default file spec and entered file spec are passed using in FileSpec. True is
  returned if user OK's the entry and false if user cancels}
begin
  // If we have an open dialog use it to get file spec from user
  if Assigned(fOpenDialog) then
    Result := GetFileFromUser(fOpenDialog, FileSpec)
  else
    Result := False;
end;

procedure TFileEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Used to respond to removal of dialog box component referenced by our
  OpenDialog property: the property is nilled when dialog box component is
  removed}
begin
  inherited;
  if (Operation = opRemove) and (AComponent = fOpenDialog) then
    fOpenDialog := nil;
end;


{ TFolderEdit }

function TFolderEdit.GetFileSpec(var FileSpec: string): Boolean;
  {Gets a folder name by triggering OnGetFolder event. True is returned if user
  OK's the entry and false if user cancels}
begin
  if Assigned(fOnGetFolder) then
    Result := GetFolderFromEvent(Self, fOnGetFolder, FileSpec)
  else
    Result := False;
end;

{ TCustomFileComboBox }

procedure TCustomFileComboBox.DoAction;
  {Override of inherited DoAction method: triggers OnBeforeBrowse event to
  enable file spec from combo's edit box to be changed before getting user to
  edit the file spec. Then triggers OnAfterBrowse event to again enable file
  spec entered by user to be altered before displaying in combo's edit box.
  Before any event is triggered previously stored text is written back to combo box to
  replace the action text that has been placed there}
var
  FileSpec: string; // file specification we are editing
begin
  // Get previous file spec spec stored in text
  FileSpec := GetPrevText;
  Text := FileSpec;
  // Trigger event to update of file spec before passing to relevant dialog
  if Assigned(fOnBeforeBrowse) then
    fOnBeforeBrowse(Self, FileSpec);
  // Get new file spec from user in dialog box: cancel => no change
  fBrowseCancelled := not GetFileSpec(FileSpec);
  // Trigger event to update of file spec before setting controls text
  if Assigned(fOnAfterBrowse) then
    fOnAfterBrowse(Self, FileSpec);
  // Update text in control with result only if browse wasn't cancelled
  if not fBrowseCancelled then
  begin
    Text := FileSpec;
    Change;
  end;
end;


{ TFileComboBox }

function TFileComboBox.GetFileSpec(var FileSpec: string): Boolean;
  {Gets a file spec from user using a standard file open/save dialog. The
  default file spec and entered file spec are passed using in FileSpec. True is
  returned if user OK's the entry and false if user cancels}
begin
  if Assigned(fOpenDialog) then
    Result := GetFileFromUser(fOpenDialog, FileSpec)
  else
    Result := False;
end;

procedure TFileComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Used to respond to removal of dialog box component referenced by our
  OpenDialog property: the property is nilled when dialog box component is
  removed}
begin
  inherited;
  if (Operation = opRemove) and (AComponent = fOpenDialog) then
    fOpenDialog := nil;
end;


{ TFolderComboBox }

function TFolderComboBox.GetFileSpec(var FileSpec: string): Boolean;
  {Gets a folder name by triggering OnGetFolder event. True is returned if user
  OK's the entry and false if user cancels}
begin
  if Assigned(fOnGetFolder) then
    Result := GetFolderFromEvent(Self, fOnGetFolder, FileSpec)
  else
    Result := False;
end;

end.

