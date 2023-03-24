{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UOSTabHandler.pas
  @COMMENTS                 TTabSheetHandler descendant that provides the
                            required additional functionality for the "Target
                            OS" tab sheet.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
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
 * The Original Code is UOSTabHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UOSTabHandler;


interface

uses
  // Delphi
  ComCtrls,
  // Project
  UTabSheetHandler, UOSTreeViewMgr;

type

  {
  TOSTabHandler:
    Encapsulates the "target OS" tab sheet and provides the relevant specialised
    processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TOSTabHandler -> TTabSheetHandler -> TObjListItem -> [TObject]
  }
  TOSTabHandler = class(TTabSheetHandler)
  private
    fTVMgr: TOSTreeViewMgr;
    procedure AllOSsClick(Sender: TObject);
      {OnClick event handler for All OSs button: selects all operating systems
      in tree view and updates description}
    procedure TVChange(Sender: TObject);
      {OnChange event handler for tree view manager: updates description of
      selected OSs}
    procedure UpdateDescription;
      {Updates the text describing the currently selected operating systems}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
    procedure UpdateProject; override;
      {Updates project to reflect values entered in tab sheet}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: creates instance of object used to manage the tree
      view control and sets the required event handlers}
    destructor Destroy; override;
      {Class destructor: nils the event handlers managed by this handler and
      frees the owned OS tree view manager}
    procedure UpdateSheet; override;
      {Updates controls on page to values per current project}
    function CanTurnPage(out Reason: string;
      out AHelpCtx: Integer): Boolean; override;
      {Returns true if the page can be turned (ie we can move to another page),
      false if not. Method checks validity of values of tab sheet's controls and
      returns any reason why the page can't be turned and an associated help
      context or 0 if none) via the out parameters}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  FmMain, UResources, UOS;


resourcestring
  // Description displayed when no OSs are specified
  sNoOS = '*** No OS Specified ***';


{ TOSTabHandler }

procedure TOSTabHandler.AllOSsClick(Sender: TObject);
  {OnClick event handler for All OSs button: selects all operating systems in
  tree view and updates description}
begin
  fTVMgr.OSOptions := cAnyOS;
  UpdateDescription;
end;

function TOSTabHandler.CanTurnPage(out Reason: string;
  out AHelpCtx: Integer): Boolean;
  {Returns true if the page can be turned (ie we can move to another page),
  false if not. Method checks validity of values of tab sheet's controls and
  returns any reason why the page can't be turned and an associated help context
  (or 0 if none) via the out parameters}
var
  ErrStr: string; // error string returned from validator
begin
  // Get tree view manager to validate OS
  Result := fTVMgr.Validate(ErrStr);
  if not Result then
  begin
    // Invalid: set reason and help context
    Reason := ErrStr;
    AHelpCtx := 0;  // no help available
  end;
end;

constructor TOSTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: creates instance of object used to manage the tree view
  control and sets the required event handlers}
begin
  inherited;
  fTVMgr := TOSTreeViewMgr.Create(MainForm.tvOSs, MainForm.ilChecks);
  fTVMgr.OnChange := TVChange;
  MainForm.btnAllOSs.OnClick := AllOSsClick;
end;

destructor TOSTabHandler.Destroy;
  {Class destructor: nils the event handlers managed by this handler and frees
  the owned OS tree view manager}
begin
  MainForm.btnAllOSs.OnClick := nil;
  fTVMgr.OnChange := nil;
  fTVMgr.Free;
  inherited;
end;

function TOSTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cTargetOSInfoResID;
end;

procedure TOSTabHandler.TVChange(Sender: TObject);
  {OnChange event handler for tree view manager: updates description of selected
  OSs}
begin
  UpdateDescription;
end;

procedure TOSTabHandler.UpdateDescription;
  {Updates the text describing the currently selected operating systems}
begin
  if fTVMgr.Validate then
    MainForm.lblOSOptions.Caption := UOS.DescribeOSs(fTVMgr.OSOptions)
  else
    MainForm.lblOSOptions.Caption := sNoOS;
end;

procedure TOSTabHandler.UpdateProject;
  {Updates project to reflect values entered in tab sheet}
begin
  GetProject.SupportedOSs := fTVMgr.OSOptions;
end;

procedure TOSTabHandler.UpdateSheet;
  {Updates controls on page to values per current project}
begin
  // Update tree view with supported OSs
  fTVMgr.OSOptions := GetProject.SupportedOSs;
  // Update displayed OS description
  UpdateDescription;
end;

end.
