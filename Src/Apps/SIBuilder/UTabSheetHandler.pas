{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UTabSheetHandler.pas
  @COMMENTS                 Helper classs used by main form to navigate and
                            update the various tab sheets of the page control.
                            Encapsulates the functionality of the various tab
                            sheets of the page control in TTabSheetHandler,
                            which acts as a base class for further classes that
                            deal with the specific functionality of individual
                            tab sheets. TTabSheetHandler also ensures the info
                            pane control is kept up to date with the correct
                            details for the current tab sheet. TTabSheetHandler
                            objects are stored in a list maintained by
                            TTabSheetHandlerList, which permits navigation among
                            the tab sheets.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added methods to help with finding pages tabsheet
                              pages
                            + Changed the FirstSheet, IsFirstSheet & IsLastSheet
                              page methods to ignore disabled sheets and gave
                              FirstSheet a Force parameter to enabled it to
                              enable CanTurnPage to be ignored.
                            + Added overloaded GotoSheet methods to access a tab
                              sheet directly
                            + Moved handling of previous and next buttons from
                              main form to tab sheet list handler and added
                              handling of new selection buttons control
                            + Added new Enabled property to tabsheets and
                              associated processing to hide/show linked controls
                            + Changed name of tab list handler's Update method
                              to UpdateSheets.
                            + Added UpdateProject method to tab list handler
                              that calls method of same name in tab sheets.
                              UpdateProject was added as a do-nothing virtual
                              method in tab sheet handler base class. A unique
                              method of this name had been introduced in each
                              sub class that needed to call it on exiting pages.
                              Changed complementary UpdateSheet method to be
                              virtual do nothing, not abstract
                            + Added code to previously empty Enter method that
                              calls UpdateSheet and code to previously empty
                              Leave method to call UpdateProject
                            + Changed to use tagged rtf code for info panes: no
                              longer need to explicitly set hot links. These are
                              included in rich text file.
                            + Updated TTabSheetHandler.CanTurnPage to return a
                              help context as well as a more details reason when
                              page can't be turned.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/11/2003
      @COMMENTS             + Changed info pane hot text handling code to use
                              new hot text controls rather than active RTF
                              classes.
                            + Removed reference to removed UActiveRTFWrapper
                              unit
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
 * The Original Code is UTabSheetHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UTabSheetHandler;


interface


uses
  // Delphi
  SysUtils, Windows, ComCtrls, Forms,
  // Project
  UObjList, UProject;

type

  {
  ETabSheetHandler:
    Exceptions raised by tab sheet handlers.

    Inheritance: ETabSheetHandler -> [Exception]
  }
  ETabSheetHandler = class(Exception);

  {
  TTabSheetHandlerClass:
    Class reference used to create instances of TTabSheetHandler derived
    classes.
  }
  TTabSheetHandlerClass = class of TTabSheetHandler;

  {
  TTabSheetHandler:
    Helper class used by main form to encapsulate the functionality of the
    various tab sheets of the page control. This acts as a base class for
    further classes that deal with the specific functionality of individual tab
    sheets. The class also ensures the info pane control is kept up to date with
    the correct details for the current tab sheet. TTabSheetHandler objects are
    stored in a list maintained by TTabSheetHandlerList below.

    Inheritance: TTabSheetHandler -> TObjListItem -> [TObject]
  }
  TTabSheetHandler = class(TObjListItem)
  private // properties
    fTabSheet: TTabSheet;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    function GetProject: TProject;
      {Returns reference to project whose properties are associated with this
      tab sheet}
    function InstructionsResId: Integer; virtual; abstract;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  protected // friend methods called by TTabSheetHandlerList
    procedure Enter; virtual;
      {Method called whenever a tab sheet is about to be displayed. Default
      action is to call UpdateSheet}
    procedure Leave; virtual;
      {Method called whenever a tab sheet is about to be left. Default action is
      to call UpdateProject}
    procedure DisplayIntructions;
      {Displays the hot text instruction relating to the tab sheet}
    procedure UpdateProject; virtual;
      {Method called to update project with details entered on a page. Default
      is to do nothing, but this can be overridden by descendants}
  public
    constructor Create(const TabSheet: TTabSheet); virtual;
      {Class constructor: records reference to related tab sheet control}
    procedure UpdateSheet; virtual;
      {Method called to update tab sheet with details from project. Default is
      to do nothing}
    function CanTurnPage(out Reason: string;
      out AHelpCtx: Integer): Boolean; virtual;
      {Returns true if the page can be turned (ie we can move to another page),
      false if not. This method simply returns true and should be overridden if
      this behaviour is not what is needed. If false is returned a message must
      be returned via the Reason parameter explaining why the page cannot be
      turned and a help context explaining the error (or 0 of no help available)
      returned via the AHelpCtx parameter}
    function IsCurrent: Boolean; virtual;
      {Returns true if the associated tab sheet is currently displayed}
    property TabSheet: TTabSheet read fTabSheet;
      {The tabsheet control associated with this object}
    property Enabled: Boolean read GetEnabled write SetEnabled;
      {Enables / disables the tab sheet and causes tab sheet list to update GUI
      accordingly}
  end;

  {
  TTabSheetHandlerList:
    This class maintains a list of TTabSheetHandler objects and allows easy
    navigation among the various tab sheets, ensuring that each sheet is
    correctly initialised before it deisplays.

    Inheritance: TTabSheetHandlerList -> TObjList -> [TObject]
  }
  TTabSheetHandlerList = class(TObjList)
  private // properties
    function GetItem(I: Integer): TTabSheetHandler;
    function GetCurrentSheet: TTabSheetHandler;
  private
    fCurrentSheetIdx: Integer;
      {Index of the currently active tabsheet within this list}
    function CanTurnCurrentPage: Boolean;
      {Returns true if the current page can be turned - ie we can move to
      another page}
    procedure ChangeSheet(const ToIdx: Integer);
      {Changes from current sheet one to the one given by idx. Causes any
      special processing required when leaving and entering sheets to be carried
      out}
    function GetFirstEnabledSheetIdx: Integer;
      {Returns index of first enabled tab sheet in list}
    function GetLastEnabledSheetIdx: Integer;
      {Returns index of last enabled tab sheet in list}
    function GetNextEnabledSheetIdx(Current: Integer): Integer;
      {Returns index of next enabled tab sheet after given one, or -1 if there
      are no more}
    function GetPrevEnabledSheetIdx(Current: Integer): Integer;
      {Returns index of previous enabled tab sheet before given one, or -1 if
      there are no more}
    function IndexOfCaption(const Caption: string): Integer;
      {Returns the index of the tab sheet with the given caption, or -1 if there
      is no such caption}
    procedure SelectorClick(Sender: TObject);
      {OnClick event handler for tab selector control: selects the sheet
      associated with the button clicked if possible}
    procedure NavButtonClick(Sender: TObject);
      {OnClick event handler for navigation button: moves to next or previous
      page as required where possible}
    procedure UpdateButtonList;
      {Updates selector button list to ensure that button selected matches
      current tab sheet, or no button is selected if there is no current tab
      sheet}
  protected // "friend" fields / methods accessed by TTabSheetHandler
    fProject: TProject;
      {Reference to the SIBuilder project whose values are displayed in the
      tabsheets}
    procedure EnabledChanged(ChangingSheet: TTabSheetHandler);
      {Called when enabled state of a tab sheet is changed. Reference to
      associated tab sheet handler is passed to method. Updates selection button
      list to display only enabled sheets. Changes currently selected sheet if
      it is disabled}
  public
    constructor Create(Project: TProject); reintroduce;
      {Class constructor: sets up list of TTabSheetHandlers and records
      reference to project}
    function AppendTabSheet(SheetClass: TTabSheetHandlerClass;
      TabSheet: TTabSheet): TTabSheetHandler;
      {Creates a new tab sheet of given type referencing given tabsheet and
      returns a reference to it}
    procedure UpdateSheets;
      {Updates all controls on all tab sheets in list from project properties}
    function FirstSheet(const Force: Boolean = False): TTabSheetHandler;
      {Returns first enabled tabsheet in list and makes it current. If current
      tab sheet can't be "turned" (and Force is false) then no action is taken
      and the curent sheet is returned}
    function NextSheet: TTabSheetHandler;
      {Returns next enabled tabsheet in list and makes it current. If current
      tab sheet can't be "turned" then no action is taken and the curent sheet
      is returned}
    function PreviousSheet: TTabSheetHandler;
      {Returns previous enabled tabsheet in list and makes it current. If
      current tab sheet can't be "turned" then no action is taken and the
      current sheet is returned}
    procedure GotoSheet(const Sheet: TTabSheetHandler;
      Force: Boolean = False); overload;
      {Selects the tab sheet associated with the given handler. If Force is true
      the sheet is always selected, but is only selected if current page can be
      turned when Force is false}
    procedure GotoSheet(const Caption: string;
      Force: Boolean = False); overload;
      {Selects the tab sheet with the given caption (as displayed in selector
      button list. If Force is true the sheet is always selected, but is only
      selected if current page can be turned when Force is false}
    function IsLastSheet: Boolean;
      {Returns true if the current tab sheet is the last enabled sheet in the
      list}
    function IsFirstSheet: Boolean;
      {Returns true if the current tab sheet is the first enabled sheet in the
      list}
    procedure UpdateControls;
      {Updates navigation controls according to which sheet we're on: next,
      previous and button list are updated}
    procedure UpdateProject;
      {Updates project to reflect values in controls in all tab sheets}
    property Items[I: Integer]: TTabSheetHandler read GetItem; default;
      {The items in the list}
    property CurrentSheet: TTabSheetHandler read GetCurrentSheet;
      {The currently active tabsheet - nil if no sheet is currently active}
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UCmdDispatcher, FmMain;

resourcestring
  // Error messages
  sCantTurnToGivenPage  = 'Can''t turn to %0:s page. '#10#10'%1:s';
  sCantTurnOverPage     = 'Can''t turn page.'#10#10'%0:s';

{ TTabSheetHandler }

function TTabSheetHandler.CanTurnPage(out Reason: string;
  out AHelpCtx: Integer): Boolean;
  {Returns true if the page can be turned (ie we can move to another page),
  false if not. This method simply returns true and should be overridden if this
  behaviour is not what is needed. If false is returned a message must be
  returned via the Reason parameter explaining why the page cannot be turned and
  a help context explaining the error (or 0 of no help available) returned via
  the AHelpCtx parameter}
begin
  Result := True;
  AHelpCtx := 0;
end;

constructor TTabSheetHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: records reference to related tab sheet control}
begin
  inherited Create;
  fTabSheet := TabSheet;
end;

procedure TTabSheetHandler.DisplayIntructions;
  {Displays the hot text instruction relating to the tab sheet}
begin
  MainForm.htInfo.LoadFromResourceId(
    HInstance, InstructionsResId, RT_RCDATA
  );
end;

procedure TTabSheetHandler.Enter;
  {Method called whenever a tab sheet is about to be displayed. Default action
  is to call UpdateSheet}
begin
  UpdateSheet;
end;

function TTabSheetHandler.GetEnabled: Boolean;
  {Read access method for Enabled property: returns value of underlying sheet's
  Enabled property}
begin
  Result := TabSheet.Enabled;
end;

function TTabSheetHandler.GetProject: TProject;
  {Returns reference to project whose properties are associated with this tab
  sheet}
begin
  Assert((List <> nil) and (List is TTabSheetHandlerList));
  Result := (List as TTabSheetHandlerList).fProject;  // accesses field of list
end;

function TTabSheetHandler.IsCurrent: Boolean;
  {Returns true if the associated tab sheet is currently displayed}
begin
  Result := ((List as TTabSheetHandlerList).GetCurrentSheet = Self);
end;

procedure TTabSheetHandler.Leave;
  {Method called whenever a tab sheet is about to be left. Default action is to
  call UpdateProject}
begin
  UpdateProject;
end;

procedure TTabSheetHandler.SetEnabled(const Value: Boolean);
  {Write access method for Enabled property: sets underlying tabsheet's enabled
  property and notify owning list that enabled state of one of sheets has
  changed}
begin
  if Value <> TabSheet.Enabled then
  begin
    TabSheet.Enabled := Value;
    (List as TTabSheetHandlerList).EnabledChanged(Self);
  end;
end;

procedure TTabSheetHandler.UpdateProject;
  {Method called to update project with details entered on a page. Default is to
  do nothing, but this can be overridden by descendants}
begin
  {Do nothing};
end;

procedure TTabSheetHandler.UpdateSheet;
  {Method called to update tab sheet with details from project. Default is to do
  nothing}
begin
  {Do nothing};
end;


{ TTabSheetHandlerList }

function TTabSheetHandlerList.AppendTabSheet(SheetClass: TTabSheetHandlerClass;
  TabSheet: TTabSheet): TTabSheetHandler;
  {Creates a new tab sheet of given type referencing given tabsheet and returns
  a reference to it}
begin
  // Create tab sheet and add to list
  Result := SheetClass.Create(TabSheet);
  Add(Result);
  // Add to selector button list
  MainForm.blTabSelector.Items.AddObject(TabSheet.Caption, Result);
end;

function TTabSheetHandlerList.CanTurnCurrentPage: Boolean;
  {Returns true if the current page can be turned - ie we can move to another
  page}
var
  Dummy: string;      // string to hold reason why page can't be turned - unused
  DummyCtx: Integer;  // int to hold help context relating to error - unused
begin
  if fCurrentSheetIdx > -1 then
    Result := GetCurrentSheet.CanTurnPage(Dummy, DummyCtx)
  else
    Result := True;
end;

procedure TTabSheetHandlerList.ChangeSheet(const ToIdx: Integer);
  {Changes from current sheet one to the one given by idx. Causes any special
  processing required when leaving and entering sheets to be carried out}
begin
  Assert(ToIdx <> -1);
  // Get sheet we're leaving to process any special exit coding
  if fCurrentSheetIdx <> -1 then
    GetItem(fCurrentSheetIdx).Leave;
  // Get sheet we're entering to process any special entry coding
  GetItem(ToIdx).Enter;
  // Make sheet we're entering current and display it
  fCurrentSheetIdx := ToIdx;
  GetCurrentSheet.TabSheet.PageControl.ActivePage := GetCurrentSheet.TabSheet;
  GetCurrentSheet.UpdateSheet;
  GetCurrentSheet.DisplayIntructions;
  // Update controls on main form
  UpdateControls;
end;

constructor TTabSheetHandlerList.Create(Project: TProject);
  {Class constructor: sets up list of TTabSheetHandlers and records reference to
  project}
begin
  inherited CreateForClass(nil, TTabSheetHandler);
  // Ensure no sheet selected
  fCurrentSheetIdx := -1;
  // Get reference to project
  fProject := Project;
  // Hook up event handlers
  MainForm.blTabSelector.OnClick := SelectorClick;
  MainForm.btnNext.OnClick := NavButtonClick;
  MainForm.btnPrev.OnClick := NavButtonClick;
  MainForm.htInfo.OnLinkClick := TCmdDispatcher.HotLinkClickHandler;
end;

procedure TTabSheetHandlerList.EnabledChanged(ChangingSheet: TTabSheetHandler);
  {Called when enabled state of a tab sheet is changed. Reference to associated
  tab sheet handler is passed to method. Updates selection button list to
  display only enabled sheets. Changes currently selected sheet if it is
  disabled}
var
  SelSheet: TTabSheetHandler; // handler of currently selected tab sheet
  SelIdx: Integer;            // index of button assoc with tab sheet
  ChangingSheetIdx: Integer;  // index of changing sheet in list
begin
  // Get current tab sheet handler
  SelSheet := GetCurrentSheet;
  if ChangingSheet.Enabled then
  begin
    // Sheet that's changing is now enabled
    // add required button to button list
    ChangingSheetIdx := IndexOf(ChangingSheet);
    MainForm.blTabSelector.Items.InsertObject(
      ChangingSheetIdx,
      ChangingSheet.TabSheet.Caption,
      ChangingSheet
    );
    // now re-select current sheet in button list
    SelIdx := MainForm.blTabSelector.Items.IndexOfObject(SelSheet);
    MainForm.blTabSelector.ItemIndex := SelIdx;
  end
  else
  begin
    // Sheet that's changing is now disabled
    // get index of button of changing sheet in button list
    ChangingSheetIdx :=
      MainForm.blTabSelector.Items.IndexOfObject(ChangingSheet);
    if ChangingSheet <> SelSheet then
    begin
      // changing sheet is not selected: delete it and reselect current sheet
      MainForm.blTabSelector.Items.Delete(ChangingSheetIdx);
      SelIdx := MainForm.blTabSelector.Items.IndexOfObject(SelSheet);
      MainForm.blTabSelector.ItemIndex := SelIdx;
    end
    else
    begin
      // changing sheet is selected: delete it and select another sheet
      MainForm.blTabSelector.Items.Delete(ChangingSheetIdx);
      SelIdx := ChangingSheetIdx;
      if SelIdx = MainForm.blTabSelector.Items.Count then
        Dec(SelIdx);
      ChangeSheet(SelIdx);
    end;
  end;
  // Finally, update controls
  UpdateControls;
end;

function TTabSheetHandlerList.FirstSheet(
  const Force: Boolean = False): TTabSheetHandler;
  {Returns first enabled tabsheet in list and makes it current. If current tab
  sheet can't be "turned" then no action is taken and the curent sheet is
  returned}
begin
  // Check if we can turn page
  if Force or CanTurnCurrentPage then
    // Go to first enabled sheet, if we have some sheets!
    if Count > 0 then
      ChangeSheet(GetFirstEnabledSheetIdx)
    else
      fCurrentSheetIdx := -1;
  // Return reference to new current sheet
  Result := GetCurrentSheet;
end;

function TTabSheetHandlerList.GetCurrentSheet: TTabSheetHandler;
  {Read access method for CurrentSheet property - returns reference to current
  sheet or nil if there is no current sheet}
begin
  if fCurrentSheetIdx <> -1 then
    Result := GetItem(fCurrentSheetIdx)
  else
    Result := nil;
end;

function TTabSheetHandlerList.GetFirstEnabledSheetIdx: Integer;
  {Returns index of first enabled tab sheet in list}
begin
  Result := GetNextEnabledSheetIdx(-1);
end;

function TTabSheetHandlerList.GetItem(I: Integer): TTabSheetHandler;
  {Read access method for Items property}
begin
  Result := inherited GetObject(I) as TTabSheetHandler;
end;

function TTabSheetHandlerList.GetLastEnabledSheetIdx: Integer;
  {Returns index of last enabled tab sheet in list}
begin
  Result := GetPrevEnabledSheetIdx(Count);
end;

function TTabSheetHandlerList.GetNextEnabledSheetIdx(Current: Integer): Integer;
  {Returns index of next enabled tab sheet after given one, or -1 if there are
  no more}
begin
  // Move 1 sheet along from current
  Result := Current + 1;
  // Now skip over any disabled sheets
  while (Result < Count) and not GetItem(Result).TabSheet.Enabled do
    Inc(Result);
  if Result >= Count then
    // We're already at last enabled sheet already: can't move so return -1
    Result := -1;
end;

function TTabSheetHandlerList.GetPrevEnabledSheetIdx(Current: Integer): Integer;
  {Returns index of previous enabled tab sheet before given one, or -1 if there
  are no more}
begin
  // Move 1 sheet back from current
  Result := Current - 1;
  // Now skip over any disabled sheets
  while (Result >= 0) and not GetItem(Result).TabSheet.Enabled do
    Dec(Result);
  if Result < 0 then
    // We're already at first enabled sheet already: can't move so return -1
    Result := -1;
end;

procedure TTabSheetHandlerList.GotoSheet(const Sheet: TTabSheetHandler;
  Force: Boolean);
  {Selects the tab sheet associated with the given handler. If Force is true the
  sheet is always selected, but is only selected if current page can be turned
  when Force is false}
var
  SheetIdx: Integer;  // index of required sheet in list
  ErrStr: string;     // error from turn page test
  HelpCtx: Integer;   // help context associated with turn page test error
begin
  Assert(Sheet <> nil);
  // Check if we can turn page
  if Assigned(CurrentSheet)
    and not CurrentSheet.CanTurnPage(ErrStr, HelpCtx)
    and not Force then
  begin
    // Can't turn page: re-select current button and raise exception
    UpdateButtonList;
    raise ETabSheetHandler.CreateFmtHelp(
      sCantTurnToGivenPage, [Sheet.TabSheet.Caption, ErrStr], HelpCtx
    );
  end;
  // Get new sheet index
  SheetIdx := IndexOf(Sheet);
  Assert(SheetIdx > -1);
  // Change sheet if it is different to current one
  if SheetIdx <> fCurrentSheetIdx then
    ChangeSheet(SheetIdx);
end;

procedure TTabSheetHandlerList.GotoSheet(const Caption: string; Force: Boolean);
  {Selects the tab sheet with the given caption (as displayed in selector button
  list. If Force is true the sheet is always selected, but is only selected if
  current page can be turned when Force is false}
var
  SheetIdx: Integer;  // index of required sheet in list
  ErrStr: string;     // error from turn page test
  HelpCtx: Integer;   // help context associated with turn page test error
begin
  // Get new sheet index from caption
  SheetIdx := IndexOfCaption(Caption);
  Assert(SheetIdx > -1);
  // Check if we can turn page
  if Assigned(CurrentSheet)
    and not CurrentSheet.CanTurnPage(ErrStr, HelpCtx)
    and not Force then
  begin
    // Can't turn page: re-select current button and raise exception
    UpdateButtonList;
    raise ETabSheetHandler.CreateFmtHelp(
      sCantTurnToGivenPage, [Caption, ErrStr], HelpCtx
    );
  end;
  // Change sheet if it is different to current one
  if SheetIdx <> fCurrentSheetIdx then
    ChangeSheet(SheetIdx);
end;

function TTabSheetHandlerList.IndexOfCaption(
  const Caption: string): Integer;
  {Returns the index of the tab sheet with the given caption, or -1 if there is
  no such caption}
var
  Idx: Integer; // index into list of tab handlers
begin
  Result := -1;
  for Idx := 0 to Pred(Count) do
  begin
    if AnsiCompareText(GetItem(Idx).TabSheet.Caption, Caption) = 0 then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

function TTabSheetHandlerList.IsFirstSheet: Boolean;
  {Returns true if the current tab sheet is the first enabled sheet in the list}
begin
  Result := fCurrentSheetIdx = GetFirstEnabledSheetIdx;
end;

function TTabSheetHandlerList.IsLastSheet: Boolean;
  {Returns true if the current tab sheet is the last enabled sheet in the list}
begin
  Result := fCurrentSheetIdx = GetLastEnabledSheetIdx;
end;

procedure TTabSheetHandlerList.NavButtonClick(Sender: TObject);
  {OnClick event handler for navigation button: moves to next or previous page
  as required where possible}
var
  ErrStr: string;     // error message if can't move from current tab sheet
  HelpCtx: Integer;   // help context associated with turn page test error
begin
  // Check if we can turn page
  if not CurrentSheet.CanTurnPage(ErrStr, HelpCtx) then
    raise ETabSheetHandler.CreateFmtHelp(
      sCantTurnOverPage, [ErrStr], HelpCtx
    );
  // We can turn page: act according to button clicked
  if Sender = MainForm.btnNext then
    NextSheet
  else if Sender = MainForm.btnPrev then
    PreviousSheet;
end;

function TTabSheetHandlerList.NextSheet: TTabSheetHandler;
  {Returns next enabled tabsheet in list and makes it current. If current tab
  sheet can't be "turned" (and Force is false) then no action is taken and the
  curent sheet is returned}
var
  NewSheetIdx: Integer;   // index of next tab sheet
begin
  // Check if we can turn page
  if CanTurnCurrentPage then
  begin
    // Move to next sheet if there is one
    NewSheetIdx := GetNextEnabledSheetIdx(fCurrentSheetIdx);
    if NewSheetIdx > -1 then
      ChangeSheet(NewSheetIdx);
  end;
  // Return reference to (any) "new" current sheet
  Result := GetCurrentSheet;
end;

function TTabSheetHandlerList.PreviousSheet: TTabSheetHandler;
  {Returns previous enabled tabsheet in list and makes it current. If current
  tab sheet can't be "turned" then no action is taken and the current sheet is
  returned}
var
  NewSheetIdx: Integer;   // index of previous tab sheet
begin
  // Check if we can turn page
  if CanTurnCurrentPage then
  begin
    // Move to previous sheet if there is one
    NewSheetIdx := GetPrevEnabledSheetIdx(fCurrentSheetIdx);
    if NewSheetIdx > -1 then
      ChangeSheet(NewSheetIdx);
  end;
  // Return reference to (any) "new" current sheet
  Result := GetCurrentSheet;
end;

procedure TTabSheetHandlerList.SelectorClick(Sender: TObject);
  {OnClick event handler for tab selector control: selects the sheet associated
  with the button clicked if possible}
var
  Item: Integer;  // index of clicked selector button
begin
  Item := MainForm.blTabSelector.ItemIndex;
  Assert(Item > -1);
  GotoSheet(MainForm.blTabSelector.Items.Objects[Item] as TTabSheetHandler);
end;

procedure TTabSheetHandlerList.UpdateButtonList;
  {Updates selector button list to ensure that button selected matches current
  tab sheet, or no button is selected if there is no current tab sheet}
begin
  if fCurrentSheetIdx > -1 then
    MainForm.blTabSelector.ItemIndex :=
      MainForm.blTabSelector.Items.IndexOf(CurrentSheet.TabSheet.Caption)
  else
    MainForm.blTabSelector.ItemIndex := -1;
end;

procedure TTabSheetHandlerList.UpdateControls;
  {Updates navigation controls according to which sheet we're on: next, previous
  and button list are updated}
begin
  // Only do anything if we have a current sheet
  if GetCurrentSheet <> nil then
  begin
    // Select required item in button list
    MainForm.blTabSelector.ItemIndex :=
      MainForm.blTabSelector.Items.IndexOf(GetCurrentSheet.TabSheet.Caption);
    // Update state of next/preivous buttons
    MainForm.btnNext.Enabled := not IsLastSheet;
    MainForm.btnPrev.Enabled := not IsFirstSheet;
  end;
end;

procedure TTabSheetHandlerList.UpdateProject;
  {Updates project to reflect values in controls in all tab sheets}
var
  Idx: Integer; // loops thru list of sheet handlers
begin
  for Idx := 0 to Pred(Count) do
    GetItem(Idx).UpdateProject;
end;

procedure TTabSheetHandlerList.UpdateSheets;
  {Updates all controls on all tab sheets in list from project properties}
var
  Idx: Integer; // loops thru list of sheet handlers
begin
  for Idx := 0 to Pred(Count) do
    GetItem(Idx).UpdateSheet;
end;

end.
