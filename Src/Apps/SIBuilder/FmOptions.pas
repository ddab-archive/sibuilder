{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmOptions.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialogue box that
                            allows user to edit current program options - this
                            inherits from the generic OK dialogue box and
                            displays OK, Cancel and Help buttons.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 03/09/2000
      @COMMENTS             + Added new option to decide whether graphic side
                              bar is displayed.
                            + Added info pane colour choice colour combo and
                              supporting code (including a colour choice dlg
                              box)
                            + Added choice of whether to display info pane
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 20/12/2000
      @COMMENTS             + Made fetch list of Delphi compilers installed on
                              system from reqistry rather than having fixed
                              list.
                            + Added option to set compiler window display style.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Redesigned dialog box, removing redundant options
                            and adding options for new features.
                            + Added combo box for report kind options.
                            + Added combo box for default compression options.
                            + Added checked list box to determine which popup
                              info windows are to be displayed
                            + Deleted compiler display combo
                            + Deleted show graphic sidebar display check box
                            + Changed text for welcome page option to reflect
                              new welcome tab sheet.
                            + Changed Delphi compiler version combo labelling
                              and contents to reflect fact we now have a built
                              in packager as well as a list of Delphi compilers.
                            + Deleted explicit cream info bar colour option:
                              no there is no longer a cream side panel and
                              therefore no need for this option.
                            + Rearranged controls and added labels and rulings
                              for "Build" and "Display" groupings.
                            + Fixed bug where, when custom colurs are being
                              used, closing dialog without reselecting colour
                              set the info pane colour to black.
                            + Made owner draw combo box look disabled when
                              Enabled is false.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 25/03/2007
      @COMMENTS             + Removed reference to UDCC32Info unit.
                            + Removed cmbCompiler combo box, referencing label
                              and all supporting code.
                            + Removed reference to TSettings.Compiler property.
                            + Removed inclusion of Registry.inc - not required.
                            + Added directives to inhibit compiler warnings.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of Registry.inc and Help.inc
                              include files with URegistry and UHelpContexts
                              units.
                            + Moved some string literals to resource string and
                              deleted redundant resource string.
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
 * The Original Code is FmOptions.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmOptions;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

interface


uses
  // Delphi
  StdCtrls, Dialogs, Windows, CheckLst, Controls, ExtCtrls, Graphics,
  // Project
  FmGenericOKDlg, USettings, Classes;

type

  {
  TOptionsDlg:
    Dialog box that allows user to edit current program options - this inherits
    from the generic OK dialogue box and displays OK, Cancel and Help buttons.
  }
  TOptionsDlg = class(TGenericOKDlg)
    chkShowWelcome: TCheckBox;
    lblColour: TLabel;
    cmbColour: TComboBox;
    dlgColour: TColorDialog;
    chkInfoPane: TCheckBox;
    lblReportKinds: TLabel;
    cbReportKinds: TComboBox;
    lblCompression: TLabel;
    cmbCompression: TComboBox;
    lblBuildOptions: TLabel;
    lblDisplayOptions: TLabel;
    bvlBuildOptions: TBevel;
    bvlDisplayOptions: TBevel;
    clbInfoWdws: TCheckListBox;
    lblInfoWdws: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbColourDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmbColourClick(Sender: TObject);
    procedure chkInfoPaneClick(Sender: TObject);
    procedure dlgColourShow(Sender: TObject);
  private // properties
    fSettings: TSettings;
    fDisplayChanged: Boolean;
    procedure SetSettings(const Value: TSettings);
  private
    fOldInfoPaneColour: TColor;
      {Records original info pane colour setting before any changes}
    fOldInfoPane: Boolean;
      {Records original info pane visibility before any changes}
    fOldInfoWdws: LongWord;
      {Records original state of info windows flag before any changes}
    fCustomColour: TColor;
      {Records custom colour value}
    procedure UpdateDisplay;
      {Updates dlg box contents to reflect current option settings}
    procedure UpdateState;
      {Updates state of controls in dlg box according to present settings}
    procedure SelectCompressor(const CompID: string);
      {Finds compressor in compression combo box that matches given compressor
      ID and highlights it}
  public
    property Settings: TSettings read fSettings write SetSettings;
      {Reference to object used to handle persistant settings}
    property DisplayChanged: Boolean read fDisplayChanged;
      {Property is true if any of the display change options have been updated}
  end;


implementation


uses
  // Project
  UHelpContexts, UGUIHelper, UCompressionMgr, URegistry;

{$R *.DFM}


resourcestring
  // Dialog box title
  sColourDlgTitle = 'Info Pane Custom Colour';
  // Popup info window names
  sGroupInfoWdw = 'Install groups';
  sFileInfoWdw = 'Install files';
  sRegKeyInfoWdw = 'Registry keys';
  sRegDataInfoWdw = 'Registry data';


{ Supporting routines and tables }

const
  // Predefined colours for info pane
  cColourList: array[0..1] of TColor = (clWindow, clBtnFace);

function ColourToIdx(Colour: TColor): Integer;
  {Looks up given colour in colour list and returns index if in list, else
  returns value one beyond last item in colour list}
var
  I: Integer; // loops thru colour table
begin
  Result := Length(cColourList);
  for I := Low(cColourList) to High(cColourList) do
    if cColourList[I] = Colour then
    begin
      Result := I;
      Break;
    end;
end;


{ TOptionsDlg }

procedure TOptionsDlg.cmbColourClick(Sender: TObject);
  {On click event handler for colour combo box - displays colour dlg box if
  custom is selected and returns user's choice}
begin
  inherited;
  if cmbColour.ItemIndex = Length(cColourList) then
  begin
    dlgColour.Color := fCustomColour;
    if dlgColour.Execute then
      fCustomColour := dlgColour.Color;
  end;
end;

procedure TOptionsDlg.cmbColourDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  {Draw item for colour combo box - draws box showing colours in in combo box}
var
  TextH: Integer;   // the height of the text in the dlg box
begin
  inherited;
  // Act on the control's canvas}
  with (Control as TComboBox).Canvas do
  begin
    TextH := TextHeight('X');
    // Set brush and pen according to if highlighted or not}
    if odSelected in State then
    begin
      // selected => highlighted
      Brush.Color := clHighlight;
      Pen.Color := clHighlightText;
    end
    else if not (Control as TComboBox).Enabled then
    begin
      // disabled => window colour but grey text and drawing
      Brush.Color := clWindow;
      Pen.Color := clGrayText;
      Font.Color := clGrayText;
    end
    else
    begin
      // not selected => window colour
      Brush.Color := clWindow;
      Pen.Color := clWindowText;
    end;
    // Clear rectangle
    FillRect(Rect);
    // Display the text}
    TextOut(Rect.Left + 28, Rect.Top + (Rect.Bottom - Rect.Top - TextH) div 2,
      (Control as TComboBox).Items[Index]);
    // Display appropriate colour rectangle
    if Index in [Low(cColourList)..High(cColourList)] then
    begin
      // These are predefined colours - draw box of colour
      Brush.Color := cColourList[Index];
      Rectangle(Rect.Left + 2, Rect.Top + 2,
        Rect.Left + 24, Rect.Bottom - 2);
    end
    else
    begin
      // This is custom colour - draw a chequered box
      Brush.Color := Pen.Color;
      Brush.Style := bsDiagCross;
      Rectangle(Rect.Left + 2, Rect.Top + 2,
        Rect.Left + 24, Rect.Bottom - 2);
      Brush.Style := bsSolid;
    end;
    // Draw invisible box - corrects bug which shows focus in one of box colours
    // if the last drawing is with a coloured brush
    Brush.Color := clBlack;
    Rectangle(Rect.Left, Rect.Top, Rect.Left, Rect.Top);
  end;
end;

procedure TOptionsDlg.chkInfoPaneClick(Sender: TObject);
  {Info pane check box click event handler - disables / enables colour combo
  as required}
begin
  UpdateState;
end;

procedure TOptionsDlg.dlgColourShow(Sender: TObject);
  {OnShow event handler for colour dialog box: sets required caption}
begin
  inherited;
  Windows.SetWindowText(dlgColour.Handle, PChar(sColourDlgTitle));
end;

procedure TOptionsDlg.FormCreate(Sender: TObject);
  {Form creation event - assign help contexts}
var
  CompMgr: TCompressionMgr; // object used to get supported compressors
begin
  inherited;
  // Set help contexts
  HelpContext := IDH_DLG_OPTIONS;
  dlgColour.HelpContext := IDH_DLG_OPTIONSCOLOUR;
  // Load possible compression options
  CompMgr := TCompressionMgr.Create;
  try
    CompMgr.GetIDs(cmbCompression.Items);
  finally
    CompMgr.Free;
  end;
  // Store info window options in list box
  with clbInfoWdws.Items do
  begin
    Clear;
    AddObject(sGroupInfoWdw, Pointer(cSIBGroupInfoWdw));
    AddObject(sFileInfoWdw, Pointer(cSIBFileInfoWdw));
    AddObject(sRegKeyInfoWdw, Pointer(cSIBRegKeyInfoWdw));
    AddObject(sRegDataInfoWdw, Pointer(cSIBRegDataInfoWdw));
  end;
end;

procedure TOptionsDlg.OKBtnClick(Sender: TObject);
  {OK button clicked - update properties from user entries}
var
  Idx: Integer;       // loops thru info window list box items
  InfoWdws: LongWord; // info windows bitmask
begin
  inherited;
  // Update settings object
  // build options
  Settings.DefaultCompressor :=
    cmbCompression.Items[cmbCompression.ItemIndex];
  Settings.ReportKind := cbReportKinds.ItemIndex;
  // display options
  Settings.ShowWelcome := chkShowWelcome.Checked;
  Settings.ShowInfoPane := chkInfoPane.Checked;
  if cmbColour.ItemIndex = Pred(cmbColour.Items.Count) then
    Settings.InfoPaneColour := fCustomColour
  else
    Settings.InfoPaneColour := cColourList[cmbColour.ItemIndex];
  // info window bitmask
  InfoWdws := 0;
  for Idx := 0 to Pred(clbInfoWdws.Items.Count) do
  begin
    if clbInfoWdws.Checked[Idx] then
      InfoWdws := InfoWdws or LongWord(clbInfoWdws.Items.Objects[Idx]);
  end;
  Settings.InfoWindows := InfoWdws;
  // Flag if any of options that affect current display have been changed
  fDisplayChanged := (Settings.InfoPaneColour <> fOldInfoPaneColour)
    or (Settings.ShowInfoPane <> fOldInfoPane);
end;

procedure TOptionsDlg.SelectCompressor(const CompID: string);
  {Finds compressor in compression combo box that matches given compressor ID
  and highlights it}
var
  ItemIndex: Integer;   // ItemIndex to select in combo box
begin
  // Find matching compressor in combobox:
  ItemIndex := cmbCompression.Items.IndexOf(CompID);
  Assert(ItemIndex > -1);
  cmbCompression.ItemIndex := ItemIndex;
end;

procedure TOptionsDlg.SetSettings(const Value: TSettings);
  {Write access method of Settings property, records reference and updates dlg
  box contents to reflect current settings}
begin
  // Record settings, inc present values of those we're monitoring
  fSettings := Value;
  fOldInfoPane := Value.ShowInfoPane;
  fOldInfoPaneColour := Value.InfoPaneColour;
  fOldInfoWdws := Value.InfoWindows;
  // Record current info pane colour as custom colour
  fCustomColour := Value.InfoPaneColour;
  // Update controls to reflect settings
  UpdateDisplay;
end;

procedure TOptionsDlg.UpdateDisplay;
  {Updates dlg box contents to reflect current option settings}
var
  Idx: Integer; // index into info window list box
begin
  SelectCompressor(Settings.DefaultCompressor);
  chkShowWelcome.Checked := Settings.ShowWelcome;
  chkInfoPane.Checked := Settings.ShowInfoPane;
  cmbColour.ItemIndex := ColourToIdx(Settings.InfoPaneColour);
  cbReportKinds.ItemIndex := Settings.ReportKind;
  for Idx := 0 to Pred(clbInfoWdws.Items.Count) do
    clbInfoWdws.Checked[Idx] :=
      LongWord(clbInfoWdws.Items.Objects[Idx]) and fOldInfoWdws <> 0;
  UpdateState;
end;

procedure TOptionsDlg.UpdateState;
  {Updates state of controls in dlg box according to present settings}
begin
  EnableCtrls([cmbColour, lblColour], chkInfoPane.Checked);
end;

end.
