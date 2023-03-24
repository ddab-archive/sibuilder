{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpBrowseEdit.pas
  @COMMENTS                 Defines a base class for components that incorporate
                            a browse button within an edit control. Components
                            derived from this base class are used by SIBuilder.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2003
      @COMMENTS             Added new TBrowseEdit control - simply publishes
                            properties of TCustomBrowseEdit control.
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
 * The Original Code is CmpBrowseEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpBrowseEdit;


interface


uses
  // Delphi
  Windows, StdCtrls, Graphics, Buttons, Classes, Controls, Messages;

type

  {
  TCustomBrowseEdit:
    Edit control derivative class that includes a speed button aligned to the
    right edge of the edit box. An event is triggered when the button is
    clicked. All properties are protected and need to be published by descendant
    classes.

    Inheritance: TCustomBrowseEdit -> [TEdit]
  }
  TCustomBrowseEdit = class(TEdit)
  private // properties
    fOnBrowse: TNotifyEvent;
    procedure SetButtonWidth(const Value: Integer);
    function GetButtonWidth: Integer;
    function GetButtonGlyph: TBitmap;
    procedure SetButtonGlyph(const Value: TBitmap);
    function GetButtonText: string;
    procedure SetButtonText(const Value: string);
    function GetButtonCursor: TCursor;
    procedure SetButtonCursor(const Value: TCursor);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
  private
    fBrowseBtn: TSpeedButton;
      {Reference to privately owned speedbutton that is displayed in edit box}
  protected
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
      {Intercepts font change events: font changes cause edit width to be reset
      - so we reset back again!}
    procedure CreateWnd; override;
      {Overridden method called when window is created. Ensures that button is
      fitted to control (this method is called when BorderStyle property is
      changed so we adjust button's alignment). Also sets edit control's margin
      so that edit text doesn't overwrite button}
    procedure BrowseBtnClick(Sender: TObject);
      {OnClick event handler for browse button: calls DoBrowse method to perform
      required action}
    procedure DoBrowse; dynamic;
      {Triggers OnBrowse event}
    procedure AlignButton;
      {Aligns the browse button to the right hand internal edge of the edit
      control and sets button's height to fill the the edit control}
    procedure SetEditMargin;
      {Sets edit control's right margin so that edit control's text area doesn't
      overwrite the button at right edge of control}
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth
      default 21;
      {Determines width of browse button in the edit control}
    property ButtonGlyph: TBitmap read GetButtonGlyph write SetButtonGlyph;
      {The glyph that is displayed in the browse button}
    property ButtonText: string read GetButtonText write SetButtonText;
      {Text displayed in browse button}
    property ButtonCursor: TCursor read GetButtonCursor write SetButtonCursor
      default crArrow;
      {Cursor displayed when mouse is over the browse button}
    property ButtonHint: string read GetButtonHint write SetButtonHint;
      {Determines the hint displayed when cursor is hovered over browse button.
      Component's ShowHint property must be enabled to show the hint}
    property OnBrowse: TNotifyEvent read fOnBrowse write fOnBrowse;
      {Event triggered when browse button is clicked: the event handler is
      passed the current text from edit control which can be altered by the
      hanlder}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: creates and intialises owned speed button}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      {Override of method called when any of edit control's bounds properties
      are changed: ensures that button is aligned to right internal edge of
      component}
  end;

  {
  TBrowseEdit:
    Edit control that includes a configurable speed button aligned to the right
    edge of the edit box. An event is triggered when the button is clicked.

    Inheritance: TBrowseEdit -> TCustomBrowseEdit -> [TEdit]
  }
  TBrowseEdit = class(TCustomBrowseEdit)
  published
    // Published inherited properties and events
    property ButtonWidth;
    property ButtonGlyph;
    property ButtonText;
    property ButtonCursor;
    property ButtonHint;
    property OnBrowse;
  end;


procedure Register;
  {Register the components}


implementation


uses
  // Delphi
  SysUtils, Forms;


procedure Register;
  {Register the component}
begin
  RegisterComponents('SIBuilder', [TBrowseEdit]);
end;


{ TCustomBrowseEdit }

procedure TCustomBrowseEdit.AlignButton;
  {Aligns the browse button to the right hand internal edge of the edit control
  and sets button's height to fill the the edit control}
begin
  case BorderStyle of
    bsSingle:
    begin
      // Edit control has a border: fit button within border
      fBrowseBtn.Height := Height - 4;
      fBrowseBtn.Left := Width - GetButtonWidth - 4;
    end;
    bsNone:
    begin
      // Edit control has no border: fit button within control
      fBrowseBtn.Height := Height;
      fBrowseBtn.Left := Width - GetButtonWidth;
    end;
    else
      // Trap in case any other BordeStyle options provided
      Assert(False, 'Unexpected BorderStyle option in TCustomBrowseEdit');
  end;
end;

procedure TCustomBrowseEdit.BrowseBtnClick(Sender: TObject);
  {OnClick event handler for browse button: calls DoBrowse method to perform
  required action}
begin
  DoBrowse;
end;

procedure TCustomBrowseEdit.CMFontChanged(var Msg: TMessage);
  {Intercepts font changes: font changes cause edit width to be reset - so we
  reset back again!}
begin
  inherited;
  SetEditMargin;
end;

constructor TCustomBrowseEdit.Create(AOwner: TComponent);
  {Class constructor: creates and intialises owned speed button}
begin
  inherited;
  // Create speed button and make this control the parent
  fBrowseBtn := TSpeedButton.Create(Self);
  with fBrowseBtn do
  begin
    Parent := Self;
    // Set default button width
    Width := 21;
    // Ensure button displays arrow cursor rather than edit control's I bar
    Cursor := crArrow;
    // Direct click event to internal handler
    OnClick := BrowseBtnClick;
  end;
end;

procedure TCustomBrowseEdit.CreateWnd;
  {Overridden method called when window is created. Ensures that button is
  fitted to control (this method is called when BorderStyle property is changed
  so we adjust button's alignment). Also sets edit control's margin so that edit
  text doesn't overwrite button}
begin
  inherited;
  // Set right margin of edit control so button doesn't get overwritten by text
  SetEditMargin;
  // Align the browse button to the edit control
  AlignButton;
end;

procedure TCustomBrowseEdit.DoBrowse;
  {Triggers OnBrowse event}
begin
  if Assigned(fOnBrowse) then
    // Call event handler: TheText could be changed in handler
    fOnBrowse(Self);
end;

function TCustomBrowseEdit.GetButtonCursor: TCursor;
  {Read access method for ButtonCursor property: returns button's own Cursor
  property}
begin
  Result := fBrowseBtn.Cursor;
end;

function TCustomBrowseEdit.GetButtonGlyph: TBitmap;
  {Read access method for ButtonGlyph property: returns button's own Glyph
  property}
begin
  Result := fBrowseBtn.Glyph;
end;

function TCustomBrowseEdit.GetButtonHint: string;
  {Read access method for ButtonHint property: returns button's own Hint
  property}
begin
  Result := fBrowseBtn.Hint;
end;

function TCustomBrowseEdit.GetButtonText: string;
  {Read access method for ButtonText property: returns button's own Caption
  property}
begin
  Result := fBrowseBtn.Caption;
end;

function TCustomBrowseEdit.GetButtonWidth: Integer;
  {Read access method for ButtonWidth property: returns button's own Width
  property}
begin
  Result := fBrowseBtn.Width;
end;

procedure TCustomBrowseEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  {Override of method called when any of edit control's bounds properties are
  changed: ensures that button is aligned to right internal edge of component}
begin
  inherited;
  if Assigned(fBrowseBtn) then
    AlignButton;
end;

procedure TCustomBrowseEdit.SetButtonCursor(const Value: TCursor);
  {Write access method for ButtonCursor property: stores new cursor in button's
  Cursor property}
begin
  fBrowseBtn.Cursor := Value;
end;

procedure TCustomBrowseEdit.SetButtonGlyph(const Value: TBitmap);
  {Write access method for ButtonGlyph property: stores new bitmap in button's
  Glyph property}
begin
  fBrowseBtn.Glyph.Assign(Value);
end;

procedure TCustomBrowseEdit.SetButtonHint(const Value: string);
  {Write access method for ButtonHint property: stores new hint in button's Hint
  property}
begin
  fBrowseBtn.Hint := Value;
end;

procedure TCustomBrowseEdit.SetButtonText(const Value: string);
  {Write access method for ButtonButtonText property: stores new value in
  button's Caption property}
begin
  fBrowseBtn.Caption := Value;
end;

procedure TCustomBrowseEdit.SetButtonWidth(const Value: Integer);
  {Write access method for ButtonWidth property: stores new value in button's
  Width property and then realigns button and sets edit control margin}
begin
  if GetButtonWidth <> Value then
  begin
    // Record new value
    fBrowseBtn.Width := Value;
    // Re-align button so it is flush with edit control's internal right edge
    AlignButton;
    // Set edit control's margin so it doesn't overwrite button
    SetEditMargin;
  end;
end;

procedure TCustomBrowseEdit.SetEditMargin;
  {Sets edit control's right margin so that edit control's text area doesn't
  overwrite the button at right edge of control}
var
  Margins: LongWord;  // stores edit control's left and right margins
begin
  // Get edit control's left and right margins encoded in Margins
  Margins := SendMessage(Handle, EM_GETMARGINS, 0, 0);
  // Set Hi word of Margins to button width: this is new margin
  LongRec(Margins).Hi := GetButtonWidth;
  // Set new margins (left margin is unchanged)
  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, Margins);
end;

end.
