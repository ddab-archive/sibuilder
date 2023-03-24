{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpButtonList.pas
  @COMMENTS                 Implements a custon list box component where each
                            list item has appearance of a flat button. This
                            component is used by SIBuilder.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/11/2005
      @COMMENTS             Changed colours of bitmap used to highlight selected
                            button to clBtnFace and clBtnHighlight since
                            previously used clMenu and clBtnHighlight can be
                            same colour on some colour schemes and highlight was
                            therefore being displayed as single colour.
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
 * The Original Code is CmpButtonList.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpButtonList;

interface

uses
  // Delphi
  Windows, Messages, Classes, Graphics, Controls, StdCtrls;

type

  {
  TNewButtonListBox:
    Custom list box component in which each list item item has the appearance of
    a flat button. The buttons display a raised edge when the mouse tracks over
    them. Selected list items display as depressed buttons.
  }
  TNewButtonListBox = class(TCustomListBox)
  private
    fPrevX: Integer;
      {Records last X mouse co-ordinate}
    fPrevY: Integer;
      {Records last X mouse co-ordinate}
    fCheckedBrushBmp: TBitmap;
      {Checked bitmap used as background brush for selected list items}
    fTrackIndex: Integer;
      {Index of list item that is currently under mouse: -1 = no such item}
    fPrevTrackIndex: Integer;
      {Index of previous list item that was under mouse: -1 = no such item}
    procedure InvalidateItem(Index: Integer);
      {Invalidates rectangle bounding list item at given index, causing it to be
      redisplayed}
    procedure UpdateTrackingHighlight(Index: Integer);
      {Updates appearance of highlight on given item. If item has lost highlight
      it is removed and if item has gained highlight it is displayed}
    procedure CancelTracking;
      {Cancels any tracking by removing highlighting}
    function CreateCheckBrushBmp: TBitmap;
      {Creates the checked bitmap used as background brush for selected list
      items}
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      {Handles mouse movement and determines what, if anthing needs highlighting
      etc}
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
      {Draws list box items, highlighted as required}
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {Mouse has left control: cancel highlight for current list item and resets
      variables used to track mouse}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      {Mouse has entered control: ensure that tracking variables are reset to
      ensure list item under mouse is highlighted in MouseMove method}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: sets property default values, internal field values
      and creates owned bitmap}
    destructor Destroy; override;
      {Class destructor: frees owned bitmap object}
  published
    // Published inherited properties
    property Align;
    property Font;
    property HelpContext;
    property Items;
    property ItemHeight;
    property ParentFont;
    property PopupMenu;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
  end;


procedure Register;
  {Registers component with Delphi}


implementation


uses
  // Delphi
  Forms;


procedure Register;
  {Registers component with Delphi}
begin
  RegisterComponents('SIBuilder', [TNewButtonListBox]);
end;


{ TNewButtonListBox }

procedure TNewButtonListBox.CancelTracking;
  {Cancels any tracking by removing highlighting}
var
  StoreIdx: Integer;  // records track index to allow it to be un-higlighted
begin
  // Unhighlight current item:
  // fTrackIndex must not be current item for this to work
  StoreIdx := fTrackIndex;
  fTrackIndex := -1;
  UpdateTrackingHighlight(StoreIdx);
end;

procedure TNewButtonListBox.CMMouseEnter(var Msg: TMessage);
  {Mouse has entered control: ensure that tracking variables are reset to ensure
  list item under mouse is highlighted in MouseMove method}
begin
  inherited;
  fPrevX := -1;
  fPrevY := -1;
end;

procedure TNewButtonListBox.CMMouseLeave(var Msg: TMessage);
  {Mouse has left control: cancel highlight for current list item and resets
  variables used to track mouse}
begin
  inherited;
  CancelTracking;
  fPrevX := -1;
  fPrevY := -1;
  fPrevTrackIndex := -2;
end;

constructor TNewButtonListBox.Create(AOwner: TComponent);
  {Class constructor: sets property default values, internal field values and
  creates owned bitmap}
begin
  inherited;
  // Override inherited property defaults
  Style := lbOwnerDrawFixed;
  // Set defaults
  fTrackIndex := -1;      // there's no tracked list item
  fPrevTrackIndex := -1;  // ..so there's no previous one either!
  fPrevX := -1;           // mouse is not inside control
  fPrevY := -1;           // ..which we record using negative values
  BorderStyle := bsNone;  // control has no border
  Color := clBtnFace;     // ..and has button colour
  // Create bitmap for checked background pattern for depressed buttons
  fCheckedBrushBmp := CreateCheckBrushBmp;
end;

function TNewButtonListBox.CreateCheckBrushBmp: TBitmap;
  {Creates the checked bitmap used as background brush for selected list items}
var
  X, Y: Integer;      // loop thru X and Y pixels of bitmap
begin
  // Create bitmap of required brush size
  Result := TBitmap.Create;
  Result.Width := 8;
  Result.Height := 8;
  // Fill background with required colour
  Result.Canvas.Brush.Color := clBtnFace;
  Result.Canvas.FillRect(Rect(0, 0, 8, 8));
  // Create chequerboard pattern using button highlight colour
  for X := 0 to 7 do
    for Y := 0 to 7 do
      if (Y mod 2) = (X mod 2) then
        Result.Canvas.Pixels[X, Y] := clBtnHighlight;
end;

destructor TNewButtonListBox.Destroy;
  {Class destructor: frees owned bitmap object}
begin
  fCheckedBrushBmp.Free;
  inherited;
end;

procedure TNewButtonListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
  {Draws list box items, highlighted as required}
var
  Flags: LongInt;   // flags passed to DrawText API function
  TextBmp: TBitmap; // background bitmap used to draw text before displaying
  TextRect: TRect;  // rectangle in which to display text
begin
  // Set up brush according to state of control
  if (odSelected in State) and (Index <> fTrackIndex) then
  begin
    // item is selected and is current: use background bitmap
    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Bitmap := fCheckedBrushBmp;
  end
  else
  begin
    // item not selected or isn't current
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Color := clBtnFace;
  end;
  // Clear the item background
  Canvas.FillRect(Rect);
  // Draw button edges if required
  if odSelected in State then
    // button is selected: draw sunken edge
    DrawEdge(Canvas.Handle, Rect, BDR_SUNKENOUTER, BF_RECT)
  else if (Index = fTrackIndex) and not (odDisabled in State) then
    // button is under cursor and we're tracking: draw raised
    DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_RECT);
  if odFocused in State then
    // button is focussed: draw focus rectangle
    DrawFocusRect(Canvas.Handle, Rect);
  // Draw text
  // set up text drawing flags
  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER);
  // create bitmap on which to draw text
  TextBmp := TBitmap.Create;
  try
    // set required font
    TextBmp.Canvas.Font := Font;
    // reduce drawing rectangle to fall within button
    InflateRect(Rect, -4, -2);
    if odSelected in State then
      OffsetRect(Rect, 1, 1);
    // set up size of text bitmap
    TextBmp.Width := Rect.Right - Rect.Left;
    TextBmp.Height := Rect.Bottom - Rect.Top;
    TextRect := Bounds(0, 0, TextBmp.Width, TextBmp.Height);
    if Enabled then
      // draw button's text, enabled
      DrawText(
        TextBmp.Canvas.Handle,
        PChar(Items[Index]),
        Length(Items[Index]),
        TextRect,
        Flags
      )
    else
    begin
      // draw button's text, disabled (ecthed)
      OffsetRect(TextRect, 1, 1);
      TextBmp.Canvas.Font.Color := clBtnHighlight;
      DrawText(
        TextBmp.Canvas.Handle,
        PChar(Items[Index]),
        Length(Items[Index]),
        TextRect,
        Flags
      );
      OffsetRect(TextRect, -1, -1);
      TextBmp.Canvas.Brush.Style := bsClear;
      TextBmp.Canvas.Font.Color := clBtnShadow;
      DrawText(
        TextBmp.Canvas.Handle,
        PChar(Items[Index]),
        Length(Items[Index]),
        TextRect,
        Flags
      );
    end;
    // copy bitmap containing text onto button
    Canvas.BrushCopy(Rect, TextBmp, TextRect, clWhite);
  finally
    TextBmp.Free;
  end;
end;

procedure TNewButtonListBox.InvalidateItem(Index: Integer);
  {Invalidates rectangle bounding list item at given index, causing it to be
  redisplayed}
var
  Rect: TRect;  // item's bounding rectangle
begin
  if Index > -1 then
  begin
    // Cause repaint of item => DrawItem called which performs any highlighting
    Rect := Self.ItemRect(Index);
    InvalidateRect(Self.Handle, @Rect, False);
  end;
end;

procedure TNewButtonListBox.MouseMove(Shift: TShiftState; X,
  Y: Integer);
  {Handles mouse movement and determines what, if anthing needs highlighting
  etc}
begin
  inherited;
  // Only process the mouse moved if mouse has actually moved!
  if ((fPrevX <> X) or (fPrevY <> Y)) then
  begin
    // Determine which, if any, item has text under mouse cursor
    fTrackIndex := Self.ItemAtPos(Point(X, Y), True);
    // Check if this has changed: update if so
    if fTrackIndex <> fPrevTrackIndex then
    begin
      // Update highlighting
      UpdateTrackingHighlight(fPrevTrackIndex);
      UpdateTrackingHighlight(fTrackIndex);
      // Record newly highlighted item as previous one
      fPrevTrackIndex := fTrackIndex;
    end;
  end;
  // Record current mouse coords
  fPrevX := X;
  fPrevY := Y;
end;

procedure TNewButtonListBox.UpdateTrackingHighlight(Index: Integer);
  {Updates appearance of highlight on given item. If item has lost highlight it
  is removed and if item has gained highlight it is displayed}
begin
  // Actual drawing done by DrawItem method when item is invalidated
  InvalidateItem(Index);
end;

end.
