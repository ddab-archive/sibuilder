{ ##                    
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInfoWdw.pas
  @COMMENTS                 Implements a popup window that is used to display
                            information and which can be dismissed by clicking
                            outside it or by using the window's close button.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
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
 * The Original Code is UInfoWdw.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInfoWdw;


interface


uses
  // Delphi
  Classes, Controls, Buttons, Windows, Messages;

type
  {
  TInfoWdw:
    Implements a popup window that is used to display information. The
    information is displayed with a bold header or title in a left hand column
    and the associated content or body text in a right hand column. Headers can
    optionally be displayed in red. The window content is provided using a
    string list of name=value pairs and red headings are displayed by setting
    the string list's Objects property to a non nil value. The window is
    dismissed by clicking its close button or clicking outside it. This class
    can act as a base class for more specialist classes.

    Inheritance: TInfoWdw -> [TCustomControl]
  }
  TInfoWdw = class(TCustomControl)
  private // properties
    fContents: TStringList;
    fColumnWidth: Integer;
    procedure SetContents(const Value: TStringList);
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
  private
    fButton: TSpeedButton;
      {Close button displayed in window}
    fTitleRect: TRect;
      {Rectangle that defines space occupied by pop-up window's title bar}
    procedure ButtonClick(Sender: TObject);
      {OnClick event handler for close button: close window}
  private // message handlers
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
      {Handles WM_NCHITTEST message: enables window to be dragged if user drags
      on fake title bar area}
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
      {Non-client paint handler: simply draw border round window (fake title bar
      is drawn in Paint method)}
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
      {Kill focus message handler: this is triggered when user clicks outside
      window. We close the window when this happens}
    function CalcOrPaint(CalcOnly: Boolean): TRect;
      {Calculates rectangle of required size to draw window: if CalcOnly is
      false then window's contents are also drawn on canvas}
  protected
    procedure Paint; override;
      {Paint the window's contents}
    procedure CreateParams(var Params: TCreateParams); override;
      {Set up window creation parameters}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: records title rectangle and creates owned close
      button}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure DisplayWdw(Left, Top: Integer);
      {Displays the window with contents per ProgramDesc property at given
      position}
    procedure Close;
      {Close the window}
    property Contents: TStringList read fContents write SetContents;
      {Contents of window. Contents are arranged into headings and body text.
      The heading appears at the LHS of the window in bold and the body text
      on the RHS. Contents are provided in form Heading=Body Text in the string
      list. To make the heading text for an item appear in read set the Objects
      property for the item to a non-nil value}
    property ColumnWidth: Integer read fColumnWidth write fColumnWidth;
      {Width of the heading column}
    property Width: Integer read GetWidth write SetWidth;
      {Total width of the window}
  end;


implementation


uses
  // Delphi
  Forms, Graphics, Math;


{ TInfoWdw }

procedure TInfoWdw.ButtonClick(Sender: TObject);
  {OnClick event handler for close button: close window}
begin
  Close;
end;

function TInfoWdw.CalcOrPaint(CalcOnly: Boolean): TRect;
  {Calculates rectangle of required size to draw window: if CalcOnly is false
  then window's contents are also drawn on canvas}

  // ---------------------------------------------------------------------------
  procedure CalcTextRect(var R: TRect; const Text: string);
    {Calculate size of rectangle required to draw given text}
  begin
    DrawText(Canvas.Handle, PChar(Text), -1, R, DT_CALCRECT or DT_LEFT or
      DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  end;

  procedure DrawTextRect(var R: TRect; const Text: string);
    {Draw the given text in the given rectangle}
  begin
    DrawText(Canvas.Handle, PChar(Text), -1, R, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;

  procedure ProcessItem(var R: TRect; const Top: Integer;
    const Heading, Body: string; IsError: Boolean = False);
    {Calculate size of and optionally display a text item: items comprise a bold
    title on left and plain text content on right. Item is displayed below the
    given rectangle}
  var
    HR, BR: TRect;    // rectangles to draw heading and body within
    Temp: TRect;      // temporary rectangle used in calculations
  begin
    // Make copy of curent rectangle
    Temp := R;
    // Allow for border
    InflateRect(Temp, -4, -4);
    // Set up heading rectangle below current one: leaving space between them
    HR := Temp;
    HR.Top := Top + 4;
    // Set up body rectangle level with top of heading
    BR := HR;
    HR.Right := fColumnWidth;
    BR.Left := fColumnWidth + 4;
    // Draw or calculate heading
    Canvas.Font.Style := [fsBold];        // use bold font
    if IsError then
      Canvas.Font.Color := clRed          // use red text if this is error msg
    else
      Canvas.Font.Color := clWindowText;  // use window text if msg is normal
    CalcTextRect(HR, Heading);            // calc required size of heading
    if not CalcOnly then
      DrawTextRect(HR, Heading);          // optionally draw heading
    Canvas.Font.Color := clWindowText;
    // Draw or calculate body text
    Canvas.Font.Style := [];              // plain font
    CalcTextRect(BR, Body);               // calc required size of body text
    if not CalcOnly then
      DrawTextRect(BR, Body);             // optionally draw body text
    // New bottom of rectangle is greater of bottom of header and body
    R.Bottom := Max(HR.Bottom, BR.Bottom);
  end;
  // ---------------------------------------------------------------------------

var
  R: TRect;         // bounding rectangle for window we are calculating
  Idx: Integer;     // loops thru all contents
  Heading: string;  // heading for an item in window
  Body: string;     // body text for a window item
begin
  // Start with title rectangle: we extend this vertically for body
  R := fTitleRect;

  // Draw fake title bar if we are actually growing
  if not CalcOnly then
  begin
    Canvas.Brush.Color := clActiveCaption;
    Canvas.FillRect(R);
  end;

  // Calculate window body optionally drawing it
  // we draw using clear brush over window
  Canvas.Brush.Style := bsClear;
  // calculate required size and optionally draw each info item
  for Idx := 0 to Pred(fContents.Count) do
  begin
    Heading := fContents.Names[Idx];
    Body := fContents.Values[Heading];
    ProcessItem(R, R.Bottom, Heading, Body, Assigned(fContents.Objects[Idx]));
  end;
  // Allow space for border
  Inc(R.Bottom, 8);
  Result := R;
end;

procedure TInfoWdw.Close;
  {Close the window}
begin
  // Invalidate owner window to ensure that screen under hint window is redrawn
  if Assigned(Owner) and (Owner is TWinControl) then
    (Owner as TWinControl).Invalidate;
  // Close the hint window: using recommended method
  DestroyHandle;
end;

constructor TInfoWdw.Create(AOwner: TComponent);
  {Class constructor: records title rectangle and creates owned close button}
begin
  inherited;
  // Record bounds of title bar
  fTitleRect := Rect(0, 0, 300, 8);
  // Create close button with appropriate properties: the button is owned by
  // this window and will be destroyed when the window is destroyed
  fButton := TSpeedButton.Create(Self);
  // display button inside this window
  fButton.Parent := Self;
  // set appearance
  fButton.Width := 16;
  fButton.Height := 16;
  fButton.Font.Name := 'Marlett';
  fButton.Caption := #$72;
  fButton.Flat := True;
  // assign click event handler
  fButton.OnClick := ButtonClick;
  // position button in window
  fButton.Top := fTitleRect.Bottom + 1;
  fButton.Left := fTitleRect.Right - fTitleRect.Left - fButton.Width - 3;
  // Create string list to hold contents
  fContents := TStringList.Create;
  // Initialise column width
  fColumnWidth := 60;
end;

procedure TInfoWdw.CreateParams(var Params: TCreateParams);
  {Set up window creation parameters}
begin
  inherited CreateParams(Params);
  with Params do
  begin
    // Set required window style
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    // we use tool window to prevent from appearing in task bar
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
    // make sure we use correct BiDi style
    AddBiDiModeExStyle(ExStyle);
  end;
end;

destructor TInfoWdw.Destroy;
  {Class destructor: frees owned objects}
begin
  fContents.Free;
  inherited;
end;

procedure TInfoWdw.DisplayWdw(Left, Top: Integer);
  {Displays the window with contents per ProgramDesc property at given position}
var
  Rect: TRect;  // bounding rectangle for popup window
begin
  // Determine rectangle in which to draw window
  Rect := CalcOrPaint(True);    // get size of rectangle required
  OffsetRect(Rect, Left, Top);  // set the required offset
  UpdateBoundsRect(Rect);       // update window's bounds to required rectangle
  // adjust window rectangle to ensure it appears on screen
  if Rect.Top + Height > Screen.DesktopHeight then
    Rect.Top := Screen.DesktopHeight - Height;
  if Rect.Left + Width > Screen.DesktopWidth then
    Rect.Left := Screen.DesktopWidth - Width;
  if Rect.Left < Screen.DesktopLeft then Rect.Left := Screen.DesktopLeft;
  if Rect.Bottom < Screen.DesktopTop then Rect.Bottom := Screen.DesktopTop;
  // size the window to rectangle
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
    SWP_SHOWWINDOW or SWP_NOACTIVATE);
  // Ensure popup window is activated (this is because we rely on kill focus
  // message to hide window if mouse clicked outside it)
  SetActiveWindow(Handle);
  Invalidate;
end;

function TInfoWdw.GetWidth: Integer;
  {Read access method for Width property: stored as width of title rectangle}
begin
  Result := fTitleRect.Right - fTitleRect.Left;
end;

procedure TInfoWdw.Paint;
  {Paint the window's contents}
begin
  CalcOrPaint(False);
end;

procedure TInfoWdw.SetContents(const Value: TStringList);
  {Write access method for Contents property: new value is assigned to property
  string list}
begin
  fContents.Assign(Value);
end;

procedure TInfoWdw.SetWidth(const Value: Integer);
  {Write access method for Width property: stores width in title bar rectangle
  and repositions close button on top right of window}
begin
  fTitleRect.Left := 0;
  fTitleRect.Right := Value;
  fButton.Top := fTitleRect.Bottom + 1;
  fButton.Left := fTitleRect.Right - fTitleRect.Left - fButton.Width - 3;
end;

procedure TInfoWdw.WMKillFocus(var Msg: TMessage);
  {Kill focus message handler: this is triggered when user clicks outside
  window. We close the window when this happens}
begin
  inherited;
  Close;
end;

procedure TInfoWdw.WMNCHitTest(var Msg: TWMNCHitTest);
  {Handles WM_NCHITTEST message: enables window to be dragged if user drags on
  fake title bar area}
var
  P: TPoint;  // point in window where mouse was clicked
begin
  inherited;
  // Get mouse click point in terms of this window
  P := ScreenToClient(SmallPointToPoint(Msg.Pos));
  if PtInRect(fTitleRect, P) then
    // mouse on title bar return HTCAPTION to enable dragging
    Msg.Result := HTCAPTION
  else
    // mouse in client area: say so
    Msg.Result := HTCLIENT
end;

procedure TInfoWdw.WMNCPaint(var Msg: TMessage);
  {Non-client paint handler: simply draw border round window (fake title bar is
  drawn in Paint method)}
var
  DC: HDC;  // device context for window
  R: TRect; // bounding rect of window in window coords
begin
  // Get device context for non client area
  DC := GetWindowDC(Handle);
  try
    // Draw edge round the window
    R := Rect(0, 0, Width, Height);
    DrawEdge(DC, R, EDGE_RAISED	, BF_RECT or BF_FLAT	);
  finally
    // Free device context
    ReleaseDC(Handle, DC);
  end;
end;

end.
