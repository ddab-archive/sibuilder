{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpHotText.pas
  @COMMENTS                 Implements a component that can render multi-format
                            text described by a pseudo SGML language called
                            "hottext". The text may contain link areas that
                            triggers command events.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
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
 * The Original Code is CmpHotText.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpHotText;


interface


uses
  // Delphi
  Classes, Controls, Windows, Graphics, Messages,
  // Project
  UHotTextDoc, UHotTextView, UHotTextLink;


type

  {
  THotTextOnLinkClick:
    Type of method required to handle OnLinkClick events triggered when the user
    links on a hot link. The link's command string is provided along with the
    link item's bounding rectangle.
  }
  THotTextOnLinkClick = procedure(Sender: TObject; const Cmd: string;
    const Rect: TRect) of object;

  {
  THotTextOnLinkOver:
    Type of method required to handle OnLinkOver events that are generated
    whenever the mouse enters a link and exits to normal text. The event
    triggered when returning to plain text contains empty parameters.
  }
  THotTextOnLinkOver = procedure(Sender: TObject; const Cmd, Hint: string;
    const Rect: TRect) of object;

  {
  THotText:
    A component that can render multi-format text described by a pseudo SGML
    language called <hottext>. The component loads the hot text code into an
    internal document using a parser to interpret it. The document is used as
    the start of a process to render the text for display. The rendering is done
    by the view object which formats the text for painting to the control's
    canvas. The text contains link areas which can trigger actions (string
    commands) that are notified to users via an event.
  }
  THotText = class(TGraphicControl)
  private // properties
    fCode: string;
    fLinkCursor: TCursor;
    fShowLinkHint: Boolean;
    fOnLinkClick: THotTextOnLinkClick;
    fOnLinkOver: THotTextOnLinkOver;
    fMargin: Integer;
    procedure SetCode(const Value: string);
    procedure SetLinkCursor(const Value: TCursor);
    procedure SetShowLinkHint(const Value: Boolean);
    procedure SetMargin(const Value: Integer);
    function GetPlainText: string;
  private
    fDocument: THotTextDoc;
      {The document that stores the essential information about the structure of
      the document we are displaying. This document is an intermediary between
      the control's rendered view and the parser and raw code. The document
      structure is not changed by sizing of the control. The component renders
      this document for a particular display using the view object}
    fView: THotTextView;
      {This object stores the rendered document in a form suitable for painting
      onto the control. The view reads the document object and displays the text
      in the control}
    fViewInvalidated: Boolean;
      {Flag true if the view object is invalidated. When true the view object
      must be rebuilt before the next paint}
    fPrevClientRect: TRect;
      {Stores the control's client rectangle the last time the control was
      painted. Used to check on current szie just before control is painted. If
      control has changed size the view needs to be re-rendered before control
      is painted}
    fCursor: TCursor;
      {Records the value of the main cursor when Cursor property is heing used
      to indicate a link}
    fCurLinkIdx: Integer;
      {Index of the currently active text item storing the selected kink. A
      value of -1 idicates there is no current link}
    fPopupWdw: THintWindow;
      {Object used to shoe a popup window that displays hints for links when the
      mouse cursor passed over them}
    procedure InvalidateView;
      {Records that the current view is no longer valid and invalidates the
      control. When the control is redrawn the view will be rebuilt before being
      displayed}
    procedure BuildView;
      {Render the document into a "view" object that contains a list of
      descriptions of how to display the document. The view fits the document to
      the available drawing canvas}
    procedure BuildDoc(const Code: string);
      {Build a document that represents the document defined by the given hot
      text source code}
    function HotLinkItemIdx(const Pt: TPoint): Integer;
      {Looks for a link in the view with bounding rectangle containing the given
      point. If found the link item's index in the view is return. -1 is
      returned if there is no matching link}
    procedure UpdateCurrentLink(const LinkIdx: Integer);
      {Updates the current link to LinkIdx and updates display accordingly. If
      LinkIdx is -1 then there is no current link (i.e. no link under cursor)}
    procedure DisplayPopup(Pos: TPoint; const BoundsRect: TRect;
      const LinkHint: string);
      {Displays popup link hint window if there is any link hint text and we are
      showing link hints}
    procedure ClosePopup;
      {Close any currently open popup link hint window}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
      {Handles CM_FONTCHANGED message: rebuild document from source code since
      document object may depend on component's font}
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {Handles CM_MOUSELEAVE message: ensures that any link cursor is restored
      to normal when mouse leaves control and closes any link hint window}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {Handles CM_ENABLEDCHANGED message: ensures that any active link hint is
      hidden and cursor is restored to normal if control is disabled}
    procedure CMCursorChanged(var Msg: TMessage); message CM_CURSORCHANGED;
      {Handles CM_CURSORCHANGED message: detects when user has changed Cursor
      property. We store new property value for later use, unless it was the
      link cursor which we may have assigned internally}
  protected
    procedure Paint; override;
      {Triggered when control needs painting: ensures hot text document and
      rendered view are up to date and then paints the control}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      {Triggered when mouse is moved: checks to see if mouse has moved onto or
      off a link and shows or hides link cursor and any pop-up hint accordingly.
      A OnLinkOver event is triggered if mouse is over a link}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
      {Triggered when mouse up event received: triggers OnLinkClick event if any
      link is under the mouse when released}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: initialises properties and private fields}
    destructor Destroy; override;
      {Class destructor: tidies up}
    procedure LoadFromFile(const FileName: string);
      {Loads the hottext code from the given file and stores the contents in the
      Code property, causing the new code to be displayed}
    procedure LoadFromStream(const Stream: TStream);
      {Loads hottext code from the given stream and stores in the Code property,
      causing the document to be rebuilt}
    procedure LoadFromResourceName(const Instance: THandle;
      const ResName: string; const ResType: PChar);
      {Loads hottext code from the given resource of the given type. The
      resource is specified by name. The resource is loaded from the module
      specified by Instance}
    procedure LoadFromResourceId(const Instance: THandle;
      const ResId: Integer; const ResType: PChar);
      {Loads hottext code from the given resource of the given type. The
      resource is specified by ID rather than name. The resource is loaded from
      the module specified by Instance}
  published
    property Code: string
      read fCode write SetCode;
      {Stores the <hottext> tagged source code that defines the display. Setting
      this value causes the code to be parsed and the internal document
      representation to be updated. Using any of th LoadFrom***** methods below
      will set this property}
    property LinkCursor: TCursor
      read fLinkCursor write SetLinkCursor default crHandPoint;
      {The cursor to display when the mouse is over a hont link}
    property Margin: Integer
      read fMargin write SetMargin default 0;
      {The margin (in pixels) that surrounds the text in the display: i.e. the
      displayed text is offset from the top and left side of the display by
      Margin pixels}
    property PlainText: string read GetPlainText;
      {Returns the text of the hot text document without formatting. New
      paragraphs cause a CRLF pair to be included in the text, except new
      paragraphs before the first text item and any new paragraph at the end of
      document are ignored}
    property ShowLinkHint: Boolean
      read fShowLinkHint write SetShowLinkHint default True;
      {Property indicates whether pop-up hints are displayed near links when the
      mouse is over the link}
    property OnLinkClick: THotTextOnLinkClick
      read fOnLinkClick write fOnLinkClick;
      {Event triggered when a link is clicked: the selected link is that under
      the mouse when it is released}
    property OnLinkOver: THotTextOnLinkOver
      read fOnLinkOver write fOnLinkOver;
      {Event triggered when the mouse cursor enters a link. The event is also
      triggered as the mouse moves to other elements of the same link or to
      different kinks. The event is triggered with nul values when the mouse
      leaves a link}
    // Inherited properties exposed
    property Align;
    property Anchors;
    property Color nodefault;   // needed to persist this property
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


procedure Register;
  {Component registration procedure}


implementation


uses
  // Delphi
  SysUtils, Forms,
  // Project
  UHotTextParser;


procedure Register;
  {Component registration procedure}
begin
  // Register component on SIBuilder palette
  RegisterComponents('SIBuilder', [THotText]);
end;


{ THotText }

procedure THotText.BuildDoc(const Code: string);
  {Build a document that represents the document defined by the given hot text
  source code}
var
  Parser: THotTextParser; // parser used to translate code into doc description
begin
  // If we have code we parse it a build doc otherwise we simply clear document
  if Code <> '' then
  begin
    // Create parser, assigned default font and parse the code
    Parser := THotTextParser.Create;
    try
      Parser.DefFont := Self.Font;
      Parser.Code := Code;
      Parser.Execute(fDocument);
    finally
      Parser.Free;
    end;
  end
  else
    // No code: just clear the document
    fDocument.Clear;
  // Existing view of document is now invalid since doc has changed
  InvalidateView;
end;

procedure THotText.BuildView;
  {Render the document into a "view" object that contains a list of descriptions
  of how to display the document. The view fits the document to the available
  drawing canvas}
var
  DocIdx: Integer;            // loops through entries in document
  DocEntry: THotTextDocEntry; // an entry in the document
begin
  // Reset the view object ready to build new view
  fView.Reset(
    Self.ClientWidth - 2 * fMargin,
    Self.ClientHeight - 2 * fMargin,
    fMargin
  );
  // Loop thru each entry in the document, updating view as required
  for DocIdx := 0 to Pred(fDocument.Count) do
  begin
    DocEntry := fDocument[DocIdx];
    case DocEntry.Kind of
      ikText:       fView.AddText(DocEntry.Text);
      ikFont:       fView.UpdateFont(DocEntry.Font);
      ikNewPara:    fView.NewPara(DocEntry.ParaStyle);
      ikLinkStart:  fView.UpdateLink(DocEntry.Link);
      ikLinkEnd:    fView.UpdateLink(nil);
    end;
  end;
  // Close off the page
  fView.EndPage;
  // Record that we have to currently active link in the component's display
  UpdateCurrentLink(-1);
  // Note that view is now invalid
  fViewInvalidated := False;
end;

procedure THotText.ClosePopup;
  {Close any currently open popup link hint window}
begin
  fPopupWdw.ReleaseHandle;
end;

procedure THotText.CMCursorChanged(var Msg: TMessage);
  {Handles CM_CURSORCHANGED message: detects when user has changed Cursor
  property. We store new property value for later use, unless it was the link
  cursor which we may have assigned internally}
begin
  inherited;
  if Cursor <> fLinkCursor then
    fCursor := Cursor;
end;

procedure THotText.CMEnabledChanged(var Msg: TMessage);
  {Handles CM_ENABLEDCHANGED message: ensures that any active link hint is
  hidden and cursor is restored to normal if control is disabled}
begin
  if not Enabled then
    UpdateCurrentLink(-1);  // "unhighlights" link and closes any hint window
  inherited;    // calls Invalidate
end;

procedure THotText.CMFontChanged(var Msg: TMessage);
  {Handles CM_FONTCHANGED message: rebuild document from source code since
  document object may depend on component's font}
begin
  BuildDoc(fCode);
  inherited;
end;

procedure THotText.CMMouseLeave(var Msg: TMessage);
  {Handles CM_MOUSELEAVE message: ensures that any link cursor is restored to
  normal when mouse leaves control and closes any link hint window}
begin
  inherited;
  UpdateCurrentLink(-1);  // "unhighlights" link and closes any hint window
end;

constructor THotText.Create(AOwner: TComponent);
  {Class constructor: initialises properties and private fields}
begin
  inherited Create(AOwner);
  // Make control fill client rect (csOpaque: reduces flicker) and
  // make replicatable (can be copied with PaintTo method)
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  // Create owned document and view objects
  fDocument := THotTextDoc.Create;
  fView := THotTextView.Create(Canvas, ClientWidth, ClientHeight, 0);
  // Create hint window to be used for link popup hints
  fPopupWdw := THintWindow.Create(Self);
  fPopupWdw.Color := clInfoBk;
  // Set curret link index to "no link"
  fCurLinkIdx := -1;
  // Set default size
  Width := 200;
  Height := 120;
  // Record cursor, default link cursor and ensure link hints shown by default
  fCursor := Cursor;
  fLinkCursor := crHandPoint;
  fShowLinkHint := True;
  // Note that view is not valid
  fViewInvalidated := True;
end;

destructor THotText.Destroy;
  {Class destructor: tidies up}
begin
  // Free owned objects
  fView.Free;
  fDocument.Free;
  inherited;
end;

procedure THotText.DisplayPopup(Pos: TPoint; const BoundsRect: TRect;
  const LinkHint: string);
  {Displays popup link hint window if there is any link hint text and we are
  showing link hints. If the link hint is separated by a bar then the short hint
  part of the hint is displayed. The link is display with is left side a the
  mouse position, with its top below the hot link's text}
var
  PopupRect: TRect;     // rectangle used for pop-up window
  ScreenPos: TPoint;    // screen position of pop-up location
  DisplayText: string;  // the text to be displayed
begin
  if fShowLinkHint and (LinkHint <> '') then
  begin
    // Determine locaion where hint pops up:
    // we used x co-ord of mouse from Pos parameter, but we align hint below the
    // link text by seeting y co-ord to bottom of link's bounding rectangle
    // We use x component of mouse pos, but we set y position to below link
    Pos.y := BoundsRect.Bottom;
    // Get popup position in screen co-ordinates
    ScreenPos := Self.ClientToScreen(Pos);
    // Calculate display rectangle for pop-up window: uses short hint from link
    // get text and size it
    DisplayText := GetShortHint(LinkHint);
    PopupRect := fPopupWdw.CalcHintRect(200, DisplayText, nil);
    // left is relative to mouse X co-ord and top to bottom of link
    OffsetRect(PopupRect, ScreenPos.X, ScreenPos.Y);
    // Show the window
    fPopupWdw.ActivateHint(PopupRect, DisplayText);
  end;
end;

function THotText.GetPlainText: string;
  {Read access method for PlainText property: scans current document extracting
  all text entries and new paragraphs and combining into a single piece of text
  with line breaks at new paragraphs (except if occur before text or at end)}
var
  Idx: Integer;             // loops thru entries in document object
  Entry: THotTextDocEntry;  // entry in document object
  HaveText: Boolean;        // flag false until some text is encountered
begin
  // Initialise
  Result := '';
  HaveText := False;
  // Loop thru all entries in document
  for Idx := 0 to Pred(fDocument.Count) do
  begin
    Entry := fDocument[Idx];
    if Entry.Kind = ikText then
    begin
      // we have text item: append to output string
      Result := Result + Entry.Text;
      HaveText := True;
    end;
    if Entry.Kind = ikNewPara then
    begin
      // new para: append CRLF only if we've written text & not last item
      if HaveText and (Idx <> Pred(fDocument.Count)) then
        Result := Result + #13#10;
    end;
  end;
end;

function THotText.HotLinkItemIdx(const Pt: TPoint): Integer;
  {Looks for a link in the view with bounding rectangle containing the given
  point. If found the link item's index in the view is return. -1 is returned
  if there is no matching link}
var
  Idx: Integer;               // loops thru list of links
  LinkItemIdx: Integer;       // stores index in link list of matching link
  LinkRef: THotTextLinkRef;   // a referene to a link item from link list
begin
  // Assume failure
  Result := -1;
  // Loop through list of all links in view
  for Idx := 0 to Pred(fView.LinkCount) do
  begin
    // Ask item from link list if any of text items in link contains Pt
    LinkRef := fView.Links[Idx];
    LinkItemIdx := LinkRef.PtInLink(Pt);
    if LinkItemIdx <> -1 then
    begin
      // Found one: return index of item in view
      Result := LinkItemIdx;
      Break;
    end
  end;
end;

procedure THotText.InvalidateView;
  {Records that the current view is no longer valid and invalidates the control.
  When the control is redrawn the view will be rebuilt before being displayed}
begin
  // Set current link to -1: it is possible for a move mouse message to query
  // an old previous link after view has been recalculated and cause index
  // out of bounds error if we don't do this
  UpdateCurrentLink(-1);
  // Now invalidate the view and invalidate the control to cause repaint
  fViewInvalidated := True;
  Invalidate;
end;

procedure THotText.LoadFromFile(const FileName: string);
  {Loads the hottext code from the given file and stores the contents in the
  Code property, causing the new code to be displayed}
var
  FS: TFileStream;  // stream onto input file
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure THotText.LoadFromResourceId(const Instance: THandle;
  const ResId: Integer; const ResType: PChar);
  {Loads hottext code from the given resource of the given type. The resource
  is specified by ID rather than name. The resource is loaded from the module
  specified by Instance}
var
  RS: TResourceStream;  // stream used to read resource
begin
  RS := TResourceStream.CreateFromID(Instance, ResID, ResType);
  try
    LoadFromStream(RS);
  finally
    RS.Free;
  end;
end;

procedure THotText.LoadFromResourceName(const Instance: THandle;
  const ResName: string; const ResType: PChar);
  {Loads hottext code from the given resource of the given type. The resource
  is specified by name. The resource is loaded from the module specified by
  Instance}
var
  RS: TResourceStream;  // stream used to read resource
begin
  RS := TResourceStream.Create(Instance, ResName, ResType);
  try
    LoadFromStream(RS);
  finally
    RS.Free;
  end;
end;

procedure THotText.LoadFromStream(const Stream: TStream);
  {Loads hottext code from the given stream and stores in the Code property,
  causing the document to be rebuilt}
var
  TheCode: string;  // code read from stream
  Size: Integer;    // size of code read
begin
  // Calculate the size of the code (assume rest of stream) and size code string
  Size := Stream.Size - Stream.Position;
  SetLength(TheCode, Size);
  // Read the code and store in Code property
  Stream.ReadBuffer(TheCode[1], Size);
  SetCode(TheCode);
end;

procedure THotText.MouseMove(Shift: TShiftState; X, Y: Integer);
  {Triggered when mouse is moved: checks to see if mouse has moved onto or off
  a link and shows or hides link cursor and any pop-up hint accordingly. A
  OnLinkOver event is triggered if mouse is over a link}
var
  FoundLinkItemIdx: Integer;  // index of any link item under cursor
begin
  inherited;
  // We don't process mouse move if view invalidated: can cause GPF
  if fViewInvalidated then Exit;
  // Find index of any link under cursor
  FoundLinkItemIdx := HotLinkItemIdx(Point(X, Y));
  // Update links as required
  UpdateCurrentLink(FoundLinkItemIdx);
end;

procedure THotText.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
  {Triggered when mouse up event received: triggers OnLinkClick event if any
  link is under the mouse when released}
var
  LinkTextItemIdx: Integer;   // index of any link under mouse
  Link: THotTextLink;         // link object for link under mouse
  TextItem: THotTextViewItem; // object describing view item under mouse
  TextItemRect: TRect;        // bounding rect of view item under mouse
begin
  inherited;
  // We don't process mouse up if view invalidated: *may* cause GPF
  if fViewInvalidated then Exit;
  // Exit if button is not left one or if control disabled
  if (Button <> mbLeft) or not Enabled then
    Exit;
  // Find index of any hot link under cursor
  LinkTextItemIdx := HotLinkItemIdx(Point(X, Y));
  if LinkTextItemIdx >= 0 then
  begin
    // There is a link item under cursor: trigger link event if assigned
    ClosePopup;
    if Assigned(fOnLinkClick) then
    begin
      // get text item associated with link
      TextItem := fView.Items[LinkTextItemIdx];
      // get bounding rectangle of item and reference to link object
      TextItemRect := TextItem.BoundsRect;
      Link := TextItem.Link;
      Assert(Assigned(Link));
      // trigger event, passing link command and bound rectangle
      fOnLinkClick(Self, Link.Cmd, TextItemRect);
    end;
  end;
end;

procedure THotText.Paint;
  {Triggered when control needs painting: ensures hot text document and rendered
  view are up to date and then paints the control}
begin
  inherited;
  // Check if control's size has changed since last paint: invalidate view if so
  if not EqualRect(ClientRect, fPrevClientRect) then
  begin
    fViewInvalidated := True; // don't call InvalidateView: it invalidates again
    fPrevClientRect := ClientRect;
  end;
  // Check if rendered view is valid and re-render if not
  if fViewInvalidated then
    BuildView;
  // Draw background, with dashed boundary at design time
  with Canvas do
  begin
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    FillRect(ClientRect);
    Brush.Style := bsClear;
    if csDesigning in ComponentState then
    begin
      Pen.Style := psDash;
      Rectangle(0, 0, Width, Height);
    end;
  end;
  // Get the view object to paint itself onto the canvas
  fView.Paint(Enabled);
end;

procedure THotText.SetCode(const Value: string);
  {Write access method for Code property: stores the code and rebuilds the
  document object by parsing the new code. Invalidates control to cause a
  repaint}
begin
  if fCode <> Value then
  begin
    // Build doc using new code value:
    // if exception raised, we preserve old value of Code property
    BuildDoc(Value);
    fCode := Value;
  end;
end;

procedure THotText.SetLinkCursor(const Value: TCursor);
  {Write access method for LinkCursor property: records new value and updates
  currently displayed cursor if link cursor is currently displayed}
var
  ChangeCursor: Boolean;  // whether physical cursor needs changing
begin
  if fLinkCursor <> Value then
  begin
    // careful here: check if actual cursor needs changing
    // (it does if its using old track cursor)
    ChangeCursor := Cursor = fLinkCursor;
    // record new value
    fLinkCursor := Value;
    // now update Cursor if required: doing it here prevents from interfering
    // with standard cursor: setting of Cursor is not recorded if cursor is
    // current track cursor (See CMCursorChanged message handler for more info)
    if ChangeCursor then
      Cursor := Value;
  end;
end;

procedure THotText.SetMargin(const Value: Integer);
  {Write access method for Margin property: requires view to be rebuilt and
  control to be re-displayed}
begin
  if (fMargin <> Value) then
  begin
    fMargin := Value;
    InvalidateView;
  end;
end;

procedure THotText.SetShowLinkHint(const Value: Boolean);
  {Write access method for LinkHint property: records new value and hides any
  current hint if we don't want hints and one is currently displayed}
begin
  if fShowLinkHint <> Value then
  begin
    ClosePopup;
    fShowLinkHint := Value;
  end;
end;

procedure THotText.UpdateCurrentLink(const LinkIdx: Integer);
  {Updates the current link to LinkIdx and updates display accordingly. If
  LinkIdx is -1 then there is no current link (i.e. no link under cursor)}
var
  CurLink: THotTextLink;  // object describing existing current link
  Link: THotTextLink;     // object describing new current link
  MousePos: TPoint;       // position of mouse on screen
  MousePosCli: TPoint;    // position of mouse relative to control's client area
  BoundsRect: TRect;      // bounds rect of new current link

  // ---------------------------------------------------------------------------
  function LinkIdx2Link(Idx: Integer): THotTextLink;
    {Returns reference to link object at given idex in document, or nil if there
    is no such item or if item has nil link}
  begin
    if Idx >= 0 then
      Result := fView[Idx].Link
    else
      Result := nil;
  end;
  // ---------------------------------------------------------------------------
  function LinkIdx2Bounds(Idx: Integer): TRect;
    {Returns bounds rectangle of link text item as given index in view. If there
    is no such item then Rect(0,0,0,0) is returned}
  begin
    if Idx >= 0 then
      Result := fView[Idx].BoundsRect
    else
      Result := Rect(0, 0, 0, 0);
  end;
  // ---------------------------------------------------------------------------

begin
  // Given index same as existing current one or ctrl disabled: nothing to do
  if (LinkIdx = fCurLinkIdx) then
    Exit;

  // Get mouse position
  GetCursorPos(MousePos);
  MousePosCli := ScreenToClient(MousePos);

  // Get references to old current link and new one
  CurLink := LinkIdx2Link(fCurLinkIdx);
  Link := LinkIdx2Link(LinkIdx);

  // Get bounds rect for new link (zero if new link is nil)
  BoundsRect := LinkIdx2Bounds(LinkIdx);

  // Process possible combinations

  // 1: Move off link onto plain text (CurLink <> nil, Link = nil)
  //    close popup
  //    restore normal cursor
  //    trigger empty OnLinkOver event
  if Assigned(CurLink) and not Assigned(Link) then
  begin
    ClosePopup;
    Cursor := fCursor;
    if Assigned(fOnLinkOver) then
      fOnLinkOver(Self, '', '', BoundsRect);  // BoundsRect = (0,0,0,0)
  end
  // 2: Move off plain text onto a link (CurLink = nil, Link <> nil)
  //    set link cursor
  //    display link popup hint
  //    trigger on link over event for new link ite,=m
  else if not Assigned(CurLink) and Assigned(Link) then
  begin
    Cursor := fLinkCursor;
    DisplayPopup(MousePosCli, BoundsRect, Link.Hint);
    if Assigned(fOnLinkOver) then
      fOnLinkOver(Self, Link.Cmd, Link.Hint, BoundsRect);
  end
  else if Assigned(Link) and Assigned(CurLink) then
  begin
    if Link = CurLink then
    begin
      // 3: Move off one link item onto another that references the same link
      //    object (CurLink <> nil, Link <> nil, Link = CurLink)
      //    keep same popup window
      //    leave cursor unchanged
      //    trigger OnLinkOver event for new item (has different bounds)
      if Assigned(fOnLinkOver) then
        fOnLinkOver(Self, Link.Cmd, Link.Hint, BoundsRect);
    end
    else
    begin
      // 4: Move off one link onto another that references a different link
      //    object (CurLink <> nil, Link <> nil, CurLink <> Link)
      //    replace popup hint with one for new link
      //    leave cursor the same
      //    trigger OnLinkOver event for new item (all different)
      DisplayPopup(MousePosCli, BoundsRect, Link.Hint);
      if Assigned(fOnLinkOver) then
        fOnLinkOver(Self, Link.Cmd, Link.Hint, BoundsRect);
    end;
  end;

  // Record new link's index as current
  fCurLinkIdx := LinkIdx;
end;

end.
