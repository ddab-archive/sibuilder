{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UHotTextView.pas
  @COMMENTS                 This unit implements a main object that encapsulates
                            a view of a hot text document as it will be rendered
                            on a canvas. Several subsidiary classes are also
                            implemented.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 24/02/2008
      @COMMENTS             Moved error message string literals to resource
                            strings.
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
 * The Original Code is UHotTextView.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHotTextView;


interface


uses
  // Delphi
  SysUtils, Windows, Graphics,
  // Project
  UHotTextLink, UHotTextParaStyle, ULists;


type

  {
  THotTextViewItem:
    Class that encapsulates a single item to be rendered on screen as part of a
    view of a hot text document. Objects of this class have a bounding rectangle
    that defines the area of the canvas where they are displayed and some text
    that is to be displayed in the rectangle. Various methods to update the
    bounds of the rectangle are provided. The object measures its own text and
    stores the extent of text.
  }
  THotTextViewItem = class(TObject)
  private // properties
    fText: string;
    fFont: TFont;
    fLink: THotTextLink;
    fDescent: Integer;
    fBoundsRect: TRect;
    function GetExtent: TSize;
  private
    fCanvas: TCanvas;
      {Canvas on which to make measurements and to output text}
    procedure MeasureDescent;
      {Measures descent for Font and records in Descent property}
    procedure MeasureText(const Txt: string); virtual;
      {Measures width and height of text in given Font: can be overridden by
      sub-classes}
    procedure SetBoundsRect(const R: TRect);
      {Sets bounds rectangle to given value: for use in sub-classes}
  public
    constructor Create(const Canvas: TCanvas; const Font: TFont;
      const Link: THotTextLink; const Text: string);
      {Class constructor: Records references to the given parameters which have
      following purpose:
        Canvas: Used to display text and take measurements. Reference to canvas
          stored, so user must ensure it exists during lifetime of this object.
          Exception if Canvas is nil.
        Font: Used to display text and for taking measurements. Reference to
          font is stored, so user must ensure it exists during lifetime of this
          object. Exception if Font is nil.
        Link: Reference to any link associated with this object. Reference to
          link object is stored so user must ensure it exists during lifetime of
          this object. May be nil.
        Text: The text displayed by the text item: may be ''}
    procedure OffsetTo(const X, Y: Integer);
      {Offset the bounding rectangle of the item to the given left,top co-
      ordinates}
    property Text: string read fText;
      {The text displayed by the text item: may be ''}
    property Font: TFont read fFont;
      {The font used to display and measure the item's text}
    property Link: THotTextLink read fLink;
      {Any hot link associated with the text item: nil if no associated link}
    property Descent: Integer read fDescent;
      {Descent of font: i.e. how far font's descenders are placed below
      baseline}
    property BoundsRect: TRect read fBoundsRect;
      {Bounding rectangle of text displayed in given font. Will be have TopLeft
      of (0,0) on creation and BottomRight will be (text width, text height)}
    property Extent: TSize read GetExtent;
      {Extent of the text displayed in the given font: .cx field is text width
      and .cy field is text height}
  end;


  {
  THotTextViewSpace:
    A special hot text view item that represents a single space. Space items are
    processed differently when building the view than are basic text items.
  }
  THotTextViewSpace = class(THotTextViewItem)
  public
    constructor Create(const Canvas: TCanvas; const Font: TFont;
      const Link: THotTextLink);
      {Class constructor: Records references to the given parameters which have
      following purpose:
        Canvas: Used to display text and take measurements. Reference to canvas
          stored, so user must ensure it exists during lifetime of this object.
          Exception if Canvas is nil.
        Font: Used to display text and for taking measurements. Reference to
          font is stored, so user must ensure it exists during lifetime of this
          object. Exception if Font is nil.
        Link: Reference to any link associated with this object. Reference to
          link object is stored so user must ensure it exists during lifetime of
          this object. May be nil.
      The Text property is set to a single space (#32)}
  end;


  {
  THotTextViewTab:
    A special hot text view item that represents a single tab. Tabs are
    processed differently when building a view than basic text items. They also
    have no text and their width represents the width of the "space" before the
    next tab stop rather than the width of text. While the extent of other text
    items is immutable, the width of a tab item can be altered externally.
  }
  THotTextViewTab = class(THotTextViewItem)
  protected
    procedure MeasureText(const Txt: string); override;
      {Measure a space character to get height of area (measuring '' gives no
      height. We set width back to zero since width is set externally based on
      tab stops when rendering a line}
  public
    constructor Create(const Canvas: TCanvas; const Font: TFont;
      const Link: THotTextLink);
      {Class constructor: Records references to the given parameters which have
      following purpose:
        Canvas: Used to display text and take measurements. Reference to canvas
          stored, so user must ensure it exists during lifetime of this object.
          Exception if Canvas is nil.
        Font: Used to display text and for taking measurements. Reference to
          font is stored, so user must ensure it exists during lifetime of this
          object. Exception if Font is nil.
        Link: Reference to any link associated with this object. Reference to
          link object is stored so user must ensure it exists during lifetime of
          this object. May be nil.
      The Text property is set to ''}
    procedure SetWidth(const NewWidth: Integer);
      {Method to enable width of tab stop to be set externally: width of tab
      stop is not known when object is created}
  end;


  {
  THotTextLinkRef:
    This class represents a single logical link within a hot text document. A
    logical link can encompass one or more display items: for example when a
    link has more than one font or spans a line end. The class provides a method
    for checking wether a point lies within any of the bounding rectangles of
    the items that make up the link. It is used internally by THotTextView.
  }
  THotTextLinkRef = class(TObject)
  private // properties
    fEndIdx: Integer;
    fStartIdx: Integer;
  private
    fItemListRef: TBasicObjList;
      {Reference to list of document view list containing items that are part of
      link: passed as parameter to constructor}
  public
    constructor Create(const ItemListRef: TBasicObjList;
      const StartIdx, EndIdx: Integer);
      {Class constructor: records details of given parameters:
        ItemListRef: reference to a list of document view items - must not be
          nil.
        StartIdx: index in ItemListRef of first doc view item in link - must lie
          within list and be <= EndIdx.
        EndIdx: index in ItemListRef of last doc view item in link - must lie
          within list and be >= StartIdx}
    function PtInLink(const Pt: TPoint): Integer;
      {Scans through list of document view items contained in this link checking
      if the given point lies in any of the bounding rectangle of the items.
      Returns index into the item list of any item that does contain the point
      or -1 if there is no matching item}
    property StartIdx: Integer read fStartIdx;
      {Index in document view's item list of first view item in the link}
    property EndIdx: Integer read fEndIdx;
      {Index in document view's item list of last view item in the link}
  end;


  {
  THotTextView:
    This class is used to render a hot text document for a particular canvas and
    to paint it onto the canvas. Its methods are used to construct a view by
    adding various items of text and formatting commands. The object builds a
    list of individual text, space and tab items each with defined display areas
    within the document.
  }
  THotTextView = class(TObject)
  private // properties
    fViewWidth: Integer;
    fViewHeight: Integer;
    fMargin: Integer;
    fItems: TBasicObjList;
    fLinks: TBasicObjList;
    function GetItem(Idx: Integer): THotTextViewItem;
    function GetItemCount: Integer;
    function GetLink(Idx: Integer): THotTextLinkRef;
    function GetLinkCount: Integer;
  private
    // miscellaneous field
    fCanvas: TCanvas;
      {Canvas used to render the document view}
    fCurFont: TFont;
      {Reference to current font: object owned externally}
    fCurParaStyle: THotTextParaStyle;
      {Current paragraph style - nil if no style}
    // fields relating to current word
    fWordStartIdx: Integer;
      {Index in item list of first view item in current word. Word always ends
      at end of item list. fWordStartIdx = ItemCount when there is no current
      word}
    fCurWordWidth: Integer;
      {Width of current word: 0 if no current word or if word is empty}
    // fields relating to current line
    fCurLineExtent: TSize;
      {Stores current width and height of current line}
    fCurLineMaxDescent: Integer;
      {The largest font descent in current line: used to calculate text item
      vertical offsets so that font baselines align}
    fIsFirstLineInPara: Boolean;
      {Flag true if current line is first in paragraph}
    fLineStartIdx: Integer;
      {Index in item lost of first view item in current line. Line ends just
      before fWordStartIdx. fLineStartIdx = fWordStartIdx when there is no
      current line}
    fLineVOffset: Integer;
      {Vertical offset in canvas of current line: always updated to give offset
      of next line when each line is ended. Starting value of offset is the
      top margin of the view - see Margin property}
    fMaxLineWidth: Integer;
      {Maximum width of current line adjusted to allow for any indents}
    // link related fields
    fCurLink: THotTextLink;
      {Reference to current link: object owned externally}
    fLinkStartIdx: Integer;
      {Index in item list of first view item in current link. Only valid when
      fCurLink is non nil. A link ends at the last item in the view item list
      when fCurLink is set to new value}
    procedure EndCurrentWord;
      {Ends current word and adds to new line if it fits. If the word did not
      fit on the line a new line is started and the word is added to that. A
      word is added to the line by adding each view item in the word to the
      line}
    procedure EndCurrentLine;
      {Ends the current line and aligns text items within it. The vertical
      position of the items is set so items are aligned by their font baselines
      at correct vertical position of line in view. Horizontal item positions
      are updated to place the item correctly in line according to the line
      offsets and alignment and the item's position in the line. The starting
      position of the next line is set}
    procedure ForceEndOfLine;
      {Forces current line to be ended. This also closes any current word before
      ending the line}
    procedure AddPartialWord(const Text: string);
      {Creates a new view item containing the text and adds the item to the
      view's item list as part of the current word. The record of the current
      word's width is updated by the width of the word. The text should not
      include any space characters. A font should have been entered in the view
      before adding text. If the text ends with a word break character, we end
      the current word}
    procedure AddSpace;
      {Creates a new view item containing a single space and adds the item to
      the view's item list. Adding a space ends any current word. If the spaces
      fits on the current line it is added to the end of the line, otherwise a
      new line is started and the space is added to it. A font must have been
      added to the view before a space is added}
    procedure AddTab;
      {Creates a new view item containing a tab and adds the item to the view's
      item list. Adding a tab ends any current word. The tab's width is set to
      next available tab positions. If tab now fits of current line it is added
      to it, otherwise a new line is started and the tab is ignored. A font must
      have been added to the view before a tab is added}
    procedure AddItemToLine(const Item: THotTextViewItem);
      {Update details of current line with properties of the given item which is
      being added to the line}
  public
    constructor Create(const Canvas: TCanvas; const ViewWidth, ViewHeight,
      Margin: Integer);
      {Class constructor: sets up the view object according the parameters as
      follows:
        Canvas: the canvas used to render and paint the view
        ViewWidth: the width of the view within the margins - i.e. the width of
          the area where text is displayed.
        ViewHeight: the height of the view within margins.
        Margin: the margin around the view - i.e. the offset from the edges of
          the canvas of the edges of the view.
      Also creates owned list and items objects and sets default values of
      properties and other fields}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure Reset(const ViewWidth, ViewHeight, Margin: Integer);
      {Resets the view ready for rendering a hot text document. The view has
      size and margins per the parameters as follows:
        ViewWidth: the width of the view within the margins - i.e. the width of
          the area where text is displayed.
        ViewHeight: the height of the view within margins.
        Margin: the margin around the view - i.e. the offset from the edges of
          the canvas of the edges of the view.
      The items and links list are cleared and fields set to default values}
    procedure AddText(const Text: string);
      {Adds given text to the rendered view a word, space or tab at a time}
    procedure UpdateFont(const Font: TFont);
      {Updates the font to be used for subsequent text items to the given value:
      we store a reference to the given font object which should persist for the
      life of this view (i.e. until reset or freed)}
    procedure NewPara(const PS: THotTextParaStyle);
      {Begins a new paragraph with the given paragraph style. Ends any current
      word and line and updates line attributes for first line in paragraph}
    procedure UpdateLink(const Link: THotTextLink);
      {Updates the current link. Any current link is first closed and
      information about it stored in the links list. Then the start of any new
      link is record. To close an open link without opening a new one, pass nil
      to the method}
    procedure EndPage;
      {Ends the page and hence closes the view object. We end any current words
      and lines and close any open links}
    procedure Paint(const Enabled: Boolean);
      {Paints the rendering document on the canvas. If enabled is false the text
      is displayed in an "engraved" disabled style that ignores font colours}
    property ViewWidth: Integer read fViewWidth default 0;
      {Width of the view to be rendered within any margins}
    property ViewHeight: Integer read fViewHeight default 0;
      {Height of the view to be rendered within any margins}
    property Margin: Integer read fMargin default 0;
      {Size of margin that surrounds the view}
    property Items[Idx: Integer]: THotTextViewItem read GetItem; default;
      {List of displayable items within the view}
    property ItemCount: Integer read GetItemCount;
      {Number of items in the view}
    property Links[Idx: Integer]: THotTextLinkRef read GetLink;
      {List of logical links within the view: each logical link can encompass
      one or more displayable items}
    property LinkCount: Integer read GetLinkCount;
      {Number of logical links within the view}
  end;


  {
  EHotTextView:
    Class of exception raised by errors in THotTextView and its supporting
    classes.
  }
  EHotTextView = class(Exception);


implementation


uses
  // Delphi
  Classes;


{ Constants required by classes }

const
  cSpace = #32;       // the space character
  cTab = #9;          // the tab character
  cWordBreakChars =   // set of non-space characters that can break words
    ['!', '%', '&', '*', ')', '-', '+', '=', '}', ']', ':', ';',
     ',', '.', '?', '/', '|', '\'];

resourcestring
  // Error messages
  sMustSetFontBeforeText = 'Can''t add text before setting font';
  sMustSetFontBeforeSpace = 'Can''t add a space before setting font';
  sMustSetFontBeforeTab = 'Can''t add a tab before setting font';
  sMustSetStyleBeforeTab = 'Can''t add a tab before setting paragraph style';
  sMustSetStyleBeforeLine
    = 'Can''t update a line before setting paragraph style';

{ THotTextViewItem }

constructor THotTextViewItem.Create(const Canvas: TCanvas;
  const Font: TFont; const Link: THotTextLink; const Text: string);
  {Class constructor: Records references to the given parameters which have
  following purpose:
    Canvas: Used to display text and take measurements. Reference to canvas
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Canvas is nil.
    Font: Used to display text and for taking measurements. Reference to font is
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Font is nil.
    Link: Reference to any link associated with this object. Reference to link
      object is stored so user must ensure it exists during lifetime of this
      object. May be nil.
    Text: The text displayed by the text item: may be ''}
begin
  // Preconditons
  Assert(
    Assigned(Canvas), 'THotTextViewItem.Create: Canvas parameter not assigned'
  );
  Assert(
    Assigned(Font), 'THotTextViewItem.Create: Font parameter not assigned'
  );
  inherited Create;
  // Record references to params
  fCanvas := Canvas;
  fFont := Font;
  fLink := Link;
  fText := Text;
  // Set default descent: use this since not a valid descent value (>=0)
  fDescent := -1;
  // Measure descent and text
  MeasureDescent;
  MeasureText(Text);
end;

function THotTextViewItem.GetExtent: TSize;
  {Getter for Extent property: extracts required values from BoundsRect
  property}
begin
  Result.cx := fBoundsRect.Right - fBoundsRect.Left;
  Result.cy := fBoundsRect.Bottom - fBoundsRect.Top;
end;

procedure THotTextViewItem.MeasureDescent;
  {Measures descent for Font and records in Descent property}
var
  TM: TTextMetric;  // structure to store test metrics, inc font descent
begin
  // Assign font to canvas
  Assert(Assigned(Font));
  fCanvas.Font.Assign(Font);
  // Get metrics for font and record it
  Windows.GetTextMetrics(fCanvas.Handle, TM); // Windows API
  fDescent := TM.tmDescent;
end;

procedure THotTextViewItem.MeasureText(const Txt: string);
  {Measures width and height of text in given Font: can be overridden by sub-
  classes}
begin
  // Assign font to canvas
  Assert(Assigned(fFont));
  fCanvas.Font.Assign(Font);
  // Set bounding rectangle empty: Windows will fill this in
  SetRectEmpty(fBoundsRect);
  // Get space required to draw text: we use this API rather than
  // Canvas.TextExtent since that call can return spurous results
  Windows.DrawText(   // Windows API
    fCanvas.Handle,
    PChar(Txt),
    Length(Txt),
    fBoundsRect,
    DT_CALCRECT + DT_LEFT + DT_SINGLELINE
  );
end;

procedure THotTextViewItem.OffsetTo(const X, Y: Integer);
  {Offset the bounding rectangle of the item to the given left,top co-ordinates}
begin
  OffsetRect(fBoundsRect, X - fBoundsRect.Left, Y - fBoundsRect.Top);
end;

procedure THotTextViewItem.SetBoundsRect(const R: TRect);
  {Sets bounds rectangle to given value: for use in sub-classes}
begin
  fBoundsRect := R;
end;


{ THotTextViewSpace }

constructor THotTextViewSpace.Create(const Canvas: TCanvas;
  const Font: TFont; const Link: THotTextLink);
  {Class constructor: Records references to the given parameters which have
  following purpose:
    Canvas: Used to display text and take measurements. Reference to canvas
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Canvas is nil.
    Font: Used to display text and for taking measurements. Reference to font is
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Font is nil.
    Link: Reference to any link associated with this object. Reference to link
      object is stored so user must ensure it exists during lifetime of this
      object. May be nil.
  The Text property is set to a single space (#32)}
begin
  // Create text item with Text=space
  inherited Create(Canvas, Font, Link, ' ');
end;


{ THotTextViewTab }

constructor THotTextViewTab.Create(const Canvas: TCanvas;
  const Font: TFont; const Link: THotTextLink);
  {Class constructor: Records references to the given parameters which have
  following purpose:
    Canvas: Used to display text and take measurements. Reference to canvas
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Canvas is nil.
    Font: Used to display text and for taking measurements. Reference to font is
      stored, so user must ensure it exists during lifetime of this object.
      Exception if Font is nil.
    Link: Reference to any link associated with this object. Reference to link
      object is stored so user must ensure it exists during lifetime of this
      object. May be nil.
  The Text property is set to ''}
begin
  // Create text item with text = ''
  inherited Create(Canvas, Font, Link, '');
  // Note overridden MeasureText method gets height required but sets width to 0
end;

procedure THotTextViewTab.MeasureText(const Txt: string);
  {Measure a space character to get height of area (measuring '' gives no
  height. We set width back to zero since width is set externally based on tab
  stops when rendering a line}
begin
  inherited MeasureText(' ');
  SetWidth(0);
end;

procedure THotTextViewTab.SetWidth(const NewWidth: Integer);
  {Method to enable width of tab stop to be set externally: width of tab stop is
  not known when object is created}
var
  R: TRect; // copy of bounds rectangle used for updating
begin
  // Get copy of bounding rectangle and update it with new width
  R := BoundsRect;
  R.Right := R.Left + NewWidth;
  // Store new value in BoundsRect property
  SetBoundsRect(R);
end;


{ THotTextLinkRef }

constructor THotTextLinkRef.Create(const ItemListRef: TBasicObjList;
  const StartIdx, EndIdx: Integer);
  {Class constructor: records details of given parameters:
    ItemListRef: reference to a list of document view items - must not be nil.
    StartIdx: index in ItemListRef of first doc view item in link - must lie
      within list and be <= EndIdx.
    EndIdx: index in ItemListRef of last doc view item in link - must lie within
      list and be >= StartIdx}
begin
  // Preconditions
  Assert(Assigned(ItemListRef));
  Assert(EndIdx >= StartIdx);
  Assert((StartIdx >= 0) and (EndIdx < ItemListRef.Count));
  inherited Create;
  // Record parameters
  fItemListRef := ItemListRef;
  fStartIdx := StartIdx;
  fEndIdx := EndIdx;
end;

function THotTextLinkRef.PtInLink(const Pt: TPoint): Integer;
  {Scans through list of document view items contained in this link checking if
  the given point lies in any of the bounding rectangle of the items. Returns
  index into the item list of any item that does contain the point or -1 if
  there is no matching item}
var
  Idx: Integer;           // loops thru doc view list for items in link
  Item: THotTextViewItem; // reference to an item in doc view list
begin
  // Assume failure
  Result := -1;
  // Scan thru all doc view items that are part of link
  for Idx := fStartIdx to fEndIdx do
  begin
    // get reference to current item
    Item := fItemListRef[Idx] as THotTextViewItem;
    // check if current item contains required point: return Idx if found
    if Windows.PtInRect(Item.BoundsRect, Pt) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;


{ THotTextView }

procedure THotTextView.AddItemToLine(const Item: THotTextViewItem);
  {Update details of current line with properties of the given item which is
  being added to the line}
begin
  // Increase length of line by length of item
  Inc(fCurLineExtent.cx, Item.Extent.cx);
  // Increase height of line if item is higher than current line
  if (fCurLineExtent.cy < Item.Extent.cy) then
    fCurLineExtent.cy := Item.Extent.cy;
  // Increase max font descent in line if item has greater descent than line
  if (fCurLineMaxDescent < Item.Descent) then
    fCurLineMaxDescent := Item.Descent;
end;

procedure THotTextView.AddPartialWord(const Text: string);
  {Creates a new view item containing the text and adds the item to the
  view's item list as part of the current word. The record of the current word's
  width is updated by the width of the word. The text should not include any
  space characters. A font should have been entered in the view before adding
  text. If the text ends with a word break character, we end the current word}
var
  Item: THotTextViewItem; // the new doc view item containing the text
begin
  Assert(Text <> '');
  // Check we have a font
  if (fCurFont = nil) then
    raise EHotTextView.Create(sMustSetFontBeforeText);
  // Create new text item and add to item list
  Item := THotTextViewItem.Create(fCanvas, fCurFont, fCurLink, Text);
  fItems.Add(Item);
  // Increase width of current word by width of texts
  Inc(fCurWordWidth, Item.Extent.cx);
  // Check if text ends with a word break character and end word if so
  if Text[Length(Text)] in cWordBreakChars then
    EndCurrentWord;
end;

procedure THotTextView.AddSpace;
  {Creates a new view item containing a single space and adds the item to the
  view's item list. Adding a space ends any current word. If the spaces fits on
  the current line it is added to the end of the line, otherwise a new line is
  started and the space is added to it. A font must have been added to the view
  before a space is added}
var
  Space: THotTextViewSpace; // the new doc view item containing the text
begin
  // Check we have a font
  if (fCurFont = nil) then
    raise EHotTextView.Create(sMustSetFontBeforeSpace);
  // Create a new space item and add to item list
  Space := THotTextViewSpace.Create(fCanvas, fCurFont, fCurLink);
  // End any current word
  if fWordStartIdx < ItemCount then
    EndCurrentWord;
  // Check if space fits on line or is just past its end. If not start new line
  //    Note: one space may be placed immediately beyond end of line since
  //    single trailing spaces following words are ignored at word breaks.
  if fCurLineExtent.cx > fMaxLineWidth then
    // current line is already beyond line end before space added => there is
    // already space beyond line end or line has single word longer than line
    EndCurrentLine;
  // Add space to item list
  fItems.Add(Space);
  // Update line to accomodate space
  AddItemToLine(Space);
  // Set start index of next word to end of list: ensures word end stays in sync
  fWordStartIdx := ItemCount;
end;

procedure THotTextView.AddTab;
  {Creates a new view item containing a tab and adds the item to the view's
  item list. Adding a tab ends any current word. The tab's width is set to next
  available tab positions. If tab now fits of current line it is added to it,
  otherwise a new line is started and the tab is ignored. A font must have been
  added to the view before a tab is added}
var
  TabItem: THotTextViewTab; // new view item storing details of the tab
  TabItemWidth: Integer;    // required width of the new tab item
  LineXOffset: Integer;     // absolute offset of current line from margin
  TabPos: Integer;          // position of tab relative to start of line
  AbsLinePos: Integer;      // absolute position of end of line from margin
begin
  // Check we have a font and a paragraph style
  if (fCurFont = nil) then
    raise EHotTextView.Create(sMustSetFontBeforeTab);
  // We must have a paragraph style assigned in order to calculate line offsets
  if not Assigned(fCurParaStyle) then
    raise EHotTextView.Create(sMustSetStyleBeforeTab);
  // End any current word
  EndCurrentWord;
  // Calculate the position of the next tab stop
  // tab stops are relative to whole line, including any indents, so first we
  // must work out the left offset of the start of the line: this is adjusted
  // for 1st line by first line index, otherwise offset is the left indent
  if fIsFirstLineInPara then
    LineXOffset := fCurParaStyle.IndentLeft + fCurParaStyle.IndentFirst
  else
    LineXOffset := fCurParaStyle.IndentLeft;
  // the absolute current position in the line is the current line extent +
  // the offset
  AbsLinePos := fCurLineExtent.cx + LineXOffset;
  // we can now get the absolute tab position, which we make relative to the
  // line by deducting the offset
  TabPos := fCurParaStyle.Tabs.GetTabAfter(AbsLinePos) - LineXOffset;
  Assert(TabPos > 0);
  // Create a tab list item
  TabItem := THotTextViewTab.Create(fCanvas, fCurFont, fCurLink);
  if TabPos > fMaxLineWidth then
  begin
    // Tab doesn't fit on line: need a new line
    // before beginning new line we store a zero width tab at end of current
    // line so that its font is stored and current line height can be adjusted
    // if necessary
    TabItem.SetWidth(0);
    fItems.Add(TabItem);
    AddItemToLine(TabItem);
    // we now start a new line
    EndCurrentLine;
  end
  else
  begin
    // Tab fits on line: set its width to take us to tab stop
    TabItemWidth := TabPos - (fCurLineExtent.cx);
    TabItem.SetWidth(TabItemWidth);
    // now add tab item to view list and to line
    fItems.Add(TabItem);
    AddItemToLine(TabItem);
  end;
  // Set start index of next word to end of list: ensures word end stays in sync
  fWordStartIdx := ItemCount;
end;

procedure THotTextView.AddText(const Text: string);
  {Adds given text to the rendered view a word, space or tab at a time}

  // ---------------------------------------------------------------------------

  function IsWhiteSpace(const Ch: Char): Boolean;
    {Returns true if given character is white space character}
  begin
    Result := Ch in [cSpace, cTab];
  end;

  function IsWordBreakChar(const Ch: Char): Boolean;
    {Returns true if given character is word-breaking character}
  begin
    Result := Ch in cWordBreakChars;
  end;

  function GetNextTextItem(const Text: string; var EndPos: Integer;
    out TextItem: string): Boolean;
    {Gets next text item (word or space) from Text and passes out thru TextItem
    parameter, updating cursor into string ready for next item. Returns True
    when an item has successfully been read and false (with TextItem undefined)
    when no more items in string. Words are delimited by spaces or word break
    punctuation characters}
  var
    StartPos: Integer;  // starting position of next item in string
  begin
    // Check if we have any more text and fetch next item if so
    Result := EndPos <= Length(Text);
    if Result then
    begin
      if not IsWhiteSpace(Text[EndPos]) then
      begin
        // We have start of a word: find end and copy word to output
        // first scan through chars looking for end of line or terminating char
        StartPos := EndPos;
        while (EndPos <= Length(Text)) and not IsWhiteSpace(Text[EndPos])
          and not IsWordBreakChar(Text[EndPos]) do
          Inc(EndPos);
        // if word stopped at word break char we want to include it word
        if (EndPos <= Length(Text)) and IsWordBreakChar(Text[EndPos]) then
          Inc(EndPos);
        // now copy word or part word to output
        TextItem := Copy(Text, StartPos, EndPos - StartPos);
      end
      else
      begin
        // We have space: which is either space or tab since parser only emits
        // white space (#32) or tab (#9)
        TextItem := Text[EndPos];
        Inc(EndPos);
      end;
    end;
  end;

  // ---------------------------------------------------------------------------

var
  TextPos: Integer;   // position of next char in text to process
  TextItem: string;   // a text item from text string
begin
  // Fetch each space or word from Text
  TextPos := 1;
  while GetNextTextItem(Text, TextPos, TextItem) do
  begin
    // Add either a space, a tab or a word to the view object
    if TextItem = cSpace then
      AddSpace
    else if TextItem = cTab then
      AddTab
    else
      AddPartialWord(TextItem);
  end;
end;

constructor THotTextView.Create(const Canvas: TCanvas;
  const ViewWidth, ViewHeight, Margin: Integer);
  {Class constructor: sets up the view object according the parameters as
  follows:
    Canvas: the canvas used to render and paint the view
    ViewWidth: the width of the view within the margins - i.e. the width of the
      area where text is displayed.
    ViewHeight: the height of the view within margins.
    Margin: the margin around the view - i.e. the offset from the edges of the
      canvas of the edges of the view.
  Also creates owned list and items objects and sets default values of
  properties and other fields}
begin
  inherited Create;
  // Record reference to canvas object
  fCanvas := Canvas;
  // Create owned items and link object
  fItems := TBasicObjList.Create(True);
  fLinks := TBasicObjList.Create(True);
  // Set default fields and property values
  Reset(ViewWidth, ViewHeight, Margin);
end;

destructor THotTextView.Destroy;
  {Class destructor: frees owned objects}
begin
  fLinks.Free;
  fItems.Free;
  inherited;
end;

procedure THotTextView.EndCurrentLine;
  {Ends the current line and aligns text items within it. The vertical position
  of the items is set so items are aligned by their font baselines at correct
  vertical position of line in view. Horizontal item positions are updated to
  place the item correctly in line according to the line offsets and alignment
  and the item's position in the line. The starting position of the next line is
  set}
var
  XOffset: Integer;           // offset of line start relative to left of canvas
  YOffset: Integer;           // offset of line top relative to top of canvas
  Item: THotTextViewItem;     // reference to each item in line
  ItemRect: TRect;            // bounds rectangle of an item in line
  LineDisplayWidth: Integer;  // width of line excluding any trailing spaces
begin
  // Exit if line is empty: nothing to do
  if fLineStartIdx = fWordStartIdx then
    Exit;
  // We must have a paragraph style assigned in order to calculate line offsets
  if not Assigned(fCurParaStyle) then
    raise EHotTextView.Create(sMustSetStyleBeforeLine);
  // Calculate size of line being displayed: reduce line width to ignore
  // trailing space
  LineDisplayWidth := fCurLineExtent.cx;
  if fLineStartIdx < fWordStartIdx then
  begin
    Item := GetItem(Pred(fWordStartIdx));
    if Item is THotTextViewSpace then
      Dec(LineDisplayWidth, Item.Extent.cx);
  end;
  // Adjust left offset relative to margin per display type
  case fCurParaStyle.Alignment of
    taLeftJustify:
    begin
      // left justified: offset varies according to if we have 1st line or not
      if fIsFirstLineInPara then
        // first line: offset left indent by first line indent
        XOffset := fCurParaStyle.IndentLeft + fCurParaStyle.IndentFirst
      else
        // 2nd and later line: just use left indent
        XOffset := fCurParaStyle.IndentLeft;
    end;
    taCenter:
    begin
      // centre justified: first set offset per left alignment ...
      if fIsFirstLineInPara then
        XOffset := fCurParaStyle.IndentLeft + fCurParaStyle.IndentFirst
      else
        XOffset := fCurParaStyle.IndentLeft;
      // ... and then centre line in display relative to left and right offsets
      XOffset := XOffset +
        (fViewWidth - XOffset - fCurParaStyle.IndentRight
          - LineDisplayWidth) div 2;
    end;
    taRightJustify:
    begin
      // right justified: just align relative to right indent
      XOffset := fViewWidth - LineDisplayWidth - fCurParaStyle.IndentRight;
    end;
    else
      XOffset := 0; // keep compiler happy
  end;
  // Adjust XOffset to allow for margin: makes the XOffset absolute in canvas
  Inc(XOffset, fMargin);
  // Scan thru line adjusting each item in line
  while fLineStartIdx < fWordStartIdx do
  begin
    // record item and its bounding rectangle
    Item := GetItem(fLineStartIdx);
    ItemRect := Item.BoundsRect;
    // calculate vertical alignment: we align to bottom of line then adjust for
    // font descents to align items by baseline of fonts
    YOffset := fCurLineExtent.cy - (ItemRect.Bottom - ItemRect.Top)
      - (fCurLineMaxDescent - Item.Descent);
    // offset the item to required position: vertical position calculated above
    // is increased by vertical offset of top of current line
    Item.OffsetTo(XOffset, fLineVOffset + YOffset);
    // update X offset to end of item just processed
    Inc(XOffset, Item.Extent.cx);
    // move to next item
    Inc(fLineStartIdx);
  end;
  // Update line's fields for next line
  // vertical offset of top of next line is bottom of current line
  Inc(fLineVOffset, fCurLineExtent.cy);
  // reset line extent and maximum font descent to 0
  fCurLineExtent.cx := 0;
  fCurLineExtent.cy := 0;
  fCurLineMaxDescent := 0;
  // since we've just added a line, next line cannot be first (we only have new
  // line when new paragraph started)
  fIsFirstLineInPara := False;
  // set mx width of next line using view width and left and right indents (we
  // ignore first line indent since not first line)
  fMaxLineWidth := fViewWidth - fCurParaStyle.IndentLeft
    - fCurParaStyle.IndentRight;
end;

procedure THotTextView.EndCurrentWord;
  {Ends current word and adds to new line if it fits. If the word did not fit on
  the line a new line is started and the word is added to that. A word is added
  to the line by adding each view item in the word to the line}
var
  Item: THotTextViewItem; // reference to each item in the word
begin
  // If we have no current word there is nothing to do
  if fWordStartIdx >= ItemCount then
    Exit;
  // Check if word fits on line and start new line if not
  // a word fits on line if its width fits in remaining space on line OR if line
  // is empty a word of any length can be added - long words are no broken
  if (fCurLineExtent.cx + fCurWordWidth > fMaxLineWidth)
    and (fCurLineExtent.cx > 0) then
    EndCurrentLine;
  // Loop thru items in word addign to line
  while fWordStartIdx < ItemCount do
  begin
    Item := GetItem(fWordStartIdx);
    AddItemToLine(Item);
    Inc(fWordStartIdx);
  end;
  // Prepare for next word: it has zero length
  fCurWordWidth := 0;
end;

procedure THotTextView.EndPage;
  {Ends the page and hence closes the view object. We end any current words
  and lines and close any open links}
begin
  ForceEndOfLine;   // closes current word and line
  UpdateLink(nil);  // closes any open link
end;

procedure THotTextView.ForceEndOfLine;
  {Forces current line to be ended. This also closes any current word before
  ending the line}
begin
  EndCurrentWord;
  EndCurrentLine;
end;

function THotTextView.GetItem(Idx: Integer): THotTextViewItem;
  {Getter method for Items object}
begin
  Result := fItems[Idx] as THotTextViewItem;
end;

function THotTextView.GetItemCount: Integer;
  {Getter methof for ItemCount object}
begin
  Result := fItems.Count;
end;

function THotTextView.GetLink(Idx: Integer): THotTextLinkRef;
  {Getter method for Links property}
begin
  Result := fLinks[Idx] as THotTextLinkRef;
end;

function THotTextView.GetLinkCount: Integer;
  {Getter method for LinkCount property}
begin
  Result := fLinks.Count;
end;

procedure THotTextView.NewPara(const PS: THotTextParaStyle);
  {Begins a new paragraph with the given paragraph style. Ends any current word
  and line and updates line attributes for first line in paragraph}
begin
  // End any current word and line
  ForceEndOfLine;
  // Record that we're first line in paragraph, setting maximum line width,
  // allowing for left, first line and right indents
  fIsFirstLineInPara := True;
  fMaxLineWidth := fViewWidth - (PS.IndentLeft + PS.IndentFirst)
    - PS.IndentRight;
  // Store the current paragraph style
  fCurParaStyle := PS;
end;

procedure THotTextView.Paint(const Enabled: Boolean);
  {Paints the rendering document on the canvas}
var
  Idx: Integer;           // loops thru all items in view
  Item: THotTextViewItem; // reference to an item in the view
  Rect: TRect;            // bounds rectangle of an item
const
  // flags used to draw text using Windows API
  cDrawFlags = DT_LEFT + DT_SINGLELINE;
begin
  // Use a clear brush
  fCanvas.Brush.Style := bsClear;
  // Scan thru all items in the view, painting each one
  for Idx := 0 to Pred(ItemCount) do
  begin
    // Get reference to current item: only paint it if it has text
    Item := GetItem(Idx);
    if Item.Text <> '' then
    begin
      // Set rectangle in which to draw the item:
      Rect := Item.BoundsRect;
      // some italic fonts display outside their measured bounds so we set right
      // of drawing rectangle to edge of canvas to avoid clipping such text
      Rect.Right := ViewWidth + fMargin;
      // if item will display beyond bottom margin we clip the text to display
      // within the view area
      if Rect.Bottom > ViewHeight + fMargin then
        Rect.Bottom := ViewHeight + fMargin;
      // Set canvas to have required font
      fCanvas.Font.Assign(Item.Font);
      // Now draw the text
      if Enabled then
        // Draw the text normally
        DrawText(
          fCanvas.Handle, PChar(Item.Text), Length(Item.Text), Rect, cDrawFlags
        )
      else
      begin
        // Draw text in "engraved" disabled style
        // first draw highlight style, down and right by 1 pixel
        OffsetRect(Rect, 1, 1);
        fCanvas.Font.Color := clBtnHighlight;
        DrawText(
          fCanvas.Handle, PChar(Item.Text), Length(Item.Text), Rect, cDrawFlags
        );
        // now draw shadow style in original position
        OffsetRect(Rect, -1, -1);
        fCanvas.Font.Color := clBtnShadow;
        DrawText(
          fCanvas.Handle, PChar(Item.Text), Length(Item.Text), Rect, cDrawFlags
        );
      end;
    end;
  end;
end;

procedure THotTextView.Reset(const ViewWidth, ViewHeight, Margin: Integer);
  {Resets the view ready for rendering a hot text document. The view has size
  and margins per the parameters as follows:
    ViewWidth: the width of the view within the margins - i.e. the width of the
      area where text is displayed.
    ViewHeight: the height of the view within margins.
    Margin: the margin around the view - i.e. the offset from the edges of the
      canvas of the edges of the view.
  The items and links list are cleared and fields set to default values}
begin
  // Record parameters
  fViewWidth := ViewWidth;
  fViewHeight := ViewHeight;
  fMargin := Margin;
  // Reset fields to default values and clear list objects
  // word related fields
  fWordStartIdx := 0;
  fCurWordWidth := 0;
  // line related fields
  fCurLineExtent.cx := 0;
  fCurLineExtent.cy := 0;
  fCurLineMaxDescent := 0;
  fLineStartIdx := 0;
  fLineVOffset := Margin;
  fIsFirstLineInPara := True;
  fMaxLineWidth := ViewWidth;
  // link related fields
  fLinkStartIdx := 0;
  fCurLink := nil;
  fLinks.Clear;
  // layout related fields
  fCurParaStyle := nil;
  fCurFont := nil;
  // list of items in view
  fItems.Clear;
end;

procedure THotTextView.UpdateFont(const Font: TFont);
  {Updates the font to be used for subsequent text items to the given value:
  we store a reference to the given font object which should persist for the
  life of this view (i.e. until reset or freed)}
begin
  fCurFont := Font;
end;

procedure THotTextView.UpdateLink(const Link: THotTextLink);
  {Updates the current link. Any current link is first closed and information
  about it stored in the links list. Then the start of any new link is record.
  To close an open link without opening a new one, pass nil to the method}
var
  LinkEndIdx: Integer;      // index of end of link in view items list
  LinkRef: THotTextLinkRef; // link reference object
begin
  // If link hasn't changed, we do nothing
  if Link = fCurLink then
    Exit;
  // If we have a current link, close it adding new item to link list that
  // contains details of all text items in the link
  if fCurLink <> nil then
  begin
    // end of link is last item in item list
    LinkEndIdx := Pred(ItemCount);
    // check we have 1 or more items in link: do nothing if link is empty
    if LinkEndIdx >= fLinkStartIdx then
    begin
      // creat a link reference object for link and add to link list
      LinkRef := THotTextLinkRef.Create(fItems, fLinkStartIdx, LinkEndIdx);
      fLinks.Add(LinkRef);
    end;
  end;
  // If new link is not nil, open it by recording its start index
  if Link <> nil then
    fLinkStartIdx := ItemCount;
  // Record new link
  fCurLink := Link;
end;


end.
