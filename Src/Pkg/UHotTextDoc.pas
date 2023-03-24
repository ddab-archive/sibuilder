{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UHotTextDoc.pas
  @COMMENTS                 Implements a main class that stores a description of
                            a hot text document stored as a sequential list of
                            actions that need to be taken to render a view the
                            document. Also implements a class that encapsulates
                            the various actions. Several several other
                            subsidiary classes are also implemented.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literals with resource strings.
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
 * The Original Code is UHotTextDoc.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHotTextDoc;


interface


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UFontEx, UHotTextParaStyle, UHotTextLink, ULists;


type

  {
  THotTextDocEntryKind:
    The kinds of items that can be stored in a hot text document. The
    information that can be read from an entry in the document depends on its
    kind.
  }
  THotTextDocEntryKind = (
    ikText,       // text doc item: content is text stored
    ikFont,       // font doc item: content is reference to a font stored
    ikNewPara,    // new paragraph doc item: no content
    ikLinkStart,  // start of link: content is an owned link object
    ikLinkEnd     // end of link: no content
  );


  {
  THotTextDocEntry:
    Class encapsulates a single entry in a hot text document (see THotTextDoc
    below). Each entry describes a part of the document to be rendered. It has
    differing contents depending on the Kind property. This class checks that
    the correct properties are being accessed per the Kind property and raises
    exceptions if accessed in error. Different class methods are used to
    construct instances of this class for each kind of entry. The inherited
    constructor should not be used and raises an exception if accessed.
  }
  THotTextDocEntry = class(TObject)
  private // properties
    fKind: THotTextDocEntryKind;
    fFont: TFont;
    fLink: THotTextLink;
    fText: string;
    fParaStyle: THotTextParaStyle;
    function GetFont: TFont;
    function GetLink: THotTextLink;
    function GetText: string;
    function GetParaStyle: THotTextParaStyle;
  private
    procedure CheckValidKind(const PropName: string;
      const WantedKind: THotTextDocEntryKind);
      {Checks the WantedKind is valid for this entry instance and raises an
      exception if this is not the case. This method is called by each of the
      Kind-specific property access methods. The name of the property requesting
      access is passed as the PropName parameter and is used in any error
      message}
  protected
    constructor CreateEx(const Kind: THotTextDocEntryKind);
      {Class constructor used internally by the class "constructor" functions to
      construct an instance of the required Kind}
  public
    constructor Create;
      {Standard class constructor: should not be called by users of the class:
      raises exception}
    destructor Destroy; override;
      {Class destructor: frees owned Link object}
    class function CreateFont(const Font: TFont): THotTextDocEntry;
      {Class function used to construct a Font entry: the given font is made
      available via the Font property. Note that the font is not copied: it is a
      reference to an external instance that should be maintained by the user
      thru the life of this entry instance}
    class function CreateLinkEnd: THotTextDocEntry;
      {Class function used to construct a LinkEnd entry: there are no valid
      properties for this entry kind}
    class function CreateLinkStart(const Cmd, Hint: string): THotTextDocEntry;
      {Class function used to construct a LinkStart entry: an internally owned
      link object is created for the given link Cmd and Hint. A reference to
      this link object is made available via the Link property. The link object
      is freed when this entry instance is freed}
    class function CreateNewPara(const PS: THotTextParaStyle): THotTextDocEntry;
      {Class function used to construct a NewPara entry: the given paragraph
      style object is made available via the ParaStyle property. Note that the
      paragraph style is not copied: it is a reference to an external instance
      that should be maintained by the user thru the life of this entry
      instance}
    class function CreateText(const Text: string): THotTextDocEntry;
      {Class function used to construct a Text entry: the text is made available
      via the Text property. A copy of the text is stored internally}
    property Kind: THotTextDocEntryKind read fKind;
      {The kind of entry this is: this value indicates the action to be taken
      when processing this document entry. The property also indicates which
      other properties can be read for this entry as follows:
        ikText        : Text property is valid
        ikFont        : Font property is valid
        ikNewPara,    : ParaStyle property is valid
        ikLinkStart   : Link property is valid
        ikLinkEnd     : no properties are valid}
    property Font: TFont read GetFont;
      {Returns reference to a font maintained externally to this object. Reading
      Font raises an exception if Kind <> ikFont}
    property Link: THotTextLink read GetLink;
      {Returns reference to a font maintained internally. Reading Link raises an
      exception if Kind <> ikLinkStart}
    property Text: string read GetText;
      {Returns the text for this entry. Reading Text raises an exception if
      Kind <> ikText}
    property ParaStyle: THotTextParaStyle read GetParaStyle;
      {Returns reference to a paragraph style maintained externally to this
      object. Reading ParaStyle raises an exception if Kind <> ikLinkStart}
  end;


  {
  THotTextFontCache:
    Class maintains a cache of font objects where each entry has unique
    properties - i.e. the cache contains no duplicates. It is used to maintain a
    list of font instances that are referenced from a hot text document - see
    THotTextDoc below. The font instances are owned by the cache and are freed
    when this object is freed.
  }
  THotTextFontCache = class(TObject)
  private // properties
    fItems: TBasicObjList;
    function GetCount: Integer;
    function GetItem(Idx: Integer): TFontEx;
  public
    constructor Create;
      {Class constructor: creates cache object}
    destructor Destroy; override;
      {Class destructor: frees cache and all font objects in it}
    procedure Clear;
      {Clears the cache and frees the font instances stored in it}
    function Lookup(const Font: TFont): TFontEx;
      {Checks cache to see if there is an equivalent font in it and returns a
      reference to it if so. Otherwise a copy of the requested font is created
      and stored in the cache: it's reference is returned}
    property Items[Idx: Integer]: TFontEx read GetItem; default;
      {The list of fonts in the cache. Each font is owned by this object and
      they are freed when this object is freed}
    property Count: Integer read GetCount;
      {The number of fonts in the cache}
  end;


  {
  THotTextParaStyleCache:
    Class maintains a cache of paragraph style objects where each entry has
    unique properties - i.e. the cache contains no duplicates. It is used to
    maintain a list of paragraph style instances that are referenced from a hot
    text document - see THotTextDoc below. The paragraph style instances are
    owned by the cache and are freed when this object is freed.
  }
  THotTextParaStyleCache = class(TObject)
  private // properties
    fItems: TBasicObjList;
    function GetCount: Integer;
    function GetItem(Idx: Integer): THotTextParaStyle;
  public
    constructor Create;
      {Class constructor: creates cache object}
    destructor Destroy; override;
      {Class destructor: frees cache and all paragraph style objects in it}
    procedure Clear;
      {Clears the cache and frees the paragraph style instances stored in it}
    function Lookup(const PS: THotTextParaStyle): THotTextParaStyle;
      {Checks cache to see if there is an equivalent paragraph style in it and
      returns a reference to it if so. Otherwise a copy of the requested
      paragraph style is created and stored in the cache: it's reference is
      returned}
    property Items[Idx: Integer]: THotTextParaStyle read GetItem; default;
      {The list of paragraph style objects in the cache. The styles are owned by
      this object and they are freed when this object is freed}
    property Count: Integer read GetCount;
      {The number of paragraph style objects in the cache}
  end;


  {
  THotTextDoc:
    This class stores a description of a hot text document that can be used to
    render a view of the document. It maintains a list of actions to be taken to
    render the document. Each entry in the list is an instance of a
    THotTextDocEntry object that stores details of each action (see above). This
    class enables the document to be cleared or sequentially added to. Caches of
    font and paragraph styles added to the document are stored to reduce the
    number of objects required. These objects are owned by this object and are
    freed when this object is freed.
  }
  THotTextDoc = class(TObject)
  private // properties
    fEntries: TBasicObjList;
    fFontCache: THotTextFontCache;
    fParaStyleCache: THotTextParaStyleCache;
    function GetCount: Integer;
    function GetEntry(Idx: Integer): THotTextDocEntry;
  private
    function AddEntry(const Entry: THotTextDocEntry): Integer;
      {Adds the given entry to the document and returns its index in the Entries
      property}
  protected // property
    property FontCache: THotTextFontCache read fFontCache;
      {This property is made available so sub-classes can "see" the font cache,
      but specifically to cache visible to DUnit for testing purposes}
    property ParaStyleCache: THotTextParaStyleCache read fParaStyleCache;
      {This property is made available so sub-classes can "see" the paragraph
      style cache, but specifically to cache visible to DUnit for testing
      purposes}
  public
    constructor Create;
      {Class constructor: creates the entries list and font & paragraph style
      caches}
    destructor Destroy; override;
      {Class destructor: free owned objects}
    function AddFont(const Font: TFont): Integer;
      {Adds a font entry to the document which maintains its own copy of the
      font. The index of the new document entry is returned}
    function AddText(const Text: string): Integer;
      {Adds a new text entry for the given text to the document and returns the
      index of the new entry}
    function AddLinkStart(const Cmd, Hint: string): Integer;
      {Adds a new link start entry to the document and returns its index}
    function AddLinkEnd: Integer;
      {Adds a link end entry to the document and returns its index}
    function AddNewPara(const PS: THotTextParaStyle): Integer;
      {Adds a new paragraph entry to the document which maintains its own copy
      of the style object. The index of the new document entry is returned}
    procedure Clear;
      {Clears the document and font and paragraph style caches}
    property Entries[Idx: Integer]: THotTextDocEntry read GetEntry; default;
      {The list of entries in the document: this list is read in sequence by
      objects that render a view of the document}
    property Count: Integer read GetCount;
      {Number of entries in the document}
  end;


  {
  EHotTextDoc:
    Class of exception raised on errors detected in THotTextDoc and its
    supporting classes.
  }
  EHotTextDoc = class(Exception);


implementation


resourcestring
  // Doc entry kind description
  sText = 'Text';
  sFont = 'Font';
  sNewPara = 'NewParagraph';
  sLinkStart = 'Link Start';
  sLinkEnd = 'Link End';
  // Error messages
  sInvalidPropAccess
    = 'Document item %s property access is not valid for %s items';
  sBadConstructorCall = 'Can''t call THotTextDocEntry.Create directly';
  sDocMustHaveFont = 'Can''t create a document item with a nil font';
  sLinkMustHaveCmd
    = 'Can''t create a link document item with empty command string';
  sDocMustHaveParaStyle
    = 'Can''t create a document item with a nil paragraph style';

{ THotTextDocEntry }

procedure THotTextDocEntry.CheckValidKind(const PropName: string;
  const WantedKind: THotTextDocEntryKind);
  {Checks the WantedKind is valid for this entry instance and raises an
  exception if this is not the case. This method is called by each of the Kind-
  specific property access methods}
const
  // Array of descriptions for each kind of entry
  cKindStr: array[THotTextDocEntryKind] of string = (
    sText,          // ikText,
    sFont,          // ikFont
    sNewPara,       // ikNewPara
    sLinkStart,     // ikLinkStart
    sLinkEnd        // ikLinkEnd
  );
begin
  if fKind <> WantedKind then
    raise EHotTextDoc.CreateFmt(sInvalidPropAccess,[PropName, cKindStr[fKind]]);
end;

constructor THotTextDocEntry.Create;
  {Standard class constructor: should not be called by users of the class:
  raises exception}
begin
  raise EHotTextDoc.Create(sBadConstructorCall);
end;

constructor THotTextDocEntry.CreateEx(const Kind: THotTextDocEntryKind);
  {Class constructor used internally by the class "constructor" functions to
  construct an instance of the required Kind}
begin
  inherited Create;
  fKind := Kind;
end;

class function THotTextDocEntry.CreateFont(
  const Font: TFont): THotTextDocEntry;
  {Class function used to construct a Font entry: the given font is made
  available via the Font property. Note that the font is not copied: it is a
  reference to an external instance that should be maintained by the user thru
  the life of this entry instance}
begin
  if not Assigned(Font) then
    raise EHotTextDoc.Create(sDocMustHaveFont);
  Result := CreateEx(ikFont);
  Result.fFont := Font;
end;

class function THotTextDocEntry.CreateLinkEnd: THotTextDocEntry;
  {Class function used to construct a LinkEnd entry: there are no valid
  properties for this entry kind}
begin
  Result := CreateEx(ikLinkEnd);
end;

class function THotTextDocEntry.CreateLinkStart(const Cmd,
  Hint: string): THotTextDocEntry;
  {Class function used to construct a LinkStart entry: an internally owned
  link object is created for the given link Cmd and Hint. A reference to this
  link object is made available via the Link property. The link object is freed
  when this entry instance is freed}
begin
  if Cmd = '' then
    raise EHotTextDoc.Create(sLinkMustHaveCmd);
  Result := CreateEx(ikLinkStart);
  Result.fLink := THotTextLink.Create(Cmd, Hint);
end;

class function THotTextDocEntry.CreateNewPara(
  const PS: THotTextParaStyle): THotTextDocEntry;
  {Class function used to construct a NewPara entry: the given paragraph style
  object is made available via the ParaStyle property. Note that the paragraph
  style is not copied: it is a reference to an external instance that should be
  maintained by the user thru the life of this entry instance}
begin
  if not Assigned(PS) then
    raise EHotTextDoc.Create(sDocMustHaveParaStyle);
  Result := CreateEx(ikNewPara);
  Result.fParaStyle := PS;
end;

class function THotTextDocEntry.CreateText(
  const Text: string): THotTextDocEntry;
  {Class function used to construct a Text entry: the text is made available via
  the Text property. A copy of the text is stored internally}
begin
  Result := CreateEx(ikText);
  Result.fText := Text;
end;

destructor THotTextDocEntry.Destroy;
  {Class destructor: frees owned Link object}
begin
  fLink.Free; // only assigned if Kind = ikLinkStart
  inherited;
end;

function THotTextDocEntry.GetFont: TFont;
  {Read access method for the Font property: raises exception if this is not a
  font entry}
begin
  CheckValidKind(sFont, ikFont);
  Assert(Assigned(fFont));
  Result := fFont;
end;

function THotTextDocEntry.GetLink: THotTextLink;
  {Read access method for the Link property: raises exception if this is not a
  link entry}
begin
  CheckValidKind(sLinkStart, ikLinkStart);
  Assert(Assigned(fLink));
  Result := fLink;
end;

function THotTextDocEntry.GetParaStyle: THotTextParaStyle;
  {Read access method for the Font property: raises exception if this is not a
  new paragraph entry}
begin
  CheckValidKind(sNewPara, ikNewPara);
  Result := fParaStyle;
end;

function THotTextDocEntry.GetText: string;
  {Read access method for the Text property: raises exception if this is not a
  text entry}
begin
  CheckValidKind(sText, ikText);
  Result := fText;
end;


{ THotTextFontCache }

procedure THotTextFontCache.Clear;
  {Clears the cache and frees the font instances stored in it}
begin
  fItems.Clear;
end;

constructor THotTextFontCache.Create;
  {Class constructor: creates cache object}
begin
  inherited;
  fItems := TBasicObjList.Create(True); // this list owns objects added to it
end;

destructor THotTextFontCache.Destroy;
  {Class destructor: frees cache and all font objects in it}
begin
  fItems.Free;  // this also frees owned font objects
  inherited;
end;

function THotTextFontCache.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fItems.Count;
end;

function THotTextFontCache.GetItem(Idx: Integer): TFontEx;
  {Read access method for Items property}
begin
  Result := fItems[Idx] as TFontEx;
end;

function THotTextFontCache.Lookup(const Font: TFont): TFontEx;
  {Checks cache to see if there is an equivalent font in it and returns a
  reference to it if so. Otherwise a copy of the requested font is created and
  stored in the cache: it's reference is returned}
var
  Idx: Integer; // loops thru font instances in cache
begin
  // Check if a font with same properties as given font is in cache
  Result := nil;
  for Idx := 0 to Pred(Count) do
    if GetItem(Idx).IsEqual(Font) then
    begin
      Result := GetItem(Idx);
      Break;
    end;
  // If not such font, create one and add to cache
  if not Assigned(Result) then
  begin
    Result := TFontEx.CreateClone(Font);
    fItems.Add(Result);
  end;
end;


{ THotTextParaStyleCache }

procedure THotTextParaStyleCache.Clear;
  {Clears the cache and frees the paragraph style instances stored in it}
begin
  fItems.Clear;
end;

constructor THotTextParaStyleCache.Create;
  {Class constructor: creates cache object}
begin
  inherited Create;
  fItems := TBasicObjList.Create(True); // this list owns objects added to it
end;

destructor THotTextParaStyleCache.Destroy;
  {Class destructor: frees cache and all paragraph style objects in it}
begin
  fItems.Free;  // this also frees owned font objects
  inherited;
end;

function THotTextParaStyleCache.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fItems.Count;
end;

function THotTextParaStyleCache.GetItem(Idx: Integer): THotTextParaStyle;
  {Read access method for Item property}
begin
  Result := fItems[Idx] as THotTextParaStyle;
end;

function THotTextParaStyleCache.Lookup(
  const PS: THotTextParaStyle): THotTextParaStyle;
  {Checks cache to see if there is an equivalent paragraph style in it and
  returns a reference to it if so. Otherwise a copy of the requested paragraph
  style is created and stored in the cache: it's reference is returned}
var
  Idx: Integer; // loops thru paragraph style instances in cache
begin
  // Check if a paragraph style with same properties as given style is in cache
  Result := nil;
  for Idx := 0 to Pred(Count) do
    if GetItem(Idx).IsEqual(PS) then
    begin
      Result := GetItem(Idx);
      Break;
    end;
  // If not such paragraph style object, create one and add to cache
  if not Assigned(Result) then
  begin
    Result := THotTextParaStyle.CreateClone(PS);
    fItems.Add(Result);
  end;
end;


{ THotTextDoc }

function THotTextDoc.AddEntry(const Entry: THotTextDocEntry): Integer;
  {Adds the given entry to the document and returns its index in the Entries
  property}
begin
  Result := fEntries.Add(Entry);
end;

function THotTextDoc.AddFont(const Font: TFont): Integer;
  {Adds a font entry to the document which maintains its own copy of the font.
  The index of the new document entry is returned}
var
  FontRef: TFont; // reference to equivalent font in font cache
begin
  // Get reference to required font from cache
  FontRef := fFontCache.Lookup(Font);
  // Add a new entry that references the cached font
  Result := AddEntry(THotTextDocEntry.CreateFont(FontRef));
end;

function THotTextDoc.AddLinkEnd: Integer;
  {Adds a link end entry to the document and returns its index}
begin
  Result := AddEntry(THotTextDocEntry.CreateLinkEnd);
end;

function THotTextDoc.AddLinkStart(const Cmd, Hint: string): Integer;
  {Adds a new link start entry to the document and returns its index}
begin
  Result := AddEntry(THotTextDocEntry.CreateLinkStart(Cmd, Hint));
end;

function THotTextDoc.AddNewPara(const PS: THotTextParaStyle): Integer;
  {Adds a new paragraph entry to the document which maintains its own copy of
  the style object. The index of the new document entry is returned}
var
  PSRef: THotTextParaStyle; // reference to equivalent style in cache
begin
  // Get reference to required paragraph style from cache
  PSRef := fParaStyleCache.Lookup(PS);
  // Add a new entry that references the cached style object
  Result := AddEntry(THotTextDocEntry.CreateNewPara(PSRef));
end;

function THotTextDoc.AddText(const Text: string): Integer;
  {Adds a new text entry for the given text to the document and returns the
  index of the new entry}
begin
  Result := AddEntry(THotTextDocEntry.CreateText(Text));
end;

procedure THotTextDoc.Clear;
  {Clears the document and font and paragraph style caches}
begin
  fEntries.Clear;
  fFontCache.Clear;
  fParaStyleCache.Clear;
end;

constructor THotTextDoc.Create;
  {Class constructor: creates the entries list and font & paragraph style
  caches}
begin
  inherited;
  fParaStyleCache := THotTextParaStyleCache.Create;
  fFontCache := THotTextFontCache.Create;
  fEntries := TBasicObjList.Create(True);
end;

destructor THotTextDoc.Destroy;
  {Class destructor: free owned objects}
begin
  fEntries.Free;        // frees owned objects
  fFontCache.Free;      // frees cached fonts
  fParaStyleCache.Free; // frees cached para style objects
  inherited;
end;

function THotTextDoc.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := fEntries.Count;
end;

function THotTextDoc.GetEntry(Idx: Integer): THotTextDocEntry;
  {Read access method for Entries property}
begin
  Result := fEntries[Idx] as THotTextDocEntry;
end;

end.
