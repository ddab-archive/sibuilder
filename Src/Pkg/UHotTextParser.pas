{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UHotTextParser.pas
  @COMMENTS                 Implements a parser class that processes tokens read
                            from hot text code and writes output into a document
                            object that describes the hot text file for later
                            rendering.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Error message string literals moved to resource
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
 * The Original Code is UHotTextParser.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UHotTextParser;


interface


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UHotTextDoc, UTaggedTextLexer;


type

  {
  THotTextTagGroup
    Enumeration of the groups that tags can belong to.
  }
  THotTextTagGroup = (
    tgDoc,        // tags operate on or structure document
    tgBlock,      // tags are block-level
    tgCharStyle,  // tags updated character styles
    tgChar,       // tags insert characters
    tgSpecial     // tags requiring special handling
  );


  {
  THotTextParser:
    Class that defines a parser that processes tokens read from hot text code
    and writes output into a document object that describes the hot text file
    for later rendering. A separate lexical analyser is used to tokenise the
    input.
  }
  THotTextParser = class(TObject)
  private // properties
    fDefFont: TFont;
    fCode: string;
    fLexer: TTaggedTextLexer;
    procedure SetDefFont(const Value: TFont);
  private
    function TagInfoProvider(const TagIdx: Integer; out TagName: string;
      out TagCode: Word; out IsContainer: WordBool): Boolean;
      {Callback method passed to the lexer that provides information about the
      valid hot text document tags}
    function EntityInfoProvider(const EntityIdx: Integer;
      out EntityName: string; out EntityChar: AnsiChar): Boolean;
      {Callback method passed to the lexer that provides information about the
      valid hot text character entities}
  public
    constructor Create;
      {Class constructor: creates owned objects}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    property DefFont: TFont read fDefFont write SetDefFont;
      {Default font used in document created by parsing hot text code when the
      code does not specify a font}
    property Code: string read fCode write fCode;
      {Stores the hot text code to be parsed into a document decription. Setting
      this property clears any existing document. Call Execute to parse the code
      and, optionally, build a document description}
    procedure Execute(const Doc: THotTextDoc);
      {Parse code per Code property and build a document in the Doc object
      passed as a parameter. The Doc object describes the document defined by
      the code. Any existing document is cleared before the parsing begins. If
      Doc is nil the code is simply syntax checked and no output is generated.
      Any errors in the code are reported via exceptions}
  end;


  {
  EHotTextParser:
    Class of exception when errors detected by the hot text parser.
  }
  EHotTextParser = class(Exception);


implementation


uses
  // Delphi
  Classes, Windows,
  // Project
  UStringProcs, UFontEx, UHotTextParaStyle, UStacks;


const

  // Code of supported tags
  cTagHotText = 0;    // <hottext>: top level tag: enclose whole document
  cTagBody = 1;       // <body>: encloses all display content
  cTagPara = 2;       // <p>: encloses paragraph
  cTagBold = 3;       // <b>: encloses bold text
  cTagUnderline = 4;  // <u>: encloses underlines text
  cTagItalic = 5;     // <i>: encloses italic text
  cTagLink = 6;       // <link>: delimits clickable hot link
  cTagFont = 7;       // <font>: font attributes of enclosed text font
  cTagParaFont = 8;   // <parafont>: default font for enclosed paragraphs
  cTagBullet = 9;     // <bullet/>: inserts bullet of given type
  cTagTab = 10;       // <tag/>: inserts a tab here

  // Codes of first and last tags
  cFirstTag = cTagHotText;  // first tag number
  cLastTag = cTagTab;       // last tag number

  // Set of tags that format text
  cTextTags = [cTagBold, cTagUnderline, cTagItalic, cTagFont];

  // Table of info about each supported tag
  cTagInfo: array[cFirstTag..cLastTag] of record
    Tag: string;                  // tag's name
    HasText: Boolean;             // whether tag can contain text
    Group: THotTextTagGroup;      // group to which tag belongs
    IsContainer: Boolean;         // whether this is a container tag
    Containers: set of Byte;      // the compound tags that can contain this tag
  end = (
    // cTagHotText
    (Tag: 'hottext';  HasText: False; Group: tgDoc;
      IsContainer: True;  Containers: [];),
    // cTagBody
    (Tag: 'body';     HasText: False; Group: tgDoc;
      IsContainer: True;  Containers: [cTagHotText];),
    // cTagPara
    (Tag: 'p';        HasText: True;  Group: tgBlock;
      IsContainer: True;  Containers: [cTagBody, cTagParaFont];),
    // cTagBold
    (Tag: 'b';        HasText: True;  Group: tgCharStyle;
      IsContainer: True;  Containers: cTextTags + [cTagPara, cTagLink];),
    // cTagUnderline
    (Tag: 'u';        HasText: True;  Group: tgCharStyle;
      IsContainer: True;  Containers: cTextTags + [cTagPara, cTagLink];),
    // cTagItalic
    (Tag: 'i';        HasText: True;  Group: tgCharStyle;
      IsContainer: True;  Containers: cTextTags + [cTagPara, cTagLink];),
    // cTagLink
    (Tag: 'link';     HasText: True;  Group: tgSpecial;
      IsContainer: True;  Containers: cTextTags + [cTagPara];),
    // cTagFont
    (Tag: 'font';     HasText: True;  Group: tgCharStyle;
      IsContainer: True;  Containers: cTextTags + [cTagPara, cTagLink];),
    // cTagParaFont
    (Tag: 'parafont'; HasText: False; Group: tgCharStyle;
      IsContainer: True;  Containers: [cTagBody, cTagParaFont]),
    // cTagBullet
    (Tag: 'bullet';   HasText: False; Group: tgChar;
      IsContainer: False; Containers: cTextTags + [cTagPara, cTagLink];),
    // cTagTab
    (Tag: 'tab';      HasText: False; Group: tgChar;
      IsContainer: False; Containers: cTextTags + [cTagPara, cTagLink];)
  );

  // Table of supported character entities with associated characters
  cEntities: array[0..3] of record
    Entity: string;   // the name of the entity
    Ch: AnsiChar;     // the character represented by the entity
  end = (
    (Entity: 'quot';  Ch: '"'),
    (Entity: 'lt';    Ch: '<'),
    (Entity: 'gt';    Ch: '>'),
    (Entity: 'amp';   Ch: '&')
  );


resourcestring
  // Error messages
  sTagNeedsParam = 'Tag <%s> requires at least one parameter';
  sBadStyleParam = 'Invalid value of style parameter: %s';
  sBadColourParam = 'Invalid colour value of colour parameter: %s';
  sBadIntegerParam =
    'Integer value parameter expected: "%s" is not a valid value';
  sBadCharsetParam =
    'Value of charset parameter must be an integer, "ansi", "default" or '
    + '"symbol" parameter expected: "%s" is not a valid value';
  sBadAlignParam =
    'Value of align parameter must be left, center or right: '
    + '"%s" is not a valid value';
  sBulletNeedsShape = 'The <bullet> tag requires a shape parameter';
  sBadShapeParam = '"%s" is not a valid value for the <bullet> shape parameter';
  sBadContainer = 'Tag "%s" can''t be contained by "%s"';
  sBadRootTag = 'Tag "%s" can''t be the root tag';
  sBadFileFormat = 'Invalid hot text file format: version 1.0 required';
  sNoValidTagParams = '<%s> tag has no recognised parameters';
  sCmdParamNeeded = '<%s> tag must have a cmd parameter';
  sTagCantHaveText = 'Can''t add text to tag <%s>';
  sDefFontIsNil = 'Can''t set DefFont to nil';

{ Helper routines }

procedure EnforceParams(const Tag: string; const Params: TStrings);
  {Raises an exception if the given Params object does not contain any
  parameters. Tag is the name of the tag whose parameters are required: this is
  used only in error messages}
begin
  if not Assigned(Params) or (Params.Count = 0) then
    raise EHotTextParser.CreateFmt(sTagNeedsParam, [Tag]);
end;

function TagValueToFontStyle(const Value: string): TFontStyles;
  {Converts the given value of the parameter specifying font style to an actual
  font style. Raises exception if invalid values are found}
var
  Values: TStringList;  // string list containing list of font styles
  Idx: Integer;         // loops thru values in list
  ValItem: string;      // a single font style item from
begin
  // NOTE: a valid value is a comma separated list of zero or more of:
  //    bold, italic, underline
  // Start with empty set of styles
  Result := [];
  // Split comma separated list into string list of items
  Values := TStringList.Create;
  try
    SplitStr(Value, ',', Values, False);
    // Loop thru list of items, checking for valid values and updating result
    for Idx := 0 to Pred(Values.Count) do
    begin
      // get lower case version of current value
      ValItem := Trim(LowerCase(Values[Idx]));
      // update result set with value
      if ValItem = 'bold' then
        Include(Result, fsBold)
      else if ValItem = 'italic' then
        Include(Result, fsItalic)
      else if ValItem = 'underline' then
        Include(Result, fsUnderline)
      else
        raise EHotTextParser.CreateFmt(sBadStyleParam, [ValItem]);
    end;
  finally
    Values.Free;
  end;
end;

function TagValueToColor(const Value: string): TColor;
  {Converts the given value of the parameter specifying a colur to an actual
  colour value. Raises exception if value is invalid}
var
  TestVal: string;  // the value to test
begin
  // NOTE: valid values are either valid Delphi colour constants (clXXXX) or
  //    are hexadecimal colour numbers, preceded by a '#'
  // Convert value to one that is valid parameter to Delphi's StringToColor fn
  if (Value <> '') and (Value[1] = '#') then
    TestVal := '$' + Copy(Value, 2, MaxInt)
  else
    TestVal := Value;
  try
    // Use Delphi's library function to perform conversion
    Result := StringToColor(TestVal);
  except
    // Convert any conversion error to parser error
    on EConvertError do
      raise EHotTextParser.CreateFmt(sBadColourParam, [Value]);
  end;
end;

function TagValueToInt(const Value: string): Integer;
  {Covert the given Value string to an integer, raising a parser exception if
  value is not a valid integer}
begin
  // Simply use Delphi's conversion function, converting any excpetion to
  // a Parser error
  try
    Result := StrToInt(Value);
  except
    on EConvertError do
      raise EHotTextParser.CreateFmt(sBadIntegerParam, [Value]);
  end;
end;

function TagValueToCharSet(Value: string): TFontCharset;
  {Converts the given parameter value representing a character set to a valid
  TFontCharset value, raising a parser exception on error}
begin
  // NOTE: valid values of Value are either a valid integer or any of:
  //    ansi, default, symbol
  // First check for any of special values
  if AnsiCompareText(Value, 'ansi') = 0 then
    Result := ANSI_CHARSET
  else if AnsiCompareText(Value, 'default') = 0 then
    Result := DEFAULT_CHARSET
  else if AnsiCompareText(Value, 'symbol') = 0 then
    Result := SYMBOL_CHARSET
  else
  begin
    // Not a special symbol: it must be an integer
    // we use Delphi's conversion fn but convert any exception to a parser error
    try
      Result := StrToInt(Value);
    except
    on EConvertError do
      raise EHotTextParser.CreateFmt(sBadCharsetParam, [Value]);
    end;
  end;
end;

function TagValueToAlign(const Value: string): TAlignment;
  {Converts the given alignment parameter value to the matching aligment code.
  Raises parser exception on error}
begin
  // NOTE: valid values for this parameter type are:
  //    left, center, right
  // Check if value is any of supported values, raising exception if not
  if AnsiCompareText(Value, 'left') = 0 then
    Result := taLeftJustify
  else if AnsiCompareText(Value, 'center') = 0 then
    Result := taCenter
  else if AnsiCompareText(Value, 'right') = 0 then
    Result := taRightJustify
  else
    raise EHotTextParser.CreateFmt(sBadAlignParam, [Value]);
end;

function BulletTagParamsToChar(Params: TStrings): Char;
  {Returns the character representing a bullet defined by the given parameters
  from a bullet tag}
type
  // Record used to map bullet shapes to bullet characters
  TShapeMap = record
    Shape: string;      // name of shape
    Bullet: Char;       // char for small version of bullet
    LargeBullet: Char;  // char for large version of bullet
  end;
const
  // Maps bullet shapes to matching characters
  cShapes: array[1..5] of TShapeMap = (
    (Shape: 'circle'; Bullet: #$9F; LargeBullet: #$6C;),
    (Shape: 'square'; Bullet: #$A7; LargeBullet: #$6E),
    (Shape: 'diamond'; Bullet: #$77; LargeBullet: #$75),
    (Shape: 'tick'; Bullet: #$FC; LargeBullet: #$FC),     // small = large
    (Shape: 'cross'; Bullet: #$FB; LargeBullet: #$FB)     // small = large
  );
var
  Shape: string;    // the bullet shape
  Large: Boolean;   // whether we're using large version of bullet
  Idx: Integer;     // loops thru shapes map
begin
  // NOTE: The bullet tag has a compulsory "shape" parameter which can have any
  //    of the values noted in the Shape element of the cShapes array. It also
  //    has an optional "large" flag that has no value but means that any large
  //    version of the bullet is used (not all shapes have large versions).
  // First we insist that there is at least one "shape" parameter present
  EnforceParams('bullet', Params);
  Shape := LowerCase(Params.Values['shape']);
  if Shape = '' then
    raise EHotTextParser.Create(sBulletNeedsShape);
  // Record whether using large version of bullet: we are if "large" is present
  Large := Params.IndexOfName('large') > -1;
  // Lookup the requested shape in the table
  Result := #0;
  for Idx := Low(cShapes) to High(cShapes) do
    if Shape = cShapes[Idx].Shape then
    begin
      // found shape: return small or large version as required
      if not Large then
        Result := cShapes[Idx].Bullet
      else
        Result := cShapes[Idx].LargeBullet;
      Break;
    end;
  // Error if we didn't find matching bullet shape
  if Result = #0 then
    raise EHotTextParser.CreateFmt(sBadShapeParam, [Shape]);
end;

procedure ProcessParaTabsParam(const Value: string; const TS: THotTextTabStops);
  {Processes the given "tabs" parameter value and updates the given tab stops
  object according to parameter value}
var
  Tabs: TStringList;  // stores any list of tab stops
  Idx: Integer;       // loops thru tab stops list
begin
  // NOTE: a tabs parameter value can either be the single word "default" or
  //    can be a comma separated list of tab stops. In the first case the
  //    default, evenly spaced list of tabs is used, and in the latter the given
  //    list of tabs is used
  // Clear the given tab stops object
  TS.Clear;
  if AnsiCompareText(Value, 'default') = 0 then
    // We have "default" word: simply set tab stop's DefaultTabs property
    TS.DefaultTabs := True
  else
  begin
    // We should have list of tab stops
    // split commas separated list into string list
    Tabs := TStringList.Create;
    try
      SplitStr(Value, ',', Tabs, False);
      // loop thru tab stops, adding to tab stop object
      // an exception will be raised by TagValueToInt if any tab is not integer
      for Idx := 0 to Pred(Tabs.Count) do
        TS.AddTab(TagValueToInt(Trim(Tabs[Idx])));
    finally
      Tabs.Free;
    end;
  end;
end;


{ THotTextParser }

constructor THotTextParser.Create;
  {Class constructor: creates owned objects}
begin
  inherited Create;
  // Font object to record DefFont property value
  fDefFont := TFont.Create;
  // Lexer object used to analyse and tokenise source file
  fLexer := TTaggedTextLexer.Create(TagInfoProvider, EntityInfoProvider);
end;

destructor THotTextParser.Destroy;
  {Class destructor: frees owned objects}
begin
  fLexer.Free;
  fDefFont.Free;
  inherited;
end;

function THotTextParser.EntityInfoProvider(const EntityIdx: Integer;
  out EntityName: string; out EntityChar: AnsiChar): Boolean;
  {Callback method passed to the lexer that provides information about the valid
  hot text character entities}
begin
  // Return information from required row of cEntities
  Result := (EntityIdx <= High(cEntities));   // assumes cEntities is zero based
  if Result then
    with cEntities[EntityIdx] do
    begin
      EntityName := Entity;
      EntityChar := Ch;
    end;
end;

procedure THotTextParser.Execute(const Doc: THotTextDoc);
  {Parse code per Code property and build a document in the Doc object passed as
  a parameter. The Doc object describes the document defined by the code. Any
  existing document is cleared before the parsing begins. If Doc is nil the
  code is simply syntax checked and no output is generated. Any errors in
  the code are reported via exceptions}
var
  // These variables may be used by any of the nested routines
  TagStack: TIntegerStack;  // stack of nested compound tags to date: used to
                            // that a tag can be contained in parent tag
  FontStack: TFontExStack;  // stack of fonts: used to revert to a previous font
                            // when a font tag close (fonts can nest)
  CurFont: TFontEx;         // the font currently in use
  ParaHasText: Boolean;     // whether any text has been written to a paragraph
  // the following are used when no item specified in hot text code
  DefFontCopy: TFontEx;             // copy of DefFont object
  DefParaStyle: THotTextParaStyle;  // default character style

  // ---------------------------------------------------------------------------

  procedure SetCurFont(const Font: TFontEx);
    {Sets the given font as the current one to be used. If given font is not nil
    we assign it to existing current font if it exists or create a copy if not.
    If given font is nil we free any existing current font and nil the value}
  begin
    if Assigned(Font) then
    begin
      // we have non-nil font: copy or create clone as necessary
      if CurFont = nil then
        CurFont := TFontEx.CreateClone(Font)
      else
        CurFont.Assign(Font);
    end
    else
      // we have nil font: free existing current font
      if Assigned(CurFont) then
      begin
        CurFont.Free;
        CurFont := nil;
      end;
  end;

  // ---------------------------------------------------------------------------

  procedure ValidateParent;
    {Checks if tag just read can be nested inside current tag (if any). Raises
    exceptions on errors and returns normally if OK}
  var
    Containers: set of Byte;  // set of valid containers for tag
  begin
    // Get set of valid containers for tag just read
    Containers := cTagInfo[fLexer.TagCode].Containers;
    if not TagStack.IsEmpty then
    begin
      // There are open tags:
      // check if current tag (top of stack) can contain new tag
      if not (TagStack.Top in Containers) then
        raise EHotTextParser.CreateFmt(
          sBadContainer,
          [cTagInfo[fLexer.TagCode].Tag, cTagInfo[TagStack.Top].Tag]
        );
    end
    else
    begin
      // There are no open tags: new tag is first
      // check if new tag can be root (Containers = [] for root tag)
      if Containers <> [] then
        raise EHotTextParser.CreateFmt(
          sBadRootTag, [cTagInfo[fLexer.TagCode].Tag]
        );
    end;
  end;

  // ---------------------------------------------------------------------------

  procedure EmitText(const TheText: string);
    {Stores the given text in the document object, but writes out details of any
    change of font first}
  begin
    // Check if font has changed has changed since last text was output (i.e.
    // if font at top of font stack is different to current font). Doing this
    // means we only emit fonts that are needed to format actual text and
    // intermediate font changes are not emitted
    if not FontStack.Top.IsEqual(CurFont) then
    begin
      // font has changed: make changed font current & output it to document
      SetCurFont(FontStack.Top);
      if Assigned(Doc) then
        Doc.AddFont(CurFont);
    end;
    // Now we output the text and record that there is text in the current para
    if Assigned(Doc) then
      Doc.AddText(TheText);
    ParaHasText := True;
  end;

  // ---------------------------------------------------------------------------

  procedure ProcessStartTag;
    {Processes the start of a new opening compound tag}
  var
    Font: TFontEx;                // temp font used by various tags
    ParamValue: string;           // temp storage for a parameter's value
    ParamCount: Integer;          // no of params for a tag
    ParaStyle: THotTextParaStyle; // paragraph style used by <p> tags
  begin
    // Check that this tag is valid inside current compound tag
    ValidateParent;
    // Push this tag onto tag stack: it is now the current compound tag
    TagStack.Push(fLexer.TagCode);
    // Process tags by group
    case cTagInfo[fLexer.TagCode].Group of
      tgDoc:
      begin
        // Document level tags: handle individual tags
        case fLexer.TagCode of
          cTagHotText:
          begin
            // HOTTEXT root tag
            // must have version param = 1.0
            if (fLexer.TagParams.Values['version'] <> '1.0') then
              raise EHotTextParser.Create(sBadFileFormat);
          end;
          cTagBody:
          begin
            // BODY tag
            // push default font object onto stack
            FontStack.Push(TFontEx.CreateClone(DefFontCopy));
          end;
        end;
      end;
      tgBlock:
      begin
        // Block level tags: handle individual tags
        case fLexer.TagCode of
          cTagPara:
          begin
            // P tag
            // set current font to nil to force font info to be emitted before
            // first text in paragraph
            SetCurFont(nil);
            // note no text yet in paragraph
            ParaHasText := False;
            // process P tags paragraph style params
            // .. first create default paragraph style for possile updating
            ParaStyle := THotTextParaStyle.CreateClone(DefParaStyle);
            try
              // .. handle any "fi" (first line indent) parameter
              ParamValue := fLexer.TagParams.Values['fi'];
              if ParamValue <> '' then
                ParaStyle.IndentFirst := TagValueToInt(ParamValue);
              // .. handle any "li" (left indent) parameter
              ParamValue := fLexer.TagParams.Values['li'];
              if ParamValue <> '' then
                ParaStyle.IndentLeft := TagValueToInt(ParamValue);
              // .. handle any "ri" (right indent) parameter
              ParamValue := fLexer.TagParams.Values['ri'];
              if ParamValue <> '' then
                ParaStyle.IndentRight := TagValueToInt(ParamValue);
              // .. handle any "align" parameter
              ParamValue := fLexer.TagParams.Values['align'];
              if ParamValue <> '' then
                ParaStyle.Alignment := TagValueToAlign(ParamValue);
              // .. handle any "tabs" parameter
              ParamValue := fLexer.TagParams.Values['tabs'];
              if ParamValue <> '' then
                ProcessParaTabsParam(ParamValue, ParaStyle.Tabs);
              // add new paragraph with required style to document
              if Assigned(Doc) then
                Doc.AddNewPara(ParaStyle);
            finally
              // free temp paragraph style object
              ParaStyle.Free;
            end;
            Assert(not FontStack.IsEmpty,
              'FontStack cannot be empty at start of paragraph');
          end;
        end;
      end;
      tgCharStyle:
      begin
        // Character style tags (always update a font)
        Assert(not FontStack.IsEmpty,
          'FontStack cannot be empty when processing character style tag');
        // create copy of font at top of font stack for updating by tags
        Font := TFontEx.CreateClone(FontStack.Top);
        // process individual tags
        case fLexer.TagCode of
          cTagBold:
            // B tag: switches on bold style in new font
            Font.Style := Font.Style + [fsBold];
          cTagUnderline:
            // U tag: switches on underline style in new font
            Font.Style := Font.Style + [fsUnderline];
          cTagItalic:
            // I tag: switches on italic style in new font
            Font.Style := Font.Style + [fsItalic];
          cTagFont, cTagParaFont:
          begin
            // FONT and PARAFONT tags
            // tags must have at least one parameter
            EnforceParams(fLexer.TagName, fLexer.TagParams);
            // update new font with details specified in parameters
            ParamCount := 0;  // counts paremeters we recognise
            // .. handle font "name" parameter
            ParamValue := fLexer.TagParams.Values['name'];
            if ParamValue <> '' then
            begin
              Font.Name := ParamValue;
              Inc(ParamCount);
            end;
            // .. handle font "size" parameter
            ParamValue := fLexer.TagParams.Values['size'];
            if ParamValue <> '' then
            begin
              Font.Size := TagValueToInt(ParamValue);
              Inc(ParamCount);
            end;
            // .. handle font "color" parameter
            ParamValue := fLexer.TagParams.Values['color'];
            if ParamValue <> '' then
            begin
              Font.Color := TagValueToColor(ParamValue);
              Inc(ParamCount);
            end;
            // .. handle font "style" parameter
            ParamValue := fLexer.TagParams.Values['style'];
            if ParamValue <> '' then
            begin
              Font.Style := TagValueToFontStyle(ParamValue);
              Inc(ParamCount);
            end;
            // .. handle font "charset" parameter
            ParamValue := fLexer.TagParams.Values['charset'];
            if ParamValue <> '' then
            begin
              Font.CharSet := TagValueToCharset(ParamValue);
              Inc(ParamCount);
            end;
            // raise exception if none of exected params present
            if ParamCount = 0 then
              raise EHotTextParser.CreateFmt(
                sNoValidTagParams, [fLexer.TagName]
              );
          end;
        end;
        // push updated copy of font onto font stack
        FontStack.Push(Font);
      end;
      tgSpecial:
      begin
        // Handle special tags individually
        case fLexer.TagCode of
          cTagLink:
          begin
            // LINK tag
            // get and check params: must have at least a "cmd" parameter
            EnforceParams(fLexer.TagName, fLexer.TagParams);
            ParamValue := fLexer.TagParams.Values['cmd'];
            if ParamValue = '' then
              raise EHotTextParser.CreateFmt(sCmdParamNeeded, [fLexer.TagName]);
            // add start link item to document with cmd and optional hint values
            if Assigned(Doc) then
              Doc.AddLinkStart(ParamValue, fLexer.TagParams.Values['hint']);
          end;
        end;
      end;
    end;
  end;

  // ---------------------------------------------------------------------------

  procedure ProcessEndTag;
    {Processes the end of a compound tag}
  var
    Font: TFontEx;  // font reference required by various tags
  begin
    Assert(TagStack.Top = fLexer.TagCode,
      'End tag code must match tag code at top of tag stack');
    // Pop the tag being closed from the tag stack
    TagStack.Pop;
    // Process the tags by group
    case cTagInfo[fLexer.TagCode].Group of
      tgDoc:
      begin
        // Document level tags: handle individually
        case fLexer.TagCode of
          cTagHotText:
            // HOTTEXT tag
            {Do nothing};
          cTagBody:
          begin
            // BODY tag
            Assert(FontStack.Top.IsEqual(DefFontCopy),
              'Default font must be top of font stack in </body> tag');
            // pop the default font copy from the font stack and free it
            Font := FontStack.Pop;
            Font.Free;
            Assert(FontStack.IsEmpty,
              'Font stack should empty at end of </body> tag processing');
          end;
        end;
      end;
      tgBlock:
      begin
        // Block level tags: process individually
        case fLexer.TagCode of
          cTagPara:
          begin
            // P tag
            if not ParaHasText then
            begin
              // empty para: record top of font stack as current and emit it
              // (this won't have been done since we didn't emit text)
              SetCurFont(FontStack.Top);
              if Assigned(Doc) then
                Doc.AddFont(CurFont);
            end;
          end;
        end;
      end;
      tgCharStyle:
      begin
        // Character style tags
        // pop the font stack and free the popped font
        Font := FontStack.Pop;
        Font.Free;
        // there are no tag-specific actions in closing tag
      end;
      tgSpecial:
      begin
        // Special tags: processed individually
        case fLexer.TagCode of
          cTagLink:
          begin
            // LINK tag
            // write out special "close of link" entry to document
            if Assigned(Doc) then
              Doc.AddLinkEnd;
          end;
        end;
      end;
    end;
  end;

  // ---------------------------------------------------------------------------

  procedure ProcessSimpleTag;
    {Processes a new simple tag}
  var
    Font: TFontEx;  // font used by bullet tag
    Bullet: Char;   // a bullet character
  begin
    // Check that this tag is valid inside current compound tag
    ValidateParent;
    // Process tag
    case fLexer.TagCode of
      cTagBullet:
      begin
        // BULLET tag
        // get bullet from params (exception on bad params)
        Bullet := BulletTagParamsToChar(fLexer.TagParams);
        // set up font for bullet (all supported bullets used Wingdings)
        Font := TFontEx.CreateClone(FontStack.Top);
        try
          Font.Name := 'Wingdings';
          Font.Charset := SYMBOL_CHARSET;
          Font.Style := Font.Style - [fsUnderline];
          // push font onto stack (EmitText uses font at TOS)
          FontStack.Push(Font);
          // now write bullet
          EmitText(Bullet);
          // remove bullet font from font stack
          Font := FontStack.Pop;
        finally
          // free font as no longer required
          Font.Free;
        end;
      end;
      cTagTab:
        // TAB tag
        // we simply emit a tab character
        EmitText(#9);
    end;
  end;

  // ---------------------------------------------------------------------------

  procedure ProcessText;
    {Processes the current text read from lexer. If the current tag accepts text
    it is output with all white space converted to space characters. If tag
    doesn't accept text and text is just white space it is ignored otherwise an
    exception is raised}

    // -------------------------------------------------------------------------
    function IsWhiteSpace(const Text: string): Boolean;
      {Returns true if the given text is all made up of white space and false
      otherwise. See cWhiteSpace for definition of white space}
    var
      Idx: Integer; // loops thru each character in text
    begin
      // Scan text looking for non-white space character: is white space if none
      Result := False;
      for Idx := 1 to Length(Text) do
        if not (Text[Idx] in cWhiteSpace) then
          Exit;
      Result := True;
    end;
    // -------------------------------------------------------------------------

  var
    TheText: string;  // the text to be output (with white space converted)
    Idx: Integer;     // loops thru each character of text
  begin
    // Whether we output text depends on whether enclosing tag accepts it
    if cTagInfo[TagStack.Top].HasText then
    begin
      // Tag accepts text: output it with whitespace turned to spaces
      // convert white space to spaces
      TheText := fLexer.PlainText;
      for Idx := 1 to Length(TheText) do
        if IsWhiteSpace(TheText[Idx]) then
          TheText[Idx] := ' ';
      // output the text
      EmitText(TheText);
    end
    else
    begin
      // Tag does not accept text: if it's just white space, ignore, else error
      if not IsWhiteSpace(fLexer.PlainText) then
        raise EHotTextParser.CreateFmt(
          sTagCantHaveText, [cTagInfo[TagStack.Top].Tag]
        );
    end;
  end;

  // ---------------------------------------------------------------------------

begin
  // Nil the required objects
  FontStack := nil;     // object created below
  TagStack := nil;      // object created below
  CurFont := nil;       // object creation deferred until needed
  DefFontCopy := nil;   // object created below
  DefParaStyle := nil;  // object create below
  // Clear the document if provided
  if Assigned(Doc) then
    Doc.Clear;
  try
    // Create copy of default font for use when no font specified in code
    // this is pushed and popped on font stack during <body> tag processing
    DefFontCopy := TFontEx.CreateClone(fDefFont);
    // Create default paragraph style for use when no style specified in code
    DefParaStyle := THotTextParaStyle.Create;
    // Create font stack: used to record the font that currently applies which
    // is required since font tags can nest and closing one tag reverts to
    // previous font (or default if all tags closed)
    FontStack := TFontExStack.Create;
    // Create tag (code) stack: used to track currently open compound tags to
    // enable us to check whether any tag can nest inside the current tag
    TagStack := TIntegerStack.Create;
    // Pass the code to be processed to the lexer
    fLexer.TaggedText := fCode;
    // Get each code item (token) from lexer and handle it
    while fLexer.NextItem <> ttsEOF do
    begin
      case fLexer.Kind of
        ttsSimpleTag:           ProcessSimpleTag;
        ttsCompoundStartTag:    ProcessStartTag;
        ttsCompoundEndTag:      ProcessEndTag;
        ttsText:                ProcessText;
        ttsComment, ttsScript:  {Ignore};
      end;
    end;
  finally
    // Free the objects
    CurFont.Free;
    TagStack.Free;
    FontStack.Free;
    DefFontCopy.Free;
    DefParaStyle.Free;
  end;
end;

procedure THotTextParser.SetDefFont(const Value: TFont);
  {Setter method for DefFont property: value must be non-nil or exception is
  raised. The value is assigned to an internal object.}
begin
  if not Assigned(Value) then
    raise EHotTextParser.Create(sDefFontIsNil);
  fDefFont.Assign(Value);
end;

function THotTextParser.TagInfoProvider(const TagIdx: Integer;
  out TagName: string; out TagCode: Word;
  out IsContainer: WordBool): Boolean;
  {Callback method passed to the lexer that provides information about the valid
  hot text document tags}
begin
  Result := (TagIdx <= High(cTagInfo));   // assumes cTagInfo is zero based
  if Result then
  begin
    TagName := cTagInfo[TagIdx].Tag;
    TagCode := TagIdx;
    IsContainer := cTagInfo[TagIdx].IsContainer;
  end;
end;

end.

