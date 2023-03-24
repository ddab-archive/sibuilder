{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UTaggedTextLexer.pas
  @COMMENTS                 Implements a main lexical analyser and subsdiary
                            classes that can tokenise "tagged text" code (i.e.
                            code in a SGML like format). The lexer is
                            customisable, the user providing the valid tags and
                            character entities. It checks the code for correctly
                            nested tags. Simple (<tag/>) and compound
                            (<tag>..</tag>) tags are supported, as are comments
                            and script tags.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             + Fixed minor error reporting bug where description
                              of invalid entities was not being reported.
                            + Moved all error message string literals to
                              resources strings.
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
 * The Original Code is UTaggedTextLexer.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UTaggedTextLexer;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UStacks;


const
  // Character constants made public
  cSingleQuote = '''';
  cDoubleQuote = '"';
  cWhiteSpace = [#0..#32];
  cQuotes = [cSingleQuote, cDoubleQuote];


type

  {
  ETaggedTextLexer:
    Class of exception raised when errors reported by the lexer and its
    supporting classes.
  }
  ETaggedTextLexer = class(Exception);


  {
  TTaggedTextKind:
    Different kinds of tokens recognised and reported by the lexer.
  }
  TTaggedTextKind = (
    ttsNull,              // used internally to flag errors
    ttsText,              // plain text
    ttsSimpleTag,         // a simple tag (form <tag />
    ttsCompoundStartTag,  // start of a compound tag (<tag>)
    ttsCompoundEndTag,    // end of a compound tag (</tag>)
    ttsComment,           // a comment (<! .. > or <!-- .. -->)
    ttsScript,            // a script <? .. ?>
    ttsEOF                // end of code: no more tokens
  );


  {
  TTaggedTextEntityHandler:
    Processes and translates character entities into the corresponding
    characters. Two types of entity are supported:
      1) numeric entities of the form &#99; where 99 is a decimal number in
         range 0..255 representing the ASCII code of a character - supported by
         default
      2) symbolic entities of form &entity_name; - the user specifies the names
         and character values of these entities: none are supported by default
    Errors are reported via the LastErrorMessage property. The case of entities
    is not significant, so that &AMP; is the same as &amp;
  }
  TTaggedTextEntityHandler = class(TObject)
  private // properties
    fLastErrorMessage: string;
  private
    fSymbolicEntities: TStringList;
      {String list storing entities with entity name stored as string item and
      the code of the represented character stored in Objects[]}
  public
    constructor Create;
      {Class constructor: creates empty entity list}
    destructor Destroy; override;
      {Class destructor: frees owned entity list}
    function AddEntity(const Entity: string; const Ch: AnsiChar): Boolean;
      {Adds the given symbolic entity and its corresponding character to the
      list of symbolic entities. Set up the list of entities before attempting
      to translate an entity. Returns true if the entity was added successfully
      and false if the entity is already recorded: the LastErrorMessage property
      stores the error message. Entities are always added in lower case}
    function TranslateEntity(const Entity: string; out Ch: AnsiChar): Boolean;
      {Translates entity into the character it represents and returns it via Ch
      parameter. If entity is translated OK then True is returned. If there is
      an error then False is returned and a description of the error is made
      available via LastErrorMessage property. Note that Entity should not
      include the opening '&' or closing ';'}
    function TranslateTextEntities(const Text: string;
      out TransStr: string): Boolean;
      {Finds and translates all entities in the given Text, and passes back the
      translated text in TransStr where all entities have been replaced by their
      corresponding characters. Returns true on success. If any errors are
      detected then LastErrorMessage stores a description of the error, TransStr
      has an undefined value and false is returned}
    property LastErrorMessage: string read fLastErrorMessage;
      {Stores a description of the last reported error. The error message is
      *not* reset when methods execute successfully}
  end;


  {
  TTaggedTextEntityHandler:
    Processes tags and translates them into the corresponding tag codes. Also
    maintains a list of supported tags as supplied by user. Errors are reported
    via the LastErrorMessage property.
  }
  TTaggedTextTagHandler = class(TObject)
  private // properties
    fLastErrorMessage: string;
  private
    fTagList: TStringList;
      {String list storing details of the supported tags. The tag name is stored
      as a string item and the tag code and whether tag is compound are merged
      into a long word that is stored in Objects[]}
    fEntityHandler: TTaggedTextEntityHandler;
      {Reference to entity handler object used to translate entities appearing
      in tag values}
    function GetKind(var WorkTag: string): TTaggedTextKind;
      {Gets kind of tag from working copy of tag and removes the '/' characters
      that indicate the kind. If kind not recognised ttsNull is returned}
    function GetTagName(const TagStr: string; out NextChPos: Integer): string;
      {Extracts the name of the tag from the given string (which should be tag
      contents excluding any opening or closing markers) and returns it. The
      character position following the end of the tag name is passed back
      through the NextChPos parameter: this is used by other routines to extract
      further information from the tag string}
    function LookupTagInfo(const TagName: string; out TagCode: Word;
      out IsCompound: WordBool): Boolean;
      {Looks up the tag with the given name in the table of supported tags and
      passes back the tag's code and whether it is a compound tag via the out
      parameters. True is retured if the tag is found in the table and false if
      not}
    function GetTagParams(const TagStr: string; var NextChPos: Integer;
      const Params: TStrings): Integer;
      {Parses tag text from TagStr starting from the NextChPos position (which
      must be set to just after tag name) to read list of parameters. If Params
      is not nil then the parameters are passes back through this list as
      name=value pairs. The count of the number of parameters is returned. This
      method can used either to fetch or simply count the parameters. TagStr
      must be the whole of the text of the tag string without the leading and
      trailing tag characters}
    procedure Error(const Msg: string); overload;
      {Records the given message in the LastErrorMessage property}
    procedure Error(const Fmt: string; const Args: array of const); overload;
      {Formats the given error message and records it in the LastErrorMessage
      property}
  public
    constructor Create(const EH: TTaggedTextEntityHandler);
      {Class constructor: creates owned tag list object and records a reference
      to the given entity handler}
    destructor Destroy; override;
      {Class destructor: frees owned object}
    function AddTag(const Tag: string; const Code: Word;
      const IsCompound: WordBool): Boolean;
      {Adds the provided information about a tag to the list of tags recognised
      by the handler. The information is:
        Tag: the name of the tag without enclosing <>
        Code: a unique word valued code associated with the tag - must not be
          $FFFF since this is reserved for comment and script tags
        IsCompound: set to true if the tag is compound (i.e. can contain other
          tags or text or false if the tag stands on its own (like the <br.>
          tag in html)
      A list of supported tags must be set up using this method before
      attempting to process them since the handler recognises no tags by
      default. If the tag is successfully added true is returned, otherwise
      false is returned and LastErrorMessage contains a description of the
      error. Tags are always recorded in lower case}
    function ProcessTag(const Tag: string; out Text: string;
      out Kind: TTaggedTextKind; out Code: Word;
      const Params: TStrings): Boolean;
      {Parses and extract information from the given tag (which must include the
      leading '<' and trailing '>'). Information from the tag is passed back
      thru the out parameters as follows:
        Text: for start, end and simple tags this is name of tag for comment and
          script tags this is the content of the tag
        Kind: Kind of tag, or ttsNull if error
        Code: The code associated with the tag ($FFFF for comments and scripts)
        Params: List of parameters to tag in name=value format. If Params=nil
          then no parameter information is returned
      True is returned if the tag is processed successfully, otherwise false is
      returned and a description of the error is in LastErrorMessage}
    property LastErrorMessage: string read fLastErrorMessage;
      {Stores a description of the last reported error. The error message is
      *not* reset when methods execute successfully}
  end;


  {
  TTaggedTextTagInfoProc:
    Type of callback procedure that lexer calls to get information about valid
    tags. The procedure is called repeatedly, with TagIdx starting at zero and
    being incremented on each call until the implementer returns false to
    indicate there are no more tags. To add information about at tag the
    implementer must set the TagName, TagCode and IsContainer parameters with
    the name and code of a tag and whether he tag is a container and return
    true.
  }
  TTaggedTextTagInfoProc = function(const TagIdx: Integer; out TagName: string;
    out TagCode: Word; out IsContainer: WordBool): Boolean of object;


  {
  TTaggedTextEntityInfoProc:
    Type of callback procedure that lexer calls to get information about valid
    characters entities. The procedure is called repeatedly with EntityIdx
    starting at zero and being incremented on each call until the implementer
    returns false to indicate there are no more entities. To add information
    about an entity the EntityName parameter is set to the entity's name and the
    EntityChar parameter is set to the character it represents and true is
    returned.
  }
  TTaggedTextEntityInfoProc = function(const EntityIdx: Integer;
    out EntityName: string; out EntityChar: AnsiChar): Boolean of object;


  {
  TTaggedTextLexer:
    The class implements a lexical analyser for tagged text code (i.e. in a
    SGML like format. The lexer can detect compound (start <tag> and end </tag>)
    tags, simple (<tag/>) tags, script tags (<? .. ?>, comment tags (<!..>) and
    plain text (which may contain character entities). It provides relevant
    information about the the code element just read and only accepts predefined
    tags and entities.
  }
  TTaggedTextLexer = class(TObject)
  private // properties
    fTaggedText: string;
    fKind: TTaggedTextKind;
    fTagCode: Word;
    fParams: TStringList;
    fCurText: string; // used to store text for various properties
    procedure SetTaggedText(const Value: string);
    function GetTagName: string;
    function GetPlainText: string;
    function GetTagParams: TStrings;
    function GetCommentText: string;
    function GetScriptText: string;
    function GetTagCode: Integer;
  private
    fEntityHandler: TTaggedTextEntityHandler;
      {Object that parses entities and replaces entities in strings of text by
      their character values}
    fTagHandler: TTaggedTextTagHandler;
      {Object that parses tags, returning their type, id code and parameters}
    fTagStack: TStringStack;
      {Stack of nested active compound tags}
    fNextCharPos: Integer;
      {The position of the next character to process in the tagged text}
    procedure GetTagInfo(const Callback: TTaggedTextTagInfoProc);
      {Gets information about the supported tags from the user of the class by
      repeatedly calling the callback method util it returns false. The tag
      information supplied from the callback is added to the tag handler
      object's list of supported tags}
    procedure GetEntityInfo(const Callback: TTaggedTextEntityInfoProc);
      {Gets information about the supported character entities from the user of
      the class by repeatedly calling the callback method util it returns false.
      The entity information supplied from the callback is added to the entity
      handler object's list of supported tags}
  public
    constructor Create(const TagInfoCallback: TTaggedTextTagInfoProc;
      const EntityInfoCallback: TTaggedTextEntityInfoProc);
      {Class contructor: sets default, creates owned objects and records
      supported tags and character entities by calling the provided callback
      methods, which must be non-nil}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure Reset;
      {Resets the lexer ready to restart the analysis of the TaggedText code}
    function NextItem: TTaggedTextKind;
      {Fetches the next logical item from the tagged text per the TaggedText
      property. Returns ttsEOF when there are no more items in the code. The
      Kind property is updated to the return value. The TagCode, TagName,
      TagParams, CommentText, ScriptCode and PlainText properties provide extra
      information about the current item}
    property TaggedText: string read fTaggedText write SetTaggedText;
      {The code to be analysed by the lexer: setting this property calls Reset}
    property Kind: TTaggedTextKind read fKind;
      {The kind of the item read from the tagged text by the last call to
      NextItem. Kind determines which of the TagCode, TagName, TagParams,
      CommentText, ScriptCode and PlainText properties are valid when getting
      further information about the item read}
    property TagCode: Integer read GetTagCode;
      {The code number (range 0..$FFFF) of the tag read by NextItem: only valid
      when Kind is ttsCompoundStartTag, ttsCompoundEndTag or ttsSimpleTag}
    property TagName: string read GetTagName;
      {The name of the tag read by NextItem excluding tag delimiters: only valid
      when Kind is ttsCompoundStartTag, ttsCompoundEndTag or ttsSimpleTag}
    property TagParams: TStrings read GetTagParams;
      {List of parameters associated with the tag read by NextItem as Name=Value
      pairse: only valid when Kind is ttsCompoundStartTag or ttsSimpleTag}
    property CommentText: string read GetCommentText;
      {Text included in the comment read by NextItem: only valid when Kind is
      ttsComment}
    property ScriptText: string read GetScriptText;
      {Text of script enclosed by script tags read by NextItem: only valid when
      Kind is ttsScript}
    property PlainText: string read GetPlainText;
      {Plain text read by NextItem. This text includes all white space and any
      character entities know to lexer have neem translated. CRLF pairs are
      converted to LF. Only valid when Kind is ttsText}
  end;


implementation


resourcestring
  // Error messages
  sEntityAlreadyReg = 'Entity "%s" already registered';
  sEntityEmpty = 'Empty entity';
  sEntityHasNoValue = 'Entity "#" has no numeric value';
  sEntityValueNotValid = 'Entity "%s" is not a valid non-negative number';
  sEntityOutOfRange = 'Numeric entity "%s" out of range';
  sEntityNotRecognised = 'Entity "%s" not recognised';
  sEntityUnterminated = 'Unterminated entity';
  sTagCodeInvalid = 'Tag "%s" cannot have code $FFFF';
  sTagAlreadyRegistered = 'Tag "%s" already registered';
  sTagEmpty = 'Tag is empty';
  sTagNotRecognised = 'Tag "%s" not recognised';
  sSimpleTagInvalid = 'Tag "%s" is not a valid simple tag';
  sCompoundTagInvalid = 'Tag "%s" is not a valid compound tag';
  sTagNotCompound = 'Tag "%s" is not a compound tag';
  sEndTagHasParams = 'End tag "%s" should not have parameters';
  sItemNotComment = 'Can''t read comment: current item not comment';
  sItemNotText = 'Can''t read text: current item not text';
  sItemNotScript = 'Can''t read script: current item not a script';
  sItemNotTagCode = 'Can''t read tag code: current item not tag';
  sItemNotTag = 'Can''t read tag name: current item not tag';
  sCantReadParams = 'Can''t read tag params: current item not a suitable tag';
  sTextBeforeOpeningTag = 'Text found before opening tag';
  sTextAfterClosingTag = 'Text found after closing tag "%s"';
  sNoMatchingStartTag = 'End tag "%s" encountered with no matching start tag';
  sNoMatchingEndTag = 'No end of tag marker found for tag beginning at %d';
  sErrorReadingTag = 'Error reading tag at %d: %s';
  sEndTagIsFirstTag
    = 'End tag "%s" found as first tag: compound start tag required';
  sStartAndEndTagMismatched = 'End tag "%s" does not match opening tag "%s"';
  sStartTagIsSimple
    = 'Simple tag "%s" found as first tag: compound start tag required';
  sErrorReadingEntities = 'Error reading entities in text at %d: %s';
  sUnexpectedEOF = 'End of file found before all tags closed';

{ TTaggedTextEntityHandler }

function TTaggedTextEntityHandler.AddEntity(const Entity: string;
  const Ch: AnsiChar): Boolean;
  {Adds the given symbolic entity and its corresponding character to the list of
  symbolic entities. Set up the list of entities before attempting to translate
  an entity. Returns true if the entity was added successfully and false if the
  entity is already recorded: the LastErrorMessage property stores the error
  message. Entities are always added in lower case}
begin
  Result := fSymbolicEntities.IndexOf(Entity) = -1;
  if Result then
    fSymbolicEntities.AddObject(LowerCase(Entity), Pointer(Ch))
  else
    fLastErrorMessage := Format(sEntityAlreadyReg, [Entity]);
end;

constructor TTaggedTextEntityHandler.Create;
  {Class constructor: creates empty entity list}
begin
  inherited;
  fSymbolicEntities := TStringList.Create;
end;

destructor TTaggedTextEntityHandler.Destroy;
  {Class destructor: frees owned entity list}
begin
  fSymbolicEntities.Free;
  inherited;
end;

function TTaggedTextEntityHandler.TranslateEntity(
  const Entity: string; out Ch: AnsiChar): Boolean;
  {Translates entity into the character it represents and returns it via Ch
  parameter. If entity is translated OK then True is returned. If there is
  an error then False is returned and a description of the error is made
  available via LastErrorMessage property. Note that Entity should not include
  the opening '&' or closing ';'}
var
  AsciiVal: Integer;    // ASCII value of a numeric entity
  SymbolIdx: Integer;   // index of symbolic entities in symbol list

begin
  // Assume failure
  Result := False;
  // Error if entity is empty string
  if Entity = '' then
  begin
    fLastErrorMessage := sEntityEmpty;
    Exit;
  end;
  // Check entity type
  if Entity[1] = '#' then
  begin
    // We have numeric entity: try to extract ascii value
    if Entity = '#' then
    begin
      // entity has no associated value
      fLastErrorMessage := sEntityHasNoValue;
      Exit;
    end;
    Assert(Length(Entity) >= 2);
    // parse out the digits: only 0..9 accepted
    // we reject -ve numbers: use default of -1 so all conversion errors give
    // -ve number to indicate error
    AsciiVal := StrToIntDef(Copy(Entity, 2, MaxInt), -1);
    if AsciiVal < 0 then
    begin
      fLastErrorMessage := Format(sEntityValueNotValid, [Entity]);
      Exit;
    end;
    // check if value is in range (already know >=0)
    if AsciiVal > 255 then
    begin
      fLastErrorMessage := Format(sEntityOutOfRange, [Entity]);
      Exit;
    end;
    // we have valid value: record it and return true
    Ch := Chr(AsciiVal);
    Result := True;
  end
  else
  begin
    // Symbolic entity
    // check if entity in list of supported entities
    SymbolIdx := fSymbolicEntities.IndexOf(Entity);
    if SymbolIdx = -1 then
    begin
      fLastErrorMessage := Format(sEntityNotRecognised, [Entity]);
      Exit;
    end;
    // entity is supported: record it's character value and return true
    Ch := AnsiChar(fSymbolicEntities.Objects[SymbolIdx]);
    Result := True;
  end;
end;

function TTaggedTextEntityHandler.TranslateTextEntities(
  const Text: string; out TransStr: string): Boolean;
  {Finds and translates all entities in the given Text, and passes back the
  translated text in TransStr where all entities have been replaced by their
  corresponding characters. Returns true on success. If any errors are detected
  then LastErrorMessage stores a description of the error, TransStr has an
  undefined value and false is returned}
var
  Idx: Integer;         // index used to scan text
  InsPos: Integer;      // index of insertion point in translated string
  Ch: AnsiChar;         // current char in text: used to check for entities
  EntityStart: Integer; // records start of entity in text
  Entity: string;       // stores any found entity
  EntityCh: AnsiChar;   // stores character represented by entity
begin
  // Assume failure
  Result := False;
  // Set up cursors into input and ouput strings
  Idx := 1;
  InsPos := 1;
  // Length of TransStr will be <= length of text: allocate max possible size
  SetLength(TransStr, Length(Text));
  // Scan thru each character of text to be translated
  while Idx <= Length(Text) do
  begin
    // Record current character in input for processing
    Ch := Text[Idx];
    case Ch of
      '&':
      begin
        // We have start of entity
        // skip past opening '&' and record positiin as start of entity
        Inc(Idx);
        EntityStart := Idx;
        // scan through string looking for ';' that ends entity
        while (Idx <= Length(Text)) and (Text[Idx] <> ';') do
          Inc(Idx);
        if Idx > Length(Text) then
        begin
          // didn't find terminating ';': report errro
          fLastErrorMessage := sEntityUnterminated;
          Exit;
        end;
        // record entity excluding opening '&' and closing ';' in lower case
        Entity := LowerCase(Copy(Text, EntityStart, Idx - EntityStart));
        // skip over ending ';' in input
        Inc(Idx);
        // try to translate entity: exit on error (LastErrorMessage set by
        // TranslateEntity)
        if not TranslateEntity(Entity, EntityCh) then
          Exit;
        // insert translated character in TransStr, and update its cursor
        TransStr[InsPos] := EntityCh;
        Inc(InsPos);
      end;
      else
      begin
        // We have ordinary character: copy into TransStr and move cursors on
        TransStr[InsPos] := Ch;
        Inc(InsPos);
        Inc(Idx);
      end;
    end;
  end;
  // If we have translated entities TransStr will be shorter than input string@
  // so we reduce TransStr length accodingly
  if Idx <> InsPos then
    SetLength(TransStr, InsPos - 1);
  Result := True;
end;


{ TTaggedTextTagHandler }

function TTaggedTextTagHandler.AddTag(const Tag: string;
  const Code: Word; const IsCompound: WordBool): Boolean;
  {Adds the provided information about a tag to the list of tags recognised by
  the handler. The information is:
    Tag: the name of the tag without enclosing <>
    Code: a unique word valued code associated with the tag - must not be $FFFF
      since this is reserved for comment and script tags
    IsCompound: set to true if the tag is compound (i.e. can contain other tags
      or text or false if the tag stands on its own (like the <br.> tag in html)
  A list of supported tags must be set up using this method before attempting to
  process them since the handler recognises no tags by default. If the tag is
  successfully added true is returned, otherwise false is returned and
  LastErrorMessage contains a description of the error. Tags are always recorded
  in lower case}
var
  Data: LongWord; // stores both the tag code and the IsCompound flag
begin
  // Check that code is not reserved value $FFFF: error if so
  if Code = $FFFF then
  begin
    Result := False;
    Error(sTagCodeInvalid, [Tag]);
    Exit;
  end;
  // Check if tag already recorded: error if so
  Result := fTagList.IndexOf(Tag) = -1;
  if Result then
  begin
    // encode Code and a IsCompound into long word
    LongRec(Data).Lo := Code;
    LongRec(Data).Hi := Word(IsCompound);
    // add the tag and the data to the list
    fTagList.AddObject(LowerCase(Tag), Pointer(Data));
  end
  else
    Error(sTagAlreadyRegistered, [Tag]);
end;

constructor TTaggedTextTagHandler.Create(const EH: TTaggedTextEntityHandler);
  {Class constructor: creates owned tag list object and records a reference to
  the given entity handler}
begin
  inherited Create;
  fTagList := TStringList.Create;
  fTagList.Sorted := True;
  fEntityHandler := EH;
end;

destructor TTaggedTextTagHandler.Destroy;
  {Class destructor: frees owned object}
begin
  fTagList.Free;
  inherited;
end;

procedure TTaggedTextTagHandler.Error(const Msg: string);
  {Records the given message in the LastErrorMessage property}
begin
  fLastErrorMessage := Msg;
end;

procedure TTaggedTextTagHandler.Error(const Fmt: string;
  const Args: array of const);
  {Formats the given error message and records it in the LastErrorMessage
  property}
begin
  Error(Format(Fmt, Args));
end;

function TTaggedTextTagHandler.GetKind(
  var WorkTag: string): TTaggedTextKind;
  {Return of ttsNull indicates error: LastErrorMessage set in this case}
var
  Len: Integer;
begin
  Len := Length(WorkTag);
  Assert(WorkTag[1] = '<');
  Assert(WorkTag[Len] = '>');
  if (WorkTag = '<>') or (WorkTag = '</>') then
  begin
    // we have <> or </> => empty tag: delete all tag
    Result := ttsNull;
    Error(sTagEmpty);
    WorkTag := '';
    Exit;
  end;
  Assert(Len >= 3);
  if (Len >= 4) and (WorkTag[Len-1] = '/') then
  begin
    // tag of form <tag> => simple: we delete the / char
    Result := ttsSimpleTag;
    Delete(WorkTag, Len-1, 1);
  end
  else if WorkTag[2] = '/' then
  begin
    // tag of form </tag> => end of compound tag: we delete the / char
    Result := ttsCompoundEndTag;
    Delete(WorkTag, 2, 1);
  end
  else if WorkTag[2] = '!' then
  begin
    // tag is of form <!directive> or <!-- comment -->: we delete ! and any --
    Result := ttsComment;
    Delete(WorkTag, 2, 1);
    if AnsiPos('--', WorkTag) = 2 then
      Delete(WorkTag, 2, 2);
    if AnsiPos('--', WorkTag) = Length(WorkTag) - 2 then
      Delete(WorkTag, Length(WorkTag) - 2, 2);
  end
  else if WorkTag[2] = '?' then
  begin
    // tag is of form <?script> or <?script?>: we delete any ? chars
    Result := ttsScript;
    Delete(WorkTag, 2, 1);
    if WorkTag[Length(WorkTag) - 1] = '?' then
      Delete(WorkTag, Length(WorkTag) -1, 1);
  end
  else
    // tag is of form <tag>: start of compound tag: no changes to text
    Result := ttsCompoundStartTag;
  // Finally strip off delimiting < and > chars
  WorkTag := Copy(WorkTag, 2, Length(WorkTag) - 2);
end;

function TTaggedTextTagHandler.GetTagName(const TagStr: string;
  out NextChPos: Integer): string;
  {Extracts the name of the tag from the given string (which should be tag
  contents excluding any opening or closing markers) and returns it. The
  character position following the end of the tag name is passed back through
  the NextChPos parameter: this is used by other routines to extract further
  information from the tag string}
var
  StartPos: Integer;  // start position of tag in TagStr
begin
  Assert(Length(TagStr) >= 1);
  // Start at the beginning of the tag string
  NextChPos := 1;
  // Skip any white space before tag
  while (NextChPos <= Length(TagStr))
    and (TagStr[NextChPos] in cWhiteSpace) do
    Inc(NextChPos);
  // Now at start of tag name: read it up to next space or end of tag str
  StartPos := NextChPos;
  while (NextChPos <= Length(TagStr))
    and not (TagStr[NextChPos] in cWhiteSpace) do
    Inc(NextChPos);
  // Copy the name from the string
  Result := Copy(TagStr, StartPos, NextChPos - StartPos);
end;

function TTaggedTextTagHandler.GetTagParams(const TagStr: string;
  var NextChPos: Integer; const Params: TStrings): Integer;
  {Parses tag text from TagStr starting from the NextChPos position (which must
  be set to just after tag name) to read list of parameters. If Params is not
  nil then the parameters are passes back through this list as name=value pairs.
  The count of the number of parameters is returned. This method can used either
  to fetch or simply count the parameters. TagStr must be the whole of the text
  of the tag string without the leading and trailing tag characters}

  // ---------------------------------------------------------------------------

  function GetNextParam(out Name, Value: string): Boolean;
    {Reads the next parameter from the current character position in the tag
    string and returns its name and value. For parameters that are "flags" (i.e.
    have no values, Value is set to ''. Returns true if a parameter was read and
    false if there are no more parameters in the tag}
  var
    StartPos: Integer;        // start position of name or value in tag string
    ValDelims: set of Char;   // characters used to delimit values (e.g. quotes)
    Len: Integer;             // length of whole tag
  begin
    // Set name & value to '' in case not found
    Name := '';
    Value := '';
    // Record length of whole tag
    Len := Length(TagStr);

    // Check to see if we have any params
    // skip white space
    while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
      Inc(NextChPos);
    // check if we've reached end of tag and get out if so: no params
    if NextChPos > Len then
    begin
      Result := False;
      Exit;
    end;

    // We have attribute: get name
    StartPos := NextChPos;
    while (NextChPos <= Len)
      and not (TagStr[NextChPos] in cWhiteSpace + ['=']) do
      Inc(NextChPos);
    Name := Copy(TagStr, StartPos, NextChPos - StartPos);
    // skip any white space following name
    while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
      Inc(NextChPos);

    // Check for value
    // if current character is '=' we have a value (else no value)
    if TagStr[NextChPos] = '=' then
    begin
      // skip '=' symbol
      Inc(NextChPos);
      // skip white space between '=' and value
      while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
        Inc(NextChPos);
      // if NextChPos > Len the there is no value: do nothing
      if NextChPos <= Len then
      begin
        // check to see if we have quoted param or not
        if TagStr[NextChPos] in cQuotes then
        begin
          // value is quoted: record quote as delimter and skip it
          ValDelims := [TagStr[NextChPos]];
          Inc(NextChPos);
        end
        else
          // value is not quoted: single word expected: white space delimits
          ValDelims := cWhiteSpace;
        // now get the value: it is between current pos and a delimter
        StartPos := NextChPos;
        while (NextChPos <= Len) and not (TagStr[NextChPos] in ValDelims) do
          Inc(NextChPos);
        // get the value: allows for closing quotes being missing
        Value := Copy(TagStr, StartPos, NextChPos - StartPos);
        // translate any entities in value: we ignore any errors here
        fEntityHandler.TranslateTextEntities(
          Copy(TagStr, StartPos, NextChPos - StartPos),
          Value
        );
        // if value was quoted, skip over any quote
        if (cQuotes * ValDelims <> [])
          and (NextChPos <= Len)
          and (TagStr[NextChPos] in cQuotes) then
          Inc(NextChPos);
      end;
    end;
    // Record that we found at least a name
    Result := True;
  end;

  // ---------------------------------------------------------------------------

var
  Name, Value: string;  // name and value of parameter
begin
  Assert(Length(TagStr) >= 1);
  // Set param count to zero
  Result := 0;
  // Loop while we have more parameters, getting the name and value of each
  while GetNextParam(Name, Value) do
  begin
    // record the name=value pair for the parameter just read, if required
    if Assigned(Params) then
      Params.Add(Name + '=' + Value);
    // count the parameter just read
    Inc(Result);
  end;
end;

function TTaggedTextTagHandler.LookupTagInfo(const TagName: string;
  out TagCode: Word; out IsCompound: WordBool): Boolean;
  {Looks up the tag with the given name in the table of supported tags and
  passes back the tag's code and whether it is a compound tag via the out
  parameters. True is retured if the tag is found in the table and false if not}
var
  TagIdx: Integer;  // index of tag in table
  Data: LongWord;   // stores tag code and flag noting if tag is compound
begin
  // Lookup tag in table and record if found
  TagIdx := fTagList.IndexOf(TagName);
  Result := TagIdx >= 0;
  if Result then
  begin
    // Found tag: extract Code and IsCompound from string list's Objects[]
    Data := LongWord(fTagList.Objects[TagIdx]);
    TagCode := LongRec(Data).Lo;
    IsCompound := WordBool(LongRec(Data).Hi);
  end;
end;

function TTaggedTextTagHandler.ProcessTag(const Tag: string;
  out Text: string; out Kind: TTaggedTextKind; out Code: Word;
  const Params: TStrings): Boolean;
  {Parses and extract information from the given tag (which must include the
  leading '<' and trailing '>'). Information from the tag is passed back thru
  the out parameters as follows:
    Text: for start, end and simple tags this is name of tag for comment and
      script tags this is the content of the tag
    Kind: Kind of tag, or ttsNull if error
    Code: The code associated with the tag ($FFFF for comments and scripts)
    Params: List of parameters to tag in name=value format. If Params=nil then
      no parameter information is returned
  True is returned if the tag is processed successfully, otherwise false is
  returned and a description of the error is in LastErrorMessage}
var
  Len: Integer;         // length of the tag
  WorkingTag: string;   // string used to manipulate the given tag
  IsCompound: WordBool; // true if tag is compound, false otherwise
  ChPos: Integer;       // indicates position of character to process in tag
begin
  // Assume failure
  Result := False;
  // Clear any parameters list
  if Assigned(Params) then
    Params.Clear;
  // Store tag in working storage and record its length
  WorkingTag := Tag;
  Len := Length(WorkingTag);
  Assert(WorkingTag[1] = '<');
  Assert(WorkingTag[Len] = '>');
  // Get kind of tag, stripping out all tag delimiting info leavig just tag name
  // and contents/parameters
  Kind := GetKind(WorkingTag);
  // Process tag content, depending on kind of tag
  case Kind of
    ttsNull:
      // Error condition: error message set in GetKind
      Exit;
    ttsSimpleTag:
    begin
      // Simple tag
      // get tag's name: must always call this method before others than extract
      // information from a tag since this method sets the character position
      // ChPos ready to extract information following tag name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about the tag from table of supported tags
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if IsCompound then
      begin
        // tag recognised but is a compound tag => not valid as simple tag
        Error(sSimpleTagInvalid, [Text]);
        Exit;
      end;
      // get parameters for the tag
      GetTagParams(WorkingTag, ChPos, Params);
      Result := True;
    end;
    ttsCompoundStartTag:
    begin
      // Compound start tag
      // get tag's name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about tag from lookup table
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if not IsCompound then
      begin
        // tag not compound type: not valid here
        Error(sCompoundTagInvalid, [Text]);
        Exit;
      end;
      // get parameters for the tag
      GetTagParams(WorkingTag, ChPos, Params);
      Result := True;
    end;
    ttsCompoundEndTag:
    begin
      // Compound end tag
      // get tag's name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about tag from lookup table
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if not IsCompound then
      begin
        // tag not compound type: not valid here
        Error(sTagNotCompound, [Text]);
        Exit;
      end;
      // check if has params: not valid for end tags
      if GetTagParams(WorkingTag, ChPos, nil) > 0 then
      begin
        Error(sEndTagHasParams, [Text]);
        Exit;
      end;
      Result := True;
    end;
    ttsComment, ttsScript:
    begin
      // Comment or Script tag
      Code := $FFFF;      // not a tag in lookup table
      Text := WorkingTag; // text to return is all that is left of tag
      Result := True;
    end;
  end;
end;


{ TTaggedTextLexer }

constructor TTaggedTextLexer.Create(
  const TagInfoCallback: TTaggedTextTagInfoProc;
  const EntityInfoCallback: TTaggedTextEntityInfoProc);
  {Class contructor: sets default, creates owned objects and records supported
  tags and character entities by calling the provided callback methods, which
  must be non-nil}
begin
  // Pre-conditions:
  Assert(Assigned(TagInfoCallback));
  Assert(Assigned(EntityInfoCallback));
  inherited Create;
  // Create entity and tag handler objects used to parse tags and char entities
  fEntityHandler := TTaggedTextEntityHandler.Create;
  fTagHandler := TTaggedTextTagHandler.Create(fEntityHandler);
  // Create stack object to track nested compound tags
  fTagStack := TStringStack.Create;
  // Create object to store a tag's parameters
  fParams := TStringList.Create;
  // Initialise ready to read tagged text
  Reset;
  // Get list of supported tags and entities using callback functions
  GetTagInfo(TagInfoCallback);
  GetEntityInfo(EntityInfoCallback);
end;

destructor TTaggedTextLexer.Destroy;
  {Class destructor: frees owned objects}
begin
  fParams.Free;
  fEntityHandler.Free;
  fTagHandler.Free;
  fTagStack.Free;
  inherited;
end;

function TTaggedTextLexer.GetCommentText: string;
  {Getter for CommentText property: returns the current text. Raises exception
  if Kind of current tagged text item is not a comment}
begin
  if fKind <> ttsComment then
    raise ETaggedTextLexer.Create(sItemNotComment);
  Result := fCurText;
end;

procedure TTaggedTextLexer.GetEntityInfo(
  const Callback: TTaggedTextEntityInfoProc);
  {Gets information about the supported character entities from the user of the
  class by repeatedly calling the callback method util it returns false. The
  entity information supplied from the callback is added to the entity handler
  object's list of supported tags}
var
  Idx: Integer;           // inrementing index number for each callback call
  Name: string;           // name of character entity
  Ch: Char;               // character associated with entity
begin
  Idx := 0;
  while Callback(Idx, Name, Ch) do
  begin
    fEntityHandler.AddEntity(Name, Ch);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetPlainText: string;
  {Getter for PlainText property: returns the current text. Raises exception if
  Kind of current tagged text item is not text}
begin
  if fKind <> ttsText then
    raise ETaggedTextLexer.Create(sItemNotText);
  Result := fCurText;
end;

function TTaggedTextLexer.GetScriptText: string;
  {Getter for ScriptText property: returns the current text. Raises exception if
  Kind of current tagged text item is not a script}
begin
  if fKind <> ttsScript then
    raise ETaggedTextLexer.Create(sItemNotScript);
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagCode: Integer;
  {Getter for TagCode property: returns the code of the current tag. Raises
  exception if Kind of current item is not a tag}
begin
  if not (fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sItemNotTagCode);
  Result := fTagCode;
end;

procedure TTaggedTextLexer.GetTagInfo(const Callback: TTaggedTextTagInfoProc);
  {Gets information about the supported tags from the user of the class by
  repeatedly calling the callback method util it returns false. The tag
  information supplied from the callback is added to the tag handler object's
  list of supported tags}
var
  Idx: Integer;           // inrementing index number for each callback call
  Tag: string;            // name of supported tag
  Code: Word;             // unique code number associated with tag
  IsContainer: WordBool;  // whether the tag can contain text and/or other tags
begin
  Idx := 0;
  while Callback(Idx, Tag, Code, IsContainer) do
  begin
    fTagHandler.AddTag(Tag, Code, IsContainer);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetTagName: string;
  {Getter for TagName property: returns the current text. Raises exception if
  Kind of current tagged text item is not a tag}
begin
  if not (fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sItemNotTag);
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagParams: TStrings;
  {Getter for TagParams property: returns the current reference to string list
  storing parameters for current tag. Raises exception if Kind of current tagged
  text item is not a tag}
begin
  if not (fKind in [ttsCompoundStartTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sCantReadParams);
  Result := fParams;
end;

function TTaggedTextLexer.NextItem: TTaggedTextKind;
  {Fetches the next logical item from the tagged text per the TaggedText
  property. Returns ttsEOF when there are no more items in the code. The Kind
  property is updated to the return value. The TagCode, TagName, TagParams,
  CommentText, ScriptCode and PlainText properties provide extra information
  about the current item}
var
  StartCh: Char;          // starting character of a string within tagged text
  StartPos: Integer;      // starting position of a string within tagged text
  Tag: string;            // name of tag read by lexer
  ExpectedTag: string;    // name of a tag expected by lexer
  IsFirstCall: Boolean;   // true if first call to method since reset
begin
  // Clear any existing parameters from param list
  fParams.Clear;
  // Check if this is first call of method since reset: next char will be first
  IsFirstCall := fNextCharPos = 1;
  if IsFirstCall then
  begin
    // Just starting: skip over any white space before start opening tag
    while (fNextCharPos <= Length(fTaggedText))
      and (fTaggedText[fNextCharPos] in cWhiteSpace) do
      Inc(fNextCharPos);
    // Check if first no space character is start of a tag: error if not
    if (fNextCharPos <= Length(fTaggedText))
      and (fTaggedText[fNextCharPos] <> '<') then
      raise ETaggedTextLexer.Create(sTextBeforeOpeningTag);
  end;
  // Scan through tagged text recognising various elements
  if fNextCharPos <= Length(fTaggedText) then
  begin
    // Record character starting this scan and check to see what next item is
    StartCh := fTaggedText[fNextCharPos];
    if StartCh = '<' then
    begin
      // We have start of a tag: get hold it and process it
      // record start of tag
      StartPos := fNextCharPos;
      // skip thru text until tag closer found
      while (fNextCharPos <= Length(fTaggedText))
        and (fTaggedText[fNextCharPos] <> '>') do
        Inc(fNextCharPos);
      // check if we have found end of tag: error if not
      if fNextCharPos > Length(fTaggedText) then
        raise ETaggedTextLexer.CreateFmt(sNoMatchingEndTag, [StartPos]);
      Assert(fTaggedText[fNextCharPos] = '>');
      // skip over tag close
      Inc(fNextCharPos);
      // record tag
      Tag := Copy(fTaggedText, StartPos, fNextCharPos - StartPos);
      // Process the tag raising exception on error
      if not fTagHandler.ProcessTag(
        Tag, fCurText, fKind, fTagCode, fParams
      ) then
        raise ETaggedTextLexer.CreateFmt(
          sErrorReadingTag, [StartPos, fTagHandler.LastErrorMessage]
        );
      // Now act on kind of tag read
      case fKind of
        ttsCompoundStartTag:
          // we have compound start tag: push onto stack of currently open tags
          fTagStack.Push(fCurText);
        ttsCompoundEndTag:
        begin
          // we have compound end tag: check validity
          if IsFirstCall then
            // .. can't have end tag as first tag in code
            raise ETaggedTextLexer.CreateFmt(sEndTagIsFirstTag, [fCurText]);
          if fTagStack.IsEmpty then
            // .. tag stack empty => no matching opening tag
            raise ETaggedTextLexer.CreateFmt(sNoMatchingStartTag, [fCurText]);
          // .. tag we expect closes the one at top of stack
          //    pop stack to close tag
          ExpectedTag := fTagStack.Pop;
          if AnsiCompareText(fCurText, ExpectedTag) <> 0 then
            // .. error if tag is not the expected one
            raise ETaggedTextLexer.CreateFmt(
              sStartAndEndTagMismatched, [fCurText, ExpectedTag]
          );
          if fTagStack.IsEmpty then
          begin
            // the tag just closed was outer most:
            // .. skip over closing white space to EOF
            while (fNextCharPos <= Length(fTaggedText))
              and (fTaggedText[fNextCharPos] in cWhiteSpace) do
              Inc(fNextCharPos);
            // .. if any more text in file this is outside closing tag: error
            if fNextCharPos <= Length(fTaggedText) then
              raise ETaggedTextLexer.CreateFmt(
                sTextAfterClosingTag, [ExpectedTag]
              );
          end;
        end;
        ttsSimpleTag:
        begin
          // we have a simple tag
          if IsFirstCall then
            // .. simple tags can't be first in code
            raise ETaggedTextLexer.CreateFmt(sStartTagIsSimple, [fCurText]);
        end;
        ttsComment, ttsScript:
          // we have script or comment
          {No special processing for these tags};
      end;
    end
    else
    begin
      // We have plain text - process it
      fKind := ttsText;
      Assert(fNextCharPos <= Length(fTaggedText));
      Assert(fTaggedText[fNextCharPos] <> '<');
      // get extent of text before ext tag or end of tagged text
      StartPos := fNextCharPos;
      Inc(fNextCharPos);
      while (fNextCharPos <= Length(fTaggedText))
        and (fTaggedText[fNextCharPos] <> '<') do
        Inc(fNextCharPos);
      // check the plain text for entities, replacing them with values
      if not fEntityHandler.TranslateTextEntities(
        Copy(fTaggedText, StartPos, fNextCharPos - StartPos),
        fCurText
      ) then
        raise ETaggedTextLexer.CreateFmt(
          sErrorReadingEntities, [StartPos, fEntityHandler.LastErrorMessage]
        );
      // replace all CR LF pairs with LF
      fCurText := StringReplace(fCurText, #13#10, #10, [rfReplaceAll]);
    end;
  end
  else
  begin
    // We're at end of tagged text it's an error if we still have unclosed tags
    if not fTagStack.IsEmpty then
      raise ETaggedTextLexer.Create(sUnexpectedEOF);
    fKind := ttsEOF;
  end;
  // Return the kind of item just analysed
  Result := fKind;
end;

procedure TTaggedTextLexer.Reset;
  {Resets the lexer ready to restart the analysis of the TaggedText code}
begin
  fNextCharPos := 1;
  fTagStack.Clear;
  fKind := ttsNull;
end;

procedure TTaggedTextLexer.SetTaggedText(const Value: string);
  {Setter for TaggedText property. Records new value and resets lexer ready to
  analyse the new tagged text}
begin
  fTaggedText := Value;
  Reset;
end;

end.

