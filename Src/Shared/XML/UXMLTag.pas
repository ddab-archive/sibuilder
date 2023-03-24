{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UXMLTag.pas
  @COMMENTS                 This unit defines a class that encapsulates a XML
                            tag that can read itself from file and write itself
                            out again. Provides access to the tag's name, all
                            its parameters and its text.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/08/2000
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
 * The Original Code is UXMLTag.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UXMLTag;


interface


uses
  // Delphi
  Classes, SysUtils,
  // Project
  UCharStreamWriter, UTokeniser;


type

  {
  EXMLTag:
    Exceptions raised by TXMLTag and descendants.
  }
  EXMLTag = class(Exception);

  {
  TXMLTag:
    Encapsulates a XML tag that can read itself from file and write itself out
    again. Provides access to the tag's name, all its parameters and its text.
  }
  TXMLTag = class(TObject)
  private // properties
    fTag: string;
    function GetSubTagCount: Integer;
    function GetSubTag(const Idx: Integer): TXMLTag;
    function GetParam(const Name: string): string;
    function GetParamAsInt(const Name: string): Integer;
    procedure SetParam(const Name, Value: string);
    procedure SetParamAsInt(const Name: string; const Value: Integer);
  private
    fPlainText: string;
      {The tag's plain text, if any}
    fSubTags: TList;
      {List of sub tag objects}
    fParams: TStringList;
      {List of params and their values in form "name=value"}
    procedure ReadParam(const Tokeniser: TTokeniser);
      {Reads a tag parameter from the tokeniser. Assumes that current token is a
      parameter}
    procedure ReadSubTag(const Tokeniser: TTokeniser);
      {Recursively reads a sub-tag and all its contents from the tokeniser.
      Assumes that current token is an opening tag}
    procedure ReadText(const Tokeniser: TTokeniser);
      {Reads a tag's plain text from tokeniser. Assumes text is current token}
    procedure ClearSubTags;
      {Frees all sub tags and clears list}
  public
    constructor Create(const Tag: string);
      {Class constructor - creates owned sub tag and parameters objects}
    destructor Destroy; override;
      {Class destructor - frees owned objects}
    procedure Clear;
      {Clears tag and all its subtags}
    procedure Read(const Tokeniser: TTokeniser);
      {Reads tag and its sub-tags from file using given tokeniser}
    procedure Write(const Writer: TCharStreamWriter);
      {Write tag and all its sub-tags to XML file using given writer}
    function AddSubTag(const Tag: string): TXMLTag;
      {Create new sub-tag with given name and return reference to it}
    property Tag: string read fTag;
      {The tag}
    property SubTagCount: Integer read GetSubTagCount;
      {Number of sub-tags}
    property SubTags[const Idx: Integer]: TXMLTag read GetSubTag;
      {List of sub-tags}
    property Params[const Name: string]: string
      read GetParam write SetParam;
      {Parameter values accessed by name as strings}
    property ParamAsInt[const Name: string]: Integer
      read GetParamAsInt write SetParamAsInt;
      {Paramaeter values accessed by name as integers. Raises exception if value
      is not a valid integer}
    property PlainText: string read fPlainText write fPlainText;
      {Plain text that appears between tags}
  end;


implementation


resourcestring
  // Error messages
  sOpenTagExpected = 'XML Error: Opening tag expected';
  sTagExpected = 'XML Error: Tag <%s> expected';
  sUnexpectedEOF = 'XMLError: Unexpected end of file while reading tag <%s>';
  sClosingTagExpected = 'XML Error: Closing tag </%s> expected but found </%s>';

{ TXMLTag }

function TXMLTag.AddSubTag(const Tag: string): TXMLTag;
  {Create new sub-tag with given name and return reference to it}
begin
  // Create sub-tag with given tag name and add to list
  Result := TXMLTag.Create(Tag);
  fSubTags.Add(Result);
end;

procedure TXMLTag.Clear;
  {Clears tag and all its subtags}
begin
  // Clear parameters, subtags and plain text
  fParams.Clear;
  ClearSubTags;
  fPlainText := '';
end;

procedure TXMLTag.ClearSubTags;
  {Frees all sub tags and clears list}
var
  I: Integer;   // loops thru sub-tags
begin
  // Free all sub tags
  for I := Pred(GetSubTagCount) downto 0 do
    TXMLTag(fSubTags[I]).Free;
  // Clear sub tag list
  fSubTags.Clear;
end;

constructor TXMLTag.Create(const Tag: string);
  {Class constructor - creates owned sub tag and parameters objects}
begin
  inherited Create;
  // Record tag
  fTag := Tag;
  // Create owned objects
  fSubTags := TList.Create;
  fParams := TStringList.Create;
end;

destructor TXMLTag.Destroy;
  {Class destructor - frees owned objects}
begin
  // Free all sub tags
  ClearSubTags;
  // Free owned objects
  fParams.Free;
  fSubTags.Free;
  inherited Destroy;
end;

function TXMLTag.GetParam(const Name: string): string;
  {Read access method for Params property}
begin
  // Value is associated with param name in string list in form "name=value"
  Result := fParams.Values[Name];
end;

function TXMLTag.GetParamAsInt(const Name: string): Integer;
  {Read access method for ParamAsInt property}
var
  Value: string;    // the string value of the parameter
begin
  // Get the string value of the parameter
  Value := Params[Name];
  if Value = '' then
    // Value is '' - return 0
    Result := 0
  else
    // Value is not '', convert to integer - throws exception if not valid int
    Result := StrToInt(Value);
end;

function TXMLTag.GetSubTag(const Idx: Integer): TXMLTag;
  {Read access method for SubTags property}
begin
  // Retrieve sub tag from list
  Result := TXMLTag(fSubTags[Idx]);
end;

function TXMLTag.GetSubTagCount: Integer;
  {Read access method for SubTagCount property}
begin
  Result := fSubTags.Count;
end;

procedure TXMLTag.Read(const Tokeniser: TTokeniser);
  {Reads tag and its sub-tags from file using given tokeniser}
begin
  // Clear parameters, subtags and plain text
  Clear;
  // Check that the expected opening tag is present and record it if so
  if (Tokeniser.Token <> tkOpenTag) then
    raise EXMLTag.Create(sOpenTagExpected);
  if (Tokeniser.Tag <> fTag) then
    raise EXMLTag.CreateFmt(sTagExpected, [Tag]);
  // Skip opening tag
  Tokeniser.NextToken;
  // Process tags until closing tag is found
  while Tokeniser.Token <> tkCloseTag do
  begin
    case Tokeniser.Token of
      tkParam: ReadParam(Tokeniser);
      tkText: ReadText(Tokeniser);
      tkOpenTag: ReadSubTag(Tokeniser);
      tkEOF:
        raise EXMLTag.CreateFmt(sUnexpectedEOF, [Tag]);
    end;
  end;
  // Check that correct matching closing tag is present
  if Tokeniser.Tag <> Tag then
    raise EXMLTag.CreateFmt(sClosingTagExpected, [Tag, Tokeniser.Tag]);
  // Skip closing tag
  Tokeniser.NextToken;
end;

procedure TXMLTag.ReadParam(const Tokeniser: TTokeniser);
  {Reads a tag parameter from the tokeniser. Assumes that current token is a
  parameter}
begin
  Assert(Tokeniser.Token = tkParam);
  // Record new value for parameter
  fParams.Values[Tokeniser.ParamName] := Tokeniser.ParamValue;
  // Fetch next token
  Tokeniser.NextToken;
end;

procedure TXMLTag.ReadSubTag(const Tokeniser: TTokeniser);
  {Recursively reads a sub-tag and all its contents from the tokeniser. Assumes
  that current token is an opening tag}
var
  SubTag: TXMLTag;            // sub-tag object
begin
  Assert(Tokeniser.Token = tkOpenTag);
  // Create sub-tag with required name and add to list
  SubTag := TXMLTag.Create(Tokeniser.Tag);
  fSubTags.Add(SubTag);
  // Get sub-tag to read itself in
  SubTag.Read(Tokeniser);
end;

procedure TXMLTag.ReadText(const Tokeniser: TTokeniser);
  {Reads a tag's plain text from tokeniser. Assumes text is current token}
begin
  Assert(Tokeniser.Token = tkText);
  // Record plain text value and skip to next token
  fPlainText := Tokeniser.TextValue;
  Tokeniser.NextToken;
end;

procedure TXMLTag.SetParam(const Name, Value: string);
  {Write access method for Params property}
begin
  fParams.Values[Name] := Value;
end;

procedure TXMLTag.SetParamAsInt(const Name: string; const Value: Integer);
  {Write access method for ParamAsInt property}
begin
  // Convert value to string and store in parameter string list
  SetParam(Name, IntToStr(Value));
end;

procedure TXMLTag.Write(const Writer: TCharStreamWriter);
  {Write tag and all its sub-tags to XML file using given writer}
var
  I: Integer;                       // loop control
  ParamName, ParamValue: string;    // name and value of a parameter
begin
  // Write opening tag with params
  Writer.WriteText('<' + Tag);
  for I := 0 to Pred(fParams.Count) do
  begin
    ParamName := fParams.Names[I];
    ParamValue := fParams.Values[ParamName];
    Writer.WriteText(' ' + ParamName + '="' + ParamValue + '"');
  end;
  Writer.WriteText('>');
  // Write plain text
  Writer.WriteText(fPlainText);
  // Write sub-tags
  if SubTagCount > 0 then
    Writer.WriteEOL;
  for I := 0 to Pred(SubTagCount) do
    SubTags[I].Write(Writer);
  // Write closing tag
  Writer.WriteTextLine('</' + Tag + '>');
end;

end.

