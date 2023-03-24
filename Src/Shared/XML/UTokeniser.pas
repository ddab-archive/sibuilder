{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UTokeniser.pas
  @COMMENTS                 This unit defines a class that parses a cahracter
                            stream coverting the text into XML tokens.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/08/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literal with resource string.
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
 * The Original Code is UTokeniser.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UTokeniser;

interface

uses
  // Delphi
  Classes, SysUtils,
  // Project
  UCharStreamReader;

type

  {
  ETokeniser:
    Exceptions raised by TTokeniser.
  }
  ETokeniser = class(Exception);

  {
  TToken:
    Tokens recognised by TTokeniser.
  }
  TToken = (tkEOF, tkOpenTag, tkCloseTag, tkParam, tkText);

  {
  TTokeniser:
    Reads XML tokens from a stream.
  }
  TTokeniser = class(TObject)
  private // properties
    fToken: TToken;
    fTag: string;
    fParamName: string;
    fParamValue: string;
    fTextValue: string;
  private
    fReader: TCharStreamReader;
      {Object that reads characters from stream}
    fCh: Char;
      {The current character in the stream}
    fInTag: Boolean;
      {Flag true if we are currently "inside" a tag}
    procedure ReadTag;
      {Reads an opening or closing tag from stream}
    procedure CloseTag;
      {"Closes" off a tag by consuming closing '>' and following white space}
    procedure ReadParam;
      {Reads a parameter and its value from stream}
    procedure ReadText;
      {Reads free text from stream}
  public
    constructor Create(const Stream: TStream);
      {Class constructor - creates stream reader object and consumes leading
      white space from stream}
    destructor Destroy; override;
      {Class destructor - closes and frees reader object}
    function NextToken: TToken;
      {Fetches next token from stream and returns it}
    property Token: TToken read fToken;
      {The token last read from stream}
    property Tag: string read fTag;
      {The tag name if last token read was a tag, undefined otherwise}
    property ParamName: string read fParamName;
      {The name of the parameter if last token was a parameter, undefined
      otherwise}
    property ParamValue: string read fParamValue;
      {The value of the parameter if last token was a parameter, undefined
      otherwise}
    property TextValue: string read fTextValue;
      {The text if last token read was free text, undefined otherwise}
  end;


implementation


resourcestring
  // Error messages
  sParamNotInDblQuotes = 'Parameter values must be enclosed in double quotes';


{ TTokeniser }

procedure TTokeniser.CloseTag;
  {"Closes" off a tag by consuming closing '>' and following white space}
begin
  Assert(fCh = '>');
  repeat
    fCh := fReader.NextChar;
  until fCh <> ' ';
  fInTag := False;
end;

constructor TTokeniser.Create(const Stream: TStream);
  {Class constructor - creates stream reader object and consumes leading white
  space from stream}
begin
  inherited Create;
  // Create reader for stream and intialise it
  fReader := TCharStreamReader.Create(Stream);
  // Skip any leading white space
  repeat
    fCh := fReader.NextChar;
  until fCh <> ' ';
  // Fetch first token
  NextToken;
end;

destructor TTokeniser.Destroy;
  {Class destructor - closes and frees reader object}
begin
  fReader.Free;
  inherited Destroy;
end;

function TTokeniser.NextToken: TToken;
  {Fetches next token from stream and returns it}
begin
  case fCh of
    #0:
      fToken := tkEOF;
    '<':
      ReadTag;
    else
      if fInTag then
        ReadParam
      else
        ReadText;
  end;
  Result := fToken;
end;

procedure TTokeniser.ReadParam;
  {Reads a parameter and its value from stream}
begin
  Assert(not (fCh in [' ', #0]));  // fCh contains first char of param name
  // Reset param name and value
  fParamName := '';
  fParamValue := '';
  // Read param name - to first white space, = or tag end
  while not (fCh in [' ', '>', '=', #0]) do
  begin
    fParamName := fParamName + fCh;
    fCh := fReader.NextChar;
  end;
  // Skip white space after name
  while fCh = ' ' do
    fCh := fReader.NextChar;
  // Check to see if a value follows
  if fCh = '=' then
  begin
    // Skip equals sign and following white space
    repeat
      fCh := fReader.NextChar;
    until fCh <> ' ';
    // We should now have the parameter
    if fCh <> '"' then
      raise ETokeniser.Create(sParamNotInDblQuotes);
    // Read parameter
    fCh := fReader.NextChar;
    while not (fCh in ['"', #0]) do
    begin
      fParamValue := fParamValue + fCh;
      fCh := fReader.NextChar;
    end;
    // Skip closing quote and any following white space
    if fCh = '"' then
    begin
      repeat
        fCh := fReader.NextChar
      until fCh <> ' ';
    end;
  end;
  // Close tag if we've got to end
  if fCh = '>' then
    CloseTag;
  fToken := tkParam;
end;

procedure TTokeniser.ReadTag;
  {Reads an opening or closing tag from stream}
begin
  Assert(fCh = '<');
  // Note we're in tag and prepare to read it
  fInTag := True;
  fCh := fReader.NextChar;
  // Decide if opening or closing tag
  if fCh = '/' then
  begin
    fToken := tkCloseTag;
    fCh := fReader.NextChar;
  end
  else
    fToken := tkOpenTag;
  fTag := '';
  // Read tag name
  while not (fCh in [#0, ' ', '>']) do
  begin
    fTag := fTag + fCh;
    fCh := fReader.NextChar;
  end;
  // Skip white space after tag name
  while fCh = ' ' do
    fCh := fReader.NextChar;
  // Close tag if we've reached '>'
  if fCh = '>' then
    CloseTag;
end;

procedure TTokeniser.ReadText;
  {Reads free text from stream}
begin
  fTextValue := '';
  // Read characters from stream until EOF or a tag are found
  while not (fCh in ['<', #0]) do
  begin
    fTextValue := fTextValue + fCh;
    fCh := fReader.NextChar;
  end;
  fToken := tkText;
end;

end.

