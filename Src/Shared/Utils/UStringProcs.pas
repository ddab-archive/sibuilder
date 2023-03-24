{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UStringProcs.pas
  @COMMENTS                 This unit is shared among two or more SITools
                            applications. It contains utility string processing
                            routines.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version - contains just the NextField
                            function.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Added numerous new string handling functions and
                            procedures: CompressWhiteSpace, IsValidCEscapedStr,
                            CEscapeStr, CUnEscapeStr, SplitStr, IsHexStr &
                            CountDelims.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 23/02/2003
      @COMMENTS             + Added overloaded SplitStr that divides string on
                              separator into list of fields.
                            + Added new JoinStr function to join a string list
                              separated by delimiters.

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
 * The Original Code is UStringProcs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UStringProcs;


interface


uses
  // Delphi
  Classes;


function CompressWhiteSpace(const S: string): string;
  {Returns a copy of given string with all white space characters replaced by
  space characters and all consequence sequences of white space replaced by a
  single space character}

function IsValidCEscapedStr(const S, ValidEscChars: string): Boolean;
  {Returns true if the given string contains only ordinary characters or escaped
  characters (in C form - introduced by '\') that match those past in
  ValidEscChars}

function CEscapeStr(S: string; const EscapeChars,
  EscapableChars: string): string;
  {Returns a copy of string S with each character appearing in EscapableChars
  replaced by escape sequence comprising of the character '\' followed by the
  matching character from EscapeChars}

function CUnEscapeStr(const S, EscapeChars, EscapableChars: string): string;
  {Returns a copy of string S with each C-style escape character introduced by
  '\' and followed by a character from EscapeChars replaced the matching
  character from EscapableChars}

function SplitStr(const S: string; Delim: Char; out S1, S2: string): Boolean;
  overload;
  {Splits the string S at the first occurence of delimiter character Delim and
  sets S1 to the sub-string before Delim and S2 to substring following Delim. If
  Delim is found in string True is returned, while if Delim is not in string
  False is returned, S1 is set to S and S2 is set to ''}

function SplitStr(S: string; const Delim: Char; const List: TStrings;
  const AllowEmpty: Boolean = True): Integer;
  overload;
  {Splits the string S into a list of string separated by Delim and returns the
  number of values in the list. If AllowEmpty is true then any empty strings
  before or after separators are added to the list, while they are ingored if
  AllowEmpty is false}

function JoinStr(const SL: TStrings; const Delim: Char;
  const AllowEmpty: Boolean = True): string;
  {Joins all strings in given string list together into single string separated
  by given delimiter. If AllowEmpty is true then any empty strings are included
  in output string, but are ignored if false}

function IsHexStr(const S: string): Boolean;
  {Returns true if string S contains only valid hex digits, false otherwise}

function CountDelims(const S, Delims: string): Integer;
  {Returns count of all occurences of any of the delimiter characters in Delims
  in the string S}

function NextField(TextLine: string; var StringStart: Integer;
  var Field: string; Separator: Char) : Boolean;
  {Given start postion in string, find next text field, delimited by
  Separator, and return in Field. Returns start of next field in
  StringStart. Return True if a next field was found, False otherwise.}


implementation


uses
  // Delphi
  SysUtils;

type

  {
  TCharSet:
    Set of ansi characters.
  }
  TCharSet = set of AnsiChar;

function PvtContainsValidChars(const S: string;
  const ValidChars: TCharSet): Boolean;
  {Returns true if the given string contains only characters from the ValidChars
  set}
var
  Idx: Integer; //loops thru all characters in string
begin
  Result := True;
  for Idx := 1 to Length(S) do
    if not (S[Idx] in ValidChars) then
    begin
      Result := False;
      Break;
    end;
end;

function IsHexStr(const S: string): Boolean;
  {Returns true if string S contains only valid hex digits, false otherwise}
const
  cHexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
begin
  Result := PvtContainsValidChars(S, cHexChars);
end;

function CountDelims(const S, Delims: string): Integer;
  {Returns count of all occurences of any of the delimiter characters in Delims
  in the string S}
var
  Idx: Integer; //loops thru all characters in string
begin
  Result := 0;
  for Idx := 1 to Length(S) do
    if SysUtils.IsDelimiter(Delims, S, Idx) then
      Inc(Result);
end;

function CompressWhiteSpace(const S: string): string;
  {Returns a copy of given string with all white space characters replaced by
  space characters and all consequence sequences of white space replaced by a
  single space character}
var
  Idx: Integer;       // loops thru all characters in string
  ResCount: Integer;  // counts number of characters in result string
  PRes: PChar;        // pointer to characters in result string
const
  // The white space characters we convert to spaces
  cWhiteSpace = [#9, #10, #11, #12, #13, ' '];
begin
  // Set length of result to length of source string and set pointer to it
  SetLength(Result, Length(S));
  PRes := PChar(Result);
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length(S) do
  begin
    if S[Idx] in cWhiteSpace then
    begin
      // Current char is white space: replace by space char and count it
      PRes^ := ' ';
      Inc(PRes);
      Inc(ResCount);
      // Skip past any following white space
      Inc(Idx);
      while S[Idx] in cWhiteSpace do
        Inc(Idx);
    end
    else
    begin
      // Current char is not white space: copy it literally and count it
      PRes^ := S[Idx];
      Inc(PRes);
      Inc(ResCount);
      Inc(Idx);
    end;
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length(S) then
    SetLength(Result, ResCount);
end;

function SplitStr(const S: string; Delim: Char; out S1, S2: string): Boolean;
  {Splits the string S at the first occurence of delimiter character Delim and
  sets S1 to the sub-string before Delim and S2 to substring following Delim. If
  Delim is found in string True is returned, while if Delim is not in string
  False is returned, S1 is set to S and S2 is set to ''}
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimter in string
  DelimPos := SysUtils.AnsiPos(Delim, S);
  if DelimPos > 0 then
  begin
    // Delimiter found: do split and return True
    S1 := Copy(S, 1, DelimPos - 1);
    S2 := Copy(S, DelimPos + 1, MaxInt);
    Result := True;
  end
  else
  begin
    // Delimeter not found: return false and set S1 to whole string
    S1 := S;
    S2 := '';
    Result := False;
  end;
end;

function SplitStr(S: string; const Delim: Char; const List: TStrings;
  const AllowEmpty: Boolean = True): Integer;
  overload;
  {Splits the string S into a list of string separated by Delim and returns the
  number of values in the list. If AllowEmpty is true then any empty strings
  before or after separators are added to the list, while they are ingored if
  AllowEmpty is false}
var
  Item: string;       // current delimted text
  Remainder: string;  // remaining unconsumed part of string
begin
  // Clear the list
  List.Clear;
  // Check we have some entries in the string
  if S <> '' then
  begin
    // Repeatedly split string until we have no more entries
    while UStringProcs.SplitStr(S, Delim, Item, Remainder) do
    begin
      // Add the current string, is required
      if (Item <> '') or AllowEmpty then
        List.Add(Item);
      // Go round again with remainder of string
      S := Remainder;
    end;
    // Add any terminal item
    if (Item <> '') or AllowEmpty then
      List.Add(Item);
  end;
  // Return number of items read
  Result := List.Count;
end;

function JoinStr(const SL: TStrings; const Delim: Char;
  const AllowEmpty: Boolean = True): string;
  {Joins all strings in given string list together into single string separated
  by given delimiter. If AllowEmpty is true then any empty strings are included
  in output string, but are ignored if false}
var
  Idx: Integer; // loops thru all items in string list
begin
  Result := '';
  for Idx := 0 to Pred(SL.Count) do
  begin
    if (SL[Idx] <> '') or AllowEmpty then
      if Result = '' then
        Result := SL[Idx]
      else
        Result := Result + Delim + SL[Idx];
  end;
end;

function IsValidCEscapedStr(const S, ValidEscChars: string): Boolean;
  {Returns true if the given string contains only ordinary characters or escaped
  characters (in C form - introduced by '\') that match those past in
  ValidEscChars}
var
  Idx: Integer;         // loops thru chars in string
  EscCharPos: Integer;  // position of esc char in ValidEscChars
begin
  // Assume we fail
  Result := False;
  // Scan through all string
  Idx := 1;
  while Idx <= Length(S) do
  begin
    if (S[Idx] = '\') then
    begin
      // We have an escape char
      if Idx = Length(S) then
        // esc char was last: this is error so exit with Result still False
        Exit;
      // skip over escape symbol and test escape char
      Inc(Idx);
      EscCharPos := SysUtils.AnsiPos(S[Idx], ValidEscChars);
      if EscCharPos = 0 then
        // we have bad escape character: exit with Result still False
        Exit;
    end;
    // Move to next character
    Inc(Idx);
  end;
  // If we get here everything's OK
  Result := True;
end;

function CEscapeStr(S: string; const EscapeChars,
  EscapableChars: string): string;
  {Returns a copy of string S with each character appearing in EscapableChars
  replaced by escape sequence comprising of the character '\' followed by the
  matching character from EscapeChars}
var
  EscCount: Integer;    // count of escaped characters in string
  Idx: Integer;         // loops thru string
  PRes: PChar;          // points to chars in result string
  EscCharPos: Integer;  // position of esc chars in EscapeChars & EscapableChars
begin
  // Count escapable characters in string
  EscCount := 0;
  for Idx := 1 to Length(S) do
  begin
    if SysUtils.AnsiPos(S[Idx], EscapableChars) > 0 then
      Inc(EscCount);
  end;
  // Set size of result string and get pointer to it
  SetLength(Result, Length(S) + EscCount);
  PRes := PChar(Result);
  // Replace escapable chars with the escaped version
  for Idx := 1 to Length(S) do
  begin
    EscCharPos := SysUtils.AnsiPos(S[Idx], EscapableChars);
    if EscCharPos > 0 then
    begin
      PRes^ := '\';
      Inc(PRes);
      PRes^ := EscapeChars[EscCharPos];
    end
    else
      PRes^ := S[Idx];
    Inc(PRes);
  end;
  // copy last character (not processed in loop)
  PRes^ := S[Length(S)];
end;

function CUnEscapeStr(const S, EscapeChars, EscapableChars: string): string;
  {Returns a copy of string S with each C-style escape character introduced by
  '\' and followed by a character from EscapeChars replaced the matching
  character from EscapableChars}
var
  EscCount: Integer;    // counts escaped characters in string
  Idx: Integer;         // loops thru source string
  PRes: PChar;          // points to chars in result string
  EscCharPos: Integer;  // position of esc chars in EscapeChars & EscapableChars
begin
  // Count escape sequences
  EscCount := 0;
  Idx := 1;
  while Idx < Length(S) do  // don't count \ as last character
  begin
    if S[Idx] = '\' then
    begin
      Inc(EscCount);
      Inc(Idx);
    end;
    Inc(Idx);
  end;
  // Set length of result string and get pointer to it
  SetLength(Result, Length(S) - EscCount);
  PRes := PChar(Result);
  // Replace escaped chars with literal ones
  Idx := 1;
  while Idx <= Length(S) do
  begin
    // check for escape char (unless last char when treat literally)
    if (S[Idx] = '\') and (Idx <> Length(S)) then
    begin
      // we have an escape char
      // skip over escape symbol (\) to escape char
      Inc(Idx);
      // find position of escape char
      EscCharPos := SysUtils.AnsiPos(S[Idx], EscapeChars);
      if EscCharPos > 0 then
        // we have recognised esc char: replace it with literal
        PRes^ := EscapableChars[EscCharPos]
      else
        // we have not recognised esc char: copy it literally
        PRes^ := S[Idx];
    end
    else
      // not an esc char: copy literally
      PRes^ := S[Idx];
    // move to next character
    Inc(Idx);
    Inc(PRes);
  end;
  // Replace all LF chars with CRLF (i.e. EOL)
  Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
end;

function NextField(TextLine: string; var StringStart: Integer;
  var Field: string; Separator: Char) : Boolean;
  {Given start postion in string, find next text field, delimited by
  Separator, and return in Field. Returns start of next field in
  StringStart. Return True if a next field was found, False otherwise.}
var
  StringEnd: Integer; // end of string
  L: Integer;         // length of string
  Done: Boolean;      // loop termination flag
begin
  // Find length of given line
  L := Length(TextLine);
  // Check if StringStart is beyond length of line
  if StringStart > L then
  begin
    // StringStart is beyond line end - return nul string & false to show no
    // field found
    Field := '';
    Result := False;
  end
  else
  begin
    // StringStart is within line
    // set string end to string start & initialise loop control flag
    StringEnd := StringStart;
    Done := False;
    // loop while string end is within string and separator not found
    while (StringEnd <= L) and not Done do
    begin
      if TextLine[StringEnd] = Separator then
        // we have found separator - halt loop
        Done := True
      else
        // haven't yet found separator - try next string position
        StringEnd := StringEnd + 1;
    end;
    // check if we found separator
    if Done then
      // separator was found - return line from StartString to just before it
      Field := Copy(TextLine, StringStart, StringEnd - StringStart)
    else
      // no separator found - return line from StringStart to end of line
      Field := Copy(TextLine, StringStart, StringEnd - StringStart);
    // Set StringStart for next time to just after StringEnd
    StringStart := StringEnd + 1;
    // Succesful result
    Result := True;
  end;
end;

end.
