{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegUtils.pas
  @COMMENTS                 Unit provides a set of routines for manipulating and
                            validating registry keys and value names.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
    )
    @REVISION(
      @VERSION              1.2
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
 * The Original Code is URegUtils.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegUtils;


interface


uses
  // Delphi
  SysUtils, Windows;

const
  // Error codes returned by validation routines
  REGERR_INVALIDROOTKEY = 1;      // root key not recognised
  REGERR_PATHCHARINKEYNAME = 2;   // key name contains path character
  REGERR_BADPATH = 3;             // key does not contain full path
  REGERR_BADCHARSINKEYNAME = 4;   // invalid characters in key name
  REGERR_BADCHARSINVALUENAME = 5; // invalid characters in value name


function ValidateRegKeyPath(const S: string): Integer;
  {Validates the given full registry key path and returns 0 if OK and non-zero
  error code on error. The path must begin with a recognised root key name and
  must contain at least one other key name, separated by '\'}

function ValidateRegRootKeyStr(const Root: string): Integer;
  {Checks that the given root key name is valid (i.e. is one of the recognised
  root key names). Returns 0 if OK and a non-zero edit code on error}

function ValidateRegKeyName(const S: string): Integer;
  {Checks the given registry key name to ensure it contains only valid
  characters and returns 0 if all OK and non-zero error code on error}

function ValidateRegValueName(const S: string): Integer;
  {Checks the given registry value name and returns 0 if OK and non-zero error
  code if the name is not valid}

function RegUtilCheck(Res: Integer): Boolean;
  {Checks given return value (which should have come from one of this unit's
  Validate* routines) and raises an ERegUtils exception with appropriate message
  for and error code any non-zero value of Res. Returns true if Res is 0}

function NormaliseKeyName(const S: string): string;
  {Returns a "normalised" version of the given key name. The routine replaces
  any consecutive sequence of backslash characters with a single character,
  removes any trailing backslashes and trims leading and trailing spaces}

function RegRootKeyStrToKey(const AName: string): HKEY;
  {Return key value associated with a given root key name. If root key is not
  known then 0 is returned}

function RegRootKeyToStr(Code: HKEY): string;
  {Returns the symbolic name associated with the given regsitry root key or ''
  if key code is not known}


type

  {
  ERegUtils:
    Class of exception raised by RegUtilCheck routine: ErrorCode property
    contains the error code related to the error message.
  }
  ERegUtils = class(Exception)
  private
    fErrorCode: Integer;
  public
    property ErrorCode: Integer read fErrorCode write fErrorCode;
      {Error code relating to error message}
  end;


implementation


uses
  // Project
  UStringProcs;


function RegUtilCheck(Res: Integer): Boolean;
  {Checks given return value (which should have come from one of this unit's
  Validate* routines) and raises an ERegUtils exception with appropriate message
  for and error code any non-zero value of Res. Returns true if Res is 0}
var
  Msg: string;  // error message used for exception
  E: ERegUtils; // exception object
resourcestring
  // Error messages
  sUnknownRootKeyName = 'Unrecognised root key name';
  sKeyNameHasSpecialSymbol = 'Key name contains ''\'' symbol';
  sFullKeyPathExpected = 'Full key path expected';
  sBadCharsInKeyName = 'Invalid characters in key name';
  sBadCharsInValueName = 'Invalid characters in value name';
begin
  if Res <> 0 then
  begin
    // Result is error code: raise exception
    // record required message
    Msg := '';
    case Res of
      REGERR_INVALIDROOTKEY: Msg := sUnknownRootKeyName;
      REGERR_PATHCHARINKEYNAME: Msg := sKeyNameHasSpecialSymbol;
      REGERR_BADPATH: Msg := sFullKeyPathExpected;
      REGERR_BADCHARSINKEYNAME: Msg := sBadCharsInKeyName;
      REGERR_BADCHARSINVALUENAME: Msg := sBadCharsInValueName;
    end;
    // Raise exception with required message and error code
    E := ERegUtils.Create(Msg);
    E.ErrorCode := Res;
    raise E;
  end;
  // Result is valid: return true
  Result := True
end;

function IsValidRegStr(const S: string): Boolean;
  {Returns true if given string contains only characters that are permitted in
  registry key or name strings, and returns false otherwise}
const
  // Set of characters that can't appear in registry keys and value names
  cInvalidRegStrChars = [#0, #10, #13];
var
  Idx: Integer; // loops thru chars in string
begin
  Result := True;
  for Idx := 1 to Length(S) do
  begin
    if S[Idx] in cInvalidRegStrChars then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function NormaliseKeyName(const S: string): string;
  {Returns a "normalised" version of the given key name. The routine replaces
  any consecutive sequence of backslash characters with a single character,
  removes any trailing backslashes and trims leading and trailing spaces}
var
  Idx: Integer;     // loops thru all chars in string
  Count: Integer;   // counts duplicate backslashes in string
  PRes: PChar;      // points to characters in result string
begin
  // Initialise
  // zero count of duplicate backslashes
  Count := 0;
  // set initial length of result to same as input string and set pointer
  SetLength(Result, Length(S));
  PRes := PChar(Result);
  // Loop through each character in string
  Idx := 1;
  while Idx <= Length(S) do
  begin
    if S[Idx] = '\' then
    begin
      // We've found a backslash: check to see if there's a sequence of them
      Inc(Idx);
      // store first backslash in output string
      PRes^ := '\';
      Inc(PRes);
      // skip over any following backslashes, counting them
      while (Idx <= Length(S)) and (S[Idx] = '\') do
      begin
        Inc(Count);
        Inc(Idx);
      end;
    end
    else
    begin
      // No a backslash: copy unchanged to output string
      PRes^ := S[Idx];
      Inc(PRes);
      Inc(Idx);
    end;
  end;
  // Set length of result string to acutal number of chars written to it
  SetLength(Result, Length(S) - Count);
  // Trim spaces and remove any trailing backslash
  Result := Trim(Result);
  if (Result <> '') and (Result[Length(Result)] = '\') then
    Delete(Result, Length(Result), 1);
end;

function ValidateRegKeyName(const S: string): Integer;
  {Checks the given registry key name to ensure it contains only valid
  characters and returns 0 if all OK and non-zero error code on error. Note this
  routine checks single key names, not key paths}
begin
  // Check key name contains valid characters
  if IsValidRegStr(S) then
  begin
    // Check key name does not contain '\' characters (this is path delimiter)
    if AnsiPos('\', S) = 0 then
      Result := 0
    else
      Result := REGERR_PATHCHARINKEYNAME
  end
  else
    Result := REGERR_BADCHARSINKEYNAME;
end;

function ValidateRegKeyPath(const S: string): Integer;
  {Validates the given full registry key path and returns 0 if OK and non-zero
  error code on error. The path must begin with a recognised root key name and
  must contain at least one other key name, separated by '\'}
var
  Dummy,        // unused (but required) parameter to SplitStr routine
  Root: string; // root key that begins  the key path
begin
  // Check that only valid characters are present in string
  if IsValidRegStr(S) then
  begin
    // Split out the root key name and check for validity
    if not UStringProcs.SplitStr(S, '\', Root, Dummy) then
      Result := REGERR_BADPATH
    else
      Result := ValidateRegRootKeyStr(Root);
  end
  else
    Result := REGERR_BADCHARSINKEYNAME;
end;

function ValidateRegValueName(const S: string): Integer;
  {Checks the given registry value name and returns 0 if OK and non-zero error
  code if the name is not valid}
begin
  // Check if name contains only valid registry key and value name characters
  if IsValidRegStr(S) then
    Result := 0
  else
    Result := REGERR_BADCHARSINVALUENAME;
end;

function ValidateRegRootKeyStr(const Root: string): Integer;
  {Checks that the given root key name is valid (i.e. is one of the recognised
  root key names). Returns 0 if OK and a non-zero edit code on error}
begin
  // Check if root key exists in lookup table
  if RegRootKeyStrToKey(Root) <> 0 then
    Result := 0
  else
    Result := REGERR_INVALIDROOTKEY;
end;

const
  // Lookup table used to convert between root key names and values
  cLookup: array[1..7] of record
    Name: string; // the root key name
    Key: HKEY;    // the root key value
  end = (
    (Name: 'HKEY_CLASSES_ROOT';      Key: HKEY_CLASSES_ROOT),
    (Name: 'HKEY_CURRENT_USER';      Key: HKEY_CURRENT_USER),
    (Name: 'HKEY_LOCAL_MACHINE';     Key: HKEY_LOCAL_MACHINE),
    (Name: 'HKEY_USERS';             Key: HKEY_USERS),
    (Name: 'HKEY_PERFORMANCE_DATA';  Key: HKEY_PERFORMANCE_DATA),
    (Name: 'HKEY_CURRENT_CONFIG';    Key: HKEY_CURRENT_CONFIG),
    (Name: 'HKEY_DYN_DATA';          Key: HKEY_DYN_DATA)
  );

function RegRootKeyStrToKey(const AName: string): HKEY;
  {Return key value associated with a given root key name. If root key is not
  known then 0 is returned}
var
  I: Integer;   // loops thru lookup table
begin
  Result := 0;
  for I := Low(cLookup) to High(cLookup) do
    if AName = cLookup[I].Name then
    begin
      Result := cLookup[I].Key;
      Break;
    end;
end;

function RegRootKeyToStr(Code: HKEY): string;
  {Returns the symbolic name associated with the given regsitry root key or ''
  if key code is not known}
var
  I: Integer;   // loops through all keys
begin
  Result := '';
  for I := Low(cLookup) to High(cLookup) do
    if cLookup[I].Key = Code then
    begin
      Result := cLookup[I].Name;
      Break;
    end;
end;

end.
