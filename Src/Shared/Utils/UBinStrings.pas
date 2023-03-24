{ ##                       
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UBinStrings.pas
  @COMMENTS                 This unit is shared among two or more SITools
                            applications. It provides routines that convert
                            between binary data and string representations of
                            the data.
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
 * The Original Code is UBinStrings.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UBinStrings;

interface


uses
  // Delphi
  SysUtils, Windows;


function NormaliseCommaBinStr(BinStr: string): string;
  {Analyses the given comma separated binary string and returns a normalised
  version of the string. Raises exceptions if the string is badly formed. A
  normalised string has two hex bytes for each byte in buffer, separated by
  commas with no intervening whitespace and no trailing comma}

function NormaliseSpaceBinStr(BinStr: string): string;
  {Analyses the given space separated binary string and returns a normalised
  version of the string. Raises exceptions if the string is badly formed. A
  normalised string has two hex bytes for each byte in buffer, separated by a
  single}

procedure CommaBinStrToBinary(const BinStr: string; out Buffer: PByte;
  out Size: Integer);
  {Parses the given comma delimited binary string and write out the binary data
  represented by the string to a buffer. The buffer is created by the method and
  is passed out to caller, along with buffer size. If there are no bytes or an
  error occurs then the buffer is set to nil and size is set to zero. NOTE: the
  caller is responsible for freeing buffer allocated by this method if not nil,
  by using FreeMem. The method raises exceptions if the binary string is
  invalid. A valid string contains single or double hex characters representing
  bytes, separated by commas. Leading and traling white space and space between
  bytes is permitted. Trailing but not leading commas are allowed}

procedure SpaceBinStrToBinary(BinStr: string; out Buffer: PByte;
  out Size: Integer);
  {Parses the given space delimited binary string and write out the binary data
  represented by the string to a buffer. The buffer is created by the method and
  is passed out to caller, along with buffer size. If there are no bytes or an
  error occurs then the buffer is set to nil and size is set to zero. NOTE: the
  caller is responsible for freeing any non-nil buffer allocated by this method
  using FreeMem. The method raises exceptions if the binary string is invalid. A
  valid string contains single or double hex characters representing bytes,
  separated by white space. Leading and traling white space is permitted}

function BinaryToCommaBinStr(const Buffer: PByte; Size: Integer): string;
  {Returns a normalised comma delimited string representation of the binary data
  of given size in given buffer. The returned string has two hex bytes for each
  byte in buffer, separated by commas with no intervening whitespace and no
  trailing comma}

function BinaryToSpaceBinStr(const Buffer: PByte; Size: Integer): string;
  {Returns a normalised space delimited string representation of the binary data
  of given size in given buffer. The returned string has two hex bytes for each
  byte in buffer, separated by single spaces with leading or trailing space}

function CommaToSpaceBinStr(const CommaStr: string): string;
  {Converts given comma delimited binary string to a normalised space delimited
  binary string. If the comma delimited string is not valid then an exception is
  raised}

function SpaceToCommaBinStr(const SpaceStr: string): string;
  {Converts given space delimited binary string to a normalised comma delimited
  binary string. If the space delimited string is not valid then an exception is
  raised}


type

  {
  EBinStrings:
    Type of exception raised by routines in this unit.
  }
  EBinStrings = class(Exception);


implementation


uses
  // Project
  UStringProcs;

resourcestring
  // Error messages
  sUnexpectedComma =
    'Invalid binary string: hex character expected, comma found';
  sCommaExpected =
    'Invalid binary string: comma expected, hex character found';
  sBadHexChar = 'Invalid binary string: unrecognised character';
  sBadHexSize = 'Invalid binary string: hex value larger than FF';


procedure SpaceBinStrToBinary(BinStr: string; out Buffer: PByte;
  out Size: Integer);
  {Parses the given space delimited binary string and write out the binary data
  represented by the string to a buffer. The buffer is created by the method and
  is passed out to caller, along with buffer size. If there are no bytes or an
  error occurs then the buffer is set to nil and size is set to zero. NOTE: the
  caller is responsible for freeing any non-nil buffer allocated by this method
  using FreeMem. The method raises exceptions in the binary string is invalid. A
  valid string contains single or double hex characters representing bytes,
  separated by white space. Leading and traling white space is permitted}
var
  PBuf: PByte;        // pointer used to write next pos in binary buffer
  NextPos: Integer;   // indexes start of next field to read in binary string
  ByteStr: string;    // string containing and hex char sequence
begin
  // Compress white space in string, and trim leading / trailing space
  BinStr := Trim(CompressWhiteSpace(BinStr));
  // Check if there's anything in string
  if BinStr = '' then
  begin
    Buffer := nil;
    Size := 0;
    Exit;
  end;
  // Required buffer size is number of spaces now in BinStr + 1
  Size := CountDelims(BinStr, ' ') + 1;
  // Allocate buffer and set pointer to start of it
  GetMem(Buffer, Size);
  try
    PBuf := Buffer;
    // Initialise next field position to start of binary string
    NextPos := 1;
    // Read all hex byte string in binary string
    while NextField(BinStr, NextPos, ByteStr, ' ') do
    begin
      // Check if byte string is valid hex and correct size (2 chars maximum)
      if not IsHexStr(ByteStr) then
        raise EBinStrings.Create(sBadHexChar);
      if not (Length(ByteStr) in [1,2]) then
        raise EBinStrings.Create(sBadHexSize);
      // Store hex value in buffer and move to point to next byte in buffer
      PBuf^ := StrToInt('$' + ByteStr);
      Inc(PBuf);
    end;
  except
    // We have error: free the buffer
    FreeMem(Buffer, Size);
    Buffer := nil;
    Size := 0;
  end;
end;

procedure CommaBinStrToBinary(const BinStr: string; out Buffer: PByte;
  out Size: Integer);
  {Parses the given comma delimited binary string and write out the binary data
  represented by the string to a buffer. The buffer is created by the method and
  is passed out to caller, along with buffer size. If there are no bytes or an
  error occurs then the buffer is set to nil and size is set to zero. NOTE: the
  caller is responsible for freeing buffer allocated by this method if not nil,
  by using FreeMem. The method raises exceptions if the binary string is
  invalid. A valid string contains single or double hex characters representing
  bytes, separated by commas. Leading and traling white space and space between
  bytes is permitted. Trailing but not leading commas are allowed}
var
  ActByteCount: Integer;  // count of actual number of bytes in BinStr
  PBuf: PByte;            // pointer used to write next pos in binary buffer
  PBS: PChar;             // pointer used to walk thru binary string
  HexCount: Integer;      // count of hex digits read for current byte
  Value: Byte;            // value corresponding to current hex digits in string
  State: (                // current state of method: identifies what we expect
    stWantHex,            //   we are not reading hex, but expect it next
    stInHex,              //   we are currently reading hex
    stWantComma           //   we have read required hex chars and expect comma
  );

  // ---------------------------------------------------------------------------
  procedure RecordValue;
    {Record current value in buffer and count the bytes}
  begin
    // Store the value in buffer and move pointer on
    PBuf^ := Value;
    Inc(PBuf);
    // Reset value and count of hex chars ready for next byte
    Value := 0;
    HexCount := 0;
    // Count the byte just written
    Inc(ActByteCount);
  end;

  function HexValue(Ch: Char): Byte;
    {Returns value in range 0..15 corresponding to given char if valid hex
    character else 0 is returned}
  begin
    if Ch in ['0'..'9'] then
      Result := Ord(Ch) - Ord('0')
    else if Ch in ['A'..'F'] then
      Result := 10 + Ord(Ch) - Ord('A')
    else if Ch in ['a'..'f'] then
      Result := 10 + Ord(Ch) - Ord('a')
    else
      Result := 0;
  end;

  procedure FreeBuffer;
    {Frees and nils the buffer allocated by the method}
  begin
    FreeMem(Buffer);
    Buffer := nil;
    Size := 0;
  end;
  // ---------------------------------------------------------------------------

begin
  // Initialise count of bytes represented by string
  ActByteCount := 0;
  // Estimate bytes in string by counting delimiters in BinStr: this is maximum
  Size := CountDelims(BinStr, ',') + 1;
  // Reserve a buffer big enough to store binary data, based on size estimate
  GetMem(Buffer, Size);
  try
    // Initialise pointer used to walk output binary buffer and input string
    PBuf := Buffer;
    PBS := PChar(BinStr);
    // Set initial state: we want to read hex character
    State := stWantHex;
    // Initialise hex char count and value ready for first byte
    Value := 0;
    HexCount := 0;
    // Loop through each cahracter in string and process it
    while PBS^ <> #0 do
    begin
      // Act on current character in string
      case PBS^ of
        ' ', #9, #10, #11, #12, #13:
          // We ignore spaces unless in hex string when they signal end byte
          if State = stInHex then
          begin
            // in hex: space -> byte is finished -> record it, comma must follow
            RecordValue;
            State := stWantComma;
          end;
        ',':
        begin
          // If we're expecting hex a comma is an error (can't have 2 together)
          if State = stWantHex then
            raise EBinStrings.Create(sUnexpectedComma);
          // If we're in hex string comma signals end: record it
          if State = stInHex then
            RecordValue;
          // We always expect hex after a comma
          State := stWantHex;
        end;
        '0'..'9', 'A'..'F', 'a'..'f':
        begin
          // If we're expecting a comma hex digit is error
          if State = stWantComma then
            raise EBinStrings.Create(sCommaExpected);
          // Note we're inside a hex byte string
          State := stInHex;
          // Update current byte value and update count of hex digits
          Value := 16 * Value + HexValue(PBS^);
          Inc(HexCount);
          // If we now have two chars -> byte is finished
          // we record byte and expect a following comma
          if HexCount = 2 then
          begin
            RecordValue;
            State := stWantComma;
          end;
        end;
        else
          // Invalid character in string: not hex, comma or space
          raise EBinStrings.Create(sBadHexChar);
      end;
      // Move to next byte in buffer
      Inc(PBS);
    end;
    // We were in hex string at end: need to record final value
    if State = stInHex then
      RecordValue;
    // Update buffer according to actual bytes read
    if ActByteCount = 0 then
    begin
      // now bytes were read: dispose of buffer
      FreeBuffer
    end
    else if Size <> ActByteCount then
    begin
      // actual byte count was less than est size: make buffer smaller
      ReallocMem(Buffer, ActByteCount);
      Size := ActByteCount;
    end;
  except
    // there was exception: free and nil buffer
    FreeBuffer;
    raise;
  end;
end;

function PvtBinaryToDelimitedBinStr(const Buffer: PByte; Size: Integer;
  Delim: AnsiChar): string;
  {Returns a normalised hex string representation of the binary data of given
  size in given buffer with each byte separated with given delimiter character}
var
  Idx: Integer; // loops thru binary data one byte at a time
  PBuf: PByte;  // points to each byte in binary data buffer
  PRes: PChar;  // points to each character in result string
const
  // Array of hex characters used to convert bytes to hex strings
  cHexChars: array[0..15] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f'
  );
begin
  // Set length of result string: 2 chars per byte + separating commas
  SetLength(Result, Size * 3 * SizeOf(AnsiChar) - SizeOf(AnsiChar));
  // Set pointers to output string and to binary data
  PRes := PChar(Result);
  PBuf := Buffer;
  // Scan thru each byte in buffer, writing out text representation of bytes
  for Idx := 1 to Size do
  begin
    if Idx <> 1 then
    begin
      // This is not first byte, so preceed by delimiter
      PRes^ := Delim;
      Inc(PRes);
    end;
    // Store text representation of current byte in output string
    PRes^ := cHexChars[PBuf^ div 16];
    Inc(PRes);
    PRes^ := cHexChars[PBuf^ mod 16];
    Inc(PRes);
    // Move to next item in binary buffer
    Inc(PBuf);
  end;
end;

function BinaryToCommaBinStr(const Buffer: PByte; Size: Integer): string;
  {Returns a normalised comma delimited string representation of the binary data
  of given size in given buffer. The returned string has two hex bytes for each
  byte in buffer, separated by commas with no intervening whitespace and no
  trailing comma}
begin
  Result := PvtBinaryToDelimitedBinStr(Buffer, Size, ',');
end;

function BinaryToSpaceBinStr(const Buffer: PByte; Size: Integer): string;
  {Returns a normalised space delimited string representation of the binary data
  of given size in given buffer. The returned string has two hex bytes for each
  byte in buffer, separated by single spaces with leading or trailing space}
begin
  Result := PvtBinaryToDelimitedBinStr(Buffer, Size, ' ');
end;

function NormaliseSpaceBinStr(BinStr: string): string;
  {Analyses the given space separated binary string and returns a normalised
  version of the string. Raises exceptions if the string is badly formed. A
  normalised string has two hex bytes for each byte in buffer, separated by a
  single}
var
  PBuf: PByte;    // points to binary data defined by string
  Size: Integer;  // size of binary data
begin
  // This method works by converting the given binary string to its binary form
  // then converting this back to comma delited text (which is normalised)
  // Convert comma delinted string to binary (raises exceptions on error)
  SpaceBinStrToBinary(BinStr, PBuf, Size);
  if Assigned(PBuf) then
  begin
    // Everything OK and there is a valid string
    // convert back to text
    Result := BinaryToSpaceBinStr(PBuf, Size);
    // free buffer allocated by CommaBinStrToBinary
    FreeMem(PBuf, Size);
  end
  else
    // There is no buffer -> no string -> return empty string
    Result := '';
end;

function NormaliseCommaBinStr(BinStr: string): string;
  {Analyses the given comma separated binary string and returns a normalised
  version of the string. Raises exceptions if the string is badly formed. A
  normalised string has two hex bytes for each byte in buffer, separated by
  commas with no intervening whitespace and no trailing comma}
var
  PBuf: PByte;    // points to binary data defined by string
  Size: Integer;  // size of binary data
begin
  // This method works by converting the given binary string to its binary form
  // then converting this back to comma delited text (which is normalised)
  // Convert comma delinted string to binary (raises exceptions on error)
  CommaBinStrToBinary(BinStr, PBuf, Size);
  if Assigned(PBuf) then
  begin
    // Everything OK and there is a valid string
    // convert back to text
    Result := BinaryToCommaBinStr(PBuf, Size);
    // free buffer allocated by CommaBinStrToBinary
    FreeMem(PBuf, Size);
  end
  else
    // There is no buffer -> no string -> return empty string
    Result := '';
end;

function CommaToSpaceBinStr(const CommaStr: string): string;
  {Converts given comma delimited binary string to a normalised space delimited
  binary string. If the comma delimited string is not valid then an exception is
  raised}
begin
  Result := SysUtils.StringReplace(
    NormaliseCommaBinStr(CommaStr), ',', ' ', [rfReplaceAll]
  );
end;

function SpaceToCommaBinStr(const SpaceStr: string): string;
  {Converts given space delimited binary string to a normalised comma delimited
  binary string. If the space delimited string is not valid then an exception is
  raised}
begin
   Result := SysUtils.StringReplace(
    NormaliseSpaceBinStr(SpaceStr), ' ', ',', [rfReplaceAll]
   )
end;


end.
