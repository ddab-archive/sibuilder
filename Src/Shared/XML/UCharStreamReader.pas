{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UCharStreamReader.pas
  @COMMENTS                 This unit defines a class that reads sequential
                            characters from a given stream using buffered input
                            and converting all white space & EOL characters to
                            spaces.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/08/2000
      @COMMENTS             Original version
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
 * The Original Code is UCharStreamReader.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UCharStreamReader;

interface

uses
  // Delphi
  Classes;

type
  TCharStreamReader = class(TObject)
    {Reads sequential characters from a given stream using buffered input and
    converting all white space & EOL characters to spaces}
  private
    FStream: TStream;
      {The current stream we're reading}
    FStartStreamPos : LongInt;
      {The stream position when reader is created}
    FBuffer: PChar;
      {Read buffer}
    FEnd, FCsr: Integer;
      {Cursors into cache buffer}
    FCharsRead: LongInt;
      {Number of characters read from file}
    procedure ReadBuffer;
      {Fill buffer from file}
  public
    constructor Create(Stream: TStream);
      {Class constructor - initialises object and creates and fills buffer}
    destructor Destroy; override;
      {Class destructor - frees buffer and tidies up stream}
    function NextChar: Char;
      {Returns the next text character from the document stream. All white space
      (inc EOL) characters are converted to spaces. Returns #0 if at EOF}
  end;

implementation

const
  CBufSize = 4096;    // Size of the cache buffer

{ TCharStreamReader }

constructor TCharStreamReader.Create(Stream: TStream);
  {Class constructor - initialises object and creates and fills buffer}
begin
  inherited Create;
  // Record given stream and current position in stream
  FStream := Stream;
  FStartStreamPos := Stream.Position;
  // Record that no chars read yet
  FCharsRead := 0;
  // Allocate buffer and fill it
  GetMem(FBuffer, CBufSize);
  // Fill the buffer
  ReadBuffer;
end;

destructor TCharStreamReader.Destroy;
  {Class destructor - frees buffer and tidies up stream}
begin
  // Ensure stream position represents actual chars read & delete ref to stream
  FStream.Seek(FStartStreamPos + FCharsRead, soFromBeginning);
  FStream := nil;
  // Free any buffer
  FreeMem(FBuffer, CBufSize);
  inherited Destroy;
end;

function TCharStreamReader.NextChar: Char;
  {Returns the next text character from the document stream. All white space
  (inc EOL) characters are converted to spaces. Returns #0 if at EOF}
begin
  // Check if we've reached EOF
  // (we've reached end of stream if buffer's end cursor points before start
  if FEnd > -1 then
  begin
    // Not at EOF
    // read next character from buffer, incrementing position and char count
    Result := FBuffer[FCsr];
    Inc(FCsr);
    Inc(FCharsRead);
    // if char is white space convert it to space character
    if Result in [' ', #9, #10, #11, #12, #13] then
      Result := ' ';
    // check if we've advanced beyond the end of the buffer and refill it if so
    if FCsr > FEnd then
      ReadBuffer;
  end
  else
    // At EOF - return #0 to indicate this
    Result := #0;
end;

procedure TCharStreamReader.ReadBuffer;
  {Fill buffer from file}
begin
  // Read from file into buffer and set end cursor to reference end of buffer
  // this will be -1 if no bytes read
  FEnd := FStream.Read(FBuffer^, CBufSize) - 1;
  // Zero the current character cursor
  FCsr := 0;
end;

end.
