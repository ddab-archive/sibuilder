{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UCharStreamWriter.pas
  @COMMENTS                 This unit defines a class that writes characters and
                            text to a given stream using buffered output.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/08/2000
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
 * The Original Code is UCharStreamWriter.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UCharStreamWriter;

interface

uses
  // Delphi
  Classes;


type
  TCharStreamWriter = class(TObject)
    {Writes characters and text to a given stream using buffered output}
  private
    FStream: TStream;
      {The current stream we're writing to}
    FBuffer: PChar;
      {Write buffer}
    FCsr: Integer;
      {Cursor into write buffer - represents number of characters currently in
      buffer}
  public
    constructor Create(const Stream: TStream);
      {Class constructor - records reference to stream and allocates write
      buffer}
    destructor Destroy; override;
      {Class destructor - flushes buffer before freeing it}
    procedure WriteChar(const Ch: Char);
      {Writes the given character to the write buffer. The buffer is written to
      the stream when it is full}
    procedure WriteEOL;
      {Writes end of line characters}
    procedure WriteText(const TheText: string);
      {Writes the given text}
    procedure WriteTextLine(const TheLine: string);
      {Writes the given text followed by end-of-line markers}
    procedure Flush;
      {Writes the contents of the buffer to file}
  end;

implementation

const
  CBufSize = 4096;    // Default size of the cache buffer

{ TCharStreamWriter }

constructor TCharStreamWriter.Create(const Stream: TStream);
  {Class constructor - records reference to stream and allocates write buffer}
begin
  inherited Create;
  // Record given stream and current position in stream, and no chars read yet
  FStream := Stream;
  // Allocate buffer
  GetMem(FBuffer, CBufSize);
  FCsr := 0;
end;

destructor TCharStreamWriter.Destroy;
  {Class destructor - flushes buffer before freeing it}
begin
  // Flush buffer
  Flush;
  // Free buffer - don't try to free - user may have closed stream
  FreeMem(FBuffer, CBufSize);
  inherited Destroy;
end;

procedure TCharStreamWriter.Flush;
  {Writes the contents of the buffer to file}
begin
  // Write the buffer providing there's something to write!
  if (FCsr > 0) then
    FStream.WriteBuffer(FBuffer^, FCsr);
  // Move cursor to start of buffer - effectively emptying it
  FCsr := 0;
end;

procedure TCharStreamWriter.WriteChar(const Ch: Char);
  {Writes the given character to the write buffer. The buffer is written to the
  stream when it is full}
begin
  // Check if buffer is full and write it out if so
  if FCsr >= CBufSize then
    Flush;
  // Store character in buffer and move cursor to next position
  FBuffer[FCsr] := Ch;
  Inc(FCsr);
end;

procedure TCharStreamWriter.WriteEOL;
  {Writes end of line characters}
begin
  WriteChar(#13);
  WriteChar(#10);
end;

procedure TCharStreamWriter.WriteText(const TheText: string);
  {Writes the given text}
var
  I: Integer;   // loops thru text string
begin
  // Write each character in the string
  for I := 1 to Length(TheText) do
    WriteChar(TheText[I]);
end;

procedure TCharStreamWriter.WriteTextLine(const TheLine: string);
  {Writes the given text followed by end-of-line markers}
begin
  WriteText(TheLine);
  WriteEOL;
end;

end.
