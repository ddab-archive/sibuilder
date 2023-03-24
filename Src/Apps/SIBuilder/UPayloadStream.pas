{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UPayloadStream.pas
  @COMMENTS                 Defines a class that provides a TStream interface to
                            a data payload appended to an executable file.
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
 * The Original Code is UPayloadStream.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UPayloadStream;


interface


uses
  // Delphi
  Classes, SysUtils;

type

  {
  TPayloadOpenMode:
    Open modes for payload streams.
  }
  TPayloadOpenMode = (
    pomRead,    // read mode: stream pos when opened is start of data
    pomWrite    // write (append) mode: stream pos when opened is end of data
  );

  {
  TPayloadStream:
    Provides a TStream interface to a payload appended to an executable file:
    correctly reads / writes payrolls, keeping executable file intact and
    updating payroll footer record as necessary.

    Inheritance: TPayloadStream -> [TStream]
  }
  TPayloadStream = class(TStream)
  private
    fMode: TPayloadOpenMode;
      {Stores open mode for payload stream}
    fOldFileMode: Integer;
      {Stores current file mode before we open our file: used to restore
      original when we close our file}
    fFile: File;
      {Untyped file opened onto file containing payload}
    fDataStart: Integer;
      {Position of start of payload in file: this position is size of original
      executable file}
    fDataSize: Integer;
      {Size of payload data}
  public
    constructor Create(const FileName: string; const Mode: TPayloadOpenMode);
      {Class constructor: opens untyped file on given file name in either read
      or write mode and then finds any existing payload data and moves file
      pointer to skip over executable file - start of payload in read mode and
      end of payload (ready for appending) in write mode}
    destructor Destroy; override;
      {Class destructor: closes file, updating and writing payload footer if in
      write mode}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
      {Moves to a specified position in the payload stream and returns new
      position}
    procedure SetSize(NewSize: LongInt); override;
      {Sets size of payload stream: works only when stream is opened for writing
      and raises exception if this is not the case}
    function Read(var Buffer; Count: LongInt): LongInt; override;
      {Attempts to read Count bytes from stream into given buffer and returns
      number of bytes actually read}
    function Write(const Buffer; Count: LongInt): LongInt; override;
      {Attempts to write Count bytes from Buffer to stream. Returns number of
      bytes actually read. If stream is not open for writing an exception is
      raised}
  end;

  {
  EPayloadStream:
    Class of exception raised by TPayloadStream.

    Inheritance: EPayloadStream -> [Exception]
  }
  EPayloadStream = class(Exception);


implementation


uses
  // Project
  UPayload;

resourcestring
  // Error messages
  sCantWrite = 'TPayloadStream can''t write in read mode.';
  sCantSetSize = 'TPayloadStream can''t change size in read mode.';


{ TPayloadStream }

constructor TPayloadStream.Create(const FileName: string;
  const Mode: TPayloadOpenMode);
  {Class constructor: opens untyped file on given file name in either read or
  write mode and then finds any existing payload data and moves file pointer
  to skip over executable file - start of payload in read mode and end of
  payload (ready for appending) in write mode}
var
  Footer: TPayloadFooter; // footer record for payload data
begin
  inherited Create;
  // Record open mode
  fMode := Mode;
  // Open file using required mode, saving pre-existing file mode
  fOldFileMode := FileMode;
  AssignFile(fFile, FileName);
  case fMode of
    pomRead: FileMode := 0;
    pomWrite: FileMode := 2;
  end;
  Reset(fFile, 1);
  // Determine position for payload
  if ReadPayloadFooter(fFile, Footer) then
  begin
    // There is an existing payload
    // find where payload starts in physical file, and size of data
    fDataStart := Footer.ExeSize;
    fDataSize := Footer.DataSize;
    // move file pointer to appropriate place in file: this is start of payload
    // in read mode and end of payload in write mode
    case fMode of
      pomRead: System.Seek(fFile, fDataStart);
      pomWrite: System.Seek(fFile, fDataStart + fDataSize);
    end;
  end
  else
  begin
    // There is no existing payload:
    // data size is zero
    fDataSize := 0;
    // payload starts at end of executable file
    fDataStart := FileSize(fFile);
  end;
end;

destructor TPayloadStream.Destroy;
  {Class destructor: closes file, updating and writing payload footer if in
  write mode}
var
  Footer: TPayloadFooter; // payload footer record
begin
  if fMode = pomWrite then
  begin
    // We're in write mode: we need to update footer
    if fDataSize > 0 then
    begin
      // We have some data in payload, so need a footer record
      // set up a footer record for payload
      InitFooter(Footer);
      Footer.ExeSize := fDataStart;
      Footer.DataSize := fDataSize;
      // write footer at end of data, as last item in file
      System.Seek(fFile, fDataStart + fDataSize);
      Truncate(fFile);
      BlockWrite(fFile, Footer, SizeOf(Footer));
    end
    else
    begin
      // There is no footer data, so we have no footer record at end of file:
      // truncate file at end of original executable
      System.Seek(fFile, fDataStart);
      Truncate(fFile);
    end;
  end;
  // Close file and restore old file mode
  CloseFile(fFile);
  FileMode := fOldFileMode;
  inherited;
end;

function TPayloadStream.Read(var Buffer; Count: Integer): LongInt;
  {Attempts to read Count bytes from stream into given buffer and returns number
  of bytes actually read}
var
  BytesRead: Integer; // Number of bytes read
begin
  // Read block of data from file at current position
  BlockRead(fFile, Buffer, Count, BytesRead);
  // Return number of bytes read
  Result := BytesRead;
end;

function TPayloadStream.Seek(Offset: Integer; Origin: Word): LongInt;
  {Moves to a specified position in the payload stream and returns new position}
begin
  // Calculate position after move (this is result value)
  case Origin of
    soFromBeginning:
    begin
      // Moving from start of stream
      if Offset >= 0 then
        // +ve offset: new position should be actual offset
        Result := Offset
      else
        // -ve offset: move to start of stream
        Result := 0;
    end;
    soFromEnd:
    begin
      // Moving from end of stream
      if Offset <= 0 then
        // -ve offset: new position is offset bytes from end
        Result := fDataSize + Offset
      else
        // +ve offset: move to end of stream
        Result := fDataSize;
    end;
    else  // soFromCurrent and other values
    begin
      // Moving from current position: current logical place in payload is file
      // position less physical start position of payload
      Result := FilePos(fFile) - fDataStart + Offset;
    end;
  end;
  // Result must be within payload: make sure it is
  if Result < 0 then
    Result := 0;
  if Result > fDataSize then
    Result := fDataSize;
  // Perform actual seek in physical file, adjusting for offset of payload start 
  System.Seek(fFile, fDataStart + Result);
end;

procedure TPayloadStream.SetSize(NewSize: Integer);
  {Sets size of payload stream: works only when stream is opened for writing and
  raises exception if this is not the case}
var
  Pos: Integer; // current position in stream
begin
  // Raise exception if not in write mode
  if fMode <> pomWrite then
    raise EPayloadStream.Create(sCantSetSize);
  // Update size if it has changed
  if NewSize < fDataSize then
  begin
    // record current position in stream
    Pos := Position;
    // update size of data to new value
    fDataSize := NewSize;
    // if old position now beyond end of stream set it to end of stream 
    if Pos > fDataSize then
      Position := fDataSize;
  end;
end;

function TPayloadStream.Write(const Buffer; Count: Integer): LongInt;
  {Attempts to write Count bytes from Buffer to stream. Returns number of bytes
  actually read. If stream is not open for writing an exception is raised}
var
  BytesWritten: Integer;  // number of bytes actually written
  Pos: Integer;           // position in stream
begin
  // Raise exception if not in write mode
  if fMode <> pomWrite then
    raise EPayloadStream.Create(sCantWrite);
  // Write the data, recording bytes read
  BlockWrite(fFile, Buffer, Count, BytesWritten);
  Result := BytesWritten;
  // Check if we've written beyond current end of file
  // find new file postion
  Pos := FilePos(fFile);
  if Pos - fDataStart > fDataSize then
    // new position beyond size of stream, so update stream size
    fDataSize := Pos - fDataStart;
end;

end.
