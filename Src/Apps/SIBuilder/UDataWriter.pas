{ ##       
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UDataWriter.pas
  @COMMENTS                 Defines a class that compresses and stores files in
                            a single data file, compressing them using a
                            compressor object and makes available the file's
                            location, size and compression status. Ensures that
                            any file is only stored in list once.
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
 * The Original Code is UDataWriter.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UDataWriter;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfCompressor;


type

  {
  TDataWriter:

    A class that compresses and stores files in a single data file, compressing
    them using a compressor object and makes available the file's location,
    size and compression status. Ensures that any file is only stored in list
    once.

    Inheritance: TDataWriter -> [TObject]
  }
  TDataWriter = class(TObject)
  private
    fDataStream: TStream;
    fCompressor: ICompressor;
    fFileList: TStringList;
    procedure WriteFile(const FileName: string; out Position,
      StoredSize: Integer; out IsCompressed: Boolean);
      {Writes file contents onto data stream, compressing as necessary the using
      the compressor object. Sets IsCompressed to true if file was actually
      compressed and false if not. Passes number of bytes written to stream back
      in StoredSize parameter. Returns position of start of file in stream in
      Position}
  public
    procedure Store(const FileName: string; out Position, StoredSize: Integer;
      out IsCompressed: Boolean);
      {Checks if the given file is already stored in data stream. If not the
      file is written to the stream, compressing with the compressor object if
      necessary. If the file is already in the stream it is not added. In either
      case file's (existing or new) position in the data stream, its (possibly
      compressed) size and whether is was compressed are returned via the out
      parameters}
    constructor Create(const DataFile: string;
      const Compressor: ICompressor);
      {Class constructor: the name of the file to which the object is to write
      files along with the object to use to compress files are provided as
      parameters. Records compressor object, opens stream onto file and creates
      object to store list of files written to stream}
    destructor Destroy; override;
      {Class destructor: disposes of list of files written and the associated
      data records and closes the data file stream}
  end;


implementation


uses
  // Delphi
  SysUtils, ActiveX,
  // PJSoft library
  PJIStreams,
  // Project
  UFileProcs;

type

  {
  TFileStats:
    Definition of record for storing information about files added to data
    stream. A record of this type is with the name of each file stored in
    stream in a TStringList object.
  }
  TFileStats = record
    IsCompressed: Boolean;  // whether file is compressed
    StoredSize: Integer;    // size of (possibly compressed) file in stream
    Position: Integer;      // position of file in stream
  end;

  {
  PFileStats:
    Pointer to TFileStats record.
  }
  PFileStats = ^TFileStats;


{ TDataWriter }

constructor TDataWriter.Create(const DataFile: string;
  const Compressor: ICompressor);
  {Class constructor: the name of the file to which the object is to write files
  along with the object to use to compress files are provided as parameters.
  Records compressor object, opens stream onto file and creates object to store
  list of files written to stream}
begin
  inherited Create;
  fDataStream := TFileStream.Create(DataFile, fmCreate);
  fCompressor := Compressor;
  fFileList := TStringList.Create;
end;

destructor TDataWriter.Destroy;
  {Class destructor: disposes of list of files written and the associated data
  records and closes the data file stream}
var
  Idx: Integer; // loops thru all files added to stream
begin
  // Free all file information records from file list
  for Idx := Pred(fFileList.Count) downto 0 do
    Dispose(PFileStats(fFileList.Objects[Idx]));
  // Destroy the file list
  fFileList.Free;
  // Close the data file stream
  fDataStream.Free;
  inherited;
end;

procedure TDataWriter.Store(const FileName: string; out Position,
  StoredSize: Integer; out IsCompressed: Boolean);
  {Checks if the given file is already stored in data stream. If not the file is
  written to the stream, compressing with the compressor object if necessary. If
  the file is already in the stream it is not added. In either case file's
  (existing or new) position in the data stream, its (possibly compressed) size
  and whether is was compressed are returned via the out parameters}
var
  Idx: Integer;           // index of file in file list
  FileStats: PFileStats;  // points to information about the file
begin
  // Lookup file in list of files already installed
  Idx := fFileList.IndexOf(FileName);
  if Idx = -1 then
  begin
    // File has not been installed: install it, recording where placed
    // allocate storage
    New(FileStats);
    // write the file recording positions
    WriteFile(FileName, FileStats^.Position, FileStats^.StoredSize,
      FileStats^.IsCompressed);
    // add file and details to list
    fFileList.AddObject(FileName, Pointer(FileStats));
  end
  else
    // File has been installed - get location, size and compression status
    FileStats := PFileStats(fFileList.Objects[Idx]);
  // Return required values
  StoredSize := FileStats^.StoredSize;
  Position := FileStats^.Position;
  IsCompressed := FileStats^.IsCompressed;
end;

procedure TDataWriter.WriteFile(const FileName: string; out Position,
  StoredSize: Integer; out IsCompressed: Boolean);
  {Writes file contents onto data stream, compressing as necessary the using the
  compressor object. Sets IsCompressed to true if file was actually compressed
  and false if not. Passes number of bytes written to stream back in StoredSize
  parameter. Returns position of start of file in stream in Position}

  // ---------------------------------------------------------------------------
  procedure CopyStream(SrcStream: TStream);
    {Copy given source stream to the data stream}
  begin
    fDataStream.CopyFrom(SrcStream, 0);
  end;

  procedure CopyFileToStream;
    {Copy the file to data stream}
  var
    InStream: TFileStream;    // input file stream
  begin
    // Open input stream on file
    InStream := TFileStream.Create(FileName, fmOpenRead);
    try
      // Copy file to data stream
      CopyStream(InStream);
    finally
      // Close the file
      InStream.Free;
    end;
  end;

  procedure CompressFileToTempStream(TempStream: TStream);
    {Compresses file onto the given temporary stream using the compressor}
  var
    InStm: IStream;           // IStream interface to file being compressed
    OutStm: IStream;          // IStream wrapper to temp stream
    FileStream: TFileStream;  // Stream onto file being compressed
    FileSize: Integer;        // Size of file being compressed
  begin
    // Open file stream and provide IStream wrapper to file and record its size
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    FileSize := FileStream.Size;
    InStm := TPJIStreamWrapper.Create(FileStream, True);
    // Wrap given stream with IStream interface
    OutStm := TPJIStreamWrapper.Create(TempStream);
    // Compress file onto stream using compressor object
    fCompressor.Compress(InStm, OutStm, FileSize);
  end;
  // ---------------------------------------------------------------------------

var
  CompStream: TMemoryStream;  // stores compressed copy of source file
  FileSize: Integer;          // size of uncompressed file
begin
  // Record current postion in stream: this is start of file
  Position := fDataStream.Position;
  // Record size of file
  FileSize := UFileProcs.SizeOfFile(FileName);
  // Check if we have a compressor available
  if Assigned(fCompressor) then
  begin
    // Perform compression and check to see if compression is worth doing
    // create a temporary stream and compress file to it
    CompStream := TMemoryStream.Create;
    try
      CompressFileToTempStream(CompStream);
      CompStream.Position := 0;
      // check if compressed stream is smaller than source file
      IsCompressed := CompStream.Size < FileSize;
      if IsCompressed then
      begin
        // comp stream is smaller: use it and record compressed size
        StoredSize := CompStream.Size;
        CopyStream(CompStream);
      end
      else
      begin
        // comp stream is not smaller: copy file to stream and record file size
        CopyFileToStream;
        StoredSize := FileSize;
      end;
    finally
      CompStream.Free;
    end;
  end
  else
  begin
    // No compressor so just copy file to stream and record size
    CopyFileToStream;
    StoredSize := FileSize;
    IsCompressed := False;
  end;
end;

end.
