{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UExtractor.pas
  @COMMENTS                 Unit defines an abstract base class for classes used
                            to extract and decompress project installation code
                            and data from an SITools installer.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 16/01/2006
      @COMMENTS             Changed to get file extraction path and name of
                            compressor DLL from object rather than explicitly
                            named strings.
    )
    @REVISION(
      @VERSION              1.2
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
 * The Original Code is UExtractor.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UExtractor;


interface

uses
  // Project
  UInstallerFiles;


type

  {
  TExtractor:
    Abstract base class for classes used to extract and decompress project
    installation code and data from an SITools installer. Data must be stored in
    a standard format. This class provides functionality for Execute method and
    requires descendants to provide code to read data and find data size,
    depending on storage medium used.
  }
  TExtractor = class(TObject)
  protected
    function GetDataSize: LongWord; virtual; abstract;
      {Returns size of data to be extracted}
    procedure ReadData(var Buffer; Count: Integer); virtual; abstract;
      {Reads Count bytes of data into Buffer from data storage medium}
  public
    procedure Execute(const InstallerFiles: TInstallerFiles);
      {Extracts all program and install data from an installer, inflating any
      compressed data using a compressor DLL. InstallerFiles specifies the
      extraction path and the compressor DLL to use}
  end;


implementation


uses
  // Project
  UEmbeddedData, UInflater, UInstProcs, UInstExcept;


procedure WriteBufToFile(const FileName: string; Buf: Pointer; Size: Integer);
  {Writes Size bytes from given buffer to file with given name. If file already
  exists it is overwritten}
var
  F: File;  // untyped file
resourcestring
  // Error message
  sCantCreateFile = 'Can''t create ';
begin
  try
    AssignFile(F, FileName);
    Rewrite(F, 1);
    BlockWrite(F, Buf^, Size);
    CloseFile(F);
  except
    raise TError.Create(sCantCreateFile + FileName);
  end;
end;

{ TExtractor }

procedure TExtractor.Execute(const InstallerFiles: TInstallerFiles);
  {Extracts all program and install data from an installer, inflating any
  compressed data using a compressor DLL. InstallerFiles specifies the
  extraction path and the compressor DLL to use}
var
  DataSize: LongWord;       // size of data to be extracted
  BytesRead: LongWord;      // number of bytes of data read to date
  Hdr: TEmbeddedFileHeader; // header for stored data records
  DataBuf: Pointer;         // buffer to receive data
  Inflater: TInflater;      // inflator object used to inflate data
begin
  // Record path we're extracting to: same as inflater DLL
  // Create inflater object: manages inflater DLL and calls its fn
  Inflater := TInflater.Create(InstallerFiles.InflaterDLLFile);
  try
    // Get size of data we're reading, and init count of bytes read to date
    DataSize := GetDataSize;
    BytesRead := 0;
    while BytesRead < DataSize do
    begin
      // Read file header record
      ReadData(Hdr, SizeOf(Hdr));
      // Read file data into buffer of required size (per header)
      GetMem(DataBuf, Hdr.DataSize);
      try
        ReadData(DataBuf^, Hdr.DataSize);
        // Create file from data buffer
        if Hdr.IsCompressed then
        begin
          // Inflate data and copy to file
          Inflater.InflateToFile(
            InstallerFiles.ExtractPath + Hdr.FileName,
            DataBuf,
            Hdr.DataSize
          );
        end
        else
        begin
          // Simply copy data to file
          WriteBufToFile(
            InstallerFiles.ExtractPath + Hdr.FileName,
            DataBuf,
            Hdr.DataSize
          );
        end;
      finally
        FreeMem(DataBuf);
      end;
      // Update count of bytes read
      BytesRead := BytesRead + SizeOf(Hdr) + Hdr.DataSize;
    end;
  finally
    Inflater.Free;
  end;
end;

end.

