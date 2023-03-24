{ ##                
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UPackageExtractor.pas
  @COMMENTS                 Unit defines a class used to extract and decompress
                            project installation code and data from the data
                            payload appended to a SITools packaged installer.
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
 * The Original Code is UPackageExtractor.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UPackageExtractor;


interface


uses
  // Project
  UExtractor;

type

  {
  TPackageExtractor:
    Class used to extract and decompress project installation code and data from
    the package of data appended to a SITools packaged installer as a paylod.
    The class is derived from an abstract base class - this class provides code
    that can access payload data.

    Inheritance: TPackageExtractor -> TExtractor -> [TObject]
  }
  TPackageExtractor = class(TExtractor)
  private
    fFile: File;
      {Untyped file used to read payload data}
    fSize: Integer;
      {Size of paylod data}
    fPosition: Integer;
      {Current position in payload data}
  protected
    function GetDataSize: LongWord; override;
      {Returns size of payload data to be extracted}
    procedure ReadData(var Buffer; Count: Integer); override;
      {Reads Count bytes of data into Buffer from payload data}
  public
    constructor Create(const PackagedFile: string);
      {Class constructor: opens given packaged file and intialises payload data}
    destructor Destroy; override;
      {Class destructor: close payload file}
  end;


implementation


uses
  // Project
  UPayload;

{ TPackageExtractor }

constructor TPackageExtractor.Create(const PackagedFile: string);
  {Class constructor: opens given packaged file and intialises payload data}
const
  cReadOnlyMode = 0;  // read only file mode flag
begin
  inherited Create;
  {$I-}   // no exceptions on file errors
  // Open packaged file for reading
  AssignFile(fFile, PackagedFile);
  FileMode := cReadOnlyMode;
  Reset(fFile, 1);
  if IOResult = 0 then
    // File opened OK: initialise
    fSize := UPayload.InitPayload(fFile)
  else
    // File couldn't be opened: assume file size is zero to prevent reading
    fSize := 0;
  // Current position is start of file
  fPosition := 0;
  {$I+}   // exceptions on file errors
end;

destructor TPackageExtractor.Destroy;
  {Class destructor: close payload file}
begin
  {$I-}   // no exceptions on file errors
  // Close file if if was opened: ignore error if file wasn't opened
  CloseFile(fFile);
  {$I+}   // exceptions on file errors
  inherited;
end;

function TPackageExtractor.GetDataSize: LongWord;
  {Returns size of payload data to be extracted}
begin
  Result := fSize;
end;

procedure TPackageExtractor.ReadData(var Buffer; Count: Integer);
  {Reads Count bytes of data into Buffer from payload data}
begin
  // If there is sufficient unread data, read required bytes and update position
  if Count + fPosition <= fSize then
  begin
    {$I-}   // no exceptions on file errors
    BlockRead(fFile, Buffer, Count);
    {$I+}   // exceptions on file errors
    Inc(fPosition, Count);
  end;
end;

end.
