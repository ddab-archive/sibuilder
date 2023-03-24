{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     USIBConverter.pas
  @COMMENTS                 Implements a class that can convert the old style
                            binary project and template files into new style XML
                            project files.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 27/08/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of FileNames.inc include file with
                              UFileNames unit.
                            + Replaced string literal with resource string.
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
 * The Original Code is USIBConverter.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USIBConverter;

interface

uses
  // Delphi
  Registry, SysUtils,
  // Project
  UXMLTag, UBinaryStream;

type

  {
  ESIBConverter:
    Class of exception raised by TSIBConverter.
  }
  ESIBConverter = class(Exception);

  {
  TSIBConverter:
    Class that can convert old style binary project and template files to new
    style XML files.
  }
  TSIBConverter = class(TObject)
  private
    fDataBuffer: Pointer;
      {Pointer to buffer where the data for this item is stored}
    fDataBufferSize: Integer;
      {The current size of fDataBuffer}
    function BinaryAsString: string;
      {Return the binary data stored in the buffer as a string - the string
      comprises the hex representation of each byte, separated by a space}
    function BufferAsString(const ValueType: TRegDataType): string;
      {Returns a string representation of value of any data type. Must not be
      called when data type is unknown}
    function IntAsString: string;
      {Returns the current contents of the buffer as an integer}
    procedure AllocateBuffer(const Size: Integer);
      {Allocates a buffer of given size in which to store registry data}
    procedure ConvertProject(Stream: TBinaryStream; XMLTag: TXMLTag);
      {Reads project from binary stream and creates a heirachy of XML tags that
      describe the project}
    procedure ConvertGroups(Stream: TBinaryStream; ParentTag: TXMLTag);
      {Converts list of groups that begins at current position in binary stream
      and stores a representation of each group as a sub tag of given XML tag}
    procedure ConvertFiles(Stream: TBinaryStream; ParentTag: TXMLTag);
      {Converts list of files that begins at current position in binary stream
      and stores a representation of each file as a sub tag of given XML tag}
    procedure ConvertRegRootKeys(Stream: TBinaryStream; ParentTag: TXMLTag);
      {Converts list of registry root keys that begins at current position in
      binary stream and stores a representation of each key as a sub tag of
      given XML tag}
    procedure ConvertRegKeys(Stream: TBinaryStream; ParentTag: TXMLTag);
      {Converts list of registry sub keys that begins at current position in
      binary stream and stores a representation of each key as a sub tag of
      given XML tag}
    procedure ConvertRegData(Stream: TBinaryStream; ParentTag: TXMLTag);
      {Converts list of registry data items that begins at current position in
      binary stream and stores a representation of each item as a sub tag of
      given XML tag}
  public
    destructor Destroy; override;
      {Class destructor - frees any data buffer}
    procedure Convert(const SIBFile, SIPFile: string);
      {Convert the given old style project or template file SIBFile into a new
      style file named by SIPFile}
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UFileNames, UXMLFile;


resourcestring
  // Error message
  sBadSIBFile = 'File is not a valid SIBuilder file';


{ TSIBConverter }

procedure TSIBConverter.AllocateBuffer(const Size: Integer);
  {Allocates a buffer of given size in which to store registry data}
begin
  // Check if we've already allocated a buffer in fDataBuffer, and free it if so
  if fDataBufferSize > 0 then
    FreeMem(fDataBuffer, fDataBufferSize);
  // Now allocate the buffer of requested size, and store the size
  GetMem(fDataBuffer, Size);
  fDataBufferSize := Size;
end;

function TSIBConverter.BinaryAsString: string;
  {Return the binary data stored in the buffer as a string - the string
  comprises the hex representation of each byte, separated by a space}
var
  StrBuf: PChar;          // Buffer to store #0 terminated output string
  StrBufSize: Integer;    // Size of StrBuf
  PDataBuf: ^Byte;        // Pointer used to walk along data buffer
  PStrBuf: PChar;         // Pointer used to walk along output string buffer
  I : Integer;            // Loops thru databuffer
begin
  // Allocate output string buffer of required size
  StrBufSize := 3 * fDataBufferSize + 1;
  GetMem(StrBuf, StrBufSize);
  try
    // Read bytes from data buffer, writing string represenation to output str
    // make walking pointers point to start of respective buffers
    PDataBuf := fDataBuffer;
    PStrBuf := StrBuf;
    // loop thru each byt in data buffer
    for I := 0 to fDataBufferSize - 1 do
    begin
      // write current byte in data buffer in hex format to output string
      StrPCopy(PStrBuf, IntToHex(PDataBuf^, 2) + ' ');
      // increment the walking pointers to next location in their buffers
      Inc(PStrBuf, 3);
      Inc(PDataBuf);
    end;
    // Terminate output string & copy to a pascal string for function to return
    PStrBuf^ := #0;
    Result := StrPas(StrBuf);
  finally
    // Dispose of output string buffer
    FreeMem(StrBuf, StrBufSize);
  end;
end;

function TSIBConverter.BufferAsString(const ValueType: TRegDataType): string;
  {Returns a string representation of value of any data type. Must not be called
  when data type is unknown}
begin
  Assert(ValueType <> rdUnknown);
  case ValueType of
    rdUnknown: Result := '';
    rdString, rdExpandstring: Result := StrPas(fDataBuffer);
    rdInteger: Result := IntAsString;
    rdBinary: Result := BinaryAsString;
  end;
end;

function TSIBConverter.IntAsString: string;
  {Returns the current contents of the buffer as an integer}
var
  PInteger: ^Integer;   // pointer to integer in data buffer
begin
  // Make pointer to integer address data buffer
  PInteger := fDataBuffer;
  // Return the integer
  Result := IntToStr(PInteger^);
end;

procedure TSIBConverter.Convert(const SIBFile, SIPFile: string);
  {Convert the given old style project or template file SIBFile into a new style
  file named by SIPFile}
var
  BinStream: TBinaryStream;   // the binary stream object we use to read data
  OutStream: TFileStream;     // the output file
  XMLFile: TXMLFile;          // XML file contents object
begin
  // Prepare
  OutStream := nil;
  XMLFile := nil;
  // Create a binary stream object that operates on file stream
  BinStream := TBinaryStream.Create(
    TFileStream.Create(SIBFile, fmOpenRead),             // use this file stream
    True);                            // .. and free it when binary stream freed
  try
    // Create output stream for XML
    OutStream := TFileStream.Create(SIPFile, fmCreate);
    // Create object to store XML info
    XMLFile := TXMLFile.Create(CPrjRootTag);
    // Perform conversion - read binary file into XML file object
    ConvertProject(BinStream, XMLFile.RootTag);
    // Save the XML
    XMLFile.SaveToStream(OutStream);
  finally
    // Tidy up
    XMLFile.Free;
    OutStream.Free;
    BinStream.Free;   // this also frees input file stream
  end;
end;

procedure TSIBConverter.ConvertFiles(Stream: TBinaryStream;
  ParentTag: TXMLTag);
  {Converts list of files that begins at current position in binary stream and
  stores a representation of each file as a sub tag of given XML tag}
var
  Tag: TXMLTag;           // The file tags
  NumFiles: Integer;      // Number of files in group
  I: Integer;             // Loops thru each file in group
begin
  // Get number of files in file
  NumFiles := Stream.ReadInt16;
  // Read each file
  for I := 1 to NumFiles do
  begin
    // Add new <file> tag
    Tag := ParentTag.AddSubTag('file');
    // Store required values and skip those not needed
    Tag.PlainText := Stream.ReadZString;    // FullFileSpec
    Stream.ReadInt32;                       // Skip ContentsPos
    Stream.ReadBoolean;                     // Skip IsCompressed
    Stream.ReadInt32;                       // Skip FileDate
    Stream.ReadInt32;                       // Skip FileAttributes
    Stream.ReadInt32;                       // Skip FileSize
  end;
end;

procedure TSIBConverter.ConvertGroups(Stream: TBinaryStream;
  ParentTag: TXMLTag);
  {Converts list of groups that begins at current position in binary stream and
  stores a representation of each group as a sub tag of given XML tag}
var
  Tag: TXMLTag;           // The group tags
  NumGroups: Integer;     // Number of groups in project
  I: Integer;             // Loops thru each group in project
begin
  // Get number of groups in file
  NumGroups := Stream.ReadInt16;
  // Read each group
  for I := 1 to NumGroups do
  begin
    // Add new <group> tag
    Tag := ParentTag.AddSubTag('group');
    // Store required values and skip those not required
    Tag.Params['name'] := Stream.ReadZString;
    Tag.Params['path'] := Stream.ReadZString;
    Tag.ParamAsInt['filedeletion'] := Stream.ReadInt8;
    Tag.ParamAsInt['dirdeletion'] := Stream.ReadInt8;
    Stream.ReadInt8;                          // Skip Hidden property
    // Process all files in group
    ConvertFiles(Stream, Tag);
  end;
end;

procedure TSIBConverter.ConvertProject(Stream: TBinaryStream; XMLTag: TXMLTag);
  {Reads project from binary stream and creates a heirachy of XML tags that
  describe the project}
begin
  // Make sure file starts with a SI Builder watermark
  if Stream.ReadZString <> cWatermark then
    raise ESIBConverter.Create(sBadSIBFile);
  // Create new XML file with required version number
  XMLTag.ParamAsInt['version'] := CCurrentPrjVersion;
  // Set project XML from binary stream
  XMLTag.Params['id'] := Stream.ReadZString;
  XMLTag.Params['name'] := Stream.ReadZString;
  XMLTag.Params['instpath'] := Stream.ReadZString;
  XMLTag.Params['instprog'] := Stream.ReadZString;
  XMLTag.ParamAsInt['instpathedit'] := Stream.ReadInt8;
  XMLTag.ParamAsInt['uninstall'] := Stream.ReadInt8;
  // Ignore GenerateWinApp value
  Stream.ReadBoolean;
  // Process all groups and registry root keys in project
  ConvertGroups(Stream, XMLTag);
  ConvertRegRootKeys(Stream, XMLTag);
end;

procedure TSIBConverter.ConvertRegData(Stream: TBinaryStream;
  ParentTag: TXMLTag);
  {Converts list of registry data items that begins at current position in
  binary stream and stores a representation of each item as a sub tag of given
  XML tag}
var
  Tag: TXMLTag;             // The group tags
  NumDataItems: Integer;    // Number of subkeys
  I: Integer;               // Loops thru each data item
  ValueType: TRegDataType;  // Type of value
  DataSize: Integer;        // Size of data buffer
begin
  // Read number of data items
  NumDataItems := Stream.ReadInt16;
  // Read in each data item
  for I := 1 to NumDataItems do
  begin
    // Add new <regdata> tag for current data item
    Tag := ParentTag.AddSubTag('regdata');
    // Store name and value type as parameters of tag
    Tag.Params['name'] := Stream.ReadZString;
    ValueType := TRegDataType(Stream.ReadInt8);
    Tag.ParamAsInt['type'] := Ord(ValueType);
    // If data type is not unknown, store a string representation as tag's text
    if ValueType <> rdUnknown then
    begin
      DataSize := Stream.ReadInt16;
      AllocateBuffer(DataSize);
      Stream.ReadBuffer(fDataBuffer^, DataSize);
      Tag.PlainText := BufferAsString(ValueType);
    end;
  end;
end;

procedure TSIBConverter.ConvertRegKeys(Stream: TBinaryStream;
  ParentTag: TXMLTag);
  {Converts list of registry sub keys that begins at current position in binary
  stream and stores a representation of each key as a sub tag of given XML tag}
var
  Tag: TXMLTag;           // The group tags
  NumSubKeys: Integer;    // Number of subkeys
  I: Integer;             // Loops thru each subkey or data item
begin
  // Read number of sub keys in list
  NumSubKeys := Stream.ReadInt16;
  // Read each sub key
  for I := 1 to NumSubKeys do
  begin
    // Add new <regkey> tag for this key
    Tag := ParentTag.AddSubTag('regkey');
    // Store required properties as parameters of tag
    Tag.Params['name'] := Stream.ReadZString;
    Tag.ParamAsInt['deletable'] := Stream.ReadInt8;
    // Add this key's data items and then the sub-keys
    ConvertRegData(Stream, Tag);
    ConvertRegKeys(Stream, Tag);
  end;
end;

procedure TSIBConverter.ConvertRegRootKeys(Stream: TBinaryStream;
  ParentTag: TXMLTag);
  {Converts list of registry root keys that begins at current position in binary
  stream and stores a representation of each key as a sub tag of given XML tag}
var
  Tag: TXMLTag;           // The group tags
  NumRootKeys: Integer;   // Number of subkeys
  I: Integer;             // Loops thru each subkey or data item
begin
  // Read number of root keys in list
  NumRootKeys := Stream.ReadInt16;
  // Read each root key
  for I := 1 to NumRootKeys do
  begin
    // Add a new <regrootkey> tag
    Tag := ParentTag.AddSubTag('regrootkey');
    // Store required parameters
    Tag.Params['name'] := Stream.ReadZString;
    Stream.ReadInt8;            // ignore Deletable
    // Now process data items (should be 0) and sub keys
    ConvertRegData(Stream, Tag);
    ConvertRegKeys(Stream, Tag);
  end;
end;

destructor TSIBConverter.Destroy;
  {Class destructor - frees any data buffer}
begin
  if fDataBufferSize > 0 then
    FreeMem(fDataBuffer, fDataBufferSize);
  inherited Destroy;
end;

end.

