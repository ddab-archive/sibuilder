{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegEditFile.pas
  @COMMENTS                 Unit implements a class that encapsulates a registry
                            editor export file that can be updated by user and
                            can read and write registry editor files in various
                            formats.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             + Fixed bug in registry import that was truncating
                              registry keys containing ']' characters.
                            + Replaced string literals with resource strings.
                            + Replaced literal registry file header strings with
                              constants.  
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
 * The Original Code is URegEditFile.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegEditFile;


interface


uses
  // Delphi
  SysUtils, Classes, Registry;


type

  {
  TRegEditFileType:
    The various kinds of regedit file types that we support, along with an
    unknown type.
  }
  TRegEditFileType = (
    rfSimple,   // the early simple type of regedit file
    rf4,        // regedit 4 files
    rf5,        // regedit 5 files
    rfUnknown   // unknown file type
  );

  {
  TRegEditFile:
    Object that can read and write a regedit file and provides read/write access
    to its contents, enabling user to read a file and create a new one to be
    written out.
  }
  TRegEditFile = class(TObject)
  private // properties
    fFileType: TRegEditFileType;
    function GetKey(Idx: Integer): string;
    function GetValue(const Key: string; Idx: Integer): string;
  private
    fKeys: TStringList;
      {String list that contains all keys in file along with a data record that
      gives the key's attributes (including value list)}
    procedure DeleteValueByIndex(ValueItems: TStrings; Idx: Integer);
      {Deletes value at given index in given value list and frees all its
      associated data}
    procedure DeleteKeyByIndex(Idx: Integer);
      {Deletes key at given index and free all its associated data}
    procedure FreeValueRec(P: Pointer);
      {Frees given value record}
    procedure FreeKeyRec(P: Pointer);
      {Free key record pointed to by P and also deletes all associated data}
    function GetKeyRec(const Key: string; Force: Boolean): Pointer;
      {Returns reference to key record associated with key. If key doesn't
      exist then action depends on Force parameter: when Force is true key is
      created while of Force is false, nil is returned}
    function GetValueRec(const Key, Name: string; Force: Boolean): Pointer;
      {Returns reference to value record associated with value of given name
      under given key. If no such value or key exists then result depends on
      Force parameter. If Force is true the key and value are created if
      required and if Force is false nil is returned}
    function GetValueList(const Key: string): TStringList;
      {Returns reference to value list associated with given key. If key doesn't
      exist then we return nil}
  public
    constructor Create;
      {Class constructor: creates owned string list and sets default property
      values}
    destructor Destroy; override;
      {Class destructor: clears all and deallocates all data structures and
      frees the owned stringlist}
    procedure LoadFromFile(const FileName: string);
      {Loads reg edit file from given file. Sets FileType property according to
      version of regedit used to write file}
    procedure LoadFromStream(const Stream: TStream);
      {Loads reg edit file from given stream. Sets FileType property according
      to version of regedit used to write file}
    procedure SaveToFile(const FileName: string);
      {Saves reg edit file to given file. Creates file with format according to
      reg edit version per the FileType property}
    procedure SaveToStream(const Stream: TStream);
      {Saves reg edit file to given stream. Creates file with format according
      to reg edit version per the FileType property}
    procedure Clear;
      {Clears all keys and entries from the object - i.e. creates an empty
      "file"}
    function KeyExists(const Key: string): Boolean;
      {Returns true if given key exists, false otherwise}
    function KeyDeleteFlag(const Key: string): Boolean;
      {Returns true if the key is flagged to be deleted (by regedit), false if
      not or if key doesn't exist}
    function ValueExists(const Key, Name: string): Boolean;
      {Returns true if the value with the given name under the given key exists
      and false if not}
    function ValueDeleteFlag(const Key, Name: string): Boolean;
      {Returns true if the value is flagged to be deleted (by regedit), false if
      not or if value doesn't exist}
    procedure ReadKeys(const Keys: TStrings);
      {Stores names of all keys in given string list: the list is cleared before
      the key names are stored}
    procedure ReadValueNames(const Key: string; const Names: TStrings);
      {Stores names of all values under given key in given string list: the list
      is cleared before value names are stored}
    function ValueType(const Key, Name: string): TRegDataType;
      {Returns the data type of the given value under the given key: rdUnknown
      is returned if value doesn't exist}
    function Value(const Key, Name: string): string;
      {Returns the string representation of the given value under the given key:
      the format of the value depends on the data type as follows:
        rdString: the string
        rdExpandString: the string
        rdInteger: string representation of integer value
        rdBinary: string of space separated hex bytes
        rdUnknown (or value doesn't exist): empty string}
    function ValueInfo(const Key, Name: string; out Value: string;
      out DataType: TRegDataType; out DeleteFlag: Boolean): Boolean;
      {Returns all info about the value with the given name under the given key
      as follows:
        Value contains string representation of the value (see Value method)
        DataType contains the data type of the value: this is rdUnknown if the
          value doesn't exist or is marked for deletion
        DeleteFlag is true if value is marked for deletion, false if not.
      The method returns true if the value exists and false if not}
    procedure WriteKey(const Key: string; DeleteFlag: Boolean = False);
      {Writes a new key with given name, flagged for deletion if DeleteFlag is
      true. If key exists its DeleteFlag property is updated to given value}
    procedure WriteValue(const Key, Name, Value: string;
      DataType: TRegDataType = rdString; DeleteFlag: Boolean = False);
      {Write a new value with given name under given key. The data item has the
      provided value and datatype and its deletion flag is set to the given
      value. If DeleteFlag is true the DataType is always set to rdUnknown. If a
      value with given name already exists it's value, type and delete flag are
      updated. If the data value is not valid for the given value type, or type
      is unknown then an exception is raised}
    procedure WriteIntegerValue(const Key, Name: string; Value: Integer);
      {Write a new integer value with given name under given key. The deletion
      flag is set false. Raises exception if Value is not a valid integer}
    procedure WriteStringValue(const Key, Name, Value: string);
      {Write a new string value with given name under given key. The deletion
      flag is set false}
    procedure WriteExpandStringValue(const Key, Name, Value: string);
      {Write a new expand string value with given name under given key. The
      deletion flag is set false}
    procedure WriteBinaryStrValue(const Key, Name, Value: string);
      {Write a new binary value with given name under given key using the the
      given string representation of the binary value. The string is a list of
      comma separated 1 or 2 digit hex strings, with each hex string
      representing one byte. The deletion flag is set false. An exception is
      raised if binary string is not in correct format}
    procedure WriteBinaryValue(const Key, Name: string; Buffer: Pointer;
      Size: Integer);
      {Write a new binary value with the given name under the given key using
      the binary data pointer to by Buffer of length Size. The deletion flag
      is set false}
    function DeleteKey(const Key: string): Boolean;
      {Deletes the given key and all its value. Returns true if key existed and
      was deleted}
    function DeleteValue(const Key, Name: string): Boolean;
      {Deletes the given value under the given key. Returns true if key existed
      and was deleted}
    function KeyCount: Integer;
      {Returns the number of keys in the file}
    function ValueCount(const Key: string): Integer;
      {Returns the number of values for the given key. Returns 0 if the key
      doesn't exist}
    property FileType: TRegEditFileType read fFileType write fFileType;
      {Type of regedit file that was read or to be written. LoadFromFile and
      LoadFromStream set this property according to format of file being read.
      SaveToFile and SaveToStream use the current value of this property when
      writing a file. If any key has other than default values then FileType
      cannot be rfSimple}
    property Keys[Idx: Integer]: string read GetKey;
      {Indexes into keys in file and returns name of key at given index. Keys
      are indexed 0..KeyCount-1. It is an error if index out of range}
    property ValueNames[const Key: string; Idx: Integer]: string read GetValue;
      {Indexes into values for given key and returns name of value at given
      index. Values are indexed 0..ValueCount-1. It is an error if the key
      doesn't exist or if index out of range}
  end;

  {
  ERegEditFile:
    Class of exception raised by reg edit file class.
  }
  ERegEditFile = class(Exception);


implementation


uses
  // Delphi
  Windows,
  // Project
  UStringProcs, UUnicodeConvStringList, UBinStrings, URegUtils;


type

  {
  TKeyRec:
    Record associated with each key in reg edit file: contains delete flag and
    list of values under key.
  }
  TKeyRec = record
    Delete: Boolean;        // deletion flag
    ValueList: TStringList; // list stores values associated with key
  end;

  {
  PKeyRec:
    Pointer to TKeyRec record.
  }
  PKeyRec = ^TKeyRec;

  {
  TValueRec:
    Record associated with each value: contains value's type, deletion flag and
    the actual value (as a string).
  }
  TValueRec = record
    ValueType: TRegDataType;
    Delete: Boolean;
    Value: string;
  end;

  {
  PValueRec:
    Points to TValueRec.
  }
  PValueRec = ^TValueRec;

  {
  TRegEditReader:
    Abstract base class for classes that read reg edit files (contained in
    string list objects) and update TRegEditFile objects based on file contents.
    Different classes are derived from this class which can read the various
    different reg edit file formats.

    Inheritance: TRegEditReader -> [TObject]
  }
  TRegEditReader = class(TObject)
  protected // data
    fLines: TStrings;
      {String list object containing contents of rich edit file}
    fREFile: TRegEditFile;
      {Reg edit file object that is updated to reflect content of file}
  protected
    procedure ParseLine(const Line: string); virtual; abstract;
      {Method called for each line in file: descendants override this method to
      parse the line and update reg edit file object}
    procedure PreProcess; virtual;
      {Pre-processes all file before analysis is performed: this method does
      nothing but may be overridden by classes that require pre-processing}
  public
    constructor Create(const REFile: TRegEditFile; const Lines: TStrings);
      {Class constructor: records reference to given rich edit file object that
      is to be updated according to file contents and records reference to
      string list that stores contents of file to be processed}
    procedure Execute; virtual;
      {Parses the reg edit file stored in string list passed to constructor and
      record its contents in rich edit file object that was also passed to
      constructor}
  end;

  {
  TEarlyRegEditReader:
    Class that parses contents of early, simple, reg edit file format (files
    introduced by REGEDIT line). Content of file is stored in a string list and
    Execute method updates rich edit file object based on contents.

    Inheritance: TEarlyRegEditReader -> TRegEditReader -> [TObject]
  }
  TEarlyRegEditReader = class(TRegEditReader)
  protected
    procedure ParseLine(const Line: string); override;
      {Method called for each line in file: parses the line and updates reg edit
      file object}
  end;

  {
  TValueRegEditReader:
    Base class for reading reg edit files which have both keys and named values
    defined within them. Classes for various file formats that declare keys and
    data values are derived from this class.

    Inheritance: TValueRegEditReader -> TRegEditReader -> [TObject]
  }
  TValueRegEditReader = class(TRegEditReader)
  protected
    fCurrentKey: string;
      {The current key being processed: used to ensure data items are associated
      with correct key}
    function ExpStrBinToStr(Buffer: PByte; BufSize: Integer): string; virtual;
      abstract;
      {Converts the given binary value (that defines a string) to an expand
      string. Descendant classes perform conversion according to mapping between
      binary and string used in the class's file format}
    function BinStringToExpStr(const BinStr: string): string;
      {Converts a string that defines binary data (comprises hex words separated
      by commas) into an expand string. (Expand strings are defined as sequences
      of bytes in hex in reg edit files)}
    function DataTypeFromData(const Data: string): TRegDataType;
      {Returns the type of data defined by the given data string. The string is
      the data value associated with a named data item. If data type can't be
      determined return rdUnknown}
    function CalcDataInfo(const Data: string; out DataType: TRegDataType;
      out Delete: Boolean; out Value: string): Boolean;
      {Given a string containing the data value component of a value definition
      line from a reg edit file, returns the type and value of line, and whether
      line marks data item for deletion via out parameters. Function returns
      true if string contains valid data and false if not}
    function SplitDataLine(const Line: string; out Name, Value: string): Boolean;
      {A data line is in form "Value"=data. This method splits out the Name and
      Value components of line and returns thru out parameters. The name
      parameter is unquoted and unescaped. If the line has valid "Value"=data
      format true is returned, else false is returned if line is not valid}
    function ProcessKeyLine(const Line: string): string;
      {Processes a line that defines a registry key: returns name of key and
      updates reg edit file object with details of new key. If line doesn't
      contain valid key then '' is returned}
    procedure ProcessDataLine(const Line: string);
      {Processes given data line by recording value name and value contained in
      line if line is valid}
    procedure PreProcess; override;
      {Pre-processes file contained in string list object before it is parsed.
      The method concatenates any lines that end with \ character with following
      line(s)}
    procedure ParseLine(const Line: string); override;
      {Method called for each line in file: parses the line and updates reg edit
      file object}
  end;

  {
  TRegEdit4Reader:
    Class that can read and parse reg edit v4 file stored in a string list and
    update reg edit file object based on file contents.

    Inheritance: TRegEdit4Reader -> TValueRegEditReader -> TRegEditReader
      -> [TObject]
  }
  TRegEdit4Reader = class(TValueRegEditReader)
  protected
    function ExpStrBinToStr(Buffer: PByte; BufSize: Integer): string; override;
      {Converts the given binary value (that defines a string) to an expand
      string. The buffer holds the expand string as a zero terminated string of
      ansi characters}
  end;

  {
  TRegEdit5Reader:
    Class that can read and parse reg edit v5 file stored in a string list and
    update reg edit file object based on file contents.

    Inheritance: TRegEdit5Reader -> TValueRegEditReader -> TRegEditReader
      -> [TObject]
  }
  TRegEdit5Reader = class(TValueRegEditReader)
  protected
    function ExpStrBinToStr(Buffer: PByte; BufSize: Integer): string; override;
      {Converts the given binary value (that defines a string) to an expand
      string. The buffer holds the expand string as a zero terminated string of
      wide (unicode) characters}
  end;

  {
  TRegEditWriter:
    Abstract base class for classes that write reg edit files to string list
    objects from TRegEditFile objects. Different classes are derived from this
    class which can write the various different reg edit file formats.

    Inheritance: TRegEditWriter -> [TObject]
  }
  TRegEditWriter = class(TObject)
  protected
    fLines: TStrings;
      {String list object to receive contents of output file}
    fREFile: TRegEditFile;
      {Reg edit file object whose content is written out}
    procedure WriteHeader; virtual; abstract;
      {Writes file header to output lines object: to be implemented by
      descendant classes}
    procedure WriteKey(const Key: string; DeleteFlag: Boolean);
      virtual; abstract;
      {Writes a key with given attribute out to output lines object: to be
      implemented by descendant classes}
    procedure WriteValue(const Key, Name, Value: string; DataType: TRegDataType;
      DeleteFlag: Boolean); virtual; abstract;
      {Writes a value with given attributes out to output lines object: to be
      implemented by descendant classes}
    procedure Finalize; virtual; abstract;
      {Finalises output to lines object: to be implemented by descendant
      classes}
    procedure WriteLine(const Line: string);
      {Writes out given line to lines object by appending line to list}
  public
    constructor Create(const REFile: TRegEditFile; const Lines: TStrings);
      {Class constructor: records reference to given rich edit file object that
      provides content and reference to string list that content is written to}
    procedure Execute; virtual;
      {Writes the contents of the reg edit file object passed to constructor to
      the string list that was also passed to constructor}
  end;

  {
  TEarlyRegEditWriter:
    Class that writes early, simple, reg edit file format (files introduced by
    REGEDIT line). Content of a rich edit file object is stored in a string
    list.

    Inheritance: TEarlyRegEditWriter -> TRegEditWriter -> [TObject]
  }
  TEarlyRegEditWriter = class(TRegEditWriter)
  private
    fCurrentLine: string;
      {Records any pending line that has not yet been written}
  protected
    procedure WriteHeader; override;
      {Writes file header to output lines object: this identifies type of file
      this is}
    procedure WriteKey(const Key: string; DeleteFlag: Boolean); override;
      {Writes a key with given attribute out to output lines object: we flush
      any pending line and record key as pending since we may need to append a
      default value to line}
    procedure WriteValue(const Key, Name, Value: string; DataType: TRegDataType;
      DeleteFlag: Boolean); override;
      {Writes a value with given attributes out to output lines object: ingores
      all but default values - file format only supports default string values}
    procedure Finalize; override;
      {Finalises output to lines object: if we have a pending line to be written
      out we write it}
  end;

  {
  TValueRegEditWriter:
    Base class for writinbg reg edit files which have both keys and named values
    stored in them. Classes that write various file formats that declare keys
    and data values are derived from this class.

    Inheritance: TValueRegEditWriter -> TRegEditWriter -> [TObject]
  }
  TValueRegEditWriter = class(TRegEditWriter)
  protected
    function ExpandStringAsBinaryString(ExpStr: string): string;
      virtual; abstract;
      {Converts the given expand string into a string comprising a comma
      separated list of hex bytes that represent the string. The exact format
      depends on the file version and therefore this method is implemented in
      sub classes}
    procedure HexWrite(const Prefix, Hex: string);
      {Writes given hex bytes with given prefix, wrapping across several lines
      if necessary}
    procedure WriteHeader; override; abstract;
      {Writes file header to output lines object: to be implemented by
      descendant classes}
    procedure WriteKey(const Key: string; DeleteFlag: Boolean); override;
      {Writes a key with given attribute out to output lines object}
    procedure WriteValue(const Key, Name, Value: string; DataType: TRegDataType;
      DeleteFlag: Boolean); override;
      {Writes a value with given attributes out to output lines object}
    procedure Finalize; override;
      {Finalises output to lines object: there is nothing to do here}
  end;

  {
  TRegEdit4Writer:
    Class that can write reg edit v4 file to a string list and using keys and
    values stored in a reg edit file object.

    Inheritance: TRegEdit4Writer -> TValueRegEditWriter -> TRegEditWriter
      -> [TObject]
  }
  TRegEdit4Writer = class(TValueRegEditWriter)
  protected
    procedure WriteHeader; override;
      {Writes file header to output lines object: this identifies type of file
      this is}
    function ExpandStringAsBinaryString(ExpStr: string): string; override;
      {Converts the given expand string into a string comprising a comma
      separated list of hex bytes that represent the string. The binary data
      represents a zero terminated ansi string}
  end;

  {
  TRegEdit5Writer:
    Class that can write reg edit v5 file to a string list and using keys and
    values stored in a reg edit file object.

    Inheritance: TRegEdit5Writer -> TValueRegEditWriter -> TRegEditWriter
      -> [TObject]
  }
  TRegEdit5Writer = class(TValueRegEditWriter)
  protected
    procedure WriteHeader; override;
      {Writes file header to output lines object: this identifies type of file
      this is}
    function ExpandStringAsBinaryString(ExpStr: string): string; override;
      {Converts the given expand string into a string comprising a comma
      separated list of hex bytes that represent the string. The binary data
      represents a zero terminated wide char (unicode) string}
  end;


const
  // Escape characters that are used in key and value names
  cFileEscChars = '\"';
  cSimpleRegEditHeader = 'REGEDIT';
  cRegEdit4Header = 'REGEDIT4';
  cRegEdit5Header = 'Windows Registry Editor Version 5.00';

resourcestring
  // Error messages
  sKeyDoesNotExist = 'Key does not exist';
  sInvalidRegFileType = 'Invalid or unsupported registry file type';
  sCantWriteRegFileType = 'Can''t write registry file of unknown type';
  sCantSetUnknownDataType = 'Can''t set a data type of unknown';


function RegFileStrToStr(var S: string): Boolean;
  {Unescapes given string (from reg edit file) and replaces it with converted
  version. Returns true if string is valid and false if not}
begin
  Result := IsValidCEscapedStr(S, cFileEscChars);
  S := CUnEscapeStr(S, cFileEscChars, cFileEscChars);
end;

procedure StrToRegFileStr(var S: string);
  {Escapes given string and replaces it with converted version ready for writing
  to regedit file}
begin
  S := CEscapeStr(S, cFileEscChars, cFileEscChars);
end;


{ TRegEditFile }

procedure TRegEditFile.Clear;
  {Clears all keys and entries from the object - i.e. creates an empty file"}
var
  Idx: Integer; // loops through all keys
begin
  // Delete each key and its contained data structures
  for Idx := Pred(fKeys.Count) downto 0 do
    DeleteKeyByIndex(Idx);
end;

constructor TRegEditFile.Create;
  {Class constructor: creates owned string list and sets default property
  values}
begin
  inherited;
  fKeys := TStringList.Create;
  fFileType := rf4;
end;

function TRegEditFile.DeleteKey(const Key: string): Boolean;
  {Deletes the given key and all its value. Returns true if key existed and was
  deleted}
var
  KeyIdx: Integer;  // index of key in keys list
begin
  // Get index of key in list
  KeyIdx := fKeys.IndexOf(NormaliseKeyName(Key));
  Result := KeyIdx > -1;
  if Result then
    // Key exists: delete it by index
    DeleteKeyByIndex(KeyIdx);
end;

procedure TRegEditFile.DeleteKeyByIndex(Idx: Integer);
  {Deletes key at given index and free all its associated data}
var
  KeyRec: PKeyRec;  // pointer to record associated with key at given index
begin
  // Get pointer to key's data record
  KeyRec := Pointer(fKeys.Objects[Idx]);
  // Free key record and associated data
  FreeKeyRec(KeyRec);
  // Delete the key
  fKeys.Delete(Idx);
end;

function TRegEditFile.DeleteValue(const Key, Name: string): Boolean;
  {Deletes the given value under the given key. Returns true if key existed and
  was deleted}
var
  ValueIdx: Integer;  // index of value in key's value list
  KeyRec: PKeyRec;    // pointer to record associated with this key
begin
  // Get record associated with key
  KeyRec := GetKeyRec(Key, False);
  Result := False;
  if Assigned(KeyRec) then
  begin
    // Get index of value in key's value list
    ValueIdx := KeyRec^.ValueList.IndexOf(Name);
    Result := ValueIdx > -1;
    if Result then
      // We have value: delete it from value list by index
      DeleteValueByIndex(KeyRec^.ValueList, ValueIdx);
  end;
end;

procedure TRegEditFile.DeleteValueByIndex(ValueItems: TStrings;
  Idx: Integer);
  {Deletes value at given index in given value list and frees all its associated
  data}
var
  ValueRec: PValueRec;  // pointer to record associated with value @ given index
begin
  // Get reference to record associated with value at given index
  ValueRec := Pointer(ValueItems.Objects[Idx]);
  // Free value record and its data
  FreeValueRec(ValueRec);
  // Delete value item from list
  ValueItems.Delete(Idx);
end;

destructor TRegEditFile.Destroy;
  {Class destructor: clears all and deallocates all data structures and frees
  the owned stringlist}
begin
  Clear;
  FreeAndNil(fKeys);
  inherited;
end;

procedure TRegEditFile.FreeKeyRec(P: Pointer);
  {Free key record pointed to by P and also deletes all associated data}
var
  KeyRec: PKeyRec;  // pointer to key record we're deleting
  SL: TStringList;  // reference to key record's value list
  Idx: Integer;     // loops thru all items in value list
begin
  // Store pointer to record we're deleting
  KeyRec := P;
  // Delete all values associated with key record by looping thru value list
  SL := KeyRec^.ValueList;
  for Idx := Pred(SL.Count) downto 0 do
    DeleteValueByIndex(SL, Idx);
  // Free key record's memory
  Dispose(KeyRec);
end;

procedure TRegEditFile.FreeValueRec(P: Pointer);
  {Frees given value record}
var
  ValueRec: PValueRec;  // pointer to value record we're deleting
begin
  ValueRec := P;
  Dispose(ValueRec);
end;

function TRegEditFile.GetKey(Idx: Integer): string;
  {Read access method for Keys property. Raises exception if key index is out
  of bounds}
begin
  Result := fKeys[Idx];
end;

function TRegEditFile.GetKeyRec(const Key: string;
  Force: Boolean): Pointer;
  {Returns reference to key record associated with key. If key doesn't exist
  then action depends on Force parameter: when Force is true key is created
  while of Force is false, nil is returned}
var
  KeyIdx: Integer;  // index of key in list
  KeyRec: PKeyRec;  // pointer to record associated with key
  KeyName: string;  // name of key converted to normal format
begin
  // Convert key name to normal form and validate it
  if UStringProcs.CountDelims(Key, '\') > 0 then
    // key contains a path delimiter, so validate whole path
    URegUtils.RegUtilCheck(URegUtils.ValidateRegKeyPath(Key))
  else
    // key has no delimiter, so just check it's a root key
    URegUtils.RegUtilCheck(URegUtils.ValidateRegRootKeyStr(Key));
  KeyName := URegUtils.NormaliseKeyName(Key);
  // Check if key exists
  KeyIdx := fKeys.IndexOf(KeyName);
  if KeyIdx > -1 then
    // Key exists: return it's associated record
    Result := PKeyRec(fKeys.Objects[KeyIdx])
  else if Force then
  begin
    // Key doesn't exist: force is true so create new key record
    New(KeyRec);
    KeyRec^.Delete := False;
    KeyRec^.ValueList := TStringList.Create;
    Result := KeyRec;
    // Add new key to list
    fKeys.AddObject(KeyName, Result);
  end
  else
    // Key doesn't exists: Force is false so return nil
    Result := nil;
end;

function TRegEditFile.GetValue(const Key: string; Idx: Integer): string;
  {Read access method for Values property. Raises exception if key doesn't exist
  or if value index is out of bounds}
var
  KeyRec: PKeyRec;  // pointer to record associated with given key
begin
  // Get pointer to key record
  KeyRec := GetKeyRec(Key, False);
  // Raise exception if key doesn't exist
  if not Assigned(KeyRec) then
    raise ERegEditFile.Create(sKeyDoesNotExist);
  // Return name of value from list
  Result := KeyRec^.ValueList[Idx];
end;

function TRegEditFile.GetValueList(const Key: string): TStringList;
  {Returns reference to value list associated with given key. If key doesn't
  exist then we return nil}
var
  KeyRec: PKeyRec;  // pointer to key record associated with key
begin
  KeyRec := GetKeyRec(Key, False);
  if Assigned(KeyRec) then
    Result := KeyRec^.ValueList
  else
    Result := nil;
end;

function TRegEditFile.GetValueRec(const Key, Name: string;
  Force: Boolean): Pointer;
  {Returns reference to value record associated with value of given name under
  given key. If no such value or key exists then result depends on Force
  parameter. If Force is true the key and value are created if required and if
  Force is false nil is returned}
var
  KeyRec: PKeyRec;      // pointer to record associated with key
  ValueIdx: Integer;    // index of value in key's value list
  ValueRec: PValueRec;  // pointer to value record we return
begin
  // Validate value name
  URegUtils.RegUtilCheck(URegUtils.ValidateRegValueName(Name));
  // Assume we can't find value
  Result := nil;
  // Get key record: it will be created if it doesn't exist if force is true
  KeyRec := GetKeyRec(Key, Force);
  if Assigned(KeyRec) then
  begin
    // Key exists: check to see if value exists
    ValueIdx := KeyRec^.ValueList.IndexOf(Name);
    if ValueIdx > -1 then
      // Value exists: return reference to it
      Result := PValueRec(KeyRec^.ValueList.Objects[ValueIdx])
    else if Force then
    begin
      // Value doesn't exist: Force is true so create new value
      New(ValueRec);
      ValueRec^.ValueType := rdUnknown;
      ValueRec^.Delete := False;
      ValueRec^.Value := '';
      Result := ValueRec;
      // Add new value to key's value list
      KeyRec^.ValueList.AddObject(Name, Result);
    end;
  end;
end;

function TRegEditFile.KeyCount: Integer;
  {Returns the number of keys in the file}
begin
  Result := fKeys.Count;
end;

function TRegEditFile.KeyDeleteFlag(const Key: string): Boolean;
  {Returns true if the key is flagged to be deleted (by regedit), false if not
  or if key doesn't exist}
var
  KeyRec: PKeyRec;  // pointer to key's data record: contains delete flag
begin
  KeyRec := GetKeyRec(Key, False);
  if Assigned(KeyRec) then
    Result := KeyRec^.Delete
  else
    Result := False;
end;

function TRegEditFile.KeyExists(const Key: string): Boolean;
  {Returns true if given key exists, false otherwise}
begin
  Result := Assigned(GetKeyRec(Key, False));
end;

procedure TRegEditFile.LoadFromFile(const FileName: string);
  {Loads reg edit file from given file. Sets FileType property according to
  version of regedit used to write file}
var
  FS: TStream;  // stream onto file to be read
begin
  // We open a stream onto file and use LoadFromStream method
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(FS);
  finally
    FreeAndNil(FS);
  end;
end;

procedure TRegEditFile.LoadFromStream(const Stream: TStream);
  {Loads reg edit file from given stream. Sets FileType property according to
  version of regedit used to write file}
var
  USL: TUnicodeConvStringList;  // string list used to read ansi or unicode text
  Line: string;                 // the current line from file being processed
  Reader: TRegEditReader;       // reader object used to interpret file
begin
  Reader := nil;

  // Clear the current entries
  Clear;

  // Load stream into string list: unicode streams are converted to ascii
  USL := TUnicodeConvStringList.Create;
  try
    USL.LoadFromStream(Stream);
    // Test kind of registry editor file: must be in first line
    fFileType := rfUnknown;
    if USL.Count > 0 then
    begin
      Line := USL[0];
      if Line = cSimpleRegEditHeader then
        fFileType := rfSimple   // early form of regedit file
      else if Line = cRegEdit4Header then
        fFileType := rf4        // version 4 of regedit file
      else if Line = cRegEdit5Header then
        fFileType := rf5;       // version 5 of regedit file
    end;
    if fFileType <> rfUnknown then
      // delete first line: not longer required
      USL.Delete(0);

    // Create reader object of required type
    case fFileType of
      rfSimple: Reader := TEarlyRegEditReader.Create(Self, USL);
      rf4:      Reader := TRegEdit4Reader.Create(Self, USL);
      rf5:      Reader := TRegEdit5Reader.Create(Self, USL);
      else      raise ERegEditFile.Create(sInvalidRegFileType);
    end;

    // Read data from file
    Reader.Execute;
  finally
    FreeAndNil(Reader);
    FreeAndNil(USL);
  end;
end;

procedure TRegEditFile.ReadKeys(const Keys: TStrings);
  {Stores names of all keys in given string list: the list is cleared before the
  key names are stored}
var
  KeyIdx: Integer;  // loops thru all keys
begin
  Keys.BeginUpdate;
  try
    // Clear the given stringlist
    Keys.Clear;
    // Add name of each key to stringlist
    for KeyIdx := 0 to Pred(fKeys.Count) do
      Keys.Add(fKeys[KeyIdx]);
  finally
    Keys.EndUpdate;
  end;
end;

procedure TRegEditFile.ReadValueNames(const Key: string; const Names: TStrings);
  {Stores names of all values under given key in given string list: the list is
  cleared before value names are stored}
var
  ValueList: TStringList; // reference to value list associated with key
  ValueIdx: Integer;      // loops thru all values
begin
  Names.BeginUpdate;
  try
    // Clear the given list
    Names.Clear;
    // Get reference to value list associated with key
    ValueList := Self.GetValueList(Key);
    if Assigned(ValueList) then
      // Store name of each value in given list
      for ValueIdx := 0 to Pred(ValueList.Count) do
        Names.Add(ValueList[ValueIdx]);
  finally
    Names.EndUpdate;
  end;
end;

procedure TRegEditFile.SaveToFile(const FileName: string);
  {Saves reg edit file to given file. Creates file with format according to reg
  edit version per the FileType property}
var
  FS: TStream;  // stream onto file to be read
begin
  // We open a stream onto new file and use SaveToStream method
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FreeAndNil(FS);
  end;
end;

procedure TRegEditFile.SaveToStream(const Stream: TStream);
  {Saves reg edit file to given stream. Creates file with format according to
  reg edit version per the FileType property}
var
  USL: TUnicodeConvStringList;  // stringlist used to write ansi or unicode text
  Writer: TRegEditWriter;       // writer object used to write required format
begin
  Writer := nil;
  // Create string list which holds contents to be written to file
  USL := TUnicodeConvStringList.Create;
  try
    // Create writer object of type to write required format file to stringlist
    case fFileType of
      rfSimple:
      begin
        // simple file: this is ansii
        USL.IsUnicode := False;
        Writer := TEarlyRegEditWriter.Create(Self, USL);
      end;
      rf4:
      begin
        // regedit v4 file: this is ansii
        USL.IsUnicode := False;
        Writer := TRegEdit4Writer.Create(Self, USL);
      end;
      rf5:
      begin
        // regedit v5 file: this is unicode
        USL.IsUnicode := True;
        Writer := TRegEdit5Writer.Create(Self, USL);
      end;
      rfUnknown:
        // unknown file type: raise exception - can't write
        raise ERegEditFile.Create(sCantWriteRegFileType);
    end;
    // Use writer to create file contents in stringlist
    Writer.Execute;
    // Save file contents in either ansii or unicode text format
    USL.SaveToStream(Stream);
  finally
    FreeAndNil(Writer);
    FreeAndNil(USL);
  end;
end;

function TRegEditFile.Value(const Key, Name: string): string;
  {Returns the string representation of the given value under the given key: the
  format of the value depends on the data type as follows:
    rdString: the string
    rdExpandString: the string
    rdInteger: string representation of integer value
    rdBinary: string of space separated hex bytes
    rdUnknown (or value doesn't exist): empty string}
var
  ValueRec: PValueRec;  // pointer to record associated with value
begin
  ValueRec := GetValueRec(Key, Name, False);
  if Assigned(ValueRec) then
    Result := ValueRec^.Value
  else
    Result := '';
end;

function TRegEditFile.ValueCount(const Key: string): Integer;
  {Returns the number of values for the given key. Returns 0 if the key doesn't
  exist}
var
  KeyRec: PKeyRec;  // pointer to record associated with key
begin
  KeyRec := GetKeyRec(Key, False);
  if Assigned(KeyRec) then
    Result := KeyRec^.ValueList.Count
  else
    Result := 0;
end;

function TRegEditFile.ValueDeleteFlag(const Key, Name: string): Boolean;
  {Returns true if the value is flagged to be deleted (by regedit), false if not
  or if value doesn't exist}
var
  ValueRec: PValueRec;  // pointer to data item's record: contains delete flag
begin
  ValueRec := GetValueRec(Key, Name, False);
  if Assigned(ValueRec) then
    Result := ValueRec^.Delete
  else
    Result := False;
end;

function TRegEditFile.ValueExists(const Key, Name: string): Boolean;
  {Returns true if the value with the given name under the given key exists and
  false if not}
begin
  Result := Assigned(GetValueRec(Key, Name, False));
end;

function TRegEditFile.ValueInfo(const Key, Name: string; out Value: string;
  out DataType: TRegDataType; out DeleteFlag: Boolean): Boolean;
  {Returns all info about the value with the given name under the given key
  as follows:
    Value contains string representation of the value (see Value method)
    DataType contains the data type of the value: this is rdUnknown if the
      value doesn't exist or is marked for deletion
    DeleteFlag is true if value is marked for deletion, false if not.
  The method returns true if the value exists and false if not}
var
  ValueRec: PValueRec;  // pointer to record associated with value
begin
  // Get pointer to value record
  ValueRec := GetValueRec(Key, Name, False);
  // Check if there is a value record
  Result := Assigned(ValueRec);
  if Result then
  begin
    // Value exists: get required data from value record
    Value := ValueRec^.Value;
    DeleteFlag := ValueRec^.Delete;
    if not DeleteFlag then
      DataType := ValueRec^.ValueType
    else
      DataType := rdUnknown;
  end
  else
  begin
    // Value doesn't exist: return unknown type and values
    Value := '';
    DeleteFlag := False;
    DataType := rdUnknown;
  end;
end;

function TRegEditFile.ValueType(const Key, Name: string): TRegDataType;
  {Returns the data type of the given value under the given key: rdUnknown is
  returned if value doesn't exist}
var
  ValueRec: PValueRec;  // pointer to record associated with value
begin
  // Get reference to value required
  ValueRec := GetValueRec(Key, Name, False);
  if Assigned(ValueRec) and (not ValueRec^.Delete) then
    // We have value record and we're not a deletion value: get type from record
    Result := ValueRec^.ValueType
  else
    // Either no value with given name or we're a deletion value
    Result := rdUnknown;
end;

procedure TRegEditFile.WriteBinaryStrValue(const Key, Name, Value: string);
  {Write a new binary value with given name under given key using the the given
  string representation of the binary value. The string is a list of comma
  separated 1 or 2 digit hex strings, with each hex string representing one
  byte. The deletion flag is set false. An exception is raised if binary string
  is not in correct format}
begin
  WriteValue(Key, Name, Value, rdBinary, False);
end;

procedure TRegEditFile.WriteBinaryValue(const Key, Name: string;
  Buffer: Pointer; Size: Integer);
  {Write a new binary value with the given name under the given key using the
  binary data pointer to by Buffer of length Size. The deletion flag is set
  false}
begin
  WriteBinaryStrValue(Key, Name, UBinStrings.BinaryToCommaBinStr(Buffer, Size));
end;

procedure TRegEditFile.WriteExpandStringValue(const Key, Name,
  Value: string);
  {Write a new expand string value with given name under given key. The deletion
  flag is set false}
begin
  WriteValue(Key, Name, Value, rdExpandString, False);
end;

procedure TRegEditFile.WriteIntegerValue(const Key, Name: string;
  Value: Integer);
  {Write a new integer value with given name under given key. The deletion flag
  is set false. Raises exception if Value is not a valid integer}
begin
  WriteValue(Key, Name, IntToStr(Value), rdInteger, False);
end;

procedure TRegEditFile.WriteKey(const Key: string; DeleteFlag: Boolean);
  {Writes a new key with given name, flagged for deletion if DeleteFlag is true.
  If key exists its DeleteFlag property is updated to given value}
var
  KeyRec: PKeyRec;  // pointer to record associated with key
begin
  // Get hold of key record, created if doesn't already exist
  KeyRec := GetKeyRec(Key, True);
  Assert(Assigned(KeyRec));
  // Set delete flag
  KeyRec^.Delete := DeleteFlag;
end;

procedure TRegEditFile.WriteStringValue(const Key, Name, Value: string);
  {Write a new string value with given name under given key. The deletion flag
  is set false}
begin
  WriteValue(Key, Name, Value, rdString, False);
end;

procedure TRegEditFile.WriteValue(const Key, Name, Value: string;
  DataType: TRegDataType; DeleteFlag: Boolean);
  {Write a new value with given name under given key. The data item has the
  provided value and datatype and its deletion flag is set to the given value.
  If DeleteFlag is true the DataType is always set to rdUnknown. If a value with
  given name already exists it's value, type and delete flag are updated. If the
  data value is not valid for the given value type, or type is unknown then an
  exception is raised}
var
  ValueRec: PValueRec;  // pointer to record associated with value
  TheValue: string;     // processed value: stores value actually stored
begin
  // Validate the value
  // assume OK
  TheValue := Value;
  if not DeleteFlag then
  begin
    case DataType of
      rdUnknown:
        // raise exception: can't store values of unknown type
        raise ERegEditFile.Create(sCantSetUnknownDataType);
      rdInteger:
      begin
        // convert value into standard integer format
        TheValue := IntToStr(StrToInt(Value));  // raises exception if not valid
      end;
      rdBinary:
        // convert to normalised form: this raises exceptions if invalid
        TheValue := UBinStrings.NormaliseCommaBinStr(Value);
      rdString, rdExpandString:
        // Don't process string and expand string: write unchanged
        ;
    end;
  end
  else
    // all values are ignored if delete is set
    TheValue := '';
  // Write the value
  // get pointer to value record, creating if doesn't exist
  ValueRec := GetValueRec(Key, Name, True);
  Assert(Assigned(ValueRec));
  // record value string, delete flag and delete flag
  ValueRec^.Value := TheValue;
  ValueRec^.Delete := DeleteFlag;
  if not DeleteFlag then
    ValueRec^.ValueType := DataType
  else
    ValueRec^.ValueType := rdUnknown;
end;


{ TRegEditReader }

constructor TRegEditReader.Create(const REFile: TRegEditFile;
  const Lines: TStrings);
  {Class constructor: records reference to given rich edit file object that is
  to be updated according to file contents and records reference to string list
  that stores contents of file to be processed}
begin
  inherited Create;
  Assert(Assigned(REFile));
  Assert(Assigned(Lines));
  fREFile := REFile;
  fLines := Lines;
end;

procedure TRegEditReader.Execute;
  {Parses the reg edit file stored in string list passed to constructor and
  record its contents in rich edit file object that was also passed to
  constructor}
var
  Idx: Integer; // loops thru all lines in string list containing file
begin
  // Pre-process the file
  PreProcess;
  // Read and process (trimmed) each line in file
  for Idx := 0 to Pred(fLines.Count) do
    ParseLine(Trim(fLines[Idx]));
end;

procedure TRegEditReader.PreProcess;
  {Pre-processes all file before analysis is performed: this method does nothing
  but may be overridden by classes that require pre-processing}
begin
  {Do nothing};
end;


{ TEarlyRegEditReader }

procedure TEarlyRegEditReader.ParseLine(const Line: string);
  {Method called for each line in file: parses the line and updates reg edit
  file object}
var
  Key: string;      // key defined by line
  DefValue: string; // default string value associated with key
begin
  // In early regedit files, all valid lines declare a key under
  // HKEY_CLASSES_ROOT, followed by optional default string value, so we only
  // process lines beginning with HKEY_CLASSES_ROOT
  if AnsiPos('HKEY_CLASSES_ROOT', Line) = 1 then
  begin
    // Line is in form Key = DefValue: so split line at any = sign
    UStringProcs.SplitStr(Line, '=', Key, DefValue);
    // Trim spaces from key and default value
    Key := Trim(Key);
    DefValue := Trim(DefValue);
    // Write key
    fREFile.WriteKey(Key);
    if DefValue <> '' then
      // We have a value, so write default string value under key
      fREFile.WriteValue(Key, '', DefValue);
  end;
end;


{ TValueRegEditReader }

function TValueRegEditReader.BinStringToExpStr(const BinStr: string): string;
  {Converts a string that defines binary data (comprises hex words separated by
  commas) into an expand string. (Expand strings are defined as sequences of
  bytes in hex in reg edit files)}
var
  BufSize: Integer; // size of binary data defined by binary string
  Buffer: PByte;    // buffer used to store binary data defined by binary string
begin
  // Initialise (empty) buffer
  BufSize := 0;
  Buffer := nil;
  try
    // Convert binary string to binary data
    // NOTE: this function allocates buffer (raises exception on invalid string)
    CommaBinStrToBinary(BinStr, Buffer, BufSize);
    if Assigned(Buffer) then
      // There is some data: convert binary data to expand string
      Result := ExpStrBinToStr(Buffer, BufSize)
    else
      // There is no data in binary string: return ''
      Result := '';
  finally
    // If a buffer was allocated (by CommaBinStrToBinary), free it
    if Assigned(Buffer) then
      FreeMem(Buffer, BufSize);
  end;
end;

function TValueRegEditReader.CalcDataInfo(const Data: string;
  out DataType: TRegDataType; out Delete: Boolean;
  out Value: string): Boolean;
  {Given a string containing the data value component of a value definition line
  from a reg edit file, returns the type and value of line, and whether line
  marks data item for deletion via out parameters. Function returns true if
  string contains valid data and false if not}
begin
  // Assume data string is not valid and we're not flagged for deletion
  Result := False;
  Delete := False;
  // Get data type
  DataType := DataTypeFromData(Data);
  // Parse data value according to type
  case DataType of
    rdString:
    begin
      // Get string value by stripping quotes and unescape it
      Value := Copy(Data, 2, Length(Data) - 2);
      if RegFileStrToStr(Value) then
        Result := True; // valid value if string can be unescaped
    end;
    rdInteger:
    begin
      // Integer value is in hex form following 'dword:' marker
      Value := Trim(Copy(Data, Length('dword:') + 1, MaxInt));
      if (Length(Value) in [1..8]) and (IsHexStr(Value)) then
      begin
        try
          Value := IntToStr(StrToInt('$' + Value));
          Result := True; // valid value only if valid hex string is found
        except
          // swallow exceptions: leaves Result=False
        end;
      end;
    end;
    rdBinary:
    begin
      try
        // Binary value is a comma delimited string of hex bytes
        // store a "normalised" version if binary string
        // this normalisation will raise exceptions if value is invalid
        Value := NormaliseCommaBinStr((Copy(Data, Length('hex:') + 1, MaxInt)));
        Result := True;
      except
        // swallow exceptions: leaves Result=False
      end;
    end;
    rdExpandString:
    begin
      // Expand string is defined by comma delimited string of hex bytes
      DataType := rdExpandString;
      try
        // we convert binary value to string (method depends on file format)
        // this conversion may raise exceptions in value is invalid
        Value := BinStringToExpStr(Copy(Data, Length('hex(2):') + 1, MaxInt));
        Result := True;
      except
        // swallow exceptions: leaves Result=False
      end;
    end;
    rdUnknown:
    begin
      // When data type is unknown then we either have an invalid data item or
      // we have a value marked for deletion if data string = '-'
      if Trim(Data) = '-' then
      begin
        // this is a deletion marker: flag is such
        DataType := rdUnknown;
        Delete := True;
        Value := '';
        Result := True;
      end;
    end;
  end;
  if not Result then
  begin
    // We have false result: ensure we have unknown value and empty value
    DataType := rdUnknown;
    Value := '';
  end;
end;

function TValueRegEditReader.DataTypeFromData(
  const Data: string): TRegDataType;
  {Returns the type of data defined by the given data string. The string is the
  data value associated with a named data item. If data type can't be determined
  return rdUnknown}
var
  DataStub: string; // 1st 12 chars of data (sufficient to determine data type)
begin
  // Get first part of data line in lower case (sufficient to determine type)
  DataStub := LowerCase(Copy(Data, 1, 12));
  // Assume type is unknown
  Result := rdUnknown;
  if DataStub <> '' then
  begin
    if DataStub[1] = '"' then
      // string values are enclosed in double quotes
      Result := rdString
    else if AnsiPos('dword:', DataStub) = 1 then
      // integer values begin with dword:
      Result := rdInteger
    else if AnsiPos('hex:', DataStub) = 1 then
      // binary values begin with 'hex:'
      Result := rdBinary
    else if AnsiPos('hex(2):', DataStub) = 1 then
      // expand strings begin with 'hex(2):'
      Result := rdExpandString;
  end;
end;

procedure TValueRegEditReader.ParseLine(const Line: string);
  {Method called for each line in file: parses the line and updates reg edit
  file object}
var
  NewKey: string; // the value of the key represented by line
begin
  // We only process lines with contents: line passed to method has been trimmed
  if Line <> '' then
  begin
    case Line[1] of
      '[':
      begin
        // Line contains a key: record it if valid
        NewKey := ProcessKeyLine(Line);
        if NewKey <> '' then
          fCurrentKey := NewKey;
      end;
      '"', '@':
      begin
        // Line contains data item: Process it only if we have a current key
        if fCurrentKey <> '' then
        begin
          // if line begins with @ (default) treat as if it begins with ""
          if Line[1] = '@' then
            ProcessDataLine('""' + Copy(Line, 2, MaxInt))
          else
            ProcessDataLine(Line);
        end;
      end;
    end;
  end;
end;

procedure TValueRegEditReader.PreProcess;
  {Pre-processes file contained in string list object before it is parsed. The
  method concatenates any lines that end with \ character with following
  line(s)}
var
  Idx: Integer; // loops thru all lines in string list
  Line: string; // current line in string list
begin
  // Loop thru all lines in string list
  Idx := 0;
  while Idx < Pred(fLines.Count) do
  begin
    // Check if current line ends with '\': concatenate if so
    Line := fLines[Idx];
    if (Line <> '') and (Line[Length(Line)] = '\') then
    begin
      // Line needs concatenating with following one
      // join to following line and store new line in string list
      Line := Copy(Line, 1, Length(Line) - 1) + Trim(fLines[Idx + 1]);
      fLines[Idx] := Line;
      // delete the following line (has now been concatenated with current)
      fLines.Delete(Idx + 1);
      // don't move to next line: we re-process current line in case further
      // concatenation is required (i.e. if new line ends with '\')
    end
    else
      // Line doesn't need concatenating: move to next line
      Inc(Idx);
  end;
end;

procedure TValueRegEditReader.ProcessDataLine(const Line: string);
  {Processes given data line by recording value name and value contained in line
  if line is valid}
var
  Name, Value: string;      // name and value of data item
  DataType: TRegDataType;   // type of data defined in line
  Data: string;             // the data part of data item (holds type and value)
  DeleteFlag: Boolean;      // deletion flag for data in line
begin
  Assert((Line <> '') and (Line[1] = '"'));
  // Split out name from value data
  if SplitDataLine(Line, Name, Data) then
  begin
    // Validate value
    if CalcDataInfo(Data, DataType, DeleteFlag, Value) then
      // Update reg edit object with value
      fREFile.WriteValue(fCurrentKey, Name, Value, DataType, DeleteFlag);
  end;
end;

function TValueRegEditReader.ProcessKeyLine(const Line: string): string;
  {Processes a line that defines a registry key: returns name of key and updates
  reg edit file object with details of new key. If line doesn't contain valid
  key then '' is returned}
var
  StartKey: Integer;    // index of start of key name in line
  DeleteFlag: Boolean;  // whether this is deletable key or not
begin
  Assert((Line <> '') and (Line[1] = '['));
  // Key name is in form [Key_name] (or [-Key_name] for keys to be deleted)
  // Find start of key name
  if (Length(Line) > 1) and (Line[2] = '-') then
  begin
    // key flagged for deletion: key starts immediately after '-'
    StartKey := 3;
    DeleteFlag := True;
  end
  else
  begin
    // key not flagged for deletion: key starts immediately after '['
    StartKey := 2;
    DeleteFlag := False;
  end;
  // Check for valid key terminator ']' character
  if Line[Length(Line)] = ']' then
  begin
    // We have key in required format: copy it out & update reg edit file object
    Result := Copy(Line, StartKey, Length(Line) - StartKey);
    fREFile.WriteKey(Result, DeleteFlag);
  end
  else
    // We have invalid key: return '' to indicate this
    Result := '';
end;

function TValueRegEditReader.SplitDataLine(const Line: string; out Name,
  Value: string): Boolean;
  {A data line is in form "Value"=data. This method splits out the Name and
  Value components of line and returns thru out parameters. The name parameter
  is unquoted and unescaped. If the line has valid "Value"=data format true is
  returned, else false is returned if line is not valid}
var
  I: Integer; // scans thru characters in line
begin
  Assert((Line <> '') and (Line[1] = '"'));
  // Get name
  // name starts after opening quote and ends just before closing quote: name
  // can contain embedded = sign, so we can't use simple line split
  I := 2;
  while (I <= Length(Line)) and (Line[I] <> '"') do
  begin
    if Line[I] = '\' then
      // jump over escaped character
      Inc(I);
    Inc(I);
  end;
  // we un-escape the \ escape characters in name
  Name := CUnEscapeStr(Copy(Line, 2, I - 2), cFileEscChars, cFileEscChars);
  // Get value: this follows the equal sign that follows end of name
  while (I <= Length(Line)) and (Line[I] <> '=') do
    Inc(I);
  Value := Trim(Copy(Line, I + 1, MaxInt));
  // We have a data value only if we have an equal sign
  Result := (I <= Length(Line));
end;


{ TRegEdit4Reader }

function TRegEdit4Reader.ExpStrBinToStr(Buffer: PByte;
  BufSize: Integer): string;
  {Converts the given binary value (that defines a string) to an expand string.
  The buffer holds the expand string as a zero terminated string of ansi
  characters}
var
  PEnd: PByte;          // points to end of data buffer
  PBuf: PChar;          // points to data buffer
  StrBufSize: Integer;  // buffer to hold string inc ending #0 character
begin
  // Set pointer to last byte in buffer
  PEnd := Buffer;
  Inc(PEnd, BufSize - 1);
  // Check if byte at end of buffer is zero
  if PEnd^ <> 0 then
  begin
    // Byte at end of buffer is not zero byte
    // reserve space for string including ending zero byte
    StrBufSize := BufSize + SizeOf(AnsiChar);
    GetMem(PBuf, StrBufSize);
    try
      // ensure that this buffer will end in zero
      FillChar(PBuf^, 0, StrBufSize);
      // move buffer to this new string buffer and return the resulting string
      Move(Buffer^, PBuf^, BufSize);
      Result := PBuf;
    finally
      // free the new buffer
      FreeMem(PBuf, StrBufSize);
    end;
  end
  else
    // Buffer ends in zero: so cast to string and return it
    Result := PChar(Buffer);
end;


{ TRegEdit5Reader }

function TRegEdit5Reader.ExpStrBinToStr(Buffer: PByte;
  BufSize: Integer): string;
  {Converts the given binary value (that defines a string) to an expand string.
  The buffer holds the expand string as a zero terminated string of wide
  (unicode) characters}
var
  PEnd: PByte;          // points to end of data buffer
  PBuf: PByte;          // points to data buffer
  StrBufSize: Integer;  // buffer to hold string inc ending #0 character
begin
  // Set pointer to last byte in buffer
  PEnd := Buffer;
  Inc(PEnd, BufSize - 1);
  // Check if byte at end of buffer is zero
  if PEnd^ <> 0 then
  begin
    // Byte at end of buffer is not zero byte
    // reserve space for wide string including ending zero byte
    StrBufSize := BufSize + SizeOf(WChar);
    GetMem(PBuf, StrBufSize);
    try
      // ensure that this buffer will end in zero
      FillChar(PBuf^, 0, StrBufSize);
      // move buffer to this new buffer and return the resulting wide string
      Move(Buffer^, PBuf^, BufSize);
      Result := WideString(PWChar(PBuf));
    finally
      // free the new buffer
      FreeMem(PBuf, StrBufSize);
    end;
  end
  else
    // Buffer ends in zero: so cast to wide string and return it
    Result := WideString(PWChar(Buffer));
end;


{ TRegEditWriter }

constructor TRegEditWriter.Create(const REFile: TRegEditFile;
  const Lines: TStrings);
  {Class constructor: records reference to given rich edit file object that
  provides content and reference to string list that content is written to}
begin
  inherited Create;
  Assert(Assigned(REFile));
  Assert(Assigned(Lines));
  fREFile := REFile;
  fLines := Lines;
end;

procedure TRegEditWriter.Execute;
  {Writes the contents of the reg edit file object passed to constructor to the
  string list that was also passed to constructor}
var
  KeyIdx: Integer;          // loops thru all keys in reg edit file
  Key: string;              // name of current key being processed
  ValueIdx: Integer;        // loops thru all values within a key
  Name: string;             // name of current value being processed
  Value: string;            // value associated with current value name
  DataType: TRegDataType;   // data type of current value
  DeleteFlag: Boolean;      // whether current value or key is deletable
begin
  // Freeze update on lines object
  fLines.BeginUpdate;
  try
    // Clear lines object
    fLines.Clear;
    // Write file header
    WriteHeader;
    // Loop thru all keys in reg edit file object, writing each one
    for KeyIdx := 0 to Pred(fREFile.KeyCount) do
    begin
      // Record attributes of key and write out
      Key := fREFile.Keys[KeyIdx];
      DeleteFlag := fREFile.KeyDeleteFlag(Key);
      WriteKey(Key, DeleteFlag);
      // Loop thru all value within current key
      for ValueIdx := 0 to Pred(fREFile.ValueCount(Key)) do
      begin
        // Record attributes of value and write out
        Name := fREFile.ValueNames[Key, ValueIdx];
        fREFile.ValueInfo(Key, Name, Value, DataType, DeleteFlag);
        WriteValue(Key, Name, Value, DataType, DeleteFlag);
      end;
    end;
    // Finalize the output
    Finalize;
  finally
    // Unfreeze lines object now updating completed
    fLines.EndUpdate;
  end;
end;

procedure TRegEditWriter.WriteLine(const Line: string);
  {Writes out given line to lines object by appending line to list}
begin
  fLines.Add(Line);
end;


{ TEarlyRegEditWriter }

procedure TEarlyRegEditWriter.Finalize;
  {Finalises output to lines object: if we have a pending line to be written out
  we write it}
begin
  if fCurrentLine <> '' then
    WriteLine(fCurrentLine);
end;

procedure TEarlyRegEditWriter.WriteHeader;
  {Writes file header to output lines object: this identifies type of file this
  is}
begin
  WriteLine(cSimpleRegEditHeader);
  WriteLine('');
end;

procedure TEarlyRegEditWriter.WriteKey(const Key: string; DeleteFlag: Boolean);
  {Writes a key with given attribute out to output lines object: we flush any
  pending line and record key as pending since we may need to append a default
  value to line}
begin
  if fCurrentLine <> '' then
    // There was a pending line which we write
    WriteLine(fCurrentLine);
  if not DeleteFlag then
    // Key not marked for deleting so we need to write: record as pending line
    fCurrentLine := Key
  else
    // Key marked for deleting:
    // we ignore it since file format doesn't support deletion of keys
    fCurrentLine := '';
end;

procedure TEarlyRegEditWriter.WriteValue(const Key, Name, Value: string;
  DataType: TRegDataType; DeleteFlag: Boolean);
  {Writes a value with given attributes out to output lines object: ingores all
  but default values - file format only supports default string values}
begin
  if (Name = '') and (DataType = rdString) and (DeleteFlag = False)
    and (fCurrentLine <> '') then
  begin
    // We have default string non-delting value
    // we append to pending line and write line out
    fCurrentLine := fCurrentLine + ' = ' + Value;
    WriteLine(fCurrentLine);
    fCurrentLine := '';
  end;
end;


{ TValueRegEditWriter }

procedure TValueRegEditWriter.Finalize;
  {Finalises output to lines object: there is nothing to do here}
begin
  {Do nothing};
end;

procedure TValueRegEditWriter.HexWrite(const Prefix, Hex: string);
  {Writes given hex bytes with given prefix, wrapping across several lines if
  necessary}
var
  StrLen: Integer;      // length of string to be written
  RemainLen: Integer;   // number of chars in text left to write
  NextPos: Integer;     // position of next character in string to be written
  EndPos: Integer;      // position of last character in string to be written
  WriteStrLen: Integer; // length of string to be written
  S: string;            // total string to be written out
  MaxLineLen: Integer;  // max length of text on line (allows for margins)
begin
  // Determine length of text that can be written to line, and write first line
  if Length(Prefix) > 76 then
  begin
    // length of prefix is long enough to sit on its own: write it
    WriteLine(Prefix + '\');
    // record just hex string as item left to write
    S := Hex;
    // note max line length reduced to allow for two space leading margin
    MaxLineLen := 78;
  end
  else
  begin
    // prefix and hex can fit on line: record it
    S := Prefix + Hex;
    // note max line length of 80 since we're still to write first line
    MaxLineLen := 80;
  end;
  // Write remainder of text
  // store length of text and simply write out if all fits on one line
  StrLen := Length(S);
  if StrLen <= MaxLineLen then
  begin
    // all text fits on line: write out (with 2 space margin if necessary)
    if MaxLineLen = 78 then
      WriteLine('  ' + S)
    else
      WriteLine(S)
  end
  else
  begin
    // text doesn't fit on one line: we need to wrap it
    // note we're starting line
    NextPos := 1;
    RemainLen := StrLen;
    // loop while we've more characters to write
    while RemainLen > MaxLineLen do
    begin
      // find location of last comma in hex before end of line
      EndPos := NextPos + MaxLineLen - 2;
      while (EndPos > 0) and (S[EndPos] <> ',') do
        Dec(EndPos);
      // determine part of string to write and write it, with margin if needed
      WriteStrLen := EndPos - NextPos + 1;
      if MaxLineLen = 78 then
        WriteLine('  ' + Copy(S, NextPos, WriteStrLen) + '\')
      else
        WriteLine(Copy(S, NextPos, WriteStrLen) + '\');
      // calculate amount of string left to write
      Dec(RemainLen, WriteStrLen);
      NextPos := EndPos + 1;
      // note next line is continuation line and needs leading margin
      MaxLineLen := 78;
    end;
    // check if we've characters left to write and write out if so
    if RemainLen > 0 then
    begin
      if MaxLineLen = 78 then
        WriteLine('  ' + Copy(S, NextPos, MaxInt))
      else
        WriteLine(Copy(S, NextPos, MaxInt));
    end;
  end;
end;

procedure TValueRegEditWriter.WriteKey(const Key: string; DeleteFlag: Boolean);
  {Writes a key with given attribute out to output lines object}
var
  KeyStr: string; // used to build up key to be written
begin
  // Build key string: mark for deletion if necessary
  if DeleteFlag then
    KeyStr := '[-' + Key + ']'  // mark for deletion
  else
    KeyStr := '[' + Key + ']';  // mark for creation of key
  // Write preceeding blank line then key
  WriteLine('');
  WriteLine(KeyStr);
end;

procedure TValueRegEditWriter.WriteValue(const Key, Name, Value: string;
  DataType: TRegDataType; DeleteFlag: Boolean);
  {Writes a value with given attributes out to output lines object}
var
  ValStr: string; // string containing value to be written
begin
  // Store value name followed by = sign
  if Name = '' then
    // default value, begin with @
    ValStr := '@='
  else
    // named value: quote and escape name
    ValStr := '"' + CEscapeStr(Name, cFileEscChars, cFileEscChars) + '"=';
  // Store value
  if DeleteFlag then
    // this is value marked for deletion: value is '-'
    WriteLine(ValStr + '-')
  else
  begin
    // this is a true value: store it
    case DataType of
      rdString:
        // string: quote and escape value
        WriteLine(
          ValStr + '"' + CEscapeStr(Value, cFileEscChars, cFileEscChars) + '"'
        );
      rdExpandString:
        // expand string: write comma delimited hex (split over several lines if
        // required) - binary format depends on file version
        HexWrite(ValStr + 'hex(2):', ExpandStringAsBinaryString(Value));
      rdInteger:
        // integer: write value in hex format
        WriteLine(ValStr + 'dword:' + IntToHex(StrToInt(Value), 8));
      rdBinary:
        // binary: write comma delimited hex (split over several lines if
        // required)
        HexWrite(ValStr + 'hex:', Value);
    end;
  end;
end;


{ TRegEdit4Writer }

function TRegEdit4Writer.ExpandStringAsBinaryString(
  ExpStr: string): string;
  {Converts the given expand string into a string comprising a comma separated
  list of hex bytes that represent the string. The binary data represents a zero
  terminated ansi string}
var
  Buffer: PByte;      // buffer that stores expand string as ansi chars
  BufferLen: Integer; // length of buffer containing ansi string
begin
  // Set pointer to buffer occupied by ExpStr
  Buffer := PByte(PChar(ExpStr));
  // Set length of buffer to include trailing #0
  BufferLen := Length(ExpStr) + SizeOf(AnsiChar);
  // Convert buffer to comma separated hex string
  Result := UBinStrings.BinaryToCommaBinStr(Buffer, BufferLen);
end;

procedure TRegEdit4Writer.WriteHeader;
  {Writes file header to output lines object: this identifies type of file this
  is}
begin
  WriteLine(cRegEdit4Header);
end;


{ TRegEdit5Writer }

function TRegEdit5Writer.ExpandStringAsBinaryString(
  ExpStr: string): string;
  {Converts the given expand string into a string comprising a comma separated
  list of hex bytes that represent the string. The binary data represents a zero
  terminated wide char (unicode) string}
var
  WExpStr: WideString;  // wide string copy of expand string
  Buffer: PByte;        // buffer that stores expand string as wide chars
  BufferLen: Integer;   // length of buffer containing wide string
begin
  // Convert expand string to wide string
  WExpStr := ExpStr;
  // Set pointer to buffer occupied by wide string
  Buffer := PByte(PWChar(WExpStr));
  // Set length of buffer to include trailing unicode #0#0
  BufferLen := SizeOf(WChar) * (Length(WExpStr) + 1);
  // Convert buffer to comma separated hex string
  Result := UBinStrings.BinaryToCommaBinStr(Buffer, BufferLen);
end;

procedure TRegEdit5Writer.WriteHeader;
  {Writes file header to output lines object: this identifies type of file this
  is}
begin
  WriteLine(cRegEdit5Header);
end;

end.

