{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UDataStream.pas
  @COMMENTS                 This unit defines the abstract TDataStream class.
                            This abstract descendant of TStreamWrapper exposes
                            methods to read and write various types of formatted
                            data to a wrapped stream. Descendant classes
                            implement the methods for various data formats. This
                            detaches classes using these classes from the
                            physical data format used.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
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
 * The Original Code is UDataStream.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UDataStream;

interface

uses
  // Project
  UStreamWrapper;

type
  {Abstract descendant of TStreamWrapper that exposes methods to read and write
  various types of formatted data to a wrapped stream. This is provided as a
  common framework for descendant classes that actually maintain and interpret
  the data in the wrapped stream}
  TDataStream = class(TStreamWrapper)
  public
    function ReadBoolean: Boolean; virtual; abstract;
      {Reads a Boolean value from underlying stream and returns}
    function ReadInt8: ShortInt; virtual; abstract;
      {Reads 8 bit value from underlying stream and returns it as a signed
      value}
    function ReadInt16: SmallInt; virtual; abstract;
      {Reads 16 bit value from underlying stream and returns it as a signed
      value}
    function ReadInt32: Integer; virtual; abstract;
      {Reads 32 bit value from underlying stream and returns it as a signed
      value}
    function ReadZString: AnsiString; virtual; abstract;
      {Reads a zero terminated string from underlying stream. Returns the
      string}
    procedure WriteBoolean(Value: Boolean); virtual; abstract;
      {Writes the given Boolean value to the underlying stream}
    procedure WriteInt8(Value: ShortInt); virtual; abstract;
      {Writes the given 8 bit value to the underlying stream}
    procedure WriteInt16(Value: SmallInt); virtual; abstract;
      {Writes the given 16 bit value to the underlying stream}
    procedure WriteInt32(Value: Integer); virtual; abstract;
      {Writes the given 32 bit value to the underlying stream}
    procedure WriteZString(Value: AnsiString); virtual; abstract;
      {Writes the given string to the underlying stream. The string is
      terminated in the stream by a #0 character}
  end;

implementation

end.
