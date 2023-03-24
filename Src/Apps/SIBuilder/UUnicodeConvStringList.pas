{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UUnicodeConvStringList.pas
  @COMMENTS                 Unit implements a TStringList sub class that enables
                            text to be read from unicode and ascii text files
                            and written out in either ascii or unicode format.
                            Text is stored internally only in ascii format.
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
 * The Original Code is UUnicodeConvStringList.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UUnicodeConvStringList;


interface


uses
  // Delphi
  Classes;

type

  {
  TUnicodeConvStringList
    TStringList sub class that enables text to be read from unicode and ascii
    text files and written out in either ascii or unicode format. The object
    converts to and from unicode as required when filing, while storing the text
    internally only in ascii format.

    Inheritance: TUnicodeConvStringList -> [TStringList]
  }
  TUnicodeConvStringList = class(TStringList)
  private // properties
    fIsUnicode: Boolean;
  public
    procedure LoadFromStream(Stream: TStream); override;
      {Fills the list with lines of text read from a stream. Detects if stream
      contains Unicode (by checking for $FEFF marker) and converts to ascii
      text if present, otherwise simply reads ascii text from stream. Sets
      IsUnicode property according to whether unicode or ascii text read}
    procedure SaveToStream(Stream: TStream); override;
      {Writes the value of the Text property to a stream object. If IsUnicode
      property is true then text is written in unicode format otherwsie text is
      written as ascii}
    property IsUnicode: Boolean read fIsUnicode write fIsUnicode;
      {Shows if text was read from or is to be written as unicode (when true) or
      ascii (when false). This property is set by LoadFromStream according to
      the type of data on the stream. Setting the property affects the format
      the data written by the SaveToStream method}
  end;


implementation


uses
  // Delphi
  Windows;


{ TUnicodeConvStringList }

procedure TUnicodeConvStringList.LoadFromStream(Stream: TStream);
  {Fills the list with lines of text read from a stream. Detects if stream
  contains Unicode (by checking for $FEFF marker) and converts to ascii text if
  present, otherwise simply reads ascii text from stream. Sets IsUnicode
  property according to whether unicode or ascii text read}
var
  UnicodeMarker: Word;        // marker that identifies stream as unicode
  StartStmPos: LongInt;       // position in stream when method called
  EffectiveStmSize: LongInt;  // size of available stream data
  UBufSize: Integer;          // size of buffer to hold unicode
  UBuf: PWChar;               // buffer to hold unicode
  UStmSize: LongInt;          // size of stream containing unicode chars
begin
  // Record position we're starting from in stream and number of bytes remaining
  StartStmPos := Stream.Position;
  EffectiveStmSize := Stream.Size - StartStmPos;
  // Read a word to check if this is unicode
  Stream.ReadBuffer(UnicodeMarker, SizeOf(UnicodeMarker));
  fIsUnicode := UnicodeMarker = $FEFF;
  if fIsUnicode then
  begin
    // We have a stream in unicode format: read and convert to AnsiString
    // calculate size of buffer required: leaving off size of marker we've read
    // and adding space for one terminating zero wide char character
    UStmSize := EffectiveStmSize - SizeOf(UnicodeMarker);
    UBufSize := UStmSize + SizeOf(WChar);
    // create buffer to hold stream
    GetMem(UBuf, UBufSize);
    try
      FillChar(UBuf^, UBufSize, 0);
      // read unicode from stream and store in string list
      Stream.ReadBuffer(UBuf^, UStmSize);
      SetTextStr(AnsiString(UBuf));
    finally
      FreeMem(UBuf, UBufSize);
    end;
  end
  else
  begin
    // We have an Ansi string stream: read using inherited method
    // rewind to starting position
    Stream.Position := StartStmPos;
    // read using inherited method
    inherited LoadFromStream(Stream);
  end;
end;

procedure TUnicodeConvStringList.SaveToStream(Stream: TStream);
  {Writes the value of the Text property to a stream object. If IsUnicode
  property is true then text is written in unicode format otherwsie text is
  written as ascii}
var
  UnicodeMarker: Word;  // marker that identifies stream as unicode
  WideText: WideString; // wide version of string list text
begin
  // Check if we're saving as unicode
  if fIsUnicode then
  begin
    // Saving in Unicode
    // write marker to indicate unicode
    UnicodeMarker := $FEFF;
    Stream.Write(UnicodeMarker, SizeOf(UnicodeMarker));
    // write string list's text as wide text
    WideText := Self.Text;
    Stream.Write(PWChar(WideText)^, SizeOf(WChar) * Length(WideText));
  end
  else
    // Saving in ASCII: just use inherited method
    inherited SaveToStream(Stream);
end;

end.
