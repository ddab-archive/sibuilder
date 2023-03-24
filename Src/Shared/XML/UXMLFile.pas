{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UXMLFile.pas
  @COMMENTS                 This unit defines a class that encapsulates an XML
                            file - the whole file can be read in and stored in
                            this object.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/08/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Moved unit references from interface to
                            implementation.
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
 * The Original Code is UXMLFile.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UXMLFile;


interface


uses
  // Delphi
  Classes,
  // Project
  UXMLTag;


type

  {
  TXMLFile:
    Encapsulates an XML file - the whole file can be read in and stored in this
    object.

    Inheritance: TXMLFile -> [TObject]
  }
  TXMLFile = class(TObject)
  private // properties
    fRootTag: TXMLTag;
  public
    constructor Create(const RootTagName: string);
      {Class constructor - creates owned root tag object}
    destructor Destroy; override;
      {Class destructor - frees owned object}
    procedure LoadFromStream(const Stream: TStream);
      {Loads an XML file from the given stream}
    procedure SaveToStream(const Stream: TStream);
      {Saves the file contained in this object as XML on the given stream}
    property RootTag: TXMLTag read fRootTag;
      {The root tag of the XML file - all other tags are sub-tags of this one.
      This tag provides access to all the tags in the file}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCharStreamWriter, UTokeniser;


{ TXMLFile }

constructor TXMLFile.Create(const RootTagName: string);
  {Class constructor - creates owned root tag object}
begin
  inherited Create;
  fRootTag := TXMLTag.Create(RootTagName);
end;

destructor TXMLFile.Destroy;
  {Class destructor - frees owned object}
begin
  fRootTag.Free;
  inherited Destroy;
end;

procedure TXMLFile.LoadFromStream(const Stream: TStream);
  {Loads an XML file from the given stream}
var
  Tokeniser: TTokeniser;    // object that splits XML file into tokens
begin
  // Create object to toeknise file
  Tokeniser := TTokeniser.Create(Stream);
  try
    // Read in the root tag (and all sub-tags) using tokeniser
    fRootTag.Read(Tokeniser);
  finally
    // Free the tokeniser object
    Tokeniser.Free;
  end;
end;

procedure TXMLFile.SaveToStream(const Stream: TStream);
  {Saves the file contained in this object as XML on the given stream}
var
  Writer: TCharStreamWriter;    // object that writes characters to a stream
begin
  // Create an object to write to the stream
  Writer := TCharStreamWriter.Create(Stream);
  try
    // Get root tag to write itself (and all sub tags) as XML using the writer
    fRootTag.Write(Writer);
  finally
    // Free the writer object
    Writer.Free;
  end;
end;

end.
