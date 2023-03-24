{ ##                   
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UPayload.pas
  @COMMENTS                 Provides functions that can be used to access
                            payload data appended to executable files along with
                            the associated payload data footer record.
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
 * The Original Code is UPayload.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UPayload;


interface


type

  {
  TPayloadFooter:
    Record placed at end of an executable file that contains a payload.
  }
  TPayloadFooter = packed record
    WaterMark: LongWord;  // watermark identifies as payload footer
    ExeSize: LongInt;     // size of original exe file without payload
    DataSize: LongInt;    // size of payload data
  end;

procedure InitFooter(out Footer: TPayloadFooter);
  {Initialises a footer record and returns it in Footer parameter.
  An initialised footer contains a valid watermark and other fields set to zero}

function InitPayload(var F: File): Integer;
  {Initialises payload in given file: it reads the payload footer record and
  seeks to start of payload, returning size of payload. If payload footer can't
  be read or is not valid then 0 is returned}

function ReadPayloadFooter(var F: File;
  out Footer: TPayloadFooter): Boolean;
  {Reads payload footer record from given file into Footer. Returns true if
  payload can be read and is valid, false otherwise}


implementation


const
  // Watermark used to stamp SITools payload code is ASCII for "SIB3"
  cWaterMark = $53494233;
 
procedure InitFooter(out Footer: TPayloadFooter);
  {Initialises a footer record and returns it in Footer parameter.
  An initialised footer contains a valid watermark and other fields set to zero}
begin
  FillChar(Footer, SizeOf(Footer), 0);
  Footer.WaterMark := cWaterMark;
end;

function ReadPayloadFooter(var F: File; out Footer: TPayloadFooter): Boolean;
  {Reads payload footer record from given file into Footer. Returns true if
  payload can be read and is valid, false otherwise}
var
  FileLen: Integer; // length of file including payload
begin
  // Check that file is large enough for a footer!
  FileLen := FileSize(F);
  if FileLen > SizeOf(Footer) then
  begin
    // Big enough: move to start of footer and read it
    Seek(F, FileLen - SizeOf(Footer));
    BlockRead(F, Footer, SizeOf(Footer));
  end
  else
    // File not large enough for footer: zero it
    // .. this ensures watermark is invalid
    FillChar(Footer, SizeOf(Footer), 0);
  // Return if watermark is valid
  Result := Footer.WaterMark = cWaterMark;
end;

function InitPayload(var F: File): Integer;
  {Initialises payload in given file: it reads the payload footer record and
  seeks to start of payload, returning size of payload. If payload footer can't
  be read or is not valid then 0 is returned}
var
  Footer: TPayloadFooter; // payload footer record
begin
  // Try to read payload footer
  if ReadPayloadFooter(F, Footer) then
  begin
    // Footer read OK: seek to start of payload data and return its size
    Seek(F, Footer.ExeSize);
    Result := Footer.DataSize;
  end
  else
    // Failed to read footer: return 0
    Result := 0;
end;

end.
