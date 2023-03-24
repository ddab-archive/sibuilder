{ ##                   
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstProcs.pas
  @COMMENTS                 This unit contains common routines used by the
                            SITools install programs. This unit does not use
                            SysUtils or Classes units to avoid overhead code.
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
 * The Original Code is UInstProcs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInstProcs;


interface


function FilePath(FileName: string): string;
  {Strips file path from given file name - used in place of ExtractFilePath
  in SysUtils to save overhead of using that unit}

function EqualStr(Str1, Str2: string): Boolean;
  {Returns true if Str1 is the same as Str2, ignoring case, and false if the
  strings are not the same}

procedure CopyPStr(Dest: PChar; Source: string);
  {Copy source string into array of char given by Dest, which must be long
  enough}


implementation


function FilePath(FileName: string): string;
  {Strips file path from given file name - used in place of ExtractFilePath
  in SysUtils to save overhead of using that unit}
var
  I: Integer;           // scans thru file name
  SlashPos: Integer;    // position of last '\' char in file name
begin
  // Set "no slash char" result as default
  SlashPos := 0;
  // Scan backwards thru filename looking for '\'
  for I := Length(FileName) downto 1 do
    if FileName[I] = '\' then
    begin
      SlashPos := I;
      Break;
    end;
  // Delete anything after last '\' in filename and return as result
  Result := FileName;
  if SlashPos > 0 then
    Delete(Result, SlashPos + 1, Length(Result));
end;

procedure CopyPStr(Dest: PChar; Source: string);
  {Copy source string into array of char given by Dest, which must be long
  enough}
var
  I: Integer; // scans thru string
begin
  for I := 1 to Length(Source) do
    Dest[I-1] := Source[I];
  Dest[Length(Source)] := #0;
end;

function EqualStr(Str1, Str2: string): Boolean;
  {Returns true if Str1 is the same as Str2, ignoring case, and false if the
  strings are not the same}
var
  I: Integer; // scans thru strings
begin
  if Length(Str1) <> Length(Str2) then
    // If lengths of strings aren't same they're not equal
    Result := False
  else
  begin
    // Lengths are same: test if all characters are same, ignoring case
    Result := True;
    for I := 1 to Length(Str1) do
    begin
      if UpCase(Str1[I]) <> UpCase(Str2[I]) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

end.
