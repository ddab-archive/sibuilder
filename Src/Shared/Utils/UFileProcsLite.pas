{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UFileProcsLite.pas
  @COMMENTS                 Contains static class that provides some file
                            utility functions. Used instead of UFileProcs to
                            avoid the overhead of using the SysUtils or
                            Classes units.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 16/01/2006
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
 * The Original Code is UFileProcsLite.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2006 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UFileProcsLite;


interface


type

  // TODO: Make equivalent routines in UFileProcs call these

  {
  TFileProcsLite:
    Static class that provides some file utility functions. Used instead of
    UFileProcs to avoid the overhead of using the SysUtils or Classes units.
  }
  TFileProcsLite = class(TObject)
  private
    class function CreateDir(const Dir: string): Boolean;
      {Creates given directory and returns true on success}
  public
    class function DirToPath(const Dir: string): string;
      {Turns given directory into path by appending trailing backslash if
      needed}
    class function DirExists(const Dir: string): Boolean;
      {Checks if given directory exists}
    class procedure EnsureFolders(Path: string);
      {Ensures that all folders in given path exist}
    class function RemoveDir(Dir: string): Boolean;
      {Removes an empty directory and returns true on on success}
    class function TempFolder: string;
      {Returns temporary folder}
  end;


implementation


uses
  // Delphi
  Windows;


{ TFileProcsLite }

class function TFileProcsLite.CreateDir(const Dir: string): Boolean;
  {Creates given directory and returns true on success}
begin
  Result := Windows.CreateDirectory(PChar(Dir), nil);
end;

class function TFileProcsLite.DirExists(const Dir: string): Boolean;
  {Checks if given directory exists}
var
  Code: DWORD;  // Result returned from API when checking file attributes
begin
  Code := Windows.GetFileAttributes(PChar(Dir));
  Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

class function TFileProcsLite.DirToPath(const Dir: string): string;
  {Turns given directory into path by appending trailing backslash if needed}
begin
  Result := Dir;
  if (Dir <> '') and (Dir[Length(Dir)] <> '\') then
    Result := Result + '\';
end;

class procedure TFileProcsLite.EnsureFolders(Path: string);
  {Ensures that all folders in given path exist}
var
  SlashPos: Integer;    // position of last backslash in path
  SubPath: string;      // immediate parent folder of given path
begin
  // Check there's a path to create
  if Length(Path) = 0 then
    Exit;
  // Remove any trailing '\'
  if Path[Length(Path)] = '\' then
    Delete(Path, Length(Path), 1);
  // We're done if folder exists
  if DirExists(Path) then
    Exit;
  // Recursively call routine on immediate parent folder to ensure it exists
  SubPath := Path;
  SlashPos := Length(SubPath);
  while (SlashPos > 2) and (SubPath[SlashPos] <> '\') do
    Dec(SlashPos);
  Delete(SubPath, SlashPos, Length(Path) - SlashPos + 1);
  EnsureFolders(SubPath);
  // Create this current folder now we know parent folder exists
  CreateDir(Path);
end;

class function TFileProcsLite.RemoveDir(Dir: string): Boolean;
  {Removes an empty directory and returns true on on success}
begin
  if Dir[Length(Dir)] = '\' then
    Delete(Dir, Length(Dir), 1);
  Result := Windows.RemoveDirectory(PChar(Dir));
end;

class function TFileProcsLite.TempFolder: string;
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds temp folder as #0 term str
begin
  // Get temporary folder
  Windows.GetTempPath(MAX_PATH, PathBuf);
  Result := PathBuf;
end;

end.
