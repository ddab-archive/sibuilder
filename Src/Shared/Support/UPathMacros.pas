{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UPathMacros.pas
  @COMMENTS                 This unit defines the UPathMacros class and creates
                            global instance, PathMacros. This maintains a list
                            of "macros" that can be included in file paths and
                            can be later expanded into actual paths, depending
                            on the system where they are expanded, or where they
                            are defined. TPathMacros provides methods to
                            maintain and expand the macros.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/11/2001
      @COMMENTS             Changed way in which Program Files folder is found:
                            instead of hard wiring value it is now fetched from
                            registry.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 01/04/2002
      @COMMENTS             Added new "Common Files" path macro which expands to
                            the location of the Common Files folder on the
                            destination system.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 29/12/2002
      @COMMENTS             + Updated PathToMacro method to fix bug where test
                              for inclusion in a path was failing with nested
                              macros. Now we expand macros before testing them.
                            + Changed routine that checks regisret for system
                              folders to only open key for read only to enable
                              to work for users with limited privileges.
                            + Added new [Temp] path macro for system's temporary
                              folder.
                            + Replaced API and helper function calls to get
                              windows, system, program file and common files
                              folders with calls to functions in UFileProcs.
    )
    @REVISION(
      @VERSION              1.4
      @DATE                 25/11/2005
      @COMMENTS             Added new "Desktop" and "Programs Menu" path macros
                            which expand to current user's desktop and programs
                            start menu folders respectively.
    )
    @REVISION(
      @VERSION              1.5
      @DATE                 15/01/2006
      @COMMENTS             Added new "ScratchPad" path macro that returns a
                            temporary scratch pad directory that is only
                            available during installation. Also added event that
                            is triggered when ScratchPad is requested to get
                            actual folder from external code. This is necessary
                            because actual location is not known until the
                            installer runs.
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
 * The Original Code is UPathMacros.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2006 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UPathMacros;


interface


uses
  // Delphi
  SysUtils, Classes, Windows;

type

  {
  TPathMacroNames:
    IDs of the available macros.
  }
  TPathMacroNames = (
    pnInstall,        // installation path: assigned by SIBuilder or user
    pnProgramFiles,   // Program Files folder
    pnWindows,        // Windows folder
    pnSystem,         // System folder
    pnCommonFiles,    // Common Files folder
    pnTemp,           // Temp folder
    pnDesktop,        // Desktop directory for current user
    pnProgramsMenu,   // Start menu programs menu directory for current user
    pnScratchPad      // Scratch pad folder for temp files
                      // only available during installation
  );

  {
  EPathMacros:
    Class of exception raised by TPathMacros.

    Inheritance: EPathMacros -> [Exception]
  }
  EPathMacros = class(Exception);

  {
  TOnGetScratchPad:
    Type of event handler for TPathMacros.OnGetScratchPad event.
  }
  TOnGetScratchPad = procedure(var FolderPath: string) of object;

  {
  TPathMacros:
    Class that maintains a list of "macros" that can be included in file paths
    and can be later expanded into actual paths, depending on the system where
    they are expanded, or where they are defined. Supported macros are:
      + [Windows]       - expands to Windows path of system
      + [System]        - expands to Windows System path of system
      + [Temp]          - expands to Windows temporary folder path
      + [Program Files] - expands to path of Program Files folder of system
      + [Common Files]  - expands to path of Common Files folder of system
      + [Install Path]  - expands to path specified in project
      + [Desktop]       - expands to path of current user's desktop folder
      + [Programs Menu] - expands to path of current user's start menu folder
      + [Unique Temp]   - expands to path of unique folder in temporary folder
    Class can convert between actual paths and paths containing macros. It is
    designed to be instantiated just once as a global object

    Inheritance: TPathMacros -> [TObject]
  }
  TPathMacros = class(TObject)
  private // properties
    fPaths: array[TPathMacroNames] of string;
    fOnGetScratchPad: TOnGetScratchPad;
    function GetInstallPath: string;
    procedure SetInstallPath(const APath: string);
    function GetPath(MacroId: TPathMacroNames): string;
    function GetName(MacroId: TPathMacroNames): string;
  private
    function IndexOfMacroName(const MacroName: string): Integer;
      {Returns the index of the given macro name, or -1 if not a valid name}
    function ExtractMacro(const MacroPath: string): string;
      {Extracts the macro name, if any, from a path, MacroPath, that may begin
      with a macro and returns the macro, or '' if no macro. The macro is not
      validated}
  public
    constructor Create;
      {Class constructor: store paths for all macros (except [Install Files]).
      Parameter gives path to special unique folder within windows temp folder}
    class procedure GetMacroNames(const MacroList: TStrings);
      {Copies names of macros into given TStrings object}
    function PathToMacro(const Path: string): string;
      {Given a path, checks to see if part of the path is the same as one of the
      macro paths and returns the path relative to the macro path.
      EG: PathToMacro('C:\Windows\Temp') returns '[Windows]\Temp'}
    function ExpandMacroPath(const MacroPath: string): string;
      {Given a path containing a macro path, expand to the actual path
      EG: ExpandPathMacro('[Windows]\Temp') returns 'C:\Windows\Temp'}
    function ValidateMacroPath(const MacroPath: string): Boolean;
      {Returns True if MacroPath begins with a valid macro or no macro, false
      otherwise}
    function IncludesMacro(const MacroPath: string;
      const MacroId: TPathMacroNames) : Boolean;
      {Returns true if the given path includes the macro with the given id}
    property InstallPath: string read GetInstallPath write SetInstallPath;
      {The name of the installation path - reading this property is equivalent
      to reading Paths['[Install Path]']}
    property Paths[MacroName: TPathMacroNames]: string read GetPath;
      {The file system paths associated with the macro ids}
    property Names[MacroId: TPathMacroNames]: string read GetName;
      {The macro names, indexed by id}
    property OnGetScratchPad: TOnGetScratchPad
      read fOnGetScratchPad write fOnGetScratchPad;
      {Event triggered when ScratchPad path macro accessed. If handled should
      return path to scratch pad folder}
  end;

var
  // Variable for global instance of class
  PathMacros: TPathMacros;


implementation


uses
  // Delphi
  Registry,
  // Project
  UFileProcs;

const
  // Table of available macro names
  cMacroNames: array[TPathMacroNames] of string = (
    '[Install Path]',
    '[Program Files]',
    '[Windows]',
    '[System]',
    '[Common Files]',
    '[Temp]',
    '[Desktop]',
    '[Programs Menu]',
    '[Scratch Pad]'
  );

resourcestring
  // Error messages
  sBadMacro = '"%s" is not a valid macro';


{ TPathMacros }

constructor TPathMacros.Create;
  {Class constructor: store paths for all macros (except [Install Files])}
begin
  inherited Create;
  // Record the macro expansions
  fPaths[pnWindows]       := UFileProcs.WindowsFolder;
  fPaths[pnSystem]        := UFileProcs.SystemFolder;
  fPaths[pnTemp]          := UFileProcs.TempFolder;
  fPaths[pnProgramFiles]  := UFileProcs.ProgramFilesFolder;
  fPaths[pnCommonFiles]   := UFileProcs.CommonFilesFolder;
  fPaths[pnDesktop]       := UFileProcs.DesktopFolder;
  fPaths[pnProgramsMenu]  := UFileProcs.ProgramsMenuFolder;
  fPaths[pnScratchPad]    := '';
end;

function TPathMacros.ExpandMacroPath(const MacroPath: string): string;
   {Given a path containing a macro path, expand to the actual path
   EG: ExpandPathMacro('[Windows]\Temp') returns 'C:\Windows\Temp'}
var
  Index: Integer;     // index of a macro name in the table of macro names
  MacroName: string;  // the macro name currently being processed
begin
  // Set result to given path - there may be no macros to expand
  Result := MacroPath;
  // Extract the macro at the start of the given path, if any
  MacroName := ExtractMacro(Result);
  // Loop while there is still a macro name at start of resulting path
  while MacroName <> '' do
  begin
    // Find index of current macro name in table
    Index := IndexOfMacroName(MacroName);
    if Index = -1 then
      // Current macro name is not in table - raise exception
      raise EPathMacros.CreateFmt(sBadMacro, [MacroName]);
    // Replace macro with it's expansion (which may include another macro)
    Result := StringReplace(Result, cMacroNames[TPathMacroNames(Index)],
      GetPath(TPathMacroNames(Index)), [rfIgnoreCase]);
    // Now record any other macro that begins the modified path and loop
    MacroName := ExtractMacro(Result);
  end;
end;

function TPathMacros.ExtractMacro(const MacroPath: string): string;
  {Extracts the macro name, if any, from a path, MacroPath, that may begin with
  a macro and returns the macro, or '' if no macro. The macro is not validated}
var
  EndPos: Integer;    // the postion of the end of the macro within a string
begin
  // Set default "no macro" result
  Result := '';
  // Check if we have a macro at start of MacroPath
  if (Length(MacroPath) >= 1) and (MacroPath[1] = '[') then
  begin
    // Find end of macro, if any
    EndPos := Pos(']', MacroPath);
    if EndPos > 1 then
      // We have a macro - extract it
      Result := Copy(MacroPath, 1, EndPos);
  end;
end;

function TPathMacros.GetInstallPath: string;
  {Read access method for InstallPath property}
begin
  Result := GetPath(pnInstall);
end;

class procedure TPathMacros.GetMacroNames(const MacroList: TStrings);
  {Copies names of macros into given TStrings object}
var
  I: TPathMacroNames;     // loops thru table of macro names
begin
  // Clear given string list
  MacroList.Clear;
  // Scan thru macro name table adding entries to string list
  for I := Low(TPathMacroNames) to High(TPathMacroNames) do
    MacroList.Add(cMacroNames[I]);
end;

function TPathMacros.GetName(MacroId: TPathMacroNames): string;
  {Read access method for Names property}
begin
  Result := cMacroNames[MacroId];
end;

function TPathMacros.GetPath(MacroId: TPathMacroNames): string;
  {Read access method for Paths property}
begin
  Result := '';
  if MacroId <> pnScratchPad then
    Result := fPaths[MacroId]
  else if Assigned(OnGetScratchPad) then
    OnGetScratchPad(Result);
end;

function TPathMacros.IncludesMacro(const MacroPath: string;
  const MacroId: TPathMacroNames): Boolean;
  {Returns true if the given path includes the macro with the given id}
begin
  Result := (Pos(UpperCase(cMacroNames[MacroId]), UpperCase(MacroPath)) > 0);
end;

function TPathMacros.IndexOfMacroName(const MacroName: string): Integer;
  {Returns the index of the given macro name, or -1 if not a valid name}
var
  I: TPathMacroNames;     // loops thru all macro names
begin
  // Set default "not found" result
  Result := -1;
  // Scan through array of valid macro names
  for I := Low(TPathMacroNames) to High(TPathMacroNames) do
    if CompareText(MacroName, cMacroNames[I]) = 0 then
    begin
      // We've found a match - return it's index
      Result := Ord(I);
      Break;
    end;
end;

function TPathMacros.PathToMacro(const Path: string): string;
  {Given a path, checks to see if part of the path is the same as one of the
  macro paths and returns the path relative to the macro path.
  EG: PathToMacro('C:\Windows\Temp') returns '[Windows]\Temp'}
var
  MatchedLength: Integer;   // longest macro path matched to given path
  Index: TPathMacroNames;   // index of matching macro in table
  I: TPathMacroNames;       // loops through macro name table
  ExpPath: string;          // fully expanded path
  ExpMacro: string;         // fully expanded macro path
begin
  // Ensure path passes to us is fully expanded before we process it
  ExpPath := ExpandMacroPath(Path);
  Index := Low(TPathMacroNames);  // keeps compiler warnings quiet!!!
  // Record that we have not yet matched any characters of path with a macro
  MatchedLength := 0;
  // Loop thru macro names table, comparing each macro's path with given path
  for I := Low(TPathMacroNames) to High(TPathMacroNames) do
  begin
    // Expand any macros in the macro before checking
    ExpMacro := ExpandMacroPath(GetPath(I));
    // Check if current macro's path matches start of given path and if so, is
    // it a longer match than any previous one
    if (Pos(UpperCase(ExpMacro), UpperCase(ExpPath)) > 0)
      and (Length(ExpMacro) > MatchedLength) then
    begin
      // Record length of macro path - we need to user longest match since some
      // macros begin with same path - eg [SYSTEM] = [WINDOWS]\System
      MatchedLength := Length(ExpMacro);
      // Record index of matching macro in table
      Index := I;
    end;
  end;
  // Set result depending on whether we matched a macro or not
  if MatchedLength > 0  then
    // a macro was matched - replace its path in string with macro
    Result := StringReplace(
      ExpPath,                          // path passed to method
      ExpandMacroPath(GetPath(Index)),  // replace expanded macro in path
      cMacroNames[Index],               // .. with this macro name
      [rfIgnoreCase]                    // ignore case and replace 1st instance
    )
  else
    // there was no match - return given path
    Result := Path;
end;

procedure TPathMacros.SetInstallPath(const APath: string);
  {Write access method for InstallPath property}
begin
  // Prevent PathToMacro using [Install Path] when inserting macro in path
  fPaths[pnInstall] := '';
  // Record the given path which may include macros except [Install Path] itself
  fPaths[pnInstall] := PathToMacro(APath);
end;

function TPathMacros.ValidateMacroPath(const MacroPath: string): Boolean;
  {Returns True if MacroPath begins with a valid macro or no macro, false
  otherwise}
var
  MacroName: string;      // the name of the macro that begins given path
begin
  // Record any macro that begins path
  MacroName := ExtractMacro(MacroPath);
  // Check if there was a macro and, if so, does it exists
  Result := (MacroName = '')
    or (IndexOfMacroName(ExtractMacro(MacroPath)) <> -1);
end;


initialization

// Create a global instance of the object

PathMacros := TPathMacros.Create;

finalization

// Free the global object instance

PathMacros.Free;

end.
