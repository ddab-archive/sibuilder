{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UFileProcs.pas
  @COMMENTS                 This unit contains various procedure and functions
                            for managing files, enumerating directories and
                            manipulating file and path names.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/07/2000
      @COMMENTS             Made SizeOfFile function return -1 if file doesn't
                            exist rather than raise exception.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 28/08/2000
      @COMMENTS             Added new routines:
                            + TempFolder
                            + ExecAndWait (ex of UDCC32 - now used in >1 unit)\
                            Removed the following routines:
                            + ListFiles
                            + EnumFiles overloaded routine that takes method as
                              parameter
                            + AddFilePathToList private routine
                            + AddFileNameToList private routine \
                            Made the following routines and types private to
                            unit.
                            + TEnumFileMethod procedural type
                            + TEnumFileProc procedural type
                            + TRecurseFolder enumerated type
                            + ClearFiles routine
                            + EnumFiles overloaded routines
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 20/12/2000
      @COMMENTS             + Added new routine - ExecAndWaitRedirect - to
                              execute a program and wait for it to complete with
                              the added ability to display the program's window
                              as required and to optionally redirect output to
                              given files.
                            + Re-wrote existing ExecAndWait routine to use
                              ExecAndWaitRedirect to do actual processing.
                              ExecAndWait now also takes an additional parameter
                              to determine how program window is displayed.
                              Routine now returns program's error code rather
                              than Boolean value. This re-write fixed a
                              potential bug in the routine.
    )
    @REVISION(
      @VERSION              1.4
      @DATE                 05/08/2002
      @COMMENTS             Changed DirExists function implementation - old
                            version wasn't returning correct result under
                            Windows 2000.
    )
    @REVISION(
      @VERSION              1.5
      @DATE                 29/12/2002
      @COMMENTS             + Added new GetFileVer procedure.
                            + Added new ExtractFolderAndFileNames function.
                            + Added new ExeType function.
                            + Added new IsRegCOMServerDLL function.
                            + Added new WindowsFolder function.
                            + Added new SystemFolder function.
                            + Added new ProgramFilesFolder, CommonFilesFolder
                              and supporting private GetCurrentVersionRegStr
                              function.
                            + Now uses registry keys from Registry.inc rather
                              than hard coded.
    )
    @REVISION(
      @VERSION              1.6
      @DATE                 25/11/2005
      @COMMENTS             + Added new ProgramsFolder function.
                            + Added new DesktopFolder function.
                            + Added new private FreePIDL, PIDLToFolderPaths and
                              SpecialFolderPath routines to support new public
                              routines.
    )
    @REVISION(
      @VERSION              1.7
      @DATE                 23/02/2008
      @COMMENTS             + Added new UserAppDataFolder function.
                            + Replaced usage of Registry.inc include file with
                              URegistry unit.
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
 * The Original Code is UFileProcs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UFileProcs;


interface


uses
  // Delphi
  Windows, Classes,
  // Project
  UCommonTypes;


function MakePathName(const Dir : string) : string;
  {Adds trailing '\' to any path that doesn't have one, unless Dir is empty
  string when no action is taken}

function MakeDirName(const Dir: string) : string;
  {Returns the "file name" of the given directory path - i.e. if Dir is a root
  directory then returns as-is (eg MakeDir('C:\') = 'C:\'), else returns the
  path without the trailing '\' (eg MakeDir('C:\WINDOWS\') = 'C:\WINDOWS')}

function ExtractFolderAndFileNames(const FileSpec: string;
  out DirName, FileName: string): Boolean;
  {Given a file spec this routine divides this into the directory and file
  names. If file spec is a folder the all of name is placed in DirName and
  FileName is set to ''. DirName has trailing '\' removed. If file spec doesn't
  exist then DirName and FileName are set to '' and False is returned. True is
  returned if file spec exists}

function DeleteFiles(const Dir: string): Boolean;
  {Delete all files in given directory, leaving directories intact. Returns
  true if all files are deleted, false if not}

function ForceRemoveFolder(Dir: string): Boolean;
  {Removes given folder and all its contents, returning true if folder is
  successfully removed and false otherwise}

function IsFolderEmpty(const Dir: string): Boolean;
  {Returns true if the given folder is empty and false otherwise}

function DirExists(const Dir: string): Boolean;
  {Returns true if the directory specified by Dir exists and false otherwise}

procedure EnsureFolders(Path: string);
  {Ensures that all folders in path exist}

function GetDocumentPath: string;
  {Returns path to user's My Documents folder}

function GetCurrentPath: string;
  {Returns path to current directory}

function ProgramFilesFolder: string;
  {Returns directory used for program files}

function CommonFilesFolder: string;
  {Returns directory used for common files}

function LongToShortFileName(const LongName: string): string;
  {Convert given long file name to it's shortened counterpart}

function WindowsFolder: string;
  {Returns path to Windows folder}

function SystemFolder: string;
  {Returns path to System folder}

function TempFolder: string;
  {Returns temporary folder}

function TempFileName(const Stub: string): string;
  {Returns a temporary file including the given (max 3 char) stub in its name,
  in the temporary directory}

function DesktopFolder: string;
  {Returns current user's desktop folder}

function ProgramsMenuFolder: string;
  {Returns current user's programs folder in start menu}

function UserAppDataFolder: string;
  {Returns current user's application data folder}

function SizeOfFile(const FName : string) : LongInt;
  {Returns size of given file}

function GetFileDate(const FName: string): Integer;
  {Returns date of given file encoded as integer}

procedure SetFileDate(const FName: string; const ADate: Integer);
  {Sets date of given file to given integer coded value}

procedure GetFileVer(const FileName: string; out VerLS, VerMS: LongWord);
  {Gets file version from version information emebedded in given file and
  returns it in VerLS and VerMS. If there is no version info or there is an
  error reading it VerLS and VerMS are set to 0}

function ExecAndWait(const CommandLine, CurrentDir: string;
  const ShowWindow: Word) : Integer;
  {Run the given application command line using given current directory,
  dislaying window per ShowWindow. (If CurrentDir is '' then no current
  directory is specified). Returns 0 if program terminates OK or error code
  returned by program. If program could not be started then MaxInt is returned}

function ExecAndWaitRedirect(const CmdLine, CurrentDir: string;
  const ShowWindow: Word;
  const InHandle, OutHandle, ErrHandle: THandle): Integer;
  {Run the given application command line using given current directory,
  dislaying window per ShowWindow. (If CurrentDir is '' then no current
  directory is specified). Redirects standard input, output and error streams to
  files with given handles. (Zero handles => not required by program. If all
  handles are zero then does not redirect). Returns 0 if program terminates OK
  or error code returned by program. If program could not be started then
  MaxInt is returned}

function ExeType(const FileName: string): TFileKind;
  {Examines given file and returns a code that indicates the type of executable
  file it is (or if it isn't an executable)}

function IsRegCOMServerDLL(FileName: string): Boolean;
  {Returns true if the given file is a COM server DLL that can register itself}


implementation


uses
  // Delphi
  SysUtils, Registry, ActiveX, ShlObj,
  // Project
  URegistry;

type

  {
  TEnumFileMethod:
    Prototype of methods that can be passed to EnumFiles.
  }
  TEnumFileMethod = procedure(const FName: string;
    const Attr: Integer; const Data: Pointer) of object;

  {
  TEnumFileProc:
    Prototype of procedures that can be passed to EnumFiles.
  }
  TEnumFileProc = procedure(const FName: string;
    const Attr: Integer; const Data: Pointer);

  {
  TRecurseFolder:
    Determines whether EnumFiles recurses folders and, if so, whether the folder
    is recursed before or after processing.
  }
  TRecurseFolder = (rfNone, rfBeforeProc, rfAfterProc);


var
  MethProc: TEnumFileMethod;  // used to store the method used in enumeration


function MakePathName(const Dir : string) : string;
  {Adds trailing '\' to any path that doesn't have one, unless Dir is empty
  string when no action is taken}
begin
  Result := Dir;
  if (Dir <> '') and (Dir[Length(Dir)] <> '\') then
    Result := Result + '\';
end;

function MakeDirName(const Dir: string) : string;
  {Returns the "file name" of the given directory path - i.e. if Dir is a root
  directory then returns as-is (eg MakeDir('C:\') = 'C:\'), else returns the
  path without the trailing '\' (eg MakeDir('C:\WINDOWS\') = 'C:\WINDOWS')}
begin
  // Ensure trailing '\' is added
  Result := MakePathName(Dir);
  // Now decide if it is needed
  if not ((Length(Result) = 3) and (Result[2] = ':')) then
    // Not a root - remove '\'
    Delete(Result, Length(Result), 1);
end;

function ExtractFolderAndFileNames(const FileSpec: string;
  out DirName, FileName: string): Boolean;
  {Given a file spec this routine divides this into the directory and file
  names. If file spec is a folder the all of name is placed in DirName and
  FileName is set to ''. DirName has trailing '\' removed. If file spec doesn't
  exist then DirName and FileName are set to '' and False is returned. True is
  returned if file spec exists}
var
  SourceFileSpec: string; // File spec without any trailing '\'
begin
  // Ensure there's no trailing '\' (unless drive)
  SourceFileSpec := MakeDirName(FileSpec);
  if DirExists(SourceFileSpec) then
  begin
    // This file spec is a directory
    DirName := SourceFileSpec;
    FileName := '';
    Result := True;
  end
  else if FileExists(SourceFileSpec)
    or DirExists(ExtractFileDir(SourceFileSpec)) then
  begin
    // The file spec is a file
    // either file exists or file doesn't exist but its directory does
    DirName := ExtractFileDir(SourceFileSpec);
    FileName := ExtractFileName(SourceFileSpec);
    Result := True;
  end
  else
  begin
    // The file spec doesn't exists
    DirName := '';
    FileName := '';
    Result := False;
  end;
end;

procedure MethToProc(const FName: string; const Attr: Integer;
  const Data: Pointer);
  {Procedure used with EnumFiles to causes processing to be done by method
  stored in MethProc}
begin
  MethProc(FName, Attr, Data);
end;

procedure EnumFiles(Dir: string; const Proc: TEnumFileProc;
  const Data: Pointer; const RecurseFolder: TRecurseFolder); overload;
  {Enumerates all files / sub-folders of given folder and applies Proc to each
  one in turn. If RecurseFolder is not rfNone each sub-folder is also analysed -
  either before or after Proc is applied to the folder itself (depending on the
  value of RecurseFolder}
var
  SearchRec: TSearchRec;  // search record used to find files
  FoundCode: Integer;     // return code of FindFirst / FindNext procs
begin
  // Ensure that Dir has trailing '\'
  Dir := MakePathName(Dir);
  // Search for first file
  FoundCode := FindFirst(Dir + '*.*', faAnyFile, SearchRec);
  try
    // Keep searching while we have a file
    while FoundCode = 0 do
    begin
      // Process file, ignoring . and .. directories
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        // Check if we've got a directory that needs recursing before processing
        if (SearchRec.Attr and faDirectory = faDirectory)
          and (RecurseFolder = rfBeforeProc) then
          EnumFiles(Dir + SearchRec.Name + '\', Proc, Data, RecurseFolder);
        // Process the file
        Proc(Dir + SearchRec.Name, SearchRec.Attr, Data);
        // Check if we've got a directory that needs recursing after processing
        if (SearchRec.Attr and faDirectory = faDirectory)
          and (RecurseFolder = rfAfterProc) then
          EnumFiles(Dir + SearchRec.Name + '\', Proc, Data, RecurseFolder);
      end;
      // Look for next file
      FoundCode := FindNext(SearchRec);
    end;
  finally
    // End the search
    FindClose(SearchRec);
  end;
end;

procedure DeleteIfFile(const FName: string; const Attr: Integer;
  const Data: Pointer);
  {Routine passed to EnumFiles to remove files. If file failure fails then the
  Boolean pointed to by Data is set false, if it not false already}
var
  Deleted: Boolean;     // flag set true if file is deleted
type
  PBoolean = ^Boolean;  // pointer to a boolean
begin
  // Check that we have a file
  if Attr and (faDirectory or faVolumeId) = 0 then
  begin
    // Remove any H & R attributes
    FileSetAttr(FName, Attr and (not (faReadOnly or faHidden)));
    // Delete file
    Deleted := DeleteFile(FName);
    // Update flag if not deleted
    if not Deleted and PBoolean(Data)^ then
      PBoolean(Data)^ := False;
  end;
end;

function DeleteFiles(const Dir: string): Boolean;
  {Delete all files in given directory, leaving directories intact. Returns
  true if all files are deleted, false if not}
begin
  // Record default "OK" result
  Result := True;
  // Enumerate all files/folders, deleting just the files
  EnumFiles(Dir, DeleteIfFile, @Result, rfNone);
end;

procedure DeleteAll(const FName: string; const Attr: Integer;
  const Data: Pointer);
  {Routine passed to EnumFiles to delete given file or folder (which must be
  empty to be deleted)}
var
  Deleted: Boolean;     // flag set true if file is deleted
type
  PBoolean = ^Boolean;  // pointer to a boolean
begin
  // Remove any H & R attributes
  FileSetAttr(FName, Attr and (not (faReadOnly or faHidden)));
  // Now remove file / folder
  if Attr and faDirectory = faDirectory then
    // Remove this folder - which should have been emptied
    Deleted := RemoveDir(FName)
  else if Attr and faVolumeId <> faVolumeID then
    // Delete this file
    Deleted := DeleteFile(FName)
  else // this is a volume id
    // We take no action but don't want to upset delete flag, so set true
    Deleted := True;
  // Update flag if not deleted
  if not Deleted and PBoolean(Data)^ then
    PBoolean(Data)^ := False;
end;

function ClearFolder(Dir: string): Boolean;
  {Delete all files and sub-folders of given directory, recursively deleting
  contents of sub-folders and return true if everything is deleted}
begin
  // Set default "OK" result
  Result := True;
  // Enumerate files/sub-folders, deleting them
  EnumFiles(Dir, DeleteAll, @Result, rfBeforeProc);
end;

function ForceRemoveFolder(Dir: string): Boolean;
  {Removes given folder and all its contents, returning true if folder is
  successfully removed and false otherwise}
var
  Attr: Integer;    // the folder's attributes
begin
  // Clear contents of folder and record result
  Result := ClearFolder(Dir);
  // If folder cleared then we can remove the folder itself
  if Result then
  begin
    // Make folder have required format for removing (i.e. no trailing '\')
    Dir := MakeDirName(Dir);
    // Remove any H & R attributes
    Attr := FileGetAttr(Dir);
    FileSetAttr(Dir, Attr and (not (faReadOnly or faHidden)));
    // Remove the folder
    Result := RemoveDir(Dir);
  end;
end;

procedure CheckIfEmpty(const FName: string; const Attr: Integer;
  const Data: Pointer);
  {If this procedure is called by EnumFiles then it is for a file or folder ->
  the parent folder is not empty. Therefore simply sets boolean variable pointed
  to by Data to false}
type
  PBoolean = ^Boolean;    // pointer to a Boolean
begin
  // Store false in given boolean
  (PBoolean(Data))^ := False;
end;

function IsFolderEmpty(const Dir: string): Boolean;
  {Returns true if the given folder is empty and false otherwise}
begin
  // Set default "OK" result
  Result := True;
  // Enumerate all files / sub-folders in folder, any file that is enumerated
  // will cause Result to be set true
  EnumFiles(Dir, CheckIfEmpty, @Result, rfNone);
end;

function DirExists(const Dir: string) : Boolean;
  {Returns true if the directory specified by Dir exists and false otherwise}
var
  Code: DWORD;  // Result returned from API when checking file attributes
begin
  // Get attributes of directory
  Code := Windows.GetFileAttributes(PChar(Dir));
  // This is a directory if:
  //  function doesn't return $FFFFFFFF (which indicates function failed) and
  //  file has directory attributes
  Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

procedure EnsureFolders(Path: string);
  {Ensures that all folders in path exist}
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
  // Check if folder exists and quit if it does - we're done
  if DirExists(Path) then
    Exit;
  // Recursively call routine on immediate parent folder
  // remove bottomost folder from path - ie move up to parent folder
  SubPath := Path;
  SlashPos := Length(SubPath);
  while (SlashPos > 2) and (SubPath[SlashPos] <> '\') do
    Dec(SlashPos);
  Delete(SubPath, SlashPos, Length(Path) - SlashPos + 1);
  // do recursive call - ensures that parent folder of current path exist
  EnsureFolders(SubPath);
  // Create this current folder now we know parent folder exists
  CreateDir(Path);
end;

function GetCurrentVersionRegStr(const SubKey, ValName: string): string;
  {Gets string info from Windows current version key in registry}
var
  Reg: TRegistry; // registry access object
begin
  // Open registry at the required key
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(cWdwCurrentVer) then
      // Read path required
      Result := Reg.ReadString(ValName)
    else
      Result := '';
  finally
    Reg.Free;
  end;
end;

function ProgramFilesFolder: string;
  {Returns directory used for program files}
begin
  Result := GetCurrentVersionRegStr('', 'ProgramFilesDir');
end;

function CommonFilesFolder: string;
  {Returns directory used for common files}
begin
  Result := GetCurrentVersionRegStr('', 'CommonFilesDir');
end;

function GetDocumentPath: string;
  {Returns path to user's My Documents folder}
var
  Reg: TRegistry;   // registry object
begin
  // Set default "no such folder" result
  Result := '';
  // OPen registry and the required key
  Reg := TRegistry.Create;
  try
    if Reg.OpenKeyReadOnly(cWdwShellFolders) then
      // Read the "My Documents" folder from registry
      Result := Reg.ReadString('Personal');
    if Result = '' then
      // We didn't get it from registry - return root of C drive
      Result := 'C:\';
  finally
    // Get rid of registry object
    Reg.Free;
  end;
end;

function GetCurrentPath: string;
  {Returns path to current directory, without trailing '\'}
var
  CurPath: array[0..MAX_PATH] of Char;    // the current path as #0 term string
begin
  // Get current directoy from Windows via API call
  GetCurrentDirectory(MAX_PATH, CurPath);
  // Convert returned #0 term string to pascal string
  Result := StrPas(CurPath);
end;

function LongToShortFileName(const LongName: string): string;
  {Convert given long file name to it's shortened counterpart}
var
  ShortPath: array[0..MAX_PATH] of Char;  // the short path as #0 term string
begin
  // Get short path name from Windows via API call
  GetShortPathName(PChar(LongName), ShortPath, MAX_PATH);
  // Convert returned #0 term string to pascal string
  Result := StrPas(ShortPath);
end;

function WindowsFolder: string;
  {Returns path to Windows folder}
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds windows folder as #0 term str
begin
  GetWindowsDirectory(PathBuf, MAX_PATH);
  Result := StrPas(PathBuf);
end;

function SystemFolder: string;
  {Returns path to System folder}
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds system folder as #0 term str
begin
  GetSystemDirectory(PathBuf, MAX_PATH);
  Result := StrPas(PathBuf);
end;

function TempFolder: string;
  {Returns temporary folder}
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds temp folder as #0 term str
begin
  // Get temporary folder
  GetTempPath(MAX_PATH, PathBuf);
  Result := StrPas(PathBuf);
end;

function TempFileName(const Stub: string): string;
  {Returns a temporary file including the given (max 3 char) stub in its name,
  in the temporary directory}
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds temp folder as #0 term str
  FileBuf: array[0..MAX_PATH] of Char;  // holds temp file name as #0 term str
begin
  // Get temporary folder
  GetTempPath(MAX_PATH, PathBuf);
  // Get temporary file in temp folder
  GetTempFileName(PathBuf, PChar(Stub), 0, FileBuf);
  // Convert resulting file name to Pascal string
  Result := StrPas(FileBuf);
end;

function SizeOfFile(const FName : string) : LongInt;
  {Returns size of given file or -1 on error}
var
  Stream: TFileStream;    // file stream object used to get file size
begin
  // OPen a file stream object for the given file
  try
    Stream := TFileStream.Create(FName, fmOpenRead);
    try
      // Record file size
      Result := Stream.Size;
    finally
      // Close file stream
      Stream.Free;
    end;
  except
    on E: EStreamError do
      Result := -1;
  end;
end;

function GetFileDate(const FName: string): Integer;
  {Returns date of given file encoded as integer}
var
  FileH: Integer;   // file handle
begin
  // Open file
  FileH := FileOpen(FName, fmOpenRead);
  if FileH = -1 then
    // Couldn't open file - return -1 to indicate can't get date
    Result := -1
  else
  begin
    // File opened OK - record date and close file
    Result := FileGetDate(FileH);
    FileClose(FileH);
  end;
end;

procedure SetFileDate(const FName: string; const ADate: Integer);
  {Sets date of given file to given integer coded value}
var
  FileH: Integer;   // file handle
begin
  // Open file
  FileH := FileOpen(FName, fmOpenWrite);
  if FileH <> -1 then
  begin
    // File opened OK - set date and close file
    FileSetDate(FileH, ADate);
    FileClose(FileH);
  end;
end;

procedure GetFileVer(const FileName: string; out VerLS, VerMS: LongWord);
  {Gets file version from version information emebedded in given file and
  returns it in VerLS and VerMS. If there is no version info or there is an
  error reading it VerLS and VerMS are set to 0}
var
  VerInfoBuf: Pointer;    // points to memory storing version info
  VerInfoSize: Integer;   // size of version info memory
  Dummy: THandle;         // unused parameter required by API function
  PFFI: Pointer;          // points to fixed file info
  FFISize: LongWord;      // size of file file info returned from API (unused)
begin
  // Assume failure: sets zero result
  VerLS := 0;
  VerMS := 0;
  // Get size of version info: there is none if this is zero
  VerInfoSize := Windows.GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize > 0 then
  begin
    // Allocate memory to store ver info
    GetMem(VerInfoBuf, VerInfoSize);
    try
      // Get the version info, filling buffer
      if Windows.GetFileVersionInfo(
        PChar(FileName), Dummy, VerInfoSize, VerInfoBuf
      ) then
      begin
        // Get a pointer to fixed file info
        if VerQueryValue(VerInfoBuf, '\', PFFI, FFISize) then
        begin
          // Got pointer OK: record file version
          VerLS := PVSFixedFileInfo(PFFI)^.dwFileVersionLS;
          VerMS := PVSFixedFileInfo(PFFI)^.dwFileVersionMS;
        end;
      end;
    finally
      // Dispose of ver info storage
      FreeMem(VerInfoBuf, VerInfoSize);
    end;
  end;
end;

function ExecAndWaitRedirect(const CmdLine, CurrentDir: string;
  const ShowWindow: Word;
  const InHandle, OutHandle, ErrHandle: THandle): Integer;
  {Run the given application command line using given current directory,
  dislaying window per ShowWindow. (If CurrentDir is '' then no current
  directory is specified). Redirects standard input, output and error streams to
  files with given handles. (Zero handles => not required by program. If all
  handles are zero then does not redirect). Returns 0 if program terminates OK
  or error code returned by program. If program could not be started then
  MaxInt is returned}
var
  StartupInfo: TStartupInfo;        // start-up information passed to process
  ProcessInfo: TProcessInformation; // information about the process
  ProcessExitCode: DWord;           // the process's exit code
  PCurDir: PChar;                   // the current directory
  Redirect: Boolean;                // flag set true if we are to redirect
begin
  // Assume failure
  Result := -MaxInt;
  // Determine if we're actually redirecting
  Redirect := (InHandle <> 0) or (OutHandle <> 0) or (ErrHandle <> 0);
  // Set up StartupInfo structure
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);                              // size of structure
    hStdOutput := OutHandle;                   // redirection of standard output
    hStdError := ErrHandle;                       // redirection of error output
    hStdInput := InHandle;                      // redirection of standard input
    wShowWindow := ShowWindow;                        // how window is displayed
    dwFlags := STARTF_USESHOWWINDOW;                // use the wShowWindow field
    if Redirect then
      dwFlags := dwFlags or STARTF_USESTDHANDLES;    // we control how displayed
  end;
  // Establish if current directory is specified, and what it is
  if CurrentDir = '' then
    PCurDir := nil
  else
    PCurDir := PChar(CurrentDir);
  // Now execute the application
  if CreateProcess(
    nil,                // application name is passed as part of command line
    PChar(CmdLine),     // the command line, including application name
    nil, nil,           // security and thread handles: ignored
    Redirect,           // application inherits handles if redirecting
    0,                  // no special creation flags
    nil,                // no environment passed
    PCurDir,            // the current directory
    StartupInfo,        // startup information (see above)
    ProcessInfo         // holds information about process once started
  ) then
  begin
    try
      // Wait for application to complete
      if WaitForSingleObject(ProcessInfo.hProcess, INFINITE)
        = WAIT_OBJECT_0 then
        // It's completed - get its exit code and return it
        if GetExitCodeProcess(ProcessInfo.hProcess, ProcessExitCode) then
          Result := Integer(ProcessExitCode)
    finally
      // Tidy up
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;
end;

function ExecAndWait(const CommandLine, CurrentDir: string;
  const ShowWindow: Word) : Integer;
  {Run the given application command line using given current directory,
  dislaying window per ShowWindow. (If CurrentDir is '' then no current
  directory is specified). Returns 0 if program terminates OK or error code
  returned by program. If program could not be started then MaxInt is returned}
begin
  Result := ExecAndWaitRedirect(CommandLine, CurrentDir, ShowWindow, 0, 0, 0);
end;

function ExeType(const FileName: string): TFileKind;
  {Examines given file and returns a code that indicates the type of executable
  file it is (or if it isn't an executable)}
const
  cWinHeaderOffset = $3C; // offset of "pointer" to windows header in file
  cDOSMagic = $5A4D;      // magic number identifying a DOS executable
  cNEMagic = $454E;       // magic number identifying a NE executable (Win 16)
  cPEMagic = $4550;       // magic nunber identifying a PE executable (Win 32)
var
  FS: TFileStream;              // stream to executable file
  DOSMagic: Word;               // word that should contain DOS magic number
  Offset: LongInt;              // offset of windows header in exec file
  WinMagic: Word;               // word that contains PE or NE magic numbers
  ImgHdrPE: IMAGE_FILE_HEADER;  // PE file header record
  AppFlagsNE: Byte;             // Byte defining DLLs in NE format
begin
  // Default result is unkown
  Result := fkUnknown;
  try
    // Error result if file doesn't exist
    if not (FileExists(FileName)) then
    begin
      Result := fkError;
      Exit;
    end;
    // Open file for analysis
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      // Try to read word at start of file: exit if not DOS magic number
      if FS.Size < SizeOf(Word) then
        Exit;
      FS.ReadBuffer(DOSMagic, SizeOf(Word));
      if DOSMagic <> cDOSMagic then
        Exit;
      // We no we have at least a DOS file
      Result := fkDOS;
      // Try to find offset of windows program header
      if FS.Size <= cWinHeaderOffset + SizeOf(LongInt) then
        Exit;
      FS.Position := cWinHeaderOffset;
      FS.ReadBuffer(Offset, SizeOf(LongInt));
      // Now try to read first word of Windows program header
      if FS.Size <= Offset + SizeOf(Word) then
        Exit;
      FS.Position := Offset;
      FS.ReadBuffer(WinMagic, SizeOf(Word));
      // This word should identifies either a NE or PE format file: check which
      if WinMagic = cNEMagic then
      begin
        // Check for DLL
        if FS.Size <= Offset + $0D then
        begin
          Result := fkError;
          Exit;
        end;
        FS.Position := Offset + $0D;
        FS.ReadBuffer(AppFlagsNE, SizeOf(AppFlagsNE));
        if (AppFlagsNE and $80) = $80 then
          Result := fkDLL16
        else
          Result := fkExe16
      end
      else if WinMagic = cPEMagic then
      begin
        // Check for DLL
        if FS.Size < Offset + SizeOf(LongWord) + SizeOf(ImgHdrPE) then
        begin
          Result := fkError;
          Exit;
        end;
        FS.Position := Offset + SizeOf(LongWord);
        FS.ReadBuffer(ImgHdrPE, SizeOf(ImgHdrPE));
        if (ImgHdrPE.Characteristics and IMAGE_FILE_DLL) = IMAGE_FILE_DLL then
        begin
          // we have 32 bit DLL - check if its a COM server
          if UFileProcs.IsRegCOMServerDLL(FileName) then
            Result := fkCOMDLL
          else
            Result := fkDLL32;
        end
        else
          // we have 32 bit application
          Result := fkExe32;
      end;
    finally
      FS.Free;
    end;
  finally
    // If we haven't determined file type its either batch or data per ext
    if Result = fkUnknown then
      if AnsiCompareText(ExtractFileExt(FileName), '.bat') = 0 then
        Result := fkBatch
      else
        Result := fkData;
  end;
end;

function IsRegCOMServerDLL(FileName: string): Boolean;
  {Returns true if the given file is a COM server DLL that can register itself}
var
  DLLHandle: THandle;     // handle to loaded DLL
  OldErrorMode: Integer;  // stores Windows error mode for later restoration
begin
  // Assume failure
  Result := False;
  // Prevent windows dialog appearing if can't find/load DLL
  OldErrorMode := SetErrorMode(
    SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS
  );
  try
    // Try to load file as DLL
    DLLHandle := LoadLibrary(PChar(FileName));
    if DLLHandle <> 0 then
    begin
      // DLL loaded: check that all COM server exported functions are available
      if Assigned(GetProcAddress(DLLHandle, 'DllGetClassObject'))
        and Assigned(GetProcAddress(DLLHandle, 'DllCanUnloadNow'))
        and Assigned(GetProcAddress(DLLHandle, 'DllRegisterServer'))
        and Assigned(GetProcAddress(DLLHandle, 'DllUnregisterServer')) then
        Result := True;
      // Unload the DLL
      FreeLibrary(DLLHandle);
    end;
  finally
    // Restore windows error mode
    SetErrorMode(OldErrorMode);
  end;
end;

procedure FreePIDL(PIDL: PItemIDList);
  {Uses to shell allocator to free the memory used by a given PIDL.}
var
  Malloc: IMalloc;  // shell's allocator
begin
  // Try to get shell allocator
  if Succeeded(SHGetMalloc(Malloc)) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    Malloc.Free(PIDL);
end;

function PIDLToFolderPath(PIDL: PItemIDList): string;
  {Returns the full path to a file system folder from a PIDL or '' if the PIDL
  refers to a virtual folder.}
begin
  // Set max length of return string
  SetLength(Result, MAX_PATH);
  // Get the path
  if SHGetPathFromIDList(PIDL, PChar(Result)) then
    Result := PChar(Result)
  else
    Result := '';
end;

function SpecialFolderPath(CSIDL: Integer): string;
  {Returns the full path to a special file system folder specified by a CSIDL
  constant FolderID or '' if the special folder is virtual or CSIDL is not
  supported.}
var
  PIDL: PItemIDList;  // PIDL of the special folder
begin
  Result := '';
  // Get PIDL for required folder
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, PIDL)) then
  begin
    try
      // Get path from PIDL
      Result := PIDLToFolderPath(PIDL);
    finally
      // Free the PIDL using shell allocator
      FreePIDL(PIDL);
    end;
  end
end;

function DesktopFolder: string;
  {Returns current user's desktop folder}
begin
  Result := SpecialFolderPath(CSIDL_DESKTOPDIRECTORY);
end;

function ProgramsMenuFolder: string;
  {Returns current user's programs folder in start menu}
begin
  Result := SpecialFolderPath(CSIDL_PROGRAMS);
end;

function UserAppDataFolder: string;
  {Returns current user's application data folder}
begin
  Result := SpecialFolderPath(CSIDL_APPDATA);
end;

end.

