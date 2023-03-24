{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstallBody.pas
  @COMMENTS                 This unit provides the core code used to extract
                            files from an install program (using an extractor
                            object provided by installer) and then to initiate
                            the installation.\
                            The code is based on that from earlier versions of
                            Install.dpr, but generalised to use extractor
                            objects.
  @DEPENDENCIES             Requires the InstallLib.dll which will be extracted
                            from the install program.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 16/01/2006
      @COMMENTS             + Changed to extract files to a sub folder of the
                              local computer's temporary folder rather than the
                              temporary folder itself.
                            + Changed copyright date displayed in console.
                            + Refactored to use new TFileProcsLite for some
                              functionality that was previously in this unit.
                            + Also changed to use new interface to
                              InstallLib.dll Install exported function.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 25/03/2007
      @COMMENTS             Removed conditional compilation of FileNames.inc
                            include statement now that unit is never compiled
                            from SIBuilder.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of FileNames.inc include file with
                              UFileNames unit.
                            + Replaced messages in string literals with resource
                              strings and made minor code changes to suit.
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
 * The Original Code is UInstallBody.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInstallBody;


interface


uses
  // Project
  UExtractor, UFileProcsLite;


procedure Install(var Extractor: TExtractor);
  {Main installation code - to be called from each install program project,
  which must provide an extractor object (which is freed and nilled within this
  routine}


implementation


uses
  // Delphi
  Windows,
  // Project
  UFileNames, UInstProcs, UInstExcept, UInstallerFiles;

var
  DLLHandle: THandle = 0;
    {Handle to installation library DLL}
  DoInstall: function(const InstFiles: TInstallerFiles;
    PInstallPath: PChar): WordBool;
    {DLL function to perform installation}
  SetMsgProc: procedure(TheMsgProc: Pointer);
    {DLL procedure that sets call back procedure that displays messages issued
    by DLL code}
  SetQueryProc: procedure(TheQueryProc: Pointer);
    {DLL procedure that sets call back procedure that gets install path from
    user}
  ErrorStr: function: PChar;
    {DLL function returning details of any error that caused install function to
    fail}


resourcestring
  // Program essages
  sDLLCallError = 'Can''t call into installation DLL';
  sDLLLoadError = 'Can''t load installation DLL';
  sInstallFailed = 'INSTALL FAILED: ';
  sInstallSuccess = 'Success';
  // Program prompts
  sPressReturn = 'Press return to exit';
  sInstallPathPrompt = 'Please enter the path where program is to be installed'
    + #13#10'or press return to accept the following path:';
  // Program sign-on
  sTitle = 'DelphiDabbler SITools 3 - Installer';
  sCopyright = 'Copyright (c) P.D.Johnson, 2000-2008';


procedure Error(const Msg: string); forward;

procedure LoadDLL(FileName: string);
  {Loads the installation DLL and the imported functions}
begin
  // Try to load the DLL
  DLLHandle := LoadLibrary(PChar(FileName));
  if DLLHandle <> 0 then
  begin
    // Success get reference to required routines in DLL
    DoInstall := GetProcAddress(DLLHandle, 'Install');
    SetMsgProc := GetProcAddress(DLLHandle, 'SetMsgProc');
    SetQueryProc := GetProcAddress(DLLHandle, 'SetQueryProc');
    ErrorStr := GetProcAddress(DLLHandle, 'ErrorStr');
    if (@DoInstall = nil)
      or (@SetMsgProc = nil)
      or (@SetQueryProc = nil)
      or (@ErrorStr = nil) then
      // Couldn't get reference to one or more routines
      Error(sDLLCallError);
  end
  else
    // Couldn't load DLL
    Error(sDLLLoadError);
end;

procedure FreeDLL;
  {Frees the installation DLL if it has been loaded}
begin
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle);
end;

procedure Error(const Msg: string);
  {Reports any error and terminates installation after tidying up}
begin
  // Free the installation DLL if loaded
  FreeDLL;
  // Write error message, get user to press return
  WriteLn(sInstallFailed + Msg);
  WriteLn(sPressReturn);
  ReadLn;
  // Terminate program
  Halt;
end;

procedure Display(const Msg: string);
  {Displays given message - this is used as a callback function for messages
  generated by DLL. Simply writes message on console}
begin
  WriteLn(Msg);
end;

procedure GetInstPath(Path: PChar);
  {Gets install path from user, using given path as default}
var
  InPath: ShortString;      // The path input by the user
begin
  // Write prompt and get text
  WriteLn(sInstallPathPrompt);
  WriteLn(Path);
  ReadLn(InPath);
  if InPath <> '' then
    // Copy entered path into string
    CopyPStr(Path, InPath);
end;

procedure Install(var Extractor: TExtractor);
  {Main installation code - to be called from each install program project,
  which must provide an extractor object (which is freed and nilled within this
  routine}
var
  InstallFiles: TInstallerFiles;  // object with info about install files
  PInstallPath:
    array[0..MAX_PATH] of Char;   // pointer to installation path
  InstallPath: string;            // installation path
  I: Integer;
begin
  // Sign on
  WriteLn(sTitle);
  WriteLn(sCopyright);
  for I := 1 to Length(sCopyright) do
    Write('=');
  WriteLn;
  WriteLn;

  InstallFiles := TInstallerFiles.Create;
  try
    // Extract files to install directory using provided extractor
    try
      Extractor.Execute(InstallFiles);
    finally
      Extractor.Free;
      Extractor := nil;
    end;

    // Load the DLL
    LoadDLL(InstallFiles.InstallDLLFile);

    // Perform install using installation DLL
    // set callbacks
    SetMsgProc(@Display);
    SetQueryProc(@GetInstPath);
    // record any install path specified on command line
    CopyPStr(PInstallPath, ParamStr(1));
    // do the installation
    try
      if DoInstall(InstallFiles, PInstallPath) then
        WriteLn(sInstallSuccess)
      else
        WriteLn(sInstallFailed + ErrorStr);

    finally
      // Tidy up
      // free the install DLL
      FreeDLL;
      // delete intermediate files:
      // .. delete info file & DLL if not installing in install prog path
      InstallPath := TFileProcsLite.DirToPath(PInstallPath);
      if not EqualStr(InstallFiles.ExtractPath, InstallPath) then
      begin
        DeleteFile(PChar(InstallFiles.InstallDLLFile));
        DeleteFile(PChar(InstallFiles.InstallInfoFile));
      end;
      // .. always delete install data and inflater DLL files
      DeleteFile(PChar(InstallFiles.InstallDataFile));
      DeleteFile(PChar(InstallFiles.InflaterDLLFile));
    end;
  finally
    InstallFiles.Free;
  end;

  // Sign off
  WriteLn;
  Write(sPressReturn);
  ReadLn;
end;

initialization

// Seed randon number generator
Randomize;
// Set untrapped exception message routine
@UInstExcept.FatalErrorMessageProc := @Error;

finalization

// Clear untrapped error exception message routine
@UInstExcept.FatalErrorMessageProc := nil;

end.

