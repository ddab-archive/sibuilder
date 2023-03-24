{
 *  UnInstall.dpr
 *
 * A console application that un-installs applications installed by the SI Tools
 * installer.
 *
 * Requires InstallLib.dll.
 *
 * v1.0 of 09 Mar 2000  - Original version.
 * v2.0 of 04 Sep 2000  - Total rewrite. Now calls into InstallLib.dll to
 *                        perform uninstall. Uninstall code is no longer
 *                        physically in this program. No other units are now
 *                        included in the project. The size of the resulting is
 *                        therefore significantly reduced.
 * v2.1 of 15 Sep 2001  - Fixed bug which caused UnInstall.exe to fail under
 *                        Windows 2000: functions in external DLL were being
 *                        referred to without specifying .dll extension.
 *                      - No uses file name from FileNames.inc.
 * v3.0 of 29 Dec 2002  - Major re-write
 *                      - Now uses constant for Uninstall.xml rather than hard
 *                        coded value.
 *                      - Changed sign on screen text for updated program.
 *                      - Added UInstProcs unit and used it for its FilePath
 *                        function. Function of same name removed from this
 *                        file.
 *                      - Added exit code to show if result was successful or
 *                        not.
 *                      - Added support for switch sent if being run from
 *                        installer: which supresses sign-on output, success or
 *                        error messages and prompt to user to press enter when
 *                        finished. This enables the program to be called from
 *                        an installer that needs old installation to be removed
 *                        without cluttering up the installer's display or
 *                        pausing it with exit prompt.
 * v3.1 of 23 Feb 2008  - Added UFileNames unit.
 *                      - Deleted inclusion of FileNames.inc: now replaced by
 *                        UFileNames unit.
 *                      - Moved program messages from string literals to
 *                        resource strings.
 * v3.2 of 11 Apr 2008  - Added USResource.res resource file containing Vista
 *                        manifest.
 *
 *
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
 * The Original Code is UnInstall.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program UnInstall;


uses
  // Delphi
  Windows,
  // Project
  UInstProcs in '..\..\Shared\Install\UInstProcs.pas',
  UFileNames in '..\..\Shared\Params\UFileNames.pas';


{$APPTYPE CONSOLE}

{$Resource VUnInstall.res}    // version information
{$Resource UIResources.res}   // other resources


resourcestring
  // Program messages
  sTitle = 'DelphiDabbler SITools 3 - Uninstaller';
  sCopyright = 'Copyright (c) P.D.Johnson, 2000-2002';
  sPressReturn = 'Press return to exit';
  sFailure = 'Uninstall failed: ';
  sSuccess = 'Success';


// Routines imported from InstallLib.dll
function DoUnInstall(InstFileName: string): WordBool;
  external cInstallLib name 'UnInstall';
  {Performs actual uninstallation}
procedure SetMsgProc(TheMsgProc: Pointer);
  external cInstallLib;
  {Sets the install library callback method by which messages are displayed}
function ErrorStr: PChar;
  external cInstallLib;
  {Description of last error that occured in install library}


procedure Display(const Msg: string);
  {Displays the given message on the console - passed as a callback routine to
  InstallLib.dll}
begin
  WriteLn(Msg);
end;

var
  RunFromInstaller: Boolean;
    {Notes whether the program is being run from an installer: sign on, error &
    success messages and prompt to close are inhibited when this variable is
    true. It is set when a specific switch is provided}

var
  I: Integer; // displays required number of underline characters in for title
begin
  // Check if we're run from installer
  RunFromInstaller := (ParamStr(1) = cNoUninstPrompt);
  // Sign on
  if not RunFromInstaller then
  begin
    WriteLn(sTitle);
    WriteLn(sCopyright);
    for I := 1 to Length(sTitle) do
      Write('=');
    WriteLn;
    WriteLn;
  end;
  // Perform uninstall using InstallLbl.dll
  SetMsgProc(@Display);
  if DoUnInstall(FilePath(ParamStr(0)) + cUninstallInfo) then
  begin
    // Everything was OK
    if not RunFromInstaller then
      WriteLn(sSuccess);
    ExitCode := 0;
  end
  else
  begin
    // Installation failed
    if not RunFromInstaller then
      WriteLn(sFailure + ErrorStr);
    ExitCode := 1;
  end;
  // Sign off
  if not RunFromInstaller then
  begin
    WriteLn;
    Write(sPressReturn);
    ReadLn;
  end;
end.

