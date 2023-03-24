{
 * RegInprocSvr.dpr
 *
 * Main project file for comsole applet that creates and deletes shortcuts
 * (shell links).
 *
 * v1.0 of 24 Nov 2005  - Original version.
 * v1.1 of 11 Apr 2008  - Added new SResources.res resource file.
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
 * The Original Code is Shortcutter.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program Shortcutter;


{$APPTYPE CONSOLE}


{
  Usage
  =====

  To create shortcut
  ------------------

    MakeShortcut <Shortcut-File> <Associated-File>
      <Working-Directory> <Description> <Icon-File> <Icon-Index>
      <Argument-1> <Argument-2> .... <Argument-n>

    All command should appear on same line where
      <Shortcut-File>
        Name of new shortcut file. Required Must have .lnk extension.
      <Associated-File>
        Name of file that shortcut links to. Required. Must exist.
      <Working-Directory>
        Working directory of any program started by shortcut. Optional. If there
        are other parameters skip this one by spwecifying *.
      <Description>
        Description of shortcut. Optional. To skip specify *.
      <Icon-File>
        File from which shortcut's icon is taken. Optional if no icon
        required. To skip specify *.
      <Icon-Index>
        Index of required icon in <Icon-File>. Optional. To skip specify *. When
        skipped or if not valid +ve number then 0 is used.
      <Argument-1> <Argument-2> .. <Argument-n>
        Zero or more parameters to pass to program started by shortcut.
        Optional. Omit if not required (do not use *).
    Notes
      1) Any parametes containing spaces should be enclosed in double quotes.
      2) * (asterisk) may be specified for any optional parameter other than
         <Argument-x> to indicate that no value is being specified.


  To delete shortcut
  ------------------

    MakeShortcut <Shortcut-File>

    <Shortcut-File> is name of file to be delected. There must be no other
    parameters.

    Note that this option checks if <Shortcut-File> is a valid
    shortcut file and only deletes it if so.
}


uses
  // Delphi
  Windows,
  // Project
  USCParams in 'USCParams.pas',
  UShellLink in 'UShellLink.pas';


{$Resource VShortcutter.res}  // version information
{$Resource SResources.res}    // other resources


begin
  // Main program code
  if TSCParams.WantCreate then
  begin
    // User wants to create shortcut
    if TShellLink.CreateShellLink(
      TSCParams.ShortcutFile,
      TSCParams.AssocFile,
      TSCParams.Desc,
      TSCParams.WorkDir,
      TSCParams.Args,
      TSCParams.IconFile,
      TSCParams.IconIndex
    ) then
      ExitCode := 0
    else
      ExitCode := 1;
  end
  else
  begin
    // User wants to delete shortcut
    if TShellLink.IsShellLink(TSCParams.ShortcutFile) then
      DeleteFile(PChar(TSCParams.ShortcutFile));
  end;
end.

