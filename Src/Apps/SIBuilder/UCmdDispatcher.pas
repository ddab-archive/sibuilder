{
 * UCmdDispatcher.pas
 *
 * Implements a static class that dispatches command strings or command IDs
 * generated from hot-links in hot text controls (or other sources). Used by
 * SIBuilder.
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 19 Nov 2003  - Replaced HotLinkAction method that handled action
 *                        triggered by tagged rich text objects with
 *                        HotLinkClickHandler method for use with replacement
 *                        hot text controls.
 * v1.2 of 25 Nov 2005  - Changed to use THelpManager to handle help display
 *                        rather than rely on Delphi's built in processing.
 * v1.3 of 20 Feb 2008  - Replaced usage of Help.inc include file with
 *                        UHelpContexts unit.
 * v1.4 of 01 Mar 2008  - Adapted to work with renamed help context constants.
 *                      - Removed popup help topic commands.
 *                      - Moved interface constants to implemenation.
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
 * The Original Code is UCmdDispatcher.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCmdDispatcher;


interface


uses
  // Delphi
  Windows,
  // Project
  UHelpContexts;


type

  {
  TCmdDispatcher:
    Static class that dispatches command strings or command IDs generated from
    hot-links in hot text controls (or other sources). Commands relating to help
    topics causes the appropriate help system/topic to be displayed while other
    commands are dispatched by emulating user input on the main form.
  }
  TCmdDispatcher = class(TObject)
  public
    class procedure HotLinkClickHandler(Sender: TObject; const Cmd: string;
      const Rect: TRect);
      {Handles hot link click events by acting on the command: the rectangle is
      ignored. This method is provided for use by classes that need to pass hot
      link action event commands directly to the dispatcher}
    class procedure DispatchCmd(const Cmd: string); overload;
      {Dispatches command based on its command string}
    class procedure DispatchCmd(const Cmd: LongWord); overload;
      {Dispatches a command based on its numeric id}
  end;


implementation


uses
  // Delphi
  SysUtils, Forms,
  // Project
  FmMain, UHelpManager;

const
  // Help commands (Hi word is non-zero): called directly by dispatcher
  // help topic masks:
  cHelpTopic          = $F0000000;    // help topics have high word $F000
  cHelpContents       = $F2000000;    // help contents
  // actual help topics and popup help topics
  cHelpCopyright      = cHelpTopic or IDH_COPYRIGHT;
  cHelpCredits        = cHelpTopic or IDH_CREDIT;
  cHelpOutProcCOMSvrs = cHelpTopic or IDH_COMSERVERAPPS;
  cHelpRegTplt        = cHelpTopic or IDH_REGMACROS;
  cHelpAuthor         = cHelpTopic or IDH_AUTHOR;
  cHelpLicDlgTest     = cHelpTopic or IDH_LICENSETEST;
  cHelpGroupsTabErr   = cHelpTopic or IDH_ERR_FILES;
  cHelpRunTabProgErr  = cHelpTopic or IDH_ERR_RUNPROGS;
  cHelpPrgDlgProgErr  = cHelpTopic or IDH_ERR_PROGRAM;
  cHelpPrgDlgParamErr = cHelpTopic or IDH_ERR_PARAMS;
  cHelpAboutLic       = cHelpTopic or IDH_LICENSEFILE;

  // Other command ids that operate on main form
  cGotoWebsite        = $00000001;
  cBuildReport        = $00000002;
  cViewBuildReport    = $00000003;
  cBuildProj          = $00000004;
  cOpenProj           = $00000005;
  cNewBlankProj       = $00000006;
  cNewDefProj         = $00000007;
  cAboutBox           = $00000008;
  cNextPage           = $00000009;
  cAllOSs             = $0000000A;
  cRegTemplates       = $0000000B;
  cRegImport          = $0000000C;

  // Lookup table mapping command strings to command IDs
  cCmds: array[1..24] of record
    Str: string;    // command string
    ID: LongWord;   // command ID
  end =
  (
    (Str: 'GotoWebsite';        ID: cGotoWebsite),
    (Str: 'HelpCopyright';      ID: cHelpCopyright),
    (Str: 'HelpCredits';        ID: cHelpCredits),
    (Str: 'BuildReport';        ID: cBuildReport),
    (Str: 'ViewBuildReport';    ID: cViewBuildReport),
    (Str: 'BuildProj';          ID: cBuildProj),
    (Str: 'OpenProj';           ID: cOpenProj),
    (Str: 'NewBlankProj';       ID: cNewBlankProj),
    (Str: 'NewDefaultProj';     ID: cNewDefProj),
    (Str: 'HelpOutProcCOMSvrs'; ID: cHelpOutProcCOMSvrs),
    (Str: 'AboutBox';           ID: cAboutBox),
    (Str: 'HelpContents';       ID: cHelpContents),
    (Str: 'NextPage';           ID: cNextPage),
    (Str: 'HelpRegTplt';        ID: cHelpRegTplt),
    (Str: 'AllOSs';             ID: cAllOSs),
    (Str: 'btnRegTemplates';    ID: cRegTemplates),
    (Str: 'btnRegImport';       ID: cRegImport),
    (Str: 'HelpAuthor';         ID: cHelpAuthor),
    (Str: 'HelpLicDlgTest';     ID: cHelpLicDlgTest),
    (Str: 'HelpPrgDlgProgErr';  ID: cHelpPrgDlgProgErr),
    (Str: 'HelpPrgDlgParamErr'; ID: cHelpPrgDlgParamErr),
    (Str: 'HelpRunTabProgErr';  ID: cHelpRunTabProgErr),
    (Str: 'HelpAboutLic';       ID: cHelpAboutLic),
    (Str: 'HelpGroupsTabErr';   ID: cHelpGroupsTabErr)
  );


{ TCmdDispatcher }

class procedure TCmdDispatcher.DispatchCmd(const Cmd: LongWord);
  {Dispatches a command based on its numeric id}
var
  CmdLo: LongWord;  // contains only value of low word of command
  CmdHi: LongWord;  // contains only value of high word of command
begin
  // Record high and low words of command, with other part masked out
  CmdLo := $0000FFFF and Cmd;
  CmdHi := $FFFF0000 and Cmd;
  // Decide if help command or other command
  if CmdHi <> 0 then
  begin
    // This is help command: access required help topic, popup or contents
    case CmdHi of
      cHelpTopic:       THelpManager.ShowTopic(CmdLo);
      cHelpContents:    THelpManager.Contents;
    end;
  end
  else
  begin
    // This is not a help command: emulate command by manipulating relevant
    // controls on main form
    case CmdLo of
      cGotoWebsite:       MainForm.miWebsite.Click;
      cBuildReport:       MainForm.btnReport.Click;
      cViewBuildReport:   MainForm.btnViewReport.Click;
      cBuildProj:         MainForm.btnBuild.Click;
      cOpenProj:          MainForm.miOpen.Click;
      cNewBlankProj:      MainForm.miNewBlank.Click;
      cNewDefProj:        MainForm.miNewDefault.Click;
      cAboutBox:          MainForm.miAbout.Click;
      cNextPage:          MainForm.btnNext.Click;
      cAllOSs:            MainForm.btnAllOSs.Click;
      cRegTemplates:      MainForm.btnRegTemplates.Click;
      cRegImport:         MainForm.btnRegImport.Click;
    end;
  end;
end;

class procedure TCmdDispatcher.DispatchCmd(const Cmd: string);
  {Dispatches command based on its command string}
var
  Idx: Integer;     // loops thru command table
  CmdID: LongWord;  // command id found
begin
  // Lookup command in table: (command of 0 => not found - a programming error)
  CmdID := 0;
  for Idx := Low(cCmds) to High(cCmds) do
  begin
    if AnsiCompareText(Cmd, cCmds[Idx].Str) = 0 then
    begin
      CmdID := cCmds[Idx].ID;
      Break;
    end;
  end;
  Assert(CmdID <> 0);
  // Now dispatch the command using ID
  DispatchCmd(CmdID);
end;

class procedure TCmdDispatcher.HotLinkClickHandler(Sender: TObject;
  const Cmd: string; const Rect: TRect);
  {Handles hot link click events by acting on the command: the rectangle is
  ignored. This method is provided for use by classes that need to pass hot link
  action event commands directly to the dispatcher}
begin
  DispatchCmd(Cmd);
end;

end.

