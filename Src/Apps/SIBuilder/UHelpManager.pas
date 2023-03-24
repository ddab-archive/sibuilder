{
 * UHelpManager.pas
 *
 * Implements static class that manages the SIBuilder HTML Help system.
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 25 Nov 2005  - Changed to use THelpManager to handle help display
 *                        rather than rely on Delphi's built in processing.
 * v1.2 of 17 Jul 2006  - Fixed bug where help window was not been closed by
 *                        THelpManager.Quit in some circumstances. Did this by
 *                        always using same window handle in WinHelp calls.
 *                      - Fixed bug where help calls that went through Delphi
 *                        were not being handled. Now automatically call
 *                        DoCommand when Delphi notifies a help call.
 * v1.3 of 23 Feb 2008  - Replaced local constant storing help file name with
 *                        use of constant from UFileNames.
 * v2.0 of 01 Mar 2008  - Converted to use HTML help instead of WinHelp,
 *                        Maintained same outward facing API so that calling
 *                        code need not be changed. Automatically converts
 *                        between WinHelp commands and HTML Help commands.
 *                        However, popup help windows are no longer supported
 *                        and some functions were converted to procedures.
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
 * The Original Code is UHelpManager.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHelpManager;


interface


uses
  // Delphi
  SysUtils, Windows;


type

  {
  THelpManager:
    Static class that manages the SIBuilder HTML help system.
  }
  THelpManager = class(TObject)
  private
    class function DoAppHelp(const Command: LongWord;
      const HelpPage: string; const Data: LongWord): HWND;
      {Calls HtmlHelp API with specified command and parameters. Command is
      command to send to HTML Help. HelpPage optionally names an HTML file
      containing a required page within help file. HelpPage should be '' if no
      specific page is required. Data is the command dependent data to pass to
      HTML Help}
  public
    class function PreventAppHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
      {Event handler for TApplication.OnHelp event that ensures help is handled
      by this help manager rather Delphi's built in manager}
    class procedure Contents; 
      {Displays help contents}
    class procedure ShowTopic(Topic: DWORD);
      {Displays a given help topic}
    class procedure Quit;
      {Closes down the help system}
    class function DoCommand(Cmd: UINT; Data: DWORD = 0): Boolean;
      {Executes a given help command with optional data. Assumes help file is in
      same directory as executable file}
  end;

  {
  EHelpManager:
    Exceptions raised by THelpManager.
  }
  EHelpManager = class(Exception);


implementation


uses
  // Project
  UFileNames, UHTMLHelp;


var
  pvtCookie: DWORD = 0; // Cookie identifying HTML help instance


{ THelpManager }

resourcestring
  // Error message
  sCmdErr = 'Unrecognised or unsupported help command #%d';

class procedure THelpManager.Contents;
  {Displays help contents}
begin
  DoCommand(HELP_FINDER);
end;

class function THelpManager.DoAppHelp(const Command: LongWord;
  const HelpPage: string; const Data: LongWord): HWND;
  {Calls HtmlHelp API with specified command and parameters. Command is command
  to send to HTML Help. HelpPage optionally names an HTML file containing a
  required page within help file. HelpPage should be '' if no specific page is
  required. Data is the command dependent data to pass to HTML Help}
var
  HelpSpec: string; // Help file followed by any required page
begin
  HelpSpec := ExtractFilePath(ParamStr(0)) + cHelpFile;
  if HelpPage <> '' then
    HelpSpec := HelpSpec + '::/' + HelpPage;
  Result := HtmlHelp(GetDesktopWindow(), PChar(HelpSpec), Command, Data);
end;

class function THelpManager.DoCommand(Cmd: UINT; Data: DWORD): Boolean;
  {Executes a given help command with optional data. Assumes help file is in
  same directory as executable file}
begin
  case Cmd of
    HELP_FINDER:
      Result := DoAppHelp(HH_DISPLAY_TOC, '', 0) <> 0;
    HELP_CONTEXT:
      Result := DoAppHelp(HH_HELP_CONTEXT, '', Data) <> 0;
    HELP_QUIT:
    begin
      HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
      Result := True;
    end;
    else
      raise EHelpManager.CreateFmt(sCmdErr, [Cmd]);
  end;
end;

class function THelpManager.PreventAppHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
  {Event handler for TApplication.OnHelp event that ensures help is handled by
  this help manager rather Delphi's built in manager}
begin
  Result := True;
  CallHelp := False;
  DoCommand(Command, Data);
end;

class procedure THelpManager.Quit;
  {Closes down the help system}
begin
  // Do nothing
  DoCommand(HELP_QUIT);
end;

class procedure THelpManager.ShowTopic(Topic: DWORD);
  {Displays a given help topic}
begin
  DoCommand(HELP_CONTEXT, Topic);
end;


initialization

// Initialise HTML help and store cookie value
HtmlHelp(0, nil, HH_INITIALIZE, Cardinal(@pvtCookie));

finalization

// Uninitialize HTML help using cookie
HtmlHelp(0, nil, HH_UNINITIALIZE, pvtCookie);

end.

