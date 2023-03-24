{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UWelcomeTabHandler.pas
  @COMMENTS                 Implements a TTabSheetHandler descendant class that
                            displays a brief description of the program and
                            provides access via hot links to various help
                            topics, common starting functions (e.g. new project,
                            open project) and the delphiDabbler website.
  @OTHER_NAMES              + Original unit name was FmWelcomeDlg.pas: this unit
                              implemented a dialog box used to display welcome
                              information.
                            + Changed to UWelcomeTabHandler.pas at v3.0, and
                              changed to be a tab sheet handler for the welcome
                              page of the main program.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 28/08/2000
      @COMMENTS             Substantial re-write. The rich text control that
                            displays the welcome message can now have hot-links
                            embedded in it. These links lead to certain actions
                            being triggered. An event is triggered to get the
                            actions performed by the calling unit. Since actions
                            are now embedded in the welcome message, the website
                            hot-label was removed and replaced by an embedded
                            action. A help button, accessing the dialog's help
                            topic, was added to the control (previously only F1
                            accessed this).
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 15/09/2001
      @COMMENTS             Changed wizard page hotspots from WingDings
                            characters to bracketed Arial numerals since
                            WingDings characters were not being detected by
                            Windows 2000.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             Totally re-written as a tab sheet handler for the
                            new tab sheet page in the main window that replaces
                            the old welcome dialog box. Much of the
                            functionality from the dialog box's methods and
                            event handlers was moved to similar methods in the
                            tab handler, but was significantly updated. Some
                            changes were:
                            + Hot link actions are now handled by command
                              dispatcher class and hot linking is handled by a
                              new class that can decode rich text with embedded
                              commands.
                            + There is no longer a check box controlling display
                              of page": visibility of welcome tab now controlled
                              only from options dialog box.
                            + Resizing code was no longer required and was
                              removed. ) )
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 19/11/2003
      @COMMENTS             + Replaced Active RTF handler for welcome page with
                              support for hot text control that replaces it. The
                              hot text is now set as a property of the hot text
                              control and is not loaded dynamically from
                              resources.
                            + Removed UpdateSheet method since no longer needed.
                            + Removed reference to removed UActiveRTFWrapper
                              unit.
    )
    @REVISION(
      @VERSION              3.2
      @DATE                 19/02/2008
      @COMMENTS             Replaced usage of ResIds.inc include file with
                            UResources unit.
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
 * The Original Code is UWelcomeTabHandler.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UWelcomeTabHandler;


interface


uses
  // Delphi
  Windows, ComCtrls,
  // Project
  UTabSheetHandler;

type

  {
  TWelcomeTabHandler:
    Encapsulates the "Welcome" tab sheet and provides the relevant specialised
    processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TWelcomeTabHandler -> TTabSheetHandler -> TObjListItem
      -> [TObject]
  }
  TWelcomeTabHandler = class(TTabSheetHandler)
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: provides an event handler to handle hot link clicks in
      the hot text control that displays the welcome page}
  end;


implementation


uses
  // Delphi
  Classes, Graphics,
  // Project
  UCmdDispatcher, UResources, FmMain;


{ TWelcomeTabHandler }

constructor TWelcomeTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: provides an event handler to handle hot link clicks in
  the hot text control that displays the welcome page}
begin
  inherited;
  // Set main form's welcome hot text control's click event handler
  MainForm.htWelcome.OnLinkClick := TCmdDispatcher.HotLinkClickHandler;
end;

function TWelcomeTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cWelcomeInfoResId;
end;

end.
