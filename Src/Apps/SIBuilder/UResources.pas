{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UResources.pas
  @COMMENTS                 Include file that provides resource identifiers for
                            the various resource files used by SIBuilder.
  @OTHER_NAMES              Changed name from ResIds.inc to UResources.pas at
                            v4.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 28/08/2000
      @COMMENTS             Almost all resource ids and many constant names
                            changed
                            + Deleted resource id for uninstall file - no longer
                              needed
                            + Renamed resource ids for project data file and
                              project info files
                            + Changed value of all ids to avoid confusion with
                              earlier versions
                            + Added new ids for info pane rich text resources
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             Major changes to number of resource ids and to
                            values given to them:
                            + Added new name constant for check boxes imagelist
                              bitmap resource.
                            + Added new name constant for logo bitmap.
                            + Added new id constant for about box info rich text
                              resource.
                            + Added new info box and warning rich text resources
                              for new pages on main interface.
                            + Added new resource for registry template dialog
                              help.
                            + Replaced all resource ids that were previously
                              used in project resource file linked into Delphi-
                              compiled installer with single resource id for the
                              single resource now used.
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 19/11/2003
      @COMMENTS             Deleted four constants relating to RCDATA resources
                            removed from Resources.rc.
    )
    @REVISION(
      @VERSION              3.2
      @DATE                 25/03/2007
      @COMMENTS             Removed resource id used for emedding payload in
                            Delphi-built installer's resource file.
    )
    @REVISION(
      @VERSION              4.0
      @DATE                 19/02/2008
      @COMMENTS             Converted from shared include file named ResIds.inc
                            to unit named UResources.pas in SIBuilder source
                            folder.
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
 * The Original Code is UResource.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UResources;


interface


const

  //
  // Resources SIBuilder
  //

  // info pane contents for various pages
  cWelcomeInfoResId     = 100;    // id of res for welcome page info pane rtf
  cDetailInfoResId      = 101;    // id of res for detail page info pane rtf
  cLicenseInfoResID     = 102;    // id of res for license page info pane
  cTargetOSInfoResID    = 103;    // id of res for target OS page info pane rtf
  cGroupsInfoResId      = 104;    // id of res for groups page info pane rtf
  cRunProgsInfoResId    = 105;    // id of res for run progs page info pane rtf
  cRegistryInfoResId    = 107;    // id of res for registry page info pane rtf
  cBuildInfoResId       = 108;    // id of res for build page info pane rtf

  // hot text pages: about box
  cAboutInfoResId       = 160;    // id of res for about box rtf

  // hot text error messages used by warner objects
  cPrgDlgProgErrResID   = 172;    // id of res for program edit dlg prog errors
  cPrgDlgParamErrResID  = 173;    // id of res for program edit dlg param errors
  cRunTabProgErrResID   = 174;    // id of res for run tab prog errors
  cGroupsTabErrResID    = 175;    // id of res for groups tab errors

  // bitmaps
  cSmallImages = 'SMALLIMAGE';    // name of res containing small imagelist
  cCheckBoxes = 'CHECKBOXES';     // name of res containing tree view checkboxes
  cLogoBmp = 'LOGO';              // name of res containing logo bitmap


implementation

end.
