{ ##                     
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UCommonTypes.pas
  @COMMENTS                 This unit is shared among two or more SITools
                            applications. It provides type definitions for
                            various enumerated types used by more than one unit.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/08/2000
      @COMMENTS             Original version - decalred various types relating
                            to how files anddirectories are installed and
                            uninstalled.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 28/12/2000
      @COMMENTS             Added types for controlling when external programs
                            are run.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 21/01/2001
      @COMMENTS             Added new type for determining what happens to the
                            license dialog box COM object after use by
                            installer.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 29/12/2002
      @COMMENTS             + Added foaIfLaterVer option to
                              TFileOverwriteActions: this option lets program
                              take account of file version info.
                            + Added new TFileKind enumeration: contains various
                              file kinds: exe, dll, batch, data etc. Also
                              added TFileKinds - a set of file kinds.
                            + Added new TRegKeyDeletionActions: gives various
                              registry key deletion options on uninstallation.
                            + Added fdTemporary option to TFileDeletionActions.
                            + Removed TLicenseDLLOptions enumerated type.
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
 * The Original Code is UCommonTypes.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UCommonTypes;


interface

type

  {
  TRegKeyDeletionActions:
    Possible ways of handling registry keys on un-installation.
  }
  TRegKeyDeletionActions = (
    rdNone,         // do not delete key
    rdAlways,       // always delete key
    rdIfNoSubKeys   // delete key only if it has no sub keys
  );

  {
  TFileDeletionActions:
    Possible ways of handling a group's files on un-installation.
  }
  TFileDeletionActions = (
    fdNone,         // do not delete files
    fdInstalled,    // delete only those files that were installed in directory
    fdAll,          // delete all files in the directory
    fdTemporary     // installer deletes before closing: used as/by programs run
  );

  {
  TDirDeletionActions:
    Possible ways of dealing a group's directory on un-installation.
  }
  TDirDeletionActions = (
    ddNone,         // do not delete directory
    ddIfEmpty,      // delete the directory only if it is empty
    ddAll           // forcibly delete the directory and all its contents
  );

  {
  TFileOverwriteActions:
    Ways in which to determine whether and how to overwrites pre-existing files.
  }
  TFileOverwriteActions = (
    foaAlways,      // always overwrite pre-existing files
    foaNever,       // never overwrite pre-existing files
    foaIfOlder,     // only overwrite older files
    foaIfLaterVer   // only overwrite files with later file versions
  );

  {
  TFileKind:
    The kinds of files recognised.
  }
  TFileKind = (
    fkUnknown,      // unknown file kind: per pre-v3 files
    fkError,        // error file kind: used for files that don't exist
    fkData,         // data files: non-executable
    fkBatch,        // batch files: *.bat extension
    fkDOS,          // DOS executable
    fkExe32,        // 32 bit executable
    fkExe16,        // 16 bit executable
    fkDLL32,        // 32 bit DLL
    fkDLL16,        // 16 bit DLL
    fkCOMDLL        // 32 bit COM server DLL (that supports registration)
  );

  {
  TFileKinds:
    Set of file kinds.
  }
  TFileKinds = set of TFileKind;

  {
  TInstallPathEditKinds:
    The kinds of editing of install path that are permitted on installation.
  }
  TInstallPathEditKinds = (
    ipeNone,        // editing of install path not permitted
    ipeCommandLine, // editing of install path via installer command line
    ipeQueryUser    // prompt user to change install path
  );

  {
  TRunSchedule:
    Determines when a program should be run by installer.
  }
  TRunSchedule = (
    rsdOnInstall,   // run program from installer, after files installed
    rsdOnUninstall, // run program from uninstaller, before files removed
    rsdBoth         // combine both the above
  );

implementation

end.
