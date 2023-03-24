{
 * UFileNames.pas
 *
 * Provides constants identifiers for various file, folder and file-extension
 * names. Used various SITools projects.
 *
 * Changed name from FileNames.inc to UFileNames.pas at v2.0
 *
 * v1.0 of 09 Mar 2000  - Original version.
 * v1.1 of 03 Sep 2000  - Changed default project template extension to .sit.
 *                      - Added constant for old style project templates (ext
 *                        .sid).
 *                      - Added constants for new and old style project file
 *                        extensions.
 *                      - Renamed uninstall info file from UnInstall.sib to
 *                        UnInstall.xml & changed name of constant.
 *                      - Added project file version control constants.
 * v1.2 of 20 Dec 2000  - Added constant for compiler log file name.
 * v1.3 of 29 Dec 2000  - Added new constant for DLL used to register &
 *                        unregister in-process COM server DLLs.
 *                      - Changed current SIBuilder file version to 203.
 * v1.4 of 06 Jan 2001  - Changed current version of install project file from
 *                        200 to 203: this is a bug fix - should have been done
 *                        for SITools release 2.3.
 * v1.5 of 21 Jan 2001  - Added constant for name of license dialog box COM
 *                        library.
 *                      - Updated project and install file version to 205.
 * v1.6 of 05 Aug 2002  - Updated project and install file versions to 206 from
 *                        205 for release 2.6.1 - this was not done when 2.6
 *                        update was released, but version 2.6 files may not be
 *                        backward compatible with earlier program releases.
 * v1.7 of 29 Dec 2002  - Added constant for Inflate.dll to list of files.
 *                      - Added constant for packaged installer stub file name.
 *                      - Added constant for batch file used to run uninstaller
 *                        from installer.
 *                      - Added constant for command line switch to make
 *                        uninstaller run without prompting user.
 *                      - Removed now unused constant for RegCOMSvrs.dll.
 *                      - Updated latest versions of project and install XML
 *                        files to 300.
 * v1.8 of 23 Feb 2003  - Updated project and info file format to 301 due to
 *                        uninstall format changes.
 *                      - Changed earliest install file version to 300 from 200:
 *                        should have been done in v1.7 for release of new
 *                        version.
 * v1.9 of 15 Jan 2006  - Changed project and install xml file versions to 302.
 * v1.10 of 25 Mar 2007 - Removed two constants that applied only to building
 *                        installer using Delphi.
 * v2.0 of 23 Feb 2008  - Changed name from FileNames.inc to UFileNames.pas and
 *                        converted from include file to unit.
 *                      - Added constants for license and help files.
 * v2.1 of 01 Mar 2008  - Renamed help file from SIBuilder.hlp to SIBuilder.chm.
 * v2.2 of 11 Apr 2008  - Updated project and install file versions to 303.
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
 * The Original Code is UFileNames.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UFileNames;


interface


const

  // Names of various project & application files etc
  // file names
  cDefInstallPrg = 'Install.exe';       // default name of install prog
  cInstallStub = 'PkgInstallStub.exe';  // name of packager install stub
  cInstallFolder = 'Install';           // name of subfolder where install built
  cInstallInfo = 'Install.xml';         // name of install info file
  cInstallData = 'Install.dat';         // name of install data file
  cInflatorLib = 'Inflate.dll';         // name of inflator dll as installed
  cUninstallPrg = 'UnInstall.exe';      // name of uninstall prog
  cUninstallInfo = 'UnInstall.xml';     // name of uninstall data file
  cInstallLib = 'InstallLib.dll';       // name of install library DLL
  cLicenseDlgLib = 'SITLicenseDlg.dll'; // name of dll that displays license
  cUninstExecBat = 'UninstallExec.bat'; // name of bat file that runs uninstall
  cHelpFile = 'SIBuilder.chm';          // name of SIBuilder help file
  cLicenseFile = 'Docs\SIBuilder.lic';  // name & sub-directory of license file
  // file extensions
  cProjExt = '.sip';                    // ext used for project files
  cOldProjExt = '.sib';                 // old ext used for project files
  cDefProjExt = '.sit';                 // ext used for default proj template
  cOldDefProjExt = '.sid';              // old ext used for default proj tplt
  cTempFileStub = 'SIB';                // "stub" used in temporary files
  // parameters and switches
  cNoUninstPrompt = '-NoPrompt';        // switch to stop unistaller prompts


  // SIBuilder files version information
  // old style binary file version information
  cWatermark = '<=* SIBuilder 01 *=>';
  // project xml file version information
  cPrjRootTag = 'sip';                  // root tag of project xml files
  cEarliestPrjVersion = 200;            // earliest proj file version that can
                                        // .. be read without conversion
  cCurrentPrjVersion = 303;             // current project file version
  // install xml file version information
  cInstRootTag = 'sii';                 // root tag of install xml files
  cEarliestInstVersion = 300;           // earliest install file version that
                                        // .. can be read without conversion
  cCurrentInstVersion = 303;            // current install file version


implementation

end.

