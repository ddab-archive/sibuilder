{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegistry.pas
  @COMMENTS                 Provides constant identifiers for the names of
                            various registry keys and values accessed by SI
                            Tools applications.
  @OTHER_NAMES              Changed name from Registry.inc to URegistry.pas at
                            v2.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 04/09/2000
      @COMMENTS             + Added root HKey value = HKEY_CURRENT_USER
                            + Added CSIBxxx registry key names for all new
                              settings
                            + Added constants for default values for all
                              settings
                            + Changed SIBuilder version registry key to 2.0
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 20/12/2000
      @COMMENTS             + Added new constant for HKEY_LOCAL_MACHINE registry
                              key where where all Delphi versions are listed.
                            + Renamed constant for HKEY_LOCAL_MACHINE registry
                              key for specific versions of Delphi.
                            + Removed constant for default Delphi version --
                              TDCC32Info now determines this for user's system.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 27/02/2002
      @COMMENTS             + Changed root SIBuilder key from 2.0 to 3.0.
                            + Added new constants for SharedDLLs and App Paths
                              keys under MS Windows Current version.
                            + Added new constant for Shell Folders regsitry key.
                            + Added registry value name and default for report
                              kind setting.
                            + Added registry value name and default for selected
                              compiler.
                            + Added registry value name and default for default
                              compressor.
                            + Added registry value name and default for
                              InfoWindows value plus constants for elements of
                              the bitmask.
                            + Deleted registry value name and default for now
                              unused DelphiVersion, CompilerDisplay and
                              ShowSideBar options.
                            + Changed entry for default info pane colour from
                              cream to system window colour.
                            + Added keys for Compression, CoreComponents, Shared
                              and Extras program/DLL entries.
                            + Added keys for registry templates
    )
    @REVISION(
      @VERSION              1.4
      @DATE                 25/03/2007
      @COMMENTS             + Removed registry info used to persist deleted
                              TSettings.Compiler property.
                            + Removed Delphi related registry entries.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 23/02/2008
      @COMMENTS             + Converted from include file named Registry.inc to
                              unit named URegistry.pas.
                            + Removed unused constants.
                            + Removed constant storing HKEY_CURRENT_USER.
                            + Changed SIBuilder base key to contain
                              "DelphiDabbler" rather than "delphiDabbler".
                            + Changed some sub key constants to include
                              SIBuilder root key and renamed some keys.
                            + Added new key to store new location of user
                              options.
                            + Corrected default info pane colour.
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
 * The Original Code is URegistry.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit URegistry;


interface

const


  //
  // Names and default values of SITools registry entries
  //

  // SIBuilder root key in HKCU and HKLM
  cSIBRootKey = '\Software\DelphiDabbler\SITools\3.0\SIBuilder';

  // Subkey of SIBuilder's key for storing window settings
  cSIBWindowKey = cSIBRootKey + '\Window';

  // Subkey where user options are stored
  cSIBOptionsKey = cSIBRootKey + '\Options';

  // SITools program information
  // Subkey under which all SITools program components are stored
  cSIBComponentsKey = cSIBRootKey + '\Components';
  // Subkey where details of all registered compression libraries are stored
  cSIBCompressionKey = cSIBComponentsKey + '\Compression';
  // Subkey where SITools core component programs are listed
  cSIBCoreComponentsKey = cSIBComponentsKey + '\Core';
  // Subkey under which SITools extra programs are listed
  cSIBExtrasKey = cSIBComponentsKey + '\Extras';

  // Registry template information
  // Key where registry template information is stored
  cSIBRegistryKey = cSIBRootKey + '\Registry';
  // Key under which the registry templates are listed
  cSIBRegTpltKey = cSIBRegistryKey + '\Templates';
  // Key under which macros used in templates are listed
  cSIBRegVarsKey = cSIBRegistryKey + '\Macros';

  // General options stored under main SIBuilder key
  // The ShowWelcome value id and default value
  cSIBShowWelcome = 'ShowWelcome';
  cSIBShowWelcomeDef = True;
  // The DefaultCompresser value id and default value
  cSIBDefaultCompressor = 'DefCompressor';
  cSIBDefaultCompressorDef = 'ZLib';
  // The ShowInfoPane value id and default value
  cSIBShowInfoPane = 'ShowInfoPane';
  cSIBShowInfoPaneDef = True;
  // The InfoPaneColour value id and default value
  cSIBInfoPaneColour = 'InfoPaneColour';
  cSIBInfoPaneColourDef = Integer($FF000000 or $00000005);  // sys window colour
  // The ReportKind value id and default value
  cSIBReportKind = 'ReportKind';
  cSIBReportKindDef = 0;
  // The InfoWindows value id, default value and bitmask constants
  cSIBInfoWindows = 'InfoWindows';
  cSIBGroupInfoWdw = $00000001;
  cSIBFileInfoWdw = $00000002;
  cSIBRegKeyInfoWdw = $00000004;
  cSIBRegDataInfoWdw = $00000008;
  cSIBInfoWindowsDef = cSIBGroupInfoWdw or cSIBFileInfoWdw or cSIBRegKeyInfoWdw
    or cSIBRegDataInfoWdw;


  //
  // Windows registry keys
  //

  // Base for Windows reg keys
  cWdwCurrentVer = '\Software\Microsoft\Windows\CurrentVersion';

  // Info about how to uninstall program
  // Uninstall key
  cWdwUnInstall = cWdwCurrentVer + '\Uninstall';
  // Where the name of the program is stored
  cWdwUnInstallDisplayName = 'DisplayName';
  // Where the path to the uninstall program is stored
  cWdwUnInstallUninstallString = 'UninstallString';

  // Where app paths are stored
  cWdwAppPath = cWdwCurrentVer + '\App Paths';
  // Where shared DLLs are recorded
  cWdwSharedDLL = cWdwCurrentVer + '\SharedDLLs';

  // Shell folders access
  cWdwShellFolders = cWdwCurrentVer + '\Explorer\Shell Folders';


implementation

end.

