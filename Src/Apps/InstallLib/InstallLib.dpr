{ ##
  @FILE                     InstallLib.dpr
  @COMMENTS                 Projetc file for DLL that performs file installation
                            on behalf of the installation program.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 04/09/2000
      @COMMENTS             A total rewrite from original - now includes an
                            uninstaller as well as the installer. Many units
                            removed from project and several new units added.
                            Calling application can now be asked to provide
                            information in addition to displaying output.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 06/01/2001
      @COMMENTS             Added new unit - IntfRegCOMServers - to permit
                            access to interface in new version of
                            RegCOMSvrs.dll.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 21/01/2001
      @COMMENTS             Added new unit - IntfSITLicenseDlg - to give access
                            to interfaces in SITLicenseDlg COM server.
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 28/06/2001
      @COMMENTS             Modifed so that the actuall installation path used
                            by installer is passed back to calling module.
    )
    @REVISION(
      @VERSION              2.4
      @DATE                 29/12/2002
      @COMMENTS             + Added new units: UOS, UInflater, UBinStrings,
                              URegUtils, UCOMSvrRegProcs.
                            + Removed IntfRegCOMSvrs unit since the RegCOMSvrs
                              DLL it references is not longer required.
                            + Added extra parameter to Install function to allow
                              inflater DLL file spec to be passed in.
                            + Changed to use separate installer and uninstaller
                              classes rather than one class for both purposes.
    )
    @REVISION(
      @VERSION              2.5
      @DATE                 16/01/2006
      @COMMENTS             + Added new UInstallerFiles and UFileProcsLite
                              units.
                            + Changed interface to exported Install routine to
                              take a TInstallerFiles object rather than specific
                              file names.
    )
    @REVISION(
      @VERSION              2.6
      @DATE                 19/02/2008
      @COMMENTS             Added UFileNames and URegistry units.
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
 * The Original Code is InstallLib.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


library InstallLib;

{
  This file is distributed within the self-extracting install programs. The
  installer extracts InstallLib.dll from within its data then calls into the
  DLL to perform the installation.

  The DLL is also used by the uninstall program.
}

uses
  SysUtils,
  Windows,
  UInstaller in 'UInstaller.pas',
  IntfSITLicenseDlg in '..\..\Intf\IntfSITLicenseDlg.pas',
  UInflater in '..\..\Shared\Install\UInflater.pas',
  UInstallerFiles in '..\..\Shared\Install\UInstallerFiles.pas',
  UCommonTypes in '..\..\Shared\Params\UCommonTypes.pas',
  UFileNames in '..\..\Shared\Params\UFileNames.pas',
  URegistry in '..\..\Shared\Params\URegistry.pas',
  UCOMSvrRegProcs in '..\..\Shared\Support\UCOMSvrRegProcs.pas',
  UOS in '..\..\Shared\Support\UOS.pas',
  UPathMacros in '..\..\Shared\Support\UPathMacros.pas',
  UFileProcs in '..\..\Shared\Utils\UFileProcs.pas',
  UFileProcsLite in '..\..\Shared\Utils\UFileProcsLite.pas',
  UBinStrings in '..\..\Shared\Utils\UBinStrings.pas',
  URegUtils in '..\..\Shared\Utils\URegUtils.pas',
  UStringProcs in '..\..\Shared\Utils\UStringProcs.pas',
  UCharStreamReader in '..\..\Shared\XML\UCharStreamReader.pas',
  UCharStreamWriter in '..\..\Shared\XML\UCharStreamWriter.pas',
  UTokeniser in '..\..\Shared\XML\UTokeniser.pas',
  UXMLFile in '..\..\Shared\XML\UXMLFile.pas',
  UXMLTag in '..\..\Shared\XML\UXMLTag.pas';

{$Resource VInstallLib.res} // version info

type
  TMsgProc = procedure(const Msg: string);    // message callback proc type
  TQueryProc = procedure(QueryStr: PChar);    // query callback proc type

var
  MsgProc: TMsgProc;      // stores pointer to message display callback routine
  QueryProc: TQueryProc;  // stores pointer to inst path query callback routine
  ErrMsg: string;         // description of last error

procedure SetMsgProc(TheMsgProc: Pointer);
  {Store a reference to the callback routine used to display messages in
  calling application}
begin
  MsgProc := TheMsgProc;
end;

procedure SetQueryProc(TheQueryProc: Pointer);
  {Store a reference to the callback routine used to query install path}
begin
  QueryProc := TheQueryProc;
end;

function ErrorStr: PChar;
  {Return pointer to the most recent error message}
begin
  Result := PChar(ErrMsg);
end;

procedure Output(const Msg: string);
  {Display given message in calling application using the provided callback
  routine}
begin
  if Assigned(MsgProc) then MsgProc(Msg);
end;

procedure QueryInstPath(var QueryStr: string);
  {Get given string from calling application using the provided callback
  routine}
var
  PathBuf: array[0..MAX_PATH] of Char;
begin
  StrPCopy(PathBuf, QueryStr);
  if Assigned(QueryProc) then QueryProc(PathBuf);
  QueryStr := PathBuf;
end;

function Install(const InstFiles: TInstallerFiles;
  PInstallPath: PChar): WordBool;
  {Perform the installation onto the given installation path per the install
  script file InstallXMLFile and files stored in InstallDataFile. Uses inflate
  DLL to inflate any compressed data. Return true on success and false on
  failure. The actual install path used (it may be changed by user) is passed
  back in PInstallPath, whose buffer should have size > MAX_PATH}
var
  Installer: TInstaller;      // installation object
  InstPath: string;           // the installation path
begin
  // Assume success
  Result := True;
  try
    // Create the installer
    Installer := TInstaller.Create;
    try
      // Message event handler - uses application provided callback
      Installer.OnMessage := Output;
      Installer.OnGetInstallPath := QueryInstPath;
      // Do the installation, passing back any changed path
      InstPath := PInstallPath;
      Installer.Install(InstFiles, InstPath);
      StrPLCopy(PInstallPath, InstPath, MAX_PATH);
    finally
      // Free the installer
      Installer.Free;
    end;
  except
    // Installer failed - record error message and return false
    on E: Exception do
    begin
      ErrMsg := E.Message;
      Result := False;
    end;
  end;
end;

function UnInstall(FileName: PChar): WordBool;
  {Uninstall the project using the given XML project file. Return true on
  success and false on failure}
var
  Uninstaller: TUninstaller;      // uninstaller object
begin
  // Assume success
  Result := True;
  try
    // Create the uninstaller object
    Uninstaller := TUninstaller.Create;
    try
      // Message event handler - uses application provided callback
      Uninstaller.OnMessage := Output;
      // Do the uninstall
      Uninstaller.UnInstall(FileName);
    finally
      // Free the uninstaller object
      Uninstaller.Free;
    end;
  except
    // Uninstall failed - record error message and return false
    on E: Exception do
    begin
      ErrMsg := E.Message;
      Result := False;
    end;
  end;
end;

exports
  Install,
  UnInstall,
  ErrorStr,
  SetMsgProc,
  SetQueryProc;

begin
end.
