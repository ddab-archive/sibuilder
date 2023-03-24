{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstallerFiles.pas
  @COMMENTS                 Implements class that creates and deletes unique sub
                            folder of Windows temp folder used for extracted
                            installation files. Provides fully specified file
                            names of the files that are extracted into the
                            folder.
  @DEPENDENCIES             None.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 16/01/2006
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/03/2007
      @COMMENTS             Removed conditional compilation of FileNames.inc
                            include statement now that unit is never compiled
                            from SIBuilder.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of FileNames.inc include file with
                              UFileNames unit.
                            + Replaced string literal used for temp file name
                              stub with constant from UFileNames.
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
 * The Original Code is UInstallerFiles.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UInstallerFiles;


interface


type

  {
  TInstallerFiles:
    Creates and deletes unique sub folder of Windows temp folder used for
    extracted installation files. Provides fully specified file names of the
    files that are extracted into the folder.
  }
  TInstallerFiles = class(TObject)
  private
    fExtractDir: string;
      {Directory where files are to be extracted}
    function GetExtractDir: string;
      {Returns name of a unique directory within the Windows temp folder to be
      used to extract files into}
    function GetExtractPath: string;
      {Returns path to folder where files are to be extracted}
    function GetInflaterDLLFile: string;
      {Returns fully specified file name of inflator DLL}
    function GetInstallDataFile: string;
      {Returns fully specified file name of data file that contains all file to
      be installed}
    function GetInstallDLLFile: string;
      {Returns fully specified file name of install DLL}
    function GetInstallInfoFile: string;
      {Returns fully specified file name of install information file}
  public
    constructor Create;
      {Class constructor. Sets up object with unique directory where files are
      to be extracted}
    destructor Destroy; override;
      {Class destructor: removes unique directory}
    property ExtractPath: string read GetExtractPath;
      {Path into which files are to be extracted}
    property InstallDLLFile: string read GetInstallDLLFile;
      {Fully specified name of extracted installation library}
    property InstallInfoFile: string read GetInstallInfoFile;
      {Fully specified name of extracted install information file}
    property InstallDataFile: string read GetInstallDataFile;
      {Fully specified name of extracted data file that contains all files to be
      installed}
    property InflaterDLLFile: string read GetInflaterDLLFile;
      {Fully specified name of extracted inflater DLL}
  end;


implementation


uses
  // Project
  UFileNames, UFileProcsLite;


{ TInstallerFiles }

constructor TInstallerFiles.Create;
  {Class constructor. Sets up object with unique directory where files are to
  be extracted}
begin
  inherited Create;
  fExtractDir := GetExtractDir;
  TFileProcsLite.EnsureFolders(fExtractDir);
end;

destructor TInstallerFiles.Destroy;
  {Class destructor: removes unique directory}
begin
  TFileProcsLite.RemoveDir(fExtractDir);
  inherited;
end;

function TInstallerFiles.GetExtractDir: string;
  {Returns name of a unique directory within the Windows temp folder to be used
  to extract files into}
var
  Suffix: string; // unique suffix to extraction path
begin
  SetLength(Suffix, 4);
  repeat
    Suffix[1] := Char(Ord('0') + Random(10));
    Suffix[2] := Char(Ord('0') + Random(10));
    Suffix[3] := Char(Ord('0') + Random(10));
    Suffix[4] := Char(Ord('0') + Random(10));
    Result := TFileProcsLite.DirToPath(TFileProcsLite.TempFolder)
      + cTempFileStub + Suffix;
  until not TFileProcsLite.DirExists(Result);
end;

function TInstallerFiles.GetExtractPath: string;
  {Returns path to folder where files are to be extracted}
begin
  Result := TFileProcsLite.DirToPath(fExtractDir);
end;

function TInstallerFiles.GetInflaterDLLFile: string;
  {Returns fully specified file name of inflator DLL}
begin
  Result := GetExtractPath + cInflatorLib;
end;

function TInstallerFiles.GetInstallDataFile: string;
  {Returns fully specified file name of data file that contains all file to be
  installed}
begin
  Result := GetExtractPath + cInstallData;
end;

function TInstallerFiles.GetInstallDLLFile: string;
  {Returns fully specified file name of install DLL}
begin
  Result := GetExtractPath + cInstallLib;
end;

function TInstallerFiles.GetInstallInfoFile: string;
  {Returns fully specified file name of install information file}
begin
  Result := GetExtractPath + cInstallInfo;
end;

end.

