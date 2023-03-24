{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UProjFileManager.pas
  @COMMENTS                 Unit contains the definition of a class that manages
                            the saving and loading of project files and
                            templates.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 04/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/12/2002
      @COMMENTS             + Modified so that v2 XML files are converted into
                              the newer v3 format when loading projects.
                            + LoadFromProject was modified to return comments
                              about any changes made in converting files.
                            + Pulled out v2 and v1 conversion code into their
                              own methods.
                            + Replaced string literals with resource strings
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 19/02/2008
      @COMMENTS             + Revised to maintain default template file in a
                              user application data folder rather than folder
                              containing SIBuilder executable. Copies legacy
                              template files to new location if needed.
                            + Replaced usage of FileNames.inc include file with
                              UFileNames unit.
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
 * The Original Code is UProjFileManager.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UProjFileManager;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UProject;


type

  {
  EProjFileManager:
    Exceptions raised by TProjFileManager.

    Inheritance: EProjFileManager -> [Exception]
  }
  EProjFileManager = class(Exception);


  {
  TProjFileManager:
    Class that manages the saving and loading of project files and templates.

    Inheritance: TProjFileManager -> [TObject]
  }
  TProjFileManager = class(TObject)
  private  // properties
    fFileName: string;
    fOnFileNameChange: TNotifyEvent;
    function GetPath: string;
  private
    fProject: TProject;
      {Reference to the project whose files are being managed}
    function LoadV1File(const FileName: string): Boolean;
      {Converts and loads a version 1 binary file into the project. If
      conversion succeeds the file is loaded into project and true is returned.
      If conversion fails project is not changed and false is returned}
    function LoadV2File(const FileName: string;
      const Comments: TStrings): Boolean;
      {Converts and loads a version 2 XML into the project. If conversion
      succeeds the file is loaded into project and true is returned. If
      conversion fails project is not changed and false is returned. If Comments
      is not nil, details of any changes made to convert the file are returned
      in it (and help contexts are stored in string list's Objects[] property}
    function LoadTemplateFile(const FileName: string): Boolean;
      {Loads the given template file which can be either a v1, v2 or v3 style
      file. Returns true on success or false if the file can't be loaded. If
      file is a v1 file a new v3 template file is created}
    procedure FileNameChange;
      {Triggers OnFileNameChange event}
    procedure SetFileName(const FileName: string);
      {Records file name and triggers OnFileNameChange event}
    class function AppDir: string;
      {Gets full path to program's user data folder}
    class function ProjTemplateFile: string;
      {Gets full path of project template file in user data folder}
    class function OldProjTemplateFile(const Ext: string): string;
      {Gets full path to old project template file in application executable
      folder. Required file extension must be specified}
  public
    constructor Create(Project: TProject);
      {Class constructor: records reference to project and calculates name of
      default template file from program's name}
    function LoadProject(const FileName: string;
      const Comments: TStrings): Integer;
      {Loads project in from given file. If file is not current version it is
      converted and opened as an un-named file. Returns values indicates version
      of file opened:
        >=300 if the file is the current version
        >=200 and <300 if the file is an SIBuilder v2 XML type file
        100 if the file is a v1 style binary file
        0 if the file is not a valid project file.
      If the file was converted and the converter returned comments about the
      modifications made to the file, the comments are stored in the Comments
      string list (and associated help topic identifiers are stored in its
      Objects[] property. If there are no comments then the list is cleared}
    function LoadTemplate: Boolean;
      {Loads the default template into the project and returns true on success
      and false on failure}
    procedure SaveProject;
      {Saves current project under current name. The project must be named to
      use this method}
    procedure SaveProjectAs(const FileName: string);
      {Saves the current project under the given name. If the extension is the
      same as old style files then an exception is raised}
    procedure SaveProjectTemplate;
      {Saves the current project as the default template}
    procedure EmptyProject;
      {Clears the current project and makes it un-named}
    procedure DeleteTemplateFiles;
      {Deletes contents of current template file and replaces with a blank
      template}
    property FileName: string read fFileName;
      {The name of the current project file}
    property Path: string read GetPath;
      {The path to the current project file}
    property OnFileNameChange: TNotifyEvent
      read fOnFileNameChange write fOnFileNameChange;
      {Event triggered whenever the project file name changes}
  end;


implementation


uses
  // Delphi
  ShlObj,
  // Project
  UFileNames, UFileProcs, UProjFileVerManager;

resourcestring
  // Error messages
  sCantSaveOldStyle = 'This version of SIBuilder can''t save %s files';


procedure CopyFile(const Src, Dest: string);
  {Copies a file. Src is path to source file and Dest is path to destination
  file}
var
  SrcStm: TFileStream;  // stream onto source file
  DestStm: TFileStream; // stream onto destination file
begin
  SrcStm := TFileStream.Create(Src, fmOpenRead or fmShareDenyWrite);
  try
    DestStm := TFileStream.Create(Dest, fmCreate);
    try
      DestStm.CopyFrom(SrcStm, 0);
    finally
      FreeAndNil(DestStm);
    end;
  finally
    FreeAndNil(SrcStm);
  end;
end;

{ TProjFileManager }

class function TProjFileManager.AppDir: string;
  {Gets full path to program's user data folder}
begin
  Result := UserAppDataFolder + '\DelphiDabbler\SIBuilder3';
end;

constructor TProjFileManager.Create(Project: TProject);
  {Class constructor: records reference to project and calculates name of
  default template file from program's name}
begin
  inherited Create;
  // Record reference to project
  fProject := Project;
  // Ensure project template folder exists and contains an appropriate template
  // file
  EnsureFolders(AppDir);
  if not FileExists(ProjTemplateFile) then
  begin
    // No template file: try to copy old one
    if FileExists(OldProjTemplateFile(cDefProjExt)) then
      CopyFile(OldProjTemplateFile(cDefProjExt), ProjTemplateFile)    // v2 type
    else if FileExists(OldProjTemplateFile(cOldDefProjExt)) then
      CopyFile(OldProjTemplateFile(cOldDefProjExt), ProjTemplateFile) // v1 type
    else
      DeleteTemplateFiles;        // this creates an empty (blank) template file
  end;
end;

procedure TProjFileManager.DeleteTemplateFiles;
  {Deletes contents of current template file and replaces with a blank template}
var
  BlankProject: TProject; // blank project used to write template
begin
  // Create and save blank project in template file
  BlankProject := TProject.Create;
  try
    BlankProject.SaveProjectToFile(ProjTemplateFile);
  finally
    FreeAndNil(BlankProject);
  end;
end;

procedure TProjFileManager.EmptyProject;
  {Clears the current project and makes it un-named}
begin
  fProject.Reset;
  SetFileName('');
end;

procedure TProjFileManager.FileNameChange;
  {Triggers OnFileNameChange event}
begin
  if Assigned(fOnFileNameChange) then fOnFileNameChange(Self);
end;

function TProjFileManager.GetPath: string;
  {Read access method for Path property}
begin
  Result := ExtractFilePath(fFileName);
end;

function TProjFileManager.LoadProject(const FileName: string;
  const Comments: TStrings): Integer;
  {Loads project in from given file. If file is not current version it is
  converted and opened as an un-named file. Returns values indicates version of
  file opened:
    >=300 if the file is the current version
    >=200 and <300 if the file is an SIBuilder v2 XML type file
    100 if the file is a v1 style binary file
    0 if the file is not a valid project file.
  If the file was converted and the converter returned comments about the
  modifications made to the file, the comments are stored in the Comments string
  list (and associated help topic identifiers are stored in its Objects[]
  property. If there are no comments then the list is cleared}
begin
  Assert(Assigned(Comments));
  // Clear the comments
  Comments.Clear;
  // Get version of project file
  Result := TProjFileVerManager.FileVersion(PChar(FileName));
  if Result >= 300 then
  begin
    // We have a v3 XML project file - read it and set as current file
    fProject.LoadProjectFromFile(FileName);
    SetFileName(FileName);
  end
  else if (Result >= 200) and (Result < 300) then
  begin
    // We have a v2 XML project file: try to convert it and open as un-named
    if LoadV2File(FileName, Comments) then
      SetFileName('')
    else
      Result := 0;
  end
  else if Result = 100 then
  begin
    // We have a v1 binary file: convert to a new XML file and open as un-named
    if LoadV1File(FileName) then
      SetFileName('')
    else
    begin
      // error: record error in comments
      Result := 0;
      Comments.Text :=TProjFileVerManager.LastError;
    end;
  end;
end;

function TProjFileManager.LoadTemplate: Boolean;
  {Loads the default template into the project and returns true on success and
  false on failure}
begin
  // Try to project template file
  Result := LoadTemplateFile(ProjTemplateFile);
  // If we succeeded then we have a new project, so delete file name
  if Result then
    SetFileName('');
end;

function TProjFileManager.LoadTemplateFile(const FileName: string): Boolean;
  {Loads the given template file which can be either a v1, v2 or v3 style file.
  Returns true on success or false if the file can't be loaded. If file is a v1
  file a new v3 template file is created}
var
  Ver: Integer;   // version of given file
begin
  try
    // Get project version of template file
    Ver := TProjFileVerManager.FileVersion(FileName);
    if Ver = 100 then
    begin
      // V1 binary: convert it
      Result := LoadV1File(FileName);
      if Result then
        // save loaded project as new style template file
        SaveProjectTemplate;
    end
    else if (Ver >= 200) and (Ver < 300) then
      // V2 XML file: convert it and load
      Result := LoadV2File(FileName, nil)
    else if Ver >= 300 then
    begin
      // V3 XML file: just load it
      fProject.LoadProjectFromFile(FileName);
      Result := True;
    end
    else
      // Unknown file: return false
      Result := False;
  except
    // Traps exceptions and return false
    Result := False;
  end;
end;

function TProjFileManager.LoadV1File(const FileName: string): Boolean;
  {Converts and loads a version 1 binary file into the project. If conversion
  succeeds the file is loaded into project and true is returned. If conversion
  fails project is not changed and false is returned}
var
  TempFile: string; // temp file used for coversion
begin
  // Convert given file into temporary file
  TempFile := UFileProcs.TempFileName(cTempFileStub);
  try
    Result := TProjFileVerManager.Convert(FileName, TempFile);
    if Result then
      // Conversion succeeded: update project
      fProject.LoadProjectFromFile(TempFile);
  finally
    // Delete temp file
    DeleteFile(TempFile);
  end;
end;

function TProjFileManager.LoadV2File(const FileName: string;
  const Comments: TStrings): Boolean;
  {Converts and loads a version 2 XML into the project. If conversion succeeds
  the file is loaded into project and true is returned. If conversion fails
  project is not changed and false is returned. If Comments is not nil, details
  of any changes made to convert the file are returned in it (and help contexts
  are stored in string list's Objects[] property}
var
  TheComments: TStringList; // used to store comments returned from converter
  TempFile: string;         // temp file used to store converted project
begin
  // We create a string list to store comments returned from convertor: we can't
  // pass Comments directly to it since Comments may be nil and convertor
  // requires a non-nil string list
  TheComments := TStringList.Create;
  try
    // Convert file into temporary file
    TempFile := UFileProcs.TempFileName(cTempFileStub);
    try
      Result := TProjFileVerManager.Convert2(FileName, TempFile, TheComments);
      if Result then
        // Conversion suceeded: load the project
        fProject.LoadProjectFromFile(TempFile);
    finally
      // Delete temp XML file
      DeleteFile(TempFile);
    end;
    if Assigned(Comments) then
      // We want the comments, so assign them to parameter
      Comments.Assign(TheComments);
  finally
    TheComments.Free;
  end;
end;

class function TProjFileManager.OldProjTemplateFile(
  const Ext: string): string;
  {Gets full path to old project template file in application executable folder.
  Required file extension must be specified}
begin
  Result := ChangeFileExt(ParamStr(0), Ext);
end;

class function TProjFileManager.ProjTemplateFile: string;
  {Gets full path of project template file in user data folder}
begin
  Result := AppDir + '\'
    + ChangeFileExt(ExtractFileName(ParamStr(0)), cDefProjExt);
end;

procedure TProjFileManager.SaveProject;
  {Saves current project under current name. The project must be named to use
  this method}
begin
  Assert(FileName <> '');
  fProject.SaveProjectToFile(FileName);
end;

procedure TProjFileManager.SaveProjectAs(const FileName: string);
  {Saves the current project under the given name. If the extension is the same
  as old style files then an exception is raised}
begin
  // We can't save to files with old extension - raise exception if trying to
  if AnsiCompareText(ExtractFileExt(FileName), cOldProjExt) = 0 then
    raise EProjFileManager.CreateFmt(
      sCantSaveOldStyle, [cOldProjExt]);
  // Record new file name and save the project using it
  SetFileName(FileName);
  SaveProject;
end;

procedure TProjFileManager.SaveProjectTemplate;
  {Saves the current project as the default template}
begin
  fProject.SaveProjectToFile(ProjTemplateFile);
end;

procedure TProjFileManager.SetFileName(const FileName: string);
  {Records file name and triggers OnFileNameChange event}
begin
  fFileName := FileName;
  FileNameChange;
end;

end.

