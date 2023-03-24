{ ##                  
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstallReport.pas
  @COMMENTS                 Implements objects that can write an installation
                            report for a given project to a stream.
                            Implementation is via interface and a factory class.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
      @COMMENTS             Original version supporting only HTML reports.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added new plain text report format by creating
                              another sub class of TInstallReport and expanding
                              the class factory to be able to create instances
                              of new text report class.
                            + Altered method for scanning files to use new
                              method of detecting folders and to ignore files in
                              temporary folders.
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
 * The Original Code is UInstallReport.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInstallReport;


interface


uses
  // Delphi
  Classes,
  // Project
  UProject;

type

  {
  TInstallReportKinds:
    The kinds of installation reports supported: HTML and Text.
  }
  TInstallReportKinds = (fmkHTML, fmkText);

  {
  IInstallReport:
    Interface to objects that write installation reports to a stream.

    Inheritance: IInstallReport -> [IUnknown]
  }
  IInstallReport = interface
    ['{EF4B0380-E93B-11D4-852A-AC0C36AEA81B}']
    procedure WriteToStream(const Stream: TStream);
    {Writes installation report for project to given stream}
  end;

  {
  TInstallReportFactory:
    Manufactures instance of install report objects of required kind.

    Inheritance: TInstallReportFactory -> [TObject]
  }
  TInstallReportFactory = class(TObject)
  public
    class function CreateInstallReport(const Project: TProject;
      const Kind: TInstallReportKinds): IInstallReport;
      {Creates an object to produce an installation report of the required kind}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UFiles, UGroups, UCOmmonTypes;

resourcestring
  // Error messages
  sBadReportKind = 'Unsupported install report kind';
  // Localisable report text common to all reports
  sEmptyFolder = 'Empty folder';
  sFileManifest = 'File Manifest';
  sFolder = 'Folder';

type

  {
  TInstallReport:
    Abstract base class for class(s) that create installation reports.

    Inheritance: TInstallReport -> [TInterfacedObject]
  }
  TInstallReport = class(TInterfacedObject, IInstallReport)
  private
    fProject: TProject; // reference to the installation project
  protected
    property Project: TProject read fProject;
      {The installation project: for use by descendants}
    procedure WriteHeader(const Stream: TStream); virtual; abstract;
      {Writes the report header: this is where initialisation is performed}
    procedure WriteManifestHeader(const Stream: TStream); virtual; abstract;
      {Writes the header to the file manifest}
    procedure WriteFolderHeader(const Stream: TStream;
      {Writes the information for each install folder}
      const Folder: string); virtual; abstract;
    procedure WriteFileName(const Stream: TStream;
      const FileName: string); virtual; abstract;
      {Writes the name of a file within a folder}
    procedure WriteNoFiles(const Stream: TStream); virtual; abstract;
      {Writes that there are no files within a folder}
    procedure WriteFolderFooter(const Stream: TStream); virtual; abstract;
      {Closes off information about a folder}
    procedure WriteFooter(const Stream: TStream); virtual; abstract;
      {Closes document: this is where finalisation is performed}
  protected
    { IInstallReport }
    procedure WriteToStream(const Stream: TStream);
      {Writes installation report for project to given stream}
  public
    constructor Create(const Project: TProject);
      {Class constructor: records reference to installation project}
  end;

  {
  THTMLInstallReport:
    Class that writes an installation report in HTML format.

    Inheritance: THTMLInstallReport -> TInstallReport -> [TInterfacedObject]
  }
  THTMLInstallReport = class(TInstallReport)
  protected
    procedure WriteHeader(const Stream: TStream); override;
      {Writes the report header: this is where initialisation is performed}
    procedure WriteManifestHeader(const Stream: TStream); override;
      {Writes the header to the file manifest}
    procedure WriteFolderHeader(const Stream: TStream;
      const Folder: string); override;
      {Writes the information for each install folder}
    procedure WriteFileName(const Stream: TStream;
      const FileName: string); override;
      {Writes the name of a file within a folder}
    procedure WriteNoFiles(const Stream: TStream); override;
      {Writes that there are no files within a folder}
    procedure WriteFolderFooter(const Stream: TStream); override;
      {Closes off information about a folder}
    procedure WriteFooter(const Stream: TStream); override;
      {Closes document: this is where finalisation is performed}
  end;

  {
  TTextInstallReport:
    Class that writes an installation report in plain text format.

    Inheritance: TTextInstallReport -> TInstallReport -> [TInterfacedObject]
  }
  TTextInstallReport = class(TInstallReport)
  protected
    procedure WriteHeader(const Stream: TStream); override;
      {Writes the report header}
    procedure WriteManifestHeader(const Stream: TStream); override;
      {Writes the header to the file manifest}
    procedure WriteFolderHeader(const Stream: TStream;
      const Folder: string); override;
      {Writes the information for each install folder}
    procedure WriteFileName(const Stream: TStream;
      const FileName: string); override;
      {Writes the name of a file within a folder}
    procedure WriteNoFiles(const Stream: TStream); override;
      {Writes that there are no files within a folder}
    procedure WriteFolderFooter(const Stream: TStream); override;
      {Closes off information about a folder}
    procedure WriteFooter(const Stream: TStream); override;
      {Closes document: this is where finalisation is performed}
  end;


{ TInstallReportFactory }

class function TInstallReportFactory.CreateInstallReport(
  const Project: TProject; const Kind: TInstallReportKinds): IInstallReport;
  {Creates an object to produce an installation report of the required kind}
begin
  case Kind of
    fmkHTML:  // HTML report
      Result := THTMLInstallReport.Create(Project);
    fmkText:  // Plain text report
      Result := TTextInstallReport.Create(Project);
    else      // unsupported report kind
      raise Exception.Create(sBadReportKind);
  end;
end;


{ Helper routines }

procedure WriteText(const Stream: TStream; const TheText: string);
  {Writes the given text to the given stream}
begin
  Stream.WriteBuffer(PChar(TheText)^, Length(TheText));
end;

procedure WriteTextLine(const Stream: TStream;
  const TheText: string);
  {Writes the given text, followed by a newline, to the given stream}
begin
  WriteText(Stream, TheText + #13#10);
end;


{ TInstallReport }

procedure CreateFolderTree(const FileList, DirList: TStrings);
  {Helper routine that creates a list of folders, with each entry having a sub-
  list of files - for all installation folders and files}
var
  FileIdx: Integer;     // loops thru all files to be installed by project
  Dir: string;          // install folder for a particular file
  FileName: string;     // unqualified name of a particular file
  DirIdx: Integer;      // index of install folder in folder list
  Obj: TObject;         // the object associated with the file
  Group: TGroup;        // the group that the file belongs to
begin
  // Process each file in project
  for FileIdx := 0 to Pred(FileList.Count) do
  begin
    // Get object associated with file: this is either TFileInfo for a file
    // or a TGroup for a folder
    Obj := FileList.Objects[FileIdx];
    Assert((Obj is TGroup) or (Obj is TFileInfo));
    // We need reference to group object
    if Obj is TGroup then
      // this is a folder: group reference is object itself
      Group := Obj as TGroup
    else {Obj is TFileInfo}
    begin
      // this is a file: so we get file's owning folder
      Group := (Obj as TFileInfo).GetGroup as TGroup;
      Assert(Assigned(Group));
    end;
    // We only report on files that are not temporary
    if Group.FileDeletion <> fdTemporary then
    begin
      // Split fully specified name into folder and file names
      Dir := ExtractFilePath(FileList[FileIdx]);
      FileName := ExtractFileName(FileList[FileIdx]);
      // Find folder in list, adding if not present
      DirIdx := DirList.IndexOf(Dir);
      if DirIdx = -1 then
        // folder not in list, so add it
        DirIdx := DirList.AddObject(Dir, TStringList.Create);
      // Add file to its folder's list: Obj is TFileInfo if this is a true file 
      if Obj is TFileInfo then
        (DirList.Objects[DirIdx] as TStringList).Add(FileName);
    end;
  end;
end;

procedure DestroyFolderTree(DirList: TStringList);
  {Helper routine that destroys and frees the tree created by CreateFolderTree}
var
  Idx: Integer; // loops thru all install folders
begin
  for Idx := 0 to Pred(DirList.Count) do
    DirList.Objects[Idx].Free;
end;

constructor TInstallReport.Create(const Project: TProject);
  {Class constructor: records reference to installation project}
begin
  inherited Create;
  fProject := Project;
end;

procedure TInstallReport.WriteToStream(const Stream: TStream);
  {Writes installation report for project to given stream}
var
  FileList: TStringList;      // all files in project (with install paths)
  DirList: TStringList;       // tree of install folders in project with files
  DirFilesList: TStringList;  // list of files in a folder
  DirIdx: Integer;            // loops thru all install folders
  FileIdx: Integer;           // loops thru all files in a folder
begin
  // Write out document and file manifest headers
  WriteHeader(Stream);
  WriteManifestHeader(Stream);
  // Get list of all files in project
  DirList := nil;
  FileList := TStringList.Create;
  try
    Project.GetInstallFileNames(FileList, True);
    // Create tree of folders with their files
    DirList := TStringList.Create;
    CreateFolderTree(FileList, DirList);
    DirList.Sort;
    // Write out each folder's info
    for DirIdx := 0 to Pred(DirList.Count) do
    begin
      WriteFolderHeader(Stream, DirList[DirIdx]);
      // Get sorted list of files in folder from tree
      DirFilesList := DirList.Objects[DirIdx] as TStringList;
      DirFilesList.Sort;
      // Write out files in folder or message to say there are none
      if DirFilesList.Count > 0 then
        for FileIdx := 0 to Pred(DirFilesList.Count) do
          WriteFileName(Stream, DirFilesList[FileIdx])
      else
        WriteNoFiles(Stream);
      // Finish off folder info
      WriteFolderFooter(Stream);
    end;
  finally
    // Tidy up
    if DirList <> nil then
      DestroyFolderTree(DirList);
    DirList.Free;
    FileList.Free;
  end;
  // Finalise document
  WriteFooter(Stream);
end;


{ THTMLInstallReport }

resourcestring
  // Localisable text strings used in HTML report
  sHTMLInstallReportTitle = 'Installation report';
  sHTMLInstallReportHeading = 'Installation report';

procedure THTMLInstallReport.WriteFileName(const Stream: TStream;
  const FileName: string);
  {Writes the name of a file within a folder}
begin
  WriteTextLine(Stream, Format('<li><code>%s</code></li>', [FileName]));
end;

procedure THTMLInstallReport.WriteFolderFooter(const Stream: TStream);
  {Closes off information about a folder}
begin
  // Finish off the file list
  WriteTextLine(Stream, '</ul>');
end;

procedure THTMLInstallReport.WriteFolderHeader(const Stream: TStream;
  const Folder: string);
  {Writes the information for each install folder}
begin
  // Write out the folder information
  WriteTextLine(Stream, Format(
    '<p><strong>%s: <code>%s</code></strong></p>', [sFolder, Folder]));
  // Begin a list of files
  WriteTextLine(Stream, '<ul>');
end;

procedure THTMLInstallReport.WriteFooter(const Stream: TStream);
  {Closes document: this is where finalisation is performed}
begin
  // Finalise the HTML body and document
  WriteTextLine(Stream, '</body>');
  WriteTextLine(Stream, '</html>');
end;

procedure THTMLInstallReport.WriteHeader(const Stream: TStream);
  {Writes the report header: this is where initialisation is performed}
begin
  // Write the HTML header with title
  WriteTextLine(Stream, '<html>');
  WriteTextLine(Stream, '<head>');
  WriteTextLine(Stream, Format('<title>%s: %s</title>',
      [sHTMLInstallReportTitle, fProject.Name]));
  WriteTextLine(Stream, '</head>');
  // Initialise the HTML body with main headings
  WriteTextLine(Stream, '<body>');
  WriteTextLine(Stream, Format('<h1>%s</h1>', [sHTMLInstallReportHeading]));
  WritetextLine(Stream, Format('<h2>%s</h2>', [fProject.Name]));
end;

procedure THTMLInstallReport.WriteManifestHeader(const Stream: TStream);
  {Writes the header to the file manifest}
begin
  WriteTextLine(Stream, Format('<h3>%s</h3>', [sFileManifest]));
end;

procedure THTMLInstallReport.WriteNoFiles(const Stream: TStream);
  {Writes that there are no files within a folder}
begin
  WriteTextLine(Stream, Format('<li>%s</li>', [sEmptyFolder]));
end;


{ TTextInstallReport }

resourcestring
  // Localisable text strings used in text report
  sTextReportHeader = 'Installation report for %s';
  sTextEndMarker = '--- End of report ---';
const
  // Non-localisable strings used in text report
  cBulletLine = '  +  %s';

procedure TTextInstallReport.WriteFileName(const Stream: TStream;
  const FileName: string);
  {Writes the name of a file within a folder}
begin
  WriteTextLine(Stream, Format(cBulletLine, [FileName]));
end;

procedure TTextInstallReport.WriteFolderFooter(const Stream: TStream);
  {Closes off information about a folder}
begin
  // Finish off the file list
  WriteTextLine(Stream, '');
end;

procedure TTextInstallReport.WriteFolderHeader(const Stream: TStream;
  const Folder: string);
  {Writes the information for each install folder}
begin
  // Write out the folder information
  WriteTextLine(Stream, Format('%s: %s', [sFolder, Folder]));
end;

procedure TTextInstallReport.WriteFooter(const Stream: TStream);
  {Closes document: this is where finalisation is performed}
begin
  // Write out end marker
  WriteTextLine(Stream, sTextEndMarker);
end;

procedure TTextInstallReport.WriteHeader(const Stream: TStream);
  {Writes the report header}
var
  HeaderText: string;   // report header text
  HeaderRuling: string; // ruling appearing above and below header text
  HeaderBorder: string; // border frame appearing on lines with no text
begin
  // Write report header text, boxed in by '*' chars
  HeaderText := Format(sTextReportHeader, [fProject.Name]);
  HeaderRuling := StringOfChar('*', Length(HeaderText) + 8);
  HeaderBorder := '**' + StringOfChar(' ', Length(HeaderText) + 4) + '**';
  WriteTextLine(Stream, HeaderRuling);
  WriteTextLine(Stream, HeaderBorder);
  WriteTextLine(Stream, Format('**  %s  **', [HeaderText]));
  WriteTextLine(Stream, HeaderBorder);
  WriteTextLine(Stream, HeaderRuling);
  // Write gap line
  WriteTextLine(Stream, '');
end;

procedure TTextInstallReport.WriteManifestHeader(const Stream: TStream);
  {Writes the header to the file manifest}
begin
  // Write file manifest text, with line of dashes above and below
  WriteTextLine(Stream, StringOfChar('-', 64));
  WriteTextLine(Stream, sFileManifest);
  WriteTextLine(Stream, StringOfChar('-', 64));
  WriteTextLine(Stream, '');
end;

procedure TTextInstallReport.WriteNoFiles(const Stream: TStream);
  {Writes that there are no files within a folder}
begin
  WriteTextLine(Stream, Format(cBulletLine, [sEmptyFolder]));
end;

end.
