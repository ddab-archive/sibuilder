{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UBuilders.pas
  @COMMENTS                 Defines classes that can create an install program
                            from a given project.
  @DEPENDENCIES             DelphiDabbler Stream Extension Classes, v2.0.1 or
                            later for the PJIStream unit.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version. Two different builder
                            implementations were provided:
                            + A builder that creates an installer that carries
                              data as a payload.
                            + A builder that compiles an installer using a
                              specified version of Delphi.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 25/03/2007
      @COMMENTS             Major revision
                            + Removed TDelphiBuilder class that built installer
                              using Delphi.
                            + Removed reference to UDCC32 and UResWriterStreams
                              units.
                            + Removed ID parameter from
                              TBuilderFactory.CreateBuilder and modified to only
                              return TPayloadBuilder instance.
                            + Removed inclusion of ResIds.inc
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 19/02/2008
      @COMMENTS             Replaced usage of FileNames.inc include file with
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
 * The Original Code is UBuilders.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UBuilders;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UProject;

type

  {
  TBuilderStage:
    Stages of build process: reported via events.
  }
  TBuilderStage = (bsBuilding, bsCompiling);

  {
  TBuilderStageReport:
    Event handler method type used to report current build stage.
  }
  TBuilderStageReport = procedure(const Code: TBuilderStage) of object;

  {
  TBuilderProgressReport:
    Event handler method type used to notify progress messages.
  }
  TBuilderProgressReport = procedure(const Msg: string) of object;

  {
  TBuilder:
    Abstract class used to define the public interface to classes that can build
    install programs and embed the required data for the given project in them.
    Must be implemented by derived classes. This class is used as follows:
    Create an instance, set the Project and InstallPath properties and
    optionally hook up the OnProgress and OnBuild event handlers. Then call the
    Execute method to produce the install file.
  }
  TBuilder = class(TObject)
  private // properties
    fProject: TProject;
    fInstallerPath: string;
    fOnStage: TBuilderStageReport;
    fOnBuild: TBuilderProgressReport;
  public
    procedure Execute; virtual; abstract;
      {Builds the installer}
    property Project: TProject
      read fProject write fProject;
      {The project that provides the install script and data to be included in
      installer}
    property InstallerPath: string
      read fInstallerPath write fInstallerPath;
      {The path to the install file that is to be created}
    property OnStage: TBuilderStageReport
      read fOnStage write fOnStage;
      {Reports the various stages of the build process}
    property OnBuild: TBuilderProgressReport
      read fOnBuild write fOnBuild;
      {Reports progress as text as the build continues}
  end;

  {
  EBuilder:
    Exceptions raised by TBuilder objects when errors encountered.
  }
  EBuilder = class(Exception);

  {
  TBuilderFactory:
    Static factory class used to create builder objects of various types.
  }
  TBuilderFactory = class(TObject)
  public
    class function CreateBuilder: TBuilder;
      {Creates a TBuilder instance that creates a packaged installer}
  end;


implementation


uses
  // Delphi
  Classes,
  // DelphiDabbler library
  PJIStreams,
  // Project
  IntfCompressor, UCompressionMgr, UDataWriter, UEmbeddedData, UFileProcs,
  UPayloadStream, UXMLFile, UFileNames;


type

  {
  TBuilderFileRec:
    Record that holds information about a contents and required operations on a
    file used by builder objects.
  }
  TBuilderFileRec = record
    Src: string;        // source file
    Int: string;        // intermediate file used to build install program
    Inst: string[24];   // name of file when extracted by installer <=24 chars
    IsComp: Boolean;    // true if file is compressed
    IsPresent: Boolean; // true if file is included in install file
    DelSrc: Boolean;    // true if source file to be deleted
    DelInt: Boolean;    // true if intermediate file to be deleted
  end;

  {
  TBuilderFileKind:
    The kinds of files used by builder classes. Used to index the array of
    builder files.
  }
  TBuilderFileKind = (
    bfkInflator,        // file used to inflate following compression
    bfkInstLib,         // install library file
    bfkProjInfo,        // project information file
    bfkProjData         // project data file
  );

  {
  TBuilderFileArray:
    Array of file records that give information about files used in build.
  }
  TBuilderFileArray = array[TBuilderFileKind] of TBuilderFileRec;


type

  {
  TBaseBuilder:
    Abstract base class for classes that create install programs from project
    data. Implements parent object's Execute method which provides core
    functionality required to build installer. Derived classes must provide code
    that is specific to the kind of installer they are reponsible for building.
  }
  TBaseBuilder = class(TBuilder)
  protected
    fCompMgr: TCompressionMgr;
      {Compression manager object used to retrieve compressor object used in
      build}
    fCompressor: ICompressor;
      {Compressor object used in build}
    fBuildPath: string;
      {Path to files required to build install program}
    fFiles: TBuilderFileArray;
      {Array of information about file required to build installer}
    procedure ReportBuildStage(Code: TBuilderStage);
      {Reports current stage of build by triggering OnProgress event and passing
      given stage code}
    procedure ReportBuildProgress(const Msg: string);
      {Reports progress by triggering OnBuildProgress event with given message}
  protected
    function CopyFilesToDataStream(const Stm: TStream): LongWord;
      {Copies each of the files required to build installer to the given stream,
      with each file preceeded by a header record that gives information
      required by installer to extract the files. Returns total size of data
      written to stream, including the headers}
    procedure SelectCompressor;
      {Selects the compressor to be used by the builder to compress project
      files. Uses compression manager object}
    procedure BuildProjectFiles;
      {Builds the files required to create the installer}
    procedure CreateInstaller; virtual; abstract;
      {Creates the installer from the project files: descendant classes need to
      provide an implementation of this method, which depends on how installer
      is being constructed}
    procedure TidyUp; virtual;
      {Delete all temporary files}
  public
    constructor Create;
      {Class constructor: sets up fields and creates instance of compression
      manager object}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure Execute; override;
      {Builds the installer}
  end;

  {
  TPayloadBuilder:
    Class that creates an install program by appending project data as payload
    to a precompiled install program stub.
  }
  TPayloadBuilder = class(TBaseBuilder)
  protected
    procedure CreateInstaller; override;
      {Creates the installer from the project files. Files to be embedded in
      installer are appended to install program file stub as a payload}
  end;


resourcestring
  // Progress messages
  sCompInstLib = 'Compressing Install library';
  sCompProjInfo = 'Compressing project information';
  // Error messages
  sBadCompressor =
    'Compressor "%s" is not supported - please choose another compressor';


function CopyFile(const Src, Dest: string; Compressor: ICompressor): Boolean;
  {Copies the given source file to the given destination file. If the compressor
  is provided (assigned) then it is used to compress the data before writing to
  the destination file. If compressed data is larger than original then the data
  is written uncompressed. Returns true if data has been compressed and false
  if uncompressed}
var
  InStm: TFileStream;   // stream opened on source file
  OutStm: TFileStream;  // stream opened on destination file
begin
  // Open input and output streams
  InStm := nil;
  OutStm := nil;
  try
    InStm := TFileStream.Create(Src, fmOpenRead);
    OutStm := TFileStream.Create(Dest, fmCreate);
    if Assigned(Compressor) then
    begin
      // We have a compressor: use it to compress data and test final size
      if Compressor.Compress(
        TPJIStreamWrapper.Create(InStm),
        TPJIStreamWrapper.Create(OutStm),
        InStm.Size
      ) < InStm.Size then
      begin
        // Compressed data smaller than original: keep it
        Result := True;
      end
      else
      begin
        // Compressed data was larger tha original: copy uncompressed
        // truncate output stream and rewind input stream
        OutStm.Size := 0;
        InStm.Position := 0;
        // do simple copy
        OutStm.CopyFrom(InStm, 0);
        Result := False;
      end
    end
    else
    begin
      // No compressor: perform simple copy
      OutStm.CopyFrom(InStm, 0);
      Result := False;
    end;
  finally
    OutStm.Free;
    InStm.Free;
  end;
end;

procedure CopyFileToStream(const SrcFile: string; const DestStm: TStream);
  {Copies the contents of the given file to the given stream}
var
  SrcStm: TStream;  // stream opened on source file
begin
  SrcStm := TFileStream.Create(SrcFile, fmOpenRead);
  try
    DestStm.CopyFrom(SrcStm, 0);
  finally
    SrcStm.Free;
  end;
end;


{ TBaseBuilder }

procedure TBaseBuilder.BuildProjectFiles;
  {Builds the files required to create the installer}
var
  DataWriter: TDataWriter;  // used by project to store files in install file
  XMLFile: TXMLFile;        // used by project to store install info
  InfoStm: TStream;         // stream onto project information (XML) file
begin
  // Say we're at the building stage of the process
  ReportBuildStage(bsBuilding);

  // Create the project files that contain data and installation script (info)
  XMLFile := nil;
  // create data writer object used by project to write files to data file
  DataWriter := TDataWriter.Create(fFiles[bfkProjData].Src, fCompressor);
  try
    // create XML file object to hold installation info
    XMLFile := TXMLFile.Create(cInstRootTag);
    // get the project object to write project files to install data file and
    // create project information XML
    Project.Build(XMLFile.RootTag, DataWriter);
    // save the project info XML to file
    InfoStm := TFileStream.Create(fFiles[bfkProjInfo].Src, fmCreate);
    try
      XMLFile.SaveToStream(InfoStm);
    finally
      InfoStm.Free;
    end;
  finally
    XMLFile.Free;
    DataWriter.Free;
  end;

  // Compress the project files
  // say we're compressing project info
  ReportBuildProgress(sCompProjInfo);
  // compress the project info file
  fFiles[bfkProjInfo].IsComp := CopyFile(
    fFiles[bfkProjInfo].Src,
    fFiles[bfkProjInfo].Int,
    fCompressor
  );
  // NOTE: individual files within project file were already compressed.
  // so we don't attempt to compress further here

  // Now attempt to compress the install library file
  // say we're compressing it
  ReportBuildProgress(sCompInstLib);
  // do compression
  fFiles[bfkInstLib].IsComp := CopyFile(
    fFiles[bfkInstLib].Src,
    fFiles[bfkInstLib].Int,
    fCompressor
  );
end;

function TBaseBuilder.CopyFilesToDataStream(const Stm: TStream): LongWord;
  {Copies each of the files required to build installer to the given stream,
  with each file preceeded by a header record that gives information required by
  installer to extract the files. Returns total size of data written to stream,
  including the headers}
var
  FileHdr: TEmbeddedFileHeader;   // header that preceeds each file in stream
  I: TBuilderFileKind;            // loops thru all builder project files
begin
  // Set result (total data size) to 0
  Result := 0;
  // Copy each required install file to stream, preceeded by data header
  for I := Low(TBuilderFileKind) to High(TBuilderFileKind) do
  begin
    // check if the file is required
    if fFiles[I].IsPresent then
    begin
      // set up and write data file header
      FileHdr.FileName := fFiles[I].Inst;                 // name when extracted
      FileHdr.DataSize := SizeOfFile(fFiles[I].Int);   // intermediate file size
      FileHdr.IsCompressed := fFiles[I].IsComp;    // whether file is compressed
      Stm.WriteBuffer(FileHdr, SizeOf(FileHdr));
      // now store file itself
      CopyFileToStream(fFiles[I].Int, Stm);
      // update total data size
      Result := Result + SizeOf(FileHdr) + FileHdr.DataSize;
    end;
  end;
end;

constructor TBaseBuilder.Create;
  {Class constructor: sets up fields and creates instance of compression manager
  object}
begin
  inherited;

  // Record path to files required to build project: these are in Install sub-
  // folder of location where program was run from
  fBuildPath := MakePathName(ExtractFilePath(ParamStr(0)))
    + cInstallFolder + '\';

  // Setup builder file array with info about files used to build installer

  // set default inflator information: assumes no compression
  with fFiles[bfkInflator] do
  begin
    {Src is set once compressor is known: depends on type of compression}
    {Int is same as .Src since there is no separate intermediate file}
    Inst := cInflatorLib;
    IsComp := False;                                         // never compressed
    {IsPresent set once compressor is known: none included when no compression}
    DelSrc := False;
    DelInt := False;
  end;

  // set default install library information
  with fFiles[bfkInstLib] do
  begin
    Src := fBuildPath + cInstallLib;                   // predefined install DLL
    Int := TempFileName(cTempFileStub);                   // compressed inst DLL
    Inst := cInstallLib;
    {IsComp is set if install library is compressed}
    IsPresent := True;                                        // always included
    DelSrc := False;
    DelInt := True;
  end;

  // set default project info file information
  with fFiles[bfkProjInfo] do
  begin
    Src := TempFileName(cTempFileStub);                   // from TProject.Build
    Int := TempFileName(cTempFileStub);                   // compressed Src file
    Inst := cInstallInfo;
    {IsComp is set if info file is compressed}
    IsPresent := True;                                        // always included
    DelSrc := True;
    DelInt := True;
  end;

  // set default project data file information
  with fFiles[bfkProjData] do
  begin
    {IsComp is set if data}
    Src := TempFileName(cTempFileStub);                   // from TProject.Build
    Int := fFiles[bfkProjData].Src;                               // same as src
    Inst := cInstallData;
    IsComp := False;                      // any compression is internal to file
    IsPresent := True;                                        // always included
    DelSrc := False;                         // false since src and int are same
    DelInt := True;
  end;

  // Create compression manager
  fCompMgr := TCompressionMgr.Create;
end;

destructor TBaseBuilder.Destroy;
  {Class destructor: frees owned objects}
begin
  fCompressor := nil; // must be freed before manager: reason below
  fCompMgr.Free;      // this frees library which contained compressor instance
  inherited;
end;

procedure TBaseBuilder.Execute;
  {Builds the installer}
begin
  try
    // Choose compressor to be used (if any) to compress files
    SelectCompressor;
    // Build the files required to create installer
    BuildProjectFiles;
    // Create the actual installer
    CreateInstaller;
  finally
    // Tidy up: delete temporary files
    TidyUp;
  end;
end;

procedure TBaseBuilder.ReportBuildProgress(const Msg: string);
  {Reports progress by triggering OnBuild event with given message}
begin
  if Assigned(fOnBuild) then
    fOnBuild(Msg);
end;

procedure TBaseBuilder.ReportBuildStage(Code: TBuilderStage);
  {Reports current stage of build by triggering OnProgress event and passing
  given stage code}
begin
  if Assigned(fOnStage) then
    fOnStage(Code);
end;

procedure TBaseBuilder.SelectCompressor;
  {Selects the compressor to be used by the builder to compress project files.
  Uses compression manager object}
var
  CompItem: TCompressionItem; // compression item from compression manager
begin
  // Get reference to compressor object identified by project:
  // error if can't find match
  CompItem := fCompMgr.FindID(Project.CompressorID);
  if not Assigned(CompItem) then
    raise EBuilder.CreateFmt(sBadCompressor, [Project.CompressorID]);
  // Get reference to deflator object: used to compress data
  fCompressor := CompItem.GetDeflatorObj;
  // Fill in inflator file info that depends on selected compressor
  fFiles[bfkInflator].Src := CompItem.Inflator;
  fFiles[bfkInflator].Int := CompItem.Inflator;
  fFiles[bfkInflator].IsPresent := CompItem.Inflator <> '';
end;

procedure TBaseBuilder.TidyUp;
  {Delete all temporary files}
var
  I: TBuilderFileKind;  // loops thru builder's file array
begin
  // Here we delete all unrequired files: fFiles[] structures have info
  for I := Low(TBuilderFileKind) to High(TBuilderFileKind) do
  begin
    if fFiles[I].DelSrc then
      // we need to delete source file
      SysUtils.DeleteFile(fFiles[I].Src);
    if fFiles[I].DelInt then
      // we need to delete intermediate file
      SysUtils.DeleteFile(fFiles[I].Int);
  end;
end;


{ TPayloadBuilder }

procedure TPayloadBuilder.CreateInstaller;
  {Creates the installer from the project files. Files to be embedded in
  installer are appended to install program file stub as a payload}
var
  InstFileStub: string;         // full file name of install program stub
  InstFile: string;             // full file name of completed install program
  PayloadStm: TPayloadStream;   // stream used to append payload to program
begin
  // Say that we're at the compiling stage of the process
  ReportBuildStage(bsCompiling);
  // Copy stub file to location where installer is to be built (uncompressed)
  InstFileStub := fBuildPath + cInstallStub;
  InstFile := MakePathName(fInstallerPath) + fProject.InstallProgName;
  CopyFile(InstFileStub, InstFile, nil);
  // Create a payload stream on installer: this appends data to install file
  // in payload format
  PayloadStm := TPayloadStream.Create(InstFile, pomWrite);
  try
    // Copy all required files to stream
    CopyFilesToDataStream(PayloadStm);
  finally
    // Close payload stream
    PayloadStm.Free;
  end;
end;


{ TBuilderFactory }

class function TBuilderFactory.CreateBuilder: TBuilder;
  {Creates a TBuilder instance that creates a packaged installer}
begin
  Result := TPayloadBuilder.Create;
end;

end.

