{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmUninstProjects.pas
  @COMMENTS                 Implements a class that maintains information about
                            projects that should be uninstalled before the
                            current project is installed.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/03/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/02/2008
      @COMMENTS             Replaced ';' literals with cListSep constant.
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
 * The Original Code is UUninstPrevProj.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UUninstPrevProj;


interface


uses
  // Delphi
  Classes,
  // Project
  UXMLObjects, UXMLTag;

type

  {
  TUninstallPrevProjects:
    Class that maintains information about projects that should be uninstalled
    before the current project is installed.
  }
  TUninstallPrevProjects = class(TXMLObjectItem)
  private // properties
    function GetAsSepText: string;
    procedure SetAsSepText(const Value: string);
  private
    fProjects: TStringList;
      {Maintains list of projects to be uninstalled}
    function Add(const Project: string): Integer;
      {Adds project with given name to uninstall list and returns its new index}
    function Count: Integer;
      {Returns number of project names in uninstall list}
  public
    constructor Create;
      {Class constructor: creates owned project list object}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    procedure SaveInstallXML(const ParentTag: TXMLTag);
      {Saves information required to determine which projects need to be
      uninstalled}
    procedure SaveProjectXML(const ParentTag: TXMLTag); override;
      {Saves object properties to given XML object}
    procedure LoadProjectXML(const XMLTag: TXMLTag); override;
      {Loads object's properties from given XML object}
    procedure Clear;
      {Clears list of projects}
    property AsSepText: string
      read GetAsSepText write SetAsSepText;
      {Provides access to projects as semi colon delinted text}
  end;



implementation


uses
  // Project
  UStringProcs;

const
  cListSep = ';'; // list separator for separator text


{ TUninstallPrevProjects }

function TUninstallPrevProjects.Add(const Project: string): Integer;
  {Adds project with given name to uninstall list and returns its new index}
begin
  Result := fProjects.Add(Project);
end;

procedure TUninstallPrevProjects.Clear;
  {Clears list of projects}
begin
  fProjects.Clear;
end;

function TUninstallPrevProjects.Count: Integer;
  {Returns number of project names in uninstall list}
begin
  Result := fProjects.Count;
end;

constructor TUninstallPrevProjects.Create;
  {Class constructor: creates owned project list object}
begin
  inherited;
  fProjects := TStringList.Create;
end;

destructor TUninstallPrevProjects.Destroy;
  {Class destructor: frees owned objects}
begin
  fProjects.Free;
  inherited;
end;

function TUninstallPrevProjects.GetAsSepText: string;
  {Read access method for AsSepText property: returns semicolon separated list
  of project names}
begin
  Result := UStringProcs.JoinStr(fProjects, cListSep, False);
end;

procedure TUninstallPrevProjects.LoadProjectXML(const XMLTag: TXMLTag);
  {Loads object's properties from given XML object}
var
  Idx: Integer; // loops thru XML sub tags defining named projects
begin
  // Clear any existing projects
  Clear;
  // Read in any named projects from <project> sub tags
  for Idx := 0 to Pred(XMLTag.SubTagCount) do
    if XMLTag.SubTags[Idx].Tag = 'project' then
      Add(XMLTag.SubTags[Idx].PlainText);
end;

procedure TUninstallPrevProjects.SaveInstallXML(const ParentTag: TXMLTag);
  {Saves information required to determine which projects need to be
  uninstalled}
var
  MainTag: TXMLTag; // the root tag under which the project are listed
  Idx: Integer;     // loops thru uninstall projects
begin
  // Don't write anything unless there's something to uninstall
  if Count > 0 then
  begin
    // Write out uninstall previous tag
    MainTag := ParentTag.AddSubTag('uninstprev');
    // Write out project ids as sub tags
    for Idx := 0 to Pred(Count) do
      MainTag.AddSubTag('project').PlainText := fProjects[Idx];
  end;
end;

procedure TUninstallPrevProjects.SaveProjectXML(const ParentTag: TXMLTag);
  {Saves object properties to given XML object}
var
  MainTag: TXMLTag; // the root tag under which the project are listed
  Idx: Integer;     // loops thru uninstall projects
begin
  // Don't write anything unless there's something to uninstall
  if Count > 0 then
  begin
    // Write out uninstall previous tag
    MainTag := ParentTag.AddSubTag('uninstprev');
    // Write out project ids as sub tags
    for Idx := 0 to Pred(Count) do
      MainTag.AddSubTag('project').PlainText := fProjects[Idx];
  end;
end;

procedure TUninstallPrevProjects.SetAsSepText(const Value: string);
  {Write access method for AsSepText property: splits given semi colon separated
  string into constituent projects, ignoring any leading, trailing or duplicated
  semi colons}
begin
  UStringProcs.SplitStr(Value, cListSep, fProjects, False);
end;

end.

