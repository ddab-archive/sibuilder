{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UShellLink.pas
  @COMMENTS                 Implements a static class that creates and checks
                            shell links.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 24/11/2005
      @COMMENTS             Original version.
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
 * The Original Code is UShellLink.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UShellLink;


interface


uses
  // Delphi
  ShlObj;


type

  {
  TShellLink:
    Static class for creating and checking shell links.
  }
  TShellLink = class(TObject)
  private
    class function LoadShellLink(const LinkFileName: string): IShellLink;
      {Loads a shell link file into a shell link object and returns the
      IShellLink interface of the object. If the given file is not a shell link
      nil is returned. The returned object can be used to access information
      about the shell link}
  public
    class function CreateShellLink(const LinkFileName, AssocFileName, Desc,
      WorkDir, Args, IconFileName: string; const IconIdx: Integer): Boolean;
      {Creates a shell link named LinkFileName that is a shortcut to file
      AssocFileName with descriprion Desc. The shortcut activates its file in
      the given working directory and passes the given command line Args to
      AssocFileName. If an icon file and index offset are provided the specified
      icon is used for the shortcut. True is returned on success and false on
      error}
    class function IsShellLink(const LinkFileName: string): Boolean;
      {Checks if the given file is a shell link}
  end;


implementation


uses
  // Delphi
  ActiveX;


{ TShellLink }

class function TShellLink.CreateShellLink(const LinkFileName,
  AssocFileName, Desc, WorkDir, Args, IconFileName: string;
  const IconIdx: Integer): Boolean;
var
  SL: IShellLink;   // shell link object
  PF: IPersistFile; // persistant file interface to shell link object
begin
  // Assume failure
  Result := False;
  // Ensure COM is initialised
  CoInitialize(nil);
  try
    // Create shell link object
    if Succeeded(
      CoCreateInstance(
        CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, SL
      )
    ) then
    begin
      // Store required properties of shell link
      SL.SetPath(PChar(AssocFileName));
      SL.SetDescription(PChar(Desc));
      SL.SetWorkingDirectory(PChar(WorkDir));
      SL.SetArguments(PChar(Args));
      if (IconFileName <> '') and (IconIdx >= 0) then
        SL.SetIconLocation(PChar(IconFileName), IconIdx);
      // Create persistant file interface to shell link to save link file
      PF := SL as IPersistFile;
      Result := Succeeded(
        PF.Save(PWideChar(WideString(LinkFileName)), True)
      );
    end;
  finally
    // Finalize COM
    CoUninitialize;
  end;
end;

class function TShellLink.IsShellLink(const LinkFileName: string): Boolean;
  {Checks if the given file is a shell link}
begin
  // Ensure COM is initialized
  CoInitialize(nil);
  try
    // Valid shell link if we can load it
    Result := Assigned(LoadShellLink(LinkFileName));
  finally
    // Finalize COM
    CoUninitialize;
  end;
end;

class function TShellLink.LoadShellLink(
  const LinkFileName: string): IShellLink;
var
  PF: IPersistFile; // persistent file interface to shell link object
begin
  // Create shell link object
  if Succeeded(
    CoCreateInstance(
      CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, Result
    )
  ) then
  begin
    // Try to load the shell link: succeeds only of file is shell link
    PF := Result as IPersistFile;
    if Failed(
      PF.Load(PWideChar(WideString(LinkFileName)), STGM_READ)
    ) then
      Result := nil;  // this frees the shell link object
  end
  else
    Result := nil;
end;

end.
