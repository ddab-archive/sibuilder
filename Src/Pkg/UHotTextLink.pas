{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UHotTextLink.pas
  @COMMENTS                 Implements a class that encapsulates and hot link in
                            a hot text document.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literal with resource string.
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
 * The Original Code is UHotTextLink.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHotTextLink;


interface


uses
  // Delphi
  SysUtils;


type

  {
  THotTextLink:
    Class encapsulates and hot link in a hot text document.
  }
  THotTextLink = class(TObject)
  private // properties
    fCmd: string;
    fHint: string;
  public
    constructor Create(const Cmd, Hint: string);
      {Class constructor: Cmd and Hint are values for the properties of the
      same name. Cmd must not be '': raises exception if so}
    property Cmd: string read fCmd write fCmd;
      {Command string for the link: this command is passed to user when a link
      is clicked}
    property Hint: string read fHint write fHint;
      {Optional hint associated with the link: hint is displayed when mouse
      hovers over link}
  end;

  
  {
  EHotTextLink:
    Class of exception raised when errors are encountered in THotTextLink.
  }
  EHotTextLink = class(Exception);


implementation


resourcestring
  // Error messages
  sLinkMustHaveCmd
    = 'A hot text link must be created with a non-empty Cmd string';


{ THotTextLink }

constructor THotTextLink.Create(const Cmd, Hint: string);
  {Class constructor: Cmd and Hint are values for the properties of the same
  name. Cmd must not be '': raises exception if so}
begin
  inherited Create;
  if Cmd = '' then
    raise EHotTextLink.Create(sLinkMustHaveCmd);
  fCmd := Cmd;
  fHint := Hint;
end;


end.

