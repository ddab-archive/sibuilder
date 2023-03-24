{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     ULicenseDlgHelp.pas
  @COMMENTS                 Defines a class implements the license dialog box's
                            ISITLicenseDlgHelp interface to display help for the
                            license dialog box using the application's main help
                            files.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/11/2005
      @COMMENTS             Changed to use THelpManager to handle help display
                            rather than rely on Delphi's built in processing.
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
 * The Original Code is ULicenseDlgHelp.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit ULicenseDlgHelp;

interface

uses
  // Delphi
  Windows,
  // Project
  IntfSITLicenseDlg, UHelpManager;


type

  {Class implements ISITLicenseDlgHelp interface that displays help for the
  license dialog box using the application's main help file}
  TLicenseDlgHelp = class(TInterfacedObject, ISITLicenseDlgHelp)
  protected
    function DisplayHelp(HelpCommand: UINT; Data: DWORD): HRESULT; stdcall;
      {Display help using the given help command and data. The two parameters
      have the same meaning as the final two parameters to WinHelp}
  end;


implementation

uses
  // Delphi
  Forms;


{ TLicenseDlgHelp }

function TLicenseDlgHelp.DisplayHelp(HelpCommand: UINT;
  Data: DWORD): HRESULT;
  {Display help using the given help command and data. The two parameters have
  the same meaning as the final two parameters to WinHelp}
begin
  if THelpManager.DoCommand(HelpCommand, Data) then
    Result := S_OK
  else
    Result := E_FAIL;
end;

end.
