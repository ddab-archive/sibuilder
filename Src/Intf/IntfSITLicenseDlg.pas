{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     IntfSITLicenseDlg.pas
  @COMMENTS                 Defines interfaces used to display license dialog
                            box & provide help. Also defines style constants
                            and CLSID for SITLicenseDlg COM object.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 20/01/2001
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
 * The Original Code is IntfSITLicenseDlg.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit IntfSITLicenseDlg;

interface

uses
  // Project
  Windows;
  

const
  // Dialog box styles
  SITLicDlg_DECLINEBTN  = $00000001;  // include a decline buttons (default)
  SITLicDlg_BTNSLEFT    = $00000002;  // align buttons at bottom left
  SITLicDlg_BTNSCENTRE  = $00000004;  // align buttons at bottom centre
  SITLicDlg_BTNSRIGHT   = $00000008;  // align buttons at bottom right (default)
  SITLicDlg_MONOFONT    = $00000010;  // display license using a mono font


type
  {Defines interface that must be implemented by help objects registered with a
  license dialog box to display help about the dialog. This interface should
  be implemented by the user of ISITLicenseDlg}
  ISITLicenseDlgHelp = interface
    ['{23B4EA81-E033-11D4-852A-A97BF8F68431}']
    function DisplayHelp(HelpCommand: UINT; Data: DWORD): HRESULT; stdcall;
      {Display help using the given help command and data. The two parameters
      have the same meaning as the final two parameters to WinHelp}
  end;

  {Interface used to configure and display the license dialog box}
  ISITLicenseDlg = interface
    ['{23B4EA80-E033-11D4-852A-A97BF8F68431}']
    procedure SetStyle(AStyle: DWORD); safecall;
      {Sets the required style flags of the dialog box}
    procedure SetSize(Width, Height: Integer); safecall;
      {Sets the size of the dialog box. If 0 is specified for any parameter then
      that dimiension is unchanged. If any value that is less than min or
      greater than max dimension is specified then min or max size is used}
    procedure RegisterHelp(HelpIntf: ISITLicenseDlgHelp;
      HelpContext: DWORD); safecall;
      {Registers a help "callback" interface instance that dialog box calls when
      help button is clicked. Also stores the help context number that is to be
      passed to the callback method}
    function DisplayLicenseText(HOwner: HWND; X, Y: Integer;
      Caption, License: PChar): HRESULT; stdcall;
      {Displays the license dialog box as a child of the given owner window, at
      given (X,Y) offset on screen wqith given caption and displaying given
      license text. Returns S_OK if user accepts or S_FALSE if user cancels or
      declines}
    function DisplayLicenseFile(HOwner: HWND; X, Y: Integer;
      Caption, FileName: PChar): HRESULT; stdcall;
      {Displays the license dialog box as a child of the given owner window, at
      given (X,Y) offset on screen wqith given caption and displaying the
      contents of the given text file. Returns S_OK if user accepts, S_FALSE if
      user cancels or declines and E_FAIL if the file cannot be read}
  end;

const
  {CLSID(s) of COM object(s) supported by this server}
  CLSIDStr_SITLicenseDlg = '{5085EDCC-EEB6-11D4-852A-BB2762DB8610}';
    {string representation of SITLicenseDlg object's CLSID}
  CLSID_SITLicenseDlg: TGUID = CLSIDStr_SITLicenseDlg;
    {SITLicenseDlg object's CLSID}

implementation

end.
