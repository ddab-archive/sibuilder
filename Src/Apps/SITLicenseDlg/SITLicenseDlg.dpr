{ ##
  @FILE                     SITLicenseDlg.dpr
  @COMMENTS                 COM server library that displays a license dialog
                            box.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
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
 * The Original Code is SITLicenseDlg.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


library SITLicenseDlg;


uses
  // Project
  IntfSITLicenseDlg in '..\..\Intf\IntfSITLicenseDlg.pas',
  ULicenseDlg in 'ULicenseDlg.pas',
  ULicenseDlgWdw in 'ULicenseDlgWdw.pas',
  ULicenseDlgExp in 'ULicenseDlgExp.pas';


{$R LicDlgBox.res}        // Resource file containing dialog box
{$R VSITLicenseDlg.res}   // Resource file containing version information


exports
  // Exports required by COM
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  // Short-cut to create object when not using COM
  CreateIntfObject;

begin
end.
