{ ##
  @FILE                     RegCOMSvrs.dpr
  @COMMENTS                 COM server DLL that registers and unregisters in-
                            process COM server DLLs.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/12/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 07/01/2001
      @COMMENTS             Major rewrite of code to provide registration /
                            unregistration via an object accessed by interface
                            rather than exporting registration / unregistration
                            functions. Added new unit: IntfRegCOMSvrs.pas.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 20/01/2001
      @COMMENTS             Further rewrite to convert the object to a COM
                            server. Main code moved to separate units:
                            + URegCOMSvrsExp
                            + URegCOMSvrs
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 29/12/2002
      @COMMENTS             Added unit UCOMSvrRegProcs to perform actual
                            registration/unregistration.
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
 * The Original Code is RegCOMSvrs.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


library RegCOMSvrs;

uses
  // Delphi
  IntfRegCOMSvrs in '..\..\Intf\IntfRegCOMSvrs.pas',
  URegCOMSvrsExp in 'URegCOMSvrsExp.pas',
  URegCOMSvrs in 'URegCOMSvrs.pas',
  UCOMSvrRegProcs in '..\..\Shared\Support\UCOMSvrRegProcs.pas';


{$R VRegCOMSvrs.res}  // version information resource


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
