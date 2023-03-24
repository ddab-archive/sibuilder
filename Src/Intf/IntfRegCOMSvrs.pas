{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     IntfRegCOMSvrs.pas
  @COMMENTS                 Defines interface used to register in-process
                            servers and CLSID for associated COM object.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 06/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 20/01/2001
      @COMMENTS             Added CLSID constant for RegCOMServers object
                            required for conversion to COM object.
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
 * The Original Code is IntfRegCOMSvrs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit IntfRegCOMSvrs;

interface

type
  {Interface that provides facility to register and unregister in process COM
  server DLLs}
  IRegInProcCOMServers = interface
    ['{FB9AF8C0-E29C-11D4-852A-845555331611}']
    function RegInProcSvr(const SvrName: PChar): HResult; stdcall;
      {Calls DllRegisterServer in the given COM server dll and returns the
      resultc code returned by that function (usually S_OK or S_FALSE on
      success). In case of error possible error codes are:
        CO_E_NOT_SUPPORTED: The DLL doesn't export DllRegisterServer
        CO_E_DLLNOTFOUND: The DLL could not be loaded
        Other errors codes reported by the given function (probably E_FAIL)}
    function UnRegInProcSvr(const SvrName: PChar): HResult; stdcall;
      {Calls DllUnRegisterServer in the given COM server dll and returns the
      result code returned by that function (usually S_OK or S_FALSE on
      success). In case of error possible error codes are:
        CO_E_NOT_SUPPORTED: The DLL doesn't export DllUnRegisterServer
        CO_E_DLLNOTFOUND: The DLL could not be loaded
        Other errors codes reported by the given function (probably E_FAIL)}
  end;

const
  {CLSID for RegCOMServers object}
  CLSIDStr_RegCOMServers = '{F8F7B040-EEF4-11D4-852A-99F099D64216}';
    {String representation of CLSID}
  CLSID_RegCOMServers: TGUID = CLSIDStr_RegCOMServers;
    {The CLSID}

implementation

end.
