{ ##                     
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UCOMSvrRegProcs.pas
  @COMMENTS                 Provides routines registering and unregistering COM
                            servers with windows.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
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
 * The Original Code is UCOMSvrRegProcs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UCOMSvrRegProcs;


interface


function RegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}

function UnRegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllUnRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllUnRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}


implementation


uses
  // Delphi
  Windows;


function PvtCallRegSvrFn(const SvrName, FnName: PChar): HResult;
  {Calls given function name (which must be type of DllRegisterServer and
  DllUnregisterServer) in COM server DLL with given name, and returns the result
  returned by the given function if the function is found. If the function can't
  be found the following error codes give the reason:
    CO_E_NOT_SUPPORTED: The DLL doesn't support the given function
    CO_E_DLLNOTFOUND: The DLL could not be loaded}
var
  DLLHandle: THandle;     // handle to COM server DLL
  Fn: function: HResult;  // the function to call
begin
  // Load the COM server DLL
  DLLHandle := LoadLibrary(SvrName);
  if DLLHandle <> 0 then
  begin
    // Get address of required function
    Fn := GetProcAddress(DLLHandle, FnName);
    if @Fn <> nil then
      // Call the function and return its result
      Result := Fn
    else
      // Couldn't find the function
      Result := CO_E_NOT_SUPPORTED;
    // Free the library
    FreeLibrary(DLLHandle);
  end
  else
    // Couldn't load the DLL
    Result := CO_E_DLLNOTFOUND;
end;

function RegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}
begin
  Result := PvtCallRegSvrFn(SvrName, 'DllRegisterServer');
end;

function UnRegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllUnRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllUnRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}
begin
  Result := PvtCallRegSvrFn(SvrName, 'DllUnregisterServer');
end;

end.
