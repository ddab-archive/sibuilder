{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegCOMSvrsExp.pas
  @COMMENTS                 Defines functions that are to be exported by the
                            RegCOMServers DLL.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 20/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Removed debug dialog box left in code in error.
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
 * The Original Code is URegCOMSvrsExp.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegCOMSvrsExp;


interface


{ COM Server required exports }

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HRESULT; stdcall;
  {Returns a class factory in Obj for the COM object specified by CLSID. IID is
  the class factory interface required. Only IClassFactory is supported}
function DllCanUnloadNow: HRESULT; stdcall;
  {DLL can unload only if there are no object instances and server is not locked
  by class factory}
function DllRegisterServer: HRESULT; stdcall;
  {Register COM object in registry}
function DllUnregisterServer: HRESULT; stdcall;
  {Remove COM object info from registry}


{ Shortcut for apps that explicitly load the DLL and don't use COM }

function CreateIntfObject(IID: TGUID; out Obj): HRESULT;
  {Creates an object that implements the interface given given IID and,
  providing IID is IRegInProcCOMServers, sets Obj to the object and returns
  S_OK. If IID is not IRegInProcCOMServers then E_NOINTERFACE is returned and
  Obj is set to nil}


implementation

uses
  // Delphi
  Windows, ActiveX,
  // Project
  IntfRegCOMSvrs, URegCOMSvrs;


{ Helper function }

function CreateRegKey(const Key, ValueName, Value: string): Boolean;
  {Helper function that creates registry keys with required values. Returns true
  only if we succeed}
var
  Handle: HKEY;         // handle to a registry key
  FnResult: LongInt;    // result code from Windows registry API
  Disposition: DWORD;   // whether a key was created or simply opened: unused
begin
  // Create or open the required key
  FnResult := RegCreateKeyEx(HKEY_CLASSES_ROOT, PChar(Key), 0, '',
    REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
    @Disposition);
  if FnResult = ERROR_SUCCESS then
  begin
    // Create the required value for the key
    FnResult := RegSetValueEx(Handle, PChar(ValueName), 0, REG_SZ,
      PChar(Value), Length(Value) + 1);
    RegCloseKey(Handle);
  end;
  // Return true if we succeeded and false if not
  Result := FnResult = ERROR_SUCCESS;
end;


{ Exported functions for COM }

function DllCanUnloadNow: HRESULT; stdcall;
  {DLL can unload only if there are no object instances and server is not locked
  by class factory}
begin
  if TRegCOMServerFactory.CanUnload then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HRESULT; stdcall;
  {Returns a class factory in Obj for the COM object specified by CLSID. IID is
  the class factory interface required. Only IClassFactory is supported}
var
  ClassFactory: TRegCOMServerFactory;  // class factory instance
begin
  // Find class we need to create
  if not IsEqualIID(CLSID, CLSID_RegCOMServers) then
  begin
    Result := CLASS_E_CLASSNOTAVAILABLE;
    Exit;
  end;
  // Create the class factory for the SITLicenseDlg COM object
  ClassFactory := TRegCOMServerFactory.Create;
  if ClassFactory.GetInterface(IID, Obj) then
    // required class factory interface supported
    Result := S_OK
  else
    // required class factory interface not supported
    Result := E_NOINTERFACE;
end;

function DllRegisterServer: HRESULT; stdcall;
  {Register COM object in registry}
var
  FileName: array[0..MAX_PATH] of Char; // file name of this DLL
  PFileName: PChar;                     // pointer to file name
begin
  // Get DLL file name and set pointer to start of it
  PFileName := FileName;
  Windows.GetModuleFileName(HInstance, FileName, SizeOf(FileName));
  // Set default failure result
  Result := E_FAIL;
  // Update registry for SITLicenseDlg object
  // store description in
  //  HKEY_CLASSES_ROOT\CLSID\{<GUID>}\(default)
  if not CreateRegKey('CLSID\' + CLSIDStr_RegCOMServers, '',
    'PJSoft: In-process COM server registrar') then
    Exit;
  // store server key name in
  //  HKEY_CLASSES_ROOT\CLSID\{<GUID>}\InprocServer32\(default)
  if not CreateRegKey('CLSID\' + CLSIDStr_RegCOMServers
    + '\InprocServer32', '', '"' + PFileName + '"') then
    Exit;
  // We made it: return success result
  Result := S_OK;
end;

function DllUnregisterServer: HRESULT; stdcall;
  {Remove COM object info from registry}
begin
  // Delete InprocServer32 keys
  RegDeleteKey(HKEY_CLASSES_ROOT,
    PChar('CLSID\' + CLSIDStr_RegCOMServers + '\InprocServer32'));
  // Delete CLSID key
  RegDeleteKey(HKEY_CLASSES_ROOT,
    PChar('CLSID\' + CLSIDStr_RegCOMServers));
  // All OK
  Result := S_OK;
end;

function CreateIntfObject(IID: TGUID; out Obj): HResult;
  {Creates an object that implements the interface given given IID and,
  providing IID is IRegInProcCOMServers, sets Obj to the object and returns
  S_OK. If IID is not IRegInProcCOMServers then E_NOINTERFACE is returned and
  Obj is set to nil}
var
  RegCOMServer: IRegInProcCOMServers; // object instance
begin
  {Set Obj to nil in case of failure}
  Pointer(Obj) := nil;
  if IsEqualIID(IID, IRegInProcCOMServers) then
  begin
    // We support IRegInProcCOMServers: so create instance and return in Obj
    RegCOMServer := TRegCOMServerFactory.CreateObj;
    Result := RegCOMServer.QueryInterface(IID, Obj);
  end
  else
    // Don't support given interface
    Result := E_NOINTERFACE;
end;

end.
