{ ##               
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegCOMSvrs.pas
  @COMMENTS                 Defines COM object that performs registration /
                            unregistration and the factory object.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 20/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/12/2002
      @COMMENTS             Now uses functions from UCOMSvrRegProcs to perform
                            actual registration / unregistration. Deleted helper
                            function that is no longer required.
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
 * The Original Code is URegCOMSvrs.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2001-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegCOMSvrs;


interface


uses
  // Delphi
  ActiveX, Windows,
  // Project
  IntfRegCOMSvrs;

type
  {
  TRegCOMServerFactory:
    COM Class factory for RegCOMServer COM object.

    Inheritance: TRegCOMServerFactory -> [TInterfacedObject]
  }
  TRegCOMServerFactory = class(TInterfacedObject, IClassFactory)
  protected
    { IClassFactory methods }
    function CreateInstance(const UnkOuter: IUnknown;
      const IID: TGUID; out Obj): HRESULT; stdcall;
      {Creates a RegCOMServers object that supports given interface and passes
      it back in Obj, returning S_OK. If interface is not supported by COM
      object an error code is returned and Obj is set to nil}
    function LockServer(Lock: BOOL): HRESULT; stdcall;
      {Locks and unlocks the server to prevent/permit unloading}
  public
    constructor Create;
      {Constructor: increments count of object instances in server}
    destructor Destroy; override;
      {Destructor: decrements count of object instances in server}
    class function CanUnload: WordBool;
      {Returns true if server can be unloaded and false if not}
    class function CreateObj: IRegInProcCOMServers;
      {Returns an RegCOMServers object instance that implements
      IRegInProcCOMServers without using CreateInstance method: for use when DLL
      is a private server}
  end;


implementation


uses
  // Project
  UCOMSvrRegProcs;

type
  {
  TRegCOMServers:
    Class that implements IRegInProcCOMServers - wraps function calls to
    register/unregister a COM DLL.

    Inheritance: TRegCOMServers -> [TInterfacedObject]
  }
  TRegCOMServers = class(TInterfacedObject, IRegInProcCOMServers)
  protected
    { IRegInProcCOMServers }
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
  public
    constructor Create;
      {Class constructor: increments instance count}
    destructor Destroy; override;
      {Class destructor: decrements instance count}
  end;

var
  InstanceCount: Integer = 0;   // count of COM object instances
  LockCount: Integer = 0;       // count of number of locks placed on server


{ TRegCOMServers }

constructor TRegCOMServers.Create;
  {Class constructor: increments instance count}
begin
  inherited Create;
  // Increment global reference count
  InterlockedIncrement(InstanceCount);
end;

destructor TRegCOMServers.Destroy;
  {Class destructor: decrements instance count}
begin
  // Decrement global reference count
  InterlockedDecrement(InstanceCount);
  inherited Destroy;
end;

function TRegCOMServers.RegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}
begin
  Result := UCOMSvrRegProcs.RegInProcSvr(SvrName);
end;

function TRegCOMServers.UnRegInProcSvr(const SvrName: PChar): HResult;
  {Calls DllUnRegisterServer in the given COM server dll and returns the result
  code returned by that function (usually S_OK or S_FALSE on success). In case
  of error possible error codes are:
    CO_E_NOT_SUPPORTED: The DLL doesn't export DllUnRegisterServer
    CO_E_DLLNOTFOUND: The DLL could not be loaded
    Other errors codes reported by the given function (probably E_FAIL)}
begin
  Result := UCOMSvrRegProcs.UnRegInProcSvr(SvrName);
end;


{ TRegCOMServerFactory }

class function TRegCOMServerFactory.CanUnload: WordBool;
  {Returns true if server can be unloaded and false if not}
begin
  Result := (LockCount = 0) and (InstanceCount = 0);
end;

constructor TRegCOMServerFactory.Create;
  {Constructor: increments count of object instances in server}
begin
  inherited Create;
  // Increment global reference count
  InterlockedIncrement(InstanceCount);
end;

function TRegCOMServerFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HRESULT;
  {Creates a RegCOMServers object that supports given interface and passes
  it back in Obj, returning S_OK. If interface is not supported by COM
  object an error code is returned and Obj is set to nil}
var
  Unk: IUnknown;  // interface instance that we create
begin
  // Init output pointer to nil - assumes failure
  Pointer(Obj) := nil;
  // Don't support aggregation
  if UnkOuter <> nil then
  begin
    Result := CLASS_E_NOAGGREGATION;
    Exit;
  end;
  // Create instance of object and return requested interface (gives error if
  // not supported.
  Unk := CreateObj;
  Result := Unk.QueryInterface(IID, Obj);
end;

class function TRegCOMServerFactory.CreateObj: IRegInProcCOMServers;
  {Returns an RegCOMServers object instance that implements IRegInProcCOMServers
  without using CreateInstance method: for use when DLL is a private server}
begin
  Result := TRegCOMServers.Create as IRegInProcCOMServers;
end;

destructor TRegCOMServerFactory.Destroy;
  {Destructor: decrements count of object instances in server}
begin
  // Decrement global reference count
  InterlockedDecrement(InstanceCount);
  inherited Destroy;
end;

function TRegCOMServerFactory.LockServer(Lock: BOOL): HRESULT;
  {Locks and unlocks the server to prevent/permit unloading}
begin
  if Lock then
    InterlockedIncrement(LockCount)
  else
    InterlockedDecrement(LockCount);
  Result := S_OK;
end;

end.
