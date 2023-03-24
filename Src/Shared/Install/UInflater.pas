{ ##                          
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInflater.pas
  @COMMENTS                 Unit defines a class used to inflate compressed
                            data using inflator DLLs.
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
 * The Original Code is UInflater.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInflater;


interface


uses
  // Delphi
  Windows;

type

  {
  TInflater:
    Class used to inflate compressed data. The class accesses an inflator DLL to
    perform inflation. The inflator DLL must export a function named
    InflateToFile with a given prototype.

    Inheritance: TInflator -> [TObject]
  }
  TInflater = class(TObject)
  private
    fDLLFileSpec: string;
      {Full path to inflator DLL}
    fDLLHandle: THandle;
      {Handle to inflator DLL}
    fInflaterFn: function(const FileName: PChar; CompBuf: Pointer;
      CompBufSize: Integer): BOOL; cdecl;
      {Pointer to inflator function exported from inflator DLL}
    procedure LoadDLL;
      {Load the inflator DLL and get pointer to it's exported InflateToFile
      function}
  public
    constructor Create(const DLLFileSpec: string);
      {Class constructor: takes specification of DLL to be used to perform
      inflation and records it}
    destructor Destroy; override;
        {Class destructor: unload inflator DLL if it was loaded}
    function InflateToFile(const FileName: string; const CompBuf: Pointer;
      CompBufSize: Integer): Boolean;
      {Inflate the compressed data stored in CompBuf (which has size
      CompBufSize) and store the inflated data in the given FileName. Return
      true if inflation and file creation succeeds and false on error. The
      object's inflator DLL is used to perform inflation}
  end;


implementation


{ TInflater }

constructor TInflater.Create(const DLLFileSpec: string);
  {Class constructor: takes specification of DLL to be used to perform inflation
  and records it}
begin
  inherited Create;
  fDLLFileSpec := DLLFileSpec;
end;

destructor TInflater.Destroy;
  {Class destructor: unload inflator DLL if it was loaded}
begin
  if fDLLHandle <> 0 then
    FreeLibrary(fDLLHandle);
  inherited;
end;

function TInflater.InflateToFile(const FileName: string;
  const CompBuf: Pointer; CompBufSize: Integer): Boolean;
  {Inflate the compressed data stored in CompBuf (which has size CompBufSize)
  and store the inflated data in the given FileName. Return true if inflation
  and file creation succeeds and false on error. The object's inflator DLL is
  used to perform inflation}
begin
  // Assume failure
  Result := False;
  // Load DLL if not already loaded
  if fDLLHandle = 0 then
    LoadDLL;
  // Perform the inflation if we have found inflator function in inflator DLL
  if Assigned(fInflaterFn) then
    Result := fInflaterFn(PChar(FileName), CompBuf, CompBufSize);
end;

procedure TInflater.LoadDLL;
  {Load the inflator DLL and get pointer to it's exported InflateToFile
  function}
begin
  fDLLHandle := LoadLibrary(PChar(fDLLFileSpec));
  if fDLLHandle <> 0 then
    fInflaterFn := GetProcAddress(fDLLHandle, 'InflateToFile');
end;

end.
