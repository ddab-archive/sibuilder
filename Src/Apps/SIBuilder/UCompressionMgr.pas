{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UCompressionMgr.pas
  @COMMENTS                 Implements classes that encapsulate and provide
                            access to all supported plug in compressors (which
                            are defined by registry entries). The compressors'
                            deflation DLLs are also wrapped by the classes.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of Registry.inc and FileNames.inc
                              include file with URegistry and UFileNames units.
                            + Changed to load compressor information from HKLM
                              rather than HKCU in registry.
                            + Made changes to use of registry constants re
                              changes in URegistry unit.
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
 * The Original Code is UCompressionMgr.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompressionMgr;


interface


uses
  // Delphi
  SysUtils, Classes, Windows,
  // Project
  IntfCompressor;


type

  {
  TCompressorCreator:
    Function prototype for functions exported from deflator DLLs. The functions
    are used to create compressor objects.
  }
  TCompressorCreator = function(IID: TGUID; out Obj): HResult;

  {
  TCompressionItem:
    Class that encapsulates a plug in compressor. A compressor has an ID and
    description, optional credits and two associated DLLs - the inflator DLL
    used by the install programs to decompress data and the deflator DLL used
    by SIBuilder to compress data. This class can load associated deflator DLLs
    and create an object within the DLLs which can perform compression.

    Inheritance: TCompressionItem -> [TObject]
  }
  TCompressionItem = class(TObject)
  private // properties
    fID: string;
    fInflator: string;
    fDeflator: string;
    fDesc: string;
    fCredits: string;
  private
    fDLLHandle: THandle;
      {Handle to loaded deflator DLL: 0 if not loaded}
    fCreator: TCompressorCreator;
      {Pointer to compressor object creation function exported from deflator
      DLL}
    procedure LoadDLL;
      {Loads the deflator DLL associated with this compressor and references the
      CreateIntfObject exported function: should not be called if there is no
      deflator DLL. Raises exception if we can't load DLL or import function}
  public
    constructor Create(const ID, Inflator, Deflator, Desc, Credits: string);
      {Class constructor: records ID, Inflator, Deflator, Desc and Credits
      passed as parameters}
    destructor Destroy; override;
      {Class destructor: if DLL has been loaded, unload it}
    property ID: string read fID;
      {Compressor ID that is displayed in GUI}
    property Inflator: string read fInflator;
      {Fully specified file name of inflator DLL (empty string if no inflator)}
    property Deflator: string read fDeflator;
      {Fully specified file name of deflator DLL (empty string if no deflator)}
    property Desc: string read fDesc;
      {Description of the compressor}
    property Credits: string read fCredits;
      {Credits associated with compressor: may be empty string}
    function GetDeflatorObj: ICompressor;
      {Creates an instance of the compressor object contained in compressor's
      deflator DLL. Returns nil if there is no deflator DLL. Raises exception if
      we can't create object for any reason}
  end;

  {
  TCompressionMgr:
    Class that manages a list of plug in compressors as defined in registry and
    provides access to individual compressor item objects.

    Inheritance: TCompressionMgr -> [TObject]
  }
  TCompressionMgr = class(TObject)
  private // properties
    fList: TList;
    function GetItem(Idx: Integer): TCompressionItem;
  private
    procedure GetCompInfo;
      {Creates list of compressor item objects from information stored in
      registry. Also creates a "nul compressor" for use when there is no
      compression to be performed}
    procedure Clear;
      {Clears list of compression items, freeing each item object}
    procedure Add(const ID, Inflator, Deflator, Desc, Credits: string);
      {Adds a new compression item object to the compression manager: object is
      created from parameters passed to function}
  public
    constructor Create;
      {Class constructor: creates list to hold the compression item objects}
    destructor Destroy; override;
      {Class destructor: frees compression items and owned list object}
    function FindID(const ID: string): TCompressionItem;
      {Finds compression item object with given ID and returns reference to it.
      If there is no compressor with given ID then nil is returned}
    function Count: Integer;
      {Returns number of items in list of compressors}
    property Items[Idx: Integer]: TCompressionItem read GetItem; default;
      {List of compression item objects}
    procedure GetIDs(const IDs: TStrings);
      {Stores a list of compressor ID strings in the given string list}
    procedure GetDescs(const Descs: TStrings);
      {Stores a list of compressor descripton strings in the given string list}
    function IsSupported(const ID: string): Boolean;
      {Returns true if a compressor with given ID is supported (i.e. in managed
      list)}
  end;

  {
  ECompressionMgr:
    Class of exceptions raised by compression manager and compression item
    objects.

    Inheritance: ECompressionMgr -> [Exception]
  }
  ECompressionMgr = class(Exception);


implementation


uses
  // Delphi
  Registry,
  // Project
  UFileProcs, UFileNames, URegistry;


{ TCompressionMgr }

resourcestring
  // Descriptions
  sNulCompDesc = 'No compression';
  sNulCompID = '(none)';

procedure TCompressionMgr.Add(const ID, Inflator, Deflator, Desc,
  Credits: string);
  {Adds a new compression item object to the compression manager: object is
  created from parameters passed to function}
begin
  fList.Add(
    TCompressionItem.Create(ID, Inflator, Deflator, Desc, Credits)
  );
end;

procedure TCompressionMgr.Clear;
  {Clears list of compression items, freeing each item object}
var
  Idx: Integer; // loops thru compression object list
begin
  // Free all compression item objects in list
  for Idx := Pred(fList.Count) downto 0 do
    GetItem(Idx).Free;
  // Clear the list
  fList.Clear;
end;

function TCompressionMgr.Count: Integer;
  {Returns number of items in list of compressors}
begin
  Result := fList.Count;
end;

constructor TCompressionMgr.Create;
  {Class constructor: creates list to hold the compression item objects}
begin
  inherited;
  fList := TList.Create;
  GetCompInfo;
end;

destructor TCompressionMgr.Destroy;
  {Class destructor: frees compression items and owned list object}
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

function TCompressionMgr.FindID(const ID: string): TCompressionItem;
  {Finds compression item object with given ID and returns reference to it. If
  there is no compressor with given ID then nil is returned}
var
  Idx: Integer; // loops thru list of compressors
begin
  // Assume can't find ID
  Result := nil;
  // Search for ID
  for Idx := 0 to Pred(Count) do
  begin
    if AnsiCompareText(ID, GetItem(Idx).ID) = 0 then
    begin
      Result := GetItem(Idx);
      Break;
    end;
  end;
end;

procedure TCompressionMgr.GetCompInfo;
  {Creates list of compressor item objects from information stored in registry.
  Also creates a "nul compressor" for use when there is no compression to be
  performed}
var
  Reg: TRegistry;     // object used to access registry
  Keys: TStringList;  // list of compressor IDs from registry
  Idx: Integer;       // loops through list of IDs
  ID: string;         // ID of a compressor
  Inflator: string;   // Compressor's inflator DLL name
  Deflator: string;   // Compressor's deflator DLL name
  Desc: string;       // Compressor's description
  Credits: string;    // Compressor's optional credits
begin
  // We first make sure we have a compressor item for the nul compressor
  // (used when no compression is required: no DLLs)
  Add(sNulCompID, '', '', sNulCompDesc, '');
  // Open registry
  Reg := TRegistry.Create;
  Keys := nil;
  try
    // Read compressor IDs into string list (IDs are subkeys)
    Keys := TStringList.Create;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly(cSIBCompressionKey);
    Reg.GetKeyNames(Keys);
    // Loop through all compressor IDs
    for Idx := 0 to Pred(Keys.Count) do
    begin
      // Record ID
      ID := Keys[Idx];
      // Open sub key for compressor
      if Reg.OpenKey(cSIBCompressionKey + '\' + ID, False) then
      begin
        // Read compressor info from registry (under ID's key)
        Desc := Reg.ReadString('');
        Inflator := Reg.ReadString('Inflator');
        Deflator := Reg.ReadString('Deflator');
        // credits value is optional, so we need to check it exists
        if Reg.ValueExists('Credits') then
          Credits := Reg.ReadString('Credits')
        else
          Credits := '';
        // Create a compressor item as add to the list
        Add(ID, Inflator, Deflator, Desc, Credits);
      end;
    end;
  finally
    FreeAndNil(Keys);
    FreeAndNil(Reg);
  end;
end;

procedure TCompressionMgr.GetDescs(const Descs: TStrings);
  {Stores a list of compressor descripton strings in the given string list}
var
  Idx: Integer; // loops thru all compressor items in list
begin
  // Clear the given string list
  Descs.Clear;
  // Add description of each compressor to list
  for Idx := 0 to Pred(Count) do
    Descs.Add(GetItem(Idx).Desc);
end;

procedure TCompressionMgr.GetIDs(const IDs: TStrings);
  {Stores a list of compressor ID strings in the given string list}
var
  Idx: Integer; // loops thru all compressor items in list
begin
  // Clear the given string list
  IDs.Clear;
  // Add ID of each compressor to list
  for Idx := 0 to Pred(Count) do
    IDs.Add(GetItem(Idx).ID);
end;

function TCompressionMgr.GetItem(Idx: Integer): TCompressionItem;
  {Read access method for Items property}
begin
  Result := TCompressionItem(fList[Idx]);
end;

function TCompressionMgr.IsSupported(const ID: string): Boolean;
  {Returns true if a compressor with given ID is supported (i.e. in managed
  list)}
begin
  Result := Assigned(FindID(ID));
end;


{ TCompressionItem }

resourcestring
  // Error messages
  sCantCreateCompressor = 'Can''t create compressor in %s';
  sCantImportFn = 'Can''t access %0:s in %1:s';
  sCantLoadDLL = 'Can''t load %s';

const
  // name of function exported from deflator DLL that creates compressor objects
  cObjCreatorFn = 'CreateIntfObject';

constructor TCompressionItem.Create(const ID, Inflator, Deflator,
  Desc, Credits: string);
  {Class constructor: records ID, Inflator, Deflator, Desc and Credits passed as
  parameters}
begin
  inherited Create;
  // Record ID, Desc and Credits unchanged
  fID := ID;
  fDesc := Desc;
  fCredits := Credits;
  // Record inflator and deflator DLLs
  //   if DLL is provided without a path the program's install path is prepended
  // record inflator
  if (Inflator <> '') and (ExtractFileDir(Inflator) = '') then
    fInflator := MakePathName(ExtractFilePath(ParamStr(0)) + cInstallFolder)
      + Inflator
  else
    fInflator := Inflator;
  // record deflator
  if (Deflator <> '') and (ExtractFileDir(Deflator) = '') then
    fDeflator := MakePathName(ExtractFilePath(ParamStr(0)) + cInstallFolder)
      + Deflator
  else
    fDeflator := Deflator;
end;

destructor TCompressionItem.Destroy;
  {Class destructor: if DLL has been loaded, unload it}
begin
  if fDLLHandle <> 0 then
    FreeLibrary(fDLLHandle);
  inherited;
end;

function TCompressionItem.GetDeflatorObj: ICompressor;
  {Creates an instance of the compressor object contained in compressor's
  deflator DLL. Returns nil if there is no deflator DLL. Raises exception if we
  can't create object for any reason}
begin
  if fDeflator <> '' then
  begin
    // We have a compressor DLL: load it and get object
    // load DLL
    LoadDLL;
    // create compressor object and return it
    if Failed(fCreator(ICompressor, Result)) then
      raise ECompressionMgr.CreateFmt(sCantCreateCompressor, [fDeflator]);
  end
  else
    // No DLL (implies no compression): return nil
    Result := nil;
end;

procedure TCompressionItem.LoadDLL;
  {Loads the deflator DLL associated with this compressor and references the
  CreateIntfObject exported function: should not be called if there is no
  deflator DLL. Raises exception if we can't load DLL or import function}
begin
  // Check we haven't already loaded DLL: nothing to do if so
  if fDLLHandle = 0 then
  begin
    // Try to load DLL
    fDLLHandle := LoadLibrary(PChar(fDeflator));
    if fDLLHandle <> 0 then
    begin
      // DLL loaded: now try to import required function
      fCreator := GetProcAddress(fDLLHandle, cObjCreatorFn);
      if not Assigned(fCreator) then
        // failed to import function
        raise ECompressionMgr.CreateFmt(
          sCantImportFn, [fDeflator, cObjCreatorFn]
        );
    end
    else
      // failed to load DLL
      raise ECompressionMgr.CreateFmt(sCantLoadDLL, [fDeflator]);
  end;
end;

end.
