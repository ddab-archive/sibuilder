{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegTemplates.pas
  @COMMENTS                 Implements a set of classes that encapsulate and
                            manipulate persistent registry templates and macros
                            that are defined in the registry.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of Registry.inc include file with
                              URegistry unit.
                            + Changed to read registry template info from HKLM
                              instead of HKCU in registry.
                            + Changed to read constant registry macro info from
                              HKLM in registry rather than HKCU while leaving
                              user-configurable info in HKCU.
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
 * The Original Code is URegTemplates.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegTemplates;


interface


uses
  // Delphi
  Classes,
  // Project
  UObjList;

type

  {
  TRegMacroItem:
    Encapsulates a registry macro (used in registry key templates). The macro
    item has a name, description and a list of possible values. The object is
    persistent: it reads its properties from the registry on creation and saves
    any changes to the registry when destroyed.

    Inheritance: TRegMacroItem -> TObjListItem -> [TObject]
  }
  TRegMacroItem = class(TObjListItem)
  private // properties
    fName: string;
    fDesc: string;
    fValues: TStrings;
    fCurrentIdx: Integer;
    function GetCurrentIdx: Integer;
    procedure SetCurrentIdx(Value: Integer);
    function GetCurrentValue: string;
    procedure SetCurrentValue(const Value: string);
  private
    procedure AdjustValueIndex(var Index: Integer);
      {A given index into the value list is adjusted to fall in range of valid
      value indexes or to be -1 to denote no current value (the name of the
      macro is taken as value in this case}
  public
    constructor Create(const AName: string);
      {Class constructor: constructs a registry macro with given name. Owned
      Values object is created and properties are read from registry}
    destructor Destroy; override;
      {Class destructor: writes properties to registry and frees owned object}
    property Name: string read fName;
      {Name of the macro: this is always in form [Name]}
    property Desc: string read fDesc write fDesc;
      {Description of macro: used in hints}
    property Values: TStrings read fValues;
      {List of values associated with this macro: these values are used to offer
      to user in menus etc}
    property CurrentIdx: Integer read GetCurrentIdx write SetCurrentIdx;
      {Index of current value or -1 if there is no current value}
    property CurrentValue: string read GetCurrentValue write SetCurrentValue;
      {The current value. This is the value used to replace the macro in
      templates. If CurrentIdx >= 0 CurrentValue is the value at that index in
      the value list. If CurrentIdx = -1 then CurrentValue is the macro name.
      Setting a current value updates CurrentIdx. If the value set is not in
      value list the value is added to the list}
  end;

  {
  TRegMacroList:
    Maintains a list of registry macro objects and reads the list from the
    registry. Additional macro objects should not be added to the list (the list
    of macro names is not written back to registry).

    Inheritance: TRegMacroList -> TObjList -> [TObject]
  }
  TRegMacroList = class(TObjList)
  private // properties
    function GetItem(Idx: Integer): TRegMacroItem;
  private
    function MacroNameFinder(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Callback method used to compare registry macro object's name with name
      pointed to by Ptr. The method is called by FindObject when searching for
      a macro with a given name}
  public
    constructor Create; reintroduce;
      {Class constructor: creates a list of macro items which get their
      properties from registry}
    function FindMacro(const MacroName: string): TRegMacroItem;
      {Searches list for macro with given name and returns reference to it
      object if it exists, or nil if not found}
    property Items[Idx: Integer]: TRegMacroItem read GetItem; default;
      {List of registry macro objects}
  end;

  {
  TRegTemplateItem:
    Encapsulates and registry template. The object is read only and gets the
    template associated with its name from registry.

    Inheritance: TRegTemplateItem -> TObjListItem -> [TObject]
  }
  TRegTemplateItem = class(TObjListItem)
  private // properties
    fValue: string;
    fName: string;
  public
    constructor Create(const AName: string);
      {Class constructor: records template name and reads value from registry}
    property Name: string read fName;
      {Descriptive name of template: this is presented to user}
    property Value: string read fValue;
      {Value of template: i.e. the template contents - which may include
      registry macros}
  end;

  {
  TRegTemplateList:
    Maintains a list og registry templates as defined in the registry. The list
    should not be added to since the object does not update the registry.

    Inheritance: TRegTemplateList -> TObjList -> [TObject]
  }
  TRegTemplateList = class(TObjList)
  private // properties
    function GetItem(Idx: Integer): TRegTemplateItem;
  private
    function TemplateNameFinder(const AnObject: TObjListItem;
      const Ptr: Pointer): Boolean;
      {Callback method used to detect in name of given template object matches
      the name pointer to by Ptr. This method is called by FindObject when
      searching for a named template}
  public
    constructor Create; reintroduce;
      {Class constructor: reads template names from registry and creates a
      registry template item for each one and adds it to the list}
    function FindTemplate(const TemplateName: string): TRegTemplateItem;
      {Searches list for a template object with given name and returns reference
      to it if found, otherwise returns nil}
    property Items[Idx: Integer]: TRegTemplateItem read GetItem; default;
      {List of template objects}
  end;


implementation


uses
  // Delphi
  SysUtils, Registry, Windows,
  // Project
  URegistry;


{ TRegMacroItem }

procedure TRegMacroItem.AdjustValueIndex(var Index: Integer);
  {A given index into the value list is adjusted to fall in range of valid value
  indexes or to be -1 to denote no current value (the name of the macro is taken
  as value in this case}
begin
  if Index < -1 then
    Index := -1
  else if Index >= fValues.Count then
    Index := fValues.Count - 1;
end;

constructor TRegMacroItem.Create(const AName: string);
  {Class constructor: constructs a registry macro with given name. Owned Values
  object is created and properties are read from registry}
var
  Reg: TRegistry;   // accesses registry
  RegKey: string;   // registry key for this macro
  Idx: Integer;     // loops thru stored values
  Count: Integer;   // number of stored values
begin
  inherited Create;
  // Record name
  fName := AName;
  // Intialise properties: assumed no current value and no values at all!
  fCurrentIdx := -1;
  Count := 0;
  // Create Values object
  fValues := TStringList.Create;
  // Read properties from registry
  RegKey := cSIBRegVarsKey + '\' + AName;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(RegKey) then
    begin
      // read description of macro from HKLM
      fDesc := Reg.ReadString('');
      // read updatable values from HKCU
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(RegKey) then
      begin
        // get number of values
        if Reg.ValueExists('Count') then
          Count := Reg.ReadInteger('Count');
        // read in all values
        for Idx := 0 to Pred(Count) do
          fValues.Add(Reg.ReadString(Format('Value %d', [Idx])));
        // get index of current value
        if Reg.ValueExists('Current') then
          SetCurrentIdx(Reg.ReadInteger('Current'));
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

destructor TRegMacroItem.Destroy;
  {Class destructor: writes properties to registry and frees owned object}
var
  Reg: TRegistry;   // accesses registry
  Idx: Integer;     // loops thru all values
begin
  // Open this macro's key in registry
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(cSIBRegVarsKey + '\' + fName, True) then
    begin
      // Write out properties
      Reg.WriteInteger('Current', fCurrentIdx);
      Reg.WriteInteger('Count', fValues.Count);
      for Idx := 0 to Pred(fValues.Count) do
        Reg.WriteString(Format('Value %d', [Idx]), fValues[Idx]);
    end;
  finally
    FreeAndNil(Reg);
    // Free owned Values object
    FreeAndNil(fValues);
  end;
  inherited;
end;

function TRegMacroItem.GetCurrentIdx: Integer;
  {Read access method for CurrentIndex property: ensures the index property
  is within range}
begin
  AdjustValueIndex(fCurrentIdx);
  Result := fCurrentIdx;
end;

function TRegMacroItem.GetCurrentValue: string;
  {Read access method for CurrentValue property: returns current value if
  CurrentIdx is in range of values or name of macro id CurrentIdx = -1}
begin
  if GetCurrentIdx = -1 then
    Result := fName
  else
    Result := fValues[fCurrentIdx];
end;

procedure TRegMacroItem.SetCurrentIdx(Value: Integer);
  {Write access method for CurrentIdx property: ensures new value is within
  range of values or is -1}
begin
  AdjustValueIndex(Value);
  fCurrentIdx := Value;
end;

procedure TRegMacroItem.SetCurrentValue(const Value: string);
  {Write access method for CurrentValue property. If value is not in list it is
  added. If value is same as macro name then CurrentIdx is set to -1 otherwise
  it is set to the index of the value in the list}
var
  Idx: Integer;
begin
  if AnsiCompareText(Value, Name) <> 0 then
  begin
    Idx := Self.Values.IndexOf(Value);
    if Idx = -1 then
      Idx := Self.Values.Add(Value);
    SetCurrentIdx(Idx);
  end
  else
    SetCurrentIdx(-1);
end;


{ TRegMacroList }

constructor TRegMacroList.Create;
  {Class constructor: creates a list of macro items which get their properties
  from registry}
var
  Reg: TRegistry;           // accesses registry
  MacroNames: TStringList;  // list of macro names from registry
  Idx: Integer;             // loops thru macros
begin
  inherited CreateForClass(Owner, TRegMacroItem);
  // Read registry macro names from registry
  MacroNames := TStringList.Create;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(cSIBRegVarsKey) then
        Reg.GetKeyNames(MacroNames);
    finally
      FreeAndNil(Reg);
    end;
    // Create a registry macro object for each name and add to list
    for Idx := 0 to Pred(MacroNames.Count) do
      Add(TRegMacroItem.Create(MacroNames[Idx]));
  finally
    FreeAndNil(MacroNames);
  end;
end;

function TRegMacroList.FindMacro(const MacroName: string): TRegMacroItem;
  {Searches list for macro with given name and returns reference to it object
  if it exists, or nil if not found}
begin
  Result := FindObject(MacroNameFinder, PChar(MacroName)) as TRegMacroItem;
end;

function TRegMacroList.GetItem(Idx: Integer): TRegMacroItem;
  {Read access method for Items property}
begin
  Result := inherited GetObject(Idx) as TRegMacroItem;
end;

function TRegMacroList.MacroNameFinder(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
  {Callback method used to compare registry macro object's name with name
  pointed to by Ptr. The method is called by FindObject when searching for
  a macro with a given name}
begin
  Result := (AnsiCompareText(PChar(Ptr), (AnObject as TRegMacroItem).Name) = 0);
end;


{ TRegTemplateItem }

constructor TRegTemplateItem.Create(const AName: string);
  {Class constructor: records template name and reads value from registry}
var
  Reg: TRegistry; // accesses regsitry
begin
  inherited Create;
  // Record name
  fName := AName;
  // Initialise value in case of read failure
  fValue := '';
  // Read value from registry
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(cSIBRegTpltKey + '\' + AName) then
      if Reg.ValueExists('') then
        fValue := Reg.ReadString('');
  finally
    FreeAndNil(Reg);
  end;
end;


{ TRegTemplateList }

constructor TRegTemplateList.Create;
  {Class constructor: reads template names from registry and creates a registry
  template item for each one and adds it to the list}
var
  Reg: TRegistry;           // accesses registry
  Templates: TStringList;   // list of names of templates
  Idx: Integer;             // loops thru template names
begin
  inherited CreateForClass(Owner, TRegTemplateItem);
  // Read list of template names from registry
  Templates := TStringList.Create;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(cSIBRegTpltKey) then
        Reg.GetKeyNames(Templates);
    finally
      FreeAndNil(Reg);
    end;
    // Create a template item for each name and add to list
    for Idx := 0 to Pred(Templates.Count) do
      Add(TRegTemplateItem.Create(Templates[Idx]));
  finally
    FreeAndNil(Templates);
  end;
end;

function TRegTemplateList.FindTemplate(
  const TemplateName: string): TRegTemplateItem;
  {Searches list for a template object with given name and returns reference to
  it if found, otherwise returns nil}
begin
  Result := FindObject(
    TemplateNameFinder, PChar(TemplateName)
  ) as TRegTemplateItem;
end;

function TRegTemplateList.GetItem(Idx: Integer): TRegTemplateItem;
  {Read access method for Items property}
begin
  Result := inherited GetObject(Idx) as TRegTemplateItem;
end;

function TRegTemplateList.TemplateNameFinder(const AnObject: TObjListItem;
  const Ptr: Pointer): Boolean;
  {Callback method used to detect in name of given template object matches the
  name pointer to by Ptr. This method is called by FindObject when searching for
  a named template}
begin
  Result := (
    AnsiCompareText(PChar(Ptr), (AnObject as TRegTemplateItem).Name) = 0
  );
end;

end.
