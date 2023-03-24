{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     USettings.pas
  @COMMENTS                 This unit is unique to the SIBuilder.exe
                            sub-project. It contains a class that stores and
                            reads persistant program settings using registry.
  @DEPENDENCIES             The following DLL is required
                            + PJSoftUtils.dll in the PJSoft sub-folder of the
                              Windows directory. The file PJSoftUtils.inc is
                              also required on the search path. \
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 04/04/2000
      @COMMENTS             Added support for reading Website address from
                            registry.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 28/08/2000
      @COMMENTS             Now gets PJSoft website address from PJSoftUtils.dll
                            rather than registry.\
                            Added support for new settings:
                            + ShowSideBar
                            + ShowInfoBar
                            + InfoPaneColour \
                            Used constants from .inc file for default values
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 20/12/2000
      @COMMENTS             + Made to check if version of Delphi read from
                              registry is valid and if not use default version.
                            + Added new CompilerDisplayStyle setting which
                              determines how command line compiler is displayed.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added property for installation report kinds
                            + Changed DelphiVersion property to Compiler (to
                              support new stand-alone packager in addition to
                              Delphi compilers)
                            + Deleted CompilerDisplay property: no longer
                              supported.
                            + Deleted ShowSideBar option: no longer supported
                            + Added routines to read strings, integers and
                              booleans from registry which test for presence of
                              the data items and return default values when
                              value is not present, rather than raising
                              exceptions as was done previously.
                            + Deleted constructor since this was used to set
                              default values (which are now set in new methods).
                              This meant that default values set in constructor
                            + Removed code use to swallow exception in registry
                              reading routine - they are no longer expected.
                            + Removed website name property - no longer
                              required.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 25/03/2007
      @COMMENTS             + Removed reference to UDCC32Info unit.
                            + Removed TSettings.Compiler property and all
                              supporting code.
                            + Moved include statement for Registry.inc from
                              interface to implementation.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 23/02/2008
      @COMMENTS             + Replaced usage of Registry.inc include file with
                              URegistry unit.
                            + Made changes to use of constants flowing from
                              changes in URegistry unit.
                            + Changed use values in new Options registry subkey.
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
 * The Original Code is USettings.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USettings;


interface


uses
  // Delphi
  Graphics, Registry;


type

  {
  TSettings:
    Class that stores and reads persistant program settings using registry.
  }
  TSettings = class(TObject)
  private // properties
    fShowWelcome: Boolean;
    fShowInfoPane: Boolean;
    fInfoPaneColour: TColor;
    fReportKind: Integer;
    fDefaultCompressor: string;
    fInfoWindows: LongWord;
    function GetShowWelcome: Boolean;
    function GetInfoPaneColour: TColor;
    function GetShowInfoPane: Boolean;
    function GetReportKind: Integer;
    function GetDefaultCompressor: string;
    function GetInfoWindows: LongWord;
  private
    fRegistryRead: Boolean;
      {Flag set false if the registry has not yet been read, and true once
      values have been read in}
    function ReadBool(Reg: TRegistry; const Name: string;
      Def: Boolean): Boolean;
      {Reads boolean value with given name from current registry key and returns
      it. If value does not exist the given default value is returned}
    function ReadInteger(Reg: TRegistry; const Name: string;
      Def: Integer): Integer;
      {Reads integer value with given name from current registry key and returns
      it. If value does not exist the given default value is returned}
    function ReadString(Reg: TRegistry; const Name, Def: string): string;
      {Reads string value with given name from current registry key and returns
      it. If value does not exist the given default value is returned}
    procedure ReadRegistry;
      {Reads all required values from the registry}
  public
    destructor Destroy; override;
      {Class destructor: updates registry with current variable settings}
    property ShowWelcome: Boolean
      read GetShowWelcome write fShowWelcome;
      {Welcome screen is displayed when this flag is true}
    property ShowInfoPane: Boolean
      read GetShowInfoPane write fShowInfoPane;
      {Whether the info pane is to be displayed}
    property InfoPaneColour: TColor
      read GetInfoPaneColour write fInfoPaneColour;
      {The colour of the info pane when displayed}
    property ReportKind: Integer
      read GetReportKind write fReportKind;
      {The kind of installation report to write}
    property DefaultCompressor: string
      read GetDefaultCompressor write fDefaultCompressor;
      {The id of the compressor to use by default. If stored (or default)
      compressor is not available, the (none) compressor is returned}
    property InfoWindows: LongWord
      read GetInfoWindows write fInfoWindows;
      {Bitmask that determines which popup info windows are to be displayed}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  UCompressionMgr, URegistry;


{ TSettings }

destructor TSettings.Destroy;
  {Class destructor: updates registry with current variable settings}
var
  Reg: TRegistry;   // registry object
begin
  // Open registry and move to required key
  Reg := TRegistry.Create;
  try
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(cSIBOptionsKey, True);
      // Output the data values
      Reg.WriteBool(cSIBShowWelcome, fShowWelcome);
      Reg.WriteBool(cSIBShowInfoPane, fShowInfoPane);
      Reg.WriteInteger(cSIBInfoPaneColour, Integer(fInfoPaneColour));
      Reg.WriteInteger(cSIBReportKind, fReportKind);
      Reg.WriteString(cSIBDefaultCompressor, fDefaultCompressor);
      Reg.WriteInteger(cSIBInfoWindows, fInfoWindows);
    finally
      // Close the registry
      FreeAndNil(Reg);
    end;
  except
    // Just swallow exception to ensure inherited destructor gets called
  end;
  inherited Destroy;
end;

function TSettings.GetDefaultCompressor: string;
  {Read access method for DefaultCompressor property: reads registry if value
  not yet retrieved. If the default compressor is not supported on this
  installation then the nul compressor ('(none)') is returned}
var
  CompMgr: TCompressionMgr; // instance of compression manager object
begin
  // Read registry if not yet accessed
  if not fRegistryRead then
    ReadRegistry;
  // Use compression manager object to validate default compressor
  CompMgr := TCompressionMgr.Create;
  try
    if CompMgr.IsSupported(fDefaultCompressor) then
      // valid compressor: return it
      Result := fDefaultCompressor
    else
      // compressor not supported: return nul compressor
      Result := '(none)';
  finally
    FreeAndNil(CompMgr);
  end;
end;

function TSettings.GetInfoPaneColour: TColor;
  {Read access method for InfoPaneColour property - reads registry if value not
  yet retrieved}
begin
  if not fRegistryRead then
    ReadRegistry;
  Result := fInfoPaneColour;
end;

function TSettings.GetInfoWindows: LongWord;
  {Read access method for InfoWindows property: reads registry if value not yet
  retrieved}
begin
  if not fRegistryRead then
    ReadRegistry;
  Result := fInfoWindows;
end;

function TSettings.GetReportKind: Integer;
  {Read access method for ReportKind property: reads registry if value not yet
  retrieved}
begin
  if not fRegistryRead then
    ReadRegistry;
  Result := fReportKind;
end;

function TSettings.GetShowInfoPane: Boolean;
  {Read access method for ShowInfoPane property - reads registry if value not
  yet retrieved}
begin
  if not fRegistryRead then
    ReadRegistry;
  Result := fShowInfoPane;
end;

function TSettings.GetShowWelcome: Boolean;
  {Read access method for ShowWelcome property - reads registry if value not yet
  retrieved}
begin
  if not fRegistryRead then
    ReadRegistry;
  Result := fShowWelcome;
end;

function TSettings.ReadBool(Reg: TRegistry; const Name: string;
  Def: Boolean): Boolean;
  {Reads boolean value with given name from current registry key and returns it.
  If value does not exist the given default value is returned}
begin
  if Reg.ValueExists(Name) then
    Result := Reg.ReadBool(Name)
  else
    Result := Def;
end;

function TSettings.ReadInteger(Reg: TRegistry; const Name: string;
  Def: Integer): Integer;
  {Reads integer value with given name from current registry key and returns it.
  If value does not exist the given default value is returned}
begin
  if Reg.ValueExists(Name) then
    Result := Reg.ReadInteger(Name)
  else
    Result := Def;
end;

procedure TSettings.ReadRegistry;
  {Reads all required values from the registry}
var
  Reg: TRegistry;   // registry object
begin
  // Flag that registry now read regardles of we succeed or not - if this call
  // fails we don't want to keep trying - we'll just use default values instead
  fRegistryRead := True;
  // set default values
  fShowWelcome := cSIBShowWelcomeDef;
  fShowInfoPane := cSIBShowInfoPaneDef;
  fInfoPaneColour := TColor(cSIBInfoPaneColourDef);
  fReportKind := cSIBReportKindDef;
  fDefaultCompressor := cSIBDefaultCompressorDef;
  fInfoWindows := cSIBInfoWindowsDef;
  // Open registry
  Reg := TRegistry.Create;
  try
    // Read entries maintained for current user
    // move to current user's root key
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(cSIBOptionsKey, True) then
    begin
      // read values, getting defaults if values not present
      fShowWelcome := ReadBool(Reg, cSIBShowWelcome, cSIBShowWelcomeDef);
      fShowInfoPane := ReadBool(Reg, cSIBShowInfoPane, cSIBShowInfoPaneDef);
      fInfoPaneColour := TColor(
        ReadInteger(Reg, cSIBInfoPaneColour, cSIBInfoPaneColourDef)
      );
      fReportKind := ReadInteger(Reg, cSIBReportKind, cSIBReportKindDef);
      fDefaultCompressor := ReadString(
        Reg, cSIBDefaultCompressor, cSIBDefaultCompressorDef
      );
      fInfoWindows := ReadInteger(
        Reg, cSIBInfoWindows, Integer(cSIBInfoWindowsDef)
      );
      // close key
      Reg.CloseKey;
    end;
  finally
    // Close registry
    FreeAndNil(Reg);
  end;
end;

function TSettings.ReadString(Reg: TRegistry; const Name, Def: string): string;
  {Reads string value with given name from current registry key and returns it.
  If value does not exist the given default value is returned}
begin
  if Reg.ValueExists(Name) then
    Result := Reg.ReadString(Name)
  else
    Result := Def;
end;

end.

