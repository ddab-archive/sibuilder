{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UWarner.pas
  @COMMENTS                 Implements a class that is used to count errors
                            associated with objects, display hotlinked rich text
                            messages when one or more errors are recorded, and
                            hide the text if there are no errors.
  @DEPENDENCIES             The following custom SIBuilder only components are
                            required:
                            + THotText 1.0\
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 19/11/2003
      @COMMENTS             + Modified to work with THotText controls to display
                              messages rather than rich text controls and active
                              RTF classes.
                            + Removed reference to removed UActiveRTFWrapper
                              unit.
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
 * The Original Code is UWarner.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UWarner;


interface


uses
  // Delphi
  Classes, Windows, ComCtrls,
  // Project specific components
  CmpHotText;


type

  {
  TWarner:
    Class that is used to count errors associated with object, display hotlinked
    messages when one or more errors are recorded, and hide the text if there
    are no errors.

    Inheritance: TWarner -> [TObject]
  }
  TWarner = class(TObject)
  private
    fHotText: THotText;
      {Hot text control in which to display warning message with hot links}
    fResID: Integer;
      {ID of the RC_DATA resource containing the hot text code to be displayed
      as warning}
    fWarnings: TList;
      {List of objects for which warnings have been registered}
    procedure LoadWarning;
      {Loads the warning text from resoruces and displays it}
    procedure ClearWarning;
      {Clears the warning display and hot links}
  public
    constructor Create(WarningCtrl: THotText; ResID: Integer);
      {Class constructor: records reference to associated hot text control and
      resource id of hot text coed. Creates owned hot linker and lits
      objects and sets object to default "no warnings" state}
    destructor Destroy; override;
      {Class destructor: clears any warning and frees owned objects}
    procedure RegisterWarning(WarnObj: TObject);
      {Registers that a warning exists for the given object: if object is
      already registered no action is taken. Unregistered objects are registered
      and, if this is this first registered object, the warning is displayed}
    procedure UnregisterWarning(WarnObj: TObject);
      {Unregisters any warning for given object. If object is not registered it
      is ignored. If object is registered its registration is removed. If this
      was last registered object the warning message is hidden}
    procedure Clear;
      {Clears all registered objects and any displayed warning message}
  end;


implementation


uses
  // Project
  UCmdDispatcher;


{ TWarner }

procedure TWarner.Clear;
  {Clears all registered objects and any displayed warning message}
begin
  fWarnings.Clear;
  ClearWarning;
end;

procedure TWarner.ClearWarning;
  {Clears the warning display and hot links}
begin
  fHotText.Code := '';
end;

constructor TWarner.Create(WarningCtrl: THotText; ResID: Integer);
  {Class constructor: records reference to associated hot text control and
  resource id of hot text coed. Creates owned hot linker and lits objects and
  sets object to default "no warnings" state}
begin
  inherited Create;
  // Record reference to params
  fHotText := WarningCtrl;
  fResID := ResID;
  fHotText.OnLinkClick := TCmdDispatcher.HotLinkClickHandler;
  // Create list to store reference to objects for which warnings are registered
  fWarnings := TList.Create;
  // Ensure no warning is displayed
  ClearWarning;
end;

destructor TWarner.Destroy;
  {Class destructor: clears any warning and frees owned objects}
begin
  ClearWarning;
  fWarnings.Free;
  inherited;
end;

procedure TWarner.LoadWarning;
  {Loads the warning text from resoruces and displays it}
begin
  fHotText.LoadFromResourceID(HInstance, fResID, RT_RCDATA);
end;

procedure TWarner.RegisterWarning(WarnObj: TObject);
  {Registers that a warning exists for the given object: if object is already
  registered no action is taken. Unregistered objects are registered and, if
  this is this first registered object, the warning is displayed}
var
  Idx: Integer; // loops thru warnings list
begin
  // Check if object is already registered in list
  Idx := fWarnings.IndexOf(WarnObj);
  if Idx = -1 then
  begin
    // Object noot registered: add it to list
    fWarnings.Add(WarnObj);
    if fWarnings.Count = 1 then
      // This was first object: display message
      LoadWarning;
  end;
end;

procedure TWarner.UnregisterWarning(WarnObj: TObject);
  {Unregisters any warning for given object. If object is not registered it is
  ignored. If object is registered its registration is removed. If this was last
  registered object the warning message is hidden}
var
  Idx: Integer; // loops through all objects in warning list
begin
  // Check if object is actually registerd: ignored if not
  Idx := fWarnings.IndexOf(WarnObj);
  if Idx > -1 then
  begin
    // If this is last warning we clear the list
    if fWarnings.Count = 1 then
      ClearWarning;
    // Delete the registered object from list
    fWarnings.Delete(Idx);
  end;
end;

end.
