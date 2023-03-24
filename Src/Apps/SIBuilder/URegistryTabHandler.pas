{ ##            
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URegistryTabHandler.pas
  @COMMENTS                 TTabSheetHandler descendant that provides the
                            required additional functionality for the "registry"
                            tab sheet. Also implements two information popup
                            window classes to provide information about selected
                            registry keys and data items.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 24/12/2000
      @COMMENTS             + Adapted methods that call registry data item edit
                              dlg box to use new TRegData.ExpandMacroPaths
                              property in addition to others.
                            + Replaced all error message string literals with
                              resource strings.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added new context menu to treeview that replicates
                              buttons.
                            + Added ability to display relevant edit dialog when
                              a node is double clicked (providing node has no
                              children).
                            + Replaced string literals with resource strings and
                              moved non-localisable string literals to
                              constants.
                            + Made tree view expand automaticaly when tab sheet
                              is displayed.
                            + Nilled all event handlers when class freed.
                            + Added optional popup window for registry key and
                              data items that gives details of the items. New
                              classes were added to unit to implement these
                              windows.
                            + Added support for optional registry key deletion
                              when key is empty. When a new key is added, its
                              default Deletable value is same as parent's unless
                              parent is root key, when default is to delete if
                              empty.
                            + Now uses UImageList unit rather than ImageList.inc
                              include file.
                            + Added new helper routine to set data safefly,
                              restoring old values on error.
                            + Added new code to import and export registry data
                              from and to reg edit files.
                            + Added facility to create some predefined registry
                              keys from templates.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 17/07/2006
      @COMMENTS             + Fixed bug in SetData helper routine that caused
                              valid binary data entries to be rejected as
                              invalid if entered via manual data entry dialog.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 19/02/2008
      @COMMENTS             Replaced usage of Registry.inc and ResiIds.inc
                            include files with URegistry and UResources units.
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
 * The Original Code is URegistryTabHandler.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URegistryTabHandler;


interface


uses
  // Delphi
  ComCtrls, Controls, Classes,
  // Project
  UTabSheetHandler, URegKeys, URegData, UInfoWdw;

type

  {
  TRegDataWdw:
    Implements an info window that displays the information from a registry data
    object.

    Inheritance: TRegDataWdw -> TInfoWdw -> [TCustomControl]
  }
  TRegDataWdw = class(TInfoWdw)
  private  // properties
    fRegData: TRegData;
    procedure SetRegData(const Value: TRegData);
  public
    property RegData: TRegData read fRegData write SetRegData;
      {The registry data object for which this window is to display information.
      Setting this property updates the window's contents}
  end;

  {
  TRegKeyWdw:
    Implements an info window that displays the information from a registry key
    object.

    Inheritance: TRegKeyWdw -> TInfoWdw -> [TCustomControl]
  }
  TRegKeyWdw = class(TInfoWdw)
  private // properties
    fRegKey: TRegKey;
    procedure SetRegKey(const Value: TRegKey);
  public
    property RegKey: TRegKey read fRegKey write SetRegKey;
      {The registry key object for which this window is to display information.
      Setting this property updates the window's contents}
  end;

  {
  TRegistryTabHandler:
    Encapsulates the "registry" tab sheet and provides the relevant specialised
    processing required for that tab sheet over and above the common
    functionality provided by TTabSheetHandler.

    Inheritance: TRegistryTabHandler -> TTabSheetHandler -> TObjListItem
      -> [TObject]
  }
  TRegistryTabHandler = class(TTabSheetHandler)
  private
    fDataWdw: TRegDataWdw;
      {Popup info window object used to display information about a registry
      data item}
    fKeyWdw: TRegKeyWdw;
      {Popup info window object used to display information about a registry
      key}
    procedure AddKey(Sender: TObject);
      {OnClick event handler for Add Key button: adds a new registry key}
    procedure EditKey(Sender: TObject);
      {OnClick event handler for Edit Key button: allows user to edit selected
      key}
    procedure DeleteKey(Sender: TObject);
      {OnClick event handler for Delete Key button: deletes selected key}
    procedure AddDataItem(Sender: TObject);
      {OnClick event handler for Add Data Item button: adds a user defined
      registry data item}
    procedure EditDataItem(Sender: TObject);
      {OnClick event handler for Edit Data Item button: allows user to edit
      selected registry data item}
    procedure DeleteDataItem(Sender: TObject);
      {OnClick event handler for Delete Data Item button: deletes selected data
      item}
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
      {OnChange event handler for treeview component on tab sheet: updates state
      of buttons according to selection}
    procedure TreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      {OnMouseDown event handler for registry tree view: ensures that the
      relevant tree node is selected when right mouse button is clicked and
      displays any required popup info window when left button is clicked}
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      {OnMouseMove event handler for registry tree view: determines if we are to
      display the node under cursor (if any) underlined to denote that a popup
      info window is available if the node is left clicked}
    procedure TreeDblClick(Sender: TObject);
      {OnDblClick event handler for regsitry tree view: triggers pop-up menu's
      default menu item if there is one, else does nothing}
    procedure ExportRegEditFile(Sender: TObject);
      {OnClick event handler for Export (registry editor file) button: creates
      a .reg file containing information from registry objects in project}
    procedure ImportRegEditFile(Sender: TObject);
      {OnClick event handler for Import (registry editor file) button: updates
      registry page with details contained in a .reg file}
    procedure KeyFromTemplate(Sender: TObject);
      {Adds a registry key to treeview that is based on one of the key templates
      that are accessed via dialog box}
    procedure AddRegTreeNode(const ParentNode: TTreeNode;
      const RegKey: TRegKey);
      {Adds a registry treeview node for given registry key as child of given
      parent node - and adds all the data items and sub keys of the given key}
  protected
    function InstructionsResId: Integer; override;
      {Returns the id of the resource containing the instructions that relate to
      the page}
  public
    constructor Create(const TabSheet: TTabSheet); override;
      {Class constructor: assigns OnChange handlers to tab sheet's controls}
    destructor Destroy; override;
      {Class destructor: nil all the form's event handlers that were handled by
      this class}
    procedure UpdateSheet; override;
      {Updates content of controls on tab sheet to reflect relavant project
      property values}
    procedure UpdateButtons;
      {Updates state of tab sheet's buttons depending on current state of
      controls}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Registry,
  // Project
  URegObjects, UImageList, URegRootKeys, UCommonTypes, URegEditFile,
  UBinStrings, URegUtils, UMessageBox, URegistry, UResources,
  FmMain, FmRegKeyEdit, FmRegDataItemEdit, FmRegTemplates;


resourcestring
  // Error messages
  sInvalidValue = 'This data item has no valid value.';
  sDupDataName =
    'A data item under the "%0:s" key already has the name "%1:s".';
  sDupSiblingDataName =
    'A sibling data item of "%0:s" already has the name "%1:s".';
  sDupSubKeyName = 'Another sub-key of "%0:s" already has the name "%1:s".';
  sDupSiblingKeyName = 'A sibling key of "%0:s" already has the name "%1:s".';
  // Queries
  sQueryKeyOverwriteName = 'The key:'#13'[%0:s]'#13
    + 'already contains data item named "%1:s".'#13#13'OK to overwrite?';
  sQueryKeyOverwriteDefault = 'The key:'#13'[%s]'#13
    + 'already contains a default data item.'#13#13'OK to overwrite?';

const
  // Non-localisable display names
  cDefDataDisplayName = '(default)';  // name of default values of reg keys


{ Helper routine }

function SetData(Data: TRegData; const Value: string;
  DataType: TRegDataType; ExpMacroPaths: Boolean): Boolean;
  {Sets the data and expand path macros property of the given project registry
  data item object to the given values and data type. If an error occurs when
  setting the data the item's previous value and type are restored}
var
  OldDataType: TRegDataType;  // data type of data item before change
  OldValue: string;           // value of data item before changing
  OldExpMacroPaths: Boolean;  // ExpandMacroPaths property value before changing
begin
  // Save current data type and data in case of error
  OldDataType := Data.ValueType;
  OldExpMacroPaths := Data.ExpandMacroPaths;
  try
    if OldDataType = rdUnknown then
      OldValue := ''
    else
      OldValue := Data.GetAsString;
    // Attempt to set new value
    if DataType = rdBinary then
      Data.SetAsString(rdBinary, UBinStrings.NormaliseSpaceBinStr(Value))
    else
      Data.SetAsString(DataType, Value);
    Data.ExpandMacroPaths := ExpMacroPaths;
    // Record success
    Result := True;
  except
    on E: ERegData do
    begin
      // We have error
      // record error
      Result := False;
      // restore previous data
      Data.ExpandMacroPaths := OldExpMacroPaths;
      if OldDataType <> rdUnknown then
        Data.SetAsString(OldDataType, Value);
      // display error message
      TMessageBox.Error(MainForm, E.Message);
    end;
  end;
end;


{ TRegistryTabHandler }

procedure TRegistryTabHandler.AddDataItem(Sender: TObject);
  {OnClick event handler for Add Data Item button: adds a user defined registry
  data item}
var
  Dlg: TRegDataItemEditDlg; // the dialogs box to enter data item info
  NewNode: TTreeNode;       // the new tree node to display data item
  CurrentNode: TTreeNode;   // the currently selected tree node
  DataItem: TRegData;       // the registry data itme object to add to key
  RegKey: TRegKey;          // the registry key to which data item is added
begin
  // Record reference to currently selected tree node and exit if not a reg key
  CurrentNode := MainForm.tvReg.Selected;
  if (CurrentNode = nil) or not (TRegObject(CurrentNode.Data) is TRegKey) then
    Exit;
  // Create dialogue box, align relative to form and display it
  Dlg := TRegDataItemEditDlg.Create(MainForm);
  try
    if Dlg.ShowModal = mrOK then
    begin
      // User OK'd - create data item object and associate with key
      // record reference to registry key
      RegKey := TRegKey(CurrentNode.Data);
      // check that a data item with same name doesn't already exist
      if RegKey.DataItems.FindByName(Dlg.DataName) = nil then
      begin
        // create new reg data item object: and check if it is valid
        DataItem := TRegData.Create(Dlg.DataName);
        if SetData(
          DataItem, Dlg.DataValue, Dlg.DataType, Dlg.ExpandMacroPaths
        ) then
        begin
          // data was set without error: add to key
          RegKey.DataItems.Add(DataItem);
          // add new child node to reg tree with reference to data item object
          if Dlg.DataName <> '' then
            NewNode := MainForm.tvReg.Items.AddChild(CurrentNode, Dlg.DataName)
          else
            NewNode := MainForm.tvReg.Items.AddChild(
              CurrentNode, cDefDataDisplayName
            );
          NewNode.Data := DataItem;
          // use required image for type of data item
          NewNode.ImageIndex := cNameImageMap[Ord(DataItem.ValueType)];
          NewNode.SelectedIndex := NewNode.ImageIndex;
          // expand tree so that new data item can been seen
          CurrentNode.Expand(True);
        end
        else
          // data was set in error: delete from tree
          DataItem.Free;
      end
      else
        // a data item with same name exists - tell user
        TMessageBox.ErrorFmt(
          MainForm, sDupDataName, [RegKey.Name, Dlg.DataName]
        );
    end;
  finally
    // Get rid of dialogue box
    Dlg.Free;
    // Return focus to reg tree view
    MainForm.tvReg.SetFocus;
  end;
end;

procedure TRegistryTabHandler.AddKey(Sender: TObject);
  {OnClick event handler for Add Key button: adds a new registry key}
var
  NewNode: TTreeNode;     // the tree node for new reg key
  CurrentNode: TTreeNode; // the currently selected node
  Dlg: TRegKeyEditDlg;    // instance of dialogue box
  RegKey: TRegKey;        // new registry key object
  ParentKey: TRegKey;     // ref to parent reg key object
begin
  // Record reference to currently selected tree node and exit if none
  CurrentNode := MainForm.tvReg.Selected;
  if CurrentNode = nil then
    Exit;
  // Get reference to parent key for any new node
  ParentKey := TRegKey(CurrentNode.Data);
  // Create, align & display dialogue box
  Dlg := TRegKeyEditDlg.Create(MainForm);
  try
    // Set deletable option on form to same as parent by default
    // (unless parent is root key, then use delete if empty)
    if not (ParentKey is TRegRootKey) then
      Dlg.Deletable := ParentKey.Deletable
    else
      Dlg.Deletable := rdIfNoSubKeys;
    // Show the dialog and wait for user to close
    if Dlg.ShowModal = mrOK then
    begin
      // User OK'd - add new key as child of current key
      // check parent doesn't have a subkey with same name
      if ParentKey.SubKeys.FindByName(Dlg.KeyName) = nil then
      begin
        // create new reg key object with required properties
        RegKey := TRegKey.Create(Dlg.KeyName);
        RegKey.Deletable := Dlg.Deletable;
        // add new reg key as child of parent key
        ParentKey.SubKeys.Add(RegKey);
        // create new tree node for this reg key & add as child of current node
        NewNode := MainForm.tvReg.Items.AddChild(CurrentNode, RegKey.Name);
        NewNode.Data := RegKey;
        // set image for this node
        NewNode.ImageIndex := cRegKeyNodeImageIndex;
        NewNode.SelectedIndex := cRegKeyNodeImageIndex;
        // expand parent node so we can see new node
        CurrentNode.Expand(True);
      end
      else
        // sub key with same name exists - report and don't add
        TMessageBox.ErrorFmt(
          MainForm, sDupSubKeyName, [ParentKey.Name, Dlg.KeyName]
        );
    end;
  finally
    // Get rid of dialogue box
    Dlg.Free;
    // Return focus to reg tree view
    MainForm.tvReg.SetFocus;
  end;
end;

procedure TRegistryTabHandler.AddRegTreeNode(const ParentNode: TTreeNode;
  const RegKey: TRegKey);
  {Adds a registry treeview node for given registry key as child of given parent
  node - and adds all the data items and sub keys of the given key}
var
  I: Integer;           // loops thru subkeys of given key
  NewNode: TTreeNode;   // the new tree node
  ChildNode: TTreeNode; // child nodes for values
  RegTree: TTreeView;   // reference to registry tree control
begin
  // Get reference to registry tree view
  RegTree := MainForm.tvReg;
  // Add node for registry key
  // create a new tree node as child of given parent node
  NewNode := RegTree.Items.AddChild(ParentNode, RegKey.Name);
  // record reference to registry key object in node
  NewNode.Data := RegKey;
  // display required image for a registry key in tree
  NewNode.ImageIndex := cRegKeyNodeImageIndex;
  NewNode.SelectedIndex := cRegKeyNodeImageIndex;
  // Add all subkeys of registry keys as children - recursively
  for I := 0 to RegKey.SubKeys.Count - 1 do
    AddRegTreeNode(NewNode, RegKey.SubKeys.Items[I]);
  // Add all data items for this key as children
  for I := 0 to RegKey.DataItems.Count - 1 do
  begin
    // create child node
    if RegKey.DataItems.Items[I].Name <> '' then
      ChildNode := RegTree.Items.AddChild(NewNode,
        RegKey.DataItems.Items[I].Name)
    else
      ChildNode := RegTree.Items.AddChild(NewNode, cDefDataDisplayName);
    // record reference to reg data item object related to this node
    ChildNode.Data := RegKey.DataItems.Items[I];
    // display require image for this data type
    ChildNode.ImageIndex :=
      cNameImageMap[Ord(RegKey.DataItems.Items[I].ValueType)];
    ChildNode.SelectedIndex :=
      cNameImageMap[Ord(RegKey.DataItems.Items[I].ValueType)];
  end;
end;

constructor TRegistryTabHandler.Create(const TabSheet: TTabSheet);
  {Class constructor: assigns OnChange handlers to tab sheet's controls}
begin
  inherited Create(TabSheet);
  with MainForm do
  begin
    tvReg.OnChange := TreeChange;
    tvReg.OnMouseDown := TreeMouseDown;
    tvReg.OnMouseMove := TreeMouseMove;
    tvReg.OnDblClick := TreeDblClick;
    btnAddKey.OnClick := AddKey;
    miRegAddKey.OnClick := AddKey;
    btnEditKey.OnClick := EditKey;
    miRegEditKey.OnClick := EditKey;
    btnDeleteKey.OnClick := DeleteKey;
    miRegDeleteKey.OnClick := DeleteKey;
    btnAddDataItem.OnClick := AddDataItem;
    miRegAddDataItem.OnClick := AddDataItem;
    btnEditDataItem.OnClick := EditDataItem;
    miRegEditDataItem.OnClick := EditDataItem;
    btnDeleteDataItem.OnClick := DeleteDataItem;
    miRegDeleteDataItem.OnClick := DeleteDataItem;
    btnRegExport.OnClick := ExportRegEditFile;
    btnRegImport.OnClick := ImportRegEditFile;
    btnRegTemplates.OnClick := KeyFromTemplate;
  end;
end;

procedure TRegistryTabHandler.DeleteDataItem(Sender: TObject);
  {OnClick event handler for Delete Data Item button: deletes selected data
  item}
var
  CurrentNode: TTreeNode; // the currently selected tree node
  DataItem: TRegData;     // the related registry data item object
begin
  // Record reference to selected tree node and exit if this isn't a data item
  CurrentNode := MainForm.tvReg.Selected;
  if (CurrentNode = nil) or not (TRegObject(CurrentNode.Data) is TRegData) then
    Exit;
  // Record reference to data item object associated with this tree node
  DataItem := TRegData(CurrentNode.Data);
  // Free reg object and delete node from tree view
  DataItem.Free;   // this detaches from whatever list attached to
  CurrentNode.Delete;
  // Return focus to reg tree view and update buttons
  MainForm.tvReg.SetFocus;
end;

procedure TRegistryTabHandler.DeleteKey(Sender: TObject);
  {OnClick event handler for Delete Key button: deletes selected key}
var
  CurrentNode: TTreeNode; // currently selected tree node
  RegKey: TRegKey;        // the current registry key object
begin
  // Record currently selected tree node, exiting if none selected
  CurrentNode := MainForm.tvReg.Selected;
  if CurrentNode = nil then
    Exit;
  // Record reference to related reg key obejct
  RegKey := TRegKey(CurrentNode.Data);
  // Free registry key object and delete current node from tree
  RegKey.Free;    // unlinks from it's owning list and frees owned objects
  CurrentNode.Delete;
  // Return focus to reg tree view
  MainForm.tvReg.SetFocus;
end;

destructor TRegistryTabHandler.Destroy;
  {Class destructor: nil all the form's event handlers that were handled by this
  class}
begin
  with MainForm do
  begin
    tvReg.OnChange := nil;
    tvReg.OnMouseDown := nil;
    tvReg.OnDblClick := nil;
    btnAddKey.OnClick := nil;
    miRegAddKey.OnClick := nil;
    btnEditKey.OnClick := nil;
    miRegEditKey.OnClick := nil;
    btnDeleteKey.OnClick := nil;
    miRegDeleteKey.OnClick := nil;
    btnAddDataItem.OnClick := nil;
    miRegAddDataItem.OnClick := nil;
    btnEditDataItem.OnClick := nil;
    miRegEditDataItem.OnClick := nil;
    btnDeleteDataItem.OnClick := nil;
    miRegDeleteDataItem.OnClick := nil;
    btnRegExport.OnClick := nil;
    btnRegImport.OnClick := nil;
    btnRegTemplates.OnClick := nil;
  end;
  inherited;
end;

procedure TRegistryTabHandler.EditDataItem(Sender: TObject);
  {OnClick event handler for Edit Data Item button: allows user to edit selected
  registry data item}
var
  Dlg: TRegDataItemEditDlg; // instance of dialogue box for editing
  CurrentNode: TTreeNode;   // currently selected registry tree
  DataItem: TRegData;       // currently selected registry value object
begin
  // Record reference to currently selected node and exit not a data item
  CurrentNode := MainForm.tvReg.Selected;
  if (CurrentNode = nil) or not (TRegObject(CurrentNode.Data) is TRegData) then
    Exit;
  // Create dialogue box and align to main form
  Dlg := TRegDataItemEditDlg.Create(MainForm);
  try
    // Record reference to data item object associated with current tree node
    DataItem := TRegData(CurrentNode.Data);
    // Record name and contents of registry data item in dlg's properties
    Dlg.DataName := DataItem.Name;
    try
      Dlg.DataValue := DataItem.GetAsString;    // raises exception for bad type
      Dlg.DataType := DataItem.ValueType;
      Dlg.ExpandMacroPaths := DataItem.ExpandMacroPaths;
    except
      on E: ERegData do
      begin
        // we have unknown data type - tell dlg, warn user and continue
        Dlg.DataType := rdUnknown;
        Dlg.DataValue := '';
        TMessageBox.Warning(MainForm, sInvalidValue);
      end;
    end;
    // Display dlg box and act on result
    if Dlg.ShowModal = mrOK then
    begin
      // User OK'd - check that item's name is still unique in key
      if not DataItem.SiblingHasName(Dlg.DataName) then
      begin
        // we have a unique name: set name and value
        DataItem.Name := Dlg.DataName;
        SetData(DataItem, Dlg.DataValue, Dlg.DataType, Dlg.ExpandMacroPaths);
        // display data item name and image according to data type required
        if DataItem.Name <> '' then
          CurrentNode.Text := DataItem.Name
        else
          CurrentNode.Text := cDefDataDisplayName;
        CurrentNode.ImageIndex := cNameImageMap[Ord(DataItem.ValueType)];
        CurrentNode.SelectedIndex := CurrentNode.ImageIndex;
      end
      else
        // name not unique - tell user
        TMessageBox.ErrorFmt(
          MainForm, sDupSiblingDataName, [DataItem.Name, Dlg.DataName]
        );
    end;
  finally
    // Get rid of dialogue box
    Dlg.Free;
    // Return focus to reg tree view
    MainForm.tvReg.SetFocus;
  end;
end;

procedure TRegistryTabHandler.EditKey(Sender: TObject);
  {OnClick event handler for Edit Key button: allows user to edit selected key}
var
  CurrentNode: TTreeNode; // currently selected tree node
  Dlg: TRegKeyEditDlg;    // instance of editing dlg box
  RegKey: TRegKey;        // reference to reg key to edit
begin
  // Record currently selected node in registry tree and exit if none selected
  CurrentNode := MainForm.tvReg.Selected;
  if CurrentNode = nil then
    Exit;
  // Record reference to reg key object referenced bu tree node
  RegKey := TRegKey(CurrentNode.Data);
  // Create and align dlg box
  Dlg := TRegKeyEditDlg.Create(MainForm);
  try
    // Set dlg's properties & display dlg
    Dlg.Deletable := RegKey.Deletable;
    Dlg.KeyName := RegKey.Name;
    if Dlg.ShowModal = mrOK then
    begin
      // User OK'd - process changes
      // check if any of key object's siblings has same name - not allowed
      if not RegKey.SiblingHasName(Dlg.KeyName) then
      begin
        // new key name is unique - add new name to reg key object and tree node
        RegKey.Name := Dlg.KeyName;
        RegKey.Deletable := Dlg.Deletable;
        CurrentNode.Text := RegKey.Name;
      end
      else
        // new key name already used - tell user
        TMessageBox.ErrorFmt(
          MainForm, sDupSiblingKeyName, [RegKey.Name, Dlg.KeyName]
        );
    end;
  finally
    // Get rid of dlg box
    Dlg.Free;
    // Return focus to reg tree view
    MainForm.tvReg.SetFocus;
  end;
end;

procedure TRegistryTabHandler.ExportRegEditFile(Sender: TObject);
  {OnClick event handler for Export (registry editor file) button: creates a
  .reg file containing information from registry objects in project}
var
  REF: TRegEditFile;  // object used to write reg edit file

  // ---------------------------------------------------------------------------
  procedure ExportKey(Key: TRegKey);
    {Recursive routine used to export a registry key and values under it. A
    key is written out if either it has no subkeys or if it has data items. When
    a key has sub key the routine recursively calls itself for all sub keys}
  var
    KeyIdx: Integer;  // loops thru all sub keys
    DataIdx: Integer; // loops thru all data items
    Data: TRegData;   // reference to a data item object
    Value: string;    // data item value as string
  begin
    if Key.HasDataItems then
    begin
      // Key has data items, so write out key followed by data items
      REF.WriteKey(Key.PathName);
      // Now write out data items
      for DataIdx := 0 to Pred(Key.DataItems.Count) do
      begin
        Data := Key.DataItems.Items[DataIdx];
        Value := Data.GetAsString;
        if Data.ValueType = rdBinary then
          Value := UBinStrings.SpaceToCommaBinStr(Data.GetAsString)
        else
          Value := Data.GetAsString;
        REF.WriteValue(Key.PathName, Data.Name, Value, Data.ValueType);
      end;
    end
    else if not Key.HasSubKeys then
      // We have no data items but this is terminal key node: write it out
      REF.WriteKey(Key.PathName);
    // Now process any sub keys
    for KeyIdx := 0 to Pred(Key.SubKeys.Count) do
      ExportKey(Key.SubKeys.Items[KeyIdx]);
  end;
  // ---------------------------------------------------------------------------

var
  RegRootKeys: TRegRootKeyList; // list of root keys
  Idx: Integer;                 // loops thru all root keys
begin
  // Get registry file name and type from user
  if MainForm.dlgExportReg.Execute then
  begin
    // We have file: create registry file object to write file
    REF := TRegEditFile.Create;
    try
      // Select type of file to be written
      case MainForm.dlgExportReg.FilterIndex of
        1: REF.FileType := rf4;
        2: REF.FileType := rf5;
      end;
      // Loops thru all root keys, updating reigstry file object with info
      RegRootKeys := GetProject.RegRootKeys;
      for Idx := 0 to Pred(RegRootKeys.Count) do
        if RegRootKeys.Items[Idx].HasSubKeys
          or RegRootKeys.Items[Idx].HasDataItems then
          ExportKey(RegRootKeys.Items[Idx]);
      // Save the reg edit file
      REF.SaveToFile(MainForm.dlgExportReg.FileName);
    finally
      REF.Free;
    end;
  end;
end;

procedure TRegistryTabHandler.ImportRegEditFile(Sender: TObject);
  {OnClick event handler for Import (registry editor file) button: updates
  registry page with details contained in a .reg file}
var
  REF: TRegEditFile;            // object used to access regedit file
  RegRootKeys: TRegRootKeyList; // list of registry editor root keys
  KeyIdx: Integer;              // loops thru all imported keys
  Key: TRegKey;                 // project reg key object instance
  KeyName: string;              // name of a reg key
  DataIdx: Integer;             // loops thru all imported data values for key
  DataName: string;             // name of a reg data item
  Value: string;                // registry data value as string
  Data: TRegData;               // project reg data object instance
  DataType: TRegDataType;       // data type of a reg data item
  CanOverwriteAll: Boolean;     // if all existing values can be overwritten

  // ---------------------------------------------------------------------------
  function QueryOverwrite(const KeyName, DataName: string): Boolean;
    {Returns true if a data item within a key can be overwritten: we can
    overwrite if user individually OKs or has siad OK to all in a dialog box}
  var
    Prompt: string; // the prompt displayed to request overwrite permission
  begin
    // Assume we have permission
    Result := True;
    if not CanOverwriteAll then
    begin
      // User has not given all overwrite permission: display dialog box
      if DataName = '' then
        Prompt := Format(sQueryKeyOverwriteDefault, [KeyName])
      else
        Prompt := Format(sQueryKeyOverwriteName, [KeyName, DataName]);
      case TMessageBox.Confirm(MainForm, Prompt) of
        False:  Result := False;          // user denied: return false
        True:   CanOverwriteAll := True; // user OKd all: set flag
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  // Get import file name from user
  if MainForm.dlgImportReg.Execute then
  begin
    // Note no given blanket overwrite permmission to overwrite existing values
    CanOverwriteAll := False;
    // Get reference to root keys
    RegRootKeys := GetProject.RegRootKeys;
    // Load registry file using reg edit file object
    REF := TRegEditFile.Create;
    try
      REF.LoadFromFile(MainForm.dlgImportReg.FileName);
      // Loop thru all imported keys
      for KeyIdx := 0 to Pred(REF.KeyCount) do
      begin
        // Record key name
        KeyName := REF.Keys[KeyIdx];
        if (URegUtils.ValidateRegKeyPath(KeyName) = 0)
          and not REF.KeyDeleteFlag(KeyName) then
        begin
          // Key is OK to import: its valid & not a root key
          Key := RegRootKeys.AddKeyFromPath(KeyName);
          // Loop thru all data items in key
          for DataIdx := 0 to Pred(REF.ValueCount(KeyName)) do
          begin
            // Record name of data item
            DataName := REF.ValueNames[KeyName, DataIdx];
            // Skip item if its delete flag is set
            if not REF.ValueDeleteFlag(KeyName, DataName) then
            begin
              // Get reference to data item: assigned if already exists
              Data := Key.DataItems.FindByName(DataName) as TRegData;
              // If value name exists and not allowed to overwrite, skip to next
              if Assigned(Data) and not QueryOverwrite(KeyName, DataName) then
                Continue;
              // Record value string and data type of item
              Value := REF.Value(KeyName, DataName);
              DataType := REF.ValueType(KeyName, DataName);
              if DataType = rdBinary then
                Value := UBinStrings.CommaToSpaceBinStr(Value);
              if Assigned(Data) then
                // Data item assigned already: update it
                SetData(Data, Value, DataType, False)
              else
              begin
                // Data item doesn't exist: create it and set value
                Data := TRegData.Create(DataName);
                if SetData(Data, Value, DataType, False) then
                  // value set OK, add to list
                  Key.DataItems.Add(Data)
                else
                  // error in value: free new item
                  Data.Free;
              end;
            end;
          end;
        end;
      end;
    finally
      REF.Free;
    end;
    // Update display with new items
    UpdateSheet;
  end;
end;

function TRegistryTabHandler.InstructionsResId: Integer;
  {Returns the id of the resource containing the instructions that relate to the
  page}
begin
  Result := cRegistryInfoResId;
end;

procedure TRegistryTabHandler.KeyFromTemplate(Sender: TObject);
  {Adds a registry key to treeview that is based on one of the key templates
  that are accessed via dialog box}
var
  KeyPath: string;  // key to key from template
begin
  // Get key from user via template dialog
  if TRegTemplatesDlg.KeyFromTemplate(MainForm, KeyPath)
    and (KeyPath <> '') then
  begin
    // We have key: add it and update sheet to display it
    GetProject.RegRootKeys.AddKeyFromPath(KeyPath);
    UpdateSheet;
  end;
end;

procedure TRegistryTabHandler.TreeChange(Sender: TObject; Node: TTreeNode);
  {OnChange event handler for treeview component on tab sheet: updates state of
  buttons according to selection}
begin
  UpdateButtons;
end;

procedure TRegistryTabHandler.TreeDblClick(Sender: TObject);
  {OnDblClick event handler for regsitry tree view: triggers pop-up menu's
  default menu item if there is one, else does nothing}
begin
  if MainForm.miRegEditKey.Default then
    MainForm.miRegEditKey.Click
  else if MainForm.miRegEditDataItem.Default then
    MainForm.miRegEditDataItem.Click;
end;

procedure TRegistryTabHandler.TreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {OnMouseDown event handler for registry tree view: ensures that the relevant
  tree node is selected when right mouse button is clicked and displays any
  required popup info window when left button is clicked}

  // ---------------------------------------------------------------------------
  procedure ShowWdw(Node: TTreeNode; Wdw: TInfoWdw);
    {Shwos the given info window adjacent to the given tree node}
  var
    DisplayAt: TPoint;  // point where window is to be displayed
    ItemRect: TRect;    // display for tree node
  begin
    Assert(Assigned(Node));
    Assert(Assigned(Wdw));
    // Determine position to display window: to right of node
    ItemRect := Node.DisplayRect(True);
    DisplayAt := MainForm.tvReg.ClientToScreen(ItemRect.TopLeft);
    Inc(
      DisplayAt.X, ItemRect.Right - MainForm.tvReg.Indent * (Node.Level + 2) + 8
    );
    // Display the window
    Wdw.DisplayWdw(DisplayAt.X, DisplayAt.Y);
  end;
  // ---------------------------------------------------------------------------

var
  ClickedNode: TTreeNode; // reference to node mouse is over
  RegObj: TRegObject;     // registry object associated with current node
begin
  if Button = mbRight then
  begin
    // Right mouse button was clicked: select node under mouse
    ClickedNode := MainForm.tvReg.GetNodeAt(X, Y);
    if Assigned(ClickedNode) then
      MainForm.tvReg.Selected := ClickedNode;
  end
  else if Button = mbLeft then
  begin
    // Left mouse button clioked: we may need to display a popup info window
    // get node under mouse when clicked
    ClickedNode := MainForm.tvReg.GetNodeAt(X, Y);
    if Assigned(ClickedNode)
      and (htOnLabel in MainForm.tvReg.GetHitTestInfoAt(X, Y)) then
    begin
      // Node was clicked on its label: this is hot region
      // record registry object associated with node
      RegObj := ClickedNode.Data;
      // check if we need to display a window
      if Assigned(fDataWdw)
        and Assigned(RegObj)
        and (RegObj is TRegData) then
      begin
        // this is a data node and we need to display a data window
        fDataWdw.RegData := RegObj as TRegData;
        ShowWdw(ClickedNode, fDataWdw);
      end
      else if Assigned(fKeyWdw)
        and Assigned(RegObj)
        and (RegObj is TRegKey) then
      begin
        // this is a key node and we need to display a key window
        fKeyWdw.RegKey := RegObj as TRegKey;
        ShowWdw(ClickedNode, fKeyWdw);
      end;
    end;
  end;
  inherited;
end;

procedure TRegistryTabHandler.TreeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  {OnMouseMove event handler for registry tree view: determines if we are to
  display the node under cursor (if any) underlined to denote that a popup info
  window is available if the node is left clicked}
var
  Node: TTreeNode;  // node under mouse curesor
begin
  // Get node under cursor
  Node := MainForm.tvReg.GetNodeAt(X, Y);
  if Assigned(Node)
    and (htOnLabel in MainForm.tvReg.GetHitTestInfoAt(X, Y)) then
  begin
    // We have a node and we're over its label
    if TRegObject(Node.Data) is TRegKey then
      // registry key or root key node:
      // hot-track if we want registry key popup windows
      MainForm.tvReg.HotTrack := Assigned(fKeyWdw)
    else if TRegObject(Node.Data) is TRegData then
      // registry data node: hot-track if we want registry data popup windows
      MainForm.tvReg.HotTrack := Assigned(fDataWdw)
    else
      // unknown node: no hot tracking
      MainForm.tvReg.HotTrack := False;
  end
  else
    // no node under cursor: no hot tracking
    MainForm.tvReg.HotTrack := False;
end;

procedure TRegistryTabHandler.UpdateButtons;
  {Updates state of tab sheet's buttons depending on current state of controls}
var
  Node: TTreeNode;  // the currently selected tree node
begin
  If(MainForm.tvReg.Selected = nil) then ;
  // Record currently selected node, which must be nil or reference a TRegObejct
  Node := MainForm.tvReg.Selected;
  Assert((Node = nil) or (TRegObject(Node.Data) is TRegKey)
    or (TRegObject(Node.Data) is TRegData));
  // Now act according to type of node
  with MainForm do
  begin
    if Node = nil then
    begin
      // There is no node selected
      btnAddKey.Enabled := False;
      btnEditKey.Enabled := False;
      btnDeleteKey.Enabled := False;
      btnAddDataItem.Enabled := False;
      btnEditDataItem.Enabled := False;
      btnDeleteDataItem.Enabled := False;
    end
    else if TRegObject(Node.Data) is TRegKey then
    begin
      // Node references a TRegKey - check whether it's a root key
      if TRegObject(Node.Data) is TRegRootKey then
      begin
        // This is a root key - can't edit it
        btnAddKey.Enabled := True;
        btnEditKey.Enabled := False;
        btnDeleteKey.Enabled := False;
        btnAddDataItem.Enabled := False;
        btnEditDataItem.Enabled := False;
        btnDeleteDataItem.Enabled := False;
      end
      else
      begin
        // This is a sub-key - can be edited
        btnAddKey.Enabled := True;
        btnEditKey.Enabled := True;
        btnDeleteKey.Enabled := True;
        btnAddDataItem.Enabled := True;
        btnEditDataItem.Enabled := False;
        btnDeleteDataItem.Enabled := False;
      end
    end
    else // TRegObject(Node.Data) is TRegData
    begin
      // Node references a TRegData
      btnAddKey.Enabled := False;
      btnEditKey.Enabled := False;
      btnDeleteKey.Enabled := False;
      btnAddDataItem.Enabled := False;
      btnEditDataItem.Enabled := True;
      btnDeleteDataItem.Enabled := True;
    end;
    // Update enabled state of popup menu items per associated buttons
    miRegAddKey.Enabled := btnAddKey.Enabled;
    miRegEditKey.Enabled := btnEditKey.Enabled;
    miRegDeleteKey.Enabled := btnDeleteKey.Enabled;
    miRegAddDataItem.Enabled := btnAddDataItem.Enabled;
    miRegEditDataItem.Enabled := btnEditDataItem.Enabled;
    miRegDeleteDataItem.Enabled := btnDeleteDataItem.Enabled;
    // Set default menu item (this acts when user double clicks node)
    Assert(not(miRegEditKey.Enabled and miRegEditDataItem.Enabled));
    // default is edit key if edit key menu item enabled and node has no
    // children (when node has children double clicking expands/collapses node)
    miRegEditKey.Default := miRegEditKey.Enabled
      and not Node.HasChildren;
    // default is edit data item if edit data item menu item enabled
    // (this button is never enabled when edit key is enabled)
    miRegEditDataItem.Default := miRegEditDataItem.Enabled;
  end;
end;

procedure TRegistryTabHandler.UpdateSheet;
  {Updates content of controls on tab sheet to reflect relavant project property
  values}
var
  I: Integer;         // loops thru root keys
  InfoWdws: LongWord; // bit mask that determines which info windows displayed
begin
  // Clear tree (uncouple tree change event first - we may get exception)
  MainForm.tvReg.OnChange := nil;
  // Freeze tree view while updating
  MainForm.tvReg.Items.BeginUpdate;
  try
    MainForm.tvReg.Items.Clear;
    MainForm.tvReg.OnChange := TreeChange;
    // Add root regkeys to RegTree, along with their child keys and names
    for I := 0 to GetProject.RegRootKeys.Count - 1 do
      AddRegTreeNode(nil, GetProject.RegRootKeys.Items[I]);
    MainForm.tvReg.FullExpand;
  finally
    MainForm.tvReg.Items.EndUpdate;
  end;
  // Determine if info windows are to be displayed
  InfoWdws := MainForm.Settings.InfoWindows;
  if cSIBRegDataInfoWdw and InfoWdws <> 0 then
  begin
    // we're displaying registry data info window: create window
    fDataWdw := TRegDataWdw.Create(MainForm);
    fDataWdw.Width := 250;
  end
  else
  begin
    // we're not displaying registry data window: free and nil object
    fDataWdw.Free;
    fDataWdw := nil;
  end;
  if cSIBRegKeyInfoWdw and InfoWdws <> 0 then
  begin
    // we're displaying registry key info window: create window
    fKeyWdw := TRegKeyWdw.Create(MainForm);
    fKeyWdw.ColumnWidth := 64;
  end
  else
  begin
    // we're not displaying registry key window: free and nil object
    fKeyWdw.Free;
    fKeyWdw := nil;
  end;
  // Now update the buttons
  UpdateButtons;
end;


{ TRegDataWdw }

procedure TRegDataWdw.SetRegData(const Value: TRegData);
  {Write access method for RegData property: updates the window's Contents
  property with the required information about the registry data object}
resourcestring
  // Descriptions of registry data types
  sUnknown = 'Unknown';
  sString = 'String';
  sExpandString = 'Expand String';
  sInteger = 'Integer';
  sBinary = 'Binary';
  // Additional text displayed with value and data types
  sExpandPathMacros = '(expand path macros)';
  sNoValueSet = '(no value set)';
  // Contents strings for display in window
  sNameContent = 'Name:=%s';
  sTypeContent = 'Type:=%s';
  sValueContent = 'Value:=%s';
const
  // Table mapping registry data types onto descriptions
  cRegData: array[TRegDataType] of string = (
    sUnknown, sString, sExpandString, sInteger, sBinary
  );
var
  Str: string;      // used to contruct display strings
  IsError: Boolean; // flag true if an data item value is in error
begin
  // Store new property value
  fRegData := Value;
  // Clear previous window contents
  Contents.Clear;
  // Add entry for data value's name or (default) if no name provided
  Str := Value.Name;
  if Str = '' then
    Str := cDefDataDisplayName;
  Contents.Add(Format(sNameContent, [Str]));
  // Add entry for data type
  Str := cRegData[Value.ValueType];
  if Value.ExpandMacroPaths
    and (Value.ValueType in [rdString, rdExpandString]) then
    // append note saying that path macros will be expanded
    Str := Str + ' ' + sExpandPathMacros;
  Contents.Add(Format(sTypeContent, [Str]));
  // Add entry for data value: or first 128 chars if longer than this
  try
    // try to get value: set error flag false if all OK
    Str := Value.GetAsString;
    IsError := False;
  except
    // we have error getting value: say say and set error flag
    Str := sNoValueSet;
    IsError := True;
  end;
  // truncate value if necessary
  if Length(Str) > 128 then
    Str := Copy(Str, 1, 128) + '...';
  // add entry for value: non nil value to Objects[] property => red title
  Contents.AddObject(Format(sValueContent, [Str]), Pointer(IsError));
end;


{ TRegKeyWdw }

procedure TRegKeyWdw.SetRegKey(const Value: TRegKey);
  {Write access method for RegKey property: updates the window's Contents
  property with the required information about the registry key object}
resourcestring
  // Contents strings for display in window
  sRootKeyContent = 'Root Key:=%s';
  sKeyContent = 'Key:=%s';
  sAlwaysDeleteContent = 'Deletion:=Always';
  sNeverDeleteContent = 'Deletion:=Never';
  sDeleteIfEmptyContent = 'Deletion:=If no sub keys';
begin
  // Record property value
  fRegKey := Value;
  // Clear window's existing contents
  Contents.Clear;
  // Set key name: use different titles for root keys and ordinary keys
  if Value is TRegRootKey then
    Contents.Add(Format(sRootKeyContent, [Value.Name]))
  else
    Contents.Add(Format(sKeyContent, [Value.Name]));
  // Set deletion method
  case Value.Deletable of
    rdNone:         Contents.Add(sNeverDeleteContent);
    rdAlways:       Contents.Add(sAlwaysDeleteContent);
    rdIfNoSubKeys:  Contents.Add(sDeleteIfEmptyContent);
  end;
end;

end.
