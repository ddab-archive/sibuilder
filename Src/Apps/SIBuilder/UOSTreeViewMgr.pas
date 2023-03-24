{
 * UOSTreeViewMgr.pas
 *
 * Defines a class that is used to manage treeview controls used to display and
 * allow user to edit operating system options. The class handles user
 * interactions with treeview and updates the treeview nodes.
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 19 Feb 2008  - Removed unused usage of ResIds.inc.
 * v1.2 of 10 Apr 2008  - Removed Win32s and added Windows Vista to treeview.
 *
 *
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
 * The Original Code is UOSTreeViewMgr.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UOSTreeViewMgr;


interface


uses
  // Delphi
  SysUtils, Classes, Controls, ComCtrls;

type

  {
  TOSTreeViewMgr:
    Class that manages a treeview control that is used to display the various
    operating system options. It creates the required nodes in the treeview:
    each has an associated check box. The class also manipulates the state
    of the check boxes according to user interactions, and maps between the
    state of the tree view and a bitmask describing the operating system
    options.
  }
  TOSTreeViewMgr = class(TObject)
  private
    fTV: TTreeView;
      {The managed treeview control used to display OS options}
    fImgList: TImageList;
      {Image list used to store the check box bitmaps used in treeview}
    fOnChange: TNotifyEvent;
      {Handler for OnChange event}
    function GetOSOptions: LongWord;
      {Read access method for OSOptions property. Reads options from checked nodes
      of tree view and validates them.
        @return Bitmask containing details of operating systems.
      }
    procedure SetOSOptions(Value: LongWord);
      {Write access method for OSOptions property. Checks/unchecks tree view as
      necessary to reflect OSs specified in new property value.
        @param Value [in] New property value.
      }
    procedure PopulateTreeView;
      {Populates tree view control with nodes describing various OS options.
      }
    procedure TVClick(Sender: TObject);
      {OnClick event handler for tree view. Toggles state of checks marks of
      selected nodes and other related nodes.
        @param Sender [in] Reference to treeview triggering event.
      }
    procedure TVCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
      {OnCollapsing event handler for tree view. Prevents nodes from collapsing
        @param Sender [in] Reference to treeview triggering event.
        @param Node [in] Node that is collapsing.
        @param AllowCollapse [in/out] Set to false to prevent collapsing.
      }
    procedure TVDblClick(Sender: TObject);
      {OnDblClick event handler for tree view. Clicks treeview again (OnClick
      handler fires and toggles once) do ensure check mark is toggled twice.
        @param Sender [in] Reference to treeview triggering event.
      }
    function CalcOSOptions: LongWord;
      {Calculates bit mask describing currently selected operating systems
      according to the checked nodes in the tree view control.
        @return Required bitmask.
      }
  public
    constructor Create(TV: TTreeView; ImgList: TImageList);
      {Class constructor. Populates tree view.
        @param TV [in] Treeview to be managed.
        @param ImgList [in] Image list containing check boxes displayed in TV.
      }
    function Validate(out Reason: string): Boolean; overload;
      {Validates entries in tree view to ensure a valid operating system has
      been specified.
        @param Reason [out] Reason for error. Only relevant if False is
          returned.
        @return True if entries are valid, False otherwise.
      }
    function Validate: Boolean; overload;
      {Validates entries in tree view to ensure a valid operating system has
      been specified.
        @return True if entries are valid, False otherwise.
      }
    property OSOptions: LongWord read GetOSOptions write SetOSOptions;
      {Operating system options: as input by users by toggling check marks on
      tree nodes}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
      {Event triggered when a change is made to the managed tree view control}
  end;

  {
  EOSTreeViewMgr:
    Class of exception raised by TTreeViewMgr.
  }
  EOSTreeViewMgr = class(Exception);


implementation


uses
  // Project
  UOS;


const
  // Image indexes into image list
  cCheckedImgIdx = 0;         // check marks in a box
  cUncheckedImgIdx = 2;       // empty box (unchecked)
  cInteterminateImgIdx = 1;   // greyed check mark in a box


resourcestring
  // Error message
  sErrNoOSSpecified = 'An operating system must be specified';


procedure SetImage(Node: TTreeNode; ImgIdx: Integer);
  {Sets image index of a tree node.
    @param Node [in] Tree node to be updated.
    @param ImgIdx [in] Required image index.
  }
begin
  Node.ImageIndex := ImgIdx;
  Node.SelectedIndex := ImgIdx;
end;


{ TOSTreeViewMgr }

function TOSTreeViewMgr.CalcOSOptions: LongWord;
  {Calculates bit mask describing currently selected operating systems
  according to the checked nodes in the tree view control.
    @return Required bitmask.
  }
var
  ParentNode: TTreeNode;  // reference to a parent tree node
  ChildNode: TTreeNode;   // reference to a child tree node
begin
  inherited;
  // Initialise OSs
  Result := 0;
  // Scan thru tree, building up OS from checked items
  ParentNode := fTV.Items.GetFirstNode;
  while ParentNode <> nil do
  begin
    case ParentNode.ImageIndex of
      0:
        // Parent node checked => all children  checked => only need to record
        // parent node's OS data since this includes all children
        Result := Result or LongWord(ParentNode.Data);
      1:
      begin
        // Parent node is indeterminate => we need to get specific values from
        // child nodes
        ChildNode := ParentNode.GetFirstChild;
        while ChildNode <> nil do
        begin
          if ChildNode.ImageIndex = cCheckedImgIdx then
            Result := Result or LongWord(ChildNode.Data);
          ChildNode := ParentNode.GetNextChild(ChildNode);
        end;
      end;
      2:
        // Parent node unchecked => all children unchecked => none of OSs are to
        // be included
        {Do nothing};
    end;
    ParentNode := ParentNode.GetNextSibling;
  end;
end;

constructor TOSTreeViewMgr.Create(TV: TTreeView; ImgList: TImageList);
  {Class constructor. Populates tree view.
    @param TV [in] Treeview to be managed.
    @param ImgList [in] Image list containing check boxes displayed in TV.
  }
begin
  Assert(Assigned(TV));
  Assert(Assigned(ImgList));
  inherited Create;
  // Record tree view and set event handlers
  fTV := TV;
  TV.OnClick := TVClick;
  TV.OnCollapsing := TVCollapsing;
  TV.OnDblClick := TVDblClick;
  // Record image list
  fImgList := ImgList;
  // Display nodes in tree view
  PopulateTreeView;
end;

function TOSTreeViewMgr.GetOSOptions: LongWord;
  {Read access method for OSOptions property. Reads options from checked nodes
  of tree view and validates them.
    @return Bitmask containing details of operating systems.
  }
begin
  // Calculate result from entries
  Result := CalcOSOptions;
  // Check some OSs have been entered: error if not
  if Result = 0 then
    raise EOSTreeViewMgr.Create(sErrNoOSSpecified);
  // If all check boxes checked set 0 (all OS) options
  if Result = cAnyOS then
    Result := 0;
end;

procedure TOSTreeViewMgr.PopulateTreeView;
  {Populates tree view control with nodes describing various OS options.
  }

  // ---------------------------------------------------------------------------
  function AddChildNode(Parent: TTreeNode; OSCode: LongWord): TTreeNode;
    {Adds a child node to tree view for an OS and gives it an unchecked image.
      @param Parent [in] Parent node under which child node is added.
      @param OSCode [in] Operating system code associated with new node.
      @return Reference to new tree node.
    }
  begin
    Result := fTV.Items.AddChild(Parent, UOS.OSCodeToStr(OSCode));
    Result.Data := Pointer(OSCode);
    SetImage(Result, cUncheckedImgIdx);
  end;
  // ---------------------------------------------------------------------------

var
  Node: TTreeNode;  // reference to a new tree node
begin
  inherited;
  // win 9x platform
  Node := AddChildNode(nil, cAnyWin9xPlatform);
  AddChildNode(Node, cWin95);
  AddChildNode(Node, cWin95OSR2);
  AddChildNode(Node, cWin98);
  AddChildNode(Node, cWin98SE);
  AddChildNode(Node, cWinMe);
  // win NT platform
  Node := AddChildNode(nil, cAnyWinNTPlatform);
  AddChildNode(Node, cWinNT351);
  AddChildNode(Node, cWinNT4);
  AddChildNode(Node, cWin2000);
  AddChildNode(Node, cWinXP);
  AddChildNode(Node, cWinVista);
  fTV.FullExpand;
end;

procedure TOSTreeViewMgr.SetOSOptions(Value: LongWord);
  {Write access method for OSOptions property. Checks/unchecks tree view as
  necessary to reflect OSs specified in new property value.
    @param Value [in] New property value.
  }

  // ---------------------------------------------------------------------------
  procedure SetImagePerOS(Node: TTreeNode);
    {Sets a tree node's image to checked or unchecked according to whether
    node's related OS code is in current OSs bit set.
      @param Node [in] Tree node to be updated.
    }
  begin
    if LongWord(Node.Data) and Value <> 0 then
      SetImage(Node, cCheckedImgIdx)
    else
      SetImage(Node, cUncheckedImgIdx);
  end;
  // ---------------------------------------------------------------------------

var
  ParentNode: TTreeNode;  // reference to a parent tree node
  ChildNode: TTreeNode;   // reference to a child tree node
  ParentImgIdx: Integer;  // image index for a parent node
begin
  // Adjust Value to cAnyOS if it is 0
  if Value = 0 then
    Value := cAnyOS;
  // Scan thru all parent nodes
  ParentNode := fTV.Items.GetFirstNode;
  while ParentNode <> nil do
  begin
    // Check out first child node and test if there is one
    ChildNode := ParentNode.GetFirstChild;
    if ChildNode <> nil then
    begin
      // Sets child image according to if node's OS is in required set
      SetImagePerOS(ChildNode);
      // Get hold of first child node's image: if all children have same checked
      // state then this image will be used for parent
      ParentImgIdx := ChildNode.ImageIndex;
      // Scan thru remaining children
      ChildNode := ParentNode.GetNextChild(ChildNode);
      while ChildNode <> nil do
      begin
        // Sets child image per OS
        SetImagePerOS(ChildNode);
        // If child has different to other children then parent has
        // indeterminate state image
        if ChildNode.ImageIndex <> ParentImgIdx then
          ParentImgIdx := cInteterminateImgIdx;
        // Move to next child
        ChildNode := ParentNode.GetNextChild(ChildNode);
      end;
      // Set parent node's image
      SetImage(ParentNode, ParentImgIdx);
    end
    else
      // There are no children: set parent image per OS set
      SetImagePerOS(ParentNode);
    // Move to next parent
    ParentNode := ParentNode.GetNextSibling;
  end;
  // Refresh tree view to display changed check boxes
  fTV.Refresh;
end;

procedure TOSTreeViewMgr.TVClick(Sender: TObject);
  {OnClick event handler for tree view. Toggles state of checks marks of
  selected nodes and other related nodes.
    @param Sender [in] Reference to treeview triggering event.
  }
var
  Node: TTreeNode;        // reference to currently selected tree node
  ChildNode: TTreeNode;   // reference to child tree node
  ParentNode: TTreeNode;  // reference to parent tree node
  ImgIdx: Integer;        // image index of selected node
  ParentImgIdx: Integer;  // image index of parent node
  Idx: Integer;           // loops thru all child nodes
begin
  // Record currently selected node and ensure assigned
  Node := fTV.Selected;
  if Assigned(Node) then
  begin
    // Record current node's image index
    ImgIdx := Node.ImageIndex;
    if Node.Level = 1 then
    begin
      // We're at the child level: process child node
      // toggle check mark on tree node
      if ImgIdx = cCheckedImgIdx then
        ImgIdx := cUncheckedImgIdx
      else
        ImgIdx := cCheckedImgIdx;
      SetImage(Node, ImgIdx);
      // now update parent node
      ParentNode := Node.Parent;
      ParentImgIdx := ParentNode.Item[0].ImageIndex;
      // scan through all siblings, check if all same state as parent
      for Idx := 1 to Pred(ParentNode.Count) do
      begin
        // check if parent image is same as child
        ChildNode := ParentNode.Item[Idx];
        if ParentImgIdx <> ChildNode.ImageIndex then
        begin
          // parent and child difference state: set parent to indeterminate
          ParentImgIdx := cInteterminateImgIdx;
          Break;
        end;
      end;
      SetImage(ParentNode, ParentImgIdx);
    end
    else
    begin
      // This is a parent level node: toggle its state
      Assert(Node.Level = 0);
      if ImgIdx in [cCheckedImgIdx, cInteterminateImgIdx] then
        ImgIdx := cUncheckedImgIdx
      else
        ImgIdx := cCheckedImgIdx;
      SetImage(Node, ImgIdx);
      // Now update children to same state as parfent
      for Idx := 0 to Pred(Node.Count) do
      begin
        ChildNode := Node.Item[Idx];
        SetImage(ChildNode, ImgIdx);
      end;
    end;
  end;
  // Refresh tree view display to display revised glyphs
  fTV.Refresh;
  // Trigger OnChange event
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TOSTreeViewMgr.TVCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
  {OnCollapsing event handler for tree view. Prevents nodes from collapsing
    @param Sender [in] Reference to treeview triggering event.
    @param Node [in] Node that is collapsing.
    @param AllowCollapse [in/out] Set to false to prevent collapsing.
  }
begin
  AllowCollapse := False;
end;

procedure TOSTreeViewMgr.TVDblClick(Sender: TObject);
  {OnDblClick event handler for tree view. Clicks treeview again (OnClick
  handler fires and toggles once) do ensure check mark is toggled twice.
    @param Sender [in] Reference to treeview triggering event.
  }
begin
  TVClick(Sender);
end;

function TOSTreeViewMgr.Validate: Boolean;
  {Validates entries in tree view to ensure a valid operating system has been
  specified.
    @return True if entries are valid, False otherwise.
  }
var
  Dummy: string;  // unused error description
begin
  // Call overloaded version to do actual validation and discard description
  Result := Validate(Dummy);
end;

function TOSTreeViewMgr.Validate(out Reason: string): Boolean;
  {Validates entries in tree view to ensure a valid operating system has been
  specified.
    @param Reason [out] Reason for error. Only relevant if False is returned.
    @return True if entries are valid, False otherwise.
  }
begin
  // It is an error if no OSs have been entered => options = 0
  Result := CalcOSOptions <> 0;
  // Set error reason if their is an error, or set empty string if OK
  if not Result then
    Reason := sErrNoOSSpecified
  else
    Reason := '';
end;

end.

