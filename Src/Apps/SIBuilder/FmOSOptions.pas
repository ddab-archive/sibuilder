{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmOSOptions.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialog box that allows
                            user to edit supported operating systems.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 20/02/2008
      @COMMENTS             Replaced usage of ResIds.inc and Help.inc include
                            files with UResources and UHelpContexts units.
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
 * The Original Code is FmOSOptions.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmOSOptions;


interface


uses
  // Delphi
  StdCtrls, ImgList, Controls, ComCtrls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UOSTreeViewMgr;

type

  {
  TOSOptionsDlg:
    Dialog box that allows user to edit supported operating systems using a tree
    view control. This dialog inherits from the generic OK dialog box and
    displays OK, Cancel and Help buttons. This class uses a manager object to
    update the tree view and respond to its events.

    Inheritance: TOSOptionsDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TOSOptionsDlg = class(TGenericOKDlg)
    tvOSs: TTreeView;
    ilChecks: TImageList;
    lblOSs: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private // properties
    fOSOptions: LongWord;
    procedure SetOSOptions(const Value: LongWord);
  private
    fTVMgr: TOSTreeViewMgr;
      {Object that manipulates and updates tree view control to display OS
      information and responds to tree view's events}
  public
    property OSOptions: LongWord read fOSOptions write SetOSOptions;
      {Operating system options: as input by users by toggling check marks on
      tree nodes}
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  UOS, UHelpContexts, UResources;


{$R *.DFM}


{ TOSOptionsDlg }

procedure TOSOptionsDlg.FormCreate(Sender: TObject);
  {Form creation event handler: loads required images and creates an object to
  manage the OS tree view}
begin
  inherited;
  // Load image list required by tree view
  ilChecks.ResourceLoad(rtBitmap, cCheckBoxes, clOlive);
  Assert(ilChecks.Count > 0);
  // Create object to manage list of operating systems
  fTVMgr := TOSTreeViewMgr.Create(tvOSs, ilChecks);
  // Set help context
  HelpContext := IDH_DLG_OSOPTIONS;
end;

procedure TOSOptionsDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler: frees owned object}
begin
  inherited;
  fTVMgr.Free;
end;

procedure TOSOptionsDlg.OKBtnClick(Sender: TObject);
  {OK button click event handler: accepts changes and updates OS options. Dlg
  box will not close if there's an error}
begin
  inherited;
  // Assume there's a problem and we won't be closing dialog box
  ModalResult := mrNone;
  // Record OS option per tree view: raises exception if there's a problem
  fOSOptions := fTVMgr.OSOptions;
  // If all check boxes checked set 0 (all OS) options
  if fOSOptions = cAnyOS then
    fOSOptions := 0;
  // Everything OK: allow dlg to close
  ModalResult := mrOK;
end;

procedure TOSOptionsDlg.SetOSOptions(const Value: LongWord);
  {Write access method for OSOptions property: passes value on to tree view
  manager which updates treeview}
begin
  fTVMgr.OSOptions := Value;
end;

end.
