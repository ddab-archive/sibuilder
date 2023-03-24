{
 * FmGenericDlg.pas
 *
 * This is a generic base class for all SIBuilder's dialog boxes. Provides layout
 * and functionality common to all dlg boxes:
 *  - Calling win help from help button.
 *  - Sizing form and aligning components.
 *  - Providing code to align subsidiary dlg boxes to the calling dlg. All
 *    descendant dlg boxes should place their additional components within the
 *    body panel and size it accordingly. The dialog and the basic components
 *    will then be sized and positioned using the body panel's size.
 *
 * v1.0 of 09 Mar 2000  - Original version.
 * v1.1 of 29 Aug 2000  - Two changes were made:
 *                        - Made dialog box align itself to it's owner if owner
 *                          is a form. Removed method that descendants could
 *                          call for aligning child dlg boxes since made
 *                          redundant by automatic alignment.
 *                        - Made a warning message appear if help button is
 *                          clicked when there is no help context assigned by
 *                          descendant forms.
 * v1.2 of 25 Jun 2001  - Changed bevel at bottom of dlg box to have line rather
 *                        than box formatting.
 * v1.3 of 25 Nov 2005  - Changed to use THelpManager to handle help display
 *                        rather than rely on Delphi's built in processing.
 * v1.4 of 16 Sep 2008  - Explicitly set dialog box's parent. This change needed
 *                        for dialogs to work stay on top when program is
 *                        selected in Vista task bar.
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
 * The Original Code is FmGenericDlg.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmGenericDlg;

interface

uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes;

type
  {Generic base class for all the program's dialog boxes - displays and handles
  help button and permits aligning of child dlg boxes to this window}
  TGenericDlg = class(TForm)
    BottomBevel: TBevel;
    BodyPanel: TPanel;
    HelpBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    procedure AlignToOwner;
      {Aligns this dialogue box relative to its owner form. Called automatically
      when form is created.
      }
    procedure SetParentToOwner;
      {Sets handle of form's owner as parent of this form's window. If owner has
      no handle, or is nil either active form or application's main form is used
      as parent.
      }
  end;

implementation

uses
  // Delphi
  Windows, Dialogs, Math,
  // Project
  UHelpManager;

{$R *.DFM}

resourcestring
  SNoHelp = 'Sorry there is no help available.';
    {message displayed if no help context is asigned and help button is clicked}


{ TGenericDlg }

procedure TGenericDlg.AlignToOwner;
  {Aligns this dialogue box relative to its owner form. Called automatically
  when form is created}
var
  OwnerForm: TForm;     // Form that owns this dialog box
begin
  // Check that we have a owner that is a form, if not do nothing
  if not (Owner is TForm) then
    Exit;
  OwnerForm := Owner as TForm;
  // Now display according to who owns us
  if OwnerForm.BorderStyle = bsDialog then
  begin
    // We're centering over another dlg box - just offset down and left a bit
    Self.Left := Max(0, Min(OwnerForm.Left + 40, Screen.Width - Self.Width));
    Self.Top := Max(0, Min(OwnerForm.Top + 40, Screen.Height - Self.Height));
  end
  else
  begin
    // We're centering over a form -
    // centre horizontally over form, while keeping on screen
    Self.Left :=
      Max(0, Min(OwnerForm.Left + (OwnerForm.Width - Self.Width) div 2,
        Screen.Width - Self.Width));
    // "centre" 1/3rd way down main window if possible}
    Self.Top :=
      Max(0, Min(OwnerForm.Top + (OwnerForm.Height - Self.Height) div 3,
        Screen.Height - Self.Height));
  end;
end;

procedure TGenericDlg.FormCreate(Sender: TObject);
  {Form creation event handler - sizes form and positions components accoridng
  to size of BodyPanel}
begin
  // Set dialog box parent
  SetParentToOwner;
  // Position components
  ClientWidth := BodyPanel.Width + 16;
  BottomBevel.Top := BodyPanel.Height + 16;
  HelpBtn.Top := BottomBevel.Top + 8;
  ClientHeight := HelpBtn.Top + HelpBtn.Height + 4;
  BottomBevel.Width := BodyPanel.Width;
  HelpBtn.Left := ClientWidth - 8 - HelpBtn.Width;
  // Align form to owner
  AlignToOwner;
end;

procedure TGenericDlg.HelpBtnClick(Sender: TObject);
  {Help button click event handler - if there's a help context assigned then
  calls main help file with topic whose context is stored in HelpContext
  otherwise a warning dialog box is displayed.
  }
begin
  if HelpContext <> 0 then
    THelpManager.ShowTopic(HelpContext)
  else
  begin
    MessageBeep(MB_ICONEXCLAMATION);
    MessageDlg(SNoHelp, mtWarning, [mbOK], 0);
  end;
end;

procedure TGenericDlg.SetParentToOwner;
  {Sets handle of form's owner as parent of this form's window. If owner has no
  handle, or is nil either active form or application's main form is used as
  parent.
  }
var
  ParentWnd: THandle; // window handle of parent control
begin
  // Get parent handle
  if Assigned(Owner) and (Owner is TWinControl) then
    ParentWnd := (Owner as TWinControl).Handle
  else if Assigned(Screen.ActiveCustomForm) then
    ParentWnd := Screen.ActiveCustomForm.Handle
  else if Assigned(Application.MainForm) then
    ParentWnd := Application.MainForm.Handle
  else
    ParentWnd := Application.Handle;
  Assert(ParentWnd <> 0,                                   // ** do not localise
    'TGenericDlg.SetParentToOwner: Can''t get parent window');
  // Set form's window handle
  SetWindowLong(Handle, GWL_HWNDPARENT, ParentWnd);
end;

end.

