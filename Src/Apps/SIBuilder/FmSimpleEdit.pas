{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmSimpleEdit.pas
  @COMMENTS                 Implements a dialog box that lets user enter a line
                            of text.
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
 * The Original Code is FmSimpleEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmSimpleEdit;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;


type
  {
  TSimpleEditDlg:
    A simple dialog box used to enter a line of text. The dialog box alings
    itself over the owning form.

    Inheritance: TSimpleEditDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TSimpleEditDlg = class(TGenericOKDlg)
    lblPrompt: TLabel;
    edValue: TEdit;
    procedure FormShow(Sender: TObject);
  public
    class function Edit(Owner: TComponent; const ACaption, APrompt: string;
      AHelpContext: Integer; var Value: string): Boolean;
      {Displays dialog box and returns text entered by user. Parameters are:
        Owner: owning form - dialog aligns to this
        ACaption: displayed in dialog box title bar
        APrompt: displayed above edit box
        AHelpContext: topic accessed by help button (if 0 button is hidden)
        Value: value passed in is displayed in edit box and parameter is
          updated to text entered if user OKs
      The method returns true if user OKs and false if user cancels}
  end;


implementation


{$R *.DFM}

{ TSimpleEditDlg }

class function TSimpleEditDlg.Edit(Owner: TComponent; const ACaption,
  APrompt: string; AHelpContext: Integer; var Value: string): Boolean;
  {Displays dialog box and returns text entered by user. Parameters are:
    Owner: owning form - dialog aligns to this
    ACaption: displayed in dialog box title bar
    APrompt: displayed above edit box
    AHelpContext: topic accessed by help button (if 0 button is hidden)
    Value: value passed in is displayed in edit box and parameter is updated to
      text entered if user OKs
    The method returns true if user OKs and false if user cancels}
begin
  with TSimpleEditDlg.Create(Owner) do
    try
      Caption := ACaption;
      lblPrompt.Caption := APrompt;
      edValue.Text := Value;
      HelpContext := AHelpContext;
      Result := ShowModal = mrOK;
      if Result then
        Value := edValue.Text;
    finally
      Free;
    end;
end;

procedure TSimpleEditDlg.FormShow(Sender: TObject);
  {Handler for Form's OnShow event: hides Help button and moves other buttons
  to right if help context is zero}
begin
  inherited;
  if HelpContext = 0 then
  begin
    HelpBtn.Visible := False;
    OKBtn.Left := CancelBtn.Left;
    CancelBtn.Left := HelpBtn.Left;
  end;
end;

end.
