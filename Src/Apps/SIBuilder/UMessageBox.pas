{
 * UMessageBox.pas
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 25 Nov 2005  - Changed to use THelpManager to handle help display
 *                        rather than rely on Delphi's built in processing.
 * v1.2 of 11 Apr 2008  - Added code that ensures messages are valid sentences.
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
 * The Original Code is UMessageBox.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UMessageBox;


interface


uses
  // Delphi
  SysUtils, Classes, Dialogs, Forms;


type

  {
  TMessageBox:
    Static class the provides methods that display various message dialog boxes
    that align themselves to the owning form.
  }
  TMessageBox = class(TObject)
  private
    class procedure PreventCloseOnCancel(Sender: TObject;
      var CanClose: Boolean);
      {OnCloseQuery event handler for dialog form. This handler is used when we
      wish to prevent dialog from being closed on pressing cancel or clicking
      close button on dialog.
        @param Sender [in] Reference to dialog box form.
        @param CanClose [in/out] Set to true to inhibit dialog from being
          closed.
      }
    class function CreateDlg(Owner: TForm; const Msg: string;
      const MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ACaption: string;
      AHelpContext: Integer = 0; InhibitCancel: Boolean = False): TForm;
      {Creates a dialog box instance customised per parameters.
        @param Owner [in] Owner of dialog box. Dialog is aligned over the owner
          form. If Owner is nil dialog is centred over screen.
        @param Msg [in] Message displayed in dialog box, adjusted to be a valid
          sentence.
        @param MsgType [in] Type of dialog. Determines icon and title message.
          If MsgType is mtCustom the program's icon is displayed and the caption
          is the Application's title. If ACaption is not '' then its value is
          used as the caption instead.
        @param Buttons [in] Set of buttons to appear in dialog. If a help button
          is included in the set it is ignored: display of help button depends
          on AHelpContext.
        @param ACaption [in] Dialog box caption. If ACaption is '' then MsgType
          determines the caption.
        @param AHelpContext [in] Help context. If non-zero a help button is
          displayed that accesses the related topic in the application's help
          file.
        @param InhibitCancel [in] Inhibits cancel buttons. When True no Cancel
          button will appear, the ESC key will not close the dialog and there
          will be no close button in the dialog's caption.
      }
    class function DisplayDlg(Owner: TForm; const Msg: string;
      const MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ACaption: string;
      AHelpContext: Integer = 0; InhibitCancel: Boolean = False): Integer;
      {Displays a modal message dialog box.
        @param Owner [in] Owner of dialog box. Dialog is aligned over the owner
          form. If Owner is nil dialog is centred over screen.
        @param Msg [in] Message displayed in dialog box, adjusted to be a valid
          sentence.
        @param MsgType [in] Type of dialog. Determines icon and title message.
          If MsgType is mtCustom the program's icon is displayed and the caption
          is the Application's title. If ACaption is not '' then its value is
          used as the caption instead.
        @param Buttons [in] Set of buttons to appear in dialog. If a help button
          is included in the set it is ignored: display of help button depends
          on AHelpContext.
        @param ACaption [in] Dialog box caption. If ACaption is '' then MsgType
          determines the caption.
        @param AHelpContext [in] Help context. If non-zero a help button is
          displayed that accesses the related topic in the application's help
          file.
        @param InhibitCancel [in] Inhibits cancel buttons. When True no Cancel
          button will appear, the ESC key will not close the dialog and there
          will be no close button in the dialog's caption.
        @return Code of the button used to close the dialog.
      }
    class procedure DlgHelpBtnClick(Sender: TObject);
      {Handles click on help button in dialogs. Displays help topic associated
      with dialog's help context. This handler should not be set when dialog's
      help context is zero.
        @param Sender [in] Reference to clicked help button.
      }
    class procedure DlgKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
      {Handles key presses in dialog. Shows help topic per dialog's help context
      when F1 pressed. This handler should not be set when dialog's help context
      is zero.
        @param Sender [in] Reference to clicked help button.
      }
    class procedure DlgShowHelp(Dlg: TForm);
      {Shows help topic according to dialog box's help context.
        @param Dlg [in] Reference to dialog box.
      }
  public
    class procedure Error(Owner: TForm; const Msg: string;
      AHelpContext: Integer = 0);
      {Displays a message in an error dialog box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param Msg [in] Message to be displayed.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
    class procedure ErrorFmt(Owner: TForm; const FmtStr: string;
      const Args: array of const; AHelpContext: Integer = 0);
      {Displays a message built from a format string in an error dialog box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param FmtStr [in] Format string for message.
        @param Args [in] Arguments to format string.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
    class procedure Warning(Owner: TForm; const Msg: string;
      AHelpContext: Integer = 0);
      {Displays a message in a warning dialog box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param Msg [in] Message to be displayed.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
    class procedure WarningFmt(Owner: TForm; const FmtStr: string;
      const Args: array of const; AHelpContext: Integer = 0);
      {Displays a message built from a format string in a warning dialog box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param FmtStr [in] Format string for message.
        @param Args [in] Arguments to format string.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
    class function Confirm(Owner: TForm; const Msg: string;
      AHelpContext: Integer = 0): Boolean;
      {Displays a message in a confirmation dialog box and returns user
      response.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param Msg [in] Message to be displayed.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
        @return True if user confirms, False otherwise.
      }
    class function ConfirmFmt(Owner: TForm; const FmtStr: string;
      const Args: array of const; AHelpContext: Integer = 0): Boolean;
      {Displays a message built from a format string in a confirmation dialog
      box and returns user response.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param FmtStr [in] Format string for message.
        @param Args [in] Arguments to format string.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
        @return True if user confirms, False otherwise.
      }
    class procedure Information(Owner: TForm; const Msg: string;
      AHelpContext: Integer = 0);
      {Displays a message in an information dialog box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param Msg [in] Message to be displayed.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
    class procedure InformationFmt(Owner: TForm; const FmtStr: string;
      const Args: array of const; AHelpContext: Integer = 0);
      {Displays a message built from a format string in an information dialog
      box.
        @param Owner [in] Owner form over which dialog is aligned. May be nil in
          which case dialog is displayed over the screen.
        @param FmtStr [in] Format string for message.
        @param Args [in] Arguments to format string.
        @param AHelpContext [in] Help context. If 0 help button not displayed.
      }
  end;

  {
  EMessageBox:
    Class of exception raised by TMessageBox methods.
  }
  EMessageBox = class(Exception);


implementation


uses
  // Delphi
  Windows, Controls, StdCtrls, ExtCtrls, Consts, StrUtils,
  // Project
  UHelpManager;


resourcestring
  // Error messages
  sTooManyButtons =
    'Too many user defined buttons in Choose message box: maximum is %d';
  // Captions
  sChoose = 'Choose';


{ Helper function }

function MakeSentence(const Txt: string): string;
  {Checks if text forms a valid sentence, i.e. it ends with a full stop, a
  question mark or an exclamation mark. If not a full stop is added to the text.
    @param Txt [in] Text to be made into sentence.
    @return Valid sentence.
  }
begin
  Result := Txt;
  while IsDelimiter('.', Result, Length(Result)) do
    Result := AnsiLeftStr(Result, Length(Result) - 1);
  if not IsDelimiter('!?', Result, Length(Result)) then
    Result := Result + '.';
end;


{ TMessageBox }

class function TMessageBox.Confirm(Owner: TForm; const Msg: string;
  AHelpContext: Integer): Boolean;
  {Displays a message in a confirmation dialog box and returns user response.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param Msg [in] Message to be displayed.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
    @return True if user confirms, False otherwise.
  }
begin
  Result := DisplayDlg(
    Owner, Msg, mtConfirmation, [mbYes, mbNo], '', AHelpContext, False
  ) = mrYes;
end;

class function TMessageBox.ConfirmFmt(Owner: TForm; const FmtStr: string;
  const Args: array of const; AHelpContext: Integer): Boolean;
  {Displays a message built from a format string in a confirmation dialog box
  and returns user response.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param FmtStr [in] Format string for message.
    @param Args [in] Arguments to format string.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
    @return True if user confirms, False otherwise.
  }
begin
  Result := Confirm(Owner, Format(FmtStr, Args), AHelpContext);
end;

class function TMessageBox.CreateDlg(Owner: TForm; const Msg: string;
  const MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ACaption: string;
  AHelpContext: Integer = 0; InhibitCancel: Boolean = False): TForm;
  {Creates a dialog box instance customised per parameters.
    @param Owner [in] Owner of dialog box. Dialog is aligned over the owner
      form. If Owner is nil dialog is centred over screen.
    @param Msg [in] Message displayed in dialog box, adjusted to be a valid
      sentence.
    @param MsgType [in] Type of dialog. Determines icon and title message. If
      MsgType is mtCustom the program's icon is displayed and the caption is the
      Application's title. If ACaption is not '' then its value is used as the
      caption instead.
    @param Buttons [in] Set of buttons to appear in dialog. If a help button is
      included in the set it is ignored: display of help button depends on
      AHelpContext.
    @param ACaption [in] Dialog box caption. If ACaption is '' then MsgType
      determines the caption.
    @param AHelpContext [in] Help context. If non-zero a help button is
      displayed that accesses the related topic in the application's help file.
    @param InhibitCancel [in] Inhibits cancel buttons. When True no Cancel
      button will appear, the ESC key will not close the dialog and there will
      be no close button in the dialog's caption.
  }

  // ---------------------------------------------------------------------------
  function FindImage(const Dlg: TForm): TImage;
    {Finds reference to a dialog's image control.
      @param Dlg [in] Dialog form containing image control.
      @return Reference to image control or nil if no such control.
    }
  var
    Idx: Integer; // loops thru all components on form
  begin
    Result := nil;
    for Idx := 0 to Pred(Dlg.ComponentCount) do
    begin
      if Dlg.Components[Idx] is TImage then
      begin
        Result := Dlg.Components[Idx] as TImage;
        Break;
      end;
    end;
  end;

  function FindHelpBtn(const Dlg: TForm): TButton;
    {Finds reference to a dialog's help button.
      @param Dlg [in] Dialog form's containing help button.
      @return Reference to help button or nil if no such control.
    }
  var
    Idx: Integer; // loops thru all components on form
  begin
    Result := nil;
    for Idx := 0 to Pred(Dlg.ComponentCount) do
    begin
      if (Dlg.Components[Idx] is TButton) and
         ((Dlg.Components[Idx] as TButton).Caption = SMsgDlgHelp) then
      begin
        Result := Dlg.Components[Idx] as TButton;
        Break;
      end;
    end;
  end;

  procedure InhibitCancelButtons(const Dlg: TForm);
    {Ensures no buttons on dialog are cancel buttons: this inhibits any button
    responding to escape key press.
      @param Dlg [in] Dialog form containing buttons.
    }
  var
    Idx: Integer; // loops thru all components on form
  begin
    // Switch off cancel property on all buttons
    for Idx := 0 to Pred(Dlg.ComponentCount) do
      if Dlg.Components[Idx] is TButton then
        (Dlg.Components[Idx] as TButton).Cancel := False;
  end;
  // ---------------------------------------------------------------------------

var
  Img: TImage;          // reference to dialog's image that contains icon
  HelpBtn: TButton;     // reference to dialog's help button
  PosX, PosY: Integer;  // left and top co-ordinates of the dlg box
const
  cXOffset = 80;        // offset of left side of dlg box relative to form
  cYOffset = 80;        // offset of top of dlg box relative to form
begin
  // Whether help button is displayed depends on whether HelpContext is set
  // inclusion of help button in button set is ignored
  if AHelpContext = 0 then
    Exclude(Buttons, mbHelp)
  else
    Include(Buttons, mbHelp);

  // If we're inhibiting cancelling make sure there's no cancel button
  if InhibitCancel then
    Exclude(Buttons, mbCancel);

  // Create a dialog box of required type
  // create a dialog box that contains an icon (mtCustom type doesn't have icon)
  if MsgType <> mtCustom then
    Result := CreateMessageDialog(MakeSentence(Msg), MsgType, Buttons)
  else
    Result := CreateMessageDialog(MakeSentence(Msg), mtInformation, Buttons);

  // Add program's icon if type if mtCustom and use program name in caption
  if MsgType = mtCustom then
  begin
    Img := FindImage(Result);
    if Assigned(Img) then
    begin
      Img.Picture.Graphic.Assign(Application.Icon);
    end;
  end;

  // Set caption of dialog if required
  if ACaption <> '' then
    // a caption has been provided: use it
    Result.Caption := ACaption
  else if MsgType = mtCustom then
    // no caption been provdided but custom type: use program title
    Result.Caption := Application.Title;

  // Inhibit dialog's built on Ctrl+C handling
  Result.OnKeyDown := nil;

  // Set up help file handling
  if AHelpContext <> 0 then
  begin
    // set custom help button click and F1 keypress handling
    HelpBtn := FindHelpBtn(Result);
    if Assigned(HelpBtn) then
      HelpBtn.OnClick := DlgHelpBtnClick;
    Result.OnKeyDown := DlgKeyDown;
  end;
  Result.HelpContext := AHelpContext;

  // Arrange dialog offset over form or centre over screen
  if Assigned(Owner) then
  begin
    // offset dialog from form
    PosX := Owner.Left + cXOffset;
    PosY := Owner.Top + cYOffset;
    if PosX > Screen.Width - Result.Width then
      PosX := Screen.Width - Result.Width
    else if PosX < 0 then
      PosX := 0;
    if PosY > Screen.Height - Result.Height then
      PosY := Screen.Height - Result.Height
    else if PosY < 0 then
      PosY := 0;
    Result.Left := PosX;
    Result.Top := PosY;
  end
  else
  begin
    // no owning form: centre on screen
    PosX := (Screen.Width - Result.Width) div 2;
    PosY := (Screen.Height - Result.Height) div 2;
    Result.Left := PosX;
    Result.Top := PosY;
  end;

  // If we're inhibiting the cancel button
  if InhibitCancel then
  begin
    Result.OnCloseQuery := PreventCloseOnCancel;
    InhibitCancelButtons(Result);
    Result.BorderIcons := Result.BorderIcons - [biSystemMenu];
  end;
end;

class function TMessageBox.DisplayDlg(Owner: TForm; const Msg: string;
  const MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ACaption: string;
  AHelpContext: Integer; InhibitCancel: Boolean): Integer;
  {Displays a modal message dialog box.
    @param Owner [in] Owner of dialog box. Dialog is aligned over the owner
      form. If Owner is nil dialog is centred over screen.
    @param Msg [in] Message displayed in dialog box, adjusted to be a valid
      sentence.
    @param MsgType [in] Type of dialog. Determines icon and title message. If
      MsgType is mtCustom the program's icon is displayed and the caption is the
      Application's title. If ACaption is not '' then its value is used as the
      caption instead.
    @param Buttons [in] Set of buttons to appear in dialog. If a help button is
      included in the set it is ignored: display of help button depends on
      AHelpContext.
    @param ACaption [in] Dialog box caption. If ACaption is '' then MsgType
      determines the caption.
    @param AHelpContext [in] Help context. If non-zero a help button is
      displayed that accesses the related topic in the application's help file.
    @param InhibitCancel [in] Inhibits cancel buttons. When True no Cancel
      button will appear, the ESC key will not close the dialog and there will
      be no close button in the dialog's caption.
    @return Code of the button used to close the dialog.
  }
begin
  with CreateDlg(
    Owner, Msg, MsgType, Buttons, ACaption, AHelpContext, InhibitCancel
  ) do
    try
      Result := ShowModal;
    finally
      Free;
    end;
end;

class procedure TMessageBox.DlgHelpBtnClick(Sender: TObject);
  {Handles click on help button in dialogs. Displays help topic associated with
  dialog's help context. This handler should not be set when dialog's help
  context is zero.
    @param Sender [in] Reference to clicked help button.
  }
var
  HelpBtn: TButton; // reference to help button that triggered this handler
begin
  // Get reference to help button that triggered message
  HelpBtn := (Sender as TButton);
  // Show required topic using help context of form that owns help button
  DlgShowHelp(HelpBtn.Owner as TForm);
end;

class procedure TMessageBox.DlgKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Handles key presses in dialog. Shows help topic per dialog's help context
  when F1 pressed. This handler should not be set when dialog's help context is
  zero.
    @param Sender [in] Reference to clicked help button.
  }
begin
  if Key = VK_F1 then
  begin
    Key := 0;
    DlgShowHelp(Sender as TForm);
  end;
end;

class procedure TMessageBox.DlgShowHelp(Dlg: TForm);
  {Shows help topic according to dialog box's help context.
    @param Dlg [in] Reference to dialog box.
  }
begin
  if Dlg.HelpContext <> 0 then
    THelpManager.ShowTopic(Dlg.HelpContext);
end;

class procedure TMessageBox.Error(Owner: TForm; const Msg: string;
  AHelpContext: Integer = 0);
  {Displays a message in an error dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param Msg [in] Message to be displayed.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  Windows.MessageBeep(MB_ICONEXCLAMATION);
  DisplayDlg(Owner, Msg, mtError, [mbOK], '', AHelpContext, False);
end;

class procedure TMessageBox.ErrorFmt(Owner: TForm; const FmtStr: string;
  const Args: array of const; AHelpContext: Integer);
  {Displays a message built from a format string in an error dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param FmtStr [in] Format string for message.
    @param Args [in] Arguments to format string.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  Error(Owner, Format(FmtStr, Args), AHelpContext);
end;

class procedure TMessageBox.Information(Owner: TForm; const Msg: string;
  AHelpContext: Integer);
  {Displays a message in an information dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param Msg [in] Message to be displayed.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  DisplayDlg(Owner, Msg, mtInformation, [mbOK], '', AHelpContext, False);
end;

class procedure TMessageBox.InformationFmt(Owner: TForm; const FmtStr: string;
  const Args: array of const; AHelpContext: Integer);
  {Displays a message built from a format string in an information dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param FmtStr [in] Format string for message.
    @param Args [in] Arguments to format string.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  Information(Owner, Format(FmtStr, Args), AHelpContext);
end;

class procedure TMessageBox.PreventCloseOnCancel(Sender: TObject;
  var CanClose: Boolean);
  {OnCloseQuery event handler for dialog form. This handler is used when we wish
  to prevent dialog from being closed on pressing cancel or clicking close
  button on dialog.
    @param Sender [in] Reference to dialog box form.
    @param CanClose [in/out] Set to true to inhibit dialog from being closed.
  }
begin
  case (Sender as TForm).ModalResult of
    mrCancel: CanClose := False;
    else CanClose := True;
  end;
end;

class procedure TMessageBox.Warning(Owner: TForm; const Msg: string;
  AHelpContext: Integer);
  {Displays a message in a warning dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param Msg [in] Message to be displayed.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  Windows.MessageBeep(MB_ICONEXCLAMATION);
  DisplayDlg(Owner, Msg, mtWarning, [mbOK], '', AHelpContext, False);
end;

class procedure TMessageBox.WarningFmt(Owner: TForm; const FmtStr: string;
  const Args: array of const; AHelpContext: Integer);
  {Displays a message built from a format string in a warning dialog box.
    @param Owner [in] Owner form over which dialog is aligned. May be nil in
      which case dialog is displayed over the screen.
    @param FmtStr [in] Format string for message.
    @param Args [in] Arguments to format string.
    @param AHelpContext [in] Help context. If 0 help button not displayed.
  }
begin
  Warning(Owner, Format(FmtStr, Args), AHelpContext);
end;

end.

