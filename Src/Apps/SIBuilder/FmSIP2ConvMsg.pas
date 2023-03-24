{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmSIP2ConvMsg.pas
  @COMMENTS                 Implements a dialog box class that is used to
                            display a list of comments resulting from reading
                            and converting a SIBuilder 2 project file in
                            SIBuilder 3. Comments are formatted for east reading
                            in a scroll box and can display a clickable line to
                            a help topic where one is available.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 25/11/2005
      @COMMENTS             Changed to use THelpManager to handle help display
                            rather than rely on Delphi's built in processing.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 20/02/2008
      @COMMENTS             Replaced usage of Help.inc include file with
                            UHelpContexts unit.
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
 * The Original Code is FmSIP2ConvMsg.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmSIP2ConvMsg;


interface

uses
  // Delphi
  StdCtrls, Forms, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg;

type

  {
  TSIP2ConvMsgDlg:
    Dialog box class that is used to display a list of comments resulting from
    reading and converting a SIBuilder 2 project file in SIBuilder 3. Each
    comment is displayed in a custom control within a scroll box for ease of
    reading and can access any available help. The dialog is designed to be
    displayed non-modally.

    Ineritance: TSIP2ConvMsgDlg -> TGenericViewDlg -> TGenericDlg -> [TForm]
  }
  TSIP2ConvMsgDlg = class(TGenericViewDlg)
    chkStayOnTop: TCheckBox;
    lblDesc: TLabel;
    scrComments: TScrollBox;
    procedure chkStayOnTopClick(Sender: TObject);
    procedure DoneBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fComments: TStrings;
      {The comments that are to be displayed in dialog box. This string list
      stores any associated help contexts in Objects[] property}
    fFileName: string;
      {The name of the v2 SIP file the comments relate to}
    function DescHeight(const Desc: string): Integer;
      {Gets height of introductory message that is displayed at top of dialog
      box}
    procedure HelpClick(Sender: TObject);
      {Event handler for clicks on "More info" text links in comment controls:
      displays WinHelp with topic given by sending control's HelpContext
      property}
  public
    class procedure Display(AOwner: TComponent; const FileName: string;
      const Comments: TStrings);
      {Displays the dialog box non-modally to display the given Comments that
      are associated with the conversion of the given SIP2 project file}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Graphics,
  // Project
  UHelpContexts, UHelpManager;

{$R *.DFM}

var
  {
  Private variable that refers to the single instance of the dialog. If a second
  instance is created while first instance is displayed, first is closed and
  freed.
  }
  DlgInst: TSIP2ConvMsgDlg = nil;


type

  {
  TCommentCtrl:
    Custom control used to display a comment with associated link to further
    help. The control sizes itself vertically to display the multi-line comment
    text. The help link is disabled and enabled according to whether HelpContext
    property is zero or non-zero. The control also displays a bullet character
    next to comment.

    NOTE: This control is only suitable for dynamic creation for use with
    clearly defined property values. It is not sufficiently generic or robust
    to be used at design time or on the Delphi component pallette.

    Inheritance: TCommentCtrl -> [TCustomPanel]
  }
  TCommentCtrl = class(TCustomPanel)
  private // properties
    fOnHelpClick: TNotifyEvent;
    function GetComment: string;
    function GetHelpContext: Integer;
    procedure SetComment(const Value: string);
    procedure SetHelpContext(const Value: Integer);
  private
    fBulletLbl: TLabel;
      {Label that displays a bullet character on left of control}
    fCommentLbl: TLabel;
      {Multi line label that displays control's comment}
    fHelpLbl: TLabel;
      {Clickable help label that triggers control's OnHelpClick event when
      clicked}
    fBulletAreaWidth: Integer;
      {Width of area occupied by bullet label and margins around it}
    fHelpAreaWidth: Integer;
      {Width of area occupied by help label and margins around it}
    procedure SizeCtrl(ATextHeight: Integer);
      {Sizes the control and sizes and arranges the owned controls according to
      given comment text height}
    procedure SizeToText;
      {Sizes the control vertically to accomodate all of text of Comment
      property}
    procedure HelpLabelClick(Sender: TObject);
      {Handler for help label's OnClick event: triggers control's OnHelpClick
      event}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: creates owned components and intialises them. The
      owned controls will be freed when this component is freed}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      {Override of SetBounds method: resizes comment label when width of control
      changes: which in turn reflwos text and may chage height}
    property Comment: string read GetComment write SetComment;
      {The multi line comment displayed by control}
    property HelpContext: Integer read GetHelpContext write SetHelpContext;
      {The control's help context: causes clickable help text to be disabled if
      context is zero}
    property OnHelpClick: TNotifyEvent read fOnHelpClick write fOnHelpClick;
      {Event triggered when help text is clicked}
  end;


{ TSIP2ConvMsgDlg }


resourcestring
  // Reports
  sRepTitle = 'Comments following loading of SIP v2 file into SIBuilder 3:';
  // Dialog box text
  sDlgIntro =
    '%s is a SIBuilder release 2 project file. It has been converted and '
    + 'loaded as an un-named file. Some changes were made to the project to '
    + 'make it compatible with release 3. The changes were:';

procedure TSIP2ConvMsgDlg.chkStayOnTopClick(Sender: TObject);
  {When Stay On Top check box is clicked it toggles the state of the form:
  between stay on top and normal state}
begin
  inherited;
  if chkStayOnTop.Checked then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;
end;

function TSIP2ConvMsgDlg.DescHeight(const Desc: string): Integer;
  {Gets height of introductory message that is displayed at top of dialog box}
var
  R: TRect; // rectangle in which text is to appear
begin
  // Set up dummy rectangle, with correct width
  R.Left := 0;
  R.Right := lblDesc.Width;
  R.Top := 0;
  R.Bottom := 1;
  // Find size actually required by text: this doesn't actually draw anything
  DrawText(
    lblDesc.Canvas.Handle,
    PChar(Desc),
    -1,
    R,
    DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX or
    lblDesc.DrawTextBiDiModeFlagsReadingOnly);
  // Return height of required text rectangle
  Result := R.Bottom - R.Top;
end;

class procedure TSIP2ConvMsgDlg.Display(AOwner: TComponent;
  const FileName: string; const Comments: TStrings);
  {Displays the dialog box to display the given Comments that are associated
  with the conversion of the given SIP2 project file}
begin
  // Close any existing window
  if Assigned(DlgInst) then
    DlgInst.Close;    // this frees and nils instance
  Assert(not Assigned(DlgInst));
  // Create new window and display it modally
  DlgInst := TSIP2ConvMsgDlg.Create(AOwner);
  with DlgInst do
  begin
    fComments.Assign(Comments);
    fFileName := FileName;
    Show; // show non-modally: this gets freed when dialog closes
  end;
end;

procedure TSIP2ConvMsgDlg.DoneBtnClick(Sender: TObject);
  {Clicking Done button closes the dialog box}
begin
  inherited;
  Close;
end;

procedure TSIP2ConvMsgDlg.FormClose(Sender: TObject; var Action: TCloseAction);
  {Form close event handler: frees the dialog box instance when it closes}
begin
  inherited;
  // Closing dialog frees it
  Action := caFree;
  // And we nil the private variable that refers to it
  DlgInst := nil;
end;

procedure TSIP2ConvMsgDlg.FormCreate(Sender: TObject);
  {Form creation event handler: creates owned string list object to hold
  comments}
begin
  inherited;
  fComments := TStringList.Create;
end;

procedure TSIP2ConvMsgDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler: frees owned object}
begin
  inherited;
  fComments.Free;
end;

procedure TSIP2ConvMsgDlg.FormShow(Sender: TObject);
  {Sets up dialog's controls in order to display required information when form
  is shown}
var
  IntroDesc: string;  // the introductory message displayed @ top of dlg
  Idx: Integer;       // loops thru comments and comment ctrls
  Cmt: TCommentCtrl;  // references a comment control
  NextTop: Integer;   // position of top of next control
begin
  inherited;
  // Set help context
  HelpContext := IDH_DLG_V2CONVRESULTS;
  // Ensure stay on top check box is displayed
  chkStayOnTop.Left := 8;
  chkStayOnTop.Top := HelpBtn.Top + (HelpBtn.Height - chkStayOnTop.Height) div 2;
  // Display introductory message in dialog and set its size
  IntroDesc := Format(sDlgIntro, [fFileName]);
  lblDesc.Height := DescHeight(IntroDesc);
  lblDesc.Caption := IntroDesc;
  // Adjust position and size of scroll box below intro text
  scrComments.Height := BodyPanel.ClientHeight -
     lblDesc.Top - lblDesc.Height - 8;
  scrComments.Top := lblDesc.Top + lblDesc.Height + 8;
  // Create comment controls and display in scroll box
  // first pass: enter the controls, arranged vertically
  // first control is displayed at top of scroll box
  NextTop := 0;
  // loop thru all comments we're to display
  for Idx := 0 to Pred(fComments.Count) do
  begin
    // create control in scroll box
    Cmt := TCommentCtrl.Create(Self);
    Cmt.Parent := scrComments;
    Cmt.Top := NextTop;
    Cmt.Width := scrComments.ClientWidth; // same width as scroll box
    Cmt.Comment := fComments[Idx];
    Cmt.HelpContext := Integer(fComments.Objects[Idx]);
    Cmt.OnHelpClick := HelpClick;
    // record position of next control
    Inc(NextTop, Cmt.Height);
  end;
  // second pass is made if controls are deeped than scroll box: this means
  // scroll box has displayed scroll bars which have overwritten some controls:
  // so we run down controls again resizing in new narrower space
  if NextTop > scrComments.ClientHeight then
  begin
    // start at top of scroll box
    NextTop := 0;
    // loop thru all child controls in scroll box, processing only TCommentCtrl
    for Idx := 0 to Pred(scrComments.ControlCount) do
    begin
      if scrComments.Controls[Idx] is TCommentCtrl then
      begin
        with TCommentCtrl(scrComments.Controls[Idx]) do
        begin
          // place the control
          Top := NextTop;
          Width := scrComments.ClientWidth; // setting width recalculates height
          Inc(NextTop, Height);
        end;
      end;
    end;
  end;
end;

procedure TSIP2ConvMsgDlg.HelpClick(Sender: TObject);
  {Event handler for clicks on "More info" text links in comment controls:
  displays WinHelp with topic given by sending control's HelpContext property}
begin
  THelpManager.ShowTopic((Sender as TCommentCtrl).HelpContext);
end;


{ TCommentCtrl }

resourcestring
  // Control text
  sHelpCaption = 'More info';

constructor TCommentCtrl.Create(AOwner: TComponent);
  {Class constructor: creates owned components and intialises them. The owned
  controls will be freed when this component is freed}
begin
  inherited;
  // Set default property values
  inherited HelpContext := 0;

  // Set up label used to display bullet character: fixed width
  fBulletLbl := TLabel.Create(Self);
  fBulletLbl.Parent := Self;
  fBulletLbl.Font.Name := 'WingDings';
  fBulletLbl.Font.Size := 12;
  fBulletLbl.Caption := #$DC;
  fBulletLbl.Left := 2;
  fBulletLbl.Top := 4;
  // this is the width of the area for display of bullet and its margins
  fBulletAreaWidth := fBulletLbl.Left + fBulletLbl.Width + 8;

  // Set up label used to display multiline text: width varies with control
  fCommentLbl := TLabel.Create(Self);
  fCommentLbl.Parent := Self;
  fCommentLbl.WordWrap := True;
  fCommentLbl.Top := 4;
  fCommentLbl.Left := fBulletAreaWidth;

  // Set up clickable "help" label: width is fixed
  fHelpLbl := TLabel.Create(Self);
  fHelpLbl.Parent := Self;
  fHelpLbl.Left := 180;
  fHelpLbl.Top := 4;
  fHelpLbl.Width := 100;
  fHelpLbl.Caption := sHelpCaption;
  fHelpLbl.Font.Style := [fsUnderline];     // use green underlined font and to indicate
  fHelpLbl.Font.Color := clGreen;           // .. a hand cursor to indicate a
  fHelpLbl.Cursor := crHandPoint;           // .. help related hotlink
  fHelpLbl.OnClick := HelpLabelClick;
  fHelpLbl.Enabled := False;                // help context defaults to 0
  // this is the width of area occupied by clickable text and its margins
  fHelpAreaWidth := fHelpLbl.Width + 24;

  // Initialise location of control
  Left := 0;
  Top := 0;
end;

function TCommentCtrl.GetComment: string;
  {Read access method for Comment property: returns caption of Comment label}
begin
  Result := fCommentLbl.Caption;
end;

function TCommentCtrl.GetHelpContext: Integer;
  {Read access method for HelpContext property: returns inherited property
  value}
begin
  Result := inherited HelpContext;
end;

procedure TCommentCtrl.HelpLabelClick(Sender: TObject);
  {Handler for help label's OnClick event: triggers control's OnHelpClick event}
begin
  if Assigned(fOnHelpClick) then fOnHelpClick(Self);
end;

procedure TCommentCtrl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  {Override of SetBounds method: resizes comment label when width of control
  changes: which in turn reflwos text and may chage height}
begin
  inherited;
  if Assigned(fCommentLbl) and (Self.Parent <> nil) then
  begin
    if AWidth <> fCommentLbl.Width + fBulletAreaWidth + fHelpAreaWidth then
    begin
      // width has changed: update width of comment
      fCommentLbl.Width := AWidth - fBulletAreaWidth - fHelpAreaWidth;
      // reflow the text, which alters height of control
      SizeToText;
    end;
  end;
end;

procedure TCommentCtrl.SetComment(const Value: string);
  {Write access method for Comment property: stores text in Comment label's
  Caption property and then reflows the text, potentially resizing control}
begin
  fCommentLbl.Caption := Value;
  SizeToText;
end;

procedure TCommentCtrl.SetHelpContext(const Value: Integer);
  {Write access method for HelpContext property: stores value in inherited
  property and enables/disables help label depending on if we have a non-zero
  help context}
begin
  inherited HelpContext := Value;
  fHelpLbl.Enabled := Value <> 0;
end;

procedure TCommentCtrl.SizeCtrl(ATextHeight: Integer);
  {Sizes the control and sizes and arranges the owned controls according to
  given comment text height}
begin
  // Make control 8 pixels higher than comment text
  Self.Height := ATextHeight + 8;
  // Set comment label to height of its text: this redisplays text
  fCommentLbl.Height := ATextHeight;
  // Set left of help label
  fHelpLbl.Left := fBulletAreaWidth + fCommentLbl.Width
    + (fHelpAreaWidth - fHelpLbl.Width) div 2;
end;

procedure TCommentCtrl.SizeToText;
  {Sizes the control vertically to accomodate all of text of Comment property}
var
  R: TRect;       // rectangle containing text
  Value: string;  // the text value
begin
  // Record comment text
  Value := fCommentLbl.Caption;
  // Set up rectangle which defines area of text: set width to that required
  R.Left := 0;
  R.Right := fCommentLbl.Width;
  R.Top := 0;
  R.Bottom := 1;
  // Calculate the rectangle (of given width) required to display text
  DrawText(
    fCommentLbl.Canvas.Handle,
    PChar(Value),
    -1,
    R,
    DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX or
    fCommentLbl.DrawTextBiDiModeFlagsReadingOnly);
  // Resize the control to hold text of this height
  SizeCtrl(R.Bottom - R.Top);
end;

end.
