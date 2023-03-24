{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     ULicenseDlgWdw.pas
  @COMMENTS                 Handles the display and use input to a license
                            dialog box on behalf of license dialog COM object.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 21/01/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/12/2002
      @COMMENTS             + Added call to SetForegroundWindow to ensure
                              license dialog is displayed on top of the install
                              console window.
                            + Added trap for WM_HELP message so that F1 key
                              press can be detected and help displayed.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literal used for Close button with
                            resource string.
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
 * The Original Code is ULicenseDlgWdw.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit ULicenseDlgWdw;

interface

uses
  // Delphi
  Windows,
  // Project
  IntfSITLicenseDlg;


type

  {
  TSITLicenseDlgData:
    Structure of data passed to dialog box procedure to customise dlg box.
  }
  TSITLicenseDlgData = record
    Caption: PChar;               // dlg box caption
    License: PChar;               // license text for display in edit control
    Bounds: TRect;                // size and position of dlg
    Style: DWORD;                 // styles applied to dlg
    HelpIntf: ISITLicenseDlgHelp; // instance used for help callback
    HelpContext: DWORD;           // help context to be passed to callback
  end;

  {
  PSITLicenseDlgData:
    Pointer to TSITLicenseDlgData structure.
  }
  PSITLicenseDlgData = ^TSITLicenseDlgData;


function DisplayLicenseDlg(HOwner: HWND;
  DlgData: TSITLicenseDlgData): HRESULT; stdcall;
  {Displays the license dialog box using the given data and returns S_OK if user
  OKs/Accepts and S_FALSE if user Declines/Cancels}


implementation

uses
  // Delphi
  Messages;


{$Include LicDlgBox.inc}  // IDs of dlg box controls


function DlgProc(DlgHandle: HWND; Msg: LongWord;
  wParam, lParam: LongInt): BOOL; stdcall; forward;
  {Dialog box procedure for license dlg box}


{ Dialog box procedure supporting routines }

function DisplayLicenseDlg(HOwner: HWND;
  DlgData: TSITLicenseDlgData): HRESULT; stdcall;
  {Displays the license dialog box using the given data and returns S_OK if user
  OKs/Accepts and S_FALSE if user Declines/Cancels}
begin
  // Create and display dialog box, returning appropriate result
  Result := DialogBoxParam(
    HInstance,
    'LICENSEDLG',
    HOwner,
    @DlgProc,
    Integer(@DlgData)
  );
end;

procedure StrCopy(Dest, Src: PChar);
  {Copies Src C-string to Dest C-string: this routine to prevent need to include
  SysUtils}
var
  PDest, PSrc: PChar; // cursors into source and dest strings
begin
  PDest := Dest;
  PSrc := Src;
  while PSrc^ <> #0 do
  begin
    PDest^ := PSrc^;
    Inc(PSrc);
    Inc(PDest);
  end;
  PDest^ := #0;
end;

procedure ArrangeControls(DlgHandle: HWND; PData: PSITLicenseDlgData;
  CliWidth, CliHeight: Integer);
  {Aligns all controls with dialog box form}
const
  CBtnWidth = 75;   // width of buttons
  CBtnHeight = 25;  // height of buttons
  CBtnSpacing = 8;  // spacing between adjacent buttons
var
  BtnCount: Integer;          // number of visible buttons
  TotalBtnWidth: Integer;     // width of all buttons inc. spacing
  NextBtnLeft: Integer;       // X co-ord of next button we are aligning
  BtnTop: Integer;            // Y co-ord of all buttons
  DeclineBtnVisible: Boolean; // whether decline buttons is displayed
  HelpBtnVisible: Boolean;    // whether help button is displayed
begin
  // Determine which buttons visible and count then
  DeclineBtnVisible := (PData^.Style
    and SITLicDlg_DECLINEBTN = SITLicDlg_DECLINEBTN);
  HelpBtnVisible := Assigned(PData^.HelpIntf);
  BtnCount := 1;
  if DeclineBtnVisible then
    Inc(BtnCount);
  if HelpBtnVisible then
    Inc(BtnCount);
  // Calc total width of buttons
  TotalBtnWidth := BtnCount * CBtnWidth + (BtnCount - 1) * CBtnSpacing;
  // Work out where top and left of buttons are
  BtnTop := CliHeight - 8 - CBtnHeight;
  if PData^.Style and SITLicDlg_BTNSLEFT = SITLicDlg_BTNSLEFT then
    NextBtnLeft := 8
  else if PData^.Style and SITLicDlg_BTNSCENTRE = SITLicDlg_BTNSCENTRE then
    NextBtnLeft := (CliWidth - TotalBtnWidth) div 2
  else {default: SITLicDlg_BTNSRIGHT}
    NextBtnLeft := CliWidth - TotalBtnWidth - 8;
  // Align visible buttons
  MoveWindow(GetDlgItem(DlgHandle, IDOK),
    NextBtnLeft, BtnTop, CBtnWidth, CBtnHeight, True);
  Inc(NextBtnLeft, CBtnWidth + CBtnSpacing);
  if DeclineBtnVisible then
  begin
    MoveWindow(GetDlgItem(DlgHandle, IDCANCEL),
      NextBtnLeft, BtnTop, CBtnWidth, CBtnHeight, True);
    Inc(NextBtnLeft, CBtnWidth + CBtnSpacing);
  end;
  if HelpBtnVisible then
    MoveWindow(GetDlgItem(DlgHandle, IDB_HELP),
      NextBtnLeft, BtnTop, CBtnWidth, CBtnHeight, True);
  // Align bevel above buttons
  MoveWindow(GetDlgItem(DlgHandle, IDD_BEVEL),
    8, BtnTop - 8, CliWidth - 16, 2, True);
  // Align label at top
  MoveWindow(GetDlgItem(DlgHandle, IDD_LABEL),
    8, 4, CliWidth - 16, 12, True);
  // Align edit control between text label and bevel
  MoveWindow(GetDlgItem(DlgHandle, IDD_EDIT),
    8, 24, CliWidth - 16, BtnTop - 40, True);
end;

function CreateFont(FontName: PChar; PointSize: Integer): HFONT;
  {Creates a font for given names and point size}
var
  DC: HDC;            // desktop device context
  LogFont: TLogFont;  // stucture used to describe font
begin
  // Get desktop DC
  DC := GetDC(0);
  try
    // Initialise logical font structure: unused records are zero
    FillChar(LogFont, SizeOf(LogFont), 0);
    with LogFont do
    begin
      // Calculate font height
      lfHeight := -MulDiv(PointSize, GetDeviceCaps(DC, LOGPIXELSY), 72);
      // Store font name in structure
      StrCopy(lfFaceName, FontName);
    end;
    // Create font
    Result := CreateFontIndirect(LogFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure InitDlg(DlgHandle: HWND; PData: PSITLicenseDlgData; MonoFont: HFONT);
  {Initialise dialog box: set caption and license text, hide any buttons that
  aren't required and set size of dialog box}
var
  HEdit: HWND;  // handle to license edit control
resourcestring
  sCloseCaption = '&Close';
begin
  // Set caption text if one provided
  if PData^.Caption <> nil then
    SetWindowText(DlgHandle, PData^.Caption);
  // Set license edit control properties
  HEdit := GetDlgItem(DlgHandle, IDD_EDIT);
  if PData^.Style and SITLicDlg_MONOFONT = SITLicDlg_MONOFONT then
    // mono font required
    SendMessage(HEdit, WM_SETFONT, Integer(MonoFont), 0);
  if PData^.License <> nil then
    // display the license text
    SetWindowText(HEdit, PData^.License);
  // Show/hide required buttons and adapt caption of Accept button
  if PData^.Style and SITLicDlg_DECLINEBTN = 0 then
  begin
    // decline button hidden: affects caption of accept button
    ShowWindow(GetDlgItem(DlgHandle, IDCANCEL), SW_HIDE);
    SetWindowText(GetDlgItem(DlgHandle, IDOK), PChar(sCloseCaption));
  end;
  if not Assigned(PData^.HelpIntf) then
    // help button hidden
    ShowWindow(GetDlgItem(DlgHandle, IDB_HELP), SW_HIDE);
  // Set required window size
  SetWindowPos(
    DlgHandle,
    0,
    PData^.Bounds.Left,
    PData^.Bounds.Top,
    PData^.Bounds.Right - PData^.Bounds.Left,
    PData^.Bounds.Bottom - PData^.Bounds.Top,
    SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOCOPYBITS
  );
end;

procedure ShowHelp(PData: PSITLicenseDlgData);
  {Calls any callback method of any assigned help interface object with user-
  supplied help context}
begin
  if Assigned(PData^.HelpIntf) then
    PData^.HelpIntf.DisplayHelp(HELP_CONTEXT, PData^.HelpContext);
end;


{ Dialog box procedure }

function DlgProc(DlgHandle: HWND; Msg: LongWord;
  wParam, lParam: LongInt): BOOL; stdcall;
  {Dialog box procedure for dlg box}
const
  PDlgData: PSITLicenseDlgData = nil; // records dialog startup info
  MonoFont: HFONT = 0;                // reference to mono spaced font
begin
  case Msg of
    WM_INITDIALOG:
    begin
      // Initialise dlg box
      // store data that defines dlg box
      PDlgData := PSITLicenseDlgData(lParam);
      // create mono font in case needed
      MonoFont := CreateFont('Courier New', 9);
      // initialise dlg box
      InitDlg(DlgHandle, PDlgData, MonoFont);
      // set focus on default push-button and return false because SetFocus used
      SetFocus(GetDlgItem(DlgHandle, IDOK));
      SetForegroundWindow(DlgHandle);
      Result := False;
    end;
    WM_SIZE:
    begin
      // Re-arrange controls
      ArrangeControls(DlgHandle, PDlgData, LoWord(lParam), HiWord(lParam));
      Result := True;
    end;
    WM_COMMAND:
    begin
      // Assume we handle command
      Result := True;
      // Now check what command is and handle if we recognise it
      case LoWord(wParam) of
        IDOK:
          EndDialog(DlgHandle, S_OK);     // "OK" button clicked
        IDCANCEL:
          EndDialog(DlgHandle, S_FALSE);  // "Cancel" button clicked
        IDB_HELP:
          ShowHelp(PDlgData);             // "Help" button clicked
        else
          Result := False;                // Not known: don't handle
      end;
    end;
    WM_HELP:
    begin
      // Help message received: show help
      ShowHelp(PDlgData);
      Result := True;
    end;
    WM_DESTROY:
    begin
      // Delete mono font created in WM_INITDIALOG
      DeleteObject(MonoFont);
      Result := True;
    end;
    else
      // Not handling the message
      Result := False;
  end;
end;

end.
