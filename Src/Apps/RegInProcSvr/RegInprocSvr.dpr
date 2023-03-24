{
 * RegInprocSvr.dpr
 *
 * Main project file for applet that can register / unregister in-process COM
 * servers.
 *
 * Requires RegCOMSvrs COM Object.
 *
 * v1.0 of 21 Jan 2001  - Original version.
 * v1.1 of 23 Aug 2008  - Moved dialog box messages from string literals to
 *                        resource strings.
 * v1.2 of 10 Apr 2008  - Changed to use resource file RISResources.res instead
 *                        of RegInProcSvrDlg.res.
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
 * The Original Code is RegInprocSvr.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


program RegInprocSvr;

uses
  Windows,
  ActiveX,
  Messages,
  ShellAPI,
  IntfRegCOMSvrs in '..\..\Intf\IntfRegCOMSvrs.pas';


{$R VRegInProcSvr.res}      // version information
{$R RISResources.res}       // other resources
{$I RegInProcSvrDlg.inc}    // dialog box control ids


{ In-proc server registration routines }

function LoadInProcServerRegObj(out RegCOMSvr: IRegInProcCOMServers;
  out Res: HResult): Boolean;
  {Creates instance of a RegCOMServers COM object.
    @param RegCOMSvr [out] Reference to server instance.
    @return True if creation succeeds and False on failure.
  }
begin
  Res := CoCreateInstance(
    CLSID_RegCOMServers,
    nil,
    CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,
    IRegInProcCOMServers,
    RegCOMSvr
  );
  Result := Succeeded(Res);
end;

function RegisterDLL(DLL: string): HResult;
  {Register the COM objects in an in-process COM server.
    @param DLL [in] DLL containing COM objects.
    @return Success or failure codes returned from COM registration.
  }
var
  Intf: IRegInProcCOMServers; // instance of object that performs registration
begin
  if LoadInProcServerRegObj(Intf, Result) then
    Result := Intf.RegInProcSvr(PChar(DLL));
end;

function UnRegisterDLL(DLL: string): HResult;
  {Unregister the COM objects in an in-process COM server.
    @param DLL [in] DLL containing COM objects.
    @return Success or failure codes returned from COM registration.
  }
var
  Intf: IRegInProcCOMServers; // instance of object that performs unregistration
begin
  if LoadInProcServerRegObj(Intf, Result) then
    Result := Intf.UnRegInProcSvr(PChar(DLL));
end;


{ Dialog box procedure helper routines }

procedure Report(DlgH: HWND; Res: HResult; IsReg: Boolean);
  {Report given result of an attempt to register or unregister.
    @param DlgH [in] Window handle of dialog box to display report.
    @param Res [in] Result code to be displayed.
    @param IsReg [in] True if report relates to registration, false if relates
      to unregistration.
  }
resourcestring
  // Dialog box messages
  sSvrUnregSucceeded = 'Server successfully unregistered';
  sSvrRegSucceeded = 'Server successfully registered';
  sSvrUnregFailed = 'Server failed to unregister';
  sSvrRegFailed = 'Server failed to register';
  sUnregNotSupported = 'Server doesn''t support unregistration';
  sRegNotSupported = 'Server doesn''t support registration';
  sCantFindRegCode = 'Program error: can''t find registration code';
  sCantLoadSvr = 'Can''t load the server';
const
  cMsgs: array[1..3, Boolean] of string =
    // Predefined messages:
    // different message according to if registering or un-registering
    (
      (sSvrUnregSucceeded, sSvrRegSucceeded),
      (sSvrUnregFailed, sSvrRegFailed),
      (sUnregNotSupported, sRegNotSupported)
    );
var
  Txt: string;                    // the message to report
  PErrMsg: array[0..256] of Char; // stores error Windows system messages
begin
  // Choose message to display
  case Res of
    S_OK, S_FALSE: Txt := cMsgs[1, IsReg];
    E_FAIL: Txt := cMsgs[2, IsReg];
    E_NOINTERFACE: Txt := sCantFindRegCode;
    CO_E_NOT_SUPPORTED: Txt := cMsgs[3, IsReg];
    CO_E_DLLNOTFOUND: Txt := sCantLoadSvr;
    else
    begin
      // No custom message: get text from Windows
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Res, 0, PErrMsg, 256, nil);
      Txt := PErrMsg;
    end;
  end;
  // Now display the required message
  SetWindowText(GetDlgItem(DlgH, IDD_REPORT), PChar(Txt));
end;

function GetEditText(DlgH: HWND): string;
  {Gets text from dialog box's edit control.
    @param DlgH [in] Handle of dialog box window.
    @return Required text.
  }
var
  EditH: HWND;                              // window handle of edit control
  TheText: array[0..MAX_PATH + 1] of Char;  // editr control's text
begin
  EditH := GetDlgItem(DlgH, IDD_EDIT);
  GetWindowText(EditH, TheText, MAX_PATH + 1);
  Result := TheText;
end;

procedure SetEditText(DlgH: HWND; const TheText: string);
  {Sets text in dialog box's edit control.
    @param DlgH [in] Handle of dialog box window.
    @param TheText [in] Text to be displayed in edit control.
  }
var
  EditH: HWND;  // window handle of edit control
begin
  EditH := GetDlgItem(DlgH, IDD_EDIT);
  SetWindowText(EditH, PChar(TheText));
end;

function GetDroppedFile(DlgH: HWND; DropH: THandle): string;
  {Gets name of first file that has been dropped on dialog box.
    @param DlgH [in] Handle of dialog box window.
    @param DropH [in] Handle of dropped files.
    @return Name of file.
  }
var
  PFileName : array[0..MAX_PATH + 1] of Char; // name of first-dropped file
begin
  try
    // Get file name
    DragQueryFile(DropH, 0, PFileName, MAX_PATH + 1);
    Result := PFileName;
  finally
    // Release drop handle
    DragFinish(DropH);
  end;
end;

procedure CentreWindow(DlgH: HWND);
  {Centres dialog box's window on screen.
    @param DlgH [in] Dialog box window handle.
  }
var
  Placement: TWindowPlacement;  // structure describing window placement
  WWidth, WHeight: Integer;     // width and height of window
  SWidth, SHeight: Integer;     // width and height of screen
  WLeft, WTop: Integer;         // required top and left of window
begin
  // Get width and height of window
  Placement.length := SizeOf(Placement);
  GetWindowPlacement(DlgH, @Placement);
  with Placement.rcNormalPosition do
  begin
    WWidth := Right - Left;
    WHeight := Bottom - Top;
  end;
  // Get width and height of screen
  SWidth := GetSystemMetrics(SM_CXSCREEN);
  SHeight := GetSystemMetrics(SM_CYSCREEN);
  // Calculate top and left of window to centre it
  WLeft := (SWidth - WWidth) div 2;
  WTop := (SHeight - WHeight) div 2;
  // Centre window
  SetWindowPos(DlgH, 0, WLeft, WTop, WWidth, WHeight,
    SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOCOPYBITS);
end;


{ Dialog box procedure }

function DlgProc(DlgHandle: HWND; Msg: LongWord;
  wParam, lParam: LongInt): BOOL; stdcall;
  {Dialog box procedure.
    @param DlgHandle [in] Dialog box window handle.
    @param Msg [in] Message to be processed.
    @param wParam [in] First message parameter.
    @param lParam [in] Second message parameter.
    @return True if message handled, False if not.
  }
var
  DroppedFile: string;  // name of a file that has been dropped on window
begin
  case Msg of
    WM_INITDIALOG:
    begin
      // Initialise dlg box
      // initialise COM libraries
      CoInitialize(nil);
      // register we accept dropped files
      DragAcceptFiles(DlgHandle, True);
      // set new icon
      SetClassLong(DlgHandle, GCL_HICON,
        Integer(LoadIcon(HInstance, 'MAINICON')));
      // arrange at screen centre
      CentreWindow(DlgHandle);
      Result := False;
    end;
    WM_DROPFILES:
    begin
      // Copy first dropped file into edit control
      DroppedFile := GetDroppedFile(DlgHandle, wParam);
      if DroppedFile <> '' then
        SetEditText(DlgHandle, DroppedFile);
      Result := True;
    end;
    WM_COMMAND:
    begin
      // Assume we handle command
      Result := True;
      // Now check what command is and handle if we recognise it
      case LoWord(wParam) of
        IDOK, IDCANCEL:                        // Close button or X icon clicked
        begin
          CoUninitialize;
          EndDialog(DlgHandle, 0);
        end;
        IDD_REGBTN:      // Attempt to tegister file in edit box & report result
          Report(DlgHandle, RegisterDLL(GetEditText(DlgHandle)), True);
        IDD_UNREGBTN:  // Attempt to unregister file in edit box & report result
          Report(DlgHandle, UnRegisterDLL(GetEditText(DlgHandle)), False);
        else
          Result := False;                    // Command not known: don't handle
      end;
    end;
    else
      // Not handling the message
      Result := False;
  end;
end;

begin
  // Display the dialog box that acts as program's main window
  DialogBoxParam(HInstance, 'REGDLG', 0, @DlgProc, 0);
end.

