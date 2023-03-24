{
 * ULicenseDlg.pas
 *
 * Implements a COM object that displays a license dialog box. Also implements
 * the associated class factory object.
 *
 * v1.0 of 21 Jan 2001  - Original version.
 * v1.1 of 11 Apr 2008  - Fixed bug that was causing file access denial when
 *                        trying to read license when file permissions don't
 *                        permit write access.
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
 * The Original Code is ULicenseDlg.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit ULicenseDlg;


interface


uses
  // Delphi
  ActiveX, Windows,
  // Project
  IntfSITLicenseDlg;


type
  {
  TSITLicenseDlgFactory:
    COM Class factory for SITLicenseDlg COM object.
  }
  TSITLicenseDlgFactory = class(TInterfacedObject, IClassFactory)
  protected
    { IClassFactory methods }
    function CreateInstance(const UnkOuter: IUnknown;
      const IID: TGUID; out Obj): HRESULT; stdcall;
      {Creates a SITLicenseDlg object.
        @param UnkOuter [in] Reference to IUnknown interface of outer object of
          aggregation. Must be nil since aggregation not supported.
        @param IID [in] Interface to required object.
        @param Obj [out] Reference to created COM object. Will be set to nil.
        @return S_OK if object created and supports IID. CLASS_E_NOAGGREGATION
        if UnkOuter is not nil. Error return if object doesn't support IID.
      }
    function LockServer(Lock: BOOL): HRESULT; stdcall;
      {Locks and unlocks the server to prevent / permit unloading.
        @param Lock [in] True if server to be locked, False if to be unlocked.
        @return S_OK;
      }
  public
    constructor Create;
      {Class constructor. Increments count of object instances in server.
      }
    destructor Destroy; override;
      {Class destructor. Decrements count of object instances in server.
      }
    class function CanUnload: WordBool;
      {Checks if server can be unloaded.
        @return True if server can be unloaded and False if not.
      }
    class function CreateSITLicenseDlg: ISITLicenseDlg;
      {Creates an SITLicenseDlg object instance that implements ISITLicenseDlg.
      For use when DLL is a private server and not to be created via COM.
        @return Instance of object.
      }
  end;


implementation

uses
  // Project
  ULicenseDlgWdw;


type
  {
  TSITLicenseDlg:
    COM object class that encapsulates a license dialog box.
  }
  TSITLicenseDlg = class(TInterfacedObject, ISITLicenseDlg)
  private
    fWidth: Integer;                // width of dlg box
    fHeight: Integer;               // height of dlg box
    fStyle: Integer;                // the dlg box style flags
    fHelpIntf: ISITLicenseDlgHelp;  // instance used for help callback
    fHelpContext: Integer;          // help context for dlg box
  protected
    procedure SetStyle(AStyle: DWORD); safecall;
      {Sets the required style flags of the dialog box}
    procedure SetSize(Width, Height: Integer); safecall;
      {Sets the size of the dialog box. If 0 is specified for any parameter then
      that dimiension is unchanged. If any value that is less than min or
      greater than max dimension is specified then min or max size is used.
        @param Width [in] Width of dialog box.
        @param Height [in] Height of dialog box.
      }
    procedure RegisterHelp(HelpIntf: ISITLicenseDlgHelp;
      HelpContext: DWORD); safecall;
      {Registers a help "callback" object that dialog box calls when help button
      is clicked, along with help context number to be passed to callback
      method.
        @param HelpIntf [in] Reference to callback object.
        @param HelpContext [in] Help context to be passed to callback method.
      }
    function DisplayLicenseText(HOwner: HWND; X, Y: Integer;
      Caption, License: PChar): HRESULT; stdcall;
      {Displays text in license dialog box.
        @param HOwner [in] Handle of window that owns the dialog box.
        @param X [in] Position of left of dialog box on screen.
        @param Y [in] Position of top of dialog box on screen.
        @param Caption [in] Dialog box's title.
        @param License [in] License text to be displayed.
        @return S_OK if user accepts or S_FALSE if user cancels.
      }
    function DisplayLicenseFile(HOwner: HWND; X, Y: Integer;
      Caption, FileName: PChar): HRESULT; stdcall;
      {Displays a license file in dialog box.
        @param HOwner [in] Handle of window that owns the dialog box.
        @param X [in] Position of left of dialog box on screen.
        @param Y [in] Position of top of dialog box on screen.
        @param Caption [in] Dialog box's title.
        @param FileName [in] Name of license file to be displayed.
        @return S_OK if user accepts, S_FALSE if user cancels or declines and
          E_FAIL if the file cannot be read.
      }
  public
    constructor Create;
      {Class constructor. Sets default values and increments object instance
      count.
      }
    destructor Destroy; override;
      {Class destructor. Decrements object instance count.
      }
  end;


{ Helper routines }

function Min(X, Y: Integer): Integer;
  {Finds minimum of two values.
    @param X [in] First value to check.
    @param Y [in] Second value to check.
    @return Minimum of X and Y.
  }
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

function Max(X, Y: Integer): Integer;
  {Finds maximum of two values.
    @param X [in] First value to check.
    @param Y [in] Second value to check.
    @return Maximum of X and Y.
  }
begin
  if X > Y then
    Result := X
  else
    Result := Y;
end;


{ Global counters }

var
  LockCount: Integer = 0;       // number of locks on server
  InstanceCount: Integer = 0;   // number of object instances in server


{ TSITLicenseDlg }

const
  cMinWidth = 280;  // minimum dialog box width
  cMinHeight = 240; // minimum dialog box height

constructor TSITLicenseDlg.Create;
  {Class constructor. Sets default values and increments object instance count.
  }
begin
  inherited Create;
  // Set defaults
  fWidth := 400;
  fHeight := 300;
  fStyle := SITLicDlg_DECLINEBTN or SITLicDlg_BTNSRIGHT;
  // Increment global reference count
  InterlockedIncrement(InstanceCount);
end;

destructor TSITLicenseDlg.Destroy;
  {Class destructor. Decrements object instance count.
  }
begin
  // Decrement global reference count
  InterlockedDecrement(InstanceCount);
  inherited Destroy;
end;

function TSITLicenseDlg.DisplayLicenseFile(HOwner: HWND; X, Y: Integer;
  Caption, FileName: PChar): HRESULT;
  {Displays a license file in dialog box.
    @param HOwner [in] Handle of window that owns the dialog box.
    @param X [in] Position of left of dialog box on screen.
    @param Y [in] Position of top of dialog box on screen.
    @param Caption [in] Dialog box's title.
    @param FileName [in] Name of license file to be displayed.
    @return S_OK if user accepts, S_FALSE if user cancels or declines and E_FAIL
      if the file cannot be read.
  }
var
  F: File;              // the file we're reading
  Buffer: PChar;        // the text buffer for file contents
  Size: Integer;        // the file's size
  OldFileMode: Integer; // saves current file mode
const
  cReadOnly = 0;        // read only file mode
begin
  // Assume failure
  Result := E_FAIL;
  {$IOCHECKS OFF} // prevent file exceptions
  try
    // Open file
    OldFileMode := FileMode;
    FileMode := cReadOnly;
    AssignFile(F, FileName);
    Reset(F, 1);
    if IOResult <> 0 then
      Exit;
    try
      // Get buffer large enough to hold all file's contents + #0
      Size := FileSize(F);
      GetMem(Buffer, Size + 1);
      try
        // Read file into buffer
        BlockRead(F, Buffer^, Size);
        if IOREsult <> 0 then
          Exit;
        Buffer[Size] := #0;
        // Display text in dlg box
        Result := DisplayLicenseText(HOwner, X, Y, Caption, Buffer);
      finally
        FreeMem(Buffer);
      end;
    finally
      CloseFile(F);
      FileMode := OldFileMode;
    end;
  except
    // Swallow any exceptions
    Result := E_FAIL;
  end;
  {$IOCHECKS ON}
end;

function TSITLicenseDlg.DisplayLicenseText(HOwner: HWND; X, Y: Integer;
  Caption, License: PChar): HRESULT;
  {Displays text in license dialog box.
    @param HOwner [in] Handle of window that owns the dialog box.
    @param X [in] Position of left of dialog box on screen.
    @param Y [in] Position of top of dialog box on screen.
    @param Caption [in] Dialog box's title.
    @param License [in] License text to be displayed.
    @return S_OK if user accepts or S_FALSE if user cancels.
  }
var
  DlgData: TSITLicenseDlgData;  // data passed to dlg box procedure
begin
  // Set up data to be passed to dlg box procedure
  DlgData.Caption := Caption;
  DlgData.License := License;
  DlgData.Bounds.Left := X;
  DlgData.Bounds.Top := Y;
  DlgData.Bounds.Right := X + fWidth;
  DlgData.Bounds.Bottom := Y + fHeight;
  DlgData.Style := fStyle;
  DlgData.HelpIntf := fHelpIntf;
  DlgData.HelpContext := fHelpContext;
  // Display the license dialog box using WIN API
  Result := DisplayLicenseDlg(HOwner, DlgData);
end;

procedure TSITLicenseDlg.RegisterHelp(HelpIntf: ISITLicenseDlgHelp;
  HelpContext: DWORD);
  {Registers a help "callback" object that dialog box calls when help button is
  clicked, along with help context number to be passed to callback method.
    @param HelpIntf [in] Reference to callback object.
    @param HelpContext [in] Help context to be passed to callback method.
  }
begin
  fHelpIntf := HelpIntf;
  fHelpContext := HelpContext;
end;

procedure TSITLicenseDlg.SetSize(Width, Height: Integer);
  {Sets the size of the dialog box. If 0 is specified for any parameter then
  that dimiension is unchanged. If any value that is less than min or greater
  than max dimension is specified then min or max size is used.
    @param Width [in] Width of dialog box.
    @param Height [in] Height of dialog box.
  }
begin
  if Width > 0 then
    fWidth := Min(GetSystemMetrics(SM_CXSCREEN), Max(cMinWidth, Width));
  if Height > 0 then
    fHeight := Min(GetSystemMetrics(SM_CYSCREEN), Max(cMinHeight, Height));
end;

procedure TSITLicenseDlg.SetStyle(AStyle: DWORD);
  {Sets the required style flags of the dialog box.
    @param AStyle [in] Required style.
  }
begin
  fStyle := AStyle;
end;


{ TSITLicenseDlgFactory }

class function TSITLicenseDlgFactory.CanUnload: WordBool;
  {Checks if server can be unloaded.
    @return True if server can be unloaded and False if not.
  }
begin
  Result := (LockCount = 0) and (InstanceCount = 0);
end;

constructor TSITLicenseDlgFactory.Create;
  {Class constructor. Increments count of object instances in server.
  }
begin
  inherited Create;
  // Increment global reference count
  InterlockedIncrement(InstanceCount);
end;

function TSITLicenseDlgFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HRESULT;
  {Creates a SITLicenseDlg object.
    @param UnkOuter [in] Reference to IUnknown interface of outer object of
      aggregation. Must be nil since aggregation not supported.
    @param IID [in] Interface to required object.
    @param Obj [out] Reference to created COM object. Will be set to nil.
    @return S_OK if object created and supports IID. CLASS_E_NOAGGREGATION if
      UnkOuter is not nil. Error return if object doesn't support IID.
  }
var
  Unk: IUnknown;  // interface instance that we create
begin
  // Init output pointer to nil - assumes failure
  Pointer(Obj) := nil;
  // Don't support aggregation
  if UnkOuter <> nil then
  begin
    Result := CLASS_E_NOAGGREGATION;
    Exit;
  end;
  // Create instance of object and return requested interface (gives error if
  // not supported.
  Unk := CreateSITLicenseDlg;
  Result := Unk.QueryInterface(IID, Obj);
end;

class function TSITLicenseDlgFactory.CreateSITLicenseDlg: ISITLicenseDlg;
  {Creates an SITLicenseDlg object instance that implements ISITLicenseDlg.
  For use when DLL is a private server and not to be created via COM.
    @return Instance of object.
  }
begin
  Result := TSITLicenseDlg.Create;
end;

destructor TSITLicenseDlgFactory.Destroy;
  {Class destructor. Decrements count of object instances in server.
  }
begin
  // Decrement global reference count
  InterlockedDecrement(InstanceCount);
  inherited Destroy;
end;

function TSITLicenseDlgFactory.LockServer(Lock: BOOL): HRESULT;
  {Locks and unlocks the server to prevent / permit unloading.
    @param Lock [in] True if server to be locked, False if to be unlocked.
    @return S_OK;
  }
begin
  if Lock then
    InterlockedIncrement(LockCount)
  else
    InterlockedDecrement(LockCount);
  Result := S_OK;
end;

end.

