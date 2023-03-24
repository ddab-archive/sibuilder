{
 * FmAbout.pas
 *
 * Implements an about dialog box for SIBuilder that displays information about
 * the SITools suite, including a list of included files. The dialog box
 * contains clickable hot links and additional info about files can appear in
 * pop-up windows.
 *
 * The following components are required:
 *   + DelphiDabbler TPJVersionInfo component v3.1 or later.
 *   + SIBuilder specific component: THotText 1.0
 *
 * v1.0 of 29 Dec 2000  - Original version.
 * v1.1 of 19 Nov 2003  - Updated hot text handling code to use new hot text
 *                        control rather than rich edit controls and associated
 *                        active RTF classes.
 *                      - Changed processing of hot linked text to handle hot
 *                        text code rather than RTF.
 *                      - Removed reference to removed UActiveRTFWrapper unit.
 * v1.2 of 23 Feb 2008  - Replaced usage of Help.inc, ResIds.inc, Registry.inc
 *                        and FileNames.inc include files with UHelpContexts,
 *                        UResources, URegistry and UFileNames units.
 *                      - Removed attempt to load information about Shared
 *                        programs since there are none.
 *                      - Changed to read program information from HKLM
 *                        instead of HKCU in registry.
 *                      - Made changes to use of registry constants re changes
 *                        in URegistry unit.
 *                      - Moved some string literals to resource strings.
 * v1.3 of 11 Apr 2008  - Resized hot text area to accomadate size of text on
 *                        Vista.
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
 * The Original Code is FmAbout.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmAbout;


interface


uses
  // Delphi
  Windows, ComCtrls, StdCtrls, Graphics, ExtCtrls, Controls, Classes, Buttons,
  Messages,
  // DelphiDabbler Library
  PJVersionInfo,
  // Project specific components
  CmpHotText,
  // Project
  FmGenericViewDlg, UInfoWdw;

type

  {
  TProgDescRec:
    Record storing information about a program that is used to display info
    about SIBuilder programs in about box list view and pop-up windows.
  }
  TProgDescRec = record
    FileName: string;     // program file name (list view and popup window)
    Version: string;      // file version (list view and popup window)
    Description: string;  // description of program (list view)
    Copyright: string;    // copyright info for program (popup window)
    Credits: string;      // credits relating to program (popup window)
    Category: string;     // program's category (popup window)
  end;


  {
  PProgDescRec:
    Pointer to a TProgDescRec record.
  }
  PProgDescRec = ^TProgDescRec;


  {
  TProgDescWdw:
    Implements a popup window that is used to display additional information
    about a SIBuilder program. Information is provided by setting ProgDesc
    property.
  }
  TProgDescWdw = class(TInfoWdw)
  private
    fProgDesc: TProgDescRec;
      {Value of ProgDesc property}
    procedure SetProgDesc(const Value: TProgDescRec);
      {Write access method for ProgDesc property. Records new value and sets lines
      of info window object from it.
        @param Value [in] New property value.
      }
  public
    property ProgDesc: TProgDescRec read fProgDesc write SetProgDesc;
      {Record that provides information to be displayed in window}
  end;


  {
  TAboutDlg:
    Implements an about box providing general information about the program and
    which lists each program file included in suite. Also supports hot links
    which lead to further info.
  }
  TAboutDlg = class(TGenericViewDlg)
    bvlTop: TBevel;
    imgIcon: TImage;
    lblProductName: TLabel;
    lvManifest: TListView;
    pnlTop: TPanel;
    viAbout: TPJVersionInfo;
    viFiles: TPJVersionInfo;
    htInfo: THotText;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvManifestClick(Sender: TObject);
    procedure lvManifestDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure lvManifestMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lvManifestDeletion(Sender: TObject; Item: TListItem);
    procedure htInfoLinkClick(Sender: TObject; const Cmd: String;
      const Rect: TRect);
  private
    fCurHotItem: TListItem;
      {Reference to list item that is currently hot (i.e. the list item who's
      file name is under cursor: nil if no hot item}
    fPopupWdw: TProgDescWdw;
      {Popup window component: used to display additional info for a program
      displayed in list view}
    procedure ChangeHotItem(const NewHotItem: TListItem);
      {Changes the hot item in the list view. Any previous hot item is
      unhighlighted while new item is highlighted unless it is nil.
        @param NewHotItem [in] List item to be highlighted as "hot".
      }
    procedure LoadListView;
      {Loads list view control with details of each program included with
      SITools. Since list view is owner drawn, we simply store reference to each
      program's data record in list view rather than setting caption and sub
      items.
      }
  private // message handlers
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {Handles message triggered when mouse leaves dialog box. Causes any
      existing hot item in list view to be unhighlighted and restores arrow
      cursor.
        @param Msg [in/out] Not used.
      }
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
      {WM_ACTIVATE message handler. Forces redraw of list view when app comes
      back to front since.
        @param Msg [in/out] Activation message.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, Math, Registry,
  // Project
  UCompressionMgr, UCmdDispatcher, UFileProcs, URegistry, UResources,
  UFileNames, UHelpContexts;


{$R *.DFM}


resourcestring
  // Display text
  sDeflatorDesc   = 'Compressor Add-in (deflator)';
  sInflatorDesc   = 'Compressor Add-in (inflator)';
  sCoreDesc       = 'Core component';
  sSharedDesc     = 'Shared component';
  sBonusDesc      = 'Bonus component';
  sNoInfo         = 'No information';


{ TAboutDlg }

procedure TAboutDlg.ChangeHotItem(const NewHotItem: TListItem);
  {Changes the hot item in the list view. Any previous hot item is unhighlighted
  while new item is highlighted unless it is nil.
    @param NewHotItem [in] List item to be highlighted as "hot".
  }

  //----------------------------------------------------------------------------
  procedure InvalidateHotItem(const Item: TListItem);
    {Invalidates rectangle of label associated with a hot list item, causing it
    to be repainted.
      @param Item [in] List item to have label invalidated.
    }
  var
    R: TRect; // rectangle being invalidated
  begin
    R := Item.DisplayRect(drLabel);
    InvalidateRect(lvManifest.Handle, @R, False);
  end;
  //----------------------------------------------------------------------------

var
  OldHotItem: TListItem;  // reference to old hot item
begin
  // Only make changes if hot item has changed
  if fCurHotItem <> NewHotItem then
  begin
    // Record old item and set new one: record of required by paint method that
    // is triggered by invalidate calls
    OldHotItem := fCurHotItem;
    fCurHotItem := NewHotItem;
    // If there was an old item cause it to be redisplayed in plain state
    if Assigned(OldHotItem) then
      InvalidateHotItem(OldHotItem);
    // If there is a new item cause it to be redisplayed in highlighted state
    if Assigned(NewHotItem) then
      InvalidateHotItem(NewHotItem);
  end;
end;

procedure TAboutDlg.CMMouseLeave(var Msg: TMessage);
  {Handles message triggered when mouse leaves dialog box. Causes any existing
  hot item in list view to be unhighlighted and restores arrow cursor.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  ChangeHotItem(nil);
  lvManifest.Cursor := crDefault;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Sets up dialog box.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Record help topic displayed when F1 is pressed
  HelpContext := IDH_DLG_ABOUT;
  // Display SIBuilder's product name from version info at top of client area
  lblProductName.Caption := viAbout.ProductName;
  // Size and position controls
  // make top bevel stretch across form
  bvlTop.Width := pnlTop.Width;
  // Create popup window used to provide additional info about program files
  // this window will free itself when form destroyed
  fPopupWdw := TProgDescWdw.Create(Self);
  fPopupWdw.ColumnWidth := 80;
end;

procedure TAboutDlg.FormShow(Sender: TObject);
  {Form show event handler. Loads hot text from stream and replaces placeholder
  tokens with info from program's version information. Also loads details of
  SITools suite programs into list view.
    @param Sender [in] Not used.
  }

  // ---------------------------------------------------------------------------
  function ResourceToString: string;
    {Load hot text code to be displayed in dialog from program resources into
    string.
      @return String containing hot text code.
    }
  var
    SS: TStringStream;    // stream used to create string
    RS: TResourceStream;  // stream used to access program's resources
  begin
    RS := nil;
    // Create required streams
    SS := TStringStream.Create('');
    try
      // loads required hottext code
      RS := TResourceStream.CreateFromID(HInstance, cAboutInfoResId, RT_RCDATA);
      // Copy stream into string
      SS.CopyFrom(RS, 0);
      Result := SS.DataString
    finally
      FreeAndNil(RS);
      FreeAndNil(SS);
    end;
  end;

  procedure ReplaceToken(var Code: string; const Token, Replacement: string);
    {Replaces given a Token hot text code a replacement value.
      @param Code [in/out] Hot text code to have token replaced. Code containing
        token passed in (in form [%Token%]). Updated code passed out.
      @param Token [in] Token to be replaced, in form.
      @param Replacement [in] Text to replace token.
    }
  begin
    Code := StringReplace(
      Code,
      '[%' + Token + '%]',
      Replacement,
      [rfReplaceAll, rfIgnoreCase]
    );
  end;
  // ---------------------------------------------------------------------------

var
  HotTextCode: string;  // string containing hot text code displayed in dlg
begin
  inherited;
  // Load hot text code from resources
  HotTextCode := ResourceToString;
  // Replace placeholder tokens with info from SIBuilder's version info
  ReplaceToken(HotTextCode, 'VI_ProductName', viAbout.ProductName);
  ReplaceToken(HotTextCode, 'VI_ProductVersion', viAbout.ProductVersion);
  ReplaceToken(HotTextCode, 'VI_LegalCopyright', viAbout.LegalCopyright);
  ReplaceToken(HotTextCode, 'VI_Comments', viAbout.Comments);
  // Load revised hot text code into hot text control
  htInfo.Code := HotTextCode;
  // Load details of SITools programs into list view
  LoadListView;
end;

procedure TAboutDlg.htInfoLinkClick(Sender: TObject; const Cmd: String;
  const Rect: TRect);
  {Handles clicks on hot text links. Dispatches the link's command.
    @param Sender [in] Not used.
    @param Cmd [in] Command from link to be dispatched.
  }
begin
  TCmdDispatcher.DispatchCmd(Cmd);
end;

procedure TAboutDlg.LoadListView;
  {Loads list view control with details of each program included with SITools.
  Since list view is owner drawn, we simply store reference to each program's
  data record in list view rather than setting caption and sub items.
  }

  // ---------------------------------------------------------------------------
  function InitProgramDesc: TProgDescRec;
    {Intialises a program description record to nul values.
    }
  begin
    Result.FileName := '';
    Result.Version := '';
    Result.Description := '';
    Result.Copyright := '';
    Result.Credits := '';
    Result.Category := '';
  end;

  procedure CreateItem(const FileSpec: string; Info: TProgDescRec);
    {Fills in program description record for program with file spec and creates
    a list view item associated with the program.
      @param FileSpec [in] File spec of program.
      @param Info [in] Information about program.
    }
  var
    PRec: ^TProgDescRec;  // pointer to description for program stored in lv
    ProgPath: string;     // path the program
    FileName: string;     // fully specified file name for program
    V: TPJVersionNumber;  // version information for program
    Item: TListItem;      // the new list item
  begin
    // Get full path to given program (FileSpec may be either a full file spec
    // or a simple file name which is assumed to be one of two folders)
    ProgPath := ExtractFilePath(ParamStr(0));
    if ExtractFileName(FileSpec) = FileSpec then
    begin
      // file name on its own: try program folder and Install sub folder
      FileName := ProgPath + FileSpec;
      if not FileExists(FileName) then
        FileName := ProgPath + cInstallFolder +'\' + FileSpec;
      if not FileExists(FileName) then
        FileName := FileSpec;
    end
    else
      // full file spec was povided
      FileName := FileSpec;
    // Create program info record and intialise it to given value
    New(PRec);
    PRec^ := Info;
    PRec^.FileName := ExtractFileName(FileName);
    // We use version information for program to fill in any gaps in program
    // description record
    // try to read program's version info
    viFiles.FileName := FileName;
    if viFiles.HaveInfo then
    begin
      // we have version info: try to fill in any gaps
      V := viFiles.FileVersionNumber;
      if PRec^.Version = '' then
        PRec^.Version := Format('v%d.%d.%d.%d', [V.V1, V.V2, V.V3, V.V4]);
      if PRec^.Description = '' then
        PRec^.Description := viFiles.FileDescription;
      if PRec^.Copyright = '' then
        PRec^.Copyright := viFiles.LegalCopyright;
      if PRec^.Credits = '' then
        PRec^.Credits := viFiles.StringFileInfo['Credits'];
    end;
    // Create list item
    Item := lvManifest.Items.Add;
    Item.Data := PRec;
  end;

  procedure GetRegisteredPrograms(const RegKey, Category: string);
    {Gets list of programs in a category from registry and adds item to list
    view for each program.
      @param RegKey [in] Registry key where programs are listed.
      @param Category [in] Desired category of programs.
    }
  var
    Keys: TStringList;  // registered program keys
    KeyIdx: Integer;    // index into key list
    Reg: TRegistry;     // object used to access registry
    Rec: TProgDescRec;  // program description record
    Path: string;       // path to a specified file
    FileSpec: string;   // full file spec to specified file

    // -------------------------------------------------------------------------
    function GetStringValue(const Name: string): string;
      {Fetches a named string value from registry under current key.
        @param Name [in] Name of required registry value.
        @return Value from registry or '' if value doesn't exist.
      }
    begin
      if Reg.ValueExists(Name) then
        Result := Reg.ReadString(Name)
      else
        Result := '';
    end;
    // -------------------------------------------------------------------------

  begin
    // Open required registry key
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(RegKey) then
      begin
        // Get list of subkey names
        Keys := TStringList.Create;
        try
          Reg.GetKeyNames(Keys);
          // Set up program desc record and create list view entry for each
          // program for which there is a key
          for KeyIdx := 0 to Pred(Keys.Count) do
          begin
            // open program subkey
            Reg.CloseKey;
            Reg.OpenKey(RegKey + '\' + Keys[KeyIdx], False);
            // build program desc entry from registry
            Rec := InitProgramDesc;
            Rec.Category := Category;
            Rec.Version := GetStringValue('FileVersion');
            Rec.Description := GetStringValue('Description');
            Rec.Copyright := GetStringValue('Copyright');
            Rec.Credits := GetStringValue('Credits');
            // build file spec from key name and any specified path
            Path := GetStringValue('Path');
            if Path <> '' then
              FileSpec := MakePathName(Path) + Keys[KeyIdx]
            else
              FileSpec := Keys[KeyIdx];
            // create list view item for program
            CreateItem(FileSpec, Rec);
          end;
        finally
          FreeAndNil(Keys);
        end;
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;

  procedure GetCompressorInfo;
    {Gets information about each registered compressor and add entries in list
    view for each one.
    }
  var
    CmpMgr: TCompressionMgr;    // reference to compression manager object
    CmpItem: TCompressionItem;  // reference to compressor
    CmpIdx: Integer;            // index into list of compressors
    Rec: TProgDescRec;          // program description record
  begin
    // Get list of compressors (1st item is (none)
    CmpMgr := TCompressionMgr.Create;
    try
      for CmpIdx := 1 to Pred(CmpMgr.Count) do   // miss first item => (none)
      begin
        // Add entry to list view for each DLL in compressor
        CmpItem := CmpMgr[CmpIdx];
        // create desc rec and list item for inflator DLL
        Rec := InitProgramDesc;
        Rec.Category := sInflatorDesc;
        Rec.Credits := CmpItem.Credits;
        CreateItem(CmpItem.Inflator, Rec);
        // create desc rec and list item for deflator DLL
        Rec := InitProgramDesc;
        Rec.Category := sDeflatorDesc;
        Rec.Credits := CmpItem.Credits;
        CreateItem(CmpItem.Deflator, Rec);
      end;
    finally
      FreeAndNil(CmpMgr);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  // Create list view items for all registered programs / DLLs
  // core components
  GetRegisteredPrograms(cSIBCoreComponentsKey, sCoreDesc);
  // bonus programs
  GetRegisteredPrograms(cSIBExtrasKey, sBonusDesc);
  // compressors
  GetCompressorInfo;
end;

procedure TAboutDlg.lvManifestClick(Sender: TObject);
  {OnClick event handler for file manifest list view. Displays popup window
  giving additional info about selected program/DLL.
    @param Sender [in] Not used.
  }
var
  ListItem: TListItem;  // reference to selected list item
  PRec: ^TProgDescRec;  // pointer to program data associated with list item
  ItemRect: TRect;      // rectangle occupied by selected item's label
  ItemTopLeft: TPoint;  // top left point of above rectangle
begin
  inherited;
  // Get ref to selected list item
  ListItem := lvManifest.Selected;
  // Check if any selected list item is hotlink (cursor is pointing hand)
  if Assigned(ListItem) and (lvManifest.Cursor = crHandPoint) then
  begin
    // This is hot link: display popup with further info
    // get hold of program desc and passed to popup window
    PRec := ListItem.Data;
    fPopupWdw.ProgDesc := PRec^;
    // get location where popup window to appear: i.e. level with top and to
    // right of program name in list view
    ItemRect := ListItem.DisplayRect(drLabel);
    ItemTopLeft := lvManifest.ClientToScreen(ItemRect.TopLeft);
    Inc(ItemTopLeft.X, lvManifest.StringWidth(PRec^.FileName) + 8);
    // display window (no text provided: it comes from fPopupWdw.ProgramDesc)
    fPopupWdw.DisplayWdw(ItemTopLeft.X, ItemTopLeft.Y);
  end;
end;

procedure TAboutDlg.lvManifestDeletion(Sender: TObject; Item: TListItem);
  {OnDeletion event handler for list view. Frees memory used to store
  information about program.
    @param Sender [in] Not used.
    @param Item [in] Item being deleted.
  }
var
  PRec: PProgDescRec;  // pointer to program data associated with list item
begin
  inherited;
  PRec := Item.Data;
  if Assigned(PRec) then
    Dispose(PRec);
end;

procedure TAboutDlg.lvManifestDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for list view. We draw info about program associated
  with this list item using program info record.
    @param Sender [in] Not used.
    @param Item [in] List item to draw.
    @param Rect [in] Bounding rectangle of list item.
    @param State [in] Draw state for list item.
  }
const
  cBackgrounds: array[Boolean] of TColor = (clWindow, $F0EEF0);
    // alternate background colours for lines in list view
var
  RF,                   // rectangle in which to display file name
  RV,                   // rectangle in which to display version number
  RD: TRect;            // rectangle in which to display description
  PRec: ^TProgDescRec;  // pointer to program data associated with list item
begin
  inherited;
  with lvManifest do
  begin
    // Get pointer to program data
    PRec := Item.Data;

    // Adjust size of rectangle to fill whole of client width, regardless of
    // column sizes
    Rect.Right := ClientWidth - Rect.Left;

    // Fill item's line with appropriate background colour (one of two colours
    // is used on alternate line)
    Canvas.Brush.Color := cBackgrounds[Odd(Item.Index)];
    Canvas.FillRect(Rect);

    // Calculate text display rectangles
    // file name rectangle (on left of line)
    RF := Rect;
    Inc(RF.Left, 2);
    RF.Right := RF.Left + 96;
    // version info rectangle (to right of file name)
    RV := Rect;
    RV.Left := RV.Left + 98;
    RV.Right := RV.Left + 58;
    // description info rectangle (whole of remainder of line)
    RD := Rect;
    RD.Left := RV.Right + 2;
    RD.Right := RD.Right - 2;

    // Display file name
    // .. if item is current hot item highlight file in red, else make it blue
    if fCurHotItem <> Item then
      Canvas.Font.Color := clBlue
    else
      Canvas.Font.Color := clRed;
    // underline item
    Canvas.Font.Style := [fsUnderline];
    // now draw the text
    DrawTextEx(Canvas.Handle, PChar(PRec.FileName), -1, RF,
      DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT, nil);

    // Display version number
    Canvas.Font.Color := clWindowText;
    Canvas.Font.Style := [];
    DrawTextEx(Canvas.Handle, PChar(PRec.Version), -1, RV,
      DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT, nil);

    // Display description
    if PRec^.Description <> '' then
      // we have info: display the description
      DrawTextEx(Canvas.Handle, PChar(PRec.Description), -1, RD,
        DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT, nil)
    else
    begin
      // we have no description: say so
      Canvas.Font.Style := [fsItalic];
      DrawTextEx(Canvas.Handle, PChar(sNoInfo), -1, RD,
        DT_VCENTER or DT_END_ELLIPSIS or DT_LEFT, nil);
    end;
  end;
end;

procedure TAboutDlg.lvManifestMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  {OnMouseMove event handler for list view. Highlights/unhighlights hot items as
  mouse passes over them.
    @param Sender [in] Not used.
    @param Shift [in] Shift state of keys.
    @param X [in] X co-ordinate of mouse.
    @param Y [in] Y co-ordinate of mouse.
  }
var
  HotItem: TListItem;   // list item that is to be highlighted (if any)
  R: TRect;             // "hot" rectangle occupied by item text
  PRec: ^TProgDescRec;  // pointer to program data associated with list item
begin
  inherited;
  // Get reference to item under cursor (nil if none)
  HotItem := lvManifest.GetItemAt(X, Y);
  if Assigned(HotItem) then
  begin
    // We have item: check if it has additional info & therefore "hot"
    PRec := HotItem.Data;
    // Item is "hot" if mouse over text
    // get "hot" rectangle
    R := HotItem.DisplayRect(drLabel);
    R.Right := R.Left + lvManifest.StringWidth(PRec^.FileName);
    // we highlight only if mouse pointer is in "hot" rectangle
    if not PtInRect(R, Point(X, Y)) then
      HotItem := nil;
  end;
  // We set cursor to hand if we have a hot item
  if Assigned(HotItem) then
    lvManifest.Cursor := crHandPoint
  else
    lvManifest.Cursor := crDefault;
  // We now update the current hot item
  // this causes item to be repainted, which in turn highlight/unhighlights
  ChangeHotItem(HotItem);
end;

procedure TAboutDlg.WMActivate(var Msg: TWMActivate);
  {WM_ACTIVATE message handler. Forces redraw of list view when app comes back
  to front since.
    @param Msg [in/out] Activation message.
  }
begin
  inherited;
  if Msg.Active <> WA_INACTIVE then
    lvManifest.Invalidate;
end;


{ TProgDescWdw }

resourcestring
  // Header descriptions for program description window
  sFileHeader = 'File:';
  sVersionHeader = 'Version:';
  sCategoryHeader = 'Category:';
  sDescriptionHeader = 'Description:';
  sCopyrightHeader = 'Copyright:';
  sCreditsHeader = 'Credits:';

procedure TProgDescWdw.SetProgDesc(const Value: TProgDescRec);
  {Write access method for ProgDesc property. Records new value and sets lines
  of info window object from it.
    @param Value [in] New property value.
  }
var
  DescStrings: TStringList; // description of program
begin
  // Record new value
  fProgDesc := Value;
  // Create a string list to hold new values
  DescStrings := TStringList.Create;
  try
    // Add descriptions for properties that are always displayed
    DescStrings.Values[sFileHeader] := Value.FileName;
    DescStrings.Values[sVersionHeader] := Value.Version;
    DescStrings.Values[sCategoryHeader] := Value.Category;
    DescStrings.Values[sDescriptionHeader] := Value.Description;
    if Value.Copyright <> '' then
      // only display copyright item if there is text
      DescStrings.Values[sCopyrightHeader] := Value.Copyright;
    if Value.Credits <> '' then
      // only display credits item if there is text
      DescStrings.Values[sCreditsHeader] := Value.Credits;
    // Store new values in Contents property
    Self.Contents := DescStrings;
  finally
    FreeAndNil(DescStrings);
  end;
end;

end.

