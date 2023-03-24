{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmRegDataItemEdit.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialogue box that
                            allows a registry data item to be edited. Name, data
                            type and data value can be edited. Only limited data
                            validation is carried out. Inherits OK, cancel and
                            help buttons from generic OK dialogue box.
  @DEPENDENCIES             Requires following component:
                            + PJ Library TPJCBViewer component v1.1
                            + Custom SIBuilder TNewGroupBox v1.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 04/09/2000
      @COMMENTS             + Can now read binary data from a file
                            + Can now paste string data from clipboard\
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 24/12/2000
      @COMMENTS             Added support for path macros in string data types:
                            + Added check box for expanding file macros.
                            + Added buttons to insert a path macro and to insert
                              a file name.\
                            Also:
                            + Permitted empty strings to be accepted.
                            + Moved error message string literals to resource
                              strings.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 29/12/2002
      @COMMENTS             + Changed name of used unit UCtrlState to
                              UGUIHelper.
                            + Made value edit control use Courier New font and
                              added vertical scroll bar.
                            + Replaced TGroupBox control with TNewGroupBox
                              custom control that changes appearance when
                              disabled.
                            + Can now accept empty binary values.
                            + Replaced calls the MessageDlg with calls to
                              methods of the TMessageBox object.
    )
    @REVISION(
      @VERSION              1.4
      @DATE                 19/11/2003
      @COMMENTS             Changed reference to CBView component library unit
                            to new unit name: PJCBView.
    )
    @REVISION(
      @VERSION              1.5
      @DATE                 17/07/2006
      @COMMENTS             + Prevented accelerator characters being added to
                              Edit Path Macros menu items.
                            + Pasting data from clipboard now inserts text
                              rather than replaces it. No longer changes data
                              type to string. Ellipsis removed from paste button
                              caption.
    )
    @REVISION(
      @VERSION              1.6
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
 * The Original Code is FmRegDataItemEdit.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmRegDataItemEdit;


interface


uses
  // Delphi
  Buttons, Menus, Dialogs, StdCtrls, Controls, ExtCtrls, Classes, Registry,
  // delphiDabbler library components
  PJCBView,
  // Project specific components
  CmpGroupBox,
  // Project
  FmGenericOKDlg, URegData;

type

  {
  TRegDataItemEditDlg:
    Dialog box that allows a registry data item to be edited. Name, data type
    and data value can be edited. No data validation is carried out. Inherits
    OK, cancel and help buttons from generic OK dialog box.

    Inheritance: TRegDataItemEditDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TRegDataItemEditDlg = class(TGenericOKDlg)
    lblName: TLabel;
    edName: TEdit;
    dlgOpenBinary: TOpenDialog;
    cbViewer: TPJCBViewer;
    dlgInsertFileName: TOpenDialog;
    mnuPathMacro: TPopupMenu;
    gpDataType: TNewGroupBox;
    rbInteger: TRadioButton;
    rbString: TRadioButton;
    rbExpandString: TRadioButton;
    rbBinary: TRadioButton;
    chkExpandMacro: TCheckBox;
    gpValue: TNewGroupBox;
    edValue: TMemo;
    btnPaste: TButton;
    btnReadFile: TButton;
    btnInsertFile: TButton;
    btnInsertPathMacro: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnReadFileClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure cbViewerClipboardChanged(Sender: TObject);
    procedure DataTypeCtrlClick(Sender: TObject);
    procedure btnInsertFileClick(Sender: TObject);
    procedure btnInsertPathMacroMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private // properties
    fDataName: string;
    fDataValue: string;
    fDataType: TRegDataType;
    fExpandMacroPaths: Boolean;
    procedure SetDataName(const AName: string);
    procedure SetDataValue(const AValue: string);
    procedure SetDataType(const AType: TRegDataType);
    procedure SetExpandMacroPaths(const AValue: Boolean);
  private
    function ReadBinaryFileData(const FileName: string;
      DataObj: TRegData): Boolean;
      {Reads binary data from given file and stores in data object. Returns true
      if some data read, false otherwise}
    procedure InsertPathMacro(Sender: TObject);
      {Click event handler for path macro pop-up menu clicks}
    procedure UpdateControls;
      {Updates state of interdependent controls}
  public
    property DataName: string read fDataName write SetDataName;
      {The name of the registry entry}
    property DataValue: string read fDataValue write SetDataValue;
      {The value of the registry entry, as a string}
    property DataType: TRegDataType read fDataType write SetDataType;
      {The type of the value}
    property ExpandMacroPaths: Boolean
      read fExpandMacroPaths write SetExpandMacroPaths;
      {Whether we expand macros in string data types}
  end;


implementation


uses
  // Delphi
  SysUtils, ClipBrd, Windows,
  // Project
  UHelpContexts, UGUIHelper, UPathMacros, UMessageBox;

{$R *.DFM}

resourcestring
  // Error messages
  sErrNoValue = 'You must provide a value';
  sErrClipboardTextTooBig = 'Text on clipboard is too long';
  sErrEmptyFile = 'File "%s" is empty - no data read';
  sErrFileTooBig = 'File "%s" is too large - limit is %d bytes';
  sErrFileMissing = 'File "%s" doesn''t exist';


{ TRegDataItemEditDlg }

procedure TRegDataItemEditDlg.btnInsertFileClick(Sender: TObject);
  {Inserts any file chosen by user in the value memo control at the current
  cursor position}
begin
  inherited;
  if dlgInsertFileName.Execute then
    edValue.SelText := dlgInsertFileName.FileName;
end;

procedure TRegDataItemEditDlg.btnInsertPathMacroMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Displays pop-up menu that contains options to any of available path macros}
var
  PopUpPoint: TPoint;   // co-ords of where menu pops up (under button)
begin
  inherited;
  PopUpPoint := gpValue.ClientToScreen(
    Point(
      btnInsertPathMacro.Left,
      btnInsertPathMacro.Top + btnInsertPathMacro.Height
    )
  );
  mnuPathMacro.PopUp(PopUpPoint.X, PopUpPoint.Y);
end;

procedure TRegDataItemEditDlg.btnPasteClick(Sender: TObject);
  {Paste button click event handler - inserts clipboard text data into memo
  control}
begin
  // Check format is CF_TEXT - it should be
  if ClipBoard.HasFormat(CF_TEXT) then
    if Length(Clipboard.AsText) <
      TRegData.MaxDataSize - (Length(edValue.Text) - edValue.SelLength) then
      // Format is text and it's within length limits - paste it
      edValue.SelText := Clipboard.AsText
    else
      // Text is too long
      TMessageBox.Warning(Self, sErrClipboardTextTooBig);
end;

procedure TRegDataItemEditDlg.btnReadFileClick(Sender: TObject);
  {Read file button click event handler - gets file name from user and loads
  contents as binary data}
var
  DataObj: TRegData;    // object to hold bin data and covert to string
begin
  // Get file name
  if dlgOpenBinary.Execute then
  begin
    if FileExists(dlgOpenBinary.FileName) then
    begin
      // Create an unnamed data object to read value
      DataObj := TRegData.Create('');
      try
        if ReadBinaryFileData(dlgOpenBinary.FileName, DataObj) then
        begin
          // Value read OK - display in memo and check the binary radio button
          edValue.Lines.Text := DataObj.GetAsString;
          rbBinary.Checked := True;
        end;
      finally
        DataObj.Free;
      end;
    end
    else
      // File doesn't exist
      TMessageBox.ErrorFmt(Self, sErrFileMissing, [dlgOpenBinary.FileName]);
  end;
end;

procedure TRegDataItemEditDlg.cbViewerClipboardChanged(Sender: TObject);
  {OnClipboardChanged event handler for clipboard viewer - update state of
  paste button - we only support text format}
begin
  btnPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TRegDataItemEditDlg.DataTypeCtrlClick(Sender: TObject);
  {Update state of other dependent controls to reflect change in state of
  controls in data type group box}
begin
  inherited;
  UpdateControls;
end;

procedure TRegDataItemEditDlg.FormCreate(Sender: TObject);
  {Form creation event - assign help context}
var
  Counter: Integer;
  MacroNames: TStringList;
  MenuItem: TMenuItem;
begin
  inherited;
  // Record help contexts
  HelpContext := IDH_DLG_EDITREGVALUE;
  dlgOpenBinary.HelpContext := IDH_DLG_EDITREGVALUEOPEN;
  dlgInsertFileName.HelpContext := IDH_DLG_EDITREGVALUEINSERT;
  // Build path macro menu
  MacroNames := TStringList.Create;
  try
    PathMacros.GetMacroNames(MacroNames);
    for Counter := 0 to Pred(MacroNames.Count) do
    begin
      MenuItem := TMenuItem.Create(mnuPathMacro);
      MenuItem.OnClick := InsertPathMacro;
      MenuItem.Caption := MacroNames[Counter];
      mnuPathMacro.Items.Add(MenuItem);
    end;
  finally
    MacroNames.Free;
  end;
end;

procedure TRegDataItemEditDlg.FormShow(Sender: TObject);
  {Update controls when form is displayed}
begin
  inherited;
  UpdateControls;
end;

procedure TRegDataItemEditDlg.InsertPathMacro(Sender: TObject);
  {Click event handler for path macro pop-up menu clicks}
begin
  edValue.SelText := (Sender as TMenuItem).Caption;
end;

procedure TRegDataItemEditDlg.OKBtnClick(Sender: TObject);
  {Validates entries and sets properties before closing dlg box}
begin
  inherited;
  // Check if some data has been entered: must be data for integers
  if (edValue.Lines.Count = 0) and rbInteger.Checked then
  begin
    // error - tell user and don't close dlg
    TMessageBox.Error(Self, sErrNoValue);
    ModalResult := mrNone;
  end
  else
  begin
    // all in order - update properties
    if rbInteger.Checked  then
      fDataType := rdInteger
    else if rbString.Checked then
      fDataType := rdString
    else if rbExpandString.Checked then
      fDataType := rdExpandString
    else // Binary radio button checked
      fDataType := rdBinary;
    fDataName := edName.Text;
    fDataValue := edValue.Lines.Text;
    fExpandMacroPaths := chkExpandMacro.Enabled
      and chkExpandMacro.Checked;      // true flag only valid for strings
    // close dlg
    ModalResult := mrOK;
  end;
end;

function TRegDataItemEditDlg.ReadBinaryFileData(const FileName: string;
  DataObj: TRegData): Boolean;
  {Reads binary data from given file and stores in data object. Returns true if
  some data read, false otherwise}
var
  FileStream: TFileStream;    // stream attached to inout file
  Buffer: Pointer;            // buffer to store file's data
begin
  // Open file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := (FileStream.Size > 0) and
      (FileStream.Size <= TRegData.MaxDataSize);
    if Result then
    begin
      // File has contents that are not too large - read them
      // allocate buffer to store contents
      GetMem(Buffer, FileStream.Size);
      try
        // read contents into buffer
        FileStream.ReadBuffer(Buffer^, FileStream.Size);
        // store in data object
        DataObj.SetBinary(Buffer^, FileStream.Size);
      finally
        // Tidy up
        FreeMem(Buffer, FileStream.Size);
      end;
    end
    else if (FileStream.Size = 0) then
      TMessageBox.WarningFmt(Self, sErrEmptyFile, [dlgOpenBinary.FileName])
    else {FileStream.Size > CBinarySizeLimit}
      TMessageBox.WarningFmt(
        Self, sErrFileTooBig, [dlgOpenBinary.FileName, TRegData.MaxDataSize]
      );
  finally
    FileStream.Free;
  end;
end;

procedure TRegDataItemEditDlg.SetDataName(const AName: string);
  {Write access method for DataName property - records name and displays it in
  edit box}
begin
  fDataName := AName;
  edName.Text := AName;
end;

procedure TRegDataItemEditDlg.SetDataType(const AType: TRegDataType);
  {Write access method for DataType edit box - records type and checks
  appropriate radio button}
begin
  // Check required radion button
  fDataType := AType;
  case AType of
    rdInteger: rbInteger.Checked := True;
    rdString: rbString.Checked := True;
    rdExpandString: rbExpandString.Checked := True;
    rdBinary: rbBinary.Checked := True;
  end;
  // Update expand macro paths check box
  UpdateControls;
end;

procedure TRegDataItemEditDlg.SetDataValue(const AValue: string);
  {Write access method for DataValue property - records value as string and
  displays it in edit box}
begin
  fDataValue := AValue;
  edValue.Lines.Text := AValue;
end;

procedure TRegDataItemEditDlg.SetExpandMacroPaths(const AValue: Boolean);
  {Write access method for ExpandMacroPaths property - records flag and checks/
  unchecks check box}
begin
  fExpandMacroPaths := AValue;
  chkExpandMacro.Checked := AValue;
end;

procedure TRegDataItemEditDlg.UpdateControls;
  {Updates state of interdependent controls}
begin
  EnableCtrls([chkExpandMacro, btnInsertFile],
    rbString.Checked or rbExpandString.Checked);
  EnableCtrls([btnInsertPathMacro],
    chkExpandMacro.Checked and chkExpandMacro.Enabled);
end;

end.
