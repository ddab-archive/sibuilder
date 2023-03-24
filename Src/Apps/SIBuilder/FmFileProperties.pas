{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmFileProperties.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialogue box that
                            displays a project file's The dialogue box class
                            inherits from the generic view dialogue box and
                            therefore displays only Done and Help buttons.
  @DEPENDENCIES             Requires following components:
                            + TPJVersionInfo v2.1
                            + Custom SIBuilder TNewGroupBox v1.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 28/08/2000
      @COMMENTS             Substantial re-write. There were three major
                            changes:
                            + Added facility to display version information for
                              files that contain it
                            + Replaced read only edit boxes with static text
                              controls and condensed layout
                            + Removed all properties and replaced with a single
                              public static method to display the dlg box with
                              info for a given file.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 01/04/2002
      @COMMENTS             + Now uses latest release of TPJVersionInfo
                              component - renamed unit reference.
                            + Moved string literals to resource strings.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 29/12/2002
      @COMMENTS             + Fixed bug that was displaying various labels
                              differently when version info group was disbled.
                            + General coding of method used to display version
                              info was revised and improved.
                            + Replaced TGroupBox control with TNewGroupBox
                              custom control that changes appearance when
                              disabled.
    )
    @REVISION(
      @VERSION              2.3
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
 * The Original Code is FmFileProperties.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmFileProperties;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // PJSoft components
  PJVersionInfo,
  // Project specific components
  CmpGroupBox,
  // Project
  FMGenericViewDlg;


type
  {
  TFilePropertiesDlg:
    Dialog box that displays a file's properties - it inherits from the generic
    view dialog box and therefore displays only Done and Help buttons.

    Inheritance: TFilePropertiesDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TFilePropertiesDlg = class(TGenericViewDlg)
    lblFileName: TLabel;
    lblSourcePath: TLabel;
    lblDate: TLabel;
    lblSize: TLabel;
    lblAttr: TLabel;
    viFile: TPJVersionInfo;
    stFileName: TStaticText;
    stSourcePath: TStaticText;
    stDate: TStaticText;
    stSize: TStaticText;
    stAttr: TStaticText;
    gpVerInfo: TNewGroupBox;
    lblFileVerNo: TLabel;
    lblTextVerInfo: TLabel;
    lblProductVerNo: TLabel;
    edVerDesc: TMemo;
    lbVerNames: TListBox;
    stFileVerNo: TStaticText;
    stProductVerNo: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure lbVerNamesClick(Sender: TObject);
  private
    procedure DisplayDate(const Value: Integer);
      {Displays given date, converted to text. (or displays 'Unknown' if value
      is -1}
    procedure DisplayPath(const Value: string);
      {Displays given path}
    procedure DisplaySize(const Value: LongInt);
      {Displays given file size (or displays 'Unknown' if size is -1}
    procedure DisplayFileName(const Value: string);
      {Displays given file name}
    procedure DisplayAttributes(const Value: Integer);
      {Displays given file attributes (or displays 'Unknown' if Value is -1)}
    procedure DisplayVersionInfo(const FileName: string);
      {Displays version information for given file (if any)}
  public
    class procedure DisplayFileInfo(const Owner: TComponent;
      const FullFileName: string);
      {Display the file properties dlg box populated with relevant info}
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UHelpContexts, UGUIHelper, UFileProcs;

{$R *.DFM}

resourcestring
  // Display strings
  sUnknown = 'Unknown';
  sBytes = 'bytes';


{ TFilePropertiesDlg }

procedure TFilePropertiesDlg.DisplayAttributes(const Value: Integer);
  {Displays given file attributes (or displays 'Unknown' if Value is -1)}
const
  {Table mapping attribute values onto the relevant char codes}
  cAttrs : array[0..6] of record
    Attr: Integer;        // attribute value
    ID: Char;             // attribute code
  end = (
    (Attr: faReadOnly; ID: 'R'),
    (Attr: faHidden; ID: 'H'),
    (Attr: faSysFile; ID: 'S'),
    (Attr: faVolumeId; ID: 'V'),
    (Attr: faDirectory; ID: 'D'),
    (Attr: faArchive; ID: 'A'),
    (Attr: faAnyFile; ID: 'X')
  );
var
  AttrStr: string;      // used to build up string of attributes to display
  I: Integer;           // loops thru attribute table
begin
  // Record value
  if Value <> -1 then
  begin
    // We have attributes - build string of codes
    AttrStr := '';
    for I := 0 to 6 do
      if cAttrs[I].Attr and Value = cAttrs[I].Attr then
        AttrStr := AttrStr + cAttrs[I].ID;
  end
  else
    // We don't know attributes
    AttrStr := sUnknown;
  // Display attributes info
  stAttr.Caption := AttrStr;
end;

procedure TFilePropertiesDlg.DisplayDate(const Value: Integer);
  {Displays given date, converted to text. (or displays 'Unknown' if value is
  -1}
begin
  if Value <> -1 then
    stDate.Caption := DateTimeToStr(FileDateToDateTime(Value))
  else
    stDate.Caption := sUnknown;
end;

class procedure TFilePropertiesDlg.DisplayFileInfo(const Owner: TComponent;
  const FullFileName: string);
  {Display the file properties dlg box populated with relevant info}
var
  Dlg: TFilePropertiesDlg;    // the dlg box instance
begin
  // Create the dlg box
  Dlg := TFilePropertiesDlg.Create(Owner);
  try
    // Populate controls with file's properties
    Dlg.DisplayFileName(ExtractFileName(FullFileName));
    Dlg.DisplayPath(ExtractFilePath(FullFileName));
    Dlg.DisplayDate(GetFileDate(FullFileName));
    Dlg.DisplaySize(SizeOfFile(FullFileName));
    Dlg.DisplayAttributes(FileGetAttr(FullFileName));
    Dlg.DisplayVersionInfo(FullFileName);
    // Display dlg box
    Dlg.ShowModal;
  finally
    // Finished - destroy dlg box
    Dlg.Free;
  end;
end;

procedure TFilePropertiesDlg.DisplayFileName(const Value: string);
  {Displays given file name}
begin
  stFileName.Caption := Value;
end;

procedure TFilePropertiesDlg.DisplayPath(const Value: string);
  {Displays given path}
begin
  stSourcePath.Caption := Value;
end;

procedure TFilePropertiesDlg.DisplaySize(const Value: LongInt);
  {Displays given file size (or displays 'Unknown' if size is -1}
begin
  if Value <> -1 then
    stSize.Caption := IntToStr(Value) + ' ' + sBytes
  else
    stSize.Caption := sUnknown;
end;

procedure TFilePropertiesDlg.DisplayVersionInfo(const FileName: string);
  {Displays version information for given file (if any)}

  // ---------------------------------------------------
  function VerNum(V1, V2, V3, V4: Integer): string;
    {Formats given version number as string}
  begin
    Result := Format('%d.%d.%d.%d', [V1, V2, V3, V4]);
  end;
  // ---------------------------------------------------

begin
  // Get the VersionInfo component to get the information for the given file
  viFile.FileName := FileName;
  // Enable / disable controls according to if ver info could be read. We
  // exclude labels in group box: this way group box greys them (see below)
  // rather than using labels' 3D disbaled look (this is now like other ctrls)
  EnableCtrls([gpVerInfo, stFileVerNo, stProductVerNo, lbVerNames, edVerDesc],
    viFile.HaveInfo, True);
  // If we got ver info, display it
  if viFile.HaveInfo then
  begin
    // display file and product numbers
    with viFile.FileVersionNumber do
      stFileVerNo.Caption := VerNum(V1, V2, V3, V4);
    with viFile.ProductVersionNumber do
      stProductVerNo.Caption := VerNum(V1, V2, V3, V4);
    // display string info assoc with first item in list box
    lbVerNames.ItemIndex := 0;
    lbVerNamesClick(Self);
  end;
end;

procedure TFilePropertiesDlg.FormCreate(Sender: TObject);
  {Form creation event handler override - assign help context}
begin
  inherited;
  HelpContext := IDH_DLG_FILEPROPERTIES;
end;

procedure TFilePropertiesDlg.lbVerNamesClick(Sender: TObject);
  {OnClick event handler for VerNamesList control - displays the relevant ver
  info for the related item in the memo control}
begin
  if viFile.HaveInfo then
  begin
    // There is version info for the current file - display appropriate text
    case lbVerNames.ItemIndex of
      0: edVerDesc.Text := viFile.Comments;
      1: edVerDesc.Text := viFile.LegalCopyright;
      2: edVerDesc.Text := viFile.CompanyName;
      3: edVerDesc.Text := viFile.FileDescription;
      4: edVerDesc.Text := viFile.FileVersion;
      5: edVerDesc.Text := viFile.InternalName;
      6: edVerDesc.Text := viFile.Language;
      7: edVerDesc.Text := viFile.OriginalFileName;
      8: edVerDesc.Text := viFile.ProductName;
      9: edVerDesc.Text := viFile.ProductVersion;
    end;
  end
  else
    // There is no version info - ensure memo control is cleared
    edVerDesc.Clear;
end;

end.
