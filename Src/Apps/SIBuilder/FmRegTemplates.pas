{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmRegTemplates.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It implements a dialog box that can be
                            used for selecting and editing registry key
                            templates.
  @DEPENDENCIES             The following custom SIBuilder only components are
                            required:
                            + THotText 1.0\
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 19/11/2003
      @COMMENTS             + Deleted reference to removed UActiveRTFWrapper
                              unit.
                            + Updated hot text code to use new hot text controls
                              rather than old rich edit control and active RTF
                              classes.
                            + Changed template code generation to generate hot
                              text code rather than RTF.
                            + The dialog's hot linked help message code is now
                              stored in the hot text component properties rather
                              than loaded from resources.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 28/11/2003
      @COMMENTS             Refactoring: Deleted reference to unused unit.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 20/02/2008
      @COMMENTS             + Fixed bug that was causing lexer parsing fault
                              when some template values were chosen from pop-up
                              menu. This was caused by menu automatically adding
                              hotkeys to menu items.
                            + Removed reference to unused ResIds.inc include
                              file.
                            + Replaced usage of Help.inc include file with
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
 * The Original Code is FmRegTemplates.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmRegTemplates;


interface


uses
  // Delphi
  SysUtils, Windows, Menus, StdCtrls, ComCtrls, Controls, ExtCtrls, Classes,
  // Project specific component
  CmpHotText,
  // Project
  FmGenericOKDlg, URegTemplates;

type

  {
  TRegTemplatesDlg:
    Provides a dialog box that can be used for selecting and editing registry
    key templates.

    Inheritance: TRegTemplatesDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TRegTemplatesDlg = class(TGenericOKDlg)
    lblTemplateList: TLabel;
    cbTemplates: TComboBox;
    mnuMacros: TPopupMenu;
    btnEditMacros: TButton;
    lblEditMacros: TLabel;
    htEditorDesc: THotText;
    pnlEditor: TPanel;
    htEditor: THotText;
    procedure FormShow(Sender: TObject);
    procedure cbTemplatesChange(Sender: TObject);
    procedure btnEditMacrosClick(Sender: TObject);
    procedure htEditorDescLinkClick(Sender: TObject; const Cmd: String;
      const Rect: TRect);
    procedure htEditorLinkClick(Sender: TObject; const Cmd: String;
      const Rect: TRect);
  private
    fTemplates: TRegTemplateList;
      {List of available templates read from registry}
    fMacros: TRegMacroList;
      {List of available macros read from and written to registry}
    fCurrentMacro: TRegMacroItem;
      {Reference to macro currently being edited}
    procedure DisplayTemplate(const TemplateName: string);
      {Displays in template editor the registry key associated with template of
      given name. Any macros in template are replaced by current value and are
      made clickable to enable editing. A hot text control is used to achieve
      this}
    procedure ProcessTemplate(const TemplateStr: string;
      out HotTextCode: string);
      {Analyses template string and converts to hot text code. Any macros
      contained in string are replaced by their current value and are turned
      into hot links that can be clicked}
    procedure BuildMenu(const Cmd: string);
      {Creates the popup menu displayed when a macro is clicked in template
      editor. Menu displays known values for macro + an option to add a new value}
    procedure MacroSelect(Sender: TObject);
      {OnClick event handler for macro value popup menu items: displays template
      with new value for macro and remember the value}
    procedure NewValueSelect(Sender: TObject);
      {OnClick event handler for popup menu's "New Value" command: displays
      dialog box in which user enters a new value for a macro and updates
      template to display the new value if user OKs}
    procedure MacroNameSelect(Sender: TObject);
      {OnClick event handler for popup menu's "Macro Name" command: sets
      macro to display its own name in template}
    procedure UpdateCurrentMacro(NewValue: string);
      {Stores given value in current macro object and makes value current.
      Updates current template to reflect this new macro value}
  public
    class function KeyFromTemplate(Owner: TComponent;
      out KeyPath: string): Boolean;
      {Displays dialog box and gets a key path based on template from user.
      Passes key back to user via KeyPath parameter. Returns true if user OKs
      and false otherwise}
    constructor Create(Owner: TComponent); override;
      {Class contructor: create and initialises owned objects}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
  end;


  {
  ERegTemplatesDlg:
    Class of exception raised by TRegTemplateDlg.

    Inheritance: ERegTemplatesDlg -> [Exception]
  }
  ERegTemplatesDlg = class(Exception);


implementation


uses
  // Delphi
  Graphics,
  // Project
  UHelpContexts, UCmdDispatcher, FmSimpleEdit, FmRegMacroEdit;


{$R *.DFM}


const
  // Constants contain fixed parts of hit text used to display template
  cHotTextHeader = '<hottext version=1.0><body><p>';
  cHotTextFooter = '</p></body></hottext>';


resourcestring
  // Dialog box captions and prompts
  sNewValDlgCaption = 'New Registry Macro Value';
  sNewValDlgPrompt = 'Enter a value for %s macro';
  // Menu item captions
  sNewValueItem = 'New value...';
  sTpltNameItem = 'Macro name';
  // Error messages
  sBadMacroName = 'Macro "%s" is not recognised. This may indicate that the ' +
    'macro is not recorded in the registry.';
  sBadMacroInTplt = 'Invalid template. It contains the unrecognised macro ' +
    '"%s". This may indicate the macro is not recorded in registry.';


{ TRegTemplatesDlg }

procedure TRegTemplatesDlg.btnEditMacrosClick(Sender: TObject);
  {OnClick event handler for Edit Macros button. Displays the macro edit dialog.
  Ensures that the macros object is updated to reflect changes made}
begin
  inherited;
  // Free old macro object: this saves its changes to registry
  fMacros.Free;
  // Create and display editor dialog:
  // this reads macros from registry and saves any changes back to registry
  with TRegMacroEditDlg.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
  // Recreate macros object to reflect any changes
  fMacros := TRegMacroList.Create;
  // Redisplay the template (this updates with any changed values)
  if cbTemplates.ItemIndex > -1 then
    DisplayTemplate(cbTemplates.Items[cbTemplates.ItemIndex]);
end;

procedure TRegTemplatesDlg.BuildMenu(const Cmd: string);
  {Creates the popup menu displayed when a macro is clicked in template editor.
  Menu displays known values for macro + options to add a new value or revert to
  template name}

  // ---------------------------------------------------------------------------
  procedure AddItem(const Caption: string; const Enabled: Boolean;
    const ClickEvent: TNotifyEvent);
    {Adds new menu item with given caption, state and click handler to popup
    menu}
  begin
    mnuMacros.Items.Add(
      Menus.NewItem(Caption, 0, False, Enabled, ClickEvent, 0, '')
    );
  end;
  // ---------------------------------------------------------------------------

var
  Macro: TRegMacroItem;   // reference to macro object being edited
  Idx: Integer;           // loops thru known values for macro
  Value: string;          // value of a macro
begin
  // Clear the previous menu
  while mnuMacros.Items.Count > 0 do
    mnuMacros.Items[0].Free;
  // Get hold of macro we're editing (from macro list)
  Macro := fMacros.FindMacro(Cmd);
  Assert(Assigned(Macro));
  // If macro has known values, add them to menu
  if Macro.Values.Count > 0 then
  begin
    for Idx := 0 to Pred(Macro.Values.Count) do
    begin
      Value := Macro.Values[Idx];
      // add item for this value: disable if it's the current value
      AddItem(
        Value,
        AnsiCompareText(Value, Macro.CurrentValue) <> 0,
        MacroSelect
      );
    end;
    // Add spacer
    AddItem('-', True, nil);
  end;
  // Add "template name" menu option with its own event handler
  // this item is disabled if template already uses its own name
  AddItem(sTpltNameItem, Macro.CurrentValue <> Macro.Name, MacroNameSelect);
  // Add "new value" menu option with its own event handler
  AddItem(sNewValueItem, True, NewValueSelect);
end;

procedure TRegTemplatesDlg.cbTemplatesChange(Sender: TObject);
  {OnChange event handler for Templates combo box: displays in template editor
  the template associated with that selected in combo}
begin
  inherited;
  // Display selected template, with macros accessible by hot links
  DisplayTemplate(cbTemplates.Items[cbTemplates.ItemIndex]);
  // Enable OK button now we have something to see
  OKBtn.Enabled := True;
end;

constructor TRegTemplatesDlg.Create(Owner: TComponent);
  {Class contructor: create and initialises owned objects}
begin
  inherited;
  // Set help context
  HelpContext := IDH_DLG_REGTEMPLATE;
  // Create objects to read and write templates and associated macros
  fTemplates := TRegTemplateList.Create;
  fMacros := TRegMacroList.Create;
end;

destructor TRegTemplatesDlg.Destroy;
  {Class destructor: frees owned objects}
begin
  fMacros.Free;
  fTemplates.Free;
  inherited;
end;

procedure TRegTemplatesDlg.DisplayTemplate(const TemplateName: string);
  {Displays in template editor the registry key associated with template of
  given name. Any macros in template are replaced by current value and are made
  clickable to enable editing. A hot text control is used to achieve this}
var
  HotText: string;            // hot text code for template
  Template: TRegTemplateItem; // reference to template with given name
begin
  htEditor.Invalidate;
  // Get hold of template with given name
  Template := fTemplates.FindTemplate(TemplateName);
  Assert(Assigned(Template));
  // Process the template, converting to hot text
  ProcessTemplate(Template.Value, HotText);
  // Load the hot text into editor, setting hot links as required
  htEditor.Code := HotText;
end;

procedure TRegTemplatesDlg.FormShow(Sender: TObject);
  {OnShow event handler for form: stores names of available templates in combo
  box}
var
  Idx: Integer; // loops thru all templates
begin
  inherited;
  // Load available templates into combo
  cbTemplates.Clear;
  for Idx := 0 to Pred(fTemplates.Count) do
    cbTemplates.Items.Add(fTemplates[Idx].Name);
end;

procedure TRegTemplatesDlg.htEditorDescLinkClick(Sender: TObject;
  const Cmd: String; const Rect: TRect);
  {OnLinkClick event handler for hot link in text describing macro editing.
  Calls the dispatcher to deal with the command}
begin
  TCmdDispatcher.DispatchCmd(Cmd);
end;

procedure TRegTemplatesDlg.htEditorLinkClick(Sender: TObject;
  const Cmd: String; const Rect: TRect);
  {OnLinkClick event handler that responds to clicks on macro hot links in
  template editor's hot text control by displaying menu containing valid macro
  values and an option to add a new value}
var
  PopupPos: TPoint; // position where menu is to pop-up
begin
  // Create the menu to be displayed
  BuildMenu(Cmd);
  // Record the macro we're about to edit
  fCurrentMacro := fMacros.FindMacro(Cmd);
  if not Assigned(fCurrentMacro) then
    raise ERegTemplatesDlg.CreateFmt(sBadMacroName, [Cmd]);
  // Popup menu item under the item we clicked
  PopupPos := htEditor.ClientToScreen(Point(Rect.Left, Rect.Bottom));
  Self.cbTemplates.SetFocus;
  mnuMacros.Popup(PopupPos.X, PopupPos.Y);
end;

class function TRegTemplatesDlg.KeyFromTemplate(Owner: TComponent;
  out KeyPath: string): Boolean;
  {Displays dialog box and gets a key path based on template from user. Passes
  key back to user via KeyPath parameter. Returns true if user OKs and false
  otherwise}
begin
  // Create and display dialog box
  with TRegTemplatesDlg.Create(Owner) do
    try
      Result := ShowModal = mrOK;
      if Result then
        // User OK'd: pass back the entered template
        KeyPath := htEditor.PlainText;
    finally
      Free;
    end;
end;

procedure TRegTemplatesDlg.MacroNameSelect(Sender: TObject);
  {OnClick event handler for popup menu's "Macro Name" command: sets macro to
  display its own name in template}
begin
  UpdateCurrentMacro(fCurrentMacro.Name);
end;

procedure TRegTemplatesDlg.MacroSelect(Sender: TObject);
  {OnClick event handler for macro value popup menu items: displays template
  with new value for macro and remember the value}
begin
  Self.UpdateCurrentMacro(
    (Sender as TMenuItem).Caption   // value is in caption of menu item
  );
end;

procedure TRegTemplatesDlg.NewValueSelect(Sender: TObject);
  {OnClick event handler for popup menu's "New Value" command: displays dialog
  box in which user enters a new value for a macro and updates template to
  display the new value if user OKs}
var
  Value: string;        // the value entered by user
begin
  // There is no default value
  Value := '';
  // Get new value from user and update macro and template if OKs
  if TSimpleEditDlg.Edit(
    Self,                                           // aligns to our own form
    sNewValDlgCaption,                              // dlg caption
    Format(sNewValDlgPrompt, [fCurrentMacro.Name]), // dlg prompt
    IDH_DLG_REGMACROVALUE,                          // help context
    Value                                           // value entered
  ) then
    Self.UpdateCurrentMacro(Value);
end;

procedure TRegTemplatesDlg.ProcessTemplate(const TemplateStr: string;
  out HotTextCode: string);
  {Analyses template string and converts to hot text code. Any macros contained
  in string are replaced by their current value and are turned into hot links
  that can be clicked}
var
  StartMacro,           // index of start of macro in string
  EndMacro: Integer;    // index of end of macro in string
  MacroName: string;    // name of a macro
  Macro: TRegMacroItem; // reference to a macro object
  RemainingStr: string; // part of template string still to be processed
begin
  // Intialise hot text code to required header
  HotTextCode := cHotTextHeader;
  // Translate any '<', '>' and '&' into entities
  RemainingStr := StringReplace(TemplateStr, '<', '&lt;', [rfReplaceAll]);
  RemainingStr := StringReplace(RemainingStr, '>', '&gt;', [rfReplaceAll]);
  RemainingStr := StringReplace(RemainingStr, '&', '&amp;', [rfReplaceAll]);
  // Loop thru template string, processing all embedded macros
  repeat
    // Find leading and trailing macro delimiters, checking if we found one
    StartMacro := AnsiPos('[', RemainingStr);
    EndMacro := AnsiPos(']', RemainingStr);
    if (StartMacro > 0) and (EndMacro > StartMacro) then
    begin
      // We have a macro: process it
      // strip out macro name
      MacroName := Copy(RemainingStr, StartMacro, EndMacro - StartMacro + 1);
      // get reference to macro from list: error if not present
      Macro := fMacros.FindMacro(MacroName);
      if not Assigned(Macro) then
        raise ERegTemplatesDlg.CreateFmt(sBadMacroInTplt, [MacroName]);
      // add hot text code to output...
      HotTextCode := HotTextCode
        + Copy(RemainingStr, 1, StartMacro - 1) // ... text before macro
        + Format(                               // ... macro as hot link
            // ... this is format of link, with text in link colour
            '<link cmd="%0:s" hint="%1:s">'
            + '<font color=clBlue style=underline>%2:s</font>'
            + '</link>',
            // ... cmd=MacroName, hint=Macro.Desc and text=Macro.CurrentValue
            [MacroName, Macro.Desc, Macro.CurrentValue]
          );
      // Skip past macro just processed and go round again
      RemainingStr := Copy(RemainingStr, EndMacro + 1, MaxInt);
    end;
  until StartMacro = 0;
  // Finalise hot text code by adding remaining text (has no macros) and closing
  // hot text code
  HotTextCode := HotTextCode + RemainingStr + cHotTextFooter;
end;

procedure TRegTemplatesDlg.UpdateCurrentMacro(NewValue: string);
  {Stores given value in current macro object and makes value current. Updates
  current template to reflect this new macro value}
begin
  // Record new value, trimmed of spaces and check not ''
  NewValue := Trim(NewValue);
  if NewValue <> '' then
  begin
    // Make the new value current (this adds value if not present)
    fCurrentMacro.CurrentValue := NewValue;
    // Redisplay current template to update with changes
    DisplayTemplate(cbTemplates.Items[cbTemplates.ItemIndex]);
  end;
end;

end.

