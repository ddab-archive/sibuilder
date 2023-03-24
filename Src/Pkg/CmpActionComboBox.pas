{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpActionComboBox.pas
  @COMMENTS                 Defines a base class for Combo boxes that specifies
                            a special "action text" item which will trigger a
                            special event when selected. This event enables user
                            to take specific action when this item is selected.
                            The base class is designed to be sub-classed and is
                            not registered with Delphi. Components derived from
                            this base class are used by SIBuilder.
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
 * The Original Code is CmpActionComboBox.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpActionComboBox;


interface


uses
  // Delphi
  Messages, StdCtrls, Classes;

const
  // Custom message posted to self when action text is selected
  ACB_ACTION = WM_USER + 1;

type


  {
  TCustomActionComboBox:
    Combo box derivative class that specifies a special "action text" item in
    the combo box which will trigger a OnSelectAction event when selected. This
    event enables user to take specific action when this item is selected and to
    determine what text is displayed in combo box. All properties are protected
    and need to be published by descendant classes.
  }
  TCustomActionComboBox = class(TComboBox)
  private // properties
    fActionText: string;
    fOnSelectAction: TNotifyEvent;
    function GetText: string;
    procedure SetText(const Value: string);
  private
    fPrevText: string;
      {Previous text displayed in combo box (other than action text item)}
  protected
    procedure Click; override;
      {Override of Click method: posts the ACB_ACTION message if the selected
      item is the action text: message is posted since otherwise any changes
      made by user to text to be displayed is overwritten by control when it
      updates to reflect the text of the selected item}
    procedure Change; override;
      {Override of Change method: records current Text property unless the
      selected item is the action text}
    procedure DoAction; dynamic;
      {Triggers the OnSelectAction event. This is called when the action text is
      selected by user. Before event is triggered the previously stored text
      (i.e. text that was displayed before the action text was selected) is
      written back to combo box text}
    function IndexOfActionText: Integer;
      {Returns index of action text in combo box list or -1 if not present}
    procedure ACBAction(var Msg: TMessage); message ACB_ACTION;
      {Custom ACB_ACTION message handler: calls the DoAction method}
    procedure Loaded; override;
      {Override of Loaded method: records initial text displayed in combo box}
    function GetPrevText: string;
      {Returns the previous text from control}
  protected // properties for later publishing
    property ActionText: string
      read fActionText write fActionText;
      {The text in the combo box's Items property that causes OnSelectAction
      event to be triggered when selected. This text is only ever displayed
      momentarily in combo box before any OnSelectAction event executes (it is
      replaced by text that was in combo box just before action text was
      selected and user may store an new value in text in OnSelectAction event
      handler}
    property OnSelectAction: TNotifyEvent
      read fOnSelectAction write fOnSelectAction;
      {Event triggered when ActionText is selected in combo box. Users should
      do whatever action is required in this handler. If the handler needs to
      change the text in the combo box it should set the text property. If the
      text property is not changed the text that was displayed before the action
      text was selected remains dispayed}
  published
    property Text: string read GetText write SetText;
      {"Overridden" Text property: records the new text unless it is the action
      text for future use}
  end;


implementation


uses
  // Delphi
  Windows;


{ TCustomActionComboBox }

procedure TCustomActionComboBox.ACBAction(var Msg: TMessage);
  {Custom ACB_ACTION message handler: calls the DoAction method}
begin
  DoAction;
end;

procedure TCustomActionComboBox.Change;
  {Override of Change method: records current Text property unless the selected
  item is the action text}
begin
  if ItemIndex <> IndexOfActionText then
    fPrevText := Text;
  inherited;
end;

procedure TCustomActionComboBox.Click;
  {Override of Click method: posts the ACB_ACTION message if the selected item
  is the action text: message is posted since otherwise any changes made by user
  to text to be displayed is overwritten by control when it updates to reflect
  the text of the selected item}
begin
  if ItemIndex = IndexOfActionText then
    PostMessage(Self.Handle, ACB_ACTION, 0, 0);
  inherited;
end;

procedure TCustomActionComboBox.DoAction;
  {Triggers the OnSelectAction event. This is called when the action text is
  selected by user. Before event is triggered the previously stored text (i.e.
  text that was displayed before the action text was selected) is written back
  to combo box text}
begin
  Text := fPrevText;
  if Assigned(fOnSelectAction) then
    fOnSelectAction(Self);
end;

function TCustomActionComboBox.GetPrevText: string;
  {Returns the previous text from control}
begin
  Result := fPrevText;
end;

function TCustomActionComboBox.GetText: string;
  {Read access method for Text property: gets value from inherited property}
begin
  Result := inherited Text;
end;

function TCustomActionComboBox.IndexOfActionText: Integer;
  {Returns index of action text in combo box list or -1 if not present}
begin
  Result := Items.IndexOf(fActionText);
end;

procedure TCustomActionComboBox.Loaded;
  {Override of Loaded method: records initial text displayed in combo box}
begin
  inherited;
  fPrevText := Text;
end;

procedure TCustomActionComboBox.SetText(const Value: string);
  {Write access method for Text property: makes a copy of new value if this is
  not the action text then stores new value in inherited property}
begin
  if Value <> fPrevText then
    // store new value: used as text in OnSelectAction event handler
    fPrevText := Value;
  inherited Text := Value;
end;

end.
