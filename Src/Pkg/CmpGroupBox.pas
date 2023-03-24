{ ##               
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     CmpGroupBox.pas
  @COMMENTS                 Implements TGroupBox descendant that improves
                            appearance of group box by separating caption from
                            group box lines and giving a proper disabled
                            appearance when control is not enabled. This
                            component is used by SIBuilder.
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
 * The Original Code is CmpGroupBox.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit CmpGroupBox;

interface

uses
  // Delphi
  Messages, Controls, StdCtrls;

type
  {
  TNewGroupBox:
    Sub class of TGroupBox that (a) displays group box text in "etched" style
    when group box is disabled and (b) surrounds group box caption with space
    for improved appearance.
  }
  TNewGroupBox = class(TGroupBox)
  private
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {Message handler called when enabled state of control changes: simply
      causes control to be repainted}
  protected
    procedure Paint; override;
      {Overridden Paint handler: uses code copied from TGroupBox.Paint to paint
      box outline. Text drawing code has been changed to (1) display disabled
      group boxes in etched text (like disabled labels) and (2) to ensure space
      between text and box lines}
  end;


procedure Register;
  {Registers component with Delphi}


implementation


uses
  // Delphi
  Windows, Classes, Graphics;


procedure Register;
  {Registers component with Delphi}
begin
  RegisterComponents('SIBuilder', [TNewGroupBox]);
end;


{ TNewGroupBox }

procedure TNewGroupBox.CMEnabledChanged(var Msg: TMessage);
  {Message handler called when enabled state of control changes: simply causes
  control to be repainted}
begin
  inherited;
  Invalidate;
end;

procedure TNewGroupBox.Paint;
  {Overridden Paint handler: uses code copied from TGroupBox.Paint to paint
  box outline. Text drawing code has been changed to (1) display disabled group
  boxes in etched text (like disabled labels) and (2) to ensure space between
  text and box lines}
var
  H: Integer;       // height of caption text
  R: TRect;         // rectangle in which displayed
  Flags: Longint;   // text display flags
  OutText: string;  // output text: caption bounded by spaces
begin
  // We output caption text separated from lines by spaces
  OutText := ' ' + Text + ' ';
  with Canvas do
  begin
    // Set required font
    Font := Self.Font;
    // Display the box's bounding rectangle
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height); // top line is halfway down text
    // draw frame 3D or plain
    if Ctl3D then
    begin
      // draw 3D frame
      Inc(R.Left);
      Inc(R.Top);
      Brush.Color := clBtnHighlight;
      FrameRect(R);
      OffsetRect(R, -1, -1);
      Brush.Color := clBtnShadow;
    end else
      // plain frame
      Brush.Color := clWindowFrame;
    FrameRect(R);
    // Display title if any
    if Text <> '' then
    begin
      // set up display frame
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(OutText) - 8, 0, 0, H);
      // set up flags
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      // calculate size of text rectangle
      DrawText(
        Handle, PChar(OutText), Length(OutText), R, Flags or DT_CALCRECT
      );
      // display text differently according to if enabled
      if Enabled then
      begin
        // control is enabled: use plain text
        Brush.Color := Color;
        DrawText(Handle, PChar(OutText), Length(OutText), R, Flags);
      end
      else
      begin
        // control is disabled: display etched text
        Brush.Color := Color;
        OffsetRect(R, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(OutText), Length(OutText), R, Flags);
        OffsetRect(R, -1, -1);
        Brush.Style := bsClear;
        Font.Color := clBtnShadow;
        DrawText(Handle, PChar(OutText), Length(OutText), R, Flags);
      end;
    end;
  end;
end;

end.
