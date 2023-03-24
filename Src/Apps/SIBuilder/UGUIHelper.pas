{ ##              
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UGUIHelper.pas
  @COMMENTS                 Routine that allows groups of controls to be enabled
                            / disabled.
  @OTHER_NAMES              + Original unit name was UCtrlState.pas
                            + Changed to UGUIHelper.pas at v2.0
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 03/09/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 20/12/2000
      @COMMENTS             + Changed EnableCtrls procedure to so it can
                              optionally be applied recursively to all child
                              controls of a TWinControl.
                            + Added procedure to enable/disable and given main
                              menu.
                            + Added procedure to enable/disable all controls on
                              a given form.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             + Added code to create hints from file paths
                              containing path macros: the hints show the
                              expanded paths.
                            + Added code to create two colour bitmaps from
                              greyscale bitmaps.
                            + Changed name of unit to UGUIHelper now that it has
                              various different GUI related helper routines
                              rather than just routines that manage control
                              states.
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
 * The Original Code is UGUIHelper.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UGUIHelper;


interface


uses
  // Delphi
  Windows, Graphics, Classes, Controls, Menus, Forms;

procedure EnableMainMenu(const Menu: TMainMenu; const Flag: Boolean);
  {Sets enabled state for all menu items on given main menu}

procedure EnableForm(const Form: TForm; const Flag: Boolean);
  {Sets enabled property for all controls on form to given state}

procedure EnableCtrls(const Ctrls: array of TControl;
  const Flag: Boolean; const Recurse: Boolean = False);
  {Sets the enable property of all controls in the given array to the state
  given by flag. If Recurse is True then the procedure is applied recursively
  to all the child controls of the given controls}

function PathMacroCtrlHint(const MacroFileSpec: string): string;
  {This function returns a string that is suitable for displaying in hints to
  be displayed for file paths that may contain path macros. If the string
  contains a valid path macro then the expanded path is returned for display in
  the hint. If the string contains invalid path macros then an error message
  is returned for display. If the string contains no path macros then the
  empty string is returned indicating that no hint is to be displayed}

procedure ColouriseGreyScale(const GreyBmp, ColourBmp: TBitmap;
  const LightColour, DarkColour: TColor);
  {Colourises the given greyscale bitmap and returns in in ColourBmp. The
  resulting bitmap is graded between LightColour and DarkColour, with the tones
  in the resulting bitmap varying between the two: LightColour maps onto white
  in the grey bitmap while DarkColour maps onto black}


implementation


uses
  // Project
  UPathMacros;


procedure EnableMainMenu(const Menu: TMainMenu; const Flag: Boolean);
  {Sets enabled state for all menu items on given main menu}
var
  Index: Integer;
begin
  for Index := 0 to Pred(Menu.Items.Count) do
    Menu.Items[Index].Enabled := Flag;
end;

procedure EnableForm(const Form: TForm; const Flag: Boolean);
  {Sets enabled property for all controls on form to given state}
begin
  EnableCtrls([Form], Flag, True);
end;

procedure EnableCtrls(const Ctrls: array of TControl;
  const Flag: Boolean; const Recurse: Boolean = False);
  {Sets the enable property of all controls in the given array to the state
  given by flag. If Recurse is True then the procedure is applied recursively
  to all the child controls of the given controls}
var
  I: Integer;   // scans through the Ctrls array
  J: Integer;   // scans through control's Controls array
begin
  // Scan thru
  for I := Low(Ctrls) to High(Ctrls) do
  begin
    Ctrls[I].Enabled := Flag;
    if Recurse and (Ctrls[I] is TWinControl) then
        for J := 0 to Pred((Ctrls[I] as TWinControl).ControlCount) do
          EnableCtrls([(Ctrls[I] as TWinControl).Controls[J]], Flag, True);
  end;
end;

function PathMacroCtrlHint(const MacroFileSpec: string): string;
  {This function returns a string that is suitable for displaying in hints to
  be displayed for file paths that may contain path macros. If the string
  contains a valid path macro then the expanded path is returned for display in
  the hint. If the string contains invalid path macros then an error message
  is returned for display. If the string contains no path macros then the
  empty string is returned indicating that no hint is to be displayed}
resourcestring
  sBadMacroHint = 'Unrecognised macro'; // hint error text
var
  ExpandedName: string; // expanded version of macro string
begin
  // Check if the macro string is valid
  if PathMacros.ValidateMacroPath(MacroFileSpec) then
  begin
    // Valid string: check to see if expanded name is different to given string
    ExpandedName := PathMacros.ExpandMacroPath(MacroFileSpec);
      // string is not changed when expanded => no path macros => '' result
    if ExpandedName = MacroFileSpec then
      Result := ''
    else
      // string was changed on expansion => path macros were present => return
      // expanded string
      Result := ExpandedName;
  end
  else
    // Invalis string => return error message
    Result := sBadMacroHint;
end;

procedure ColouriseGreyScale(const GreyBmp, ColourBmp: TBitmap;
  const LightColour, DarkColour: TColor);
  {Colourises the given greyscale bitmap and returns in in ColourBmp. The
  resulting bitmap is graded between LightColour and DarkColour, with the tones
  in the resulting bitmap varying between the two: LightColour maps onto white
  in the grey bitmap while DarkColour maps onto black}

  // ---------------------------------------------------------------------------
  function MapColor(const Lo, Hi, Color: TColor): TColor;
    {Scales each RGB element of Color into the range defined by the related RGB
    values of Lo and Hi and returns the resulting colour}
  var
    R, G, B: Integer;     // RGB elements of Color
    RHi, GHi, BHi: Byte;  // RGB elements of Lo
    RLo, GLo, BLo: Byte;  // RGB elements of Hi
  begin
    // Get RGB elements of Lo and Hi
    RLo := GetRValue(Lo);
    GLo := GetGValue(Lo);
    BLo := GetBValue(Lo);
    RHi := GetRValue(Hi);
    GHi := GetGValue(Hi);
    BHi := GetBValue(Hi);
    // Get RGB elements of Color
    R := GetRValue(Color);
    G := GetGValue(Color);
    B := GetBValue(Color);
    // Scale Color's RGB elements into range defined by equivalent
    // Lo and Hi RGB elements
    R := RLo + Round(R * (RHi - RLo) / 255);
    G := GLo + Round(G * (GHi - GLo) / 255);
    B := BLo + Round(B * (BHi - BLo) / 255);
    // Combine resulting RGB elements into a colour and return it
    Result := RGB(R, G, B);
  end;
  // ---------------------------------------------------------------------------

var
  G,                            // loops thru all shades of grey
  I,                            // loops thru horizontal pixels in bitmap
  J: Integer;                   // loops thru vertical pixels in bitmap
  Map: array[0..255] of TColor; // maps between grey shades and colour grade
  LoColour, HiColour: TColor;   // the colours that map to black and white
begin
  // Record given light and dark colour's RGB values
  LoColour := ColorToRGB(DarkColour);
  HiColour := ColorToRGB(LightColour);
  // Record intermediate colour that relates to each of the 256 shades of grey
  // .. this acts as lookup table for colour equivalents of greyscale pixels
  for G := 0 to 255 do
    Map[G] := MapColor(LoColour, HiColour, RGB(G, G, G));
  // Set grey bitmap and colour bitmap to same size and 24 bit colour
  GreyBmp.PixelFormat := pf24bit;
  ColourBmp.PixelFormat := pf24bit;
  ColourBmp.Width := GreyBmp.Width;
  ColourBmp.Height := GreyBmp.Height;
  // Scan thru grey bitmap, writing the related colour pixel in colour bitmap
  for I := 0 to Pred(GreyBmp.Width) do
    for J := 0 to Pred(GreyBmp.Height) do
      ColourBmp.Canvas.Pixels[I, J] :=
        Map[GetRValue(GreyBmp.Canvas.Pixels[I, J])];
end;

end.
