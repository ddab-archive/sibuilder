{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UFontEx.pas
  @COMMENTS                 Implements a class that extends TFont by adding new
                            constructor to create a clone of an existing font
                            and to test fonts for equality. Also provided is an
                            object that maintains a stack of fonts.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/11/2003
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
 * The Original Code is UFontEx.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UFontEx;


interface


uses
  // Delphi
  Graphics,
  // Project
  UStacks;


type

  {
  TFontEx:
    Class that extends TFont by adding new constructor to create a clone of an
    existing font and to test fonts for equality.
  }
  TFontEx = class(TFont)
  public
    constructor CreateClone(const Font: TFont);
      {Creates a new extended font instance that is a copy of the given font}
    function IsEqual(const Font: TFont): Boolean;
      {Returns true if the given font is equal to this font (i.e. has same
      properties. If the given font is nil then False is returned}
  end;

  {
  TFontExStack:
    Implements a stack of TFontEx classes. Note that the stack doesn't allow
    ordinary TFont entries.
  }
  TFontExStack = class(TObjectStack)
  public
    procedure Push(const Obj: TFontEx);
      {Pushes given extended font object onto the stack}
    function Pop: TFontEx;
      {Pops extended font object from top of stack and returns reference to it.
      Raises exception if stack is empty}
    function Top: TFontEx;
      {Returns reference to extended font object at top of stack. Raises
      exception if stack is empty}
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TFontEx }

constructor TFontEx.CreateClone(const Font: TFont);
  {Creates a new extended font instance that is a copy of the given font}
begin
  inherited Create;
  Self.Assign(Font);
end;

function TFontEx.IsEqual(const Font: TFont): Boolean;
  {Returns true if the given font is equal to this font (i.e. has same
  properties. If the given font is nil then False is returned}
begin
  if Assigned(Font) then
    Result := (Self.Color = Font.Color)
      and (Self.Size = Font.Size)
      and (Self.Style = Font.Style)
      and (AnsiCompareText(Self.Name, Font.Name) = 0)
      and (Self.Pitch = Font.Pitch)
      and (Self.Charset = Font.Charset)
      and (Self.PixelsPerInch = Font.PixelsPerInch)
  else
    Result := False;
end;


{ TFontExStack }

function TFontExStack.Pop: TFontEx;
  {Pops extended font object from top of stack and returns reference to it.
  Raises exception if stack is empty}
begin
  Result := inherited Pop as TFontEx;
end;

procedure TFontExStack.Push(const Obj: TFontEx);
  {Pushes given extended font object onto the stack}
begin
  inherited Push(Obj);
end;

function TFontExStack.Top: TFontEx;
  {Returns reference to extended font object at top of stack. Raises exception
  if stack is empty}
begin
  Result := inherited Top as TFontEx;
end;


end.
