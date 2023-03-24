{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UHotTextParaStyle.pas
  @COMMENTS                 Implements a class that stores information about a
                            the format applied to a paragraph in a hot text
                            document. A subsidiary class that encapsulates the
                            tab stops that are part of the paragraph style is
                            also provided.
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
 * The Original Code is UHotTextParaStyle.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UHotTextParaStyle;


interface


uses
  // Delphi
  Classes;


type

  {
  THotTextTabStops:
    Class that stores information about the tab stops that are part of a hot
    text document's paragraph style. Permits tabs stops to be specified or to
    use a default equally spaced setting. This class supports assignment from
    another object of the same type, can test tab stops for equality and can
    clone tabs stop settings. Tabs are maintained in ascending order.
  }
  THotTextTabStops = class(TPersistent)
  private // properties
    fTabs: TList;
    fDefaultTabs: Boolean;
    function GetTabCount: Integer;
    function GetTab(Idx: Integer): Integer;
    procedure SetDefaultTabs(const Value: Boolean);
  public
    constructor Create;
      {Basic class constructor: creates an instance with a default set of tab
      stops that use the default, equally spaced tab settings}
    constructor CreateClone(Src: THotTextTabStops);
      {Cloning class constructor: creates an instance that is a copy of the
      Src object}
    destructor Destroy; override;
      {Class destructor: frees owned tab stop list}
    procedure Assign(Src: TPersistent); override;
      {If Src is a THotTextTabStops instance this method sets all properties of
      this object to be same as the source object. If Src object is nil then no
      action is taken, but if Src is any other type an exception is raised}
    procedure AddTab(Tab: Integer);
      {Inserts the given tab to the list of tab stops, maintaining sort order of
      list. If the tab stop is already in the list, no action is taken. Setting
      a tab also causes the DefaultTabs property to become False}
    function IsEqual(TS: THotTextTabStops): Boolean;
      {Returns true if this object's properties are equal to those of TS and
      False otherwise. If TS is nil then False is returned}
    procedure Clear;
      {Clears tab stop list. Does not affect DefaultTabs property}
    function GetTabAfter(Pos: Integer): Integer;
      {Returns the next tab stop after the given position. If Pos falls on an
      existing tab stop, the position of the following tab is returned. If Pos
      is negative then zero is returned}
    property Tabs[Idx: Integer]: Integer read GetTab; default;
      {List of tab stops. If DefaultTabs is true a calculated value is returned
      that gives a sequence of equally spaced tabs. If DefaultTabs is false then
      Tabs[] returns the value set by the user when Idx < TabCount. When Idx >
      TabCount MaxInt is returned. Regardless of DefaultTabs a negative Idx
      always returns zero}
    property TabCount: Integer read GetTabCount;
      {The number of recorded tabstops. If DefaultTabs is true zero is always
      returned}
    property DefaultTabs: Boolean read fDefaultTabs write SetDefaultTabs;
      {When true the tab stops are equally spaced values. When false tab stops
      are as set usng the AddTab method}
  end;


  {
  THotTextParaStyle:
    Class that stores information about a the format applied to a paragraph in a
    hot text document. This class supports assignment from another object of the
    same type, can test paragraph style objects for equality and can clone
    paragraph style settings.
  }
  THotTextParaStyle = class(TPersistent)
  private // properties
    fIndentLeft: Integer;
    fIndentFirst: Integer;
    fIndentRight: Integer;
    fAlignment: TAlignment;
    fTabs: THotTextTabStops;
  public
    constructor Create;
      {Basic class constructor: creates an instance default settings: i.e. zero
      indents, left justified and default tab stops}
    constructor CreateClone(const Src: THotTextParaStyle);
      {Cloning class constructor: creates an instance that is a copy of the
      Src object}
    destructor Destroy; override;
      {Class destructor: frees owned tab stops object}
    procedure Assign(Src: TPersistent); override;
      {If Src is a THotTextParaStyle instance this method sets all properties of
      this object to be same as the source object. If Src object is nil then no
      action is taken, but if Src is any other type an exception is raised}
    function IsEqual(const PS: THotTextParaStyle): Boolean;
      {Returns true if this object's properties are equal to those of PS and
      False otherwise. If PS is nil then False is returned}
    property IndentFirst: Integer read fIndentFirst write fIndentFirst
      default 0;
      {The amount of left indentation used for the first line of a paragraph,
      relative to IndentLeft}
    property IndentLeft: Integer read fIndentLeft write fIndentLeft
      default 0;
      {The amount of indentation of a paragraph from the left margin. The first
      line of the paragraph may have different identation per IndentFirst}
    property IndentRight: Integer read fIndentRight write fIndentRight
      default 0;
      {The amount of indentation of a paragraph from the right margin}
    property Alignment: TAlignment read fAlignment write fAlignment
      default taLeftJustify;
      {The way paragraph text is aligned: left justified, centred or right
      justified}
    property Tabs: THotTextTabStops read fTabs;
      {The tab stops for the paragraph: this is a reference to an ownded tab
      stops object}
  end;


implementation


{ THotTextTabStops }

const
  // Tab spacing when DefaultTabs is True
  cDefTabStop = 20;

procedure THotTextTabStops.AddTab(Tab: Integer);
  {Inserts the given tab to the list of tab stops, maintaining sort order of
  list. If the tab stop is already in the list, no action is taken. Setting a
  tab also causes the DefaultTabs property to become False}
var
  Idx: Integer; // loops thru tab list
begin
  // Ensure DefaultTabs property is False
  if DefaultTabs then
    DefaultTabs := False;
  // Scan tab list looking for correct insertion position to maintain order
  Idx := 0;
  while (Idx < TabCount) and (GetTab(Idx) < Tab) do
    Inc(Idx);
  // Only insert tab if no alredy in list
  if (Idx = TabCount) or (Tab < GetTab(Idx)) then
    fTabs.Insert(Idx, Pointer(Tab));
end;

procedure THotTextTabStops.Assign(Src: TPersistent);
  {If Src is a THotTextTabStops instance this method sets all properties of this
  object to be same as the source object. If Src object is nil then no action is
  taken, but if Src is any other type an exception is raised}
var
  SrcTS: THotTextTabStops;  // the source object as a tab stop
  Idx: Integer;             // loops thru tab stops
begin
  if (Src is THotTextTabStops) then
  begin
    if not Assigned(Src) then
      // Src is nil: do nothing
      Exit;
    // Copy properties of Src
    SrcTS := Src as THotTextTabStops;
    Self.DefaultTabs := SrcTS.DefaultTabs;
    if not SrcTS.DefaultTabs then
    begin
      // Src has tab stops: replace this object's list with that from Src
      Self.Clear;
      for Idx := 0 to Pred(SrcTS.TabCount) do
        Self.AddTab(SrcTS[Idx]);
    end;
  end
  else
    // Src is not a supported type: call inherited method (raises exception)
    inherited;
end;

procedure THotTextTabStops.Clear;
  {Clears tab stop list. Does not affect DefaultTabs property}
begin
  fTabs.Clear;
end;

constructor THotTextTabStops.Create;
  {Basic class constructor: creates an instance with a default set of tab stops
  that use the default, equally spaced tab settings}
begin
  inherited;
  // Create tab stop list
  fTabs := TList.Create;
  // We use default, equally spaced tabs by default
  fDefaultTabs := True;
end;

constructor THotTextTabStops.CreateClone(Src: THotTextTabStops);
  {Cloning class constructor: creates an instance that is a copy of the Src
  object}
begin
  // Create default object
  Create;
  // Now copy Src object to it
  Self.Assign(Src);
end;

destructor THotTextTabStops.Destroy;
  {Class destructor: frees owned tab stop list}
begin
  fTabs.Free;
  inherited;
end;

function THotTextTabStops.GetTab(Idx: Integer): Integer;
  {Getter method for Tabs property: value depends on DefaultTabs property - when
  DefaultTabs is true Tabs[Idx] is a calculated value for equally spaced tab
  stops, but when DefaultTabs is false, return is value of tab at given index or
  MaxInt if no tab specified at given index. If Idx is negative 0 is always
  returned, regardless of DefaultTabs}
begin
  if Idx >= 0 then
  begin
    if fDefaultTabs then
      Result := cDefTabStop * (1 + Idx)
    else if Idx < TabCount then
      Result := Integer(fTabs[Idx])
    else
      Result := MaxInt;
  end
  else
    Result := 0;
end;

function THotTextTabStops.GetTabAfter(Pos: Integer): Integer;
  {Returns the next tab stop after the given position. If Pos falls on an
  existing tab stop, the position of the following tab is returned. If Pos is
  negative then zero is returned}
var
  Idx: Integer; // loops thru tab stops
begin
  if Pos < 0 then
    // Negative Pos => 0 return
    Result := 0
  else
  begin
    if DefaultTabs then
      // Using default tabs: calculate next one
      Result := cDefTabStop * (1 + Pos div cDefTabStop)
    else
    begin
      // Using custom tab stops: find next tab
      // (note: this method always works since unset tabs are always MaxInt)
      Idx := 0;
      while GetTab(Idx) <= Pos do
        Inc(Idx);
      Result := GetTab(Idx);
    end
  end;
end;

function THotTextTabStops.GetTabCount: Integer;
  {Getter method for Count property: always returns 0 if DefaultTabs is true and
  returns number of tab stops set otherwise}
begin
  if fDefaultTabs then
    Result := 0
  else
    Result := fTabs.Count;
end;

function THotTextTabStops.IsEqual(TS: THotTextTabStops): Boolean;
  {Returns true if this object's properties are equal to those of TS and False
  otherwise. If TS is nil then False is returned}
var
  Idx: Integer; // loops thru tab stops in this object
begin
  if Assigned(TS) then
  begin
    if Self.DefaultTabs <> TS.DefaultTabs then
      // DefaultTabs properties not the same
      Result := False
    else if not Self.DefaultTabs then
    begin
      // DefaultTabs for both objects is False: need to check tab stops
      if Self.TabCount = TS.TabCount then
      begin
        // We have same number of tab stops: need each one to be same
        Result := True;
        for Idx := 0 to Pred(Self.TabCount) do
        begin
          if Self[Idx] <> TS[Idx] then
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      else
        // Number of tab stops differs
        Result := False;
    end
    else
      // DefaulTabs property for both objects is True: no more to do
      Result := True;
  end
  else
    // Given object is nil
    Result := False;
end;

procedure THotTextTabStops.SetDefaultTabs(const Value: Boolean);
  {Setter method for DefaultTabs property: records value and clears tab stops if
  DefaultTabs is being set true}
begin
  fDefaultTabs := Value;
  if fDefaultTabs then
    Clear;
end;


{ THotTextParaStyle }

procedure THotTextParaStyle.Assign(Src: TPersistent);
  {If Src is a THotTextParaStyle instance this method sets all properties of
  this object to be same as the source object. If Src object is nil then no
  action is taken, but if Src is any other type an exception is raised}
var
  PS: THotTextParaStyle;  // the source object as a paragraph style
begin
  if (Src is THotTextParaStyle) then
  begin
    // We have a THotTextParaStyle object
    PS := Src as THotTextParaStyle;
    // copy simple properties
    Self.fIndentFirst := PS.fIndentFirst;
    Self.fIndentLeft := PS.fIndentLeft;
    Self.fIndentRight := PS.fIndentRight;
    Self.fAlignment := PS.fAlignment;
    // get Tabs property to perform its own assignment with tabs from Src
    Self.Tabs.Assign(PS.Tabs);
  end
  else
    // Unsupported type: call inherited method that raises exception
    inherited;
end;

constructor THotTextParaStyle.Create;
  {Basic class constructor: creates an instance default settings: i.e. zero
  indents, left justified and default tab stops}
begin
  inherited Create;
  // Set simple property defaults (unspecified properties are zero)
  fAlignment := taLeftJustify;
  // Create tab stop object with default properties
  fTabs := THotTextTabStops.Create;
end;

constructor THotTextParaStyle.CreateClone(const Src: THotTextParaStyle);
  {Cloning class constructor: creates an instance that is a copy of the Src
  object}
begin
  // Use basic constructor to set up object
  Create;
  // Assign Src to new object
  Self.Assign(Src);
end;

destructor THotTextParaStyle.Destroy;
  {Class destructor: frees owned tab stops object}
begin
  fTabs.Free;
  inherited;
end;

function THotTextParaStyle.IsEqual(const PS: THotTextParaStyle): Boolean;
  {Returns true if this object's properties are equal to those of PS and False
  otherwise. If PS is nil then False is returned}
begin
  if Assigned(PS) then
    // Object being checked assigned: compare properties
    Result := (Self.fIndentLeft = PS.fIndentLeft)
      and (Self.fIndentFirst = PS.fIndentFirst)
      and (Self.fIndentRight = PS.fIndentRight)
      and (Self.fAlignment = PS.fAlignment)
      and (Self.Tabs.IsEqual(PS.Tabs))  // tabs stops compare themselves
  else
    // Object being checked is nil: not equal
    Result := False;
end;

end.
