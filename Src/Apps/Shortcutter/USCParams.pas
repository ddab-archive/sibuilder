{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     USCParams.pas
  @COMMENTS                 Implements a static class that interprets parameters
                            passed to Shortcutter applet.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 24/11/2005
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
 * The Original Code is USCParams.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit USCParams;


interface


type

  {
  TSCParams:
    Static class that parses parameters from command line and returns logical
    values from it.
  }
  TSCParams = class(TObject)
    class procedure SetValFromParam(const ParamIdx: Integer; out Value: string);
      {Sets Value to value of parameter at given index. If required parameter is
      '*' Value is set to ''}
  public
    class function ShortcutFile: string;
      {Name of shortcut file to be created or deleted}
    class function AssocFile: string;
      {Name of file to be associated with shortcut}
    class function IconFile: string;
      {Name of file containing shortcut's icon}
    class function IconIndex: Integer;
      {Index of shorcut icon within file. If no index provided 0 is used}
    class function WorkDir: string;
      {Working directory for any program run from shortcut}
    class function Desc: string;
      {Shortcut description}
    class function Args: string;
      {Arguments to pass to shortcut}
    class function WantCreate: Boolean;
      {True if shortcut to be created and false if shortcut to be deleted.
      Shorcut deleted if only one parameter - name of shortcut file}
  end;


implementation


{ TSCParams }

class function TSCParams.Args: string;
  {Arguments to pass to shortcut}
var
  NextArg: string;  // next argument to be processed
  ArgIdx: Integer;  // loops thru arguments on command line
begin
  // Build shortcut argument string from remaining program arguments
  Result := '';
  // arguments begin with 7th parameter
  for ArgIdx := 7 to ParamCount do
  begin
    if Pos(' ', ParamStr(ArgIdx)) > 0 then
      // argument contains spaces => wrap in quotes
      NextArg := '"' + ParamStr(ArgIdx) + '"'
    else
      // argument is single word => pass as-is
      NextArg := ParamStr(ArgIdx);
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + NextArg;
  end;
end;

class function TSCParams.AssocFile: string;
  {Name of file to be associated with shortcut}
begin
  // Associated file is 2nd parameter
  SetValFromParam(2, Result);
end;

class function TSCParams.Desc: string;
  {Shortcut description}
begin
  // Description is 4th parameter
  SetValFromParam(4, Result);
end;

class function TSCParams.IconFile: string;
  {Name of file containing shortcut's icon}
begin
  // Icon file is 5th parameter
  SetValFromParam(5, Result);
end;

class function TSCParams.IconIndex: Integer;
  {Index of shorcut icon within file. If no index provided 0 is used}
var
  IdxStr: string;         // icon index as string
  ConvertError: Integer;  // none-zero error code if string conversion fails
begin
  // Icon index is 6th parameter
  SetValFromParam(6, IdxStr);
  if IdxStr = '' then
    // no parameter: use 0
    Result := 0
  else
  begin
    // convert param to integer
    Val(IdxStr, Result, ConvertError);
    if ConvertError <> 0 then
      // not a valid integer: use 0
      Result := 0;
  end;
end;

class procedure TSCParams.SetValFromParam(const ParamIdx: Integer;
  out Value: string);
  {Sets Value to value of parameter at given index. If required parameter is
  '*' Value is set to ''}
begin
  if ParamStr(ParamIdx) = '*' then
    Value := ''
  else
    Value := ParamStr(ParamIdx);
end;

class function TSCParams.ShortcutFile: string;
  {Name of shortcut file to be created or deleted}
begin
  // Shortcut file name is 1st parameter
  SetValFromParam(1, Result);
end;

class function TSCParams.WantCreate: Boolean;
  {True if shortcut to be created and false if shortcut to be deleted.
  Shorcut deleted if only one parameter - name of shortcut file}
begin
  Result := ParamCount >= 2;
end;

class function TSCParams.WorkDir: string;
  {Working directory for any program run from shortcut}
begin
  // Working directory is 3rd parameter
  SetValFromParam(3, Result);
end;

end.

