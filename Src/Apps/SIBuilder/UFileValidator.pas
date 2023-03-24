{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UFileValidator.pas
  @COMMENTS                 Implements a class that can validate the files
                            contained in the project. Reasons for problems and
                            associated help contexts can optionally be provided.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
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
 * The Original Code is UFileValidator.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UFileValidator;


interface


uses
  // Project
  UFiles;

type

  {
  TFileValidator:
    class that can validate the files contained in a project. The reasons for
    problems and associated help contexts can optionally be provided.

    Inheritance: TFileValidator -> [TObject]
  }
  TFileValidator = class(TObject)
  public
    function ValidateFile(const FileInfo: TFileInfo;
      UninstallPrevious: Boolean): Boolean; overload;
      {Checks that given file is valid according to whether project is permitted
      to uninstall previous installations rather than overwriting them. Returns
      true if the file is valid and false oterhwise}
    function ValidateFile(const FileInfo: TFileInfo;
      UninstallPrevious: Boolean; out Reason: string;
      out HelpCtx: Integer): Boolean; overload;
      {Checks that given file is valid according to whether project is permitted
      to uninstall previous installations rather than overwriting them. Returns
      true if the file is valid. If file is not valid then false is returned and
      the reason for the error and any associated help context giving further
      information are passed back through the out parameters}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelpContexts;


resourcestring
  // Error messages
  sFileDoesntExist = 'File does not exist';
  sCantRegDLL =
    'Can''t register shared DLL when not uninstalling any previous project';


{ TFileValidator }

function TFileValidator.ValidateFile(const FileInfo: TFileInfo;
  UninstallPrevious: Boolean): Boolean;
  {Checks that given file is valid according to whether project is permitted to
  uninstall previous installations rather than overwriting them. Returns true if
  the file is valid and false oterhwise}
var
  DummyReason: string;    // dummy value passed to overloaded method
  DummyHelpCtx: Integer;  // dummy value passed to overloaded method
begin
  // Call overloaded version of method to do actual processing
  Result := ValidateFile(
    FileInfo, UninstallPrevious, DummyReason, DummyHelpCtx
  );
end;

function TFileValidator.ValidateFile(const FileInfo: TFileInfo;
  UninstallPrevious: Boolean; out Reason: string;
  out HelpCtx: Integer): Boolean;
  {Checks that given file is valid according to whether project is permitted to
  uninstall previous installations rather than overwriting them. Returns true if
  the file is valid. If file is not valid then false is returned and the reason
  for the error and any associated help context giving further information are
  passed back through the out parameters}
begin
  // Assume success
  Result := True;
  Reason := '';
  HelpCtx := 0;
  // Check that source file actually exists
  if not SysUtils.FileExists(FileInfo.SourceFileSpec) then
  begin
    Result := False;
    Reason := sFileDoesntExist;
    HelpCtx := 0;   // self explanatory
  end
  // If we can uninstall previous installations in same folder then we can't
  // register shared DLLs
  else if not UninstallPrevious and (FileInfo.RegisterSharedDLL) then
  begin
    Result := False;
    Reason := sCantRegDLL;
    HelpCtx := IDH_ERR_CANTREGSHAREDDLL;
  end;
end;

end.
